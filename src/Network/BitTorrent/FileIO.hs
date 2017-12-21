{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Network.BitTorrent.FileIO
Description : File I/O operations
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

File I/O operations

-}

module Network.BitTorrent.FileIO
  ( initFiles
  , checkPieces
  , readAndVerifyPiece
  , readPiece
  , writePiece) where
  

import Protolude hiding (concat, pi)
import System.IO
import System.Directory
import Data.Text (unpack)
import Data.Maybe
import Control.Concurrent.STM.TVar
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import Control.Concurrent.Extra (Lock, newLock, withLock)

import Data.MetaInfo
import Data.Crypto
import Network.BitTorrent.Types

initFiles :: Int64 -> [ByteString] -> FileInfo -> IO (IOConfig)
initFiles pieceSize pieces (SingleFileInfo fileName (FileProp len _ _)) = do
  file <- openTFile (unpack fileName) len
  m <- makePiece2FileMap file
  return $ IOConfig [file] m
    where makePiece2FileMap file = foldlM fn M.empty $ zip pieces [0,pieceSize..]
            where fn m (h, o) = do tvarPI <- (mkpi o)
                                   return $ M.insert h tvarPI  m
                  mkpi o = newTVarIO $ (PieceInfo 0 Incomplete [FileSection file o pieceSize])
initFiles pieceSize pieces (MultiFileInfo dirName fileProps) = do
  files <- mapM openDirFile fileProps
  m <- makePiece2FileMap pieces files 0 M.empty
  return $ IOConfig files $ m
    where openDirFile (FileProp len _ fileName) = openTDirFile dirName (fromJust fileName) len
          makePiece2FileMap :: [ByteString] -> [File] -> Int64 -> Map ByteString (TVar PieceInfo) -> IO (Map ByteString (TVar PieceInfo))
          makePiece2FileMap [] _ _ m = pure m
          makePiece2FileMap _ [] _ m = pure m
          makePiece2FileMap (p:ps) ((f@(File _ _ len):fs)) offset m
            | offset + pieceSize < len = do m' <- insertPI p m f offset pieceSize
                                            makePiece2FileMap ps (f:fs) (offset + pieceSize) m'
            | otherwise                = do m' <- insertPI p m f offset (len - offset)
                                            makePiece2FileMap (p:ps) fs (pieceSize - (len - offset)) m'
          insertPI :: ByteString -> Map ByteString (TVar PieceInfo) -> File -> Int64 -> Int64 -> IO (Map ByteString (TVar PieceInfo))
          insertPI p m f off len = case M.lookup p m of
                                     Just v  -> do v' <- readTVarIO v
                                                   tvarPI <- mkpiV f off len v'
                                                   return $ M.insert p tvarPI m
                                     Nothing -> do tvarPI <- mkpi_ f off len
                                                   return $ M.insert p tvarPI m
          mkpiV f off len v = newTVarIO $ (PieceInfo 0 Incomplete $ view piSections v ++ [FileSection f off len])
          mkpi_ f off len   = newTVarIO $ (PieceInfo 0 Incomplete [FileSection f off len])
                      
openTDirFile :: FilePath -> FilePath -> Int64 -> IO (File)
openTDirFile dir fn len = do
  createDirectoryIfMissing True dir
  f <- openTFile (dir ++ "/" ++  fn) len
  return f
  
openTFile :: FilePath -> Int64 -> IO (File)
openTFile fn len = do
  h <- openFile fn ReadWriteMode
  hSetFileSize h $ toInteger len
  l <- newLock
  return $ File h l len

checkPieces :: TorrentState -> IO ()
checkPieces state = do
  let ioCfg = _tsIOConfig state
  let p2fmap = _ioPiece2FileMap ioCfg
  forM_ (M.assocs p2fmap) checkPiece
    where checkPiece :: (ByteString, TVar PieceInfo) -> IO (ByteString, TVar PieceInfo)
          checkPiece (hash, tvarPI) = do
            pi <- readTVarIO tvarPI
            maybeString <- readAndVerifyPiece hash pi
            void $ return $ case maybeString of
                              Just _  -> modifyTVar' tvarPI (set piState Complete)
                              Nothing -> modifyTVar' tvarPI (set piState Incomplete)
            pi' <- readTVarIO tvarPI
            return (hash, tvarPI)

readAndVerifyPiece :: ByteString -> PieceInfo -> IO (Maybe ByteString)
readAndVerifyPiece hash pi = do
  bytes <- readPiece pi
  return $ if hashSHA1 bytes == hash then Just bytes else Nothing

readPiece :: PieceInfo -> IO (ByteString)
readPiece (PieceInfo _ _ sections) = do
  byteStrings <- mapM readSection sections
  return $ BS.concat byteStrings
    where readSection :: FileSection -> IO (ByteString)
          readSection section = do
            let h = view (fsFile . fileHandle) section
                l = view (fsFile . fileLock)   section
            withLock l $ do
              hSeek h AbsoluteSeek (toInteger $ view fsOffset section)
              BS.hGet (view (fsFile . fileHandle) section) (fromInteger $ toInteger $ view fsLen section)

writePiece :: PieceInfo -> ByteString -> IO ()
writePiece (PieceInfo _ _ sections) piece = do
  void $ foldM writeSection piece sections
  return ()
    where writeSection :: ByteString -> FileSection -> IO (ByteString)
          writeSection piece section = do
            let (curr, rest) = BS.splitAt (fromInteger $ toInteger $ view fsLen section) piece
                h = view (fsFile . fileHandle) section
                l = view (fsFile . fileLock)   section
            withLock l $ do
              hSeek h AbsoluteSeek (toInteger $ view fsOffset section)
              BS.hPut (view (fsFile . fileHandle) section) curr
            return rest
