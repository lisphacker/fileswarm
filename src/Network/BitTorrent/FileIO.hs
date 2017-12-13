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
  , checkPieces ) where
  

import Protolude hiding (concat)
import System.IO
import System.Directory
import Data.Text (unpack)
import Data.Maybe
import Control.Concurrent.STM.TVar
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS

import Data.MetaInfo
import Data.Crypto
import Network.BitTorrent.Types

initFiles :: Int64 -> [ByteString] -> FileInfo -> IO (IOConfig)
initFiles pieceSize pieces (SingleFileInfo fileName (FileProp len _ _)) = do
  file <- openTFile (unpack fileName) len
  return $ IOConfig [file] $ makePiece2FileMap file
    where makePiece2FileMap file = foldl' fn M.empty $ zip pieces [0,pieceSize..]
            where fn m (h, o) = M.insert h (PieceInfo 0 Incomplete [FileSection file o pieceSize]) m
initFiles pieceSize pieces (MultiFileInfo dirName fileProps) = do
  files <- mapM openDirFile fileProps
  return $ IOConfig files $ (makePiece2FileMap pieces files 0 M.empty)
    where openDirFile (FileProp len _ fileName) = openTDirFile dirName (fromJust fileName) len
          makePiece2FileMap [] _ _ m = m
          makePiece2FileMap _ [] _ m = m
          makePiece2FileMap (p:ps) ((f@(File _ len):fs)) offset m
            | offset + pieceSize < len = makePiece2FileMap ps (f:fs) (offset + pieceSize) $ makePI p m f offset pieceSize
            | otherwise = makePiece2FileMap (p:ps) fs (pieceSize - (len - offset)) $ makePI p m f offset (len - offset)
          makePI p m f off len = case M.lookup p m of
                                   Just v  -> M.insert p (PieceInfo 0 Incomplete $ view piSections v ++ [FileSection f off len]) m
                                   Nothing -> M.insert p (PieceInfo 0 Incomplete [FileSection f off len]) m
                      
openTDirFile :: FilePath -> FilePath -> Int64 -> IO (File)
openTDirFile dir fn len = do
  createDirectoryIfMissing True dir
  f <- openTFile (dir ++ "/" ++  fn) len
  return f
  
openTFile :: FilePath -> Int64 -> IO (File)
openTFile fn len = do
  h <- openFile fn ReadWriteMode
  hSetFileSize h $ toInteger len
  return $ File h len

checkPieces :: TorrentState -> IO ()
checkPieces state = do
  atomically $ do
    ioCfg <- readTVar (view tsIOConfig state)
    return ()
  return ()


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
            let handle = view (fsFile . fileHandle) section
            hSeek handle AbsoluteSeek (toInteger $ view fsOffset section)
            BS.hGet (view (fsFile . fileHandle) section) (fromInteger $ toInteger $ view fsLen section)
