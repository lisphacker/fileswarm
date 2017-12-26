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
  ( PieceIOError
  , PieceIORequestChannel
  , PieceIOResponseChannel
  , readPiece
  , writePiece
  , listIncompletePieces
  , getPieceState
  , getState
  , setPieceState
  , pieceIOThread ) where
  

import Protolude hiding (concat, pi)
import System.IO
import System.Directory
import Data.Text (unpack)
import Data.Maybe
import Data.IORef
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import Control.Concurrent.Extra (Lock, newLock, withLock)

import Data.MetaInfo
import Data.Crypto
import Network.BitTorrent.Types


data PieceIOError = SUCCESS
                  | WRITE_TO_COMPLETE_PIECE
                  | ERROR

type PieceIORequestChannel  = TQueue PieceIORequest                  
type PieceIOResponseChannel = TQueue PieceIOResponse
                  
data PieceIORequest = PieceIOReadRequest { _pioRdReqHash           :: ByteString
                                         , _pioRdReqResChannel     :: TQueue PieceIOResponse
                                         }
                    | PieceIOWriteRequest { _pioWrReqHash          :: ByteString
                                          , _pioWrReqData          :: ByteString
                                          , _pioWrReqResChannel    :: TQueue PieceIOResponse
                                          }
                    | PieceIOListRequest { _pioListReqResChannel   :: TQueue PieceIOResponse }
                    | PieceIOStat1Request { _pioStat1ReqHash       :: ByteString
                                          , _pioStat1ReqResChannel :: TQueue PieceIOResponse
                                          }
                    | PieceIOStat2Request { _pioStat2ReqResChannel :: TQueue PieceIOResponse
                                          }

data PieceIOResponse = PieceIOReadResponse { _pioWrResError        :: PieceIOError
                                           , _pioWrResData         :: Maybe ByteString
                                           }
                     | PieceIOWriteResponse {  _pioWrResError      :: PieceIOError
                                            }
                     | PieceIOListResponse { _pioListResHashes     :: [ByteString] }
                     | PieceIOStat1Response { _pioStat1State       :: PieceState }
                     | PieceIOStat2Response { _pioStat2Incomplete  :: Int
                                            , _pioStat2Downloading :: Int
                                            , _pioStat2Complete    :: Int
                                            }




readPiece :: PieceIORequestChannel -> PieceIOResponseChannel -> ByteString -> IO (Maybe ByteString)
readPiece reqChan resChan hash = undefined

writePiece :: PieceIORequestChannel -> PieceIOResponseChannel -> ByteString -> ByteString -> IO (Maybe ByteString)
writePiece reqChan resChan hash pieceData = undefined

listIncompletePieces :: PieceIORequestChannel -> PieceIOResponseChannel -> IO ([ByteString])
listIncompletePieces reqChan resChan = undefined

getPieceState :: PieceIORequestChannel -> PieceIOResponseChannel -> ByteString -> IO (PieceState)
getPieceState reqChan resChan hash = undefined

setPieceState :: PieceIORequestChannel -> PieceIOResponseChannel -> ByteString -> PieceState -> IO ()
setPieceState reqChan resChan hash pieceState = undefined

getState :: PieceIORequestChannel -> PieceIOResponseChannel -> IO (Int, Int, Int)
getState reqChan resChan = undefined

pieceIOThread :: Int64 -> [ByteString] -> FileInfo -> IO ()
pieceIOThread pieceSize pieces fileInfo = do
  ioCfgRef <- initFiles pieceSize pieces fileInfo >>= newIORef
  checkPieces ioCfgRef
  forever $ do
    ioCfg <- readIORef ioCfgRef
    return ()
    




  
--------------------------------------------------------------------------------

initFiles :: Int64 -> [ByteString] -> FileInfo -> IO (IOConfig)
initFiles pieceSize pieces (SingleFileInfo fileName (FileProp len _ _)) = do
  file <- openTFile (unpack fileName) len
  return $ IOConfig [file] $ makePiece2FileMap file
    where makePiece2FileMap file = foldl fn M.empty $ zip pieces [0,pieceSize..]
            where fn m (h, o) = M.insert h (mkpi h o)  m
                  mkpi h o = PieceInfo h 0 Incomplete [FileSection file o pieceSize]
initFiles pieceSize pieces (MultiFileInfo dirName fileProps) = do
  files <- mapM openDirFile fileProps
  let m = makePiece2FileMap pieces files 0 M.empty
  return $ IOConfig files m
    where openDirFile (FileProp len _ fileName) = openTDirFile dirName (fromJust fileName) len
          makePiece2FileMap :: [ByteString] -> [File] -> Int64 -> Map ByteString PieceInfo -> Map ByteString PieceInfo
          makePiece2FileMap [] _ _ m = m
          makePiece2FileMap _ [] _ m = m
          makePiece2FileMap (p:ps) ((f@(File _ _ len):fs)) offset m
            | offset + pieceSize < len = let m' =insertPI p m f offset pieceSize
                                         in makePiece2FileMap ps (f:fs) (offset + pieceSize) m'
            | otherwise                = let m' = insertPI p m f offset (len - offset)
                                         in makePiece2FileMap (p:ps) fs (pieceSize - (len - offset)) m'
          insertPI :: ByteString -> Map ByteString PieceInfo -> File -> Int64 -> Int64 -> Map ByteString PieceInfo
          insertPI p m f off len = case M.lookup p m of
                                     Just v  -> M.insert p (mkpiV p f off len v) m
                                     Nothing -> M.insert p (mkpi_ p f off len) m
          mkpiV h f off len v = PieceInfo h 0 Incomplete $ view piSections v ++ [FileSection f off len]
          mkpi_ h f off len   = PieceInfo h 0 Incomplete [FileSection f off len]
                      
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

checkPieces :: IORef IOConfig -> IO ()
checkPieces ioCfgRef = do
  ioCfg <- readIORef ioCfgRef
  let p2fmap = _ioPiece2FileMap ioCfg
  p2fmap' <- mapM checkPiece p2fmap
  writeIORef ioCfgRef (set ioPiece2FileMap p2fmap' ioCfg)
  return ()
    where checkPiece pi = do
            maybeString <- readAndVerifyPieceInt pi
            return $ case maybeString of
                       Just _  -> set piState Complete pi
                       Nothing -> set piState Incomplete pi


readAndVerifyPieceInt :: PieceInfo -> IO (Maybe ByteString)
readAndVerifyPieceInt pi = do
  bytes <- readPieceInt pi
  return $ if hashSHA1 bytes == (_piHash pi) then Just bytes else Nothing

readPieceInt :: PieceInfo -> IO (ByteString)
readPieceInt (PieceInfo _ _ _ sections) = do
  byteStrings <- mapM readSection sections
  return $ BS.concat byteStrings
    where readSection :: FileSection -> IO (ByteString)
          readSection section = do
            let h = view (fsFile . fileHandle) section
            hSeek h AbsoluteSeek (toInteger $ view fsOffset section)
            BS.hGet (view (fsFile . fileHandle) section) (fromInteger $ toInteger $ view fsLen section)

writePieceInt :: PieceInfo -> ByteString -> IO ()
writePieceInt (PieceInfo _ _ _ sections) piece = do
  void $ foldM writeSection piece sections
  return ()
    where writeSection :: ByteString -> FileSection -> IO (ByteString)
          writeSection piece section = do
            let (curr, rest) = BS.splitAt (fromInteger $ toInteger $ view fsLen section) piece
                h = view (fsFile . fileHandle) section
            hSeek h AbsoluteSeek (toInteger $ view fsOffset section)
            BS.hPut (view (fsFile . fileHandle) section) curr
            return rest
