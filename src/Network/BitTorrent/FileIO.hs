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
  , fileIOThread ) where
  

import Protolude hiding (concat, pi, putStrLn, show)
import System.IO
import System.Directory
import Data.Text (unpack)
import Data.Maybe
import Data.IORef
import Control.Concurrent.STM
import Control.Lens
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import Control.Concurrent.Extra (Lock, newLock, withLock)
import GHC.Show (Show(..)) 

import Data.MetaInfo
import Data.Crypto
import Network.BitTorrent.Types


data PieceIOError = SUCCESS
                  | WRITE_TO_COMPLETE_PIECE
                  | ERROR
                  deriving (Eq, Show)

type PieceIORequestChannel  = TQueue PieceIORequest                  
type PieceIOResponseChannel = TQueue PieceIOResponse

instance Show (TQueue a) where
  show _ = "<tq>"
                  
data PieceIORequestData = PieceIOReadRequest { _pioRdReqHash     :: ByteString }
                        | PieceIOWriteRequest { _pioWrReqHash    :: ByteString
                                              , _pioWrReqData    :: ByteString
                                              }
                        | PieceIOListRequest { _pioListReqState :: PieceState }
                        | PieceIOGetStateRequest { _pioGetStateReqHash :: ByteString }
                        | PieceIOSetStateRequest { _pioSetStateReqHash :: ByteString
                                                 , _pioSetStateReqState :: PieceState }
                        | PieceIOStatusRequest
                        deriving (Show)

data PieceIORequest = PieceIORequest { _pioReqResChannel :: PieceIOResponseChannel
                                     , _pioReqData       :: PieceIORequestData
                                     }
                    deriving (Show)
                                     
data PieceIOResponse = PieceIOReadResponse { _pioRdResError        :: PieceIOError
                                           , _pioRdResData         :: Maybe ByteString
                                           }
                     | PieceIOWriteResponse {  _pioWrResError      :: PieceIOError
                                            }
                     | PieceIOListResponse { _pioListResHashes     :: [ByteString] }
                     | PieceIOGetStateResponse { _pioGetStateResState  :: PieceState }
                     | PieceIOSetStateResponse 
                     | PieceIOStatusResponse { _pioStatusIncomplete  :: Int
                                             , _pioStatusDownloading :: Int
                                             , _pioStatusComplete    :: Int
                                            }
                     deriving (Show)

data File = File { _fileHandle :: Handle
                 , _fileLen    :: Int64
                 } deriving (Show)
makeLenses ''File

data FileSection = FileSection { _fsFile   :: File
                               , _fsOffset :: Int64
                               , _fsLen    :: Int64
                               } deriving (Show)
makeLenses ''FileSection

data PieceInfo = PieceInfo { _piHash       :: ByteString
                           , _piDownloaded :: Int64
                           , _piState      :: PieceState
                           , _piSections   :: [FileSection]
                           } deriving (Show)
makeLenses ''PieceInfo

data FileIOState = FileIOState { _ioFiles :: [File]
                         , _ioPiece2FileMap :: Map ByteString PieceInfo
                         } 
makeLenses ''FileIOState

readQ :: TQueue a -> IO a
readQ q = atomically $ readTQueue q

writeQ :: TQueue a -> a -> IO ()
writeQ q v = atomically $ writeTQueue q v

readPiece :: PieceIORequestChannel -> PieceIOResponseChannel -> ByteString -> IO (Maybe ByteString)
readPiece reqChan resChan hash = do
  writeQ reqChan $ PieceIORequest resChan $ PieceIOReadRequest hash
  res <- readQ resChan
  return $ case res of
             PieceIOReadResponse err pieceData -> if err == SUCCESS then pieceData else Nothing
             _                                 -> Nothing
      
writePiece :: PieceIORequestChannel -> PieceIOResponseChannel -> ByteString -> ByteString -> IO ()
writePiece reqChan resChan hash pieceData = do
  writeQ reqChan $ PieceIORequest resChan $ PieceIOWriteRequest hash pieceData
  void $ readQ resChan

listIncompletePieces :: PieceIORequestChannel -> PieceIOResponseChannel -> IO ([ByteString])
listIncompletePieces reqChan resChan = listPieces reqChan resChan Incomplete
  
listPieces :: PieceIORequestChannel -> PieceIOResponseChannel -> PieceState -> IO ([ByteString])
listPieces reqChan resChan state = do
  writeQ reqChan $ PieceIORequest resChan $ PieceIOListRequest state
  res <- readQ resChan
  return $ case res of
             PieceIOListResponse l -> l
             _                     -> []

getPieceState :: PieceIORequestChannel -> PieceIOResponseChannel -> ByteString -> IO (PieceState)
getPieceState reqChan resChan hash = do
  writeQ reqChan $ PieceIORequest resChan $ PieceIOGetStateRequest hash
  res <- readQ resChan
  return $ case res of
             PieceIOGetStateResponse s -> s
             _                         -> Incomplete

setPieceState :: PieceIORequestChannel -> PieceIOResponseChannel -> ByteString -> PieceState -> IO ()
setPieceState reqChan resChan hash pieceState = do
  writeQ reqChan $ PieceIORequest resChan $ PieceIOSetStateRequest hash pieceState
  void $ readQ resChan

getState :: PieceIORequestChannel -> PieceIOResponseChannel -> IO (Int, Int, Int)
getState reqChan resChan = do
  writeQ reqChan $ PieceIORequest resChan $ PieceIOStatusRequest 
  res <- readQ resChan
  return $ case res of
             PieceIOStatusResponse i d c -> (i,d,c)
             _                           -> (-1, -1, -1)

fileIOThread :: MetaInfo -> PieceIORequestChannel -> IO ()
fileIOThread metaInfo pioReqChan = do
  let info = miInfo metaInfo
  putStrLn "Starting FileIO Thread"
  ioStateRef <- initFiles (miPieceLength info) (miPieces info) (miFileInfo info) >>= newIORef
  checkPieces ioStateRef
  forever $ do
    void $ readQ pioReqChan >>= processRequest ioStateRef
      where processRequest ioStateRef (PieceIORequest pioResChan pioReqData) = do
              ioState <- readIORef ioStateRef
              (res,ioState') <- processRequest' ioState pioReqData
              writeIORef ioStateRef ioState'
              writeQ pioResChan res
            processRequest' ioState (PieceIOReadRequest h) = do
              maybeRes <- readAndVerifyPieceInt $ lkup h ioState
              return $ case maybeRes of
                         Just _  -> (PieceIOReadResponse SUCCESS maybeRes, ioState)
                         Nothing -> (PieceIOReadResponse ERROR   Nothing,  ioState)
            processRequest' ioState (PieceIOWriteRequest h d) = do
              writePieceInt (lkup h ioState) d
              return (PieceIOWriteResponse SUCCESS, ioState)
            processRequest' ioState (PieceIOListRequest state) = do
              let hashes = foldMap f $ ioState ^. ioPiece2FileMap
              return (PieceIOListResponse hashes, ioState)
                where f (PieceInfo h _ s _) = if s == state then [h] else []
            processRequest' ioState (PieceIOGetStateRequest h) = do
              let pi = lkup h ioState
              return $ (PieceIOGetStateResponse $ pi ^. piState, ioState)
            processRequest' ioState (PieceIOSetStateRequest h s) = do
              traceM "SetState"
              let pi = lkup h ioState
              return $ (PieceIOSetStateResponse, ioPiece2FileMap %~ M.insert h (piState .~ s $ pi) $ ioState)
            processRequest' ioState (PieceIOStatusRequest) = do
              let (i,d,c) = foldr f (0,0,0) $ ioState ^. ioPiece2FileMap
              return (PieceIOStatusResponse i d c, ioState)
                where f (PieceInfo _ _ s _) (i,d,c) = case s of
                                                        Incomplete  -> (i+1,d,c)
                                                        Downloading -> (i,d+1,c)
                                                        Complete    -> (i,d,c+1)
              


  
--------------------------------------------------------------------------------

initFiles :: Int64 -> [ByteString] -> FileInfo -> IO (FileIOState)
initFiles pieceSize pieces (SingleFileInfo fileName (FileProp len _ _)) = do
  file <- openTFile (unpack fileName) len
  return $ FileIOState [file] $ makePiece2FileMap file
    where makePiece2FileMap file = foldl fn M.empty $ zip pieces [0,pieceSize..]
            where fn m (h, o) = M.insert h (mkpi h o)  m
                  mkpi h o = PieceInfo h 0 Incomplete [FileSection file o pieceSize]
initFiles pieceSize pieces (MultiFileInfo dirName fileProps) = do
  files <- mapM openDirFile fileProps
  let m = makePiece2FileMap pieces files 0 M.empty
  return $ FileIOState files m
    where openDirFile (FileProp len _ fileName) = openTDirFile dirName (fromJust fileName) len
          makePiece2FileMap :: [ByteString] -> [File] -> Int64 -> Map ByteString PieceInfo -> Map ByteString PieceInfo
          makePiece2FileMap [] _ _ m = m
          makePiece2FileMap _ [] _ m = m
          makePiece2FileMap (p:ps) ((f@(File _ len):fs)) offset m
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
  return $ File h len

checkPieces :: IORef FileIOState -> IO ()
checkPieces ioStateRef = do
  ioState <- readIORef ioStateRef
  let p2fmap = ioState ^. ioPiece2FileMap
  p2fmap' <- mapM checkPiece p2fmap
  writeIORef ioStateRef $ ioPiece2FileMap .~ p2fmap' $ ioState
  return ()
    where checkPiece pi = do
            maybeString <- readAndVerifyPieceInt pi
            return $ case maybeString of
                       Just _  -> set piState Complete pi
                       Nothing -> set piState Incomplete pi


lkup :: ByteString -> FileIOState -> PieceInfo
lkup h ioState = fromJust $ M.lookup h (_ioPiece2FileMap ioState)

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
            let h = section ^. fsFile . fileHandle
            hSeek h AbsoluteSeek (toInteger $ view fsOffset section)
            BS.hGet h (fromInteger $ toInteger $ view fsLen section)

writePieceInt :: PieceInfo -> ByteString -> IO ()
writePieceInt (PieceInfo _ _ _ sections) piece = do
  void $ foldM writeSection piece sections
  return ()
    where writeSection :: ByteString -> FileSection -> IO (ByteString)
          writeSection piece section = do
            let (curr, rest) = BS.splitAt (fromInteger $ toInteger $ view fsLen section) piece
                h = section ^. fsFile . fileHandle
            hSeek h AbsoluteSeek (toInteger $ view fsOffset section)
            BS.hPut h curr
            return rest
