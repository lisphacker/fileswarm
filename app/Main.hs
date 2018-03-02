module Main where

import Protolude
import Options.Applicative
import qualified Data.ByteString as B
import Data.Text (pack, unpack)
import Control.Lens (view)
import Control.Monad
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import Data.Maybe

import Data.Crypto
import qualified Data.Bencoding as Benc
import qualified Data.MetaInfo as MI
import Network.BitTorrent.Types
import Network.BitTorrent.Tracker
import Network.BitTorrent.State
import Network.BitTorrent.FileIO
import Network.BitTorrent.PeerClient
import Network.BitTorrent.PeerServer
import Network.BitTorrent.Status

data CommandLineOptions = CommandLineOptions { optTorrentFile :: Text
                                             , optPort        :: Int
                                             } deriving (Show)


run :: CommandLineOptions -> IO ()
run opts = do
  torrentFileContents <- (B.readFile . unpack . optTorrentFile) opts
  let eiMetaInfo = Benc.decode torrentFileContents >>= MI.decode
  case eiMetaInfo of
    Left err -> putStrLn ("Unable to decode metainfo" :: Text)
    Right metaInfo -> do
      torrentState <- newTorrentState (optPort opts) metaInfo
      updateTorrentStateFromTracker torrentState

      pioReqChan <- newTQueueIO

      fioThread <- async (fileIOThread metaInfo pioReqChan)
      
      spawnPeerClientThreads torrentState pioReqChan
      peerServerListenThread <- async (peerServerListenThread torrentState pioReqChan)
      statusThread <- async (statusThread torrentState pioReqChan)
      
      void $ wait peerServerListenThread
      void $ wait fioThread
      void $ wait statusThread
      return ()
  return ()
  

main :: IO ()
main = execParser opts >>= run
  where opts = info parser mempty
        parser = CommandLineOptions
          <$> pack <$> argument str ( metavar "TORRENT-FILE" <> help "Torrent file")
          <*> argument auto (metavar "PORT" <> help "Port to listen to" <> showDefault <> value 6881)

spawnPeerClientThreads :: TorrentState -> PieceIORequestChannel -> IO ()
spawnPeerClientThreads state pioReqChan = do
  pioResChan <- newTQueueIO
  pieces <- listIncompletePieces pioReqChan pioResChan
  forM_ [fromJust $ head pieces] spawnPeerClientThread 
    where spawnPeerClientThread h = async (peerClientThread h state pioReqChan)
