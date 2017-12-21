module Main where

import Protolude
import Options.Applicative
import qualified Data.ByteString as B
import Data.Text (pack, unpack)
import Control.Lens (view)
import Control.Monad
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM.TVar
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
      checkPieces torrentState
      
      --peers <- readTVarIO (view tsPeers torrentState)
      --ioCfg <- readTVarIO (view tsIOConfig torrentState)
      --putStrLn $ ((show peers) :: Text)
      --putStrLn $ ((show $ fmap _piState $ M.elems $ _ioPiece2FileMap ioCfg) :: Text)

      spawnPeerClientThreads torrentState
      
      peerServerListenThread <- async (peerServerListenThread torrentState)
      statusThread <- async (statusThread torrentState)
      void $ wait peerServerListenThread
      void $ wait statusThread
      return ()
  return ()
  

main :: IO ()
main = execParser opts >>= run
  where opts = info parser mempty
        parser = CommandLineOptions
          <$> pack <$> argument str ( metavar "TORRENT-FILE" <> help "Torrent file")
          <*> argument auto (metavar "PORT" <> help "Port to listen to" <> showDefault <> value 6881)

spawnPeerClientThreads :: TorrentState -> IO ()
spawnPeerClientThreads state = do
  let ioCfg = _tsIOConfig state
  let h = fromJust $ head $ M.keys $ _ioPiece2FileMap ioCfg
  forM_ ((M.assocs . _ioPiece2FileMap) ioCfg) $ spawnPeerClientThread h
    where spawnPeerClientThread h (hash, tvarPI) = do pi <- readTVarIO tvarPI
                                                      return $ case _piState pi of
                                                        Incomplete -> void $ async (peerClientThread hash state)
                                                        _          -> return ()
    
