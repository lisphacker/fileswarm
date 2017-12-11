module Main where

import Protolude
import Options.Applicative
import qualified Data.ByteString as B
import Data.Text (pack, unpack)
import Control.Lens (view)
import Control.Concurrent.STM.TVar

import Data.Crypto
import qualified Data.Bencoding as Benc
import qualified Data.MetaInfo as MI
import qualified Network.BitTorrent.Tracker as Trk
import qualified Network.BitTorrent.State as S

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
      torrentState <- S.newTorrentState (optPort opts) metaInfo
      Trk.updateTorrentStateFromTracker torrentState
      peers <- atomically $ readTVar (view S.tsPeers torrentState)
      putStrLn $ ((show peers) :: Text)
      return ()
  return ()
  

main :: IO ()
main = execParser opts >>= run
  where opts = info parser mempty
        parser = CommandLineOptions
          <$> pack <$> argument str ( metavar "TORRENT-FILE" <> help "Torrent file")
          <*> argument auto (metavar "PORT" <> help "Port to listen to" <> showDefault <> value 6881)
