module Main where

import Protolude
import Options.Applicative
import qualified Data.ByteString as B
import Data.Text (pack, unpack)
import Data.Bencoding (decode)
import qualified Data.MetaInfo as MI

data CommandLineOptions = CommandLineOptions { torrentFile :: Text
                                             , port        :: Int
                                             } deriving (Show)


run :: CommandLineOptions -> IO ()
run cmdLineOpts = do
  torrentFileContents <- (B.readFile . unpack . torrentFile) cmdLineOpts
  case decode torrentFileContents of
    Just bencVal -> putStrLn $ pack $ show $ MI.decode bencVal
    Nothing      -> return ()

main :: IO ()
main = execParser opts >>= run
  where opts = info parser mempty
        parser = CommandLineOptions
          <$> pack <$> argument str ( metavar "TORRENT-FILE" <> help "Torrent file")
          <*> argument auto (metavar "PORT" <> help "Port to listen to" <> showDefault <> value 6881)
