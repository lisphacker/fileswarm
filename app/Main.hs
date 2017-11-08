module Main where

import Protolude
import Options.Applicative
import Options.Applicative.Text
import qualified Data.ByteString as B
import Data.Text (pack, unpack)
import Data.Bencoding (decode)

data CommandLineOptions = CommandLineOptions { torrentFile :: Text }


run :: CommandLineOptions -> IO ()
run cmdLineOpts = do
  torrentFileContents <- (B.readFile . unpack . torrentFile) cmdLineOpts
  case decode torrentFileContents of
    Just bencVal -> putStrLn $ pack $ show bencVal
    Nothing      -> return ()

main :: IO ()
main = execParser opts >>= run
  where opts = info parser mempty
        parser = CommandLineOptions <$> argument text (metavar "TORRENT-FILE")
