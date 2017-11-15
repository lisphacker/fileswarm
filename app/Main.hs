module Main where

import Protolude
import Options.Applicative
import Options.Applicative.Text
import qualified Data.ByteString as B
import Data.Text (pack, unpack)
import Data.Bencoding (decode)
import qualified Data.MetaInfo as MI

data CommandLineOptions = CommandLineOptions { torrentFile :: Text }


run :: CommandLineOptions -> IO ()
run cmdLineOpts = do
  torrentFileContents <- (B.readFile . unpack . torrentFile) cmdLineOpts
  case decode torrentFileContents of
    Just bencVal -> putStrLn $ pack $ show $ MI.decode bencVal
    Nothing      -> return ()

main :: IO ()
main = execParser opts >>= run
  where opts = info parser mempty
        parser = CommandLineOptions <$> argument text (metavar "TORRENT-FILE")
