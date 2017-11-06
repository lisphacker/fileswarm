module Main where

import Protolude
import Data.Text (unpack)
import Options.Applicative
import Options.Applicative.Text
import System.IO hiding (putStrLn)

--import BitTorrent.MetaInfo
import BitTorrent.Bencoding

data CommandLineOptions = CommandLineOptions { torrentFile :: Text }

run :: CommandLineOptions -> IO ()
run cmdLineOpts = do
  torrentFile <- openFile (unpack $ torrentFile cmdLineOpts) ReadMode
  hSetBinaryMode torrentFile True
  torrentData <- hGetContents torrentFile
  putStrLn torrentData
  hClose torrentFile

main :: IO ()
main = execParser opts >>= run
  where opts = info parser mempty
        parser = CommandLineOptions <$> argument text (metavar "TORRENT-FILE")
