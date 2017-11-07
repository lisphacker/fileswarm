module Main where

import Protolude
import Options.Applicative
import Options.Applicative.Text
import Data.Text (unpack)

data CommandLineOptions = CommandLineOptions { torrentFile :: Text }

run :: CommandLineOptions -> IO ()
run cmdLineOpts = do
  putStrLn $ "To be implemented - decode " ++ (unpack $ torrentFile cmdLineOpts) 

main :: IO ()
main = execParser opts >>= run
  where opts = info parser mempty
        parser = CommandLineOptions <$> argument text (metavar "TORRENT-FILE")
