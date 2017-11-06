module BitTorrent.Bencoding where

import Protolude
import Data.Attoparsec.ByteString

data BencElement = BencString ByteString
                 | BencInt Int64
                 | BencList [BencElement]
                 | BencDict (Map Text BencElement)

parseBencString :: Text -> IO (Maybe BencElement)
parseBencString text
