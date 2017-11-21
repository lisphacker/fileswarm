{-|
Module      : Network.BitTorrent.State
Description : BitTorrent client/peer state
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

Client / peer state

-}

module Network.BitTorrent.State
  ( Tracker(..)
  , Client(..)
  , TorrentState(..)
  , newTorrentState ) where

import Protolude
import Data.Crypto

data Tracker = Tracker { trkAnnouneURL :: Text
                       , trkInfoHash :: ByteString
                       } deriving (Show)

data Torrent = Torrent { trnPieceSize :: Int6
                       } deriving (Show)

data Client = Client { cliPeerId   :: ByteString
                     , cliPort     :: Int
                     , cliPieceDir :: Text
                     } deriving (Show)

data Piece = Piece { pcHash :: ByteString
                   , downloaded :: Int64
                   } deriving (Show)
data TorrentState = TorrentState { tracker :: Tracker
                                 , client  :: Client
                                 } deriving (Show)

newTorrentState :: Int -> IO (TorrentState)
newTorrentState port = do
  uuid <- makeUUID
  return $ TorrentState (Tracker "") (Client uuid port)
