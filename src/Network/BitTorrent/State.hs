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
  ( Piece(..)
  , Peer(..)
  , TorrentState(..)
  , newTorrentState ) where

import Protolude
import Network.Socket

import Data.Crypto
import Data.MetaInfo

data Piece = Piece { pcHash :: ByteString
                   , pcDownloaded :: Int64
                   } deriving (Show)

data Peer = Peer { peerId   :: ByteString
                 , peerAddr :: SockAddr
                 } deriving (Show)

data TorrentState = TorrentState { tsAnnounceURL :: Text
                                 , tsInfoHash    :: ByteString
                                 , tsPieceSize   :: Int64
                                 , tsPeerId      :: ByteString
                                 , tsPort        :: Int
                                 , tsPieceDir    :: Text
                                 , tsPieces      :: [Piece]
                                 , tsUploaded    :: Int64
                                 , tsDownloaded  :: Int64
                                 , tsLeft        :: Int64
                                 } deriving (Show)

newTorrentState :: Int -> MetaInfo -> IO (TorrentState)
newTorrentState port mi = do
  uuid <- makeUUID
  return $ TorrentState (miAnnounce mi) (miInfoHash mi) (miPieceLength $ miInfo mi) uuid port "" pieces 0 0 0
    where pieces = fmap (\h -> Piece h 0) $ miPieces $ miInfo mi
