{-# LANGUAGE TemplateHaskell #-}
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
  , tsAnnounceURL
  , tsInfoHash
  , tsPeerId
  , tsTrackerId
  , tsPort
  , tsDownloaded
  , tsUploaded
  , tsLeft
  , newTorrentState ) where

import Protolude
import Control.Lens hiding (element)
import Network.Socket

import Data.Crypto
import Data.MetaInfo

data Piece = Piece { _pcHash :: ByteString
                   , _pcDownloaded :: Int64
                   } deriving (Show)

data Peer = Peer { _peerId   :: ByteString
                 , _peerAddr :: SockAddr
                 } deriving (Show)

data TorrentState = TorrentState { _tsAnnounceURL :: Text
                                 , _tsInfoHash    :: ByteString
                                 , _tsPieceSize   :: Int64
                                 , _tsPeerId      :: ByteString
                                 , _tsTrackerId   :: ByteString
                                 , _tsPort        :: Int
                                 , _tsPieceDir    :: Text
                                 , _tsPieces      :: [Piece]
                                 , _tsUploaded    :: Int64
                                 , _tsDownloaded  :: Int64
                                 , _tsLeft        :: Int64
                                 } deriving (Show)

makeLenses ''Piece
makeLenses ''Peer
makeLenses ''TorrentState

newTorrentState :: Int -> MetaInfo -> IO (TorrentState)
newTorrentState port mi = do
  uuid <- makeUUID
  return $ TorrentState (miAnnounce mi) (miInfoHash mi) (miPieceLength $ miInfo mi) uuid "" port "" pieces 0 0 0
    where pieces = fmap (\h -> Piece h 0) $ miPieces $ miInfo mi
