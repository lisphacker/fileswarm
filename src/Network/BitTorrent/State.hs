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
  , tsPeers
  , tsDownloaded
  , tsUploaded
  , tsLeft
  , newTorrentState ) where

import Protolude
import Control.Lens hiding (element)
import Network.Socket
import Control.Monad

import Data.Crypto
import Data.MetaInfo
import Control.Concurrent.STM.TVar

import Network.BitTorrent.FileIO

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
                                 , _tsPeers       :: TVar [Peer]
                                 , _tsPieceDir    :: Text
                                 , _tsPieces      :: [Piece]
                                 , _tsUploaded    :: Int64
                                 , _tsDownloaded  :: Int64
                                 , _tsLeft        :: Int64
                                 , _tsIOConfig    :: TVar IOConfig
                                 } --deriving (Show)

makeLenses ''Piece
makeLenses ''Peer
makeLenses ''TorrentState

newPeerState :: IO (TVar [Peer])
newPeerState = newTVarIO []

newTorrentState :: Int -> MetaInfo -> IO (TorrentState)
newTorrentState port mi = do
  uuid <- makeUUID
  peerState <- newPeerState
  let info = miInfo mi
  ioCfg <- initFiles (miPieceLength info) (miPieces info) (miFileInfo info) >>= newTVarIO
  return $ TorrentState (miAnnounce mi) (miInfoHash mi) (miPieceLength info) uuid "" port peerState "" pieces 0 0 0 ioCfg
    where pieces = fmap (\h -> Piece h 0) $ miPieces $ miInfo mi
