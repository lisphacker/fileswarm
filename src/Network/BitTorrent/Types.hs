{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Network.BitTorrent.Types
Description : Data types
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

Data types

-}

module Network.BitTorrent.Types
  ( module X
  , Peer(..)
  , PieceState(..)
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
  ) where

import Protolude
import Network.Socket
import Control.Concurrent.STM
import Control.Lens as X (view, set)
import Control.Lens hiding (element)
import GHC.Show (Show(..))

data Peer = Peer { _peerId   :: ByteString
                 , _peerAddr :: SockAddr
                 } deriving (Show)
makeLenses ''Peer

data PieceState = Incomplete | Downloading | Complete
  deriving (Eq, Show)


data TorrentState = TorrentState { _tsAnnounceURL :: Text
                                 , _tsInfoHash    :: ByteString
                                 , _tsPieceSize   :: Int64
                                 , _tsPeerId      :: ByteString
                                 , _tsTrackerId   :: ByteString
                                 , _tsPort        :: Int
                                 , _tsPeers       :: TVar [Peer]
                                 , _tsPieceDir    :: Text
                                 , _tsUploaded    :: Int64
                                 , _tsDownloaded  :: Int64
                                 , _tsLeft        :: Int64
                                 }
makeLenses ''TorrentState




