{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Network.BitTorrent.Peer
Description : Peer operations
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

Peer operations

-}

module Network.BitTorrent.Peer where

import Protolude

import Network.BitTorrent.Types

data PeerState = PeerState { _peerChoked       :: Bool
                           , _peerInterested   :: Bool
                           , _peerAmChoked     :: Bool
                           , _peerAmInterested :: Bool
                           } deriving (Show)
makeLenses ''PeerState

data PeerConnection = PeerConnection { _connPeer :: Peer
                                     , _connState :: PeerState
                                     } deriving (Show)
makeLenses ''PeerConnection


newPeerConn :: Peer -> PeerConnection
newPeerConn peer = PeerConnection peer $ PeerState True False True False
