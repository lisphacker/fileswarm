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
import Control.Lens
import System.Random

import Network.BitTorrent.Types

data PeerState = PeerState { _peerChoked       :: Bool
                           , _peerInterested   :: Bool
                           , _peerAmChoked     :: Bool
                           , _peerAmInterested :: Bool
                           } deriving (Show)
makeLenses ''PeerState

data PeerConnection = PeerConnection { _connPeer  :: Peer
                                     , _connState :: PeerState
                                     } deriving (Show)
makeLenses ''PeerConnection

newPeerConn :: Peer -> PeerConnection
newPeerConn peer = PeerConnection peer $ PeerState True False True False

data PWPMessage = KeepAlive
                | Choke
                | Unchoke
                | Interested
                | NotInterested
                | Have Int
                | Bitfield
                | Request Int Int Int
                | Piece Int Int ByteString
                | Cancel Int Int Int
                | Port Int

pickRandomPeer :: [Peer] -> IO Peer
pickRandomPeer peers = do
  r <- randomIO
  let i = r `mod` (length peers)
  return $ el peers i
    where el (x:xs) 0 = x
          el (x:xs) i = el xs (i - 1)
