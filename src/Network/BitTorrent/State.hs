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
  ( newTorrentState ) where

import Protolude
import Control.Monad
import Control.Concurrent.STM.TVar

import Data.Crypto
import Data.MetaInfo

import Network.BitTorrent.Types
import Network.BitTorrent.FileIO

newPeerState :: IO (TVar [Peer])
newPeerState = newTVarIO []

newTorrentState :: Int -> MetaInfo -> IO (TorrentState)
newTorrentState port mi = do
  uuid <- makeUUID
  peerState <- newPeerState
  let info = miInfo mi
  ioCfg <- initFiles (miPieceLength info) (miPieces info) (miFileInfo info) >>= newTVarIO
  return $ TorrentState (miAnnounce mi) (miInfoHash mi) (miPieceLength info) uuid "" port peerState "" 0 0 0 ioCfg
