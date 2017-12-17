{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Network.BitTorrent.PeerServer
Description : Peer client operations
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

Peer client operations

-}

module Network.BitTorrent.PeerServer where

import Protolude
import Network.Socket
import Control.Concurrent

import Network.BitTorrent.Types

peerServerThread :: TorrentState -> IO ()
peerServerThread state = do
  threadDelay 5000000
  putStrLn $ ("Started peer server" :: Text)

openListeningSocket :: Int -> IO (Socket)
openListeningSocket port = undefined
