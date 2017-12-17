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
import Control.Concurrent.Async (async, wait)

import Network.BitTorrent.Types

peerServerListenThread :: TorrentState -> IO ()
peerServerListenThread state = do
  s <- openListeningSocket $ _tsPort state
  putStrLn $ ("Started peer server" :: Text)
  loop s
  return ()
    where loop s = do
            (s',a) <- accept s
            async (peerServerThread s' a state)
            loop s
      

openListeningSocket :: Int -> IO (Socket)
openListeningSocket port = do
  s <- socket AF_INET Stream defaultProtocol
  bind s $ SockAddrInet (fromInteger $ toInteger port) 0
  listen s 5
  return s

peerServerThread :: Socket -> SockAddr -> TorrentState -> IO ()
peerServerThread sock addr state = do
  putStrLn ("Received connection" :: Text)
  close sock
  return ()

