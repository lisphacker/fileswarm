{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Network.BitTorrent.PeerClient
Description : Peer client operations
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

Peer client operations

-}

module Network.BitTorrent.PeerClient where

import Protolude
import Data.Maybe
import Control.Concurrent.STM
import qualified Data.Map.Strict as M

import Network.BitTorrent.Types
import Network.BitTorrent.FileIO

peerClientThread :: ByteString -> TorrentState -> PieceIORequestChannel -> IO ()
peerClientThread hash state pioReqChan = do
  putStrLn ("Starting peer client thread" :: Text)
  pioResChan <- newTQueueIO
  setPieceState pioReqChan pioResChan hash Downloading

