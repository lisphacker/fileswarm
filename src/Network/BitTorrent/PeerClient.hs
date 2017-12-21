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
import Control.Concurrent.STM.TVar
import qualified Data.Map.Strict as M

import Network.BitTorrent.Types

peerClientThread :: ByteString -> TorrentState -> IO ()
peerClientThread hash state = do
  atomically $ do
    let ioCfg =_tsIOConfig state
    pi <- readTVar $ fromJust $ M.lookup hash (_ioPiece2FileMap ioCfg)
    check (Incomplete == _piState pi)
    writeTVar (fromJust $ M.lookup hash (_ioPiece2FileMap ioCfg))  $ (set piState Downloading pi)
    
  let ioCfg = _tsIOConfig state
  pi <- readTVarIO $ fromJust $ M.lookup hash (_ioPiece2FileMap ioCfg)
  let state = _piState pi
  putStrLn $ if state == Incomplete then ("Incomplete" :: Text) else ("Downloading" :: Text)
    
    
