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
    ioCfg <- readTVar (_tsIOConfig state)
    let pi = fromJust $ M.lookup hash (_ioPiece2FileMap ioCfg)
    check (Incomplete == _piState pi)
    writeTVar (_tsIOConfig state) $ (set ioPiece2FileMap (M.insert hash (set piState Downloading pi) (_ioPiece2FileMap ioCfg)) ioCfg)
    
  ioCfg <- readTVarIO (_tsIOConfig state)
  let state = _piState $ fromJust $ M.lookup hash (_ioPiece2FileMap ioCfg)
  putStrLn $ if state == Incomplete then ("Incomplete" :: Text) else ("Downloading" :: Text)
    
    
