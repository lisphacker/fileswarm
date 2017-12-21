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
  ps <- atomically $ do
    let ioCfg =_tsIOConfig state
    pi <- readTVar $ fromJust $ M.lookup hash (_ioPiece2FileMap ioCfg)
    let pieceState = _piState pi
    return $ if Incomplete == pieceState
             then
               do writeTVar (fromJust $ M.lookup hash (_ioPiece2FileMap ioCfg))  $ (set piState Downloading pi)
                  return $ Downloading
             else
               pure pieceState

  let ioCfg = _tsIOConfig state
  let tvarPI = fromJust $ M.lookup hash (_ioPiece2FileMap ioCfg)
  pi <- readTVarIO tvarPI
  when (Downloading == _piState pi) $ do
    fetchPiece tvarPI state

fetchPiece :: (TVar PieceInfo) -> TorrentState -> IO ()
fetchPiece = undefined
