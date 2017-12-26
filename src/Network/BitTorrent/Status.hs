{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Network.BitTorrent.Status
Description : Displays current status
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

Displays current status

-}

module Network.BitTorrent.Status where

import Protolude
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import Network.BitTorrent.Types
import Network.BitTorrent.FileIO

statusThread :: TorrentState -> PieceIORequestChannel -> IO ()
statusThread state pioReqChan = do
  pioResChan <- newTQueueIO
  forever $ do
    (i,d,c) <- getState pioReqChan pioResChan
    putStrLn $ ((show (i,d,c) :: Text))
    threadDelay 1000000
