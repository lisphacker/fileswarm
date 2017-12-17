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
import Control.Concurrent.STM.TVar
import qualified Data.Map.Strict as M
import Network.BitTorrent.Types

statusThread :: TorrentState -> IO ()
statusThread state = do
  ioCfg <- readTVarIO (_tsIOConfig state)
  let (i,d,c) = foldr countPieces (0, 0, 0) (_ioPiece2FileMap ioCfg)
  putStrLn $ ((show (i,d,c) :: Text))
  threadDelay 1000000
  statusThread state
    where countPieces pi (i,d,c) = case _piState pi of
                                     Incomplete  -> (i + 1,d,    c)
                                     Downloading -> (i,    d + 1,c)
                                     Complete    -> (i,    d,    c + 1)
