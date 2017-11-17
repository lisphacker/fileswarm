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

module Network.BitTorrent.State where

import Protolude

data Tracker = Tracker { trkInfoHash :: ByteString
                       } deriving (Show)

data Client = Client { cliPeerId :: ByteString
                     , cliPort   :: Int
                     } deriving (Show)


data State = { tracker :: Tracker
             , client  :: Client
             } deriving (Show)
