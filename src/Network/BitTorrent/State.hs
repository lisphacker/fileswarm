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

data TrackerInfo = TrackerInfo { tiInfoHash :: ByteString
                               } deriving (Show)

data ClientInfo = ClientInfo { ciPeerId :: ByteString
                             } deriving (Show)

