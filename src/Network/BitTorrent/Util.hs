{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Network.BitTorrent.Util
Description : Utility function
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

Peer client operations

-}

module Network.BitTorrent.Util where

import Protolude
import Data.ByteString

bytesToInt16 :: ByteString -> Int
bytesToInt16 bytes = let b0 = fromIntegral (index bytes 0) :: Int
                         b1 = fromIntegral (index bytes 1) :: Int
                     in (shift b1 8) .|. b0
                        
bytesToInt32 :: ByteString -> Int
bytesToInt32 bytes = let b0 = fromIntegral (index bytes 0) :: Int
                         b1 = fromIntegral (index bytes 1) :: Int
                         b2 = fromIntegral (index bytes 2) :: Int
                         b3 = fromIntegral (index bytes 3) :: Int
                     in (shift b3 24) .|. (shift b2 16) .|. (shift b1 8) .|. b0
