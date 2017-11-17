{-|
Module      : Data.Crypto
Description : Hash functions
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

Hash functions

-}

module Data.Crypto
  ( hashSHA1 ) where

import Protolude
import Crypto.Hash
import Data.ByteArray

hashSHA1 :: ByteString -> ByteString
hashSHA1 = convert . (hash :: ByteString -> Digest SHA1)
