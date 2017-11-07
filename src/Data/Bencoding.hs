{-|
Module      : Data.Bencoding
Description : Bencoding format encoder / decoder
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

Module for encoding and decoding data in BitTorrent's Bencoding format.

-}
{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module Data.Bencoding
       ( BencElement(..)
       , decode
       , encode ) where

import Protolude hiding (length)
import Data.ByteString (length)
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Builder
import Data.ByteString.Conversion (toByteString)
import Data.Attoparsec.ByteString.Char8 as P
import Data.Map.Strict (fromList, assocs)

-- | Bencoding element
data BencElement = BencString ByteString                  -- ^ ByteString
                 | BencInt Int64                          -- ^ Integer   
                 | BencList [BencElement]                 -- ^ List
                 | BencDict (Map ByteString BencElement)  -- ^ Dictionary
                 deriving (Show)

parseString :: Parser BencElement
parseString = do
  len <- P.decimal
  void $ P.char ':'
  BencString <$> P.take len

parseNumber :: Parser BencElement
parseNumber = do
  BencInt <$> (P.char 'i' *> P.signed P.decimal <* P.char 'e')

parseList :: Parser BencElement
parseList = do
  BencList <$> (P.char 'l' *> P.many' parseElement <* P.char 'e')

parseDictionary :: Parser BencElement
parseDictionary = do
  void $ P.char 'd'
  pairs <- P.many' ((,) <$> parseString <*> parseElement)
  void $ P.char 'e'
  return $ BencDict $ fromList $ fixPair <$> pairs
    where fixPair (BencString s, v) = (s, v)

parseElement :: Parser BencElement
parseElement = parseString <|> parseNumber <|> parseList <|> parseDictionary

-- | Decode a byte-string into a BencElement
decode :: ByteString -> Maybe BencElement
decode bytes = case P.parseOnly parseElement bytes of
  Left _    -> Nothing
  Right val -> Just val

-- | Encode a BencElement into a byte string
encode :: BencElement -> ByteString
encode = LB.toStrict . toByteString . encode'
  where encode' (BencString s) = mconcat [intDec (length s), char7 ':', byteString s]
        encode' (BencInt i)    = mconcat [char7 'i', int64Dec i, char7 'e']
        encode' (BencList l)   = mconcat [char7 'l', mconcat (map encode' l), char7 'e']
        encode' (BencDict d)   = mconcat [char7 'd', mconcat (map encodePair (assocs d)), char7 'e']
        encodePair (k, v)      = mappend (encode' (BencString k)) (encode' v)
