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

module Data.Bencoding
       ( BencElement(..)
       , BencDict
       , lookupStr
       , lookupStrOpt
       , lookupInt
       , lookupIntOpt
       , lookupDict
       , decode
       , encode ) where

import Protolude hiding (length)
import Data.ByteString (length)
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Builder
import Data.ByteString.Conversion (toByteString)
import Data.Attoparsec.ByteString.Char8 as P
import Data.Map.Strict (fromList, assocs, lookup)

import Data.Text (append)
import Data.Text.Encoding (decodeUtf8)

type ErrorMsg = Text
type BencDict = (Map ByteString BencElement)

-- | Bencoding element
data BencElement = BencString ByteString  -- ^ ByteString
                 | BencInt Int64          -- ^ Integer   
                 | BencList [BencElement] -- ^ List
                 | BencDict BencDict      -- ^ Dictionary
                 deriving (Show)

parseString :: Parser BencElement
parseString = do
  len <- P.decimal
  void $ P.char ':'
  BencString <$> P.take len

parseNumber :: Parser BencElement
parseNumber = BencInt <$> (P.char 'i' *> P.signed P.decimal <* P.char 'e')

parseList :: Parser BencElement
parseList = BencList <$> (P.char 'l' *> P.many' parseElement <* P.char 'e')

parseDictionary :: Parser BencElement
parseDictionary = do
  void $ P.char 'd'
  pairs <- P.many' ((,) <$> parseString <*> parseElement)
  void $ P.char 'e'
  return $ BencDict $ fromList $ fixPair <$> pairs
    where fixPair (BencString s, v) = (s, v)

parseElement :: Parser BencElement
parseElement = parseString <|> parseNumber <|> parseList <|> parseDictionary

lookupStr :: ByteString -> BencDict -> Either ErrorMsg ByteString
lookupStr k dict = case lookup k dict of
                      Just (BencString s) -> Right s
                      _                   -> Left $ append "Unable to find string for key " (decodeUtf8 k)

lookupStrOpt :: ByteString -> ByteString -> BencDict -> ByteString
lookupStrOpt def k dict = case lookup k dict of
                      Just (BencString s) -> s
                      _                   -> def

lookupInt :: ByteString -> BencDict -> Either ErrorMsg Int64
lookupInt k dict = case lookup k dict of
                      Just (BencInt i) -> Right i
                      _                -> Left $ append "Unable to find integer for key " (decodeUtf8 k)

lookupIntOpt :: Int64 -> ByteString -> BencDict -> Int64
lookupIntOpt def k dict = case lookup k dict of
                            Just (BencInt i) -> i
                            _                -> def

lookupDict :: ByteString -> BencDict -> Either ErrorMsg BencDict
lookupDict k dict = case lookup k dict of
                      Just (BencDict d) -> Right d
                      _                 -> Left $ append "Unable to find dictionary for key " (decodeUtf8 k)

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
