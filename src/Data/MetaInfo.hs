{-|
Module      : Data.MetaInfo
Description : Torrent metainfo
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

Module for managing torrent metainfo as well as serializing/deserializing..

-}

module Data.MetaInfo
  ( FileProp(..)
  , FileInfo(..)
  , Info(..)
  , MetaInfo(..)
  , decode ) where

import Protolude
import Data.Text (pack)
import qualified Data.Map as M
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Bencoding as Benc
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

type ErrorMsg = Text

type MD5Sum = ByteString
type Size = Int64

data FileProp = FileProp { length :: Size
                         , md5sum :: Maybe MD5Sum
                         , path   :: Maybe FilePath
                         } deriving (Show)

data FileInfo = SingleFileInfo { singleFileName :: Text
                               , singleFileProp :: FileProp
                               } 
              | MultiFileInfo { dirName :: Text
                              , files :: [FileProp]
                              } deriving (Show)

data Info = Info { pieceLength :: Int64
                 , pieces      :: ByteString
                 , private     :: Bool
                 , fileInfo    :: FileInfo
                 } deriving (Show)
                                 
                
data MetaInfo = MetaInfo { info         :: Info
                         , announce     :: Text
                         , announceList :: Maybe [[Text]]
                         , creationDate :: Maybe Text
                         , comment      :: Maybe Text
                         , createdBy    :: Maybe Text
                         , encoding     :: Maybe Text
                         } deriving (Show)

parseFileProp :: Benc.BencDict -> Either ErrorMsg FileProp
parseFileProp d = do
  len <- Benc.lookupInt "length" d
  let md5 = Benc.lookupStrOpt "" "md5sum" d
  return $ FileProp len (if md5 == "" then Nothing else Just md5) Nothing

parseSingleFileInfo :: Benc.BencDict -> Either ErrorMsg FileInfo
parseSingleFileInfo infoDict = do
  fileName <- decodeUtf8 <$> Benc.lookupStr "name" infoDict
  fileProp <- parseFileProp infoDict
  return $ SingleFileInfo fileName fileProp

parseMultiFileInfo :: Benc.BencDict -> Either ErrorMsg FileInfo
parseMultiFileInfo _ = Left "Not implemented"

parseInfo :: Benc.BencDict -> Either ErrorMsg Info
parseInfo infoDict = do
  pieceLen <- Benc.lookupInt "piece length" infoDict
  pcs <- Benc.lookupStr "pieces" infoDict
  let priv = Benc.lookupIntOpt 0 "private" infoDict
  fi <- case M.lookup "files" infoDict of
          Just _  -> parseMultiFileInfo infoDict
          Nothing -> parseSingleFileInfo infoDict

  return $ Info pieceLen "" {-pcs-} (priv /= 0) fi

parseAnnounceList :: Benc.BencElement -> [[Text]]
parseAnnounceList (Benc.BencList l) = map parse' l
  where parse' (Benc.BencList l') = map parse'' l'
        parse' _ = []
        parse'' (Benc.BencString s) = decodeUtf8 s
        parse'' _ = ""
parseAnnounceList _ = []

strTime :: Int64 -> Text
strTime = pack . formatTime defaultTimeLocale "%c" . posixSecondsToUTCTime . fromInteger . toInteger

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right a) = Just a
  
parseMetaInfo :: Benc.BencDict -> Either ErrorMsg MetaInfo
parseMetaInfo metaInfoDict = do
  infoDict <- Benc.lookupDict "info" metaInfoDict
  inf <- parseInfo infoDict
  annce <- decodeUtf8 <$> Benc.lookupStr "announce" metaInfoDict
  let annceList = case M.lookup "announce-list" metaInfoDict of
                       Just v  -> parseAnnounceList v
                       Nothing -> []
      crDate = eitherToMaybe $ strTime <$> Benc.lookupInt "creation date" metaInfoDict
      cmmnt = eitherToMaybe $ decodeUtf8 <$> Benc.lookupStr "comment" metaInfoDict
      crBy = eitherToMaybe $ decodeUtf8 <$> Benc.lookupStr "created by" metaInfoDict
      enc = eitherToMaybe $ decodeUtf8 <$> Benc.lookupStr "encoding" metaInfoDict
  return $ MetaInfo inf annce (Just annceList) crDate cmmnt crBy enc

decode :: Benc.BencElement -> Either ErrorMsg MetaInfo
decode (Benc.BencDict metaInfoDict) = parseMetaInfo metaInfoDict
decode _                            = Left "meta-info dictionary expected"