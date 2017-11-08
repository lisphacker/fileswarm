{-|
Module      : Network.BitTorrent.MetaInfo
Description : Torrent metainfo
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

Module for managing torrent metainfo as well as serializing/deserializing..

-}

module Network.BitTorrent.MetaInfo
  ( FileProp(..)
  , FileInfo(..)
  , Info(..)
  , MetaInfo(..)
  , decode ) where

import Protolude
--import Data.Maybe
--import Text.Show
--import Data.String
--import Data.Int
import Data.Map as M

import qualified Data.Bencoding as Benc

type MD5Sum = ByteString
type Size = Int64

data FileProp = FileProp { length :: Size
                         , md5sum :: MD5Sum
                         , path   :: Maybe FilePath
                         } deriving (Show)

data FileInfo = SingleFileInfo { singleFileName :: Text
                               , singleFileInfo :: FileInfo
                               } 
              | MultiFileInfo { dirName :: Text
                              , files :: [FileProp]
                              } deriving (Show)

data Info = Info { pieceLength :: Int
                 , pieces      :: ByteString
                 , private     :: Int
                 , fileInfo    :: FileInfo
                 } deriving (Show)
                                 
                
data MetaInfo = MetaInfo { info         :: Info
                         , announce     :: Text
                         , announceList :: Maybe [[Text]]
                         , creationData :: Maybe Text
                         , comment      :: Maybe Text
                         , createdBy    :: Maybe Text
                         , encoding     :: Maybe Text
                         } deriving (Show)


parseInfo :: Benc.BencDict -> Maybe Info
parseInfo m = Nothing

parseMetaInfo :: Benc.BencDict -> Maybe MetaInfo
parseMetaInfo metaInfoDict = case lookup "info" metaInfoDict of
                    Just (Benc.BencDict infoDict) -> case parseInfo infoDict of
                                                       Just info -> Just $ MetaInfo info "" Nothing Nothing Nothing Nothing Nothing
                                                       Nothing -> Nothing
                    _ -> Nothing


decode :: Benc.BencElement -> Maybe MetaInfo
decode (Benc.BencDict metaInfoDict) = parseMetaInfo metaInfoDict
decode _ = Nothing
