module Network.BitTorrent.MetaInfo where

import Protolude
--import Data.Maybe
--import Text.Show
--import Data.String
--import Data.Int

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
