{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Network.BitTorrent.Types
Description : Data types
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

Data types

-}

module Network.BitTorrent.Types
  ( module X
  , Peer(..)
  , File(..)
  , fileHandle
  , fileLen
  , FileSection(..)
  , fsFile
  , fsOffset
  , fsLen
  , PieceState(..)
  , piSections
  , PieceInfo(..)
  , IOConfig(..)
  , TorrentState(..)
  , tsAnnounceURL
  , tsInfoHash
  , tsPeerId
  , tsTrackerId
  , tsPort
  , tsPeers
  , tsDownloaded
  , tsUploaded
  , tsLeft
  , tsIOConfig ) where

import Protolude
import Network.Socket
import Control.Concurrent.STM.TVar
import Control.Lens as X (view, set)
import Control.Lens hiding (element)

data Peer = Peer { _peerId   :: ByteString
                 , _peerAddr :: SockAddr
                 } deriving (Show)
makeLenses ''Peer
  
data File = File { _fileHandle :: Handle
                 , _fileLen    :: Int64
                 } deriving (Show)
makeLenses ''File

data FileSection = FileSection { _fsFile :: File
                               , _fsOffset :: Int64
                               , _fsLen    :: Int64
                               } deriving (Show)
makeLenses ''FileSection

data PieceState = Incomplete | Downloading | Complete
  deriving (Show)

data PieceInfo = PieceInfo { _piDownloaded :: Int64
                           , _piState      :: PieceState
                           , _piSections   :: [FileSection]
                           } deriving (Show)
makeLenses ''PieceInfo

data IOConfig = IOConfig { _ioFiles :: [File]
                         , _ioPiece2FileMap :: Map ByteString PieceInfo
                         } deriving (Show)
makeLenses ''IOConfig

data TorrentState = TorrentState { _tsAnnounceURL :: Text
                                 , _tsInfoHash    :: ByteString
                                 , _tsPieceSize   :: Int64
                                 , _tsPeerId      :: ByteString
                                 , _tsTrackerId   :: ByteString
                                 , _tsPort        :: Int
                                 , _tsPeers       :: TVar [Peer]
                                 , _tsPieceDir    :: Text
                                 , _tsUploaded    :: Int64
                                 , _tsDownloaded  :: Int64
                                 , _tsLeft        :: Int64
                                 , _tsIOConfig    :: TVar IOConfig
                                 } --deriving (Show)
makeLenses ''TorrentState
