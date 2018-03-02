{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Network.BitTorrent.Peer
Description : Peer operations
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

Peer operations

-}

module Network.BitTorrent.Peer where

import Protolude
import Control.Lens
import System.Random
import qualified Data.ByteString as BS

import Network.BitTorrent.Types
import Network.BitTorrent.Util

data PeerState = PeerState { _peerChoked       :: Bool
                           , _peerInterested   :: Bool
                           , _peerAmChoked     :: Bool
                           , _peerAmInterested :: Bool
                           } deriving (Show)
makeLenses ''PeerState

data PeerConnection = PeerConnection { _connPeer  :: Peer
                                     , _connState :: PeerState
                                     } deriving (Show)
makeLenses ''PeerConnection

newPeerConn :: Peer -> PeerConnection
newPeerConn peer = PeerConnection peer $ PeerState True False True False

data PWPMessage = KeepAlive
                | Choke
                | Unchoke
                | Interested
                | NotInterested
                | Have Int
                | Bitfield ByteString
                | Request Int Int Int
                | Piece Int Int ByteString
                | Cancel Int Int Int
                | Port Int
                deriving (Show)

pickRandomPeer :: [Peer] -> IO Peer
pickRandomPeer peers = do
  r <- randomIO
  let i = r `mod` (length peers)
  return $ el peers i
    where el (x:xs) 0 = x
          el (x:xs) i = el xs (i - 1)

readPWP :: Handle -> IO (Maybe PWPMessage)
readPWP h = do
  lenBytes <- BS.hGetNonBlocking h 4
  case lenBytes of
    empty -> return Nothing
    _     -> do
      let len = bytesToInt32 lenBytes
      case len of
        0 -> return $ Just KeepAlive
        _ -> do
          msgId <- BS.head <$> BS.hGet h 1
          case msgId of
            0 -> return $ Just Choke
            1 -> return $ Just Unchoke
            2 -> return $ Just Interested
            3 -> return $ Just NotInterested
            4 -> readHave h
            5 -> readBitfield h len
            6 -> readReq h
            7 -> readPiece h len
            8 -> readCancel h
            9 -> readPort h

  where readHave h = do
          bytes <- BS.hGet h 4
          return $ Just $ Have $ bytesToInt32 bytes
          
        readBitfield h len = do
          bytes <- BS.hGet h (len - 1)
          return $ Just $ Bitfield bytes
          
        readReq h = do
          index <- bytesToInt32 <$> BS.hGet h 4
          begin <- bytesToInt32 <$> BS.hGet h 4
          len   <- bytesToInt32 <$> BS.hGet h 4
          return $ Just $ Request index begin len
          
        readPiece h len = do
          index <- bytesToInt32 <$> BS.hGet h 4
          begin <- bytesToInt32 <$> BS.hGet h 4
          block <- BS.hGet h (len - 9)
          return $ Just $ Piece index begin block

        readCancel h = do
          index <- bytesToInt32 <$> BS.hGet h 4
          begin <- bytesToInt32 <$> BS.hGet h 4
          len   <- bytesToInt32 <$> BS.hGet h 4
          return $ Just $ Cancel index begin len

        readPort h = do
          port <- bytesToInt16 <$> BS.hGet h 2
          return $ Just $ Port port
