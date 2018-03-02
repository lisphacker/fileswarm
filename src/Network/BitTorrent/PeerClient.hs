{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Network.BitTorrent.PeerClient
Description : Peer client operations
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

Peer client operations

-}

module Network.BitTorrent.PeerClient where

import Protolude hiding (head, length)
import Data.Maybe
import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import Control.Lens
import Network.Socket
import Data.ByteString (head, hGet, pack)
import Data.ByteString.Builder
import Data.Text hiding (head, pack)
import Data.Text.Lazy.Builder

import System.IO (hClose, hFlush, stdout)

import Network.BitTorrent.Types
import Network.BitTorrent.FileIO
import Network.BitTorrent.Peer

peerClientThread :: ByteString -> TorrentState -> PieceIORequestChannel -> IO ()
peerClientThread hash state pioReqChan = do
  pioResChan <- newTQueueIO
  
  fetchPieceFromRandomPeer hash state (pioReqChan, pioResChan)

  hFlush stdout
  --setPieceState pioReqChan pioResChan hash Downloading
  

fetchPieceFromRandomPeer :: ByteString -> TorrentState -> PieceIOChannelPair -> IO ()
fetchPieceFromRandomPeer hash state (pioReqChan, pioResChan) = do
  peers <- readTVarIO $ state ^. tsPeers
  peer <- pickRandomPeer peers
  putStrLn $ ((show peer) :: Text)

  h <- connectToPeer $ peer ^. peerAddr

  putStrLn ("Connected to peer" :: Text)

  sendHandshake h "BitTorrent protocol" (state ^. tsInfoHash) (state ^. tsPeerId)
  recvPeerId <- recvHandshake h

  if recvPeerId == (peer ^. peerId)
    then processPeer hash state (pioReqChan, pioResChan) peer h
    else do
      hClose h
      putStrLn ("Mismatch" :: Text)
      fetchPieceFromRandomPeer hash state (pioReqChan, pioResChan)
  
processPeer :: ByteString -> TorrentState -> PieceIOChannelPair -> Peer -> Handle -> IO ()
processPeer hash state (pioReqChan, pioResChan) peer h = do
  putStrLn ("processPeer" :: Text)
  pwp <- readPWP h
  putStrLn $ ((show pwp) :: Text)
  return ()
  
connectToPeer :: SockAddr -> IO (Handle)
connectToPeer addr@(SockAddrInet _ _) = do
  s <- socket AF_INET Stream defaultProtocol
  connect s addr
  h <- socketToHandle s ReadWriteMode
  return h
connectToPeer addr@(SockAddrInet6 _ _ _ _) = do
  s <- socket AF_INET6 Stream defaultProtocol
  putStrLn ("Created socket" :: Text)
  connect s addr
  putStrLn ("Connected socket" :: Text)
  h <- socketToHandle s ReadWriteMode
  return h

sendHandshake :: Handle -> Text -> ByteString -> ByteString -> IO ()
sendHandshake h name infoHash peerId = do
  let l = length name
  putStrLn ("Sending handshake" :: Text)
  hPutBuilder h $ int8 (fromIntegral l)
  hPutBuilder h $ byteString $ encodeUtf8 name
  hPutBuilder h $ byteString $ pack [0, 0, 0, 0, 0, 0, 0, 0]
  hPutBuilder h $ byteString infoHash
  hPutBuilder h $ byteString peerId
  putStrLn ("Sent handshake" :: Text)
  return ()
  
recvHandshake :: Handle -> IO (ByteString)
recvHandshake h = do
  putStrLn ("Receiving handshake" :: Text)
  lenStr <- hGet h 1
  let nameLen = fromIntegral $ head lenStr :: Int
  name <- decodeUtf8 <$> hGet h nameLen
  void $ hGet h 8
  infoHash <- hGet h 20
  peerId <- hGet h 20
  return (peerId)
