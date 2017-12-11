{-|
Module      : Network.BitTorrent.Tracker
Description : Bittorrent tracker API
Copyright   : (c) Gautham Ganapathy, 2017
License     : BSD
Maintainer  : gautham@lisphacker.org
Stability   : experimental
Portability : POSIX

API for querying a BitTorrent tracker

-}

module Network.BitTorrent.Tracker where

import Protolude
import Control.Lens (view)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Bits
import Network.HTTP.Types.URI (urlEncode)
import Network.HTTP.Simple
import Network.Socket
import Control.Concurrent.STM.TVar

import Data.MetaInfo
import Data.Crypto
import qualified Data.Bencoding as Benc
import Network.BitTorrent.State

import qualified Data.ByteString as BS

data TrackerResponse = TrackerResponse { trWarning    :: Text
                                       , trInterval   :: Int
                                       , trTrackedId  :: ByteString
                                       , trComplete   :: Int
                                       , trIncomplete :: Int
                                       , trPeers      :: [Peer]
                                       } deriving (Show)

type ErrorMsg = Text

parseTrackerResponse :: Benc.BencDict -> TrackerResponse
parseTrackerResponse d = let warn = decodeUtf8 $ Benc.lookupStrOpt "" "warning message" d
                             interval = fromIntegral $ Benc.lookupIntOpt 30 "interval" d
                             trackerid = Benc.lookupStrOpt "" "tracker id" d
                             complete = fromIntegral $ Benc.lookupIntOpt 0 "complete" d
                             incomplete = fromIntegral $ Benc.lookupIntOpt 0 "incomplete" d
                             peers = case Benc.lookupVal "peers" d of
                                       Left  _ -> []
                                       Right v -> parsePeers v
                         in TrackerResponse warn interval trackerid complete incomplete peers
  where parsePeers (Benc.BencList peerDicts) = map parsePeerDict peerDicts
        parsePeers (Benc.BencString peers)   = parsePeers' peers where
          parsePeers' peers = if BS.length peers > 6
                              then
                                 let (p,s) = BS.splitAt 6 peers
                                 in (bytesToSockAddr p):(parsePeers' s)
                              else
                                []
          bytesToSockAddr bs = let b0 = BS.index bs 0
                                   b1 = BS.index bs 1
                                   b2 = BS.index bs 2
                                   b3 = BS.index bs 3
                                   b4 = BS.index bs 4
                                   b5 = BS.index bs 5
                                   w4 = (fromIntegral b4 :: Word16)
                                   w5 = (fromIntegral b5 :: Word16)
                                   hostAddr = tupleToHostAddress (b0, b1, b2, b3)
                                   port = PortNum $ (shift w4 8) .|. w5
                               in Peer "" (SockAddrInet port hostAddr)
        parsePeers _                         = []
        parsePeerDict d                      = Peer "" (SockAddrInet 0 0)

buildTrackerResponse :: Benc.BencElement -> Either ErrorMsg TrackerResponse
buildTrackerResponse (Benc.BencDict trkRespDict) = parse trkRespDict
  where parse d = case Benc.lookupStr "failure reason" d of
                    Left e  -> Right $ parseTrackerResponse d
                    Right v -> Left $ decodeUtf8 v
buildTrackerResponse _                           = Left "Tracker did not respond with a proper Bencoded dictionary"

queryTracker :: TorrentState -> IO (Either ErrorMsg TrackerResponse)
queryTracker state = do
  request <- setRequestMethod "GET" <$>
             setRequestQueryString [("info_hash", Just $ view tsInfoHash state),
                                    ("peer_id", Just $ view tsPeerId state),
                                    ("port", Just $ show $ view tsPort state),
                                    ("uploaded", Just $ show $ view tsUploaded state),
                                    ("downloaded", Just $ show $ view tsDownloaded state),
                                    ("left", Just $ show $ view tsLeft state),
                                    ("compact", Just "1"),
                                    ("event", Just "started"){-,
                                    ("trackerid", Just $ tsPeerId state)-}] <$>
             parseRequest ((unpack . view tsAnnounceURL) state)
  response <- Benc.bsLazyToStrictBS <$> getResponseBody <$> httpLBS request
  return $ Benc.decode response >>= buildTrackerResponse

updateTorrentStateFromTracker :: TorrentState -> IO (TorrentState)
updateTorrentStateFromTracker ts = do
  etr <- queryTracker ts
  case etr of
    Left _   -> return ts
    Right tr -> do atomically $ writeTVar (view tsPeers ts) (trPeers tr)
                   return ts
