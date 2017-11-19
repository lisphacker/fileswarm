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

import Data.Text (unpack)
import Data.MetaInfo
import Network.HTTP.Types.URI (urlEncode)
import Network.HTTP.Simple

import Data.Crypto
import qualified Data.Bencoding as Benc
import Network.BitTorrent.State

data TrackerResponse = TrackerResponse
  deriving (Show)

type ErrorMsg = Text

buildTrackerResponse :: Benc.BencElement -> Either ErrorMsg TrackerResponse
buildTrackerResponse (Benc.BencDict trkRespDict) = error "Not implemented"
buildTrackerResponse _                           = Left "Tracker did not respond with a proper Bencoded dictionary"

queryTracker :: TorrentState -> MetaInfo -> IO (Either ErrorMsg TrackerResponse)
queryTracker state metainfo = do
  let announceURL = (unpack . miAnnounce) metainfo
  putStrLn $ "announce url = " ++ show announceURL
  request <- setRequestMethod "POST" <$>
             setRequestQueryString [("info_hash", Just ((urlEncode True . miInfoHash) metainfo)),
                                    ("peer_id", Just (urlEncode True ((cliPeerId . client) state))),
                                    ("port", Just $ show ((cliPort . client) state))] <$>
             parseRequest announceURL
  response <- getResponseBody <$> httpLBS request
  return $ Benc.decode response >>= buildTrackerResponse

