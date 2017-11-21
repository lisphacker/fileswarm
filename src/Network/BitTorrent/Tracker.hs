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

queryTracker :: TorrentState -> IO (Either ErrorMsg TrackerResponse)
queryTracker state = do
  request <- setRequestMethod "POST" <$>
             setRequestQueryString [("info_hash", Just $ urlEncode True $ tsInfoHash state),
                                    ("peer_id", Just $ urlEncode True $ tsPeerId state),
                                    ("port", Just $ show $ tsPort state),
                                    ("uploaded", Just $ show $ tsUploaded state),
                                    ("downloaded", Just $ show $ tsDownloaded state),
                                    ("left", Just $ show $ tsLeft state),
                                    ("compact", Just "1"),
                                    ("event", Just "started"),
                                    ("trackerid", Just $ tsPeerId state)] <$>
             parseRequest ((unpack . tsAnnounceURL) state)
  response <- Benc.bsLazyToStrictBS <$> getResponseBody <$> httpLBS request
  return $ Benc.decode response >>= buildTrackerResponse

