{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Skema.Indexer.Types
  ( SearchQuery (..)
  , SearchResult (..)
  , IndexerResult (..)
  , IndexerError (..)
  , ReleaseInfo (..)
  , DownloadType (..)
  ) where

import Data.Time (UTCTime)
import qualified Skema.Domain.Quality as Quality

-- | Type of download (NZB or Torrent)
data DownloadType
  = NZB
  | Torrent
  deriving (Show, Eq, Generic)

-- | Search query parameters
data SearchQuery = SearchQuery
  { sqArtist :: Maybe Text
  , sqAlbum :: Maybe Text
  , sqYear :: Maybe Int
  , sqQuery :: Maybe Text  -- Generic query if artist/album not available
  , sqCategories :: [Int]  -- Newznab categories (3000=Audio, 3010=MP3, 3020=FLAC, etc.)
  , sqLimit :: Int
  , sqOffset :: Int
  } deriving (Show, Eq, Generic)

-- | Release information from indexer
data ReleaseInfo = ReleaseInfo
  { riTitle :: Text
  , riDownloadUrl :: Text
  , riInfoUrl :: Maybe Text
  , riSize :: Maybe Integer  -- bytes
  , riPublishDate :: Maybe UTCTime
  , riCategory :: Maybe Int
  , riSeeders :: Maybe Int  -- For torrents
  , riPeers :: Maybe Int    -- For torrents
  , riGrabs :: Maybe Int    -- Download count
  , riDownloadType :: DownloadType
  , riQuality :: Quality.Quality  -- Parsed quality from title
  , riProwlarrGuid :: Maybe Text  -- Prowlarr GUID for grab API
  , riProwlarrIndexerId :: Maybe Int  -- Prowlarr indexer ID for grab API
  } deriving (Show, Eq, Generic)

-- | Result from a single indexer
data IndexerResult = IndexerResult
  { irIndexerName :: Text
  , irReleases :: [ReleaseInfo]
  , irSearchTime :: Double  -- seconds
  } deriving (Show, Eq, Generic)

-- | Error from an indexer
data IndexerError = IndexerError
  { ieIndexerName :: Text
  , ieError :: Text
  } deriving (Show, Eq, Generic)

-- | Combined search result
data SearchResult = SearchResult
  { srResults :: [IndexerResult]
  , srErrors :: [IndexerError]
  , srTotalReleases :: Int
  } deriving (Show, Eq, Generic)
