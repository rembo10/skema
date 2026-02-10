{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skema.Indexer.Types
  ( SearchQuery (..)
  , SearchResult (..)
  , IndexerResult (..)
  , IndexerError (..)
  , ReleaseInfo (..)
  , DownloadType (..)
  , SlskdFile (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=), (.!=))
import Data.Time (UTCTime)
import qualified Skema.Domain.Quality as Quality

-- | Type of download (NZB, Torrent, or Slskd)
data DownloadType
  = NZB
  | Torrent
  | Slskd
  deriving (Show, Eq, Generic)

-- | File information for slskd downloads.
-- Re-exported from Slskd.Types for convenience.
data SlskdFile = SlskdFile
  { sfFilename :: Text
    -- ^ Full path including filename
  , sfSize :: Integer
    -- ^ File size in bytes
  , sfBitRate :: Maybe Int
    -- ^ Audio bitrate (for MP3/lossy formats)
  , sfSampleRate :: Maybe Int
    -- ^ Sample rate in Hz
  , sfBitDepth :: Maybe Int
    -- ^ Bit depth (e.g., 16, 24)
  , sfLength :: Maybe Int
    -- ^ Duration in seconds
  , sfIsLocked :: Bool
    -- ^ Whether file is locked (non-downloadable)
  } deriving (Show, Eq, Generic)

instance FromJSON SlskdFile where
  parseJSON = withObject "SlskdFile" $ \o -> do
    sfFilename <- o .: "filename"
    sfSize <- o .: "size"
    sfBitRate <- o .:? "bitRate"
    sfSampleRate <- o .:? "sampleRate"
    sfBitDepth <- o .:? "bitDepth"
    sfLength <- o .:? "length"
    sfIsLocked <- o .:? "isLocked" .!= False
    pure SlskdFile {..}

instance ToJSON SlskdFile where
  toJSON sf = object
    [ "filename" .= sfFilename sf
    , "size" .= sfSize sf
    , "bitRate" .= sfBitRate sf
    , "sampleRate" .= sfSampleRate sf
    , "bitDepth" .= sfBitDepth sf
    , "length" .= sfLength sf
    , "isLocked" .= sfIsLocked sf
    ]

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
  , riGuid :: Maybe Text    -- RSS GUID for tracking
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
  , riSlskdUsername :: Maybe Text  -- For slskd downloads: username
  , riSlskdFiles :: Maybe [SlskdFile]  -- For slskd downloads: files to queue
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
