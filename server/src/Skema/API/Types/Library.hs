{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Library API types.
module Skema.API.Types.Library
  ( LibraryAPI
  , StatsAPI
  , ScanResponse(..)
  , FileInfo(..)
  , LibraryStats(..)
  , UpdateTrackRequest(..)
  , TrackWithCluster(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2)
import GHC.Generics ()
import Servant
import Database.SQLite.Simple.FromRow (FromRow(..), field)

-- | Library management endpoints.
type LibraryAPI = "library" :> Header "Authorization" Text :>
  ( "scan" :> Post '[JSON] ScanResponse
  :<|> "files" :> Get '[JSON] [FileInfo]
  :<|> "tracks" :> Get '[JSON] [TrackWithCluster]
  :<|> "tracks" :> Capture "trackId" Int64 :> ReqBody '[JSON] UpdateTrackRequest :> Patch '[JSON] NoContent
  )

-- | Stats API endpoints.
type StatsAPI = "stats" :> Header "Authorization" Text :> Get '[JSON] LibraryStats

-- | Response for library scan.
data ScanResponse = ScanResponse
  { scanSuccess :: Bool
  , scanMessage :: Text
  , scanFilesAdded :: Int
  , scanFilesModified :: Int
  , scanFilesDeleted :: Int
  , scanIdentifyRun :: Bool
    -- ^ Whether identification was run
  , scanIdentifyTotalGroups :: Maybe Int
    -- ^ Total number of file groups (albums) processed
  , scanIdentifyMatchedGroups :: Maybe Int
    -- ^ Number of groups successfully matched to MusicBrainz
  , scanIdentifyFilesUpdated :: Maybe Int
    -- ^ Number of individual files updated with MusicBrainz IDs
  } deriving (Show, Eq, Generic)

instance ToJSON ScanResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }

instance FromJSON ScanResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }

-- | File information for API responses.
data FileInfo = FileInfo
  { fileinfoPath :: Text
  , fileinfoSize :: Integer
  , fileinfoTitle :: Maybe Text
  , fileinfoArtist :: Maybe Text
  , fileinfoAlbum :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON FileInfo where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 8 }

instance FromJSON FileInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 8 }

-- | Library statistics.
data LibraryStats = LibraryStats
  { statsTotalFiles :: Int
  , statsTotalAlbums :: Int
  , statsTotalArtists :: Int
  , statsMatchedFiles :: Int
  , statsUnmatchedFiles :: Int
  , statsMetadataAccuracy :: Double  -- 0-100 percentage
  , statsTotalDiffs :: Int
  , statsLibrarySize :: Integer  -- Total size in bytes
  , statsTotalRuntime :: Double  -- Total duration in seconds
  , statsLibraryPath :: Maybe Text  -- Library path
  } deriving (Show, Eq, Generic)

instance ToJSON LibraryStats where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 5 }

instance FromJSON LibraryStats where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 5 }

-- | Request to update track properties (e.g., move to different cluster).
data UpdateTrackRequest = UpdateTrackRequest
  { updateTrackClusterId :: Maybe Int64  -- Nothing = remove from cluster
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateTrackRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 11 }

instance FromJSON UpdateTrackRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 11 }

-- | Track with cluster and MusicBrainz information for frontend.
data TrackWithCluster = TrackWithCluster
  { trackId :: Int64
  , trackPath :: Text
  , trackTitle :: Maybe Text
  , trackArtist :: Maybe Text
  , trackTrackNumber :: Maybe Int
  , trackDiscNumber :: Maybe Int
  , trackDuration :: Maybe Double
  -- MusicBrainz recording match (track level)
  , trackMbRecordingId :: Maybe Text
  , trackMbRecordingTitle :: Maybe Text
  -- Cluster information
  , trackClusterId :: Maybe Int64
  , trackClusterAlbum :: Maybe Text
  , trackClusterAlbumArtist :: Maybe Text
  , trackClusterYear :: Maybe Int
  -- MusicBrainz release match (cluster level)
  , trackMbReleaseId :: Maybe Text
  , trackMbReleaseTitle :: Maybe Text
  , trackMbReleaseArtist :: Maybe Text
  , trackMbConfidence :: Maybe Double
  , trackMatchSource :: Maybe Text
  , trackMatchLocked :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON TrackWithCluster where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 5 }

instance FromJSON TrackWithCluster where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 5 }

instance FromRow TrackWithCluster where
  fromRow = TrackWithCluster
    <$> field -- id
    <*> field -- path
    <*> field -- title
    <*> field -- artist
    <*> field -- track_number
    <*> field -- disc_number
    <*> field -- duration
    <*> field -- mb_recording_id
    <*> field -- mb_recording_title
    <*> field -- cluster_id
    <*> field -- cluster_album
    <*> field -- cluster_album_artist
    <*> field -- cluster_year
    <*> field -- mb_release_id
    <*> field -- mb_release_title
    <*> field -- mb_release_artist
    <*> field -- mb_confidence
    <*> field -- match_source
    <*> field -- match_locked
