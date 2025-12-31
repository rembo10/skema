{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Clusters API types.
module Skema.API.Types.Clusters
  ( ClustersAPI
  , ClusterResponse(..)
  , ClusterWithTracksResponse(..)
  , ClusterTrackInfo(..)
  , MBTrackInfo(..)
  , CandidateRelease(..)
  , AssignReleaseRequest(..)
  , UpdateTrackRecordingRequest(..)
  , CreateClusterRequest(..)
  , ClusterTaskRequest(..)
  ) where

import Skema.API.Types.Tasks (TaskResponse)
import Data.Aeson (ToJSON(..), FromJSON(..), defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2)
import GHC.Generics ()
import Servant

-- | Clusters API endpoints.
type ClustersAPI = "clusters" :> Header "Authorization" Text :>
  ( "tasks" :> ReqBody '[JSON] ClusterTaskRequest :> PostCreated '[JSON] TaskResponse
  :<|> Get '[JSON] [ClusterResponse]
  :<|> Capture "clusterId" Int64 :> Get '[JSON] ClusterWithTracksResponse
  :<|> Capture "clusterId" Int64 :> "candidates" :> Get '[JSON] [CandidateRelease]
  :<|> Capture "clusterId" Int64 :> "release" :> ReqBody '[JSON] AssignReleaseRequest :> Put '[JSON] ClusterResponse
  :<|> Capture "clusterId" Int64 :> "release" :> DeleteNoContent
  -- Track recording mapping endpoint
  :<|> Capture "clusterId" Int64 :> "tracks" :> Capture "trackId" Int64 :> "recording" :> ReqBody '[JSON] UpdateTrackRecordingRequest :> Put '[JSON] NoContent
  -- Search for releases (for manual matching)
  :<|> "search-releases" :> QueryParam' '[Required, Strict] "query" Text :> QueryParam "limit" Int :> Get '[JSON] [CandidateRelease]
  -- Search for recordings (for manual track matching)
  :<|> "search-recordings" :> QueryParam' '[Required, Strict] "query" Text :> QueryParam "limit" Int :> Get '[JSON] [MBTrackInfo]
  -- Cluster manipulation endpoints
  :<|> ReqBody '[JSON] CreateClusterRequest :> Post '[JSON] ClusterResponse
  :<|> Capture "clusterId" Int64 :> QueryParam "merge_into" Int64 :> DeleteNoContent
  )

-- | Cluster response (without track details).
data ClusterResponse = ClusterResponse
  { clusterResponseId :: Int64
  , clusterResponseMetadataHash :: Text
  , clusterResponseAlbum :: Maybe Text
  , clusterResponseAlbumArtist :: Maybe Text
  , clusterResponseTrackCount :: Int
  , clusterResponseMBReleaseId :: Maybe Text
  , clusterResponseMBReleaseGroupId :: Maybe Text
  , clusterResponseMBConfidence :: Maybe Double
  , clusterResponseCreatedAt :: Text
  , clusterResponseUpdatedAt :: Text
  , clusterResponseLastIdentifiedAt :: Maybe Text
  -- Match provenance fields
  , clusterResponseMatchSource :: Maybe Text  -- "auto_fingerprint", "auto_metadata", or "manual"
  , clusterResponseMatchLocked :: Bool  -- True if manually assigned and locked from re-matching
  -- Cluster metadata (from first track, used for identification)
  , clusterResponseLabel :: Maybe Text
  , clusterResponseCatalogNumber :: Maybe Text
  , clusterResponseBarcode :: Maybe Text
  , clusterResponseCountry :: Maybe Text
  , clusterResponseDate :: Maybe Text
  , clusterResponseYear :: Maybe Int
  -- Matched release details from MusicBrainz
  , clusterResponseMBReleaseTitle :: Maybe Text
  , clusterResponseMBReleaseArtist :: Maybe Text
  , clusterResponseMBReleaseDate :: Maybe Text
  , clusterResponseMBReleaseCountry :: Maybe Text
  , clusterResponseMBReleaseLabel :: Maybe Text
  , clusterResponseMBReleaseCatalogNumber :: Maybe Text
  , clusterResponseMBReleaseBarcode :: Maybe Text
  -- Alternative match candidates (for user selection)
  , clusterResponseMBCandidates :: Maybe Text  -- JSON array of MBRelease objects
  } deriving (Show, Eq, Generic)

instance ToJSON ClusterResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 15 }

instance FromJSON ClusterResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 15 }

-- | Track info for cluster detail view.
data ClusterTrackInfo = ClusterTrackInfo
  { clusterTrackId :: Int64
  , clusterTrackPath :: Text
  , clusterTrackTitle :: Maybe Text
  , clusterTrackArtist :: Maybe Text
  , clusterTrackTrackNumber :: Maybe Int
  , clusterTrackDiscNumber :: Maybe Int
  , clusterTrackDuration :: Maybe Double
  -- MusicBrainz recording mapping (from track matching)
  , clusterTrackMBRecordingId :: Maybe Text
  , clusterTrackMBRecordingTitle :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON ClusterTrackInfo where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 12 }

instance FromJSON ClusterTrackInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 12 }

-- | MusicBrainz track info from cached release data.
data MBTrackInfo = MBTrackInfo
  { mbTrackInfoPosition :: Int
  , mbTrackInfoTitle :: Text
  , mbTrackInfoLength :: Maybe Int
  , mbTrackInfoRecordingId :: Text
  , mbTrackInfoArtist :: Maybe Text
  , mbTrackInfoDiscNumber :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON MBTrackInfo where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 11 }

instance FromJSON MBTrackInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 11 }

-- | Cluster response with track details.
data ClusterWithTracksResponse = ClusterWithTracksResponse
  { clusterWithTracksCluster :: ClusterResponse
  , clusterWithTracksTracks :: [ClusterTrackInfo]
  , clusterWithTracksMBTracks :: Maybe [MBTrackInfo]
  } deriving (Show, Eq, Generic)

instance ToJSON ClusterWithTracksResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 17 }

instance FromJSON ClusterWithTracksResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 17 }

-- | MusicBrainz candidate release.
data CandidateRelease = CandidateRelease
  { candidateReleaseId :: Text
  , candidateTitle :: Text
  , candidateArtist :: Text
  , candidateDate :: Maybe Text
  , candidateCountry :: Maybe Text
  , candidateTrackCount :: Int
  , candidateConfidence :: Double
  -- Additional identifying information
  , candidateBarcode :: Maybe Text
  , candidateLabel :: Maybe Text
  , candidateCatalogNumber :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON CandidateRelease where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 9 }

instance FromJSON CandidateRelease where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 9 }

-- | Request to assign a release to a cluster.
data AssignReleaseRequest = AssignReleaseRequest
  { assignReleaseId :: Text
  , assignReleaseGroupId :: Maybe Text
  , assignConfidence :: Maybe Double
  } deriving (Show, Eq, Generic)

instance ToJSON AssignReleaseRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 6 }

instance FromJSON AssignReleaseRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 6 }

-- | Request to update a track's recording mapping.
data UpdateTrackRecordingRequest = UpdateTrackRecordingRequest
  { updateRecordingId :: Text
  , updateRecordingTitle :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateTrackRecordingRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 6 }

instance FromJSON UpdateTrackRecordingRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 6 }

-- | Request to create a new cluster from tracks.
data CreateClusterRequest = CreateClusterRequest
  { createClusterTrackIds :: [Int64]
  , createClusterAlbum :: Maybe Text
  , createClusterAlbumArtist :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON CreateClusterRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 13 }

instance FromJSON CreateClusterRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 13 }

-- | Request to create a cluster task.
data ClusterTaskRequest = ClusterTaskRequest
  { clusterTaskType :: Text
    -- ^ Task type: "identify"
  , clusterTaskClusterId :: Int64
    -- ^ Cluster ID to operate on
  } deriving (Show, Eq, Generic)

instance ToJSON ClusterTaskRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 11 }

instance FromJSON ClusterTaskRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 11 }
