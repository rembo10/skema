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
  , CandidateRelease(..)
  , AssignReleaseRequest(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2)
import GHC.Generics ()
import Servant

-- | Clusters API endpoints.
type ClustersAPI = "clusters" :> Header "Authorization" Text :>
  ( Get '[JSON] [ClusterResponse]
  :<|> Capture "clusterId" Int64 :> Get '[JSON] ClusterWithTracksResponse
  :<|> Capture "clusterId" Int64 :> "candidates" :> Get '[JSON] [CandidateRelease]
  :<|> Capture "clusterId" Int64 :> "release" :> ReqBody '[JSON] AssignReleaseRequest :> Put '[JSON] ClusterResponse
  :<|> Capture "clusterId" Int64 :> "release" :> DeleteNoContent
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
  } deriving (Show, Eq, Generic)

instance ToJSON ClusterTrackInfo where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 12 }

instance FromJSON ClusterTrackInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 12 }

-- | Cluster response with track details.
data ClusterWithTracksResponse = ClusterWithTracksResponse
  { clusterWithTracksCluster :: ClusterResponse
  , clusterWithTracksTracks :: [ClusterTrackInfo]
  } deriving (Show, Eq, Generic)

instance ToJSON ClusterWithTracksResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 16 }

instance FromJSON ClusterWithTracksResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 16 }

-- | MusicBrainz candidate release.
data CandidateRelease = CandidateRelease
  { candidateReleaseId :: Text
  , candidateTitle :: Text
  , candidateArtist :: Text
  , candidateDate :: Maybe Text
  , candidateCountry :: Maybe Text
  , candidateTrackCount :: Int
  , candidateConfidence :: Double
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
