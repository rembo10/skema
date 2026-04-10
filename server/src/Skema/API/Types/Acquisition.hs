{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Acquisition API types.
module Skema.API.Types.Acquisition
  ( AcquisitionAPI
  , CreateRuleRequest(..)
  , UpdateRuleRequest(..)
  , TrackedArtistResponse(..)
  , AcquisitionRuleResponse(..)
  , WantedAlbumResponse(..)
  , SourceStatsResponse(..)
  , AcquisitionSummaryResponse(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2)
import Data.OpenApi (ToSchema(..), genericDeclareNamedSchema)
import Skema.API.Types.Common (schemaOptions)
import Skema.API.Types.Tasks (TaskRequest, TaskResponse)
import GHC.Generics ()
import Servant

-- | Acquisition API endpoints.
type AcquisitionAPI = "acquisition" :> Header "Authorization" Text :>
  ( "sources" :> Get '[JSON] [AcquisitionRuleResponse]
  :<|> "sources" :> ReqBody '[JSON] CreateRuleRequest :> PostCreated '[JSON] AcquisitionRuleResponse
  :<|> "sources" :> Capture "sourceId" Int64 :> ReqBody '[JSON] UpdateRuleRequest :> Put '[JSON] AcquisitionRuleResponse
  :<|> "sources" :> Capture "sourceId" Int64 :> DeleteNoContent
  :<|> "tasks" :> ReqBody '[JSON] TaskRequest :> PostCreated '[JSON] TaskResponse
  :<|> "summary" :> Get '[JSON] AcquisitionSummaryResponse
  )

-- | Create acquisition rule request.
data CreateRuleRequest = CreateRuleRequest
  { createRuleName :: Text
  , createRuleDescription :: Maybe Text
  , createRuleType :: Text  -- "library_artists" | "metacritic" | "pitchfork"
  , createRuleEnabled :: Bool
  , createRuleFilters :: Maybe Text
  , createRuleQualityProfileId :: Maybe Int64
  } deriving (Show, Eq, Generic)

instance ToJSON CreateRuleRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 10 }

instance FromJSON CreateRuleRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 10 }

instance ToSchema CreateRuleRequest where
  declareNamedSchema = genericDeclareNamedSchema (schemaOptions 10)

-- | Update acquisition rule request.
data UpdateRuleRequest = UpdateRuleRequest
  { updateRuleName :: Text
  , updateRuleDescription :: Maybe Text
  , updateRuleType :: Text
  , updateRuleEnabled :: Bool
  , updateRuleFilters :: Maybe Text
  , updateRuleQualityProfileId :: Maybe Int64
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateRuleRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 10 }

instance FromJSON UpdateRuleRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 10 }

instance ToSchema UpdateRuleRequest where
  declareNamedSchema = genericDeclareNamedSchema (schemaOptions 10)

-- | Tracked artist response.
data TrackedArtistResponse = TrackedArtistResponse
  { trackedArtistResponseId :: Int64
  , trackedArtistResponseMBID :: Text
  , trackedArtistResponseName :: Text
  , trackedArtistResponseImageUrl :: Maybe Text
  , trackedArtistResponseThumbnailUrl :: Maybe Text
  , trackedArtistResponseAddedByRuleId :: Int64
  , trackedArtistResponseSourceClusterId :: Maybe Int64
  , trackedArtistResponseLastCheckedAt :: Maybe Text
  , trackedArtistResponseCreatedAt :: Text
  , trackedArtistResponseUpdatedAt :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON TrackedArtistResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 21 }

instance FromJSON TrackedArtistResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 21 }

instance ToSchema TrackedArtistResponse where
  declareNamedSchema = genericDeclareNamedSchema (schemaOptions 21)

-- | Acquisition rule response.
data AcquisitionRuleResponse = AcquisitionRuleResponse
  { acquisitionRuleResponseId :: Int64
  , acquisitionRuleResponseName :: Text
  , acquisitionRuleResponseDescription :: Maybe Text
  , acquisitionRuleResponseSourceType :: Text
  , acquisitionRuleResponseEnabled :: Bool
  , acquisitionRuleResponseFilters :: Maybe Text
  , acquisitionRuleResponseQualityProfileId :: Maybe Int64
  , acquisitionRuleResponseCreatedAt :: Text
  , acquisitionRuleResponseUpdatedAt :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON AcquisitionRuleResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 23 }

instance FromJSON AcquisitionRuleResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 23 }

instance ToSchema AcquisitionRuleResponse where
  declareNamedSchema = genericDeclareNamedSchema (schemaOptions 23)

-- | Wanted album response.
data WantedAlbumResponse = WantedAlbumResponse
  { wantedAlbumResponseId :: Int64
  , wantedAlbumResponseReleaseGroupMBID :: Text
  , wantedAlbumResponseTitle :: Text
  , wantedAlbumResponseArtistMBID :: Text
  , wantedAlbumResponseArtistName :: Text
  , wantedAlbumResponseStatus :: Text
  , wantedAlbumResponseAddedByRuleId :: Maybe Int64
  , wantedAlbumResponseFirstReleaseDate :: Maybe Text
  , wantedAlbumResponseMatchedClusterId :: Maybe Int64
  , wantedAlbumResponseCreatedAt :: Text
  , wantedAlbumResponseUpdatedAt :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON WantedAlbumResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 19 }

instance FromJSON WantedAlbumResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 19 }

instance ToSchema WantedAlbumResponse where
  declareNamedSchema = genericDeclareNamedSchema (schemaOptions 19)

-- | Per-source stats response.
data SourceStatsResponse = SourceStatsResponse
  { sourceStatsResponseSourceId :: Int64
  , sourceStatsResponseArtistCount :: Int
  , sourceStatsResponseAlbumCount :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON SourceStatsResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 19 }

instance FromJSON SourceStatsResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 19 }

instance ToSchema SourceStatsResponse where
  declareNamedSchema = genericDeclareNamedSchema (schemaOptions 19)

-- | Acquisition summary response.
data AcquisitionSummaryResponse = AcquisitionSummaryResponse
  { acquisitionSummaryResponseSources :: [SourceStatsResponse]
  , acquisitionSummaryResponseTotalArtistsFollowed :: Int
  , acquisitionSummaryResponseTotalAlbumsWanted :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON AcquisitionSummaryResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 26 }

instance FromJSON AcquisitionSummaryResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 26 }

instance ToSchema AcquisitionSummaryResponse where
  declareNamedSchema = genericDeclareNamedSchema (schemaOptions 26)
