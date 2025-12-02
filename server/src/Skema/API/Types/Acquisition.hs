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
  , SyncResponse(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2)
import GHC.Generics ()
import Servant

-- | Acquisition API endpoints.
type AcquisitionAPI = "acquisition" :> Header "Authorization" Text :>
  ( "sources" :> Get '[JSON] [AcquisitionRuleResponse]
  :<|> "sources" :> ReqBody '[JSON] CreateRuleRequest :> PostCreated '[JSON] AcquisitionRuleResponse
  :<|> "sources" :> Capture "sourceId" Int64 :> ReqBody '[JSON] UpdateRuleRequest :> Put '[JSON] AcquisitionRuleResponse
  :<|> "sources" :> Capture "sourceId" Int64 :> DeleteNoContent
  :<|> "sources" :> Capture "sourceId" Int64 :> "enable" :> Put '[JSON] NoContent
  :<|> "sources" :> Capture "sourceId" Int64 :> "disable" :> Put '[JSON] NoContent
  :<|> "sources" :> Capture "sourceId" Int64 :> "sync" :> Post '[JSON] SyncResponse
  )

-- | Sync response.
data SyncResponse = SyncResponse
  { syncResponseSuccess :: Bool
  , syncResponseMessage :: Text
  , syncResponseCount :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON SyncResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 12 }

instance FromJSON SyncResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 12 }

-- | Create acquisition rule request.
data CreateRuleRequest = CreateRuleRequest
  { createRuleName :: Text
  , createRuleDescription :: Maybe Text
  , createRuleType :: Text  -- "library_artists" | "metacritic" | "pitchfork"
  , createRuleEnabled :: Bool
  , createRuleFilters :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON CreateRuleRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 10 }

instance FromJSON CreateRuleRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 10 }

-- | Update acquisition rule request.
data UpdateRuleRequest = UpdateRuleRequest
  { updateRuleName :: Text
  , updateRuleDescription :: Maybe Text
  , updateRuleType :: Text
  , updateRuleEnabled :: Bool
  , updateRuleFilters :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateRuleRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 10 }

instance FromJSON UpdateRuleRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 10 }

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

-- | Acquisition rule response.
data AcquisitionRuleResponse = AcquisitionRuleResponse
  { acquisitionRuleResponseId :: Int64
  , acquisitionRuleResponseName :: Text
  , acquisitionRuleResponseDescription :: Maybe Text
  , acquisitionRuleResponseSourceType :: Text
  , acquisitionRuleResponseEnabled :: Bool
  , acquisitionRuleResponseFilters :: Maybe Text
  , acquisitionRuleResponseCreatedAt :: Text
  , acquisitionRuleResponseUpdatedAt :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON AcquisitionRuleResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 23 }

instance FromJSON AcquisitionRuleResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 23 }

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
