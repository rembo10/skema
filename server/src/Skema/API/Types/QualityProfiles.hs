{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Quality profiles API types.
module Skema.API.Types.QualityProfiles
  ( QualityProfilesAPI
  , CreateQualityProfileRequest(..)
  , UpdateQualityProfileRequest(..)
  ) where

import Skema.Domain.Quality (QualityProfile, QualityPreference, Quality)
import Data.Aeson (ToJSON(..), FromJSON(..), defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2)
import GHC.Generics ()
import Servant

-- | Quality profiles API endpoints.
type QualityProfilesAPI = "quality-profiles" :> Header "Authorization" Text :>
  ( Get '[JSON] [QualityProfile]
  :<|> Capture "profileId" Int64 :> Get '[JSON] QualityProfile
  :<|> ReqBody '[JSON] CreateQualityProfileRequest :> PostCreated '[JSON] QualityProfile
  :<|> Capture "profileId" Int64 :> ReqBody '[JSON] UpdateQualityProfileRequest :> Put '[JSON] QualityProfile
  :<|> Capture "profileId" Int64 :> DeleteNoContent
  :<|> "default" :> Get '[JSON] (Maybe QualityProfile)
  :<|> "default" :> Capture "profileId" Int64 :> Put '[JSON] NoContent
  )

-- | Request to create a new quality profile.
data CreateQualityProfileRequest = CreateQualityProfileRequest
  { createQualityProfileName :: Text
  , createQualityProfileQualityPreferences :: [QualityPreference]
  , createQualityProfileCutoffQuality :: Quality
  , createQualityProfileUpgradeAutomatically :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON CreateQualityProfileRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 20 }

instance FromJSON CreateQualityProfileRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 20 }

-- | Request to update a quality profile.
data UpdateQualityProfileRequest = UpdateQualityProfileRequest
  { updateQualityProfileName :: Text
  , updateQualityProfileQualityPreferences :: [QualityPreference]
  , updateQualityProfileCutoffQuality :: Quality
  , updateQualityProfileUpgradeAutomatically :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateQualityProfileRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 20 }

instance FromJSON UpdateQualityProfileRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 20 }
