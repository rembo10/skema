{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Version API types.
module Skema.API.Types.Version
  ( VersionAPI
  , VersionResponse(..)
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.OpenApi (ToSchema(..), genericDeclareNamedSchema)
import Skema.API.Types.Common (schemaOptions)
import Servant

-- | Version endpoint - no auth required so anyone can check their version.
type VersionAPI = "version" :> Get '[JSON] VersionResponse

-- | Version information response.
data VersionResponse = VersionResponse
  { vrVersion :: Text
  , vrCommit :: Text
  , vrChannel :: Text
  , vrLatestVersion :: Maybe Text
  , vrUpdateAvailable :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON VersionResponse where
  toJSON vr = object
    [ "version" .= vrVersion vr
    , "commit" .= vrCommit vr
    , "channel" .= vrChannel vr
    , "latest_version" .= vrLatestVersion vr
    , "update_available" .= vrUpdateAvailable vr
    ]

instance ToSchema VersionResponse where
  declareNamedSchema = genericDeclareNamedSchema (schemaOptions 2)
