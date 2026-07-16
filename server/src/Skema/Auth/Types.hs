{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Authentication types for Skema.
module Skema.Auth.Types
  ( -- * Types
    CredentialsRequest (..)
  , AuthResponse (..)
  , AuthError (..)
  ) where

import GHC.Generics ()
import Data.Aeson (FromJSON(..), ToJSON(..), Options(..), defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2)
import Data.OpenApi (ToSchema(..), genericDeclareNamedSchema)
import Skema.API.Types.Common (schemaOptions)
import Data.Time (UTCTime)

-- | Credentials request from client.
data CredentialsRequest = CredentialsRequest
  { credUsername :: Text
  , credPassword :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON CredentialsRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }

instance ToJSON CredentialsRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }

instance ToSchema CredentialsRequest where
  declareNamedSchema = genericDeclareNamedSchema (schemaOptions 4)

-- | Authentication response containing JWT.
data AuthResponse = AuthResponse
  { authJwt :: Text
    -- ^ JWT token for authentication
  , authExpiresAt :: UTCTime
    -- ^ When the token expires
  , authMessage :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON AuthResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }

instance ToJSON AuthResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }

instance ToSchema AuthResponse where
  declareNamedSchema = genericDeclareNamedSchema (schemaOptions 4)

-- | Authentication errors.
data AuthError
  = InvalidCredentials
  | AuthenticationDisabled
  deriving (Show, Eq)
