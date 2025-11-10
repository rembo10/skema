{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Authentication types for Skema.
module Skema.Auth.Types
  ( -- * Types
    ApiKey (..)
  , CredentialsRequest (..)
  , AuthResponse (..)
  , AuthError (..)
    -- * Storage
  , AuthStore (..)
  , ApiKeyInfo (..)
  ) where

import GHC.Generics ()
import Data.Aeson (FromJSON(..), ToJSON(..), Options(..), defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime)
import Control.Concurrent.STM ()

-- | API key newtype wrapper.
newtype ApiKey = ApiKey { unApiKey :: Text }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ApiKey
instance ToJSON ApiKey

-- | Credentials request from client.
data CredentialsRequest = CredentialsRequest
  { credUsername :: Text
  , credPassword :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON CredentialsRequest where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }

instance ToJSON CredentialsRequest where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }

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

-- | Authentication errors.
data AuthError
  = InvalidCredentials
  | InvalidApiKey
  | AuthenticationDisabled
  deriving (Show, Eq)

-- | Thread-safe storage for API keys.
-- The internal structure is opaque to maintain encapsulation.
newtype AuthStore = AuthStore (TVar (Map.Map ApiKey ApiKeyInfo))

-- | Internal storage for API key validity (not exported).
data ApiKeyInfo = ApiKeyInfo
  { apiKeyValue :: ApiKey
  , apiKeyCreatedAt :: UTCTime
  , apiKeyExpiresAt :: Maybe UTCTime
  } deriving (Show, Eq)
