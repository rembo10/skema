{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Authentication API types.
module Skema.API.Types.Auth
  ( AuthAPI
  , AuthStatusResponse(..)
  ) where

import Skema.Auth.Types (CredentialsRequest, AuthResponse)
import Data.Aeson (ToJSON(..), FromJSON(..), defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2)
import GHC.Generics ()
import Servant

-- | Authentication endpoints.
type AuthAPI = "auth" :>
  ( "status" :> Get '[JSON] AuthStatusResponse
  :<|> "credentials" :> ReqBody '[JSON] CredentialsRequest :> Post '[JSON] AuthResponse
  )

-- | Auth status response.
data AuthStatusResponse = AuthStatusResponse
  { authStatusEnabled :: Bool
    -- ^ Whether authentication is enabled
  } deriving (Show, Eq, Generic)

instance ToJSON AuthStatusResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 10 }

instance FromJSON AuthStatusResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 10 }
