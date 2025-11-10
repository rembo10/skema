{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Common API types shared across endpoints.
module Skema.API.Types.Common
  ( ErrorResponse(..)
  , mkErrorResponse
  , SourceIO
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2)
import GHC.Generics ()
import Servant.Types.SourceT (SourceT)

-- | Type alias for SourceT IO.
type SourceIO = SourceT IO

-- | Error response for API errors.
data ErrorResponse = ErrorResponse
  { errorMessage :: Text
  , errorCode :: Maybe Text
    -- ^ Optional error code for programmatic error handling
  , errorDetails :: Maybe Text
    -- ^ Optional additional details about the error
  } deriving (Show, Eq, Generic)

instance ToJSON ErrorResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 5 }

instance FromJSON ErrorResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 5 }

-- | Helper to create a simple error response.
mkErrorResponse :: Text -> ErrorResponse
mkErrorResponse msg = ErrorResponse
  { errorMessage = msg
  , errorCode = Nothing
  , errorDetails = Nothing
  }
