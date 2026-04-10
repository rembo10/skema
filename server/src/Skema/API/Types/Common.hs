{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Common API types shared across endpoints.
module Skema.API.Types.Common
  ( ErrorResponse(..)
  , mkErrorResponse
  , SourceIO
  , RawHtml
  , schemaOptions
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value, defaultOptions, genericToJSON, genericParseJSON, fieldLabelModifier, camelTo2)
import Data.OpenApi (ToSchema(..), NamedSchema(..), genericDeclareNamedSchema, SchemaOptions, fromAesonOptions)
import GHC.Generics ()
import Network.HTTP.Media ((//))
import Servant (Accept(..), MimeRender(..))
import Servant.Types.SourceT (SourceT)

-- | HTML content type for serving raw HTML.
data RawHtml

instance Accept RawHtml where
  contentType _ = "text" // "html"

instance MimeRender RawHtml ByteString where
  mimeRender _ = fromStrict

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

instance ToSchema ErrorResponse where
  declareNamedSchema = genericDeclareNamedSchema (schemaOptions 5)

-- | Create OpenAPI schema options matching our Aeson convention.
schemaOptions :: Int -> SchemaOptions
schemaOptions n = fromAesonOptions defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop n }

-- | Orphan instance for Aeson Value (arbitrary JSON).
instance ToSchema Value where
  declareNamedSchema _ = pure $ NamedSchema (Just "Value") mempty

-- | Helper to create a simple error response.
mkErrorResponse :: Text -> ErrorResponse
mkErrorResponse msg = ErrorResponse
  { errorMessage = msg
  , errorCode = Nothing
  , errorDetails = Nothing
  }
