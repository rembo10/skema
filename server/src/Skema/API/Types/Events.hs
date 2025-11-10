{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Server-Sent Events API types.
module Skema.API.Types.Events
  ( EventsAPI
  , ImageProxyAPI
  , ServerEvent(..)
  , EventRequest(..)
  , EventResponse(..)
  ) where

import Skema.API.Types.Common (SourceIO)
import Data.Aeson (ToJSON(..), FromJSON(..), Value, object, (.=), withObject, (.:), (.:?), encode, defaultOptions, fieldLabelModifier, camelTo2, genericToJSON, genericParseJSON)
import GHC.Generics ()
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE
import Servant hiding (ServerSentEvents, SourceIO)
import Servant.API.EventStream (ServerSentEvents, ToServerEvent(..))
import qualified Servant.API.EventStream as SSE

-- | Server-sent events API.
-- GET: Stream events via SSE (requires JWT as query parameter)
-- POST: Submit events to the event bus (requires Authorization header)
type EventsAPI =
  "events" :>
    ( QueryParam "token" Text :> ServerSentEvents (SourceIO ServerEvent)
    :<|> Header "Authorization" Text :> ReqBody '[JSON] EventRequest :> Post '[JSON] EventResponse
    )

-- | Image proxy API for caching and serving external images.
type ImageProxyAPI = "image-proxy" :> QueryParam' '[Required, Strict] "url" Text :> Get '[OctetStream] BSL.ByteString

-- | Server event types for SSE.
data ServerEvent = ServerEvent
  { eventType :: Text
  , eventData :: Value
  } deriving (Show, Eq, Generic)

instance ToJSON ServerEvent where
  toJSON (ServerEvent evtType evtData) = object
    [ "type" .= evtType
    , "data" .= evtData
    ]

instance FromJSON ServerEvent where
  parseJSON = withObject "ServerEvent" $ \o ->
    ServerEvent <$> o .: "type" <*> o .: "data"

-- | ToServerEvent instance for servant-event-stream.
instance ToServerEvent ServerEvent where
  toServerEvent (ServerEvent evtType evtData) =
    let jsonData = encode evtData  -- Already a lazy ByteString
        typeData = BSL.fromStrict $ TE.encodeUtf8 evtType  -- Convert to lazy ByteString
    in SSE.ServerEvent
         { SSE.eventType = Just typeData
         , SSE.eventId = Nothing
         , SSE.eventData = jsonData
         }

-- | Event request for client-submitted events (POST /api/events).
data EventRequest = EventRequest
  { eventRequestType :: Text
  , eventRequestData :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON EventRequest where
  toJSON (EventRequest evtType maybeData) = object $
    [ "type" .= evtType ] ++
    maybe [] (\d -> ["data" .= d]) maybeData

instance FromJSON EventRequest where
  parseJSON = withObject "EventRequest" $ \o ->
    EventRequest <$> o .: "type" <*> o .:? "data"

-- | Event response for submitted events.
data EventResponse = EventResponse
  { eventResponseSuccess :: Bool
  , eventResponseMessage :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON EventResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 13 }

instance FromJSON EventResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 13 }
