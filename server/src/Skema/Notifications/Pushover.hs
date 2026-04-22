{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Pushover notification provider.
--
-- Sends notifications via the Pushover API (https://pushover.net).
module Skema.Notifications.Pushover
  ( PushoverClient(..)
  , newPushoverClient
  ) where

import Skema.Notifications.Types (Notification(..), NotificationSender(..))
import Skema.Config.Types (PushoverConfig(..))
import Skema.HTTP.Client (HttpClient, postForm, prettyHttpError)
import Data.Aeson (decode, Value, withObject)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)

-- | Hardcoded Pushover application API token.
pushoverAppToken :: Text
pushoverAppToken = "a6k8np3n9ac8o1s9a125hfizrq5d3b"

-- | Pushover API client.
data PushoverClient = PushoverClient
  { pushoverConfig :: PushoverConfig
  , pushoverHttpClient :: HttpClient
  }

-- | Create a new Pushover client.
newPushoverClient :: PushoverConfig -> HttpClient -> PushoverClient
newPushoverClient = PushoverClient

instance NotificationSender PushoverClient where
  sendNotification PushoverClient{..} notification = do
    let params =
          [ ("token", pushoverAppToken)
          , ("user", pushoverUserKey pushoverConfig)
          , ("title", notificationTitle notification)
          , ("message", notificationMessage notification)
          , ("priority", show $ pushoverPriority pushoverConfig)
          , ("html", "1")
          ]
          <> maybe [] (\d -> [("device", d)]) (pushoverDevice pushoverConfig)
          <> maybe [] (\u -> [("url", u), ("url_title", "View on MusicBrainz")]) (notificationUrl notification)

    result <- postForm pushoverHttpClient "https://api.pushover.net/1/messages.json" params
    case result of
      Left err -> pure $ Left $ prettyHttpError err
      Right body ->
        case decode body :: Maybe Value of
          Just val ->
            case parseMaybe (withObject "response" (Aeson..: "status")) val of
              Just (1 :: Int) -> pure $ Right ()
              _ -> pure $ Left "Pushover API returned non-success status"
          Nothing -> pure $ Left "Failed to parse Pushover API response"
