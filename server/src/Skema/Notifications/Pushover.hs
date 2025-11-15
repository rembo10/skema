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
import Network.HTTP.Client (Manager, Request, parseRequest, httpLbs, responseBody, responseStatus, urlEncodedBody)
import Network.HTTP.Types.Status (statusCode)
import Data.Aeson (decode, Value, Object, withObject)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS

-- | Hardcoded Pushover application API token.
pushoverAppToken :: Text
pushoverAppToken = "a6k8np3n9ac8o1s9a125hfizrq5d3b"

-- | Pushover API client.
data PushoverClient = PushoverClient
  { pushoverConfig :: PushoverConfig
  , pushoverHttpManager :: Manager
  }

-- | Create a new Pushover client.
newPushoverClient :: PushoverConfig -> Manager -> PushoverClient
newPushoverClient = PushoverClient

instance NotificationSender PushoverClient where
  sendNotification client notification = do
    let config = pushoverConfig client
    let manager = pushoverHttpManager client

    -- Build the request
    baseReq <- parseRequest "https://api.pushover.net/1/messages.json"

    let params =
          [ ("token", TE.encodeUtf8 pushoverAppToken)
          , ("user", TE.encodeUtf8 $ pushoverUserKey config)
          , ("title", TE.encodeUtf8 $ notificationTitle notification)
          , ("message", TE.encodeUtf8 $ notificationMessage notification)
          , ("priority", TE.encodeUtf8 $ show $ pushoverPriority config)
          , ("html", "1")  -- Enable HTML formatting
          ]
          <> maybe [] (\d -> [("device", TE.encodeUtf8 d)]) (pushoverDevice config)
          <> maybe [] (\u -> [("url", TE.encodeUtf8 u), ("url_title", "View on MusicBrainz")]) (notificationUrl notification)

    let req = urlEncodedBody params baseReq

    -- Send the request
    resp <- httpLbs req manager

    let status = statusCode $ responseStatus resp
    let body = responseBody resp

    -- Check response
    if status == 200
      then do
        -- Parse response to verify success
        case decode body :: Maybe Value of
          Just val ->
            case parseMaybe (withObject "response" (Aeson..: "status")) val of
              Just (1 :: Int) -> pure $ Right ()
              _ -> pure $ Left "Pushover API returned non-success status"
          Nothing -> pure $ Left "Failed to parse Pushover API response"
      else do
        -- Try to extract error message from response
        let errorMsg = case decode body :: Maybe Value of
              Just val ->
                case parseMaybe (withObject "errors" (Aeson..: "errors")) val of
                  Just (errs :: [Text]) -> "Pushover error: " <> show errs
                  Nothing -> "Pushover API returned status " <> show status
              Nothing -> "Pushover API returned status " <> show status
        pure $ Left errorMsg
