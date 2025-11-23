{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Notification service - sends notifications on important events.
--
-- This service is responsible for:
-- - Subscribing to events from the event bus
-- - Filtering events based on user configuration
-- - Sending notifications through configured providers (Pushover, etc.)
module Skema.Services.Notifications
  ( startNotificationService
  ) where

import Skema.Services.Dependencies (NotificationDeps(..))
import Skema.Events.Bus (subscribe)
import Skema.Events.Types (Event(..), EventEnvelope(..), envelopeEvent)
import Skema.Config.Types (Config(..), NotificationConfig(..), NotificationProvider(..))
import Skema.Notifications.Types (Notification(..), NotificationSender(..))
import Skema.Notifications.Pushover (newPushoverClient)
import Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (readTChan)
import Control.Exception (try)
import Katip (Severity(..), logStr, logTM, runKatipContextT, sl, katipAddContext)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)

-- | Start the notification service.
--
-- This service listens for events and sends notifications based on configuration:
-- - WantedAlbumAdded (when new wanted albums are found)
-- - DownloadCompleted (when albums are downloaded)
-- - DownloadImported (when albums are imported to library)
startNotificationService :: NotificationDeps -> IO (Async ())
startNotificationService deps = do
  chan <- STM.atomically $ subscribe (notifEventBus deps)
  async $ forever $ do
    envelope <- STM.atomically $ readTChan chan
    case envelopeEvent envelope of
      CatalogAlbumAdded{..} | catalogAlbumWanted -> do
        -- Process asynchronously to avoid blocking event loop
        _ <- async $ do
          result <- try $ handleCatalogAlbumAdded deps catalogAlbumReleaseGroupMBID catalogAlbumTitle catalogAlbumArtistName catalogAlbumFirstReleaseDate
          logResult deps result "CatalogAlbumAdded"
        pure ()

      DownloadImported{..} -> do
        _ <- async $ do
          result <- try $ handleDownloadImported deps downloadTitle
          logResult deps result "DownloadImported"
        pure ()

      _ -> pure ()  -- Ignore other events

-- | Log the result of handling an event.
logResult :: NotificationDeps -> Either SomeException () -> Text -> IO ()
logResult deps result eventType = do
  let le = notifLogEnv deps
  case result of
    Left (e :: SomeException) -> do
      runKatipContextT le () "notifications.error" $ do
        $(logTM) ErrorS $ logStr $ ("Exception handling " <> eventType <> " event: " <> show e :: Text)
    Right () -> pure ()

-- | Handle a catalog album added event (only for wanted albums with future release dates).
handleCatalogAlbumAdded :: NotificationDeps -> Text -> Text -> Text -> Maybe Text -> IO ()
handleCatalogAlbumAdded deps catalogAlbumReleaseGroupMBID albumTitle artistName maybeReleaseDate = do
  config <- STM.atomically $ STM.readTVar (notifConfigVar deps)
  let notifConfig = notifications config

  when (notificationEnabled notifConfig && notificationOnAlbumFound notifConfig) $ do
    -- Check if the release date is in the future
    now <- getCurrentTime
    let isUpcoming = case maybeReleaseDate of
          Nothing -> False  -- No release date, not upcoming
          Just releaseDateStr ->
            case parseTimeM True defaultTimeLocale "%Y-%m-%d" (toString releaseDateStr) :: Maybe UTCTime of
              Nothing -> False  -- Invalid date format
              Just releaseDate -> releaseDate > now  -- Future release

    when isUpcoming $ do
      let mbUrl = "https://musicbrainz.org/release-group/" <> catalogAlbumReleaseGroupMBID
      let releaseDateText = maybe "Unknown date" identity maybeReleaseDate
      let message = "<b>" <> artistName <> "</b>\n"
                 <> albumTitle <> "\n"
                 <> "Release Date: " <> releaseDateText
      let notification = Notification
            { notificationTitle = "🎵 Upcoming Album"
            , notificationMessage = message
            , notificationUrl = Just mbUrl
            }
      sendToAllProviders deps notification

-- | Handle a download imported event.
handleDownloadImported :: NotificationDeps -> Text -> IO ()
handleDownloadImported deps downloadTitle = do
  config <- STM.atomically $ STM.readTVar (notifConfigVar deps)
  let notifConfig = notifications config

  when (notificationEnabled notifConfig && notificationOnAlbumImported notifConfig) $ do
    let message = "<b>" <> downloadTitle <> "</b>\n"
               <> "Successfully imported to your library"
    let notification = Notification
          { notificationTitle = "📀 Album Imported"
          , notificationMessage = message
          , notificationUrl = Nothing
          }
    sendToAllProviders deps notification

-- | Send a notification to all configured providers.
sendToAllProviders :: NotificationDeps -> Notification -> IO ()
sendToAllProviders deps notification = do
  config <- STM.atomically $ STM.readTVar (notifConfigVar deps)
  let notifConfig = notifications config
  let providers = notificationProviders notifConfig
  let le = notifLogEnv deps
  let manager = notifHttpManager deps

  forM_ providers $ \provider -> do
    case provider of
      PushoverProvider pushoverConfig -> do
        let client = newPushoverClient pushoverConfig manager
        result <- sendNotification client notification

        case result of
          Right () -> do
            runKatipContextT le () "notifications.pushover" $ do
              katipAddContext (sl "title" (notificationTitle notification)) $ do
                $(logTM) InfoS $ logStr ("Notification sent successfully" :: Text)

          Left err -> do
            runKatipContextT le () "notifications.pushover" $ do
              katipAddContext (sl "title" (notificationTitle notification)) $ do
                katipAddContext (sl "error" err) $ do
                  $(logTM) ErrorS $ logStr ("Failed to send notification: " <> err :: Text)
