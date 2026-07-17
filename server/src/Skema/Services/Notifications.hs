{-# LANGUAGE LambdaCase #-}
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
import Skema.Services.EventLoop (JobMode(..), runEventService)
import Skema.Events.Types (Event(..))
import Skema.Config.Types (Config(..), NotificationConfig(..), NotificationProvider(..))
import Skema.Notifications.Types (Notification(..), NotificationSender(..))
import Skema.Notifications.Pushover (newPushoverClient)
import Skema.Database.Connection (withConnection)
import qualified Skema.Database.Repository as DB
import qualified Skema.Database.Types as DBTypes
import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.STM as STM
import Katip (Severity(..), logStr, logTM, runKatipContextT, sl, katipAddContext)
import Data.Time (UTCTime)
import Skema.Clock (getNow)
import Data.Time.Format (parseTimeM, defaultTimeLocale)

-- | Start the notification service.
--
-- This service listens for events and sends notifications based on configuration:
-- - WantedAlbumAdded (when new wanted albums are found)
-- - DownloadCompleted (when albums are downloaded)
-- - DownloadImported (when albums are imported to library)
startNotificationService :: NotificationDeps -> IO (Async ())
startNotificationService deps =
  -- Notifications are independent HTTP calls; overlap is safe
  runEventService (notifLogEnv deps) (notifEventBus deps) "services.notifications" Concurrent $ \case
    WantedAlbumAdded{..} ->
      Just $ handleWantedAlbumAdded deps wantedReleaseGroupId wantedAlbumTitle wantedArtistName
    DownloadImported{..} ->
      Just $ handleDownloadImported deps downloadTitle
    _ -> Nothing

-- | Handle a wanted album added event (sends notifications for upcoming wanted albums).
handleWantedAlbumAdded :: NotificationDeps -> Text -> Text -> Text -> IO ()
handleWantedAlbumAdded deps releaseGroupId albumTitle artistName = do
  config <- STM.atomically $ STM.readTVar (notifConfigVar deps)
  let notifConfig = notifications config

  when (notificationEnabled notifConfig && notificationOnAlbumFound notifConfig) $ do
    -- Fetch album details from database to get release date
    maybeAlbum <- withConnection (notifDbPool deps) $ \conn ->
      DB.getCatalogAlbumByReleaseGroupMBID conn releaseGroupId

    case maybeAlbum of
      Nothing -> pure ()  -- Album not found, skip
      Just album -> do
        let maybeReleaseDate = DBTypes.catalogAlbumFirstReleaseDate album

        -- Check if the release date is in the future
        now <- getNow (notifClock deps)
        let isUpcoming = case maybeReleaseDate of
              Nothing -> False  -- No release date, not upcoming
              Just releaseDateStr ->
                case parseTimeM True defaultTimeLocale "%Y-%m-%d" (toString releaseDateStr) :: Maybe UTCTime of
                  Nothing -> False  -- Invalid date format
                  Just releaseDate -> releaseDate > now  -- Future release

        when isUpcoming $ do
          let mbUrl = "https://musicbrainz.org/release-group/" <> releaseGroupId
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
  let httpClient = notifHttpClient deps

  forM_ providers $ \provider -> do
    case provider of
      PushoverProvider pushoverConfig -> do
        let client = newPushoverClient pushoverConfig httpClient
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
