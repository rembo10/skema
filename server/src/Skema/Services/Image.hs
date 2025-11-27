{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Image service - fetches and stores artist images and album covers.
module Skema.Services.Image
  ( startImageService
  ) where

import Skema.Services.Dependencies (ImageDeps(..))
import Skema.Events.Bus
import Skema.Events.Types
import Skema.Database.Connection
import Skema.Database.Repository (getCatalogArtistByMBID)
import Skema.Database.Types (catalogArtistId, catalogArtistImageUrl)
import Skema.Config.Types (media, mediaLastFmApiKey)
import Skema.MusicBrainz.Types (MBID(..))
import Skema.Media.Client (fetchArtistImage, fetchAlbumCover, defaultMediaConfig)
import Skema.Media.Types (MediaResult(..), MediaSource(..), mcLastFmApiKey)
import Skema.Media.Storage (downloadAndStoreImages)
import Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (readTChan)
import Control.Exception (try)
import Database.SQLite.Simple (Only(..))
import Katip

-- | Convert MediaSource to Text for event emission.
mediaSourceToText :: MediaSource -> Text
mediaSourceToText FanartTV = "FanartTV"
mediaSourceToText CoverArtArchive = "CoverArtArchive"
mediaSourceToText LastFM = "Last.fm"
mediaSourceToText ITunes = "iTunes"
mediaSourceToText MusicBrainz = "MusicBrainz"
mediaSourceToText Deezer = "Deezer"

-- | Start the image service.
--
-- This service listens for events and fetches/stores images as needed.
startImageService :: ImageDeps -> IO (Async ())
startImageService deps = do
  chan <- STM.atomically $ subscribe (imgEventBus deps)
  async $ forever $ do
    envelope <- STM.atomically $ readTChan chan
    case envelopeEvent envelope of
      CatalogArtistFollowed{..} -> do
        -- Fetch image for followed catalog artist
        -- Get the artist ID from the database using the MBID
        maybeArtistRecord <- withConnection (imgDbPool deps) $ \conn ->
          getCatalogArtistByMBID conn catalogArtistMBID
        case maybeArtistRecord of
          Nothing -> pure ()  -- Artist not found, skip
          Just artistRecord -> do
            -- Skip if artist already has an image to avoid duplicate fetches
            case (catalogArtistId artistRecord, catalogArtistImageUrl artistRecord) of
              (Just artistId, Nothing) -> do
                -- Artist has ID but no image - fetch it
                _ <- async $ do
                  result <- try $ handleArtistImageFetch deps artistId catalogArtistMBID catalogArtistName
                  case result of
                    Left (e :: SomeException) -> do
                      let le = imgLogEnv deps
                      runKatipContextT le () "image.error" $ do
                        $(logTM) ErrorS $ logStr $ ("Exception fetching image for catalog artist " <> catalogArtistName <> ": " <> show e :: Text)
                    Right () -> pure ()
                pure ()
              _ -> pure ()  -- Artist already has image or no ID, skip

      CatalogAlbumAdded{..} -> do
        -- Fetch cover for newly added catalog album
        _ <- async $ do
          result <- try $ handleAlbumCoverFetch deps catalogAlbumReleaseGroupMBID catalogAlbumTitle catalogAlbumId
          case result of
            Left (e :: SomeException) -> do
              let le = imgLogEnv deps
              runKatipContextT le () "image.error" $ do
                $(logTM) ErrorS $ logStr $ ("Exception fetching cover for album " <> catalogAlbumTitle <> ": " <> show e :: Text)
            Right () -> pure ()
        pure ()

      ArtistDiscographyRequested{..} -> do
        -- User is viewing this artist's page - prioritize fetching covers for their albums
        _ <- async $ handleArtistDiscographyImageRequest deps requestedArtistId requestedArtistMBID
        pure ()

      _ -> pure ()  -- Ignore other events

-- | Handle artist image fetching and storage.
handleArtistImageFetch :: ImageDeps -> Int64 -> Text -> Text -> IO ()
handleArtistImageFetch ImageDeps{..} artistId artistMBID artistName = do
  let le = imgLogEnv
  let pool = imgDbPool
  let bus = imgEventBus
  let initialContext = ()
  let initialNamespace = "services.image.artist"

  -- Read current config
  config <- STM.atomically $ STM.readTVar imgConfigVar

  runKatipContextT le initialContext initialNamespace $ do
    katipAddContext (sl "artist_mbid" artistMBID) $ do
      katipAddContext (sl "artist_name" artistName) $ do
        $(logTM) InfoS $ logStr $ ("Fetching image for artist: " <> artistName :: Text)

        -- Build media config with user's Last.fm API key if configured
        let mediaConfig = defaultMediaConfig
              { mcLastFmApiKey = mediaLastFmApiKey (media config)
              }
        maybeResult <- liftIO $ fetchArtistImage le imgHttpClient mediaConfig (MBID artistMBID)

        -- Download and store image locally if found
        case maybeResult of
          Nothing -> do
            $(logTM) DebugS $ logStr ("No artist image found" :: Text)
          Just result -> do
            let externalUrl = mrUrl result
            let thumbnailUrl = mrThumbnailUrl result
            let source = mrSource result
            $(logTM) InfoS $ logStr $ ("Found artist image from " <> mediaSourceToText source <> ": " <> externalUrl :: Text)
            downloadResult <- liftIO $ downloadAndStoreImages
              imgHttpClient
              imgCacheDir
              "artists"
              artistMBID
              externalUrl
              thumbnailUrl
            case downloadResult of
              Left err -> do
                $(logTM) ErrorS $ logStr $ ("Failed to download image: " <> err :: Text)
              Right (localPath, maybeThumbPath) -> do
                $(logTM) InfoS $ logStr $ ("Image stored locally: " <> localPath :: Text)
                forM_ maybeThumbPath $ \thumbPath ->
                  $(logTM) InfoS $ logStr $ ("Thumbnail stored locally: " <> thumbPath :: Text)

                -- Update catalog_artists with image URLs using internal ID
                liftIO $ withConnection pool $ \conn ->
                  executeQuery conn
                    "UPDATE catalog_artists SET image_url = ?, thumbnail_url = ? WHERE id = ?"
                    (Just localPath, maybeThumbPath, artistId)

                -- Emit event that image is ready (with both full and thumbnail URLs)
                liftIO $ publishAndLog bus le "image.artist" $ ArtistImageFetched
                  { artistImageId = artistId
                  , artistImageMBID = artistMBID
                  , artistImageUrl = localPath
                  , artistImageThumbnailUrl = maybeThumbPath
                  , artistImageSource = mediaSourceToText source
                  }

-- | Handle album cover fetching and storage.
handleAlbumCoverFetch :: ImageDeps -> Text -> Text -> Int64 -> IO ()
handleAlbumCoverFetch ImageDeps{..} releaseGroupMBID albumTitle albumId = do
  let le = imgLogEnv
  let pool = imgDbPool
  let bus = imgEventBus
  let initialContext = ()
  let initialNamespace = "services.image.album"

  -- Read current config
  config <- STM.atomically $ STM.readTVar imgConfigVar

  runKatipContextT le initialContext initialNamespace $ do
    katipAddContext (sl "release_group_mbid" releaseGroupMBID) $ do
      katipAddContext (sl "album_title" albumTitle) $ do
        $(logTM) InfoS $ logStr $ ("Fetching album cover for: " <> albumTitle :: Text)

        -- Build media config with user's Last.fm API key if configured
        let mediaConfig = defaultMediaConfig
              { mcLastFmApiKey = mediaLastFmApiKey (media config)
              }
        maybeResult <- liftIO $ fetchAlbumCover le imgHttpClient mediaConfig (MBID releaseGroupMBID)

        -- Download and store cover image locally if found
        case maybeResult of
          Nothing -> do
            $(logTM) DebugS $ logStr ("No album cover found" :: Text)
          Just result -> do
            let externalUrl = mrUrl result
            let thumbnailUrl = mrThumbnailUrl result
            let source = mrSource result
            $(logTM) InfoS $ logStr $ ("Found album cover from " <> mediaSourceToText source <> ": " <> externalUrl :: Text)
            downloadResult <- liftIO $ downloadAndStoreImages
              imgHttpClient
              imgCacheDir
              "albums"
              releaseGroupMBID
              externalUrl
              thumbnailUrl
            case downloadResult of
              Left err -> do
                $(logTM) ErrorS $ logStr $ ("Failed to download cover: " <> err :: Text)
              Right (localPath, maybeThumbPath) -> do
                $(logTM) InfoS $ logStr $ ("Cover stored locally: " <> localPath :: Text)
                forM_ maybeThumbPath $ \thumbPath ->
                  $(logTM) InfoS $ logStr $ ("Cover thumbnail stored locally: " <> thumbPath :: Text)

                -- Update catalog_albums with both cover URLs using internal ID
                liftIO $ withConnection pool $ \conn -> do
                  executeQuery conn
                    "UPDATE catalog_albums SET album_cover_url = ?, album_cover_thumbnail_url = ? WHERE id = ?"
                    (Just localPath, maybeThumbPath, albumId)

                -- Emit event that cover is ready
                liftIO $ publishAndLog bus le "image.album" $ AlbumCoverFetched
                  { albumCoverReleaseGroupMBID = releaseGroupMBID
                  , albumCoverUrl = localPath
                  , albumCoverThumbnailUrl = maybeThumbPath
                  , albumCoverSource = mediaSourceToText source
                  }

-- | Handle artist discography image request (on-demand priority fetch).
--
-- When a user views an artist's page, immediately fetch covers for all
-- albums by that artist that don't have covers yet.
handleArtistDiscographyImageRequest :: ImageDeps -> Int64 -> Text -> IO ()
handleArtistDiscographyImageRequest ImageDeps{..} artistId artistMBID = do
  let le = imgLogEnv
  let pool = imgDbPool
  let initialContext = ()
  let initialNamespace = "services.image.priority"

  runKatipContextT le initialContext initialNamespace $ do
    katipAddContext (sl "artist_id" artistId) $ do
      katipAddContext (sl "artist_mbid" artistMBID) $ do
        $(logTM) InfoS $ logStr $ ("Prioritizing album covers for artist ID: " <> show artistId :: Text)

        -- Query for albums by this artist that don't have covers
        albums <- liftIO $ withConnection pool $ \conn -> do
          queryRows conn
            "SELECT id, release_group_mbid, title FROM catalog_albums \
            \WHERE artist_id = ? AND album_cover_url IS NULL"
            (Only artistId)

        $(logTM) InfoS $ logStr $ ("Found " <> show (length albums) <> " albums without covers for this artist" :: Text)

        -- Fetch covers for each album without a cover (in parallel for better UX)
        liftIO $ forM_ albums $ \(albumId :: Int64, releaseGroupMBID :: Text, albumTitle :: Text) -> do
          _ <- async $ do
            result <- try $ handleAlbumCoverFetch (ImageDeps imgEventBus imgLogEnv imgDbPool imgConfigVar imgHttpClient imgCacheDir) releaseGroupMBID albumTitle albumId
            case result of
              Left (e :: SomeException) -> do
                runKatipContextT le () "image.error" $ do
                  $(logTM) ErrorS $ logStr $ ("Exception fetching cover for album " <> albumTitle <> ": " <> show e :: Text)
              Right () -> pure ()
          pure ()

        $(logTM) InfoS $ logStr $ ("Started priority fetch for " <> show (length albums) <> " album covers" :: Text)
