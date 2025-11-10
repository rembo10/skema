{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Catalog service - syncs catalog with external providers (MusicBrainz).
--
-- This service is responsible for:
-- - Fetching artist discographies from MusicBrainz
-- - Storing all albums in catalog_albums
-- - Emitting CatalogAlbumAdded events for downstream services
-- - Keeping the catalog in sync with external providers
module Skema.Services.Catalog
  ( startCatalogService
  ) where

import Skema.Services.Dependencies (CatalogDeps(..))
import Skema.Events.Bus
import Skema.Events.Types
import Skema.Database.Connection
import Skema.Database.Repository
import qualified Skema.Database.Types as DB
import Skema.MusicBrainz.Client (getArtist)
import Skema.MusicBrainz.Types (MBID(..), MBArtist(..), MBReleaseGroup(..))
import Skema.Config.Types (Config(..), MusicBrainzConfig(..), mbAlbumTypes, mbExcludeSecondaryTypes)
import Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (readTChan)
import Control.Monad ()
import Control.Exception (try)
import Data.Time (getCurrentTime)
import Data.IORef (newIORef, modifyIORef', readIORef)
import Katip

-- | Start the catalog service.
--
-- This service listens for:
-- - CatalogArtistFollowed events (initial discography fetch)
-- - CatalogArtistRefreshRequested events (refresh to check for new releases)
startCatalogService :: CatalogDeps -> IO (Async ())
startCatalogService deps = do
  chan <- STM.atomically $ subscribe (catEventBus deps)
  async $ forever $ do
    envelope <- STM.atomically $ readTChan chan
    case envelopeEvent envelope of
      CatalogArtistFollowed{..} -> do
        -- Process each event asynchronously to avoid blocking the event loop
        _ <- async $ do
          result <- try $ handleCatalogArtistFollowed deps catalogArtistMBID catalogArtistName
          case result of
            Left (e :: SomeException) -> do
              let le = catLogEnv deps
              runKatipContextT le () "catalog.error" $ do
                $(logTM) ErrorS $ logStr $ ("Exception syncing artist catalog " <> catalogArtistName <> ": " <> show e :: Text)
            Right () -> pure ()
        pure ()
      CatalogArtistRefreshRequested{..} -> do
        -- Process refresh request asynchronously
        _ <- async $ do
          result <- try $ handleCatalogArtistRefresh deps refreshArtistMBID
          case result of
            Left (e :: SomeException) -> do
              let le = catLogEnv deps
              runKatipContextT le () "catalog.error" $ do
                $(logTM) ErrorS $ logStr $ ("Exception refreshing artist catalog: " <> show e :: Text)
            Right () -> pure ()
        pure ()
      _ -> pure ()  -- Ignore other events

-- | Handle a catalog artist followed event.
--
-- Fetches the artist's discography from MusicBrainz and stores all albums
-- in catalog_albums, emitting CatalogAlbumAdded events for each.
handleCatalogArtistFollowed :: CatalogDeps -> Text -> Text -> IO ()
handleCatalogArtistFollowed CatalogDeps{..} artistMBID artistName = do
  let le = catLogEnv
  let pool = catDbPool
  let bus = catEventBus
  let mbEnv = catMBClient

  -- Read current config from TVar
  config <- STM.atomically $ STM.readTVar catConfigVar
  let mbConfig = musicbrainz config
  let allowedTypes = mbAlbumTypes mbConfig
  let excludedSecondaryTypes = mbExcludeSecondaryTypes mbConfig
  let initialContext = ()
  let initialNamespace = "services.catalog"

  runKatipContextT le initialContext initialNamespace $ do
    katipAddContext (sl "artist_mbid" artistMBID) $ do
      katipAddContext (sl "artist_name" artistName) $ do
        $(logTM) InfoS $ logStr $ ("Fetching discography for: " <> artistName :: Text)

        -- Get the artist record from database to get its internal ID
        maybeArtistRecord <- liftIO $ withConnection pool $ \conn ->
          getCatalogArtistByMBID conn artistMBID

        case maybeArtistRecord of
          Nothing -> do
            $(logTM) ErrorS $ logStr $ ("Artist not found in catalog database: " <> artistName :: Text)
          Just artistRecord -> do
            case DB.catalogArtistId artistRecord of
              Nothing -> do
                $(logTM) ErrorS $ logStr $ ("Artist has no ID in catalog database: " <> artistName :: Text)
              Just artistId -> do
                -- Fetch artist discography from MusicBrainz
                result <- liftIO $ getArtist mbEnv (MBID artistMBID)

                case result of
                  Left err -> do
                    $(logTM) ErrorS $ logStr $ ("Failed to fetch artist discography: " <> show err :: Text)
                  Right artist -> do
                    let allReleaseGroups = mbArtistReleaseGroups artist
                    -- Filter release groups by configured album types and excluded secondary types
                    let releaseGroups = filter (shouldIncludeReleaseGroup allowedTypes excludedSecondaryTypes) allReleaseGroups

                    -- Emit event for observability
                    liftIO $ publishAndLog bus le "catalog" $ ArtistDiscographyFetched
                      { artistMBID = artistMBID
                      , releaseGroupCount = length releaseGroups
                      }

                    -- Store ALL albums in catalog_albums (not just wanted ones)
                    -- This is the catalog's responsibility - it tracks what exists
                    forM_ releaseGroups $ \rg -> do
                      let rgId = unMBID (mbrgId rg)
                      let title = mbrgTitle rg
                      let firstReleaseDate = mbrgFirstReleaseDate rg
                      let albumType = mbrgType rg

                      -- Check if album already exists in catalog
                      existingAlbum <- liftIO $ withConnection pool $ \conn ->
                        getCatalogAlbumByReleaseGroupMBID conn rgId

                      case existingAlbum of
                        Just _ -> do
                          -- Album already exists, skip (catalog is already up to date)
                          $(logTM) DebugS $ logStr $ ("Album already in catalog: " <> title :: Text)
                        Nothing -> do
                          -- New album - add to catalog with wanted=false (let acquisition decide)
                          albumId <- liftIO $ withConnection pool $ \conn ->
                            upsertCatalogAlbum conn rgId title artistId artistMBID artistName albumType firstReleaseDate False Nothing

                          $(logTM) InfoS $ logStr $ ("Added album to catalog: " <> title :: Text)

                          -- Emit CatalogAlbumAdded event with complete album data
                          -- This eliminates the need for frontend to make GET requests
                          liftIO $ publishAndLog bus le "catalog" $ CatalogAlbumAdded
                            { catalogAlbumId = albumId
                            , catalogAlbumReleaseGroupMBID = rgId
                            , catalogAlbumTitle = title
                            , catalogAlbumArtistId = artistId
                            , catalogAlbumArtistMBID = artistMBID
                            , catalogAlbumArtistName = artistName
                            , catalogAlbumType = albumType
                            , catalogAlbumFirstReleaseDate = firstReleaseDate
                            , catalogAlbumWanted = False
                            }

-- | Handle a catalog artist refresh request.
--
-- Re-fetches the artist's discography from MusicBrainz and adds any new albums
-- that aren't already in the catalog.
handleCatalogArtistRefresh :: CatalogDeps -> Text -> IO ()
handleCatalogArtistRefresh CatalogDeps{..} artistMBID = do
  let le = catLogEnv
  let pool = catDbPool
  let bus = catEventBus
  let mbEnv = catMBClient

  -- Read current config from TVar
  config <- STM.atomically $ STM.readTVar catConfigVar
  let mbConfig = musicbrainz config
  let allowedTypes = mbAlbumTypes mbConfig
  let excludedSecondaryTypes = mbExcludeSecondaryTypes mbConfig
  let initialContext = ()
  let initialNamespace = "services.catalog"

  runKatipContextT le initialContext initialNamespace $ do
    katipAddContext (sl "artist_mbid" artistMBID) $ do
      $(logTM) InfoS $ logStr $ ("Refreshing catalog for artist: " <> artistMBID :: Text)

      -- Get artist info from database
      artistRecord <- liftIO $ withConnection pool $ \conn ->
        getCatalogArtistByMBID conn artistMBID

      case artistRecord of
        Nothing -> do
          $(logTM) WarningS $ logStr $ ("Artist not found in catalog, cannot refresh: " <> artistMBID :: Text)
        Just artist -> do
          let artistName = DB.catalogArtistName artist

          katipAddContext (sl "artist_name" artistName) $ do
            $(logTM) InfoS $ logStr $ ("Refreshing discography for: " <> artistName :: Text)

            -- Fetch artist discography from MusicBrainz
            result <- liftIO $ getArtist mbEnv (MBID artistMBID)

            case result of
              Left err -> do
                $(logTM) ErrorS $ logStr $ ("Failed to fetch artist discography: " <> show err :: Text)
              Right mbArtist -> do
                let allReleaseGroups = mbArtistReleaseGroups mbArtist
                -- Filter release groups by configured album types and excluded secondary types
                let releaseGroups = filter (shouldIncludeReleaseGroup allowedTypes excludedSecondaryTypes) allReleaseGroups

                -- Update last_checked_at timestamp
                case DB.catalogArtistId artist of
                  Just artistId -> do
                    now <- liftIO getCurrentTime
                    liftIO $ withConnection pool $ \conn ->
                      executeQuery conn
                        "UPDATE catalog_artists SET last_checked_at = ? WHERE id = ?"
                        (now, artistId)
                  Nothing -> pure ()

                -- Emit event for observability
                liftIO $ publishAndLog bus le "catalog" $ ArtistDiscographyFetched
                  { artistMBID = artistMBID
                  , releaseGroupCount = length releaseGroups
                  }

                -- Check for new albums and add them
                newAlbumsCount <- liftIO $ Data.IORef.newIORef (0 :: Int)

                forM_ releaseGroups $ \rg -> do
                  let rgId = unMBID (mbrgId rg)
                  let title = mbrgTitle rg
                  let firstReleaseDate = mbrgFirstReleaseDate rg
                  let albumType = mbrgType rg

                  -- Check if album already exists in catalog
                  existingAlbum <- liftIO $ withConnection pool $ \conn ->
                    getCatalogAlbumByReleaseGroupMBID conn rgId

                  case existingAlbum of
                    Just _ -> do
                      -- Album already exists, skip
                      $(logTM) DebugS $ logStr $ ("Album already in catalog: " <> title :: Text)
                    Nothing -> do
                      -- New album discovered! Add to catalog
                      -- artistId is already available from the artist record
                      let artistId = case DB.catalogArtistId artist of
                            Just aid -> aid
                            Nothing -> error "Artist ID should be available here"
                      albumId <- liftIO $ withConnection pool $ \conn ->
                        upsertCatalogAlbum conn rgId title artistId artistMBID artistName albumType firstReleaseDate False Nothing

                      liftIO $ Data.IORef.modifyIORef' newAlbumsCount (+1)
                      $(logTM) InfoS $ logStr $ ("NEW album discovered: " <> title :: Text)

                      -- Emit CatalogAlbumAdded event with complete album data
                      -- This eliminates the need for frontend to make GET requests
                      liftIO $ publishAndLog bus le "catalog" $ CatalogAlbumAdded
                        { catalogAlbumId = albumId
                        , catalogAlbumReleaseGroupMBID = rgId
                        , catalogAlbumTitle = title
                        , catalogAlbumArtistId = artistId
                        , catalogAlbumArtistMBID = artistMBID
                        , catalogAlbumArtistName = artistName
                        , catalogAlbumType = albumType
                        , catalogAlbumFirstReleaseDate = firstReleaseDate
                        , catalogAlbumWanted = False
                        }

                newCount <- liftIO $ Data.IORef.readIORef newAlbumsCount
                $(logTM) InfoS $ logStr $ ("Catalog refresh complete. Found " <> show newCount <> " new albums for " <> artistName :: Text)

-- | Check if a release group should be included based on configured album types.
--
-- Checks both primary type and secondary types:
-- - Primary type must be in allowedTypes (or allowedTypes is empty)
-- - None of the secondary types can be in excludedSecondaryTypes
--
-- If the release group has no primary type, it's included by default.
shouldIncludeReleaseGroup :: [Text] -> [Text] -> MBReleaseGroup -> Bool
shouldIncludeReleaseGroup allowedTypes excludedSecondaryTypes rg =
  let -- Check primary type
      primaryTypeOk = case mbrgType rg of
        Nothing -> True  -- Unknown type, include by default
        Just rgType
          | null allowedTypes -> True  -- No filter configured, include all
          | otherwise -> rgType `elem` allowedTypes

      -- Check secondary types - exclude if ANY secondary type is in the exclude list
      secondaryTypesOk =
        let secondaryTypes = mbrgSecondaryTypes rg
        in null excludedSecondaryTypes ||
           not (any (`elem` excludedSecondaryTypes) secondaryTypes)

  in primaryTypeOk && secondaryTypesOk
