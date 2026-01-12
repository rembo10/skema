{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Acquisition service - evaluates acquisition sources and marks wanted albums.
--
-- This service is responsible for:
-- - Following artists from library based on acquisition sources
-- - Listening to CatalogAlbumAdded events
-- - Applying acquisition sources to determine if albums should be wanted
-- - Updating wanted status in catalog_albums
module Skema.Services.Acquisition
  ( startAcquisitionService
  ) where

import Skema.Services.Dependencies (AcquisitionDeps(..))
import Skema.Services.Filters (shouldProcessArtistById, parseSourceFilters, LibraryArtistsFilters(..), ReleaseStatusFilter(..), SourceFilters(..))
import Skema.Events.Bus
import Skema.Events.Types
import Skema.Database.Connection
import Skema.Database.Repository
import Skema.Database.Types (CatalogAlbumRecord(..), AcquisitionSourceRecord(..), SourceType(..))
import qualified Skema.Database.Types as DB
import Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (readTChan)
import Control.Monad ()
import Control.Exception (try)
import Data.Time (getCurrentTime, UTCTime)
import qualified Data.Text as T
import Katip

-- | Start the acquisition service.
--
-- This service listens for:
-- - LibraryArtistFound events to follow artists based on rules
-- - CatalogAlbumAdded events to evaluate if albums should be wanted
startAcquisitionService :: AcquisitionDeps -> IO (Async ())
startAcquisitionService deps = do
  chan <- STM.atomically $ subscribe (acqEventBus deps)
  async $ forever $ do
    envelope <- STM.atomically $ readTChan chan
    case envelopeEvent envelope of
      LibraryArtistFound{..} -> do
        -- Process each event asynchronously to avoid blocking the event loop
        _ <- async $ do
          result <- try $ handleLibraryArtistFound deps foundArtistMBID foundArtistName foundClusterId foundReleaseGroupId
          case result of
            Left (e :: SomeException) -> do
              let le = acqLogEnv deps
              runKatipContextT le () "acquisition.error" $ do
                $(logTM) ErrorS $ logStr $ ("Exception processing artist " <> foundArtistName <> ": " <> show e :: Text)
            Right () -> pure ()
        pure ()
      CatalogAlbumAdded{..} -> do
        -- Process album additions from catalog service
        _ <- async $ do
          result <- try $ handleCatalogAlbumAdded deps catalogAlbumReleaseGroupMBID catalogAlbumTitle catalogAlbumArtistMBID catalogAlbumId
          case result of
            Left (e :: SomeException) -> do
              let le = acqLogEnv deps
              runKatipContextT le () "acquisition.error" $ do
                $(logTM) ErrorS $ logStr $ ("Exception evaluating album " <> catalogAlbumTitle <> ": " <> show e :: Text)
            Right () -> pure ()
        pure ()
      _ -> pure ()  -- Ignore other events

-- | Handle a library artist found event.
--
-- Evaluates acquisition sources and follows artists that match.
-- The Catalog service will then fetch the discography.
handleLibraryArtistFound :: AcquisitionDeps -> Text -> Text -> Int64 -> Maybe Text -> IO ()
handleLibraryArtistFound AcquisitionDeps{..} artistMBID artistName clusterId _mbReleaseGroupId = do
  let le = acqLogEnv
  let pool = acqDbPool
  let bus = acqEventBus
  let initialContext = ()
  let initialNamespace = "services.acquisition"

  runKatipContextT le initialContext initialNamespace $ do
    katipAddContext (sl "artist_mbid" artistMBID) $ do
      katipAddContext (sl "artist_name" artistName) $ do
        katipAddContext (sl "cluster_id" clusterId) $ do
          $(logTM) InfoS $ logStr $ ("Processing library artist: " <> artistName :: Text)

          -- Skip "Various Artists" - it's not a trackable artist
          if DB.isVariousArtists artistMBID
            then $(logTM) DebugS $ logStr ("Skipping Various Artists (not a trackable artist)" :: Text)
            else do
              -- Get enabled acquisition sources (only LibraryArtists sources are relevant here)
              sources <- liftIO $ withConnection pool $ \conn ->
                getEnabledAcquisitionRules conn

              let libraryArtistsSources = filter (\s -> sourceType s == LibraryArtists) sources

              when (null libraryArtistsSources) $ do
                $(logTM) DebugS $ logStr ("No enabled LibraryArtists sources found, skipping" :: Text)

              -- First, ensure the artist exists in catalog_artists to get an ID
              -- We insert with followed=false initially, then update if it matches filters
              existingArtist <- liftIO $ withConnection pool $ \conn ->
                getCatalogArtistByMBID conn artistMBID

              artistId <- case existingArtist of
                Just artist -> case DB.catalogArtistId artist of
                  Just aid -> do
                    $(logTM) DebugS $ logStr $ ("Artist already exists with ID: " <> T.pack (show aid) :: Text)
                    pure aid
                  Nothing -> do
                    $(logTM) ErrorS $ logStr ("Artist exists but has no ID!" :: Text)
                    -- Create new entry to get an ID
                    liftIO $ withConnection pool $ \conn ->
                      upsertCatalogArtist conn artistMBID artistName Nothing Nothing Nothing False Nothing (Just clusterId) Nothing
                Nothing -> do
                  $(logTM) DebugS $ logStr ("Creating new catalog artist entry" :: Text)
                  -- Create unfollowed artist to get an ID
                  liftIO $ withConnection pool $ \conn ->
                    upsertCatalogArtist conn artistMBID artistName Nothing Nothing Nothing False Nothing (Just clusterId) Nothing

              -- Now check if this artist ID matches any LibraryArtists source filters
              forM_ libraryArtistsSources $ \source -> do
                when (shouldProcessArtistById source artistId) $ do
                  $(logTM) InfoS $ logStr $ ("Artist ID " <> T.pack (show artistId) <> " matches source: " <> sourceName source :: Text)

                  -- Update artist to followed=true and associate with this source
                  case sourceId source of
                    Just sid -> do
                      _ <- liftIO $ withConnection pool $ \conn ->
                        upsertCatalogArtist conn artistMBID artistName Nothing Nothing Nothing True (Just sid) (Just clusterId) Nothing

                      $(logTM) InfoS $ logStr ("Artist marked as followed" :: Text)

                      -- Emit TrackedArtistAdded event with the database ID and all needed data
                      -- This is the single source of truth for frontend - includes ID, name, MBID
                      -- Image will be updated later via ARTIST_IMAGE_FETCHED event
                      liftIO $ publishAndLog bus le "acquisition" $ TrackedArtistAdded
                        { trackedArtistId = artistId
                        , trackedArtistMBID = artistMBID
                        , trackedArtistName = artistName
                        , trackedArtistClusterId = clusterId
                        , trackedArtistReleaseGroupId = _mbReleaseGroupId
                        }

                      -- Emit CatalogArtistFollowed event
                      -- This will be picked up by:
                      -- - Catalog service (to fetch discography)
                      -- - Image service (to fetch artist image)
                      liftIO $ publishAndLog bus le "acquisition" $ CatalogArtistFollowed
                        { catalogArtistMBID = artistMBID
                        , catalogArtistName = artistName
                        }

                    Nothing -> do
                      $(logTM) ErrorS $ logStr ("Source has no ID, cannot add tracked artist" :: Text)

-- | Handle a catalog album added event.
--
-- Evaluates acquisition sources to determine if the album should be wanted.
handleCatalogAlbumAdded :: AcquisitionDeps -> Text -> Text -> Text -> Int64 -> IO ()
handleCatalogAlbumAdded AcquisitionDeps{..} releaseGroupMBID albumTitle artistMBID _albumId = do
  let le = acqLogEnv
  let pool = acqDbPool
  let bus = acqEventBus
  let initialContext = ()
  let initialNamespace = "services.acquisition"

  runKatipContextT le initialContext initialNamespace $ do
    katipAddContext (sl "release_group_mbid" releaseGroupMBID) $ do
      katipAddContext (sl "album_title" albumTitle) $ do
        katipAddContext (sl "artist_mbid" artistMBID) $ do
          $(logTM) DebugS $ logStr $ ("Evaluating album: " <> albumTitle :: Text)

          -- Get the catalog album from database to check its details
          catalogAlbum <- liftIO $ withConnection pool $ \conn ->
            getCatalogAlbumByReleaseGroupMBID conn releaseGroupMBID

          case catalogAlbum of
            Nothing -> do
              $(logTM) WarningS $ logStr $ ("Album not found in catalog: " <> albumTitle :: Text)
            Just album -> do
              -- Check if user has explicitly marked this album as unwanted
              -- NOTE: "user_unwanted" column no longer exists
              -- Albums that are not wanted simply won't have a quality profile assigned
              if isNothing (DB.catalogAlbumQualityProfileId album)
                then $(logTM) DebugS $ logStr $ ("Album explicitly unwanted by user, skipping: " <> albumTitle :: Text)
                else do
                  -- Get the catalog artist to find which sources apply
                  artistRecord <- liftIO $ withConnection pool $ \conn ->
                    getCatalogArtistByMBID conn artistMBID

                  case artistRecord of
                    Nothing -> do
                      $(logTM) DebugS $ logStr $ ("Artist not followed, skipping album evaluation" :: Text)
                    Just artist -> do
                      -- Only evaluate if artist is followed
                      when (DB.catalogArtistFollowed artist) $ do
                        -- Get enabled acquisition sources
                        sources <- liftIO $ withConnection pool $ \conn ->
                          getEnabledAcquisitionRules conn

                        -- Check if any source wants this album
                        -- Filter to only LibraryArtists sources that match this artist
                        let libraryArtistsSources = filter (\s -> sourceType s == LibraryArtists) sources

                        -- Get artist ID to check filters - skip if no ID
                        case DB.catalogArtistId artist of
                          Nothing -> do
                            $(logTM) WarningS $ logStr ("Artist has no ID, cannot evaluate filters" :: Text)
                          Just artistId -> do
                            let matchingSources = filter (\source -> shouldProcessArtistById source artistId) libraryArtistsSources

                            forM_ matchingSources $ \source -> do
                              -- Evaluate source filters to determine if album should be wanted
                              now <- liftIO getCurrentTime

                              -- Evaluate provider-specific filters
                              let shouldWant = case parseSourceFilters LibraryArtists (sourceFilters source) of
                                    Just (LibraryArtistsSourceFilters filters) ->
                                      evaluateLibraryArtistsAlbumFilters filters now album
                                    _ -> False  -- No valid filters configured

                              when shouldWant $ do
                                $(logTM) InfoS $ logStr $ ("Album matches acquisition source, marking as wanted: " <> albumTitle :: Text)

                                -- Emit WantedAlbumAdded event
                                case DB.catalogAlbumId album of
                                  Just albumId -> do
                                    case DB.catalogArtistName <$> Just artist of
                                      Just artistName -> do
                                        liftIO $ publishAndLog bus le "acquisition" $ WantedAlbumAdded
                                          { wantedCatalogAlbumId = albumId
                                          , wantedReleaseGroupId = releaseGroupMBID
                                          , wantedAlbumTitle = albumTitle
                                          , wantedArtistName = artistName
                                          }
                                      _ -> pure ()
                                  Nothing -> do
                                    $(logTM) WarningS $ logStr $ ("Album has no ID, cannot emit event: " <> albumTitle :: Text)

-- | Evaluate LibraryArtistsFilters against a catalog album to determine if it should be wanted.
-- This is the new ID-based filter evaluation for LibraryArtists sources.
evaluateLibraryArtistsAlbumFilters :: LibraryArtistsFilters -> UTCTime -> CatalogAlbumRecord -> Bool
evaluateLibraryArtistsAlbumFilters filters now album =
  -- Check if album type matches filter
  let albumTypeMatch = case lafAlbumTypes filters of
        Nothing -> True  -- No album type filter specified, accept all types
        Just types -> case DB.catalogAlbumType album of
          Just albumType -> albumType `elem` types
          Nothing -> True  -- If no type, include by default

      -- Check if release status matches filter
      isUpcoming = case DB.catalogAlbumFirstReleaseDate album of
        Nothing -> False  -- No release date, not upcoming
        Just dateStr ->
          -- Compare ISO 8601 date strings (YYYY-MM-DD format)
          let today = T.take 10 (T.pack (show now))
          in dateStr > today

      releaseStatusMatch = case lafReleaseStatus filters of
        Nothing -> isUpcoming  -- Default: only upcoming albums (backward compatible)
        Just UpcomingOnly -> isUpcoming
        Just ReleasedOnly -> not isUpcoming
        Just UpcomingAndReleased -> True  -- Accept both

  -- Combine all checks (AND by default for album criteria)
  in albumTypeMatch && releaseStatusMatch
