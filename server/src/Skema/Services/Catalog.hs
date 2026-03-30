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
-- - Periodically refreshing followed artists on a smart schedule
module Skema.Services.Catalog
  ( startCatalogService
  , startCatalogRefreshScheduler
  ) where

import Skema.Services.Dependencies (CatalogDeps(..))
import Skema.Events.Bus
import Skema.Events.Types
import Skema.Database.Connection
import Skema.Database.Repository
import qualified Skema.Database.Types as DB
import Skema.MusicBrainz.Client (getArtist, getArtistConditional, ConditionalResult(..))
import Skema.MusicBrainz.Types (MBID(..), MBArtist(..), MBReleaseGroup(..))
import Skema.Config.Types (Config(..), MusicBrainzConfig(..), mbAlbumTypes, mbExcludeSecondaryTypes, media, mediaLastFmApiKey)
import qualified Skema.Media.Providers.LastFM as LastFM
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (readTChan)
import Control.Monad ()
import Control.Exception (try)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale, UTCTime, NominalDiffTime, diffUTCTime, nominalDay)
import Data.Time.Format (parseTimeM)
import Data.Text (pack)
import qualified Data.Text as T
import qualified Data.List as List
import Database.SQLite.Simple (Only(..))
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
                    -- Note: last_checked_at is not updated during initial follow, only during refresh
                    now <- liftIO getCurrentTime
                    liftIO $ publishAndLog bus le "catalog" $ ArtistDiscographyFetched
                      { artistDiscographyArtistId = artistId
                      , artistMBID = artistMBID
                      , artistDiscographyArtistName = artistName
                      , releaseGroupCount = length releaseGroups
                      , artistLastCheckedAt = pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" now
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
                          -- New album - add to catalog (wanted status derived from quality profile)
                          albumId <- liftIO $ withConnection pool $ \conn ->
                            upsertCatalogAlbum conn rgId title artistId artistMBID artistName albumType firstReleaseDate

                          -- Resolve quality profile: artist > default (no source for catalog refresh)
                          resolvedProfileId <- liftIO $ withConnection pool $ \conn ->
                            resolveQualityProfileId conn Nothing (Just artistId) Nothing

                          -- Set the resolved profile on the album
                          case resolvedProfileId of
                            Just _pid -> liftIO $ withConnection pool $ \conn ->
                              updateCatalogAlbum conn albumId (Just resolvedProfileId)
                            Nothing -> pure ()

                          $(logTM) InfoS $ logStr $ ("Added album to catalog: " <> title :: Text)

                          -- Compute wanted status: album is wanted if it got a quality profile
                          let isWanted = isJust resolvedProfileId

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
                            , catalogAlbumWanted = isWanted
                            }

-- | Handle a catalog artist refresh request.
--
-- Re-fetches the artist's discography from MusicBrainz using conditional requests
-- (ETags). Updates existing album metadata when it has changed and adds new albums.
-- Triggers diff regeneration for affected library clusters.
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
          let storedEtag = DB.catalogArtistEtag artist

          katipAddContext (sl "artist_name" artistName) $ do
            $(logTM) InfoS $ logStr $ ("Refreshing discography for: " <> artistName :: Text)

            -- Fetch artist discography from MusicBrainz using conditional request
            result <- liftIO $ getArtistConditional mbEnv (MBID artistMBID) storedEtag

            case result of
              Left err -> do
                $(logTM) ErrorS $ logStr $ ("Failed to fetch artist discography: " <> show err :: Text)

              Right NotModified -> do
                -- Data hasn't changed, just update last_checked_at
                now <- liftIO getCurrentTime
                case DB.catalogArtistId artist of
                  Just artistId -> do
                    liftIO $ withConnection pool $ \conn ->
                      executeQuery conn
                        "UPDATE catalog_artists SET last_checked_at = ? WHERE id = ?"
                        (now, artistId)
                    $(logTM) InfoS $ logStr $ ("Catalog unchanged for " <> artistName <> " (304 Not Modified)" :: Text)

                    -- Backfill bio from Last.fm if missing
                    when (isNothing (DB.catalogArtistBio artist)) $ do
                      _ <- liftIO $ async $ do
                        let apiKey = fromMaybe LastFM.defaultApiKey (mediaLastFmApiKey (media config))
                        bioResult <- LastFM.fetchArtistBio catHttpClient apiKey (MBID artistMBID)
                        case bioResult of
                          Right bio -> do
                            withConnection pool $ \conn ->
                              updateCatalogArtistBio conn artistId (Just bio)
                            publishAndLog bus le "catalog.bio" $ ArtistBioFetched
                              { artistBioId = artistId
                              , artistBioText = bio
                              }
                          Left _ -> pure ()
                      pure ()
                  Nothing -> pure ()

              Right (Modified mbArtist newEtag) -> do
                let allReleaseGroups = mbArtistReleaseGroups mbArtist
                -- Filter release groups by configured album types and excluded secondary types
                let releaseGroups = filter (shouldIncludeReleaseGroup allowedTypes excludedSecondaryTypes) allReleaseGroups

                -- Update last_checked_at timestamp and ETag
                now <- liftIO getCurrentTime
                case DB.catalogArtistId artist of
                  Just artistId -> do
                    liftIO $ withConnection pool $ \conn -> do
                      executeQuery conn
                        "UPDATE catalog_artists SET last_checked_at = ? WHERE id = ?"
                        (now, artistId)
                      updateCatalogArtistEtag conn artistId (if T.null newEtag then Nothing else Just newEtag)

                    -- Check if artist name changed in MusicBrainz
                    when (mbArtistName mbArtist /= artistName) $ do
                      $(logTM) InfoS $ logStr $ ("Artist name changed: " <> artistName <> " -> " <> mbArtistName mbArtist :: Text)
                      liftIO $ withConnection pool $ \conn ->
                        updateCatalogArtistName conn artistId (mbArtistName mbArtist)

                    let currentArtistName = mbArtistName mbArtist

                    -- Emit event for observability with updated timestamp
                    liftIO $ publishAndLog bus le "catalog" $ ArtistDiscographyFetched
                      { artistDiscographyArtistId = artistId
                      , artistMBID = artistMBID
                      , artistDiscographyArtistName = currentArtistName
                      , releaseGroupCount = length releaseGroups
                      , artistLastCheckedAt = pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" now
                      }

                    -- Refresh bio from Last.fm
                    _ <- liftIO $ async $ do
                      let apiKey = fromMaybe LastFM.defaultApiKey (mediaLastFmApiKey (media config))
                      bioResult <- LastFM.fetchArtistBio catHttpClient apiKey (MBID artistMBID)
                      case bioResult of
                        Right bio -> do
                          withConnection pool $ \conn ->
                            updateCatalogArtistBio conn artistId (Just bio)
                          publishAndLog bus le "catalog.bio" $ ArtistBioFetched
                            { artistBioId = artistId
                            , artistBioText = bio
                            }
                        Left _ -> pure ()

                    -- Process albums: add new ones, update existing ones
                    newAlbumsCount <- liftIO $ newIORef (0 :: Int)
                    updatedAlbumsCount <- liftIO $ newIORef (0 :: Int)

                    forM_ releaseGroups $ \rg -> do
                      let rgId = unMBID (mbrgId rg)
                      let title = mbrgTitle rg
                      let firstReleaseDate = mbrgFirstReleaseDate rg
                      let albumType = mbrgType rg

                      -- Check if album already exists in catalog
                      existingAlbum <- liftIO $ withConnection pool $ \conn ->
                        getCatalogAlbumByReleaseGroupMBID conn rgId

                      case existingAlbum of
                        Just existing -> do
                          -- Album exists - check if metadata changed
                          let titleChanged = DB.catalogAlbumTitle existing /= title
                          let typeChanged = DB.catalogAlbumType existing /= albumType
                          let dateChanged = DB.catalogAlbumFirstReleaseDate existing /= firstReleaseDate
                          let nameChanged = DB.catalogAlbumArtistName existing /= currentArtistName

                          when (titleChanged || typeChanged || dateChanged || nameChanged) $ do
                            -- Update via upsert (ON CONFLICT handles the update)
                            _ <- liftIO $ withConnection pool $ \conn ->
                              upsertCatalogAlbum conn rgId title artistId artistMBID currentArtistName albumType firstReleaseDate
                            liftIO $ modifyIORef' updatedAlbumsCount (+1)
                            $(logTM) InfoS $ logStr $ ("Album metadata updated: " <> title :: Text)

                            -- Find clusters matched to this release group and trigger diff regeneration
                            affectedClusters <- liftIO $ withConnection pool $ \conn ->
                              queryRows conn
                                "SELECT id, mb_release_id, mb_confidence, track_count FROM clusters WHERE mb_release_group_id = ? AND mb_release_id IS NOT NULL"
                                (Only rgId) :: IO [(Int64, Text, Maybe Double, Int)]

                            forM_ affectedClusters $ \(cid, releaseId, confidence, trackCount) -> do
                              $(logTM) InfoS $ logStr $ ("Triggering diff regeneration for cluster " <> show cid :: Text)
                              liftIO $ publishAndLog bus le "catalog" $ ClusterIdentified
                                { identifiedClusterId = cid
                                , identifiedReleaseId = releaseId
                                , identifiedReleaseGroupId = Just rgId
                                , identifiedConfidence = fromMaybe 1.0 confidence
                                , identifiedTrackCount = trackCount
                                }

                        Nothing -> do
                          -- New album discovered! Add to catalog
                          albumId <- liftIO $ withConnection pool $ \conn ->
                            upsertCatalogAlbum conn rgId title artistId artistMBID currentArtistName albumType firstReleaseDate

                          -- Resolve quality profile: artist > default (no source for catalog refresh)
                          resolvedProfileId <- liftIO $ withConnection pool $ \conn ->
                            resolveQualityProfileId conn Nothing (Just artistId) Nothing

                          -- Set the resolved profile on the album
                          case resolvedProfileId of
                            Just _pid -> liftIO $ withConnection pool $ \conn ->
                              updateCatalogAlbum conn albumId (Just resolvedProfileId)
                            Nothing -> pure ()

                          liftIO $ modifyIORef' newAlbumsCount (+1)
                          $(logTM) InfoS $ logStr $ ("NEW album discovered: " <> title :: Text)

                          -- Compute wanted status: album is wanted if it got a quality profile
                          let isWanted = isJust resolvedProfileId

                          -- Emit CatalogAlbumAdded event with complete album data
                          liftIO $ publishAndLog bus le "catalog" $ CatalogAlbumAdded
                            { catalogAlbumId = albumId
                            , catalogAlbumReleaseGroupMBID = rgId
                            , catalogAlbumTitle = title
                            , catalogAlbumArtistId = artistId
                            , catalogAlbumArtistMBID = artistMBID
                            , catalogAlbumArtistName = currentArtistName
                            , catalogAlbumType = albumType
                            , catalogAlbumFirstReleaseDate = firstReleaseDate
                            , catalogAlbumWanted = isWanted
                            }

                    newCount <- liftIO $ readIORef newAlbumsCount
                    updatedCount <- liftIO $ readIORef updatedAlbumsCount
                    let totalAlbums = length releaseGroups
                    $(logTM) InfoS $ logStr $ ("Catalog refresh complete for " <> currentArtistName <> " (total: " <> show totalAlbums <> ", new: " <> show newCount <> ", updated: " <> show updatedCount <> ")" :: Text)

                  Nothing -> pure ()

-- | Start the catalog refresh scheduler.
--
-- Periodically checks which followed artists are due for a refresh based on
-- smart scheduling tiers (albums released recently are checked more frequently).
-- Emits CatalogArtistRefreshRequested events for artists that are due.
startCatalogRefreshScheduler :: CatalogDeps -> IO (Async ())
startCatalogRefreshScheduler deps = async $ do
  let le = catLogEnv deps
  let pool = catDbPool deps
  let bus = catEventBus deps

  -- Wait 5 minutes before first check to let the system stabilize on startup
  threadDelay (5 * 60 * 1000000)

  runKatipContextT le () "catalog-refresh" $ do
    $(logTM) InfoS "Starting catalog refresh scheduler"

    forever $ do
      -- Get all followed artists
      artists <- liftIO $ withConnection pool $ \conn ->
        getCatalogArtists conn (Just True) Nothing Nothing Nothing

      now <- liftIO getCurrentTime
      scheduledCount <- liftIO $ newIORef (0 :: Int)

      forM_ artists $ \artist -> do
        case DB.catalogArtistId artist of
          Nothing -> pure ()
          Just artistId -> do
            -- Get this artist's albums to compute freshness
            albums <- liftIO $ withConnection pool $ \conn ->
              getCatalogAlbumsByArtistId conn artistId

            let interval = computeRefreshInterval now albums
            let isDue = case DB.catalogArtistLastCheckedAt artist of
                  Nothing -> True  -- Never checked
                  Just checked -> diffUTCTime now checked >= interval

            when isDue $ do
              liftIO $ modifyIORef' scheduledCount (+1)
              liftIO $ publishAndLog bus le "catalog-refresh" $
                CatalogArtistRefreshRequested { refreshArtistMBID = DB.catalogArtistMBID artist }

      count <- liftIO $ readIORef scheduledCount
      when (count > 0) $
        $(logTM) InfoS $ logStr $ ("Scheduled refresh for " <> show count <> " artists" :: Text)

      -- Run scheduler every hour
      liftIO $ threadDelay (60 * 60 * 1000000)

-- | Compute the refresh interval for an artist based on album freshness.
--
-- Artists with upcoming or recently released albums are checked more frequently:
-- - Has upcoming/future-dated album: every 12 hours
-- - Most recent album < 3 months: every 1 day
-- - Most recent album < 1 year: every 3 days
-- - Most recent album < 3 years: every 1 week
-- - All albums older than 3 years: every 2 weeks
computeRefreshInterval :: UTCTime -> [DB.CatalogAlbumRecord] -> NominalDiffTime
computeRefreshInterval now albums =
  let parsedDates = mapMaybe (DB.catalogAlbumFirstReleaseDate >=> parseAlbumDate) albums
      hasFutureAlbum = any (> now) parsedDates
      daysSinceLatest = case filter (<= now) parsedDates of
        [] -> 365 * 5  -- No past albums, check infrequently
        dates -> floor (diffUTCTime now (List.maximum dates) / nominalDay) :: Int
  in if hasFutureAlbum
     then 12 * 3600                -- 12 hours
     else if daysSinceLatest < 90
     then 24 * 3600                -- 1 day
     else if daysSinceLatest < 365
     then 3 * nominalDay           -- 3 days
     else if daysSinceLatest < 365 * 3
     then 7 * nominalDay           -- 1 week
     else 14 * nominalDay          -- 2 weeks

-- | Parse a MusicBrainz date string into UTCTime.
-- Handles "YYYY", "YYYY-MM", and "YYYY-MM-DD" formats.
parseAlbumDate :: Text -> Maybe UTCTime
parseAlbumDate dateText =
  let s = T.unpack dateText
  in parseTimeM True defaultTimeLocale "%Y-%m-%d" s
     <|> parseTimeM True defaultTimeLocale "%Y-%m" s
     <|> parseTimeM True defaultTimeLocale "%Y" s

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
