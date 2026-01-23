{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | RSS monitoring service for indexers.
--
-- Monitors indexer RSS feeds for new releases and automatically matches them
-- against wanted albums. This is more efficient than repeatedly searching
-- for each wanted album.
module Skema.Services.Download.RSSMonitor
  ( runRSSMonitor
  , IndexerRSSState(..)
  , getIndexerState
  , updateIndexerState
  , detectIndexerCapabilities
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import qualified Data.Text as T
import qualified Data.List as List
import Database.SQLite.Simple (Only(..))
import Katip

import Skema.Config.Types (Config(..), Indexer(..), IndexerConfig(..))
import Skema.Database.Connection (ConnectionPool, withConnection, queryRows, executeQuery)
import Skema.Events.Bus (EventBus)
import Skema.HTTP.Client (HttpClient)
import Skema.Indexer.Types (IndexerResult(..), IndexerError(..), SearchQuery(..), ReleaseInfo(..))
import qualified Skema.Indexer.Client as IndexerClient
import qualified Database.SQLite.Simple as SQLite
import Skema.Domain.Quality (Quality, QualityProfile, textToQuality, meetsProfile, isBetterQuality)
import qualified Skema.Domain.Quality as Qual
import Skema.Database.Repository.Quality (getEffectiveQualityProfile)
import Skema.Services.Download.Submission (submitDownload, DownloadSubmissionContext(..))

-- | RSS state for an indexer
data IndexerRSSState = IndexerRSSState
  { irsUrl :: Text
  , irsName :: Text
  , irsLastSeenGuid :: Maybe Text
  , irsLastCheckAt :: Maybe UTCTime
  , irsLastSuccessfulCheckAt :: Maybe UTCTime
  , irsSupportsRSSPagination :: Maybe Bool
  , irsCapabilitiesDetectedAt :: Maybe UTCTime
  , irsConsecutiveFailures :: Int
  , irsCreatedAt :: UTCTime
  , irsUpdatedAt :: UTCTime
  } deriving (Show, Eq)

-- | RSS monitoring settings
data RSSSettings = RSSSettings
  { rssEnabled :: Bool
  , rssSyncIntervalSeconds :: Int
  , rssSyncMaxThresholdHours :: Int
  } deriving (Show, Eq)

-- | Get or create RSS state for an indexer
getIndexerState :: ConnectionPool -> Indexer -> IO (Maybe IndexerRSSState)
getIndexerState pool indexer = withConnection pool $ \conn -> do
  let url = indexerUrl indexer
  results <- SQLite.query conn
    "SELECT url, name, last_seen_guid, last_check_at, last_successful_check_at, \
    \       supports_rss_pagination, capabilities_detected_at, consecutive_failures, \
    \       created_at, updated_at \
    \FROM indexer_rss_state WHERE url = ?"
    (Only url)
  case results of
    [(url', name, lastGuid, lastCheck, lastSuccess, supportsPagination, capsDetected, failures, created, updated)] ->
      pure $ Just $ IndexerRSSState url' name lastGuid lastCheck lastSuccess supportsPagination capsDetected failures created updated
    _ -> do
      -- Create new state
      now <- getCurrentTime
      executeQuery conn
        "INSERT INTO indexer_rss_state (url, name, created_at, updated_at) VALUES (?, ?, ?, ?)"
        (url, indexerName indexer, now, now)
      -- Return the newly created state
      pure $ Just $ IndexerRSSState url (indexerName indexer) Nothing Nothing Nothing Nothing Nothing 0 now now

-- | Update RSS state for an indexer
updateIndexerState :: ConnectionPool -> IndexerRSSState -> IO ()
updateIndexerState pool rssState = withConnection pool $ \conn -> do
  now <- getCurrentTime
  executeQuery conn
    "UPDATE indexer_rss_state SET \
    \  name = ?, \
    \  last_seen_guid = ?, \
    \  last_check_at = ?, \
    \  last_successful_check_at = ?, \
    \  supports_rss_pagination = ?, \
    \  capabilities_detected_at = ?, \
    \  consecutive_failures = ?, \
    \  updated_at = ? \
    \WHERE url = ?"
    ( irsName rssState
    , irsLastSeenGuid rssState
    , irsLastCheckAt rssState
    , irsLastSuccessfulCheckAt rssState
    , irsSupportsRSSPagination rssState
    , irsCapabilitiesDetectedAt rssState
    , irsConsecutiveFailures rssState
    , now
    , irsUrl rssState
    )

-- | Get RSS monitoring settings
getRSSSettings :: ConnectionPool -> IO (Maybe RSSSettings)
getRSSSettings pool = withConnection pool $ \conn -> do
  results <- SQLite.query conn
    "SELECT rss_sync_enabled, rss_sync_interval_seconds, rss_sync_max_threshold_hours \
    \FROM settings WHERE id = 1"
    ()
  case results of
    [(enabled :: Int, interval :: Int, threshold :: Int)] ->
      pure $ Just $ RSSSettings (enabled == 1) interval threshold
    _ -> pure Nothing

-- | Check if there are any wanted albums
hasWantedAlbums :: ConnectionPool -> IO Bool
hasWantedAlbums pool = withConnection pool $ \conn -> do
  results <- SQLite.query conn
    "SELECT COUNT(*) FROM catalog_albums WHERE quality_profile_id IS NOT NULL"
    () :: IO [Only Int]
  case results of
    [Only count] -> pure (count > 0)
    _ -> pure False

-- | Fetch RSS feed from an indexer (no search query = RSS mode)
fetchRSSFeed :: HttpClient -> Indexer -> Int -> Int -> IO (Either Text [IndexerResult])
fetchRSSFeed httpClient indexer limit offset = do
  let query = SearchQuery
        { sqArtist = Nothing
        , sqAlbum = Nothing
        , sqYear = Nothing
        , sqQuery = Nothing  -- Empty query triggers RSS mode
        , sqCategories = [3000, 3010, 3020]  -- Audio categories
        , sqLimit = limit
        , sqOffset = offset
        }

  result <- IndexerClient.searchIndexer httpClient indexer query
  case result of
    Left err -> pure $ Left $ ieError err
    Right indexerResult -> pure $ Right [indexerResult]

-- | Detect if indexer supports RSS pagination
detectIndexerCapabilities :: HttpClient -> Indexer -> IO (Maybe Bool)
detectIndexerCapabilities httpClient indexer = do
  -- Fetch two pages and check if they're different
  result1 <- try @SomeException $ fetchRSSFeed httpClient indexer 10 0
  result2 <- try @SomeException $ fetchRSSFeed httpClient indexer 10 10

  case (result1, result2) of
    (Right (Right [res1]), Right (Right [res2])) -> do
      -- Check if the results are different (indicates pagination works)
      let guids1 = mapMaybe riGuid (irReleases res1)
      let guids2 = mapMaybe riGuid (irReleases res2)
      let hasDifferent = not $ any (`elem` guids2) guids1
      pure $ Just hasDifferent
    _ -> pure $ Just False  -- If fetching failed, assume no pagination

-- | Wanted album information with quality profile
data WantedAlbumInfo = WantedAlbumInfo
  { waiAlbumId :: Int64
  , waiTitle :: Text
  , waiArtistName :: Text
  , waiCurrentQuality :: Maybe Quality
  , waiQualityProfile :: Maybe QualityProfile
  } deriving (Show, Eq)

-- | Get all wanted albums from database with quality profile info
getWantedAlbums :: ConnectionPool -> IO [WantedAlbumInfo]
getWantedAlbums pool = withConnection pool $ \conn -> do
  results <- queryRows conn
    "SELECT id, title, artist_name, current_quality FROM catalog_albums WHERE quality_profile_id IS NOT NULL"
    () :: IO [(Int64, Text, Text, Maybe Text)]

  -- For each album, get its effective quality profile
  forM results $ \(albumId, title, artistName, maybeQualityText) -> do
    let currentQuality = maybeQualityText >>= textToQuality
    qualityProfile <- getEffectiveQualityProfile pool albumId
    pure $ WantedAlbumInfo albumId title artistName currentQuality qualityProfile

-- | Resync RSS feed on startup (find last GUID or fetch recent)
resyncRSSFeed :: HttpClient -> Indexer -> IndexerRSSState -> Int -> IO (Either Text [ReleaseInfo])
resyncRSSFeed httpClient indexer rssState thresholdHours = do
  now <- getCurrentTime

  case irsLastCheckAt rssState of
    Nothing -> do
      -- First run, just get recent RSS
      result <- fetchRSSFeed httpClient indexer 100 0
      case result of
        Right [indexerResult] -> pure $ Right (irReleases indexerResult)
        Right _ -> pure $ Right []
        Left err -> pure $ Left err

    Just lastCheck -> do
      let downtimeHours = floor $ diffUTCTime now lastCheck / 3600

      if downtimeHours > (fromIntegral thresholdHours :: Integer)
        then pure $ Left "Downtime exceeded threshold, needs explicit search"
        else do
          -- Try to find last GUID in recent feed
          result <- fetchRSSFeed httpClient indexer 100 0
          case result of
            Right [indexerResult] -> do
              let releases = irReleases indexerResult
              case irsLastSeenGuid rssState of
                Nothing -> pure $ Right releases
                Just lastGuid -> do
                  -- Find index of last GUID
                  let guidIndex = List.findIndex (\r -> riGuid r == Just lastGuid) releases
                  case guidIndex of
                    Just idx -> pure $ Right (take idx releases)  -- New releases since last GUID
                    Nothing -> pure $ Right releases  -- GUID not found, return all recent
            Right _ -> pure $ Right []
            Left err -> pure $ Left err

-- | Check if a release matches an album and meets quality requirements
matchesAndMeetsQuality :: ReleaseInfo -> WantedAlbumInfo -> Bool
matchesAndMeetsQuality release album =
  let releaseTitle = T.toLower (riTitle release)
      albumTitle = T.toLower (waiTitle album)
      artistName = T.toLower (waiArtistName album)

      -- Check basic title/artist match
      titleMatches = T.isInfixOf albumTitle releaseTitle && T.isInfixOf artistName releaseTitle

      -- Parse quality from release
      releaseQuality = riQuality release  -- Already parsed from title

  in if not titleMatches
     then False
     else case waiQualityProfile album of
       -- No quality profile: accept any match
       Nothing -> True

       -- Has quality profile: check if quality is acceptable
       Just profile ->
         let qualityOk = meetsProfile releaseQuality profile

             -- Check if this is an upgrade
             isUpgrade = case waiCurrentQuality album of
               Nothing -> True  -- No current quality, this is first download
               Just currentQual -> isBetterQuality profile releaseQuality currentQual

         in qualityOk && isUpgrade

-- | Process RSS releases and match against wanted albums with quality filtering.
-- When multiple releases match the same album, select the one that best fits the quality profile.
processRSSReleases :: ConnectionPool -> [ReleaseInfo] -> [WantedAlbumInfo] -> IO [(ReleaseInfo, WantedAlbumInfo)]
processRSSReleases _pool releases wantedAlbums = do
  -- Match each release against each wanted album, filtering by quality
  let allMatches = [(release, album) | release <- releases, album <- wantedAlbums, matchesAndMeetsQuality release album]

  -- Group matches by album ID
  let groupedByAlbum = List.groupBy (\(_, a1) (_, a2) -> waiAlbumId a1 == waiAlbumId a2)
                     $ List.sortOn (waiAlbumId . snd) allMatches

  -- For each album, select the release with the best quality according to profile
  let bestMatches = mapMaybe selectBestMatch groupedByAlbum

  pure bestMatches
  where
    selectBestMatch :: [(ReleaseInfo, WantedAlbumInfo)] -> Maybe (ReleaseInfo, WantedAlbumInfo)
    selectBestMatch [] = Nothing
    selectBestMatch matches@((_, album):_) = do
      profile <- waiQualityProfile album
      let releaseQualities = [(release, riQuality release) | (release, _) <- matches]
      bestQuality <- Qual.selectBestQuality (map snd releaseQualities) profile
      -- Find the release with the best quality
      (bestRelease, _) <- List.find (\(_, q) -> q == bestQuality) releaseQualities
      pure (bestRelease, album)

-- | Run the RSS monitoring service
runRSSMonitor :: LogEnv -> EventBus -> ConnectionPool -> HttpClient -> Config -> IO ()
runRSSMonitor le bus pool httpClient config = do
  let initialContext = ()
  let initialNamespace = "rss-monitor"

  runKatipContextT le initialContext initialNamespace $ do
    $(logTM) InfoS "Starting RSS monitoring service"

    forever $ do
      -- Check if RSS monitoring is enabled
      settings <- liftIO $ getRSSSettings pool

      case settings of
        Nothing -> do
          $(logTM) WarningS "RSS settings not found, using defaults"
          liftIO $ threadDelay (15 * 60 * 1000000) -- 15 minutes

        Just rssSettings | not (rssEnabled rssSettings) -> do
          $(logTM) DebugS "RSS monitoring is disabled, sleeping"
          liftIO $ threadDelay (60 * 1000000) -- 1 minute

        Just rssSettings -> do
          -- Check if there are wanted albums
          wantedExists <- liftIO $ hasWantedAlbums pool

          unless wantedExists $ do
            $(logTM) DebugS "No wanted albums, RSS monitor sleeping"
            liftIO $ threadDelay (60 * 1000000) -- 1 minute

          when wantedExists $ do
            $(logTM) DebugS "Checking RSS feeds for wanted albums"

            let indexerConfig = indexers config
            let enabledIndexers = filter indexerEnabled (indexerList indexerConfig)

            -- Process each indexer
            forM_ enabledIndexers $ \indexer -> do
              $(logTM) DebugS $ logStr $ "Checking RSS for indexer: " <> indexerName indexer

              -- Get or create state
              maybeState <- liftIO $ getIndexerState pool indexer

              case maybeState of
                Nothing -> $(logTM) WarningS $ logStr $ "Failed to get state for indexer: " <> indexerName indexer
                Just indexerState -> do
                  -- Check if we need to detect capabilities
                  updatedState <- if isNothing (irsSupportsRSSPagination indexerState)
                    then do
                      $(logTM) InfoS $ logStr $ "Detecting capabilities for: " <> indexerName indexer
                      supportsPagination <- liftIO $ detectIndexerCapabilities httpClient indexer
                      now <- liftIO getCurrentTime
                      let newState = indexerState
                            { irsSupportsRSSPagination = supportsPagination
                            , irsCapabilitiesDetectedAt = Just now
                            }
                      liftIO $ updateIndexerState pool newState
                      pure newState
                    else pure indexerState

                  -- Fetch RSS feed and match against wanted albums
                  syncResult <- liftIO $ resyncRSSFeed httpClient indexer updatedState (rssSyncMaxThresholdHours rssSettings)

                  case syncResult of
                    Left err -> do
                      $(logTM) WarningS $ logStr $ "RSS sync failed for " <> indexerName indexer <> ": " <> err
                      -- Update consecutive failures
                      liftIO $ updateIndexerState pool $ updatedState
                        { irsConsecutiveFailures = irsConsecutiveFailures updatedState + 1
                        }

                    Right releases -> do
                      $(logTM) InfoS $ logStr $ "Fetched " <> T.pack (show (length releases)) <> " releases from " <> indexerName indexer

                      -- Get wanted albums
                      wanted <- liftIO $ getWantedAlbums pool

                      -- Match releases to wanted albums
                      matches <- liftIO $ processRSSReleases pool releases wanted

                      $(logTM) InfoS $ logStr $ "Found " <> T.pack (show (length matches)) <> " matches for wanted albums"

                      -- Submit matched releases to download queue
                      let downloadConfig = download config
                      forM_ matches $ \(release, album) -> do
                        $(logTM) InfoS $ logStr $ "Submitting download: " <> riTitle release <> " for album: " <> waiTitle album

                        let submissionCtx = DownloadSubmissionContext
                              { dscEventBus = bus
                              , dscLogEnv = le
                              , dscDbPool = pool
                              , dscHttpClient = httpClient
                              , dscDownloadConfig = downloadConfig
                              , dscIndexerName = indexerName indexer
                              }

                        downloadIdMaybe <- liftIO $ submitDownload submissionCtx release (waiAlbumId album)

                        case downloadIdMaybe of
                          Just downloadId -> $(logTM) InfoS $ logStr $ ("Download submitted successfully with ID: " <> show downloadId :: Text)
                          Nothing -> $(logTM) WarningS $ logStr $ ("Failed to submit download for: " <> riTitle release :: Text)

                      -- Update state with last GUID and reset failures
                      now <- liftIO getCurrentTime
                      let newLastGuid = listToMaybe releases >>= riGuid
                      liftIO $ updateIndexerState pool $ updatedState
                        { irsLastSeenGuid = newLastGuid
                        , irsLastCheckAt = Just now
                        , irsLastSuccessfulCheckAt = Just now
                        , irsConsecutiveFailures = 0
                        }

                  $(logTM) DebugS $ logStr $ "RSS check completed for: " <> indexerName indexer

            -- Sleep for configured interval
            let intervalMicros = rssSyncIntervalSeconds rssSettings * 1000000
            liftIO $ threadDelay intervalMicros
