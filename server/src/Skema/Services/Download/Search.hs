{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Search orchestration for wanted albums.
--
-- This module handles searching across multiple indexers to find the best
-- release for a wanted album, then submitting it to a download client.
module Skema.Services.Download.Search
  ( runSearchOrchestrator
  , handleWantedAlbumAdded
  ) where

import Skema.Services.Dependencies (DownloadDeps(..))
import Skema.Services.Download.Scoring (scoreRelease)
import Skema.Services.Download.Submission (submitDownload, DownloadSubmissionContext(..))
import Skema.Events.Bus
import Skema.Events.Types
import Skema.Database.Connection
import Skema.Database.Repository
import Skema.HTTP.Client (HttpClient)
import Skema.Indexer.Search (rankResultsWithContext)
import Skema.Indexer.Client (searchIndexer)
import Skema.Indexer.Types
import Skema.Config.Types (Config(..), Indexer(..), IndexerConfig(..))
import Skema.Domain.Quality (meetsProfile, isBetterQuality, textToQuality)
import Database.SQLite.Simple (Only(..))
import Control.Concurrent.Async (async, race, mapConcurrently)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (readTChan)
import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Data.Time (getCurrentTime, diffUTCTime)
import qualified Data.Map.Strict as M
import Katip

-- ============================================================================
-- CONSTANTS
-- ============================================================================

-- | Timeout for multi-indexer search (30 seconds in microseconds)
searchTimeoutMicros :: Int
searchTimeoutMicros = 30 * 1000000

-- | Newznab categories for audio content
-- 3000 = Audio, 3010 = MP3, 3020 = FLAC
audioCategories :: [Int]
audioCategories = [3000, 3010, 3020]

-- | Maximum number of results per indexer search
maxResultsPerIndexer :: Int
maxResultsPerIndexer = 50

-- ============================================================================
-- SEARCH ORCHESTRATOR
-- ============================================================================

-- | Run the search orchestrator sub-process.
--
-- Listens for WantedAlbumAdded events and coordinates searching across
-- all indexers to find the best release.
runSearchOrchestrator :: DownloadDeps -> IO ()
runSearchOrchestrator deps = do
  chan <- STM.atomically $ subscribe (dlEventBus deps)
  _ <- async $ forever $ do
    envelope <- STM.atomically $ readTChan chan
    case envelopeEvent envelope of
      WantedAlbumAdded{..} -> do
        -- Process each event asynchronously to avoid blocking the event loop
        _ <- async $ do
          result <- try $ handleWantedAlbumAdded deps wantedCatalogAlbumId wantedReleaseGroupId wantedAlbumTitle wantedArtistName
          case result of
            Left (e :: SomeException) -> do
              let le = dlLogEnv deps
              runKatipContextT le () "download.search" $ do
                $(logTM) ErrorS $ logStr $ ("Exception searching for album " <> wantedAlbumTitle <> ": " <> show e :: Text)
            Right () -> pure ()
        pure ()
      _ -> pure ()  -- Ignore other events
  pure ()

-- | Handle a wanted album added event by searching indexers.
handleWantedAlbumAdded :: DownloadDeps -> Int64 -> Text -> Text -> Text -> IO ()
handleWantedAlbumAdded DownloadDeps{..} catalogAlbumId releaseGroupId albumTitle artistName = do
  let le = dlLogEnv
  let pool = dlDbPool
  let bus = dlEventBus
  let httpClient = dlHttpClient

  -- Read current config from TVar
  config <- STM.atomically $ STM.readTVar dlConfigVar

  let indexerConfig = indexers config
  let downloadConfig = download config
  let initialContext = ()
  let initialNamespace = "download.search"

  runKatipContextT le initialContext initialNamespace $ do
    katipAddContext (sl "release_group_id" releaseGroupId) $ do
      katipAddContext (sl "album_title" albumTitle) $ do
        katipAddContext (sl "artist_name" artistName) $ do
          $(logTM) InfoS $ logStr $ ("RECEIVED WANTED_ALBUM_ADDED event for: " <> albumTitle :: Text)

          -- Count enabled indexers
          let enabledIndexers = filter indexerEnabled (indexerList indexerConfig)
              indexerCount = length enabledIndexers

          when (indexerCount == 0) $ do
            $(logTM) WarningS $ logStr ("No enabled indexers, cannot search for album" :: Text)

          -- Emit search started event
          liftIO $ publishAndLog bus le "download" $ AlbumSearchStarted
            { searchAlbumTitle = albumTitle
            , searchArtistName = artistName
            , searchIndexerCount = indexerCount
            }

          $(logTM) InfoS $ logStr $ ("Searching " <> show indexerCount <> " indexers for: " <> albumTitle :: Text)

          -- Start concurrent search with timeout
          startTime <- liftIO getCurrentTime

          searchResult <- liftIO $ race
            (threadDelay searchTimeoutMicros)
            (searchAllIndexersWithTracking bus le httpClient indexerConfig albumTitle artistName)

          endTime <- liftIO getCurrentTime
          let searchDuration = realToFrac $ diffUTCTime endTime startTime :: Double

          case searchResult of
            Left () -> do
              -- Timeout occurred
              $(logTM) WarningS $ logStr $ ("Search timed out after " <> show (searchTimeoutMicros `div` 1000000) <> " seconds" :: Text)
              liftIO $ publishAndLog bus le "download" $ AlbumSearchCompleted
                { searchTotalResults = 0
                , searchBestScore = Nothing
                , searchDuration = searchDuration
                }

            Right searchResults -> do
              -- Got results from indexers
              -- Pair each release with its source indexer name
              let releasesWithSource = concat $ map (\ir -> map (\r -> (irIndexerName ir, r)) (irReleases ir)) (srResults searchResults)
                  allReleases = map snd releasesWithSource
                  totalResults = length allReleases

              $(logTM) InfoS $ logStr $ ("Found " <> show totalResults <> " total releases across all indexers" :: Text)

              -- Get quality profile and current quality for filtering
              qualityProfile <- liftIO $ getEffectiveQualityProfile pool catalogAlbumId

              -- Get current quality from catalog album
              currentQuality <- liftIO $ withConnection pool $ \conn -> do
                results <- queryRows conn
                  "SELECT current_quality FROM catalog_albums WHERE id = ?"
                  (Only catalogAlbumId) :: IO [Only (Maybe Text)]
                case results of
                  [Only maybeQualText] -> pure $ maybeQualText >>= textToQuality
                  _ -> pure Nothing

              -- Filter releases by quality profile (keep indexer name paired)
              let qualifyingReleasesWithSource = case qualityProfile of
                    Nothing -> releasesWithSource  -- No profile = accept all
                    Just profile -> filter (\(_, release) ->
                      let releaseQuality = riQuality release
                          meetsRequirement = meetsProfile releaseQuality profile
                          isUpgrade = case currentQuality of
                            Nothing -> True  -- No current quality, any quality is an upgrade
                            Just currentQual -> isBetterQuality profile releaseQuality currentQual
                      in meetsRequirement && isUpgrade
                      ) releasesWithSource

              let qualifiedCount = length qualifyingReleasesWithSource

              when (qualifiedCount < totalResults) $ do
                $(logTM) InfoS $ logStr $ ("Filtered to " <> show qualifiedCount <> " releases that meet quality requirements" :: Text)

              -- Rank qualified results by quality (with album title context for better matching)
              -- Keep track of indexer names with a Map using download URL as key
              let qualifyingReleases = map snd qualifyingReleasesWithSource
                  indexerMap = M.fromList $ map (\(name, r) -> (riDownloadUrl r, name)) qualifyingReleasesWithSource
                  rankedReleases = rankResultsWithContext (Just albumTitle) qualifyingReleases
                  bestRelease = listToMaybe rankedReleases
                  -- Find the indexer name for the best release using its download URL
                  bestIndexerName = bestRelease >>= \r -> M.lookup (riDownloadUrl r) indexerMap

              -- Calculate best score
              let bestScore = case bestRelease of
                    Just rel -> Just $ scoreRelease rel
                    Nothing -> Nothing

              -- Emit search completed event
              liftIO $ publishAndLog bus le "download" $ AlbumSearchCompleted
                { searchTotalResults = qualifiedCount  -- Use qualified count instead of total
                , searchBestScore = bestScore
                , searchDuration = searchDuration
                }

              -- If we found releases, pick the best one and queue download
              case bestRelease of
                Nothing -> do
                  $(logTM) WarningS $ logStr ("No releases found for album" :: Text)

                Just release -> do
                  let indexerName = fromMaybe "unknown" bestIndexerName
                  $(logTM) InfoS $ logStr $ ("Best release: " <> riTitle release <> " from " <> indexerName :: Text)

                  -- Emit best release selected event
                  liftIO $ publishAndLog bus le "download" $ BestReleaseSelected
                    { selectedTitle = riTitle release
                    , selectedIndexer = indexerName
                    , selectedScore = scoreRelease release
                    , selectedSeeders = riSeeders release
                    }

                  -- Submit download using shared submission module
                  let submissionCtx = DownloadSubmissionContext
                        { dscEventBus = bus
                        , dscLogEnv = le
                        , dscDbPool = pool
                        , dscHttpClient = httpClient
                        , dscDownloadConfig = downloadConfig
                        , dscIndexerName = indexerName
                        }

                  _ <- liftIO $ submitDownload submissionCtx release catalogAlbumId
                  pure ()

-- | Search all indexers and emit events for each completion.
searchAllIndexersWithTracking :: EventBus -> LogEnv -> HttpClient -> IndexerConfig -> Text -> Text -> IO SearchResult
searchAllIndexersWithTracking bus le httpClient indexerConfig albumTitle artistName = do
  let enabledIndexers = filter indexerEnabled (indexerList indexerConfig)

  -- Search each indexer and track completion
  indexerResults <- mapConcurrently (\indexer -> do
    startTime <- getCurrentTime
    result <- searchIndexer httpClient indexer (buildSearchQuery albumTitle artistName)
    endTime <- getCurrentTime
    let duration = realToFrac $ diffUTCTime endTime startTime :: Double

    case result of
      Left err -> do
        runKatipContextT le () "download.search" $ do
          $(logTM) WarningS $ logStr $ ("Indexer " <> indexerName indexer <> " failed: " <> ieError err :: Text)
        publishAndLog bus le "download" $ IndexerSearchCompleted
          { searchIndexerName = indexerName indexer
          , searchResultCount = 0
          , searchDuration = duration
          }
        pure $ Left err

      Right indexerResult -> do
        let resultCount = length (irReleases indexerResult)
        runKatipContextT le () "download.search" $ do
          $(logTM) InfoS $ logStr $ ("Indexer " <> indexerName indexer <> " returned " <> show resultCount <> " results" :: Text)
        publishAndLog bus le "download" $ IndexerSearchCompleted
          { searchIndexerName = indexerName indexer
          , searchResultCount = resultCount
          , searchDuration = duration
          }
        pure $ Right indexerResult
    ) enabledIndexers

  -- Partition results into successes and failures
  let (errors, successes) = partitionEithers indexerResults
      totalReleases = sum $ map (length . irReleases) successes

  pure SearchResult
    { srResults = successes
    , srErrors = errors
    , srTotalReleases = totalReleases
    }

-- | Build a search query from album and artist name.
buildSearchQuery :: Text -> Text -> SearchQuery
buildSearchQuery albumTitle artistName = SearchQuery
  { sqArtist = Just artistName
  , sqAlbum = Just albumTitle
  , sqYear = Nothing
  , sqQuery = Nothing
  , sqCategories = audioCategories
  , sqLimit = maxResultsPerIndexer
  , sqOffset = 0
  }
