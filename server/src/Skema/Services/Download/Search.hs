{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Search orchestration for wanted albums.
--
-- This module handles searching across multiple indexers to find the best
-- release for a wanted album, then submitting it to a download client.
module Skema.Services.Download.Search
  ( startSearchOrchestrator
  , handleWantedAlbumAdded
    -- * Streaming release search
  , StreamEvent (..)
  , streamReleaseSearch
  ) where

import Skema.Services.Types (ServiceContext(..))
import Skema.Services.EventLoop (JobMode(..), runEventService)
import Skema.Domain.Scoring (scoreRelease, rankReleases)
import Skema.Services.Download.Submission (submitDownload, SubmissionMode(..), DownloadSubmissionContext(..))
import Skema.Domain.Download (releaseFailureKey)
import Skema.Events.Bus
import Skema.Events.Types
import Skema.Database.Connection
import Skema.Database.Repository
import Skema.Database.Types (DownloadRecord(..))
import qualified Skema.Database.Types as DB
import Skema.HTTP.Client (HttpClient)
import Skema.Indexer.Client (searchIndexer)
import Skema.Indexer.Types
import Skema.Config.Types (Config(..), DownloadConfig(..), Indexer(..), IndexerConfig(..), SlskdConfig(..))
import Skema.Domain.Quality (meetsProfile, isBetterQuality, textToQuality)
import Skema.Slskd.Client (createSlskdClient)
import Skema.Slskd.Search (searchSlskd, searchSlskdStreaming, slskdCandidateToReleaseInfo)
import Database.SQLite.Simple (Only(..))
import Control.Concurrent.Async (Async, async, wait, race, mapConcurrently, concurrently)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent (threadDelay)
import Data.Time (getCurrentTime, diffUTCTime)
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Katip

-- ============================================================================
-- CONSTANTS
-- ============================================================================

-- | Newznab categories for audio content
-- 3000 = Audio, 3010 = MP3, 3020 = FLAC
audioCategories :: [Int]
audioCategories = [3000, 3010, 3020]

-- ============================================================================
-- SEARCH ORCHESTRATOR
-- ============================================================================

-- | Start the search orchestrator sub-process.
--
-- Listens for WantedAlbumAdded events and coordinates searching across
-- all indexers to find the best release. Searches for different albums are
-- independent and slow (a slskd search alone can take a minute), so they
-- run concurrently.
startSearchOrchestrator :: ServiceContext -> IO (Async ())
startSearchOrchestrator deps =
  runEventService (scLogEnv deps) (scEventBus deps) "download.search" Concurrent $ \case
    WantedAlbumAdded{..} ->
      Just $ handleWantedAlbumAdded deps wantedCatalogAlbumId wantedReleaseGroupId wantedAlbumTitle wantedArtistName
    DownloadFailed{..} ->
      Just $ handleDownloadFailed deps downloadId
    _ -> Nothing

-- | Handle a download failure by looking up the album and auto-retrying with a new search.
handleDownloadFailed :: ServiceContext -> Int64 -> IO ()
handleDownloadFailed deps@ServiceContext{..} failedDownloadId = do
  let le = scLogEnv
  let pool = scDbPool

  -- Read the configured retry limit from current config
  config <- STM.atomically $ STM.readTVar scConfigVar
  let maxAutoRetries = downloadMaxSearchRetries (download config)

  -- Look up the failed download record
  maybeDownload <- withConnection pool $ \conn ->
    getDownloadById conn failedDownloadId
  case maybeDownload of
    Nothing -> pure ()  -- Download record gone, nothing to retry
    Just download -> do
      let albumId = downloadCatalogAlbumId download

      -- Check retry count
      failedCount <- withConnection pool $ \conn ->
        getFailedDownloadCountForAlbum conn albumId
      when (failedCount > maxAutoRetries) $ do
        runKatipContextT le () "download.search" $
          $(logTM) WarningS $ logStr $ ("Max retries (" <> show maxAutoRetries <> ") reached for album " <> show albumId <> ", not retrying" :: Text)

      when (failedCount <= maxAutoRetries) $ do
        -- Look up the catalog album for search context
        maybeAlbum <- withConnection pool $ \conn ->
          getCatalogAlbumById conn albumId
        case maybeAlbum of
          Nothing -> runKatipContextT le () "download.search" $
            $(logTM) WarningS $ logStr $ ("Cannot retry: catalog album " <> show albumId <> " not found" :: Text)
          Just album -> do
            runKatipContextT le () "download.search" $
              $(logTM) InfoS $ logStr $ ("Auto-retrying search for " <> DB.catalogAlbumTitle album <> " (attempt " <> show failedCount <> "/" <> show maxAutoRetries <> ")" :: Text)

            handleWantedAlbumAdded deps albumId
              (DB.catalogAlbumReleaseGroupMBID album)
              (DB.catalogAlbumTitle album)
              (DB.catalogAlbumArtistName album)

-- | Handle a wanted album added event by searching indexers.
handleWantedAlbumAdded :: ServiceContext -> Int64 -> Text -> Text -> Text -> IO ()
handleWantedAlbumAdded ServiceContext{..} catalogAlbumId releaseGroupId albumTitle artistName = do
  let le = scLogEnv
  let pool = scDbPool
  let bus = scEventBus
  let httpClient = scHttpClient

  -- Read current config from TVar
  config <- STM.atomically $ STM.readTVar scConfigVar

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

          -- Start concurrent search with timeout (from indexer config)
          startTime <- liftIO getCurrentTime

          let searchTimeoutMicros = indexerSearchTimeout indexerConfig * 1000000
          let slskdConfig = downloadSlskdClient downloadConfig
          searchResult <- liftIO $ race
            (threadDelay searchTimeoutMicros)
            (searchAllIndexersWithTracking bus le httpClient indexerConfig slskdConfig albumTitle artistName)

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

              -- Cache all found releases (pre-filter) so the album's releases
              -- page can render them instantly without re-searching.
              searchCacheNow <- liftIO getCurrentTime
              liftIO $ forM_ releasesWithSource $ \(sourceName, release) ->
                withConnection pool $ \conn ->
                  upsertCachedRelease conn catalogAlbumId sourceName "search" searchCacheNow release

              -- Get quality profile and current quality for filtering
              qualityProfile <- liftIO $ getEffectiveQualityProfile pool catalogAlbumId

              -- Get current quality from the latest cluster linked to this album
              currentQuality <- liftIO $ withConnection pool $ \conn -> do
                results <- queryRows conn
                  "SELECT quality FROM clusters \
                  \WHERE catalog_album_id = ? AND quality IS NOT NULL \
                  \ORDER BY id DESC LIMIT 1"
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

              -- Exclude releases that previously failed for this album so a
              -- broken release can't be re-grabbed by any automatic search.
              excludedUrls <- liftIO $ withConnection pool $ \conn ->
                Set.fromList <$> getFailedDownloadUrlsForAlbum conn catalogAlbumId

              let afterExclusion = if Set.null excludedUrls
                    then qualifyingReleasesWithSource
                    else filter (\(_, release) -> maybe True (\key -> not (Set.member key excludedUrls)) (releaseFailureKey release)) qualifyingReleasesWithSource
                  qualifiedCount = length afterExclusion

              when (qualifiedCount < totalResults) $ do
                $(logTM) InfoS $ logStr $ ("Filtered to " <> show qualifiedCount <> " releases that meet quality requirements" :: Text)

              when (not (Set.null excludedUrls) && qualifiedCount < length qualifyingReleasesWithSource) $ do
                $(logTM) InfoS $ logStr $ ("Excluded " <> show (length qualifyingReleasesWithSource - qualifiedCount) <> " previously failed releases" :: Text)

              -- Rank qualified results by quality (with album title context for better matching)
              -- Keep track of indexer names with a Map using download URL as key
              let qualifyingReleases = map snd afterExclusion
                  indexerMap = M.fromList $ map (\(name, r) -> (riDownloadUrl r, name)) afterExclusion
                  rankedReleases = rankReleases (Just albumTitle) qualifyingReleases
                  bestRelease = listToMaybe rankedReleases
                  -- Find the indexer name for the best release using its download URL
                  bestIndexerName = bestRelease >>= \r -> M.lookup (riDownloadUrl r) indexerMap

              -- Calculate best score
              let bestScore = case bestRelease of
                    Just rel -> Just $ scoreRelease (Just albumTitle) rel
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
                    , selectedScore = scoreRelease (Just albumTitle) release
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
                        , dscClock = scClock
                        }

                  _ <- liftIO $ submitDownload submissionCtx AutoSubmit release catalogAlbumId
                  pure ()

-- | Search all indexers and emit events for each completion.
searchAllIndexersWithTracking :: EventBus -> LogEnv -> HttpClient -> IndexerConfig -> Maybe SlskdConfig -> Text -> Text -> IO SearchResult
searchAllIndexersWithTracking bus le httpClient indexerConfig maybeSlskdConfig albumTitle artistName = do
  let enabledIndexers = filter indexerEnabled (indexerList indexerConfig)
      maxResults = indexerMaxResultsPerIndexer indexerConfig

  -- Search indexers and slskd in parallel
  (indexerResults, slskdResults) <- concurrently
    (searchIndexersParallel bus le httpClient enabledIndexers maxResults albumTitle artistName)
    (searchSlskdIfEnabled bus le httpClient maybeSlskdConfig albumTitle artistName)

  -- Partition indexer results into successes and failures
  let (errors, successes) = partitionEithers indexerResults
      indexerReleases = concatMap irReleases successes
      allReleases = indexerReleases ++ slskdResults
      totalReleases = length allReleases

  pure SearchResult
    { srResults = successes ++ [IndexerResult "slskd" slskdResults 0.0 | not (null slskdResults)]
    , srErrors = errors
    , srTotalReleases = totalReleases
    }

-- | Search all indexers in parallel.
searchIndexersParallel :: EventBus -> LogEnv -> HttpClient -> [Indexer] -> Int -> Text -> Text -> IO [Either IndexerError IndexerResult]
searchIndexersParallel bus le httpClient enabledIndexers maxResults albumTitle artistName = do
  mapConcurrently (\indexer -> do
    startTime <- getCurrentTime
    result <- searchIndexer httpClient indexer (buildSearchQuery maxResults albumTitle artistName)
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

-- | Search slskd if enabled.
searchSlskdIfEnabled :: EventBus -> LogEnv -> HttpClient -> Maybe SlskdConfig -> Text -> Text -> IO [ReleaseInfo]
searchSlskdIfEnabled bus le httpClient maybeSlskdConfig artistName albumTitle = do
  case maybeSlskdConfig of
    Nothing -> pure []
    Just config | not (slskdEnabled config) -> pure []
    Just config -> do
      startTime <- getCurrentTime
      let client = createSlskdClient config httpClient
      result <- searchSlskd le client artistName albumTitle (slskdMinTrackCount config)
      endTime <- getCurrentTime
      let duration = realToFrac $ diffUTCTime endTime startTime :: Double

      case result of
        Left err -> do
          runKatipContextT le () "download.search" $ do
            $(logTM) WarningS $ logStr $ ("slskd search failed: " <> err :: Text)
          publishAndLog bus le "download" $ SlskdSearchCompleted
            { slskdSearchResultCount = 0
            , slskdSearchDuration = duration
            , slskdSearchError = Just err
            }
          pure []

        Right candidates -> do
          let releases = map slskdCandidateToReleaseInfo candidates
              resultCount = length releases
          runKatipContextT le () "download.search" $ do
            $(logTM) InfoS $ logStr $ ("slskd returned " <> show resultCount <> " album candidates" :: Text)
          publishAndLog bus le "download" $ SlskdSearchCompleted
            { slskdSearchResultCount = resultCount
            , slskdSearchDuration = duration
            , slskdSearchError = Nothing
            }
          pure releases

-- | Build a search query from album and artist name.
buildSearchQuery :: Int -> Text -> Text -> SearchQuery
buildSearchQuery maxResults albumTitle artistName = SearchQuery
  { sqArtist = Just artistName
  , sqAlbum = Just albumTitle
  , sqYear = Nothing
  , sqQuery = Nothing
  , sqCategories = audioCategories
  , sqLimit = maxResults
  , sqOffset = 0
  }

-- ============================================================================
-- STREAMING RELEASE SEARCH
-- ============================================================================

-- | Lifecycle events of a streaming release search, keyed by source name.
data StreamEvent
  = StreamSearchStarted Text
  | StreamReleaseFound Text ReleaseInfo
  | StreamSearchCompleted Text Int
  | StreamSearchError Text Text
  | StreamDone Double
    -- ^ All sources finished; the total search time in seconds

-- | Search all given indexers (and slskd, when enabled) concurrently for an
-- album, invoking the callback for every lifecycle event as it happens.
-- Found releases are cached per album so the UI can render them instantly
-- next time. Blocks until every source has finished.
streamReleaseSearch
  :: ServiceContext
  -> [Indexer]
  -> Maybe SlskdConfig
  -> SearchQuery
  -> Int64                    -- ^ Catalog album id (for the release cache)
  -> (StreamEvent -> IO ())
  -> IO ()
streamReleaseSearch ServiceContext{..} searchIndexers maybeSlskd query albumId emit = do
  startTime <- getCurrentTime

  -- Launch indexer searches concurrently; slskd streams in this thread
  indexerSearches <- forM searchIndexers $ \indexer -> async $ do
    let name = indexerName indexer
    emit (StreamSearchStarted name)
    result <- searchIndexer scHttpClient indexer query
    case result of
      Left err -> emit (StreamSearchError name (show err))
      Right indexerResult -> do
        let releases = irReleases indexerResult
        now <- getCurrentTime
        forM_ releases $ \release -> do
          withConnection scDbPool $ \conn ->
            upsertCachedRelease conn albumId name "search" now release
          emit (StreamReleaseFound name release)
        emit (StreamSearchCompleted name (length releases))

  case maybeSlskd of
    Just slskdConfig | slskdEnabled slskdConfig -> do
      let name = "slskd"
      emit (StreamSearchStarted name)
      let client = createSlskdClient slskdConfig scHttpClient
          artistName = fromMaybe "" (sqArtist query)
          albumName = fromMaybe "" (sqAlbum query)
          minTrackCount = slskdMinTrackCount slskdConfig
      resultCountRef <- IORef.newIORef (0 :: Int)
      result <- searchSlskdStreaming scLogEnv client artistName albumName 60 minTrackCount $ \candidate -> do
        let release = slskdCandidateToReleaseInfo candidate
        now <- getCurrentTime
        withConnection scDbPool $ \conn ->
          upsertCachedRelease conn albumId name "search" now release
        emit (StreamReleaseFound name release)
        IORef.modifyIORef' resultCountRef (+1)
      finalCount <- IORef.readIORef resultCountRef
      case result of
        Left err -> emit (StreamSearchError name err)
        Right _ -> emit (StreamSearchCompleted name finalCount)
    _ -> pure ()

  mapM_ wait indexerSearches

  endTime <- getCurrentTime
  emit (StreamDone (realToFrac (diffUTCTime endTime startTime)))
