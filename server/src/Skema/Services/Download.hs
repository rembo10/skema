{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Download service - handles the complete download lifecycle.
--
-- This service coordinates:
-- 1. SEARCH: Multi-indexer searches for wanted albums
-- 2. SUBMISSION: Submitting downloads to download clients
-- 3. MONITORING: Polling download clients for status updates
--
-- The service is split into two concurrent sub-processes:
-- - Search orchestrator: Listens for WantedAlbumAdded events
-- - Download monitor: Polls download clients periodically
module Skema.Services.Download
  ( startDownloadService
  , DownloadClientInstance(..)  -- Re-exported for Importer service
  ) where

import Skema.Services.Dependencies (DownloadDeps(..))
import Skema.Events.Bus
import Skema.Events.Types
import Skema.Database.Connection
import Skema.Database.Repository
import Skema.HTTP.Client (HttpClient)
import Skema.Indexer.Search (rankResultsWithContext)
import Skema.Indexer.Client (searchIndexer)
import Skema.Indexer.Prowlarr (searchProwlarr, grabRelease, ProwlarrRelease(..))
import Skema.Indexer.Types
import Skema.Config.Types (Config(..), Indexer(..), IndexerConfig(..), ProwlarrConfig(..), DownloadClient(..), DownloadConfig(..), DownloadClientType(..), downloadClientTypeName)
import Skema.DownloadClient.Types (DownloadClientAPI(..), AddDownloadRequest(..), AddDownloadResult(..))
import Skema.DownloadClient.SABnzbd (createSABnzbdClient, SABnzbdClient)
import Skema.DownloadClient.Transmission (createTransmissionClient, TransmissionClient)
import Skema.DownloadClient.QBittorrent (createQBittorrentClient, QBittorrentClient)
import Skema.DownloadClient.Deluge (createDelugeClient, DelugeClient)
import Control.Concurrent.Async (Async, async, race, mapConcurrently)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (readTChan)
import Control.Concurrent (threadDelay)
import Control.Monad ()
import Control.Exception (try)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, diffUTCTime)
import qualified Data.Map.Strict as Map
import Database.SQLite.Simple (Only(..))
import Skema.Database.Types (DownloadRecord(..), SearchOutcome(..))
import qualified Skema.Database.Types as DB
import Skema.Database.Utils (downloadStatusToText)
import qualified Skema.Domain.Quality as Quality
import qualified Skema.DownloadClient.Types as DC
import Katip

-- | Wrapper for different download client types
data DownloadClientInstance
  = SABInstance SABnzbdClient
  | TransmissionInstance TransmissionClient
  | QBittorrentInstance QBittorrentClient
  | DelugeInstance DelugeClient

instance DownloadClientAPI DownloadClientInstance where
  testConnection (SABInstance c) = testConnection c
  testConnection (TransmissionInstance c) = testConnection c
  testConnection (QBittorrentInstance c) = testConnection c
  testConnection (DelugeInstance c) = testConnection c

  addDownload (SABInstance c) = addDownload c
  addDownload (TransmissionInstance c) = addDownload c
  addDownload (QBittorrentInstance c) = addDownload c
  addDownload (DelugeInstance c) = addDownload c

  getDownloadStatus (SABInstance c) = getDownloadStatus c
  getDownloadStatus (TransmissionInstance c) = getDownloadStatus c
  getDownloadStatus (QBittorrentInstance c) = getDownloadStatus c
  getDownloadStatus (DelugeInstance c) = getDownloadStatus c

  getAllDownloads (SABInstance c) = getAllDownloads c
  getAllDownloads (TransmissionInstance c) = getAllDownloads c
  getAllDownloads (QBittorrentInstance c) = getAllDownloads c
  getAllDownloads (DelugeInstance c) = getAllDownloads c

  pauseDownload (SABInstance c) = pauseDownload c
  pauseDownload (TransmissionInstance c) = pauseDownload c
  pauseDownload (QBittorrentInstance c) = pauseDownload c
  pauseDownload (DelugeInstance c) = pauseDownload c

  resumeDownload (SABInstance c) = resumeDownload c
  resumeDownload (TransmissionInstance c) = resumeDownload c
  resumeDownload (QBittorrentInstance c) = resumeDownload c
  resumeDownload (DelugeInstance c) = resumeDownload c

  removeDownload (SABInstance c) = removeDownload c
  removeDownload (TransmissionInstance c) = removeDownload c
  removeDownload (QBittorrentInstance c) = removeDownload c
  removeDownload (DelugeInstance c) = removeDownload c

-- | Start the unified download service.
--
-- Spawns two concurrent sub-processes:
-- 1. Search orchestrator - handles WantedAlbumAdded events
-- 2. Download monitor - polls download clients for status updates
-- Returns both async handles for graceful shutdown.
startDownloadService :: DownloadDeps -> IO (Async (), Async ())
startDownloadService deps = do
  searchHandle <- async $ runSearchOrchestrator deps
  monitorHandle <- async $ runDownloadMonitor deps
  pure (searchHandle, monitorHandle)

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
          let searchTimeout = 30 * 1000000  -- 30 seconds in microseconds

          searchResult <- liftIO $ race
            (threadDelay searchTimeout)
            (searchAllIndexersWithTracking bus le httpClient indexerConfig albumTitle artistName)

          endTime <- liftIO getCurrentTime
          let searchDuration = realToFrac $ diffUTCTime endTime startTime :: Double

          case searchResult of
            Left () -> do
              -- Timeout occurred
              $(logTM) WarningS $ logStr ("Search timed out after 30 seconds" :: Text)
              liftIO $ publishAndLog bus le "download" $ AlbumSearchCompleted
                { searchTotalResults = 0
                , searchBestScore = Nothing
                , searchDuration = searchDuration
                }
              -- Save search history with timeout
              _ <- liftIO $ saveSearchHistory pool catalogAlbumId searchDuration [] [] Nothing SearchTimeout
              pure ()

            Right searchResults -> do
              -- Got results from indexers with indexer names attached
              let indexedResults = concat 
                    [ [(irIndexerName ir, rel) | rel <- irReleases ir] 
                    | ir <- srResults searchResults 
                    ]
                  allReleases = map snd indexedResults
                  totalResults = length allReleases

              $(logTM) InfoS $ logStr $ ("Found " <> show totalResults <> " total releases across all indexers" :: Text)

              -- Rank results by quality (with album title context for better matching)
              let rankedReleases = rankResultsWithContext (Just albumTitle) allReleases
                  bestRelease = listToMaybe rankedReleases

              -- Calculate best score
              let bestScore = case bestRelease of
                    Just rel -> Just $ scoreRelease rel
                    Nothing -> Nothing

              -- Emit search completed event
              liftIO $ publishAndLog bus le "download" $ AlbumSearchCompleted
                { searchTotalResults = totalResults
                , searchBestScore = bestScore
                , searchDuration = searchDuration
                }

              -- If we found releases, pick the best one and queue download
              case bestRelease of
                Nothing -> do
                  $(logTM) WarningS $ logStr ("No releases found for album" :: Text)
                  -- Save search history with no results
                  _ <- liftIO $ saveSearchHistory pool catalogAlbumId searchDuration indexedResults rankedReleases Nothing SearchNoResults
                  pure ()

                Just release -> do
                  $(logTM) InfoS $ logStr $ ("Best release: " <> riTitle release :: Text)

                  -- Emit best release selected event
                  liftIO $ publishAndLog bus le "download" $ BestReleaseSelected
                    { selectedTitle = riTitle release
                    , selectedIndexer = "unknown"  -- TODO: track indexer name with release
                    , selectedScore = scoreRelease release
                    , selectedSeeders = riSeeders release
                    }

                  -- Pick appropriate download client based on download type
                  let maybeClient = case riDownloadType release of
                        NZB -> downloadNzbClient downloadConfig
                        Torrent -> downloadTorrentClient downloadConfig

                  case maybeClient of
                    Nothing -> do
                      let clientType = case riDownloadType release of
                            NZB -> "NZB" :: Text
                            Torrent -> "Torrent"
                      $(logTM) ErrorS $ logStr $ ("No " <> clientType <> " download client configured, cannot queue download" :: Text)
                      -- Save search history with no client
                      _ <- liftIO $ saveSearchHistory pool catalogAlbumId searchDuration indexedResults rankedReleases (Just release) SearchNoClient
                      pure ()

                    Just client | not (dcEnabled client) -> do
                      $(logTM) ErrorS $ logStr $ ("Download client " <> downloadClientTypeName (dcType client) <> " is disabled" :: Text)
                      -- Save search history with no client (disabled)
                      _ <- liftIO $ saveSearchHistory pool catalogAlbumId searchDuration indexedResults rankedReleases (Just release) SearchNoClient
                      pure ()

                    Just client -> do
                      $(logTM) InfoS $ logStr $ ("Submitting download to client: " <> downloadClientTypeName (dcType client) :: Text)
                      $(logTM) InfoS $ logStr $ ("Category: " <> fromMaybe "<none>" (dcCategory client) :: Text)

                      -- Check if this is a Prowlarr release and Prowlarr is configured
                      -- If so, use Prowlarr's grab API to let Prowlarr handle the download client
                      let prowlarrConfig = indexerProwlarr indexerConfig
                          isProwlarrRelease = case (riProwlarrGuid release, riProwlarrIndexerId release) of
                            (Just _, Just _) -> True
                            _ -> False
                          useProwlarrGrab = isProwlarrRelease && 
                                           maybe False prowlarrEnabled prowlarrConfig

                      addResult <- if useProwlarrGrab
                        then do
                          -- Use Prowlarr's grab API
                          $(logTM) InfoS $ logStr ("Using Prowlarr grab API to send to download client" :: Text)
                          let Just guid = riProwlarrGuid release
                              Just indexerId = riProwlarrIndexerId release
                              Just pConfig = prowlarrConfig
                              prowlarrRelease = ProwlarrRelease
                                { prGuid = guid
                                , prIndexerId = indexerId
                                , prTitle = riTitle release
                                }
                          grabResult <- liftIO $ grabRelease httpClient pConfig prowlarrRelease
                          case grabResult of
                            Left err -> pure $ Left err
                            Right () -> pure $ Right AddDownloadResult
                              { adrClientId = guid  -- Use guid as client ID for tracking
                              , adrSuccess = True
                              , adrMessage = Just "Sent to Prowlarr"
                              }
                        else do
                          -- Direct submission to download client
                          clientInstance <- liftIO $ createClientInstance httpClient client
                          liftIO $ addDownload clientInstance (AddDownloadRequest
                            { adrUrl = riDownloadUrl release
                            , adrTitle = riTitle release
                            , adrCategory = dcCategory client
                            , adrPriority = Nothing
                            })

                      case addResult of
                        Left err -> do
                          $(logTM) ErrorS $ logStr $ ("Failed to add download to client: " <> err :: Text)

                          -- Insert failed download record
                          now <- liftIO getCurrentTime
                          downloadId <- liftIO $ withConnection pool $ \conn ->
                            insertDownload conn
                              (Just catalogAlbumId)  -- catalog_album_id from event
                              "unknown"  -- indexer name
                              (riDownloadUrl release)
                              (downloadClientTypeName (dcType client))
                              Nothing  -- no client_id since it failed
                              "failed"
                              Nothing  -- download_path
                              (riTitle release)
                              (riSize release)
                              Nothing  -- quality
                              (case riDownloadType release of
                                 NZB -> Just "NZB"
                                 Torrent -> Just "Torrent")
                              (riSeeders release)
                              0.0  -- progress
                              (Just err)  -- error_message
                              now

                          -- Emit download failed event
                          liftIO $ publishAndLog bus le "download" $ DownloadFailed
                            { downloadId = downloadId
                            , downloadTitle = riTitle release
                            , downloadError = Just err
                            }

                          -- Save search history with failed download
                          _ <- liftIO $ saveSearchHistory pool catalogAlbumId searchDuration indexedResults rankedReleases (Just release) SearchFailed
                          pure ()

                        Right addResultData -> do
                          $(logTM) InfoS $ logStr $ ("Download added to client with ID: " <> adrClientId addResultData :: Text)

                          -- Insert download record into database with client_id
                          now <- liftIO getCurrentTime
                          downloadId <- liftIO $ withConnection pool $ \conn ->
                            insertDownload conn
                              (Just catalogAlbumId)  -- catalog_album_id from event
                              "unknown"  -- indexer name
                              (riDownloadUrl release)
                              (downloadClientTypeName (dcType client))
                              (Just $ adrClientId addResultData)  -- client_id from download client
                              "downloading"
                              Nothing  -- download_path
                              (riTitle release)
                              (riSize release)
                              Nothing  -- quality
                              (case riDownloadType release of
                                 NZB -> Just "NZB"
                                 Torrent -> Just "Torrent")
                              (riSeeders release)
                              0.0  -- progress
                              Nothing  -- error_message
                              now

                          -- Emit download started event
                          liftIO $ publishAndLog bus le "download" $ DownloadStarted
                            { downloadId = downloadId
                            , downloadTitle = riTitle release
                            }

                          $(logTM) InfoS $ logStr $ ("Download started with DB ID: " <> show downloadId :: Text)

                          -- Save search history with successful download
                          _ <- liftIO $ saveSearchHistory pool catalogAlbumId searchDuration indexedResults rankedReleases (Just release) SearchDownloaded
                          pure ()

-- | Search all indexers and emit events for each completion.
-- Also searches Prowlarr if configured and enabled.
searchAllIndexersWithTracking :: EventBus -> LogEnv -> HttpClient -> IndexerConfig -> Text -> Text -> IO SearchResult
searchAllIndexersWithTracking bus le httpClient indexerConfig albumTitle artistName = do
  let enabledIndexers = filter indexerEnabled (indexerList indexerConfig)
      searchQuery = buildSearchQuery albumTitle artistName

  -- Search each manual indexer and track completion
  indexerResults <- mapConcurrently (\indexer -> do
    startTime <- getCurrentTime
    result <- searchIndexer httpClient indexer searchQuery
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

  -- Also search Prowlarr if configured and enabled
  prowlarrResults <- case indexerProwlarr indexerConfig of
    Just prowlarrConfig | prowlarrEnabled prowlarrConfig -> do
      runKatipContextT le () "download.search" $ do
        $(logTM) InfoS "Searching Prowlarr..."
      startTime <- getCurrentTime
      result <- searchProwlarr httpClient prowlarrConfig searchQuery
      endTime <- getCurrentTime
      let duration = realToFrac $ diffUTCTime endTime startTime :: Double

      case result of
        Left err -> do
          runKatipContextT le () "download.search" $ do
            $(logTM) WarningS $ logStr $ ("Prowlarr search failed: " <> ieError err :: Text)
          publishAndLog bus le "download" $ IndexerSearchCompleted
            { searchIndexerName = "Prowlarr"
            , searchResultCount = 0
            , searchDuration = duration
            }
          pure [Left err]

        Right prowlarrResult -> do
          let resultCount = length (irReleases prowlarrResult)
          runKatipContextT le () "download.search" $ do
            $(logTM) InfoS $ logStr $ ("Prowlarr returned " <> show resultCount <> " results" :: Text)
          publishAndLog bus le "download" $ IndexerSearchCompleted
            { searchIndexerName = "Prowlarr"
            , searchResultCount = resultCount
            , searchDuration = duration
            }
          pure [Right prowlarrResult]
    _ -> pure []  -- Prowlarr not configured or disabled

  -- Combine all results
  let allResults = indexerResults ++ prowlarrResults
      (errors, successes) = partitionEithers allResults
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
  , sqCategories = [3000, 3010, 3020]  -- Audio, MP3, FLAC
  , sqLimit = 50
  , sqOffset = 0
  }

-- | Calculate quality score for a release.
scoreRelease :: ReleaseInfo -> Int
scoreRelease ReleaseInfo{..} =
  formatScore + seedScore + sizeScore + grabScore
  where
    -- Format score (prefer lossless)
    formatScore = case riDownloadType of
      NZB -> 500  -- NZBs generally more reliable
      Torrent -> case riSeeders of
        Just s | s > 10 -> 400  -- Well-seeded torrents
        Just s | s > 0  -> 200  -- Some seeders
        _               -> 50   -- No seeder info or dead

    -- Seeder score (for torrents)
    seedScore = case riSeeders of
      Just s -> min 500 (s * 10)  -- Cap at 500
      Nothing -> 0

    -- Size score (prefer reasonable sizes, penalize too small or too large)
    sizeScore = case riSize of
      Just size ->
        let mb = fromIntegral size / (1024.0 * 1024.0) :: Double
        in if mb > 50 && mb < 2000  -- 50MB - 2GB range
           then 100
           else if mb > 2000 && mb < 5000
                then 50  -- Large but acceptable
                else 0   -- Too small or too large
      Nothing -> 0

    -- Grab score (popularity)
    grabScore = case riGrabs of
      Just g -> min 200 (g * 2)  -- Cap at 200
      Nothing -> 0

-- ============================================================================
-- DOWNLOAD MONITOR
-- ============================================================================

-- | Run the download monitor sub-process.
--
-- Uses an event-driven approach: loads initial state on startup, then subscribes
-- to download events to know when to start/stop polling download clients.
-- When downloads are active, polls every 1 second and emits progress events.
-- Download client instances are created once at startup and recreated only when
-- the config changes (via ConfigUpdated event).
runDownloadMonitor :: DownloadDeps -> IO ()
runDownloadMonitor DownloadDeps{..} = do
  runKatipContextT dlLogEnv () "download.monitor" $ do
    let bus = dlEventBus
        pool = dlDbPool
        httpClient = dlHttpClient

    $(logTM) InfoS "Starting download monitor"

    -- Create initial client instances from config
    config <- liftIO $ STM.atomically $ STM.readTVar dlConfigVar
    let downloadCfg = download config
    initialClients <- liftIO $ createClientInstances httpClient downloadCfg

    $(logTM) InfoS $ logStr $ ("Created " <> show (length initialClients) <> " initial client instances" :: Text)
    forM_ initialClients $ \(clientName, _) ->
      $(logTM) InfoS $ logStr $ ("  - Client: '" <> clientName <> "'" :: Text)

    -- Store clients in a TVar so they can be updated when config changes
    clientsVar <- liftIO $ STM.atomically $ STM.newTVar initialClients

    -- Check initial state: are there any active downloads?
    initialCount <- liftIO $ withConnection pool $ \conn -> do
      results <- queryRows conn
        "SELECT COUNT(*) FROM downloads WHERE status IN ('queued', 'downloading')"
        () :: IO [Only Int]
      case results of
        [Only count] -> pure count
        _ -> pure 0

    $(logTM) InfoS $ logStr $ ("Found " <> show initialCount <> " active downloads on startup" :: Text)

    -- Create a TVar to track the count of active downloads
    activeCountVar <- liftIO $ STM.atomically $ STM.newTVar initialCount

    -- Subscribe to events to maintain the count and reload clients on config changes
    eventChan <- liftIO $ STM.atomically $ subscribe bus
    _ <- liftIO $ async $ forever $ do
      envelope <- STM.atomically $ readTChan eventChan
      case envelopeEvent envelope of
        DownloadStarted{} -> do
          -- Increment active download count
          STM.atomically $ STM.modifyTVar' activeCountVar (+1)

        DownloadCompleted{} -> do
          -- Decrement active download count
          STM.atomically $ STM.modifyTVar' activeCountVar (\n -> max 0 (n - 1))

        DownloadFailed{} -> do
          -- Decrement active download count
          STM.atomically $ STM.modifyTVar' activeCountVar (\n -> max 0 (n - 1))

        ConfigUpdated{} -> do
          -- Config changed - recreate download client instances
          runKatipContextT dlLogEnv () "download.monitor" $ do
            $(logTM) InfoS "Config updated, recreating download client instances"
          newConfig <- STM.atomically $ STM.readTVar dlConfigVar
          let newDownloadCfg = download newConfig
          newClients <- createClientInstances httpClient newDownloadCfg
          runKatipContextT dlLogEnv () "download.monitor" $ do
            $(logTM) InfoS $ logStr $ ("Created " <> show (length newClients) <> " client instances from updated config" :: Text)
          STM.atomically $ STM.writeTVar clientsVar newClients

        _ -> pure ()  -- Ignore other events

    -- Spawn monitoring loop in background thread
    _ <- liftIO $ async $ runKatipContextT dlLogEnv () "download.monitor" $ forever $ do
      -- Read current client instances (only changes when config changes)
      clients <- liftIO $ STM.atomically $ STM.readTVar clientsVar

      -- Check if there are active downloads
      activeCount <- liftIO $ STM.atomically $ STM.readTVar activeCountVar

      $(logTM) DebugS $ logStr $ ("Monitor loop: activeCount=" <> show activeCount <> ", clients=" <> show (length clients) :: Text)

      if activeCount > 0 && not (null clients)
        then do
          -- Active downloads exist - poll every 1 second for real-time updates
          $(logTM) DebugS $ logStr $ ("Checking " <> show (length clients) <> " clients..." :: Text)
          forM_ clients $ \(clientName, client) ->
            liftIO $ checkAndUpdateDownloads dlLogEnv bus pool dlProgressMap clientName client

          -- Sleep for 1 second to provide real-time updates to frontend
          liftIO $ threadDelay 1000000  -- 1 second

        else do
          -- No active downloads or no clients - just sleep and check again later
          $(logTM) DebugS "No active downloads or no clients, sleeping..."
          liftIO $ threadDelay 5000000  -- 5 seconds

    pure ()
  pure ()

-- | Create download client instances from configuration
createClientInstances :: HttpClient -> DownloadConfig -> IO [(Text, DownloadClientInstance)]
createClientInstances httpClient DownloadConfig{..} = do
  let clients = catMaybes [downloadNzbClient, downloadTorrentClient]
  catMaybes <$> forM clients (\client -> do
    if not (dcEnabled client)
      then pure Nothing
      else do
        instance' <- case dcType client of
          SABnzbd -> do
            let apiKey = fromMaybe "" (dcApiKey client)
            pure $ Just $ SABInstance $ createSABnzbdClient
              (dcUrl client)
              apiKey
              httpClient
              (dcDownloadDir client)
              (dcCategory client)

          NZBGet -> do
            -- TODO: Implement NZBGet client
            pure Nothing

          Transmission -> do
            client' <- createTransmissionClient
              (dcUrl client)
              (dcUsername client)
              (dcPassword client)
              httpClient
            pure $ Just $ TransmissionInstance client'

          QBittorrent -> do
            let username = fromMaybe "" (dcUsername client)
                password = fromMaybe "" (dcPassword client)
            client' <- createQBittorrentClient
              (dcUrl client)
              username
              password
              httpClient
            pure $ Just $ QBittorrentInstance client'

          Deluge -> do
            let password = fromMaybe "" (dcPassword client)
            client' <- createDelugeClient
              (dcUrl client)
              password
              httpClient
            pure $ Just $ DelugeInstance client'

        pure $ fmap (\inst -> (downloadClientTypeName (dcType client), inst)) instance'
    )

-- | Check all downloads for a specific client and update database
checkAndUpdateDownloads :: LogEnv -> EventBus -> ConnectionPool -> STM.TVar (Map.Map Int64 (Double, Text)) -> Text -> DownloadClientInstance -> IO ()
checkAndUpdateDownloads le bus pool progressMap clientName client = do
  -- Get all active downloads from database for this client
  activeDownloads <- withConnection pool $ \conn ->
    queryRows conn
      "SELECT id, catalog_album_id, indexer_name, download_url, download_client, \
      \download_client_id, status, download_path, title, size_bytes, quality, \
      \format, seeders, progress, error_message, queued_at, started_at, \
      \completed_at, imported_at, updated_at, \
      \matched_cluster_id, library_path \
      \FROM downloads \
      \WHERE download_client = ? AND status IN ('queued', 'downloading')"
      (Only clientName) :: IO [DownloadRecord]

  forM_ activeDownloads $ \download -> do
    -- Get current status from client
    case DB.downloadClientId download of
      Nothing -> runKatipContextT le () "download.monitor" $
        $(logTM) WarningS $ logStr $ "Download has no client ID: " <> DB.downloadTitle download
      Just clientId -> do
        statusResult <- getDownloadStatus client clientId

        -- If lookup by ID fails, try to find by title (for Prowlarr-grabbed torrents)
        finalResult <- case statusResult of
          Left _ -> do
            -- Try to find by scanning all downloads and matching title/name
            allDownloads <- getAllDownloads client
            case allDownloads of
              Left _ -> pure statusResult  -- Keep original error
              Right downloads -> do
                let title = DB.downloadTitle download
                    -- Normalize title for matching (remove indexer suffix like "[TorrentGalaxyClone]")
                    cleanTitle = T.strip $ fst $ T.breakOn "[" title
                    normalizedTitle = T.toLower cleanTitle
                    -- Try to match by name (torrent name from client)
                    matchingDownload = find (\d -> 
                      -- Match by torrent name (most reliable)
                      maybe False (\name -> 
                        let normalizedName = T.toLower name
                        in normalizedTitle `T.isInfixOf` normalizedName ||
                           normalizedName `T.isInfixOf` normalizedTitle ||
                           -- Also try matching individual words
                           matchesMostWords normalizedTitle normalizedName
                      ) (DC.diName d) ||
                      -- Fallback: match by download path
                      maybe False (\p -> normalizedTitle `T.isInfixOf` T.toLower p) (DC.diDownloadPath d)
                      ) downloads
                case matchingDownload of
                  Just foundDownload -> do
                    -- Update the client_id in database to the actual torrent hash
                    runKatipContextT le () "download.monitor" $ do
                      $(logTM) InfoS $ logStr $ ("Found matching torrent by name: " <> fromMaybe "?" (DC.diName foundDownload) :: Text)
                    withConnection pool $ \conn ->
                      executeQuery conn
                        "UPDATE downloads SET download_client_id = ? WHERE id = ?"
                        (DC.diClientId foundDownload, DB.downloadId download)
                    pure $ Right foundDownload
                  Nothing -> pure statusResult  -- Keep original error
          Right _ -> pure statusResult

        case finalResult of
          Left err -> do
            -- Only log at debug level for torrent clients since title matching is unreliable
            -- For NZB clients (SABnzbd), we can be more confident about "not found" errors
            let isTorrentClient = clientName `elem` ["Deluge", "Transmission", "qBittorrent"]
            if isTorrentClient
              then runKatipContextT le () "download.monitor" $ do
                $(logTM) DebugS $ logStr $ ("Could not find torrent in " <> clientName <> " for: " <> DB.downloadTitle download :: Text)
              else do
                runKatipContextT le () "download.monitor" $ do
                  $(logTM) ErrorS $ logStr $ ("Failed to get download status for " <> DB.downloadTitle download <> ": " <> err :: Text)
                
                -- Only mark as failed for NZB clients where ID tracking is reliable
                when ("not found" `T.isInfixOf` T.toLower err || "does not exist" `T.isInfixOf` T.toLower err) $ do
                  runKatipContextT le () "download.monitor" $ do
                    $(logTM) WarningS $ logStr $ ("Marking download as failed (not found in client): " <> DB.downloadTitle download :: Text)
                  now <- getCurrentTime
                  withConnection pool $ \conn -> do
                    executeQuery conn
                      "UPDATE downloads SET status = ?, error_message = ?, completed_at = ? WHERE id = ?"
                      ( downloadStatusToText DB.DownloadFailed
                      , Just ("Download not found in client: " <> err)
                      , Just now
                      , DB.downloadId download
                      )
                  -- Emit failed event
                  publishAndLog bus le "download" $ DownloadFailed
                    { downloadId = fromMaybe 0 (DB.downloadId download)
                    , downloadTitle = DB.downloadTitle download
                    , downloadError = Just $ "Download not found in client: " <> err
                    }

          Right downloadInfo -> do
            let oldStatus = DB.downloadStatus download
                newStatus = mapClientStatusToDBStatus (DC.diStatus downloadInfo)
                oldProgress = DB.downloadProgress download
                newProgress = DC.diProgress downloadInfo
                downloadIdVal = fromMaybe 0 (DB.downloadId download)

            -- Only update database when status changes
            when (oldStatus /= newStatus) $ do
              now <- getCurrentTime
              withConnection pool $ \conn -> do
                executeQuery conn
                  "UPDATE downloads SET status = ?, download_path = ?, \
                  \error_message = ?, size_bytes = ?, started_at = COALESCE(started_at, ?), \
                  \completed_at = ? WHERE id = ?"
                  ( downloadStatusToText newStatus
                  , DC.diDownloadPath downloadInfo
                  , DC.diErrorMessage downloadInfo
                  , DC.diSizeBytes downloadInfo
                  , if newStatus == DB.DownloadDownloading then Just now else Nothing
                  , if newStatus == DB.DownloadCompleted then Just now else Nothing
                  , DB.downloadId download
                  )

            -- Update in-memory progress map for active downloads (store progress + display status)
            when (newStatus == DB.DownloadDownloading) $ do
              let displayStatus = clientStatusToDisplayText (DC.diStatus downloadInfo)
              STM.atomically $ STM.modifyTVar' progressMap $ \m ->
                Map.insert downloadIdVal (newProgress, displayStatus) m

            -- Remove from progress map when download is complete/failed
            when (newStatus == DB.DownloadCompleted || newStatus == DB.DownloadFailed) $ do
              STM.atomically $ STM.modifyTVar' progressMap $ \m ->
                Map.delete downloadIdVal m

            -- Emit progress event if download is active and progress changed significantly
            -- (emit if progress increased by at least 1% to avoid spamming)
            when (newStatus == DB.DownloadDownloading && abs (newProgress - oldProgress) >= 0.01) $ do
              publishAndLog bus le "download" $ DownloadProgress
                { downloadId = fromMaybe 0 (DB.downloadId download)
                , downloadTitle = DB.downloadTitle download
                , downloadProgress = newProgress
                , downloadSizeBytes = DC.diSizeBytes downloadInfo
                , downloadedBytes = DC.diDownloadedBytes downloadInfo
                }

            -- If status changed to completed, emit event for post-processing
            when (oldStatus /= DB.DownloadCompleted && newStatus == DB.DownloadCompleted) $ do
              runKatipContextT le () "download.monitor" $
                $(logTM) InfoS $ logStr $ "Download completed: " <> DB.downloadTitle download
              publishAndLog bus le "download" $ DownloadCompleted
                { downloadId = fromMaybe 0 (DB.downloadId download)
                , downloadTitle = DB.downloadTitle download
                , downloadPath = DB.downloadPath download
                }

            -- If download failed, emit event
            when (oldStatus /= DB.DownloadFailed && newStatus == DB.DownloadFailed) $ do
              runKatipContextT le () "download.monitor" $
                $(logTM) ErrorS $ logStr $ "Download failed: " <> DB.downloadTitle download
              publishAndLog bus le "download" $ DownloadFailed
                { downloadId = fromMaybe 0 (DB.downloadId download)
                , downloadTitle = DB.downloadTitle download
                , downloadError = DB.downloadErrorMessage download
                }

            -- If download was cancelled, emit failed event (treat as terminal state)
            when (oldStatus /= DB.DownloadCancelled && newStatus == DB.DownloadCancelled) $ do
              runKatipContextT le () "download.monitor" $
                $(logTM) WarningS $ logStr $ "Download cancelled: " <> DB.downloadTitle download
              publishAndLog bus le "download" $ DownloadFailed
                { downloadId = fromMaybe 0 (DB.downloadId download)
                , downloadTitle = DB.downloadTitle download
                , downloadError = Just "Download cancelled"
                }

-- | Map download client status to database status
-- For persistence, we only care about: downloading (active), completed, or failed
mapClientStatusToDBStatus :: DC.DownloadStatus -> DB.DownloadStatus
mapClientStatusToDBStatus DC.DSQueued = DB.DownloadDownloading      -- Active download
mapClientStatusToDBStatus DC.DSDownloading = DB.DownloadDownloading -- Active download
mapClientStatusToDBStatus DC.DSPaused = DB.DownloadDownloading      -- Still active, just paused
mapClientStatusToDBStatus DC.DSCompleted = DB.DownloadCompleted     -- Terminal state
mapClientStatusToDBStatus DC.DSFailed = DB.DownloadFailed           -- Terminal state
mapClientStatusToDBStatus DC.DSCancelled = DB.DownloadFailed        -- Terminal state (treat as failed)

-- | Convert client status to display text for frontend
clientStatusToDisplayText :: DC.DownloadStatus -> Text
clientStatusToDisplayText DC.DSQueued = "queued"
clientStatusToDisplayText DC.DSDownloading = "downloading"
clientStatusToDisplayText DC.DSPaused = "paused"
clientStatusToDisplayText DC.DSCompleted = "completed"
clientStatusToDisplayText DC.DSFailed = "failed"
clientStatusToDisplayText DC.DSCancelled = "cancelled"

-- ============================================================================
-- SHARED UTILITIES
-- ============================================================================

-- | Create a download client instance from configuration.
createClientInstance :: HttpClient -> DownloadClient -> IO DownloadClientInstance
createClientInstance httpClient DownloadClient{..} = do
  case dcType of
    SABnzbd -> do
      let apiKey = fromMaybe "" dcApiKey
      pure $ SABInstance $ createSABnzbdClient dcUrl apiKey httpClient dcDownloadDir dcCategory

    NZBGet ->
      fail "NZBGet client not yet implemented"

    Transmission -> do
      client <- createTransmissionClient dcUrl dcUsername dcPassword httpClient
      pure $ TransmissionInstance client

    QBittorrent -> do
      let username = fromMaybe "" dcUsername
          password = fromMaybe "" dcPassword
      client <- createQBittorrentClient dcUrl username password httpClient
      pure $ QBittorrentInstance client

    Deluge -> do
      let password = fromMaybe "" dcPassword
      client <- createDelugeClient dcUrl password httpClient
      pure $ DelugeInstance client

-- | Check if most words from title1 appear in title2
-- This helps match titles like "GRiZ Ouroboros 2023 Mp3 320kbps PMEDIA" with
-- torrent names like "GRiZ - Ouroboros (2023) [MP3 320]"
matchesMostWords :: Text -> Text -> Bool
matchesMostWords title1 title2 =
  let words1 = filter (\w -> T.length w > 2) $ T.words $ T.toLower title1
      words2 = T.toLower title2
      matchCount = length $ filter (`T.isInfixOf` words2) words1
      totalWords = length words1
  in totalWords > 0 && matchCount >= (totalWords * 2 `div` 3)  -- At least 2/3 of words match

-- | Save search history to database
saveSearchHistory :: ConnectionPool
                  -> Int64                    -- ^ catalog_album_id
                  -> Double                   -- ^ search_duration (seconds)
                  -> [(Text, ReleaseInfo)]    -- ^ indexed results with indexer name
                  -> [ReleaseInfo]            -- ^ ranked results
                  -> Maybe ReleaseInfo        -- ^ selected release (if any)
                  -> SearchOutcome            -- ^ outcome
                  -> IO Int64
saveSearchHistory pool albumId durationSecs indexedResults rankedResults selectedRelease outcome = do
  now <- getCurrentTime
  let durationMs = Just $ round (durationSecs * 1000)
      totalResults = length rankedResults
      selectedTitle = riTitle <$> selectedRelease
      selectedIndexer = selectedRelease >>= \rel -> 
        listToMaybe [idx | (idx, r) <- indexedResults, riDownloadUrl r == riDownloadUrl rel]
      selectedScore = scoreRelease <$> selectedRelease

  -- Insert search history record
  historyId <- withConnection pool $ \conn ->
    insertSearchHistory conn albumId now totalResults durationMs selectedTitle selectedIndexer selectedScore outcome

  -- Insert individual results (top 50 to avoid storing too much)
  let topResults = take 50 $ zip [1..] rankedResults
  withConnection pool $ \conn ->
    forM_ topResults $ \(rank, rel) -> do
      let indexerName = fromMaybe "unknown" $ 
            listToMaybe [idx | (idx, r) <- indexedResults, riDownloadUrl r == riDownloadUrl rel]
          downloadType = case riDownloadType rel of
            NZB -> "nzb"
            Torrent -> "torrent"
          qualityText = Just $ Quality.qualityToText (riQuality rel)
      insertSearchHistoryResult conn historyId
        indexerName
        (riTitle rel)
        (riDownloadUrl rel)
        (riInfoUrl rel)
        (riSize rel)
        (riPublishDate rel)
        (riSeeders rel)
        (riPeers rel)
        (riGrabs rel)
        downloadType
        qualityText
        (scoreRelease rel)
        rank

  pure historyId
