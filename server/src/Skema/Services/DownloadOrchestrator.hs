{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Download orchestrator service - coordinates multi-indexer searches.
--
-- This service listens for WantedAlbumAdded events and:
-- 1. Searches all enabled indexers concurrently
-- 2. Waits for all results with timeout
-- 3. Aggregates and ranks results
-- 4. Picks the best release
-- 5. Adds download to queue
-- 6. Emits appropriate status events
module Skema.Services.DownloadOrchestrator
  ( startDownloadOrchestrator
  ) where

import Skema.Services.Types
import Skema.Events.Bus
import Skema.Events.Types
import Skema.Database.Connection
import Skema.Database.Repository
import Skema.HTTP.Client (HttpClient)
import Skema.Indexer.Search (rankResultsWithContext)
import Skema.Indexer.Client (searchIndexer)
import Skema.Indexer.Types
import Skema.Config.Types (Config(..), Indexer(..), IndexerConfig(..), DownloadClient(..), DownloadConfig(..), DownloadClientType(..), downloadClientTypeName)
import Skema.DownloadClient.Types (DownloadClientAPI(..), AddDownloadRequest(..), AddDownloadResult(..))
import Skema.DownloadClient.SABnzbd (createSABnzbdClient)
import Skema.DownloadClient.Transmission (createTransmissionClient)
import Skema.DownloadClient.QBittorrent (createQBittorrentClient)
import Skema.Services.DownloadMonitor (DownloadClientInstance(..))
import Control.Concurrent.Async (async, race, mapConcurrently)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (readTChan, readTVar, atomically)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Control.Exception (try)
import Data.Time (getCurrentTime, diffUTCTime)
import qualified Data.Text as T
import Katip

-- | Start the download orchestrator service.
--
-- This service listens for WantedAlbumAdded events and coordinates
-- searching across all indexers to find the best release.
startDownloadOrchestrator :: ServiceContext -> IO ()
startDownloadOrchestrator ctx = do
  chan <- STM.atomically $ subscribe (scEventBus ctx)
  _ <- async $ forever $ do
    envelope <- STM.atomically $ readTChan chan
    case envelopeEvent envelope of
      WantedAlbumAdded{..} -> do
        -- Process each event asynchronously to avoid blocking the event loop
        _ <- async $ do
          result <- try $ handleWantedAlbumAdded ctx wantedCatalogAlbumId wantedReleaseGroupId wantedAlbumTitle wantedArtistName
          case result of
            Left (e :: SomeException) -> do
              let le = scLogEnv ctx
              runKatipContextT le () "orchestrator.error" $ do
                $(logTM) ErrorS $ logStr $ ("Exception searching for album " <> wantedAlbumTitle <> ": " <> show e :: Text)
            Right () -> pure ()
        pure ()
      _ -> pure ()  -- Ignore other events
  pure ()

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
  let initialNamespace = "services.orchestrator"

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
          liftIO $ publishAndLog bus le "orchestrator" $ AlbumSearchStarted
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
              liftIO $ publishAndLog bus le "orchestrator" $ AlbumSearchCompleted
                { searchTotalResults = 0
                , searchBestScore = Nothing
                , searchDuration = searchDuration
                }

            Right searchResults -> do
              -- Got results from indexers
              let allReleases = concat $ map irReleases $ srResults searchResults
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
              liftIO $ publishAndLog bus le "orchestrator" $ AlbumSearchCompleted
                { searchTotalResults = totalResults
                , searchBestScore = bestScore
                , searchDuration = searchDuration
                }

              -- If we found releases, pick the best one and queue download
              case bestRelease of
                Nothing -> do
                  $(logTM) WarningS $ logStr ("No releases found for album" :: Text)

                Just release -> do
                  $(logTM) InfoS $ logStr $ ("Best release: " <> riTitle release :: Text)

                  -- Emit best release selected event
                  liftIO $ publishAndLog bus le "orchestrator" $ BestReleaseSelected
                    { selectedTitle = riTitle release
                    , selectedIndexer = "unknown"  -- TODO: track indexer name with release
                    , selectedScore = scoreRelease release
                    , selectedSeeders = riSeeders release
                    }

                  -- Pick a download client based on download type
                  let maybeClient = case riDownloadType release of
                        NZB -> downloadNzbClient downloadConfig
                        Torrent -> downloadTorrentClient downloadConfig

                  case maybeClient of
                    Nothing -> do
                      let clientType = case riDownloadType release of
                            NZB -> "NZB" :: Text
                            Torrent -> "Torrent"
                      $(logTM) ErrorS $ logStr $ ("No " <> clientType <> " download client configured, cannot queue download" :: Text)

                    Just client | not (dcEnabled client) -> do
                      $(logTM) ErrorS $ logStr $ ("Download client " <> downloadClientTypeName (dcType client) <> " is disabled, cannot queue download" :: Text)

                    Just client -> do
                      $(logTM) InfoS $ logStr $ ("Submitting download to client: " <> downloadClientTypeName (dcType client) :: Text)
                      $(logTM) InfoS $ logStr $ ("Category: " <> fromMaybe "<none>" (dcCategory client) :: Text)

                      -- Create download client instance
                      clientInstance <- liftIO $ createClientInstance httpClient client

                      -- Submit to download client
                      addResult <- liftIO $ addDownload clientInstance (AddDownloadRequest
                        { adrUrl = riDownloadUrl release
                        , adrTitle = riTitle release
                        , adrCategory = dcCategory client  -- Use category from config
                        , adrPriority = Nothing  -- Priority not needed (only one client per type)
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
                          liftIO $ publishAndLog bus le "orchestrator" $ DownloadFailed
                            { downloadId = downloadId
                            , downloadTitle = riTitle release
                            , downloadError = Just err
                            }

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
                          liftIO $ publishAndLog bus le "orchestrator" $ DownloadStarted
                            { downloadId = downloadId
                            , downloadTitle = riTitle release
                            }

                          $(logTM) InfoS $ logStr $ ("Download started with DB ID: " <> show downloadId :: Text)

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
        runKatipContextT le () "orchestrator" $ do
          $(logTM) WarningS $ logStr $ ("Indexer " <> indexerName indexer <> " failed: " <> ieError err :: Text)
        publishAndLog bus le "orchestrator" $ IndexerSearchCompleted
          { searchIndexerName = indexerName indexer
          , searchResultCount = 0
          , searchDuration = duration
          }
        pure $ Left err

      Right indexerResult -> do
        let resultCount = length (irReleases indexerResult)
        runKatipContextT le () "orchestrator" $ do
          $(logTM) InfoS $ logStr $ ("Indexer " <> indexerName indexer <> " returned " <> show resultCount <> " results" :: Text)
        publishAndLog bus le "orchestrator" $ IndexerSearchCompleted
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
  , sqCategories = [3000, 3010, 3020]  -- Audio, MP3, FLAC
  , sqLimit = 50
  , sqOffset = 0
  }

-- | Calculate quality score for a release (from Indexer.Search).
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
