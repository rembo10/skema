{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

-- | Shared download submission logic.
--
-- This module contains the common logic for submitting releases to download clients.
-- Used by both the Search orchestrator and RSS monitor.
module Skema.Services.Download.Submission
  ( submitDownload
  , DownloadSubmissionContext(..)
  ) where

import Skema.Services.Download.Client (createClientInstance)
import Skema.Events.Bus
import Skema.Events.Types
import Skema.Database.Connection
import Skema.Database.Repository
import Skema.Indexer.Types
import Skema.Config.Types (DownloadClient(..), DownloadConfig(..), SlskdConfig(..), downloadClientTypeName)
import Skema.DownloadClient.Types (DownloadClientAPI(..), AddDownloadRequest(..), AddDownloadResult(..))
import Skema.Domain.Quality (qualityToText)
import Data.Time (getCurrentTime)
import Katip
import Skema.HTTP.Client (HttpClient)
import Skema.Slskd.Client (createSlskdClient, queueDownloads)
import qualified Skema.Slskd.Types as Slskd

-- | Context needed for download submission
data DownloadSubmissionContext = DownloadSubmissionContext
  { dscEventBus :: EventBus
  , dscLogEnv :: LogEnv
  , dscDbPool :: ConnectionPool
  , dscHttpClient :: HttpClient
  , dscDownloadConfig :: DownloadConfig
  , dscIndexerName :: Text
  }

-- | Submit a release to the appropriate download client.
--
-- This function:
-- 1. Selects the appropriate download client based on download type
-- 2. Submits the download to the client
-- 3. Records the download in the database
-- 4. Emits appropriate events (DownloadStarted or DownloadFailed)
--
-- Returns the download ID if successful, Nothing otherwise.
submitDownload
  :: DownloadSubmissionContext
  -> ReleaseInfo
  -> Int64  -- ^ Catalog album ID
  -> IO (Maybe Int64)
submitDownload ctx@DownloadSubmissionContext{..} release catalogAlbumId = do
  -- Handle slskd downloads separately
  case riDownloadType release of
    Slskd -> submitSlskdDownload ctx release catalogAlbumId
    _ -> submitTraditionalDownload ctx release catalogAlbumId

-- | Submit a slskd download.
submitSlskdDownload
  :: DownloadSubmissionContext
  -> ReleaseInfo
  -> Int64  -- ^ Catalog album ID
  -> IO (Maybe Int64)
submitSlskdDownload DownloadSubmissionContext{..} release catalogAlbumId = do
  let le = dscLogEnv
  let pool = dscDbPool
  let bus = dscEventBus
  let httpClient = dscHttpClient
  let downloadConfig = dscDownloadConfig

  case downloadSlskdClient downloadConfig of
    Nothing -> do
      runKatipContextT le () "download.submission" $ do
        $(logTM) ErrorS $ logStr ("No slskd client configured, cannot submit download" :: Text)
      pure Nothing
    Just slskdConfig | not (slskdEnabled slskdConfig) -> do
      runKatipContextT le () "download.submission" $ do
        $(logTM) ErrorS $ logStr ("slskd client is disabled" :: Text)
      pure Nothing
    Just slskdConfig -> do
      case (riSlskdUsername release, riSlskdFiles release) of
        (Just username, Just files) | not (null files) -> do
          runKatipContextT le () "download.submission" $ do
            $(logTM) InfoS $ logStr $ ("Submitting " <> show (length files) <> " files to slskd from user: " <> username :: Text)

          let client = createSlskdClient slskdConfig httpClient
              slskdFiles = map convertToSlskdFile files

          result <- queueDownloads client username slskdFiles
          case result of
            Left err -> do
              runKatipContextT le () "download.submission" $ do
                $(logTM) ErrorS $ logStr $ ("Failed to queue slskd downloads: " <> err :: Text)

              -- Insert failed download record
              now <- getCurrentTime
              downloadId <- withConnection pool $ \conn ->
                insertDownload conn
                  (Just catalogAlbumId)
                  "slskd"
                  (username <> ":" <> riTitle release)
                  "slskd"
                  Nothing
                  "failed"
                  Nothing
                  (riTitle release)
                  (riSize release)
                  (Just $ qualityToText $ riQuality release)
                  (Just "Slskd")
                  (riSeeders release)
                  0.0
                  (Just err)
                  now

              publishAndLog bus le "download" $ DownloadFailed
                { downloadId = downloadId
                , downloadTitle = riTitle release
                , downloadError = Just err
                }

              pure Nothing

            Right () -> do
              runKatipContextT le () "download.submission" $ do
                $(logTM) InfoS $ logStr $ ("Successfully queued " <> show (length files) <> " files with slskd" :: Text)

              -- Insert download record
              now <- getCurrentTime
              downloadId <- withConnection pool $ \conn ->
                insertDownload conn
                  (Just catalogAlbumId)
                  "slskd"
                  (username <> ":" <> riTitle release)
                  "slskd"
                  (Just $ username <> ":" <> riTitle release)
                  "downloading"
                  Nothing
                  (riTitle release)
                  (riSize release)
                  (Just $ qualityToText $ riQuality release)
                  (Just "Slskd")
                  (riSeeders release)
                  0.0
                  Nothing
                  now

              publishAndLog bus le "download" $ DownloadStarted
                { downloadId = downloadId
                , downloadTitle = riTitle release
                }

              publishAndLog bus le "download" $ SlskdFilesQueued
                { slskdQueuedDownloadId = downloadId
                , slskdQueuedUsername = username
                , slskdQueuedFileCount = length files
                , slskdQueuedTotalSize = sum $ map sfSize files
                }

              pure $ Just downloadId

        _ -> do
          runKatipContextT le () "download.submission" $ do
            $(logTM) ErrorS $ logStr ("Release missing slskd username or files" :: Text)
          pure Nothing
  where
    -- Convert from Indexer.Types.SlskdFile to Slskd.Types.SlskdFile
    convertToSlskdFile :: SlskdFile -> Slskd.SlskdFile
    convertToSlskdFile f = Slskd.SlskdFile
      { Slskd.sfFilename = sfFilename f
      , Slskd.sfSize = sfSize f
      , Slskd.sfBitRate = sfBitRate f
      , Slskd.sfSampleRate = sfSampleRate f
      , Slskd.sfBitDepth = sfBitDepth f
      , Slskd.sfLength = sfLength f
      , Slskd.sfIsLocked = sfIsLocked f
      }

-- | Submit a traditional (NZB/Torrent) download.
submitTraditionalDownload
  :: DownloadSubmissionContext
  -> ReleaseInfo
  -> Int64  -- ^ Catalog album ID
  -> IO (Maybe Int64)
submitTraditionalDownload DownloadSubmissionContext{..} release catalogAlbumId = do
  let le = dscLogEnv
  let pool = dscDbPool
  let bus = dscEventBus
  let httpClient = dscHttpClient
  let downloadConfig = dscDownloadConfig
  let indexerName = dscIndexerName

  runKatipContextT le () "download.submission" $ do
    -- Pick appropriate download client based on download type
    let maybeClient = case riDownloadType release of
          NZB -> downloadNzbClient downloadConfig
          Torrent -> downloadTorrentClient downloadConfig
          Slskd -> Nothing  -- Handled separately

    case maybeClient of
      Nothing -> do
        let clientType = case riDownloadType release of
              NZB -> "NZB" :: Text
              Torrent -> "Torrent"
        $(logTM) ErrorS $ logStr $ ("No " <> clientType <> " download client configured, cannot queue download" :: Text)
        pure Nothing

      Just client | not (dcEnabled client) -> do
        $(logTM) ErrorS $ logStr $ ("Download client " <> downloadClientTypeName (dcType client) <> " is disabled" :: Text)
        pure Nothing

      Just client -> do
        $(logTM) InfoS $ logStr $ ("Submitting download to client: " <> downloadClientTypeName (dcType client) :: Text)
        $(logTM) InfoS $ logStr $ ("Release: " <> riTitle release :: Text)
        $(logTM) InfoS $ logStr $ ("Category: " <> fromMaybe "<none>" (dcCategory client) :: Text)

        -- Create download client instance
        clientInstance <- liftIO $ createClientInstance httpClient client

        -- Submit to download client
        addResult <- liftIO $ addDownload clientInstance (AddDownloadRequest
          { adrUrl = riDownloadUrl release
          , adrTitle = riTitle release
          , adrCategory = dcCategory client  -- Use category from config
          , adrPriority = Nothing
          })

        case addResult of
          Left err -> do
            $(logTM) ErrorS $ logStr $ ("Failed to add download to client: " <> err :: Text)

            -- Insert failed download record
            now <- liftIO getCurrentTime
            downloadId <- liftIO $ withConnection pool $ \conn ->
              insertDownload conn
                (Just catalogAlbumId)
                indexerName
                (riDownloadUrl release)
                (downloadClientTypeName (dcType client))
                Nothing  -- no client_id since it failed
                "failed"
                Nothing  -- download_path
                (riTitle release)
                (riSize release)
                (Just $ qualityToText $ riQuality release)  -- Store parsed quality
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

            pure Nothing

          Right addResultData -> do
            $(logTM) InfoS $ logStr $ ("Download added to client with ID: " <> adrClientId addResultData :: Text)

            -- Insert download record into database with client_id
            now <- liftIO getCurrentTime
            downloadId <- liftIO $ withConnection pool $ \conn ->
              insertDownload conn
                (Just catalogAlbumId)
                indexerName
                (riDownloadUrl release)
                (downloadClientTypeName (dcType client))
                (Just $ adrClientId addResultData)
                "downloading"
                Nothing  -- download_path
                (riTitle release)
                (riSize release)
                (Just $ qualityToText $ riQuality release)  -- Store parsed quality
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

            pure $ Just downloadId
