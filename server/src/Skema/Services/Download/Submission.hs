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
import Skema.Config.Types (DownloadClient(..), DownloadConfig(..), downloadClientTypeName)
import Skema.DownloadClient.Types (DownloadClientAPI(..), AddDownloadRequest(..), AddDownloadResult(..))
import Skema.Domain.Quality (qualityToText)
import Data.Time (getCurrentTime)
import Katip
import Skema.HTTP.Client (HttpClient)

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
submitDownload DownloadSubmissionContext{..} release catalogAlbumId = do
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
