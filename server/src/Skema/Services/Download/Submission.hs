{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | Shared download submission logic.
--
-- This module wires the pure decisions in "Skema.Domain.Download" to IO:
-- it routes a release via 'planSubmission', performs the HTTP call, then
-- persists the resulting row and emits the corresponding events. Used by
-- both the Search orchestrator and the RSS monitor.
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
import Skema.Config.Types (DownloadConfig, DownloadClient(..), downloadClientTypeName)
import Skema.DownloadClient.Types (DownloadClientAPI(..), AddDownloadRequest(..), AddDownloadResult(..))
import Skema.Domain.Download
import Skema.Clock (Clock, getNow)
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
  , dscClock :: Clock
  }

-- | Submit a release to the appropriate download client.
--
-- Selects the client (or rejects), submits to it, records the download in
-- the database, and emits the appropriate events. Returns the download ID
-- on success, 'Nothing' otherwise (including when a failed-download row is
-- still recorded for visibility).
submitDownload
  :: DownloadSubmissionContext
  -> ReleaseInfo
  -> Int64  -- ^ Catalog album ID
  -> IO (Maybe Int64)
submitDownload DownloadSubmissionContext{..} release catalogAlbumId = do
  -- Check for an existing active download to prevent duplicates.
  alreadyExists <- withConnection dscDbPool $ \conn ->
    hasActiveDownloadForAlbum conn catalogAlbumId
  if alreadyExists
    then do
      logLine InfoS $ "Skipping download for album " <> show catalogAlbumId <> ": active download already exists"
      pure Nothing
    else case planSubmission dscDownloadConfig release of
      PlanReject reason -> do
        logLine ErrorS (rejectionMessage reason)
        pure Nothing

      PlanSlskd slskdConfig username files -> do
        logLine InfoS $ "Submitting " <> show (length files) <> " files to slskd from user: " <> username
        let client = createSlskdClient slskdConfig dscHttpClient
        result <- queueDownloads client username (map convertToSlskdFile files)
        persist (slskdSubmission catalogAlbumId release username files result)

      PlanTraditional client -> do
        logLine InfoS $ "Submitting download to client: " <> downloadClientTypeName (dcType client)
        clientInstance <- createClientInstance dscHttpClient client
        addResult <- addDownload clientInstance AddDownloadRequest
          { adrUrl = riDownloadUrl release
          , adrTitle = riTitle release
          , adrCategory = dcCategory client
          , adrPriority = Nothing
          }
        persist (traditionalSubmission catalogAlbumId dscIndexerName release client (second adrClientId addResult))
  where
    -- Persist the planned row, emit its events, and report the outcome.
    persist (DownloadInsert{..}, outcome) = do
      now <- getNow dscClock
      downloadDbId <- withConnection dscDbPool $ \conn ->
        insertDownload conn
          (Just diCatalogAlbumId)
          diIndexerName
          diDownloadUrl
          diDownloadClient
          diClientId
          diStatus
          Nothing  -- download_path
          diTitle
          diSize
          diQuality
          diFormat
          diSeeders
          0.0      -- progress
          diError
          now
      forM_ (outcomeEvents downloadDbId diTitle outcome) $
        publishAndLog dscEventBus dscLogEnv "download"
      runKatipContextT dscLogEnv () "download.submission" (logOutcome downloadDbId outcome)
      pure $ case outcome of
        SubmissionFailed _ -> Nothing
        _ -> Just downloadDbId

    logLine sev msg =
      runKatipContextT dscLogEnv () "download.submission" $
        $(logTM) sev (logStr (msg :: Text))

    logOutcome downloadDbId = \case
      SubmissionFailed err ->
        $(logTM) ErrorS $ logStr ("Download submission failed: " <> err :: Text)
      SubmissionStarted ->
        $(logTM) InfoS $ logStr ("Download started with DB ID: " <> show downloadDbId :: Text)
      SubmissionSlskdStarted _ fileCount _ ->
        $(logTM) InfoS $ logStr ("Successfully queued " <> show fileCount <> " files with slskd" :: Text)

-- | Map a submission outcome to the events to publish once the row exists.
outcomeEvents :: Int64 -> Text -> SubmissionOutcome -> [Event]
outcomeEvents dlId title = \case
  SubmissionFailed err ->
    [ DownloadFailed { downloadId = dlId, downloadTitle = title, downloadError = Just err } ]
  SubmissionStarted ->
    [ DownloadStarted { downloadId = dlId, downloadTitle = title } ]
  SubmissionSlskdStarted username fileCount totalSize ->
    [ DownloadStarted { downloadId = dlId, downloadTitle = title }
    , SlskdFilesQueued
        { slskdQueuedDownloadId = dlId
        , slskdQueuedUsername = username
        , slskdQueuedFileCount = fileCount
        , slskdQueuedTotalSize = totalSize
        }
    ]

-- | Human-readable log message for a rejected submission.
rejectionMessage :: SubmissionRejection -> Text
rejectionMessage = \case
  NoSlskdClient -> "No slskd client configured, cannot submit download"
  SlskdDisabled -> "slskd client is disabled"
  MissingSlskdDetails -> "Release missing slskd username or files"
  NoDownloadClient downloadType ->
    "No " <> downloadTypeLabel downloadType <> " download client configured, cannot queue download"
  DownloadClientDisabled name -> "Download client " <> name <> " is disabled"

downloadTypeLabel :: DownloadType -> Text
downloadTypeLabel NZB = "NZB"
downloadTypeLabel Torrent = "Torrent"
downloadTypeLabel Slskd = "Slskd"

-- | Convert from Indexer.Types.SlskdFile to Slskd.Types.SlskdFile.
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
