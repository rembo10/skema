{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | slskd download handling service.
--
-- This module handles:
-- - Submitting downloads to slskd
-- - Monitoring slskd transfer progress
-- - Emitting progress events
module Skema.Services.Slskd
  ( -- * Dependencies
    SlskdDeps (..)
    -- * Download Submission
  , submitSlskdDownload
    -- * Progress Monitoring
  , runSlskdMonitor
  ) where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.STM as STM
import Data.Time (getCurrentTime)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Katip

import Skema.Config.Types (Config (..), DownloadConfig (..), SlskdConfig (..))
import Skema.Database.Connection (ConnectionPool, withConnection, queryRows, executeQuery)
import Skema.Database.Repository (insertDownload)
import Database.SQLite.Simple (Only(..))
import Skema.Events.Bus (EventBus, publishAndLog)
import Skema.Events.Types (Event (..))
import Skema.HTTP.Client (HttpClient)
import qualified Skema.Indexer.Types as Indexer
import Skema.Indexer.Types (ReleaseInfo (..))
import Skema.Slskd.Client
import qualified Skema.Slskd.Types as Slskd
import Skema.Slskd.Types (SlskdTransfer (..), SlskdTransferState (..))

-- | Dependencies for the slskd service.
data SlskdDeps = SlskdDeps
  { slskdEventBus :: EventBus
  , slskdLogEnv :: LogEnv
  , slskdDbPool :: ConnectionPool
  , slskdConfigVar :: TVar Config
  , slskdHttpClient :: HttpClient
  , slskdProgressMap :: TVar (Map.Map Int64 (Double, Text))
  }

-- | Convert Indexer.SlskdFile to Slskd.SlskdFile for API calls.
convertToSlskdFile :: Indexer.SlskdFile -> Slskd.SlskdFile
convertToSlskdFile f = Slskd.SlskdFile
  { Slskd.sfFilename = Indexer.sfFilename f
  , Slskd.sfSize = Indexer.sfSize f
  , Slskd.sfBitRate = Indexer.sfBitRate f
  , Slskd.sfSampleRate = Indexer.sfSampleRate f
  , Slskd.sfBitDepth = Indexer.sfBitDepth f
  , Slskd.sfLength = Indexer.sfLength f
  , Slskd.sfIsLocked = Indexer.sfIsLocked f
  }

-- | Submit a slskd download.
--
-- This function:
-- 1. Queues all files with slskd
-- 2. Creates a download record in the database
-- 3. Emits a DownloadStarted event
--
-- Returns the download ID if successful.
submitSlskdDownload ::
  SlskdDeps ->
  ReleaseInfo ->
  Int64 -> -- Catalog album ID
  IO (Maybe Int64)
submitSlskdDownload SlskdDeps {..} release catalogAlbumId = do
  let le = slskdLogEnv
  let pool = slskdDbPool
  let bus = slskdEventBus

  -- Get slskd config
  config <- STM.atomically $ STM.readTVar slskdConfigVar
  let maybeSlskdConfig = downloadSlskdClient (download config)

  case maybeSlskdConfig of
    Nothing -> do
      runKatipContextT le () "slskd.submission" $ do
        $(logTM) ErrorS $
          logStr
            ("No slskd client configured, cannot submit download" :: Text)
      pure Nothing
    Just slskdConfig | not (slskdEnabled slskdConfig) -> do
      runKatipContextT le () "slskd.submission" $ do
        $(logTM) ErrorS $
          logStr
            ("slskd client is disabled" :: Text)
      pure Nothing
    Just slskdConfig -> do
      -- Extract slskd-specific fields from release
      case (riSlskdUsername release, riSlskdFiles release) of
        (Just username, Just files) | not (null files) -> do
          runKatipContextT le () "slskd.submission" $ do
            $(logTM) InfoS $
              logStr $
                ( "Submitting "
                    <> show (length files)
                    <> " files to slskd from user: "
                    <> username
                    :: Text
                )

          -- Create slskd client
          let client = createSlskdClient slskdConfig slskdHttpClient

          -- Convert files from Indexer type to Slskd type for API
          let slskdFiles = map convertToSlskdFile files

          -- Queue all files
          result <- queueDownloads client username slskdFiles
          case result of
            Left err -> do
              runKatipContextT le () "slskd.submission" $ do
                $(logTM) ErrorS $
                  logStr $
                    ("Failed to queue downloads: " <> err :: Text)

              -- Insert failed download record
              now <- getCurrentTime
              downloadId <-
                withConnection pool $ \conn ->
                  insertDownload
                    conn
                    (Just catalogAlbumId)
                    "slskd"
                    (username <> ":" <> riTitle release)
                    "slskd"
                    Nothing -- No client ID for failed
                    "failed"
                    Nothing -- download_path
                    (riTitle release)
                    (riSize release)
                    Nothing -- quality (stored separately)
                    (Just "Slskd")
                    Nothing -- seeders
                    0.0 -- progress
                    (Just err) -- error_message
                    now

              -- Emit download failed event
              publishAndLog bus le "slskd" $
                DownloadFailed
                  { downloadId = downloadId
                  , downloadTitle = riTitle release
                  , downloadError = Just err
                  }

              pure Nothing
            Right () -> do
              runKatipContextT le () "slskd.submission" $ do
                $(logTM) InfoS $
                  logStr $
                    ("Successfully queued " <> show (length files) <> " files" :: Text)

              -- Insert download record
              now <- getCurrentTime
              downloadId <-
                withConnection pool $ \conn ->
                  insertDownload
                    conn
                    (Just catalogAlbumId)
                    "slskd"
                    (username <> ":" <> riTitle release)
                    "slskd"
                    (Just $ username <> ":" <> riTitle release) -- client_id
                    "downloading"
                    Nothing -- download_path
                    (riTitle release)
                    (riSize release)
                    Nothing -- quality
                    (Just "Slskd")
                    Nothing -- seeders
                    0.0 -- progress
                    Nothing -- error_message
                    now

              -- Emit download started event
              publishAndLog bus le "slskd" $
                DownloadStarted
                  { downloadId = downloadId
                  , downloadTitle = riTitle release
                  }

              -- Emit slskd-specific event
              publishAndLog bus le "slskd" $
                SlskdFilesQueued
                  { slskdQueuedDownloadId = downloadId
                  , slskdQueuedUsername = username
                  , slskdQueuedFileCount = length files
                  , slskdQueuedTotalSize = sum $ map Indexer.sfSize files
                  }

              pure $ Just downloadId
        _ -> do
          runKatipContextT le () "slskd.submission" $ do
            $(logTM) ErrorS $
              logStr
                ("Release missing slskd username or files" :: Text)
          pure Nothing

-- | Run the slskd progress monitor.
--
-- This service polls slskd for transfer status and emits
-- DownloadProgress events. It groups transfers by username/directory
-- to track album-level progress.
runSlskdMonitor :: SlskdDeps -> IO ()
runSlskdMonitor SlskdDeps {..} = forever $ do
  -- Check every 5 seconds
  threadDelay (5 * 1000000)

  -- Get config
  config <- STM.atomically $ STM.readTVar slskdConfigVar
  let maybeSlskdConfig = downloadSlskdClient (download config)

  case maybeSlskdConfig of
    Nothing -> pure () -- No slskd configured
    Just slskdConfig | not (slskdEnabled slskdConfig) -> pure ()
    Just slskdConfig -> do
      -- Check if there are any active slskd downloads before polling the API
      activeDownloads <- withConnection slskdDbPool $ \conn ->
        queryRows conn
          "SELECT COUNT(*) FROM downloads WHERE download_client = 'slskd' AND status IN ('downloading', 'queued')"
          () :: IO [Only Int]

      let hasActiveDownloads = case activeDownloads of
            [Only count] -> count > 0
            _ -> False

      -- Only poll slskd API if there are active downloads
      when hasActiveDownloads $ do
        let client = createSlskdClient slskdConfig slskdHttpClient

        -- Get all transfers
        result <- getTransfers client
        case result of
          Left err -> do
            runKatipContextT slskdLogEnv () "slskd.monitor" $ do
              $(logTM) WarningS $
                logStr $
                  ("Failed to get transfers: " <> err :: Text)
          Right transfers -> do
            -- Log transfer count for debugging
            when (not (null transfers)) $ do
              runKatipContextT slskdLogEnv () "slskd.monitor" $ do
                $(logTM) InfoS $
                  logStr $
                    ("Found " <> show (length transfers) <> " active transfers" :: Text)

            -- Group transfers by username to track album-level progress
            let grouped = groupTransfersByUser transfers
            forM_ (Map.toList grouped) $ \(username, userTransfers) -> do
              -- Calculate aggregate stats for this user's transfers
              let totalFiles = length userTransfers
                  completedFiles = length $ filter isCompleted userTransfers
                  erroredFiles = filter isErrored userTransfers
                  inProgressFiles = filter isInProgress userTransfers

                  -- Calculate progress using bytes transferred for accuracy
                  totalBytes = sum $ map stSize userTransfers
                  transferredBytes = sum $ map stBytesTransferred userTransfers

                  -- Progress as 0.0-1.0 based on bytes
                  progress = if totalBytes > 0
                    then fromIntegral transferredBytes / fromIntegral totalBytes
                    else 0.0

                  -- Determine display status
                  displayStatus
                    | not (null erroredFiles) = "failed"
                    | completedFiles == totalFiles && totalFiles > 0 = "completed"
                    | not (null inProgressFiles) = "downloading"
                    | transferredBytes > 0 = "downloading"
                    | otherwise = "queued"

              -- Update progress map for UI
              -- Find downloads for this username
              downloads <- withConnection slskdDbPool $ \conn ->
                queryRows conn
                  "SELECT id, title FROM downloads WHERE download_client = 'slskd' AND download_client_id LIKE ? AND status IN ('downloading', 'queued')"
                  (Only (username <> ":%"))

              -- Log progress for debugging
              runKatipContextT slskdLogEnv () "slskd.monitor" $ do
                $(logTM) InfoS $
                  logStr $
                    ("User " <> username <> ": " <> show completedFiles <> "/" <> show totalFiles
                     <> " files, " <> show transferredBytes <> "/" <> show totalBytes
                     <> " bytes, progress=" <> show (round (progress * 100) :: Int) <> "%" :: Text)

              forM_ (downloads :: [(Int64, Text)]) $ \(downloadId, title) -> do
                -- Update in-memory progress
                STM.atomically $ STM.modifyTVar' slskdProgressMap $
                  Map.insert downloadId (progress, displayStatus)

                -- Emit progress event for SSE
                when (displayStatus == "downloading") $ do
                  publishAndLog slskdEventBus slskdLogEnv "slskd.monitor" $
                    DownloadProgress
                      { downloadId = downloadId
                      , downloadTitle = title
                      , downloadProgress = progress
                      , downloadSizeBytes = Just totalBytes
                      , downloadedBytes = Just transferredBytes
                      }

                -- If there are errors, mark download as failed
                when (not (null erroredFiles)) $ do
                  let errorMsg = T.intercalate "; " $ map formatTransferError erroredFiles
                  runKatipContextT slskdLogEnv () "slskd.monitor" $ do
                    $(logTM) WarningS $
                      logStr $
                        ("Download " <> show downloadId <> " has failed transfers: " <> errorMsg :: Text)

                  -- Update database status to failed
                  withConnection slskdDbPool $ \conn ->
                    executeQuery conn
                      "UPDATE downloads SET status = 'failed', error_message = ?, progress = ? WHERE id = ?"
                      (errorMsg, progress, downloadId)

                  -- Remove from progress map
                  STM.atomically $ STM.modifyTVar' slskdProgressMap $
                    Map.delete downloadId

                  -- Emit failure event
                  publishAndLog slskdEventBus slskdLogEnv "slskd.monitor" $
                    DownloadFailed
                      { downloadId = downloadId
                      , downloadTitle = ""
                      , downloadError = Just errorMsg
                      }

                -- If all completed, mark download as completed
                when (completedFiles == totalFiles && totalFiles > 0 && null erroredFiles) $ do
                  -- Extract the album directory from the transfer filenames
                  -- slskd filenames look like: @@username\path\to\album\track.flac
                  -- We need to find the common parent directory
                  let albumDir = extractAlbumDirectory slskdConfig username userTransfers

                  runKatipContextT slskdLogEnv () "slskd.monitor" $ do
                    $(logTM) InfoS $
                      logStr $
                        ("Download " <> show downloadId <> " completed all " <> show totalFiles <> " files to: " <> albumDir :: Text)

                  let downloadPath = Just albumDir

                  -- Update database status to completed
                  withConnection slskdDbPool $ \conn ->
                    executeQuery conn
                      "UPDATE downloads SET status = 'completed', progress = 100, download_path = ? WHERE id = ?"
                      (downloadPath, downloadId)

                  -- Remove from progress map
                  STM.atomically $ STM.modifyTVar' slskdProgressMap $
                    Map.delete downloadId

                  -- Get download title for event
                  maybeTitle <- withConnection slskdDbPool $ \conn -> do
                    rows <- queryRows conn
                      "SELECT title FROM downloads WHERE id = ?"
                      (Only downloadId) :: IO [Only Text]
                    pure $ case rows of
                      [Only t] -> Just t
                      _ -> Nothing

                  -- Emit completion event
                  publishAndLog slskdEventBus slskdLogEnv "slskd.monitor" $
                    DownloadCompleted
                      { downloadId = downloadId
                      , downloadTitle = fromMaybe "" maybeTitle
                      , downloadPath = downloadPath
                      }
  where
    -- Group transfers by username
    groupTransfersByUser :: [SlskdTransfer] -> Map.Map Text [SlskdTransfer]
    groupTransfersByUser = foldr insertTransfer Map.empty
      where
        insertTransfer t = Map.insertWith (++) (stUsername t) [t]

    -- Check transfer state helpers
    isCompleted t = stState t == TransferCompleted
    isErrored t = stState t == TransferErrored
    isInProgress t = stState t == TransferInProgress

    -- Format error message from transfer
    formatTransferError :: SlskdTransfer -> Text
    formatTransferError t =
      let filename = T.takeWhileEnd (/= '\\') $ T.takeWhileEnd (/= '/') $ stFilename t
      in filename <> ": " <> fromMaybe "unknown error" (stException t)

    -- Extract the album directory from transfer filenames
    -- slskd downloads files to: <download_dir>/<album_folder>
    -- Remote paths look like: @@username\Music\Artist\Album\track.flac
    -- slskd extracts just the album folder name (parent of the files)
    extractAlbumDirectory :: SlskdConfig -> Text -> [SlskdTransfer] -> Text
    extractAlbumDirectory config _username transfers =
      let baseDir = slskdDownloadDirectory config
          -- Get all directories from filenames (strip the filename, keep directory)
          directories = map (getDirectory . stFilename) transfers
          -- Find the common prefix among all directories
          commonDir = case directories of
            [] -> ""
            (d:ds) -> foldl' commonPrefix d ds
          -- slskd stores files using just the album folder name (last component of path)
          -- e.g., "@@user\Music\Artist\Album" -> "Album"
          albumFolder = T.takeWhileEnd (\c -> c /= '\\' && c /= '/') $
                        T.dropWhileEnd (\c -> c == '\\' || c == '/') commonDir
      in baseDir <> "/" <> albumFolder

    -- Get directory part of a path (works with both / and \ separators)
    getDirectory :: Text -> Text
    getDirectory path =
      let normalized = T.replace "\\" "/" path
          parts = T.splitOn "/" normalized
      in T.intercalate "/" (init' parts)

    init' :: [a] -> [a]
    init' [] = []
    init' [_] = []
    init' (x:xs) = x : init' xs

    -- Find common prefix of two paths
    commonPrefix :: Text -> Text -> Text
    commonPrefix a b =
      let partsA = T.splitOn "/" (T.replace "\\" "/" a)
          partsB = T.splitOn "/" (T.replace "\\" "/" b)
          common = takeWhile (uncurry (==)) (zip partsA partsB)
      in T.intercalate "/" (map fst common)
