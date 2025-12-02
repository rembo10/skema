{-# LANGUAGE OverloadedStrings #-}

module Skema.DownloadClient.Types
  ( DownloadClientAPI(..)
  , DownloadStatus(..)
  , DownloadInfo(..)
  , AddDownloadRequest(..)
  , AddDownloadResult(..)
  ) where

-- | Download status from client
data DownloadStatus
  = DSQueued
  | DSDownloading
  | DSPaused
  | DSCompleted
  | DSFailed
  | DSCancelled
  deriving (Show, Eq)

-- | Information about a download from the client
data DownloadInfo = DownloadInfo
  { diClientId :: Text           -- Client-specific ID
  , diName :: Maybe Text         -- Download name (for matching)
  , diStatus :: DownloadStatus
  , diProgress :: Double         -- 0.0 to 1.0
  , diDownloadPath :: Maybe Text
  , diErrorMessage :: Maybe Text
  , diSizeBytes :: Maybe Integer
  , diDownloadedBytes :: Maybe Integer
  , diUploadRatio :: Maybe Double  -- For torrents
  , diSeeders :: Maybe Int         -- For torrents
  , diPeers :: Maybe Int           -- For torrents
  , diCategory :: Maybe Text       -- Category from client
  } deriving (Show, Eq)

-- | Request to add a new download
data AddDownloadRequest = AddDownloadRequest
  { adrUrl :: Text              -- URL to .nzb or .torrent file
  , adrTitle :: Text            -- Title/label for the download
  , adrCategory :: Maybe Text   -- Category for organization
  , adrPriority :: Maybe Int    -- Priority (client-specific meaning)
  } deriving (Show, Eq)

-- | Result of adding a download
data AddDownloadResult = AddDownloadResult
  { adrClientId :: Text         -- Client-assigned ID
  , adrSuccess :: Bool
  , adrMessage :: Maybe Text
  } deriving (Show, Eq)

-- | Unified download client API
-- Each client implementation should provide these operations
class DownloadClientAPI client where
  -- | Test connection to the download client
  testConnection :: client -> IO (Either Text ())

  -- | Add a new download
  addDownload :: client -> AddDownloadRequest -> IO (Either Text AddDownloadResult)

  -- | Get status of a specific download
  getDownloadStatus :: client -> Text -> IO (Either Text DownloadInfo)

  -- | Get status of all downloads
  getAllDownloads :: client -> IO (Either Text [DownloadInfo])

  -- | Pause a download
  pauseDownload :: client -> Text -> IO (Either Text ())

  -- | Resume a paused download
  resumeDownload :: client -> Text -> IO (Either Text ())

  -- | Remove/delete a download
  removeDownload :: client -> Text -> Bool -> IO (Either Text ())
    -- ^ (client, downloadId, deleteFiles)
