{-# LANGUAGE OverloadedStrings #-}

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

import Skema.Services.Dependencies (DownloadDeps)
import Skema.Services.Download.Client (DownloadClientInstance(..))
import Skema.Services.Download.Search (runSearchOrchestrator)
import Skema.Services.Download.Monitor (runDownloadMonitor)
import Control.Concurrent.Async (Async, async)

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
