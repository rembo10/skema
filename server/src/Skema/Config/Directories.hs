{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Cross-platform directory management for Skema.
--
-- This module provides platform-aware directory discovery that follows:
-- - XDG Base Directory Specification on Linux/Unix
-- - Windows conventions (AppData) on Windows
-- - macOS conventions (Library) on macOS
--
-- Priority order for directory resolution:
-- 1. Command-line flags (highest priority)
-- 2. Environment variables
-- 3. Config file settings
-- 4. Platform defaults (lowest priority)
module Skema.Config.Directories
  ( -- * Directory types
    SkemaDirectories(..)
  , DirectoryOverrides(..)
    -- * Discovery
  , getSkemaDirectories
  , getPlatformDefaults
    -- * Environment variables
  , envDataDir
  , envCacheDir
  , envStateDir
  , envLogFile
  ) where

import System.Directory
  ( XdgDirectory(..)
  , getXdgDirectory
  , createDirectoryIfMissing
  )
import System.FilePath ((</>), takeDirectory)
import Control.Exception (catch)
-- Note: lookupEnv is provided by Relude

-- | Environment variable names for directory overrides.
envDataDir, envCacheDir, envStateDir, envLogFile :: String
envDataDir = "SKEMA_DATA_DIR"
envCacheDir = "SKEMA_CACHE_DIR"
envStateDir = "SKEMA_STATE_DIR"
envLogFile = "SKEMA_LOG_FILE"

-- | Directory overrides from command-line or config.
data DirectoryOverrides = DirectoryOverrides
  { overrideDataDir :: Maybe FilePath
    -- ^ CLI override (highest priority)
  , overrideCacheDir :: Maybe FilePath
    -- ^ CLI override (highest priority)
  , overrideStateDir :: Maybe FilePath
    -- ^ CLI override (highest priority)
  , overrideLogFile :: Maybe FilePath
    -- ^ CLI override (highest priority)
  , configDataDir :: Maybe FilePath
    -- ^ Config file value (lower priority than CLI/ENV, higher than defaults)
  , configCacheDir :: Maybe FilePath
    -- ^ Config file value (lower priority than CLI/ENV, higher than defaults)
  , configStateDir :: Maybe FilePath
    -- ^ Config file value (lower priority than CLI/ENV, higher than defaults)
  } deriving (Show, Eq)

-- | Resolved Skema directories for the current platform.
data SkemaDirectories = SkemaDirectories
  { dataDir :: FilePath
    -- ^ Persistent data directory (database, etc.)
  , cacheDir :: FilePath
    -- ^ Cache directory (artist images, HTTP cache, etc.)
  , stateDir :: FilePath
    -- ^ State directory (logs, runtime state)
  , logFile :: Maybe FilePath
    -- ^ Optional log file path (Nothing = stdout only)
  } deriving (Show, Eq)

-- | Get Skema directories with the following precedence:
--
-- 1. Command-line overrides (passed as DirectoryOverrides)
-- 2. Environment variables (SKEMA_DATA_DIR, etc.)
-- 3. Config file settings (from overrides parameter)
-- 4. Platform defaults (XDG on Linux, AppData on Windows, Library on macOS)
--
-- This function also ensures all directories exist.
getSkemaDirectories :: DirectoryOverrides -> IO SkemaDirectories
getSkemaDirectories overrides = do
  -- Get platform defaults
  defaults <- getPlatformDefaults

  -- Check environment variables
  envData <- lookupEnv envDataDir
  envCache <- lookupEnv envCacheDir
  envState <- lookupEnv envStateDir
  envLog <- lookupEnv envLogFile

  -- Apply precedence: CLI > ENV > config > defaults
  let resolvedDataDir =
        overrideDataDir overrides
        `orElse` envData
        `orElse` configDataDir overrides
        `orElse` Just (dataDir defaults)
        `orDefault` dataDir defaults

  let resolvedCacheDir =
        overrideCacheDir overrides
        `orElse` envCache
        `orElse` configCacheDir overrides
        `orElse` Just (cacheDir defaults)
        `orDefault` cacheDir defaults

  let resolvedStateDir =
        overrideStateDir overrides
        `orElse` envState
        `orElse` configStateDir overrides
        `orElse` Just (stateDir defaults)
        `orDefault` stateDir defaults

  let resolvedLogFile =
        overrideLogFile overrides
        `orElse` envLog
        `orElse` logFile defaults

  -- Ensure directories exist
  createDirectoryIfMissing True resolvedDataDir
  createDirectoryIfMissing True resolvedCacheDir
  createDirectoryIfMissing True resolvedStateDir

  -- Ensure log directory exists if log file is specified
  case resolvedLogFile of
    Just logPath -> createDirectoryIfMissing True (takeDirectory logPath)
    Nothing -> pure ()

  pure $ SkemaDirectories
    { dataDir = resolvedDataDir
    , cacheDir = resolvedCacheDir
    , stateDir = resolvedStateDir
    , logFile = resolvedLogFile
    }
  where
    -- Helper to apply Maybe precedence
    orElse :: Maybe a -> Maybe a -> Maybe a
    orElse (Just x) _ = Just x
    orElse Nothing y = y

    orDefault :: Maybe a -> a -> a
    orDefault (Just x) _ = x
    orDefault Nothing d = d

-- | Get platform-specific default directories.
--
-- Uses System.Directory.getXdgDirectory which automatically handles:
-- - Linux/Unix: XDG Base Directory Specification
-- - Windows: %APPDATA% and %LOCALAPPDATA%
-- - macOS: ~/Library/Application Support, ~/Library/Caches, ~/Library/Logs
getPlatformDefaults :: IO SkemaDirectories
getPlatformDefaults = do
  -- XdgData:  Linux: ~/.local/share/skema  Windows: %APPDATA%\skema  macOS: ~/Library/Application Support/skema
  -- XdgCache: Linux: ~/.cache/skema        Windows: %LOCALAPPDATA%\skema  macOS: ~/Library/Caches/skema
  -- XdgState: Linux: ~/.local/state/skema  (no direct Windows/macOS equiv, uses cache)

  dataDirPath <- getXdgDirectory XdgData "skema"
  cacheDirPath <- getXdgDirectory XdgCache "skema"

  -- XdgState might not be available on all platforms/versions
  -- Fall back to cache dir if not available
  stateDirPath <- (getXdgDirectory XdgState "skema") `catch`
    (\(_ :: SomeException) -> pure $ cacheDirPath </> "state")

  -- Default log file in state directory
  let defaultLogFile = stateDirPath </> "logs" </> "skema.log"

  pure $ SkemaDirectories
    { dataDir = dataDirPath
    , cacheDir = cacheDirPath
    , stateDir = stateDirPath
    , logFile = Just defaultLogFile  -- By default, enable log files
    }
