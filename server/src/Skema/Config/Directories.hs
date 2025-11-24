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
  , envLogFile
  ) where

import System.Directory
  ( XdgDirectory(..)
  , getXdgDirectory
  , createDirectoryIfMissing
  )
import System.FilePath ((</>), takeDirectory)
-- Note: lookupEnv is provided by Relude

-- | Environment variable names for directory overrides.
envDataDir, envCacheDir, envLogFile :: String
envDataDir = "SKEMA_DATA_DIR"
envCacheDir = "SKEMA_CACHE_DIR"
envLogFile = "SKEMA_LOG_FILE"

-- | Directory overrides from command-line or config.
data DirectoryOverrides = DirectoryOverrides
  { overrideDataDir :: Maybe FilePath
    -- ^ CLI override (highest priority)
  , overrideCacheDir :: Maybe FilePath
    -- ^ CLI override (highest priority)
  , overrideLogFile :: Maybe FilePath
    -- ^ CLI override (highest priority)
  , configDataDir :: Maybe FilePath
    -- ^ Config file value (lower priority than CLI/ENV, higher than defaults)
  , configCacheDir :: Maybe FilePath
    -- ^ Config file value (lower priority than CLI/ENV, higher than defaults)
  } deriving (Show, Eq)

-- | Resolved Skema directories for the current platform.
data SkemaDirectories = SkemaDirectories
  { dataDir :: FilePath
    -- ^ Persistent data directory (database, logs, etc.)
  , cacheDir :: FilePath
    -- ^ Cache directory (artist images, HTTP cache, etc.)
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

  -- Log file: if data dir was overridden, update log path to be in data dir
  let defaultLogInDataDir = Just (resolvedDataDir </> "logs" </> "skema.log")
  let resolvedLogFile =
        overrideLogFile overrides
        `orElse` envLog
        `orElse` defaultLogInDataDir

  -- Ensure directories exist
  createDirectoryIfMissing True resolvedDataDir
  createDirectoryIfMissing True resolvedCacheDir

  -- Ensure log directory exists if log file is specified
  case resolvedLogFile of
    Just logPath -> createDirectoryIfMissing True (takeDirectory logPath)
    Nothing -> pure ()

  pure $ SkemaDirectories
    { dataDir = resolvedDataDir
    , cacheDir = resolvedCacheDir
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

  dataDirPath <- getXdgDirectory XdgData "skema"
  cacheDirPath <- getXdgDirectory XdgCache "skema"

  -- Default log file in data directory
  let defaultLogFile = dataDirPath </> "logs" </> "skema.log"

  pure $ SkemaDirectories
    { dataDir = dataDirPath
    , cacheDir = cacheDirPath
    , logFile = Just defaultLogFile  -- By default, enable log files
    }
