{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Safe configuration file operations with atomic writes and backups.
module Skema.Config.Safe
  ( safeWriteConfig
  , createBackup
  , atomicWriteConfig
  ) where

import Skema.Config.Types (Config)
import Data.Yaml (encodeFile, decodeFileEither, prettyPrintParseException)
import System.Directory (copyFile, doesFileExist, renameFile, removeFile)
import System.FilePath ((<.>))
import Control.Exception (catch, SomeException, try)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

-- | Safely write config with atomic write.
--
-- Process:
-- 1. Write to temporary file
-- 2. Validate the written file can be parsed
-- 3. Atomically rename temp file to target
-- 4. If any step fails, preserve original file
--
-- Note: Does not create backups automatically. Use createBackup explicitly if needed.
safeWriteConfig :: FilePath -> Config -> IO (Either Text ())
safeWriteConfig = atomicWriteConfig

-- | Create a timestamped backup of the config file.
--
-- Backups are named: config.yaml.backup.2025-11-15T14-30-00
-- Keeps the original file intact.
createBackup :: FilePath -> IO (Either Text FilePath)
createBackup configPath = do
  exists <- doesFileExist configPath
  if not exists
    then pure $ Right configPath  -- No backup needed for new files
    else do
      -- Generate timestamp for backup filename
      now <- getCurrentTime
      let timestamp = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" now
      let backupPath = configPath <.> "backup" <.> timestamp

      -- Copy file to backup
      result <- try $ copyFile configPath backupPath :: IO (Either SomeException ())
      case result of
        Left ex -> pure $ Left $ "Backup failed: " <> show ex
        Right () -> pure $ Right backupPath

-- | Atomically write config to prevent corruption.
--
-- Writes to a temporary file, validates it, then atomically renames.
-- This ensures the config file is never in a partial/corrupt state.
atomicWriteConfig :: FilePath -> Config -> IO (Either Text ())
atomicWriteConfig configPath cfg = do
  let tempPath = configPath <.> "tmp"

  -- Write to temporary file
  writeResult <- try $ encodeFile tempPath cfg :: IO (Either SomeException ())
  case writeResult of
    Left ex -> do
      -- Clean up temp file if it exists
      catch (removeFile tempPath) (\(_ :: SomeException) -> pure ())
      pure $ Left $ "Failed to write temp config: " <> show ex

    Right () -> do
      -- Validate that the temp file can be parsed back
      validateResult <- decodeFileEither tempPath
      case validateResult of
        Left parseErr -> do
          -- Validation failed - remove bad temp file
          catch (removeFile tempPath) (\(_ :: SomeException) -> pure ())
          pure $ Left $ "Config validation failed: " <> toText (prettyPrintParseException parseErr)

        Right (_ :: Config) -> do
          -- Valid config - atomically rename temp to actual
          renameResult <- try $ renameFile tempPath configPath :: IO (Either SomeException ())
          case renameResult of
            Left ex -> do
              catch (removeFile tempPath) (\(_ :: SomeException) -> pure ())
              pure $ Left $ "Failed to rename temp config: " <> show ex
            Right () ->
              pure $ Right ()
