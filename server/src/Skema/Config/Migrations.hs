{-# LANGUAGE OverloadedStrings #-}

-- | Config migrations for version upgrades.
--
-- Handles migrating config files from older versions to the current format.
module Skema.Config.Migrations
  ( migrateConfig
  , needsMigration
  ) where

import Skema.Config.Types (Config(..), currentConfigVersion)
import System.IO (hPutStrLn, stderr)

-- | Check if config needs migration.
needsMigration :: Config -> Bool
needsMigration cfg = configVersion cfg < currentConfigVersion

-- | Migrate config to current version.
--
-- Applies migrations sequentially from the config's version to current.
-- Returns the migrated config or an error message.
migrateConfig :: Config -> IO (Either Text Config)
migrateConfig cfg
  | not (needsMigration cfg) = pure $ Right cfg
  | otherwise = do
      hPutStrLn stderr $ "[CONFIG] Migrating from version " <> show (configVersion cfg) <> " to " <> show currentConfigVersion
      runMigrations cfg (configVersion cfg)

-- | Run migrations from a specific version to current.
runMigrations :: Config -> Int -> IO (Either Text Config)
runMigrations cfg fromVersion
  | fromVersion >= currentConfigVersion = pure $ Right cfg
  | otherwise = do
      -- Apply migration for the next version
      result <- migrateTo (fromVersion + 1) cfg
      case result of
        Left err -> pure $ Left err
        Right migratedCfg -> runMigrations migratedCfg (fromVersion + 1)

-- | Migrate config to a specific version.
migrateTo :: Int -> Config -> IO (Either Text Config)
migrateTo targetVersion cfg = case targetVersion of
  1 -> do
    -- Initial version - no migration needed
    -- This case handles configs without a version field (pre-versioning)
    hPutStrLn stderr "[CONFIG] Migrating to version 1 (adding version field)"
    pure $ Right $ cfg { configVersion = 1 }

  -- Future migrations go here:
  -- 2 -> migrate1to2 cfg
  -- 3 -> migrate2to3 cfg

  _ -> pure $ Left $ "Unknown migration target version: " <> show targetVersion

{- Example future migration:

migrate1to2 :: Config -> IO (Either Text Config)
migrate1to2 cfg = do
  hPutStrLn stderr "[CONFIG] Migrating from version 1 to 2"
  -- Perform migration logic here
  -- e.g., add new fields with defaults, transform existing fields, etc.
  pure $ Right $ cfg
    { configVersion = 2
    -- , newField = defaultValue
    }
-}
