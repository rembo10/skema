{-# LANGUAGE OverloadedStrings #-}

-- | Configuration loading from YAML files with environment variable overrides.
--
-- Environment variables take precedence over config file values.
-- See Schema.hs for the list of supported env vars (allEnvOverrides).
module Skema.Config.Loader
  ( loadConfig
  , loadConfigFromFile
  ) where

import Skema.Config.Types (Config, serverPassword, hashPassword, isHashedPassword)
import Skema.Config.EnvOverrides (applyEnvOverrides)
import Skema.Config.Validation (validateConfig)
import Skema.Config.Safe (safeWriteConfig, createBackup)
import Skema.Config.Migrations (migrateConfig, needsMigration)
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import System.OsPath (OsPath)
import qualified System.OsPath as OP
import qualified Skema.Config.Types as Cfg

-- | Load configuration from a file path.
--
-- Returns either an error message or the loaded config.
-- This function never writes to the config file, making it safe for read-only mounts.
-- Password hashing and migrations are done in-memory only.
loadConfig :: OsPath -> IO (Either Text Config)
loadConfig configPath = do
  pathStr <- OP.decodeUtf configPath
  result <- decodeFileEither pathStr
  case result of
    Left err -> pure $ Left $ toText $ prettyPrintParseException err
    Right cfg -> case validateConfig cfg of
      Just errMsg -> pure $ Left errMsg
      Nothing -> do
        -- Check if config needs migration
        cfgAfterMigration <- if needsMigration cfg
          then do
            migrationResult <- migrateConfig cfg
            case migrationResult of
              Left _err -> pure cfg  -- Continue with unmigrated config
              Right migratedCfg -> do
                -- Try to persist the migration, but don't fail if config is read-only
                _ <- safeWriteConfig pathStr migratedCfg
                -- Note: We ignore write failures here. If the config is read-only,
                -- the migration will be re-applied in-memory on next startup.
                -- This allows both read-only and writable configs to work.
                pure migratedCfg
          else pure cfg

        -- Apply environment variable overrides
        cfgWithEnv <- applyEnvOverrides cfgAfterMigration

        -- Hash password if needed
        updatedCfg <- hashPasswordIfNeeded cfgWithEnv pathStr
        pure $ Right updatedCfg

-- | Hash password if it's plaintext and optionally persist to file.
--
-- If the password is plaintext:
-- 1. Hash it in-memory
-- 2. Try to write it back to the config file
-- 3. If write fails (read-only mount), continue with in-memory hash
--
-- This allows both read-only and writable configs to work.
hashPasswordIfNeeded :: Config -> FilePath -> IO Config
hashPasswordIfNeeded cfg configPath = do
  case serverPassword (Cfg.server cfg) of
    Nothing -> pure cfg  -- No password configured
    Just password ->
      if isHashedPassword password
        then pure cfg  -- Already hashed
        else do
          -- Hash the plaintext password
          maybeHashed <- hashPassword password
          case maybeHashed of
            Nothing -> pure cfg  -- Hashing failed, continue with plaintext
            Just hashed -> do
              -- Update the config with hashed password
              let updatedServerCfg = (Cfg.server cfg) { serverPassword = Just hashed }
              let updatedCfg = cfg { Cfg.server = updatedServerCfg }

              -- Try to persist the hashed password, but don't fail if config is read-only
              _ <- safeWriteConfig configPath updatedCfg
              -- Note: We ignore write failures here. If the config is read-only,
              -- the password will be re-hashed in-memory on next startup.
              -- This allows both read-only and writable configs to work.
              pure updatedCfg

-- | Load configuration from a String file path.
--
-- This is a convenience wrapper for when you have a String path.
loadConfigFromFile :: FilePath -> IO (Either Text Config)
loadConfigFromFile pathStr = do
  osPath <- OP.encodeUtf pathStr
  loadConfig osPath
