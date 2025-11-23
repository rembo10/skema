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
-- If the password is plaintext, it will be hashed with bcrypt and saved back to the file.
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
            -- Create backup before migration
            _ <- createBackup pathStr

            migrationResult <- migrateConfig cfg
            case migrationResult of
              Left _err -> pure cfg  -- Continue with unmigrated config
              Right migratedCfg -> do
                -- Save migrated config
                _ <- safeWriteConfig pathStr migratedCfg
                pure migratedCfg
          else pure cfg

        -- Apply environment variable overrides
        cfgWithEnv <- applyEnvOverrides cfgAfterMigration

        -- Check if password needs to be hashed
        updatedCfg <- hashPasswordIfNeeded cfgWithEnv pathStr
        pure $ Right updatedCfg

-- | Hash password if it's plaintext and save the config back.
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

              -- Save the updated config back to the file safely
              writeResult <- safeWriteConfig configPath updatedCfg
              case writeResult of
                Left _err -> pure cfg  -- Return original config if save fails
                Right () -> pure updatedCfg

-- | Load configuration from a String file path.
--
-- This is a convenience wrapper for when you have a String path.
loadConfigFromFile :: FilePath -> IO (Either Text Config)
loadConfigFromFile pathStr = do
  osPath <- OP.encodeUtf pathStr
  loadConfig osPath
