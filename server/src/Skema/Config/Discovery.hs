{-# LANGUAGE OverloadedStrings #-}

-- | Configuration file discovery.
--
-- This module handles finding config files in standard locations and
-- creating default configs when none exist.
module Skema.Config.Discovery
  ( findConfigFile
  , getOrCreateConfig
  , ensureConfigDirectory
  , saveConfigToFile
  ) where

import Skema.Config.Types
import Skema.Config.Loader
import Skema.Config.Safe (safeWriteConfig)
import Skema.Auth.JWT (generateJWTSecretString)
import System.Directory (doesFileExist, createDirectoryIfMissing, getXdgDirectory, XdgDirectory(..))
import System.FilePath (takeDirectory)

-- | Find the config file in standard locations.
--
-- Searches in this order:
-- 1. Explicitly provided path (if Just) - returns the path even if file doesn't exist
-- 2. $XDG_CONFIG_HOME/skema/config.yaml
-- 3. /etc/skema/config.yaml
--
-- Returns Nothing if no config file is found in standard locations.
findConfigFile :: Maybe FilePath -> IO (Maybe FilePath)
findConfigFile (Just explicitPath) = do
  -- If user explicitly provided a path, use it (even if it doesn't exist yet)
  -- This allows creating a config at a specific location
  pure $ Just explicitPath

findConfigFile Nothing = do
  -- Try XDG config directory
  xdgConfigHome <- getXdgDirectory XdgConfig "skema"
  let xdgConfigPath = xdgConfigHome <> "/config.yaml"
  xdgExists <- doesFileExist xdgConfigPath

  if xdgExists
    then pure $ Just xdgConfigPath
    else do
      -- Try system-wide config
      let systemConfigPath = "/etc/skema/config.yaml"
      systemExists <- doesFileExist systemConfigPath
      pure $ if systemExists then Just systemConfigPath else Nothing

-- | Get configuration from file or create default.
--
-- This function:
-- 1. Searches for config in standard locations
-- 2. If found, loads and validates it
-- 3. If not found, creates a default config (at explicit path if provided, otherwise XDG)
-- 4. Ensures JWT secret is generated and persisted
-- 5. Returns the loaded or created config along with its path
--
-- Takes an optional port override to use when creating a new config file.
getOrCreateConfig :: Maybe FilePath -> Maybe Int -> IO (Either Text (FilePath, Config))
getOrCreateConfig explicitPath maybePort = do
  maybeConfigPath <- findConfigFile explicitPath

  case maybeConfigPath of
    Just configPath -> do
      -- Check if config file exists
      exists <- doesFileExist configPath
      if exists
        then do
          -- Config exists, load it
          result <- loadConfigFromFile configPath
          case result of
            Left err -> pure $ Left err
            Right config -> do
              -- Apply environment variable overrides
              configWithEnv <- applyPortToConfig maybePort config
              -- Ensure JWT secret exists
              resultWithSecret <- ensureJWTSecret configPath configWithEnv
              pure $ fmap (\cfg -> (configPath, cfg)) resultWithSecret
        else do
          -- Config path specified but doesn't exist, create it there
          -- Extract directory from configPath
          let configDir = takeDirectory configPath
          createDirectoryIfMissing True configDir

          -- Write default config with resolved port and directories
          configToWrite <- applyPortToConfig maybePort defaultConfig
          writeResult <- safeWriteConfig configPath configToWrite
          case writeResult of
            Left err -> pure $ Left $ "Failed to create config: " <> err
            Right () -> do
              -- Ensure JWT secret exists in the newly created config
              resultWithSecret <- ensureJWTSecret configPath configToWrite
              pure $ fmap (\cfg -> (configPath, cfg)) resultWithSecret

    Nothing -> do
      -- No config found and no explicit path, create default in XDG
      xdgConfigDir <- getXdgDirectory XdgConfig "skema"
      let configPath = xdgConfigDir <> "/config.yaml"

      -- Ensure directory exists
      createDirectoryIfMissing True xdgConfigDir

      -- Write default config with resolved port and directories
      configToWrite <- applyPortToConfig maybePort defaultConfig
      writeResult <- safeWriteConfig configPath configToWrite
      case writeResult of
        Left err -> pure $ Left $ "Failed to create config: " <> err
        Right () -> do
          -- Ensure JWT secret exists in the newly created config
          resultWithSecret <- ensureJWTSecret configPath configToWrite
          pure $ fmap (\cfg -> (configPath, cfg)) resultWithSecret

-- | Apply port and directory overrides to config if provided.
--
-- This is used when creating a new config file to ensure CLI args and
-- environment variables are persisted to the config.
applyPortToConfig :: Maybe Int -> Config -> IO Config
applyPortToConfig maybePort config = do
  -- Check for environment variables
  envDataDir <- lookupEnv "SKEMA_DATA_DIR"
  envCacheDir <- lookupEnv "SKEMA_CACHE_DIR"
  envHost <- lookupEnv "SKEMA_HOST"
  envPort <- lookupEnv "SKEMA_PORT"

  -- Apply port override (CLI arg takes precedence over env var)
  let srvConfig = server config
  let portToUse = maybePort <|> (envPort >>= readMaybe)
  let updatedServerConfig = srvConfig
        { serverPort = fromMaybe (serverPort srvConfig) portToUse
        , serverHost = maybe (serverHost srvConfig) toText envHost
        }

  -- Apply directory overrides
  let sysConfig = system config
  let updatedSystemConfig = sysConfig
        { systemDataDir = fmap toText envDataDir `orElse` systemDataDir sysConfig
        , systemCacheDir = fmap toText envCacheDir `orElse` systemCacheDir sysConfig
        }

  pure $ config
    { server = updatedServerConfig
    , system = updatedSystemConfig
    }
  where
    orElse :: Maybe a -> Maybe a -> Maybe a
    orElse (Just x) _ = Just x
    orElse Nothing y = y

-- | Ensure JWT secret exists in config, checking environment variable if needed.
--
-- Priority order:
-- 1. JWT secret in config file
-- 2. SKEMA_JWT_SECRET environment variable
-- 3. Auto-generate a new secret and try to persist it
--
-- If a secret needs to be generated:
-- - Try to write it back to the config file
-- - If write fails (read-only mount), continue with in-memory secret
-- - Next startup will re-generate if config still has no secret
ensureJWTSecret :: FilePath -> Config -> IO (Either Text Config)
ensureJWTSecret configPath config = do
  let srvConfig = server config

  case serverJwtSecret srvConfig of
    Just _ -> do
      -- JWT secret already exists in config
      pure $ Right config
    Nothing -> do
      -- Check for environment variable
      envSecret <- lookupEnv "SKEMA_JWT_SECRET"
      case envSecret of
        Just secret | not (null secret) -> do
          -- Use secret from environment
          let updatedServerConfig = srvConfig { serverJwtSecret = Just (toText secret) }
          let updatedConfig = config { server = updatedServerConfig }
          pure $ Right updatedConfig
        _ -> do
          -- No secret in config or environment - generate one
          newSecret <- generateJWTSecretString
          let updatedServerConfig = srvConfig { serverJwtSecret = Just newSecret }
          let updatedConfig = config { server = updatedServerConfig }

          -- Try to persist the secret, but don't fail if config is read-only
          _ <- safeWriteConfig configPath updatedConfig
          -- Note: We ignore write failures here. If the config is read-only,
          -- the secret will be re-generated in-memory on next startup.
          -- Production deployments should use SKEMA_JWT_SECRET environment variable.
          pure $ Right updatedConfig

-- | Ensure the config directory exists.
--
-- Creates the XDG config directory if it doesn't exist.
ensureConfigDirectory :: IO FilePath
ensureConfigDirectory = do
  xdgConfigDir <- getXdgDirectory XdgConfig "skema"
  createDirectoryIfMissing True xdgConfigDir
  pure xdgConfigDir

-- | Save configuration to a YAML file with atomic write and backup.
--
-- Safely writes config with automatic backup and validation.
saveConfigToFile :: FilePath -> Config -> IO (Either Text ())
saveConfigToFile = safeWriteConfig
