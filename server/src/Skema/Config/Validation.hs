{-# LANGUAGE OverloadedStrings #-}

-- | Configuration validation and helper functions.
module Skema.Config.Validation
  ( -- * Validation
    validateConfig
    -- * Helpers
  , getAuthCredentials
  , getServerPort
  , getServerHost
  ) where

import Skema.Config.Types
  ( Config(..)
  , LibraryConfig(..)
  , ServerConfig(..)
  )

-- | Validate configuration.
--
-- Returns an error message if invalid, Nothing if valid.
validateConfig :: Config -> Maybe Text
validateConfig cfg
  | libraryAutoScanIntervalMins (library cfg) < 1 =
      Just "auto_scan_interval_mins must be at least 1"
  | otherwise = Nothing

-- | Get authentication credentials with environment variable overrides.
--
-- Checks SKEMA_USERNAME and SKEMA_PASSWORD environment variables first,
-- then falls back to config file values.
-- Returns (username, password) or Nothing if not configured.
getAuthCredentials :: ServerConfig -> IO (Maybe (Text, Text))
getAuthCredentials cfg = do
  envUsername <- lookupEnv "SKEMA_USERNAME"
  envPassword <- lookupEnv "SKEMA_PASSWORD"

  let username = case envUsername of
        Just u -> Just (toText u)
        Nothing -> serverUsername cfg

  let password = case envPassword of
        Just p -> Just (toText p)
        Nothing -> serverPassword cfg

  pure $ case (username, password) of
    (Just u, Just p) -> Just (u, p)
    _ -> Nothing

-- | Get server port with command line and environment variable overrides.
--
-- Priority order (highest to lowest):
-- 1. Command line --port/-p option
-- 2. SKEMA_PORT environment variable
-- 3. Config file value
getServerPort :: Maybe Int -> ServerConfig -> IO Int
getServerPort cliPort cfg = do
  case cliPort of
    Just p -> pure p  -- CLI override has highest priority
    Nothing -> do
      envPort <- lookupEnv "SKEMA_PORT"
      pure $ case envPort of
        Just p -> fromMaybe (serverPort cfg) (readMaybe p)
        Nothing -> serverPort cfg

-- | Get server host with environment variable override.
--
-- Priority order (highest to lowest):
-- 1. SKEMA_HOST environment variable
-- 2. Config file value
getServerHost :: ServerConfig -> IO Text
getServerHost cfg = do
  envHost <- lookupEnv "SKEMA_HOST"
  pure $ case envHost of
    Just h -> toText h
    Nothing -> serverHost cfg
