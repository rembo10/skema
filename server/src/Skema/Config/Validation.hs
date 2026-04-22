{-# LANGUAGE OverloadedStrings #-}

-- | Configuration helper functions for auth/server access.
--
-- Note: Environment variable overrides are applied in Loader.hs.
-- These helpers just read from the already-merged config.
--
-- Validation logic lives in "Skema.Domain.ConfigJSON".
module Skema.Config.Validation
  ( getAuthCredentials
  , getServerPort
  , getServerHost
  ) where

import Skema.Config.Types (ServerConfig(..))

-- | Get authentication credentials from config.
--
-- Returns (username, password) or Nothing if not configured.
-- Note: Env vars (SKEMA_USERNAME, SKEMA_PASSWORD) are applied in Loader.hs
getAuthCredentials :: ServerConfig -> IO (Maybe (Text, Text))
getAuthCredentials cfg =
  pure $ case (serverUsername cfg, serverPassword cfg) of
    (Just u, Just p) -> Just (u, p)
    _ -> Nothing

-- | Get server port with optional CLI override.
--
-- Priority: CLI option > config value (env vars already applied in Loader)
getServerPort :: Maybe Int -> ServerConfig -> IO Int
getServerPort cliPort cfg =
  pure $ fromMaybe (serverPort cfg) cliPort

-- | Get server host from config.
--
-- Note: SKEMA_HOST env var is applied in Loader.hs
getServerHost :: ServerConfig -> IO Text
getServerHost cfg = pure $ serverHost cfg
