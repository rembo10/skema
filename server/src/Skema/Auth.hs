{-# LANGUAGE OverloadedStrings #-}

-- | Authentication and authorization for Skema API.
--
-- Request authentication is enforced centrally by the default-deny
-- middleware in Skema.API.Server; this module provides the credential
-- checks it and the login endpoint build on.
module Skema.Auth
  ( -- * Re-exports from Types
    module Skema.Auth.Types
    -- * Authentication
  , authenticateCredentials
  , checkAuthEnabled
  ) where

import Skema.Auth.Types
import Skema.Config.Types (ServerConfig, verifyPassword)
import Skema.Config.Validation (getAuthCredentials)

-- | Check if authentication is enabled.
--
-- Authentication is enabled if credentials are configured
-- (either in config file or via environment variables).
checkAuthEnabled :: ServerConfig -> IO Bool
checkAuthEnabled cfg = do
  creds <- getAuthCredentials cfg
  pure $ isJust creds

-- | Authenticate user credentials.
--
-- Returns AuthError if credentials are invalid or auth is disabled.
-- Uses bcrypt to verify passwords.
-- Note: JWT generation is handled separately in the API handler.
authenticateCredentials
  :: ServerConfig
  -> Text  -- ^ Username
  -> Text  -- ^ Password
  -> IO (Either AuthError ())
authenticateCredentials cfg username password = do
  maybeCreds <- getAuthCredentials cfg

  case maybeCreds of
    Nothing -> pure $ Left AuthenticationDisabled

    Just (validUsername, passwordHash) ->
      -- Check username and verify password with bcrypt
      if username == validUsername && verifyPassword password passwordHash
        then pure $ Right ()
        else pure $ Left InvalidCredentials

