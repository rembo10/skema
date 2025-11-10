{-# LANGUAGE OverloadedStrings #-}

-- | Authentication and authorization for Skema API.
module Skema.Auth
  ( -- * Re-exports from Types
    module Skema.Auth.Types
    -- * Re-exports from Store
  , module Skema.Auth.Store
    -- * Authentication
  , authenticateCredentials
  , checkAuthEnabled
  , requireAuth
  ) where

import Skema.Auth.Types
import Skema.Auth.Store
import Skema.Auth.JWT (JWTSecret, JWTClaims(..), validateJWT)
import Skema.Config.Types (Config, ServerConfig, verifyPassword, server)
import Skema.Config.Validation (getAuthCredentials)
import Servant (Handler, throwError, err401, errBody)
import Data.Text (stripPrefix)
import qualified Control.Concurrent.STM as STM

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

-- | Require JWT authentication for an endpoint.
--
-- Checks if auth is enabled (by reading the current config from TVar), and if so,
-- validates the JWT from the Authorization header.
-- If auth is disabled, allows the request through.
-- If auth is enabled but no valid JWT is provided, returns 401 Unauthorized.
--
-- Returns the username from the JWT claims if auth is enabled and valid.
requireAuth
  :: STM.TVar Config
  -> JWTSecret
  -> Maybe Text  -- ^ Authorization header value (e.g., "Bearer <jwt>")
  -> Handler (Maybe Text)  -- ^ Username if authenticated, Nothing if auth disabled
requireAuth configVar jwtSecret maybeAuthHeader = do
  -- Read current config to get latest auth settings
  currentCfg <- liftIO $ STM.atomically $ STM.readTVar configVar
  let cfg = server currentCfg

  authEnabled <- liftIO $ checkAuthEnabled cfg

  -- If auth is not enabled, allow the request
  if not authEnabled
    then pure Nothing
    else do
      -- If auth is enabled, validate the JWT
      case maybeAuthHeader of
        Nothing -> throwError err401 { errBody = "Authorization header required" }
        Just authHeader ->
          case stripPrefix "Bearer " authHeader of
            Nothing -> throwError err401 { errBody = "Invalid authorization header format. Expected: Bearer <jwt>" }
            Just token -> do
              result <- liftIO $ validateJWT jwtSecret token
              case result of
                Left _err -> throwError err401 { errBody = "Invalid or expired JWT" }
                Right claims -> pure $ Just (jwtUsername claims)
