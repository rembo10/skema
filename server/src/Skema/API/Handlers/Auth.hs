{-# LANGUAGE OverloadedStrings #-}

-- | Authentication API handlers.
module Skema.API.Handlers.Auth
  ( authServer
  ) where

import Skema.API.Types.Auth (AuthAPI, AuthStatusResponse(..))
import Skema.API.Handlers.Utils (throw401, throw500, readConfig)
import Skema.Auth
import Skema.Auth.JWT (JWTSecret, generateJWT)
import qualified Skema.Config.Types as Cfg
import Servant

-- | Auth API handlers.
authServer :: AuthStore -> JWTSecret -> TVar Cfg.Config -> Server AuthAPI
authServer _authStore jwtSecret configVar =
  statusHandler :<|> credentialsHandler
  where
    statusHandler :: Handler AuthStatusResponse
    statusHandler = do
      -- Read current config to check if auth is enabled
      currentCfg <- liftIO $ readConfig configVar
      let serverCfg = Cfg.server currentCfg
      authEnabled <- liftIO $ checkAuthEnabled serverCfg
      pure $ AuthStatusResponse { authStatusEnabled = authEnabled }

    credentialsHandler :: CredentialsRequest -> Handler AuthResponse
    credentialsHandler req = do
      -- Read current config from TVar to get latest credentials
      currentCfg <- liftIO $ readConfig configVar
      let serverCfg = Cfg.server currentCfg

      -- Authenticate credentials
      result <- liftIO $ authenticateCredentials serverCfg (credUsername req) (credPassword req)
      case result of
        Left InvalidCredentials -> throw401 "Invalid username or password"
        Left AuthenticationDisabled -> throw401 "Authentication is not configured"
        Left InvalidApiKey -> throw500 "Internal error during authentication"
        Right () -> do
          -- Generate JWT for the authenticated user
          let expHours = Cfg.serverJwtExpirationHours serverCfg
          jwtResult <- liftIO $ generateJWT jwtSecret (credUsername req) expHours
          case jwtResult of
            Left _jwtErr -> throw500 "Failed to generate JWT"
            Right (jwt, expiresAt) -> pure $ AuthResponse
              { authJwt = jwt
              , authExpiresAt = expiresAt
              , authMessage = "Authentication successful"
              }
