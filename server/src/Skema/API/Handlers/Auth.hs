{-# LANGUAGE OverloadedStrings #-}

-- | Authentication API handlers.
module Skema.API.Handlers.Auth
  ( authServer
  , throwJsonError
  ) where

import Skema.API.Types.Common (mkErrorResponse)
import Skema.API.Types.Auth (AuthAPI, AuthStatusResponse(..))
import Skema.Auth
import Skema.Auth.JWT (JWTSecret, generateJWT)
import qualified Skema.Config.Types as Cfg
import Servant
import qualified Control.Concurrent.STM as STM
import Data.Aeson (encode)

-- | Helper to throw a JSON error with a specific status code.
throwJsonError :: ServerError -> Text -> Handler a
throwJsonError statusErr msg = throwError $ statusErr
  { errBody = encode $ mkErrorResponse msg
  , errHeaders = [("Content-Type", "application/json")]
  }

-- | Throw a 401 Unauthorized error.
throw401 :: Text -> Handler a
throw401 = throwJsonError err401

-- | Throw a 500 Internal Server Error.
throw500 :: Text -> Handler a
throw500 = throwJsonError err500

-- | Auth API handlers.
authServer :: AuthStore -> JWTSecret -> TVar Cfg.Config -> Server AuthAPI
authServer _authStore jwtSecret configVar =
  statusHandler :<|> credentialsHandler
  where
    statusHandler :: Handler AuthStatusResponse
    statusHandler = do
      -- Read current config to check if auth is enabled
      currentCfg <- liftIO $ STM.atomically $ STM.readTVar configVar
      let serverCfg = Cfg.server currentCfg
      authEnabled <- liftIO $ checkAuthEnabled serverCfg
      pure $ AuthStatusResponse { authStatusEnabled = authEnabled }

    credentialsHandler :: CredentialsRequest -> Handler AuthResponse
    credentialsHandler req = do
      -- Read current config from TVar to get latest credentials
      currentCfg <- liftIO $ STM.atomically $ STM.readTVar configVar
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
