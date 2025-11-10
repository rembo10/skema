{-# LANGUAGE OverloadedStrings #-}

-- | Config API handlers.
module Skema.API.Handlers.Config
  ( configServer
  ) where

import Skema.API.Types.Config (ConfigAPI, ConfigUpdate(..))
import Skema.API.Handlers.Auth (throwJsonError)
import Skema.Auth (requireAuth)
import Skema.Auth.JWT (JWTSecret)
import Skema.Database.Connection
import Skema.Config.Discovery (saveConfigToFile)
import qualified Skema.Config.Types as Cfg
import Skema.Domain.Converters (configToResponse)
import Skema.Events.Bus (EventBus)
import qualified Skema.Events.Bus as EventBus
import qualified Skema.Events.Types as Events
import qualified Skema.Domain.Config as DomainConfig
import qualified Skema.Adapters.Crypto as Crypto
import Servant
import Katip
import Data.Aeson (toJSON)
import qualified Control.Concurrent.STM as STM

-- | Throw a 400 Bad Request error.
throw400 :: Text -> Handler a
throw400 = throwJsonError err400

-- | Throw a 500 Internal Server Error.
throw500 :: Text -> Handler a
throw500 = throwJsonError err500

-- | Config API handlers.
configServer :: LogEnv -> EventBus -> Cfg.ServerConfig -> JWTSecret -> ConnectionPool -> TVar Cfg.Config -> FilePath -> Server ConfigAPI
configServer le bus _serverCfg jwtSecret _connPool configVar configPath = \maybeAuthHeader ->
  getConfigHandler maybeAuthHeader
  :<|> updateConfigHandler maybeAuthHeader
  where
    getConfigHandler authHeader = do
      _ <- requireAuth configVar jwtSecret authHeader
      cfg <- liftIO $ STM.atomically $ STM.readTVar configVar
      liftIO $ configToResponse cfg

    updateConfigHandler authHeader update = do
      _ <- requireAuth configVar jwtSecret authHeader
      currentCfg <- liftIO $ STM.atomically $ STM.readTVar configVar

      -- Hash password if needed (IO adapter)
      maybeHashedPassword <- case updateServerPassword update of
        Just (Just plaintext) -> liftIO $ Crypto.hashPasswordIfNeeded plaintext
        _ -> pure Nothing

      -- Apply the updates (uses pure domain logic)
      result <- liftIO $ DomainConfig.applyConfigUpdate currentCfg update maybeHashedPassword
      case result of
        Left err -> throw400 $ "Invalid configuration: " <> err
        Right updatedCfg -> do
          -- Save to file (Registry will reload it via ConfigUpdated event)
          saveResult <- liftIO $ saveConfigToFile configPath updatedCfg
          case saveResult of
            Left err -> throw500 $ "Failed to save configuration: " <> err
            Right () -> do
              -- Convert config to response for both API return and SSE event
              configResp <- liftIO $ configToResponse updatedCfg

              -- Emit CONFIG_UPDATED event with the updated config
              -- (Registry will reload from file and update TVar)
              liftIO $ EventBus.publishAndLog bus le "api" $ Events.ConfigUpdated (toJSON configResp)

              -- Return updated config
              pure configResp
