{-# LANGUAGE OverloadedStrings #-}

-- | Config API handlers.
--
-- Uses JSON merge-based updates instead of typed ConfigUpdate.
-- This eliminates the need for ConfigResponse/ConfigUpdate types.
module Skema.API.Handlers.Config
  ( configServer
  , configSchemaServer
  ) where

import Skema.API.Types.Config (ConfigAPI, ConfigSchemaAPI)
import Skema.API.Handlers.Utils (throw400, throw500, readConfig)
import Skema.Auth (requireAuth)
import Skema.Auth.JWT (JWTSecret, generateApiKey)
import Skema.Database.Connection
import Skema.Config.Discovery (saveConfigToFile)
import Skema.Config.Schema (schemaToJSON, allSchemas)
import qualified Skema.Config.Types as Cfg
import Skema.Domain.ConfigJSON (configToAPIJSON, applyConfigJSONUpdate)
import Skema.Events.Bus (EventBus)
import qualified Skema.Events.Bus as EventBus
import qualified Skema.Events.Types as Events
import qualified Skema.Adapters.Crypto as Crypto
import Servant
import Katip
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KM

-- | Config API handlers.
configServer :: LogEnv -> EventBus -> Cfg.ServerConfig -> JWTSecret -> ConnectionPool -> TVar Cfg.Config -> FilePath -> Server ConfigAPI
configServer le bus _serverCfg jwtSecret _connPool configVar configPath =
  authProtectedHandlers
  :<|> generateApiKeyHandler
  :<|> revokeApiKeyHandler
  where
    authProtectedHandlers maybeAuthHeader =
      getConfigHandler maybeAuthHeader
      :<|> updateConfigHandler maybeAuthHeader

    getConfigHandler authHeader = do
      _ <- requireAuth configVar jwtSecret authHeader
      cfg <- liftIO $ readConfig configVar
      liftIO $ configToAPIJSON cfg

    updateConfigHandler authHeader updateValue = do
      _ <- requireAuth configVar jwtSecret authHeader
      currentCfg <- liftIO $ readConfig configVar

      -- Extract password from update if present, and hash it
      maybeHashedPassword <- case extractPassword updateValue of
        Just plaintext -> liftIO $ Crypto.hashPasswordIfNeeded plaintext
        Nothing -> pure Nothing

      -- Apply JSON merge-based update
      result <- liftIO $ applyConfigJSONUpdate currentCfg updateValue maybeHashedPassword
      case result of
        Left err -> throw400 $ "Invalid configuration: " <> err
        Right updatedCfg -> do
          -- Save to file (Registry will reload it via ConfigUpdated event)
          saveResult <- liftIO $ saveConfigToFile configPath updatedCfg
          case saveResult of
            Left err -> throw500 $ "Failed to save configuration: " <> err
            Right () -> do
              -- Convert config to API JSON for response and SSE event
              configJSON <- liftIO $ configToAPIJSON updatedCfg

              -- Emit CONFIG_UPDATED event
              liftIO $ EventBus.publishAndLog bus le "api" $ Events.ConfigUpdated configJSON

              -- Return updated config
              pure configJSON

    generateApiKeyHandler = do
      currentCfg <- liftIO $ readConfig configVar
      apiKey <- liftIO generateApiKey
      let serverCfg = Cfg.server currentCfg
          updatedCfg = currentCfg { Cfg.server = serverCfg { Cfg.serverApiKey = Just apiKey } }
      saveResult <- liftIO $ saveConfigToFile configPath updatedCfg
      case saveResult of
        Left err -> throw500 $ "Failed to save configuration: " <> err
        Right () -> do
          liftIO $ atomically $ writeTVar configVar updatedCfg
          configJSON <- liftIO $ configToAPIJSON updatedCfg
          liftIO $ EventBus.publishAndLog bus le "api" $ Events.ConfigUpdated configJSON
          pure $ object ["api_key" .= apiKey]

    revokeApiKeyHandler = do
      currentCfg <- liftIO $ readConfig configVar
      let serverCfg = Cfg.server currentCfg
          updatedCfg = currentCfg { Cfg.server = serverCfg { Cfg.serverApiKey = Nothing } }
      saveResult <- liftIO $ saveConfigToFile configPath updatedCfg
      case saveResult of
        Left err -> throw500 $ "Failed to save configuration: " <> err
        Right () -> do
          liftIO $ atomically $ writeTVar configVar updatedCfg
          configJSON <- liftIO $ configToAPIJSON updatedCfg
          liftIO $ EventBus.publishAndLog bus le "api" $ Events.ConfigUpdated configJSON
          pure NoContent

-- | Extract password from update JSON if present.
extractPassword :: Value -> Maybe Text
extractPassword (Object obj) = case KM.lookup "server" obj of
  Just (Object srvObj) -> case KM.lookup "password" srvObj of
    Just (String pwd) -> Just pwd
    _ -> Nothing
  _ -> Nothing
extractPassword _ = Nothing

-- | Config schema handler for UI generation.
--
-- GET /config/schema - Returns field metadata for dynamic UI
configSchemaServer :: Server ConfigSchemaAPI
configSchemaServer = pure $ schemaToJSON allSchemas
