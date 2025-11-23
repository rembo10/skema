{-# LANGUAGE OverloadedStrings #-}

-- | Config JSON handling for API.
--
-- This module provides:
-- - Config to API JSON conversion (with computed fields like auth_enabled)
-- - JSON merge-based partial updates (no need for ConfigUpdate type)
--
-- This eliminates the need for:
-- - ConfigResponse (35 manually duplicated fields)
-- - ConfigUpdate (35 manually Maybe-wrapped fields)
-- - configToResponse (35 manual field mappings)
-- - HKD.hs (275 lines duplicating config types)
module Skema.Domain.ConfigJSON
  ( -- * API JSON conversion
    configToAPIJSON
    -- * JSON merge updates
  , applyConfigJSONUpdate
  ) where

import Data.Aeson (Value(..), ToJSON(..), encode, eitherDecode)
import qualified Data.Aeson.KeyMap as KM
import qualified Skema.Config.Types as Cfg
import qualified Skema.Config.Validation as CfgVal
import qualified System.OsPath as OP

-- | Convert Config to API JSON with computed fields.
--
-- Adds:
-- - auth_enabled: Whether authentication is enabled (computed from username/password)
-- - Converts OsPath fields to Text for JSON serialization
--
-- This replaces ConfigResponse and configToResponse entirely.
configToAPIJSON :: Cfg.Config -> IO Value
configToAPIJSON cfg = do
  -- Convert library path from OsPath to Text
  libPathText <- case Cfg.libraryPath (Cfg.library cfg) of
    Nothing -> pure Nothing
    Just osPath -> Just . toText <$> OP.decodeUtf osPath

  -- Check if auth is enabled (with environment variable overrides)
  maybeAuthCreds <- CfgVal.getAuthCredentials (Cfg.server cfg)
  let authEnabled = isJust maybeAuthCreds

  -- Get effective username (with env var override if present)
  let effectiveUsername = case maybeAuthCreds of
        Just (username, _) -> Just username
        Nothing -> Cfg.serverUsername (Cfg.server cfg)

  -- Start with base config JSON
  let baseJSON = toJSON cfg

  -- Add computed fields and fix library path
  case baseJSON of
    Object obj -> do
      -- Add computed server fields
      let serverObj = case KM.lookup "server" obj of
            Just (Object srvObj) -> Object $ srvObj
              & KM.insert "auth_enabled" (toJSON authEnabled)
              & KM.insert "username" (toJSON effectiveUsername)
              -- Remove password from response (security)
              & KM.delete "password"
              & KM.delete "jwt_secret"
            Just other -> other
            Nothing -> Object KM.empty

      -- Fix library path (OsPath -> Text)
      let libraryObj = case KM.lookup "library" obj of
            Just (Object libObj) -> Object $
              KM.insert "path" (toJSON libPathText) libObj
            Just other -> other
            Nothing -> Object KM.empty

      pure $ Object $ obj
        & KM.insert "server" serverObj
        & KM.insert "library" libraryObj

    other -> pure other

-- | Apply a partial JSON update to Config using merge.
--
-- Accepts any JSON object with partial config fields.
-- Merges with current config, decodes, and validates.
--
-- This replaces ConfigUpdate and applyConfigUpdate entirely.
applyConfigJSONUpdate :: Cfg.Config
                      -> Value  -- ^ Partial update JSON
                      -> Maybe Text  -- ^ Pre-hashed password (if password update requested)
                      -> IO (Either Text Cfg.Config)
applyConfigJSONUpdate currentCfg updateValue maybeHashedPassword = do
  -- Get current config as JSON
  let currentJSON = toJSON currentCfg

  -- Handle password specially - use hashed version if provided
  let updateWithHashedPassword = case updateValue of
        Object updateObj -> case KM.lookup "server" updateObj of
          Just (Object srvObj) ->
            if KM.member "password" srvObj
              then
                -- Replace plaintext password with hashed version
                let newSrvObj = case maybeHashedPassword of
                      Just hashed -> KM.insert "password" (toJSON (Just hashed)) srvObj
                      Nothing -> srvObj  -- Keep as-is if no hash provided
                in Object $ KM.insert "server" (Object newSrvObj) updateObj
              else updateValue
          _ -> updateValue
        _ -> updateValue

  -- Deep merge: update values override current values
  let mergedJSON = deepMerge currentJSON updateWithHashedPassword

  -- Decode back to Config
  let decoded = eitherDecode (encode mergedJSON) :: Either String Cfg.Config
  case decoded of
    Left err -> pure $ Left $ "Failed to parse updated config: " <> toText err
    Right updatedCfg -> do
      -- Validate the updated config
      case CfgVal.validateConfig updatedCfg of
        Just err -> pure $ Left err
        Nothing -> pure $ Right updatedCfg

-- | Deep merge two JSON Values.
--
-- For objects: recursively merge, with right side taking precedence.
-- For other types: right side takes precedence.
deepMerge :: Value -> Value -> Value
deepMerge (Object base) (Object update) =
  Object $ KM.unionWith deepMerge base update
deepMerge _ update = update
