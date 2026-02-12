{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Generic-based environment variable overrides for config.
--
-- Derives env var names automatically from field names:
--   libraryPath -> SKEMA_LIBRARY_PATH
--   serverPort  -> SKEMA_SERVER_PORT (also accepts SKEMA_PORT)
--   systemDataDir -> SKEMA_SYSTEM_DATA_DIR (also accepts SKEMA_DATA_DIR)
--
-- For server.* and system.* config sections, both the full form and short
-- form (without section prefix) are accepted. The full form takes precedence.
module Skema.Config.EnvOverrides
  ( EnvParseable(..)
  , loadEnvOverrides
  , applyEnvOverrides
  , fieldToEnvVar
  , fieldToEnvVars
  , lookupEnvField
  ) where

import GHC.Generics
import qualified Data.Text as T
import Data.Char (isUpper, toUpper, toLower)
import Data.List (stripPrefix)
import System.IO.Unsafe (unsafePerformIO)
import qualified Skema.Config.Types as Cfg
import Skema.FileSystem.Utils (stringToOsPath)
import System.OsPath (OsPath)

-- =============================================================================
-- Type class for parsing env var values
-- =============================================================================

class EnvParseable a where
  parseEnvValue :: String -> Maybe a

instance EnvParseable Text where
  parseEnvValue = Just . toText

instance EnvParseable Int where
  parseEnvValue = readMaybe

instance EnvParseable Bool where
  parseEnvValue s = case T.toLower (toText s) of
    "true"  -> Just True
    "1"     -> Just True
    "yes"   -> Just True
    "false" -> Just False
    "0"     -> Just False
    "no"    -> Just False
    _       -> Nothing

instance EnvParseable OsPath where
  parseEnvValue s = unsafePerformIO $ Just <$> stringToOsPath s

instance {-# OVERLAPPABLE #-} EnvParseable a => EnvParseable (Maybe a) where
  parseEnvValue s = Just (parseEnvValue s)

instance EnvParseable Cfg.MusicBrainzServer where
  parseEnvValue s = Just $ case T.toLower (toText s) of
    "headphones_vip" -> Cfg.HeadphonesVIP
    "headphones"     -> Cfg.HeadphonesVIP
    "vip"            -> Cfg.HeadphonesVIP
    _                -> Cfg.OfficialMusicBrainz

-- Instances for complex types that shouldn't be loaded from env
instance EnvParseable (Maybe Cfg.DownloadClient) where
  parseEnvValue _ = Nothing  -- Can't parse complex types from env

instance EnvParseable [Cfg.Indexer] where
  parseEnvValue _ = Nothing

instance EnvParseable [Text] where
  parseEnvValue _ = Nothing

instance EnvParseable [Cfg.NotificationProvider] where
  parseEnvValue _ = Nothing

instance EnvParseable (Maybe Cfg.SlskdConfig) where
  parseEnvValue _ = Nothing  -- SlskdConfig is too complex for env vars

-- =============================================================================
-- Field name to env var conversion
-- =============================================================================

-- | Convert camelCase field name to SKEMA_UPPER_SNAKE env var
-- e.g., "libraryPath" -> "SKEMA_LIBRARY_PATH"
fieldToEnvVar :: String -> String
fieldToEnvVar fieldName = "SKEMA_" ++ toUpperSnake fieldName

-- | Get all valid env var names for a field, in priority order.
-- For server.* and system.* fields, also accepts the short form.
-- e.g., "serverPort" -> ["SKEMA_SERVER_PORT", "SKEMA_PORT"]
--       "systemDataDir" -> ["SKEMA_SYSTEM_DATA_DIR", "SKEMA_DATA_DIR"]
--       "libraryPath" -> ["SKEMA_LIBRARY_PATH"]
fieldToEnvVars :: String -> [String]
fieldToEnvVars fieldName =
  let fullName = fieldToEnvVar fieldName
      -- Try to strip server/system prefix for short form
      shortForm = case stripPrefix "server" fieldName of
        Just (c:cs) | isUpper c -> Just $ "SKEMA_" ++ toUpperSnake (toLower c : cs)
        _ -> case stripPrefix "system" fieldName of
          Just (c:cs) | isUpper c -> Just $ "SKEMA_" ++ toUpperSnake (toLower c : cs)
          _ -> Nothing
  in fullName : maybe [] pure shortForm

-- | Look up an env var for a field, checking all valid names in priority order.
lookupEnvField :: String -> IO (Maybe String)
lookupEnvField fieldName = firstJustM lookupEnv (fieldToEnvVars fieldName)

-- Helper: find first Just result from a list of monadic lookups
firstJustM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstJustM _ [] = pure Nothing
firstJustM f (x:xs) = do
  result <- f x
  case result of
    Just v -> pure (Just v)
    Nothing -> firstJustM f xs

toUpperSnake :: String -> String
toUpperSnake = map toUpper . camelToSnake

camelToSnake :: String -> String
camelToSnake [] = []
camelToSnake (c:cs)
  | isUpper c = '_' : toUpper c : camelToSnake cs
  | otherwise = toUpper c : camelToSnake cs

-- =============================================================================
-- Generic env var loading
-- =============================================================================

class GEnvLoad f where
  gEnvLoad :: f p -> IO (f p)

instance GEnvLoad U1 where
  gEnvLoad U1 = pure U1

instance (GEnvLoad a, GEnvLoad b) => GEnvLoad (a :*: b) where
  gEnvLoad (a :*: b) = (:*:) <$> gEnvLoad a <*> gEnvLoad b

instance GEnvLoad a => GEnvLoad (M1 D c a) where
  gEnvLoad (M1 a) = M1 <$> gEnvLoad a

instance GEnvLoad a => GEnvLoad (M1 C c a) where
  gEnvLoad (M1 a) = M1 <$> gEnvLoad a

instance (Selector s, EnvParseable a) => GEnvLoad (M1 S s (K1 i a)) where
  gEnvLoad m@(M1 (K1 _)) = do
    let fieldName = selName m
    -- Check all valid env var names (full form first, then short form)
    maybeEnvVal <- lookupEnvField fieldName
    pure $ case maybeEnvVal >>= parseEnvValue of
      Nothing -> m
      Just newVal -> M1 (K1 newVal)

-- | Load env var overrides for a Generic config type
loadEnvOverrides :: (Generic a, GEnvLoad (Rep a)) => a -> IO a
loadEnvOverrides cfg = to <$> gEnvLoad (from cfg)

-- | Apply env var overrides to full Config by loading each section
applyEnvOverrides :: Cfg.Config -> IO Cfg.Config
applyEnvOverrides cfg = do
  lib <- loadEnvOverrides (Cfg.library cfg)
  sys <- loadEnvOverrides (Cfg.system cfg)
  srv <- loadEnvOverrides (Cfg.server cfg)
  dl  <- loadEnvOverrides (Cfg.download cfg)
  idx <- loadEnvOverrides (Cfg.indexers cfg)
  mb  <- loadEnvOverrides (Cfg.musicbrainz cfg)
  med <- loadEnvOverrides (Cfg.media cfg)
  notif <- loadEnvOverrides (Cfg.notifications cfg)
  pure cfg
    { Cfg.library = lib
    , Cfg.system = sys
    , Cfg.server = srv
    , Cfg.download = dl
    , Cfg.indexers = idx
    , Cfg.musicbrainz = mb
    , Cfg.media = med
    , Cfg.notifications = notif
    }
