{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Generic-based environment variable overrides for config.
--
-- Derives env var names automatically from field names:
--   libraryPath -> SKEMA_LIBRARY_PATH
--   serverPort  -> SKEMA_SERVER_PORT
module Skema.Config.EnvOverrides
  ( EnvParseable(..)
  , loadEnvOverrides
  , applyEnvOverrides
  , fieldToEnvVar
  ) where

import GHC.Generics
import qualified Data.Text as T
import Data.Char (isUpper, toUpper)
import System.IO.Unsafe (unsafePerformIO)
import qualified Skema.Config.Types as Cfg
import System.OsPath (OsPath)
import qualified System.OsPath as OP

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
  parseEnvValue s = unsafePerformIO $ Just <$> OP.encodeUtf s

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

-- =============================================================================
-- Field name to env var conversion
-- =============================================================================

-- | Convert camelCase field name to SKEMA_UPPER_SNAKE env var
-- e.g., "libraryPath" -> "SKEMA_LIBRARY_PATH"
fieldToEnvVar :: String -> String
fieldToEnvVar fieldName = "SKEMA_" ++ toUpperSnake fieldName
  where
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
        envVarName = fieldToEnvVar fieldName
    maybeEnvVal <- lookupEnv envVarName
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
