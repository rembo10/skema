{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Version information for Skema.
--
-- Provides compile-time version and git commit info.
-- Git hash is embedded via Template Haskell (gitrev) when building from source.
-- For Docker/Nix builds where .git isn't available, the SKEMA_COMMIT env var
-- is checked at runtime as a fallback.
module Skema.Version
  ( version
  , gitCommit
  , channel
  , VersionInfo(..)
  , getVersionInfo
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.Version (showVersion)
import qualified Paths_skema as Meta
import Development.GitRev (gitHash)

-- | The package version string (from cabal file).
version :: Text
version = toText $ showVersion Meta.version

-- | The git commit hash, embedded at compile time.
-- Returns "UNKNOWN" when not building from a git checkout.
gitCommit :: Text
gitCommit = $(gitHash)

-- | Detect the distribution channel from the SKEMA_CHANNEL env var.
-- Falls back to "source" if not set.
channel :: IO Text
channel = do
  env <- lookupEnv "SKEMA_CHANNEL"
  pure $ maybe "source" toText env

-- | Complete version information.
data VersionInfo = VersionInfo
  { viVersion :: Text
  , viCommit :: Text
  , viChannel :: Text
  , viLatestVersion :: Maybe Text
  , viUpdateAvailable :: Bool
  } deriving (Show, Eq)

instance ToJSON VersionInfo where
  toJSON vi = object
    [ "version" .= viVersion vi
    , "commit" .= viCommit vi
    , "channel" .= viChannel vi
    , "latest_version" .= viLatestVersion vi
    , "update_available" .= viUpdateAvailable vi
    ]

-- | Build version info, optionally checking for the runtime commit override.
-- The latest version check is handled separately by the API handler.
getVersionInfo :: IO VersionInfo
getVersionInfo = do
  ch <- channel
  -- Check for runtime commit override (set by Docker/Nix builds)
  envCommit <- lookupEnv "SKEMA_COMMIT"
  let commit = case envCommit of
        Just c | not (null c) -> toText c
        _ -> gitCommit
  pure $ VersionInfo
    { viVersion = version
    , viCommit = commit
    , viChannel = ch
    , viLatestVersion = Nothing
    , viUpdateAvailable = False
    }
