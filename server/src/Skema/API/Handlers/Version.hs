{-# LANGUAGE OverloadedStrings #-}

-- | Version API handler.
module Skema.API.Handlers.Version
  ( versionServer
  ) where

import Skema.API.Types.Version (VersionAPI, VersionResponse(..))
import Skema.API.App (Env(..), AppM)
import Skema.Services.UpdateChecker (LatestRelease(..))
import Skema.Version (getVersionInfo, VersionInfo(..))
import Servant

-- | Version API handler.
versionServer :: Env -> ServerT VersionAPI AppM
versionServer Env{envLatestRelease = latestVar} = liftIO $ do
  vi <- getVersionInfo
  latest <- readTVarIO latestVar
  pure $ VersionResponse
    { vrVersion = viVersion vi
    , vrCommit = viCommit vi
    , vrChannel = viChannel vi
    , vrLatestVersion = lrVersion <$> latest
    , vrUpdateAvailable = maybe False lrUpdateAvailable latest
    }
