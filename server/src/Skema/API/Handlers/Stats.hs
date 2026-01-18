{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Stats API handlers.
module Skema.API.Handlers.Stats
  ( statsServer
  ) where

import Skema.API.Types.Library (StatsAPI, LibraryStats(..))
import Skema.Auth (requireAuth)
import Skema.Auth.JWT (JWTSecret)
import Skema.Database.Connection
import Skema.Database.Repository
import qualified Skema.Config.Types as Cfg
import Skema.FileSystem.Utils (osPathToString, stringToOsPath)
import qualified System.OsPath as OP
import Servant
import qualified Control.Concurrent.STM as STM

-- | Stats API handler.
statsServer :: Cfg.ServerConfig -> JWTSecret -> ConnectionPool -> TVar Cfg.Config -> Server StatsAPI
statsServer _serverCfg jwtSecret connPool configVar = \maybeAuthHeader -> do
  _ <- requireAuth configVar jwtSecret maybeAuthHeader
  liftIO $ withConnection connPool $ \conn -> do
    (totalFiles, totalAlbums, totalArtists, matchedFiles, unmatchedFiles, accuracy, totalDiffs, totalSize, totalRuntime) <- getLibraryStats conn

    -- Read current library path from config
    config <- STM.atomically $ STM.readTVar configVar
    libPath <- case Cfg.libraryPath (Cfg.library config) of
      Nothing -> pure Nothing
      Just osPath -> Just . toText <$> osPathToString osPath

    pure $ LibraryStats
      { statsTotalFiles = totalFiles
      , statsTotalAlbums = totalAlbums
      , statsTotalArtists = totalArtists
      , statsMatchedFiles = matchedFiles
      , statsUnmatchedFiles = unmatchedFiles
      , statsMetadataAccuracy = accuracy
      , statsTotalDiffs = totalDiffs
      , statsLibrarySize = totalSize
      , statsTotalRuntime = totalRuntime
      , statsLibraryPath = libPath
      }
