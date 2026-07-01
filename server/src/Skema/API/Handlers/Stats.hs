{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Stats API handlers.
module Skema.API.Handlers.Stats
  ( statsServer
  ) where

import Skema.API.Types.Library (StatsAPI, LibraryStats(..))
import Skema.API.Handlers.Utils (readConfig)
import Skema.Database.Connection
import Skema.Database.Repository
import qualified Skema.Config.Types as Cfg
import Skema.FileSystem.Utils (osPathToString)
import Servant

-- | Stats API handler.
statsServer :: Cfg.ServerConfig -> ConnectionPool -> TVar Cfg.Config -> Server StatsAPI
statsServer _serverCfg connPool configVar =
  liftIO $ withConnection connPool $ \conn -> do
    (totalFiles, totalAlbums, totalArtists, matchedFiles, unmatchedFiles, accuracy, totalDiffs, totalSize, totalRuntime, catalogInLibrary, catalogWanted) <- getLibraryStats conn

    -- Read current library path from config
    config <- readConfig configVar
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
      , statsCatalogInLibrary = catalogInLibrary
      , statsCatalogWanted = catalogWanted
      }
