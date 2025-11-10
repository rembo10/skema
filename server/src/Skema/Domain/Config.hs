{-# LANGUAGE OverloadedStrings #-}

-- | Pure configuration domain logic.
--
-- This module contains all pure business logic for configuration management.
-- Validation, transformation, and update logic with no side effects.
module Skema.Domain.Config
  ( -- * Config updates
    applyConfigUpdate
  ) where

import Skema.API.Types.Config (ConfigUpdate(..))
import Skema.Domain.Converters (downloadClientFromAPI, indexerFromAPI, textToMusicBrainzServer)
import qualified Skema.Config.Validation as CfgVal
import qualified Skema.Config.Types as Cfg
import qualified System.OsPath as OP

-- | Apply ConfigUpdate to Config, returning updated Config.
--
-- This is the pure part of config updates. Password hashing must be done
-- by the caller before passing plaintext passwords here.
--
-- Returns Left error if the update would result in invalid configuration.
applyConfigUpdate :: Cfg.Config
                  -> ConfigUpdate
                  -> Maybe Text  -- ^ Pre-hashed password (if password update requested)
                  -> IO (Either Text Cfg.Config)
applyConfigUpdate cfg update maybeHashedPassword = do
  let libCfg = Cfg.library cfg
  let sysCfg = Cfg.system cfg
  let srvCfg = Cfg.server cfg
  let dlCfg = Cfg.download cfg
  let idxCfg = Cfg.indexers cfg
  let mbCfg = Cfg.musicbrainz cfg

  -- Apply library config updates
  updatedLibPath <- case updateLibraryPath update of
    Nothing -> pure (Cfg.libraryPath libCfg)
    Just Nothing -> pure Nothing
    Just (Just pathText) -> Just <$> OP.encodeUtf (toString pathText)

  let updatedLibCfg = libCfg
        { Cfg.libraryPath = updatedLibPath
        , Cfg.libraryWatch = fromMaybe (Cfg.libraryWatch libCfg) (updateLibraryWatch update)
        , Cfg.libraryAutoScan = fromMaybe (Cfg.libraryAutoScan libCfg) (updateLibraryAutoScan update)
        , Cfg.libraryAutoScanIntervalMins = fromMaybe (Cfg.libraryAutoScanIntervalMins libCfg) (updateLibraryAutoScanIntervalMins update)
        , Cfg.libraryAutoScanOnStartup = fromMaybe (Cfg.libraryAutoScanOnStartup libCfg) (updateLibraryAutoScanOnStartup update)
        , Cfg.libraryNormalizeFeaturing = fromMaybe (Cfg.libraryNormalizeFeaturing libCfg) (updateLibraryNormalizeFeaturing update)
        , Cfg.libraryNormalizeFeaturingTo = fromMaybe (Cfg.libraryNormalizeFeaturingTo libCfg) (updateLibraryNormalizeFeaturingTo update)
        }

  -- Apply system config updates
  let updatedSysCfg = sysCfg
        { Cfg.systemWatchConfigFile = fromMaybe (Cfg.systemWatchConfigFile sysCfg) (updateSystemWatchConfigFile update)
        , Cfg.systemDatabaseBackend = fromMaybe (Cfg.systemDatabaseBackend sysCfg) (updateSystemDatabaseBackend update)
        , Cfg.systemDatabasePath = fromMaybe (Cfg.systemDatabasePath sysCfg) (updateSystemDatabasePath update)
        }

  -- Apply server config updates
  -- Use pre-hashed password if provided
  let updatedPassword = case updateServerPassword update of
        Nothing -> Cfg.serverPassword srvCfg  -- No change
        Just Nothing -> Nothing  -- Clear password
        Just (Just _) -> maybeHashedPassword  -- Use pre-hashed version

  let updatedSrvCfg = srvCfg
        { Cfg.serverHost = fromMaybe (Cfg.serverHost srvCfg) (updateServerHost update)
        , Cfg.serverPort = fromMaybe (Cfg.serverPort srvCfg) (updateServerPort update)
        , Cfg.serverUsername = case updateServerUsername update of
            Nothing -> Cfg.serverUsername srvCfg
            Just val -> val
        , Cfg.serverPassword = updatedPassword
        , Cfg.serverJwtExpirationHours = fromMaybe (Cfg.serverJwtExpirationHours srvCfg) (updateServerJwtExpirationHours update)
        }

  -- Apply download config updates
  let updatedDlCfg = dlCfg
        { Cfg.downloadNzbClient = case updateDownloadNzbClient update of
            Nothing -> Cfg.downloadNzbClient dlCfg
            Just Nothing -> Nothing
            Just (Just apiClient) -> Just (downloadClientFromAPI apiClient)
        , Cfg.downloadTorrentClient = case updateDownloadTorrentClient update of
            Nothing -> Cfg.downloadTorrentClient dlCfg
            Just Nothing -> Nothing
            Just (Just apiClient) -> Just (downloadClientFromAPI apiClient)
        , Cfg.downloadDirectory = fromMaybe (Cfg.downloadDirectory dlCfg) (updateDownloadDirectory update)
        , Cfg.downloadCheckInterval = fromMaybe (Cfg.downloadCheckInterval dlCfg) (updateDownloadCheckInterval update)
        , Cfg.downloadAutoImport = fromMaybe (Cfg.downloadAutoImport dlCfg) (updateDownloadAutoImport update)
        , Cfg.downloadMinSeeders = case updateDownloadMinSeeders update of
            Nothing -> Cfg.downloadMinSeeders dlCfg
            Just val -> val
        , Cfg.downloadMaxSize = case updateDownloadMaxSizeMB update of
            Nothing -> Cfg.downloadMaxSize dlCfg
            Just val -> val
        }

  -- Apply indexer config updates
  let updatedIdxCfg = idxCfg
        { Cfg.indexerList = case updateIndexersList update of
            Nothing -> Cfg.indexerList idxCfg
            Just apiIndexers -> map indexerFromAPI apiIndexers
        , Cfg.indexerSearchTimeout = fromMaybe (Cfg.indexerSearchTimeout idxCfg) (updateIndexersSearchTimeout update)
        }

  -- Apply MusicBrainz config updates
  updatedMBServer <- case updateMusicBrainzServer update of
    Nothing -> pure $ Cfg.mbServer mbCfg
    Just serverText -> case textToMusicBrainzServer serverText of
      Nothing -> pure $ Cfg.mbServer mbCfg  -- Invalid value, keep current
      Just svr -> pure svr

  let updatedMbCfg = mbCfg
        { Cfg.mbServer = updatedMBServer
        , Cfg.mbUsername = case updateMusicBrainzUsername update of
            Nothing -> Cfg.mbUsername mbCfg
            Just val -> val
        , Cfg.mbPassword = case updateMusicBrainzPassword update of
            Nothing -> Cfg.mbPassword mbCfg
            Just val -> val
        }

  let updatedCfg = cfg
        { Cfg.library = updatedLibCfg
        , Cfg.system = updatedSysCfg
        , Cfg.server = updatedSrvCfg
        , Cfg.download = updatedDlCfg
        , Cfg.indexers = updatedIdxCfg
        , Cfg.musicbrainz = updatedMbCfg
        }

  -- Validate the updated config
  case CfgVal.validateConfig updatedCfg of
    Just err -> pure $ Left err
    Nothing -> pure $ Right updatedCfg
