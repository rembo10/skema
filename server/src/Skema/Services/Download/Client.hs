{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Download client instance creation and management.
--
-- This module provides utilities for creating and managing download client
-- instances (SABnzbd, Transmission, QBittorrent, etc.).
module Skema.Services.Download.Client
  ( DownloadClientInstance(..)
  , createClientInstance
  , createClientInstances
  ) where

import Skema.HTTP.Client (HttpClient)
import Skema.Config.Types (DownloadConfig(..), DownloadClient(..), DownloadClientType(..), downloadClientTypeName)
import Skema.DownloadClient.Types (DownloadClientAPI(..))
import Skema.DownloadClient.SABnzbd (createSABnzbdClient, SABnzbdClient)
import Skema.DownloadClient.Transmission (createTransmissionClient, TransmissionClient)
import Skema.DownloadClient.QBittorrent (createQBittorrentClient, QBittorrentClient)

-- ============================================================================
-- TYPES
-- ============================================================================

-- | Wrapper for different download client types
data DownloadClientInstance
  = SABInstance SABnzbdClient
  | TransmissionInstance TransmissionClient
  | QBittorrentInstance QBittorrentClient

instance DownloadClientAPI DownloadClientInstance where
  testConnection (SABInstance c) = testConnection c
  testConnection (TransmissionInstance c) = testConnection c
  testConnection (QBittorrentInstance c) = testConnection c

  addDownload (SABInstance c) = addDownload c
  addDownload (TransmissionInstance c) = addDownload c
  addDownload (QBittorrentInstance c) = addDownload c

  getDownloadStatus (SABInstance c) = getDownloadStatus c
  getDownloadStatus (TransmissionInstance c) = getDownloadStatus c
  getDownloadStatus (QBittorrentInstance c) = getDownloadStatus c

  getAllDownloads (SABInstance c) = getAllDownloads c
  getAllDownloads (TransmissionInstance c) = getAllDownloads c
  getAllDownloads (QBittorrentInstance c) = getAllDownloads c

  pauseDownload (SABInstance c) = pauseDownload c
  pauseDownload (TransmissionInstance c) = pauseDownload c
  pauseDownload (QBittorrentInstance c) = pauseDownload c

  resumeDownload (SABInstance c) = resumeDownload c
  resumeDownload (TransmissionInstance c) = resumeDownload c
  resumeDownload (QBittorrentInstance c) = resumeDownload c

  removeDownload (SABInstance c) = removeDownload c
  removeDownload (TransmissionInstance c) = removeDownload c
  removeDownload (QBittorrentInstance c) = removeDownload c

-- ============================================================================
-- CLIENT CREATION
-- ============================================================================

-- | Create a download client instance from configuration.
createClientInstance :: HttpClient -> DownloadClient -> IO DownloadClientInstance
createClientInstance httpClient DownloadClient{..} = do
  case dcType of
    SABnzbd -> do
      let apiKey = fromMaybe "" dcApiKey
      pure $ SABInstance $ createSABnzbdClient dcUrl apiKey httpClient dcDownloadDir dcCategory

    NZBGet ->
      fail "NZBGet client not yet implemented"

    Transmission -> do
      client <- createTransmissionClient dcUrl dcUsername dcPassword httpClient
      pure $ TransmissionInstance client

    QBittorrent -> do
      let username = fromMaybe "" dcUsername
          password = fromMaybe "" dcPassword
      client <- createQBittorrentClient dcUrl username password httpClient
      pure $ QBittorrentInstance client

-- | Create download client instances from configuration
createClientInstances :: HttpClient -> DownloadConfig -> IO [(Text, DownloadClientInstance)]
createClientInstances httpClient DownloadConfig{..} = do
  let clients = catMaybes [downloadNzbClient, downloadTorrentClient]
  catMaybes <$> forM clients (\client -> do
    if not (dcEnabled client)
      then pure Nothing
      else do
        instance' <- case dcType client of
          SABnzbd -> do
            let apiKey = fromMaybe "" (dcApiKey client)
            pure $ Just $ SABInstance $ createSABnzbdClient
              (dcUrl client)
              apiKey
              httpClient
              (dcDownloadDir client)
              (dcCategory client)

          NZBGet -> do
            -- TODO: Implement NZBGet client
            pure Nothing

          Transmission -> do
            client' <- createTransmissionClient
              (dcUrl client)
              (dcUsername client)
              (dcPassword client)
              httpClient
            pure $ Just $ TransmissionInstance client'

          QBittorrent -> do
            let username = fromMaybe "" (dcUsername client)
                password = fromMaybe "" (dcPassword client)
            client' <- createQBittorrentClient
              (dcUrl client)
              username
              password
              httpClient
            pure $ Just $ QBittorrentInstance client'

        pure $ fmap (\inst -> (downloadClientTypeName (dcType client), inst)) instance'
    )
