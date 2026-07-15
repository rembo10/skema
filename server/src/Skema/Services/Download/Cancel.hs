{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Cancelling downloads.
--
-- Cancellation has two halves: telling the download client (slskd, NZB, or
-- torrent) to stop the transfer, and removing the database record. Every
-- code path that cancels a download — the downloads DELETE endpoint as well
-- as quality-profile changes that unwant an album — must go through
-- 'cancelDownload' so the client-side half cannot be skipped.
module Skema.Services.Download.Cancel
  ( cancelDownload
  ) where

import Skema.Database.Connection (ConnectionPool, withConnection)
import qualified Skema.Database.Types as DBTypes
import qualified Skema.Database.Repository.Downloads as DownloadsRepo
import qualified Skema.Config.Types as Cfg
import Skema.HTTP.Client (HttpClient)
import Skema.Services.Download.Client (createClientInstance)
import Skema.DownloadClient.Types (removeDownload)
import qualified Skema.Slskd.Client as Slskd
import qualified Skema.Slskd.Types as SlskdTypes
import Katip
import qualified Data.Text as T

-- | Cancel a download: stop it at the download client if it is still queued
-- or downloading, then delete its database record.
cancelDownload :: LogEnv -> HttpClient -> Cfg.DownloadConfig -> ConnectionPool -> Int64 -> IO ()
cancelDownload le httpClient downloadConfig pool downloadId = do
  maybeDownload <- withConnection pool $ \conn ->
    DownloadsRepo.getDownloadById conn downloadId

  forM_ maybeDownload $ \download -> do
    let status = DBTypes.downloadStatus download
    when (status == DBTypes.DownloadDownloading || status == DBTypes.DownloadQueued) $
      cancelAtClient le httpClient downloadConfig download

  withConnection pool $ \conn ->
    DownloadsRepo.deleteDownload conn downloadId

-- | Tell the download client to stop the transfer, logging failures.
cancelAtClient :: LogEnv -> HttpClient -> Cfg.DownloadConfig -> DBTypes.DownloadRecord -> IO ()
cancelAtClient le httpClient downloadConfig download = do
  let clientName = DBTypes.downloadClient download
      clientId = DBTypes.downloadClientId download

  runKatipContextT le () "download.cancel" $
    $(logTM) InfoS $ logStr $
      ("Cancelling in-progress download " <> show (DBTypes.downloadId download)
        <> " from client: " <> show clientName :: Text)

  case (clientName, clientId) of
    (Just "slskd", Just cid) -> do
      -- The client_id format is "username:title"; slskd cancels per transfer
      let username = T.takeWhile (/= ':') cid
      case Cfg.downloadSlskdClient downloadConfig of
        Just slskdConfig | Cfg.slskdEnabled slskdConfig -> do
          let client = Slskd.createSlskdClient slskdConfig httpClient
          result <- Slskd.getTransfersByUsername client username
          case result of
            Left err -> runKatipContextT le () "download.cancel" $
              $(logTM) WarningS $ logStr $ ("Failed to get slskd transfers: " <> err :: Text)
            Right transfers -> do
              forM_ transfers $ \transfer ->
                Slskd.cancelTransfer client username (SlskdTypes.stId transfer) True
              runKatipContextT le () "download.cancel" $
                $(logTM) InfoS $ logStr $
                  ("Cancelled " <> show (length transfers) <> " slskd transfers" :: Text)
        _ -> pure ()

    (Just client, Just cid)
      | client == "SABnzbd" || client == "NZBGet" ->
          removeFromClient (Cfg.downloadNzbClient downloadConfig) cid "NZB"
      | client == "Transmission" || client == "qBittorrent" ->
          removeFromClient (Cfg.downloadTorrentClient downloadConfig) cid "torrent"

    _ -> pure ()  -- Unknown client or no client id, just delete from DB
  where
    removeFromClient :: Maybe Cfg.DownloadClient -> Text -> Text -> IO ()
    removeFromClient maybeClientConfig cid kind =
      case maybeClientConfig of
        Just clientConfig | Cfg.dcEnabled clientConfig -> do
          clientInstance <- createClientInstance httpClient clientConfig
          result <- removeDownload clientInstance cid True
          case result of
            Left err -> runKatipContextT le () "download.cancel" $
              $(logTM) WarningS $ logStr $
                ("Failed to remove from " <> kind <> " client: " <> err :: Text)
            Right () -> runKatipContextT le () "download.cancel" $
              $(logTM) InfoS $ logStr $
                ("Removed download from " <> kind <> " client" :: Text)
        _ -> pure ()
