{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Skema.DownloadClient.QBittorrent
  ( QBittorrentClient(..)
  , createQBittorrentClient
  ) where

import Control.Exception (try)
import Data.Aeson (FromJSON(..), (.:), (.:?), withObject)
import qualified Data.Aeson as Aeson
import qualified Data.IORef as IORef
import Data.IORef ()
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics ()
import Network.HTTP.Client (RequestBody(..), parseRequest, responseBody, responseCookieJar, CookieJar, method, cookieJar, requestBody, requestHeaders)
import Network.HTTP.Types (hContentType, urlEncode)
import qualified Data.ByteString.Char8 as BS8

import Skema.DownloadClient.Types
import Skema.HTTP.Client (HttpClient, makeRequestWithRetry, prettyHttpError)

-- | qBittorrent client configuration
data QBittorrentClient = QBittorrentClient
  { qbtUrl :: Text
  , qbtUsername :: Text
  , qbtPassword :: Text
  , qbtHttpClient :: HttpClient
  , qbtCookieJar :: IORef (Maybe CookieJar)
  }

-- | Create a new qBittorrent client
createQBittorrentClient :: Text -> Text -> Text -> HttpClient -> IO QBittorrentClient
createQBittorrentClient url username password httpClient = do
  cookieJarRef <- IORef.newIORef Nothing
  pure $ QBittorrentClient url username password httpClient cookieJarRef

-- qBittorrent API response types
data QBTorrent = QBTorrent
  { qbtHash :: Text
  , qbtName :: Text
  , qbtState :: Text
  , qbtProgress :: Double
  , qbtSize :: Maybe Integer
  , qbtDownloaded :: Maybe Integer
  , qbtUploaded :: Maybe Integer
  , qbtRatio :: Maybe Double
  , qbtNumSeeds :: Maybe Int
  , qbtNumLeechs :: Maybe Int
  , qbtSavePath :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON QBTorrent where
  parseJSON = withObject "QBTorrent" $ \v -> do
    qbtHash <- v .: "hash"
    qbtName <- v .: "name"
    qbtState <- v .: "state"
    qbtProgress <- v .: "progress"
    qbtSize <- v .:? "size"
    qbtDownloaded <- v .:? "downloaded"
    qbtUploaded <- v .:? "uploaded"
    qbtRatio <- v .:? "ratio"
    qbtNumSeeds <- v .:? "num_seeds"
    qbtNumLeechs <- v .:? "num_leechs"
    qbtSavePath <- v .:? "save_path"
    pure QBTorrent{..}

instance DownloadClientAPI QBittorrentClient where
  testConnection client = do
    result <- try $ ensureLoggedIn client
    case result of
      Left (err :: SomeException) -> pure $ Left $ T.pack $ show err
      Right () -> pure $ Right ()

  addDownload client AddDownloadRequest{..} = do
    ensureLoggedIn client
    cookieJar <- IORef.readIORef (qbtCookieJar client)

    let url = T.unpack (qbtUrl client) <> "/api/v2/torrents/add"
    request <- parseRequest url
    let request' = request
          { method = "POST"
          , requestBody = RequestBodyBS $ "urls=" <> urlEncode True (TE.encodeUtf8 adrUrl)
          , requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
          , cookieJar = cookieJar
          }

    result <- makeRequestWithRetry (qbtHttpClient client) request'
    case result of
      Right _ ->
        -- qBittorrent doesn't return the hash immediately, we'll use the URL as a temp ID
        pure $ Right $ AddDownloadResult
          { adrClientId = adrUrl  -- We'll need to poll to get the actual hash
          , adrSuccess = True
          , adrMessage = Just "Torrent added, hash will be available shortly"
          }
      Left err -> pure $ Left $ "Failed to add torrent: " <> prettyHttpError err

  getDownloadStatus client downloadId = do
    ensureLoggedIn client
    torrents <- getAllTorrentsInternal client
    case find (\t -> qbtHash t == downloadId) torrents of
      Just torrent -> pure $ Right $ qbtTorrentToDownloadInfo torrent
      Nothing -> pure $ Left "Torrent not found"

  getAllDownloads client = do
    ensureLoggedIn client
    torrents <- getAllTorrentsInternal client
    pure $ Right $ map qbtTorrentToDownloadInfo torrents

  pauseDownload client downloadId = do
    ensureLoggedIn client
    cookieJar <- IORef.readIORef (qbtCookieJar client)

    let url = T.unpack (qbtUrl client) <> "/api/v2/torrents/pause"
    request <- parseRequest url
    let request' = request
          { method = "POST"
          , requestBody = RequestBodyBS $ "hashes=" <> TE.encodeUtf8 downloadId
          , requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
          , cookieJar = cookieJar
          }

    result <- makeRequestWithRetry (qbtHttpClient client) request'
    case result of
      Right _ -> pure $ Right ()
      Left err -> pure $ Left $ "Failed to pause torrent: " <> prettyHttpError err

  resumeDownload client downloadId = do
    ensureLoggedIn client
    cookieJar <- IORef.readIORef (qbtCookieJar client)

    let url = T.unpack (qbtUrl client) <> "/api/v2/torrents/resume"
    request <- parseRequest url
    let request' = request
          { method = "POST"
          , requestBody = RequestBodyBS $ "hashes=" <> TE.encodeUtf8 downloadId
          , requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
          , cookieJar = cookieJar
          }

    result <- makeRequestWithRetry (qbtHttpClient client) request'
    case result of
      Right _ -> pure $ Right ()
      Left err -> pure $ Left $ "Failed to resume torrent: " <> prettyHttpError err

  removeDownload client downloadId deleteFiles = do
    ensureLoggedIn client
    cookieJar <- IORef.readIORef (qbtCookieJar client)

    let deleteParam = if deleteFiles then "true" else "false"
        url = T.unpack (qbtUrl client) <> "/api/v2/torrents/delete"
    request <- parseRequest url
    let request' = request
          { method = "POST"
          , requestBody = RequestBodyBS $
              "hashes=" <> TE.encodeUtf8 downloadId <> "&deleteFiles=" <> BS8.pack deleteParam
          , requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
          , cookieJar = cookieJar
          }

    result <- makeRequestWithRetry (qbtHttpClient client) request'
    case result of
      Right _ -> pure $ Right ()
      Left err -> pure $ Left $ "Failed to remove torrent: " <> prettyHttpError err

-- Helper functions

ensureLoggedIn :: QBittorrentClient -> IO ()
ensureLoggedIn QBittorrentClient{..} = do
  cookieJar <- IORef.readIORef qbtCookieJar
  case cookieJar of
    Just _ -> pure ()  -- Already logged in
    Nothing -> do
      -- Need to login
      let url = T.unpack qbtUrl <> "/api/v2/auth/login"
      request <- parseRequest url
      let encodedUsername = urlEncode True (TE.encodeUtf8 qbtUsername)
          encodedPassword = urlEncode True (TE.encodeUtf8 qbtPassword)
          request' = request
            { method = "POST"
            , requestBody = RequestBodyBS $
                "username=" <> encodedUsername <>
                "&password=" <> encodedPassword
            , requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
            }

      result <- makeRequestWithRetry qbtHttpClient request'
      case result of
        Right response -> IORef.writeIORef qbtCookieJar (Just (responseCookieJar response))
        Left err -> fail $ "Failed to login to qBittorrent: " <> T.unpack (prettyHttpError err)

getAllTorrentsInternal :: QBittorrentClient -> IO [QBTorrent]
getAllTorrentsInternal client = do
  cookieJar <- IORef.readIORef (qbtCookieJar client)

  let url = T.unpack (qbtUrl client) <> "/api/v2/torrents/info"
  request <- parseRequest url
  let request' = case cookieJar of
        Just jar -> request { cookieJar = Just jar }
        Nothing -> request

  result <- makeRequestWithRetry (qbtHttpClient client) request'
  case result of
    Left err -> fail $ "Failed to fetch qBittorrent torrents: " <> T.unpack (prettyHttpError err)
    Right response ->
      case Aeson.eitherDecode (responseBody response) of
        Left decodeErr -> fail $ "Failed to parse qBittorrent response: " <> decodeErr
        Right torrents -> pure torrents

qbtTorrentToDownloadInfo :: QBTorrent -> DownloadInfo
qbtTorrentToDownloadInfo QBTorrent{..} =
  let status = case T.toLower qbtState of
        st | "downloading" `T.isInfixOf` st -> DSDownloading
           | "paused" `T.isInfixOf` st -> DSPaused
           | "queued" `T.isInfixOf` st -> DSQueued
           | "completed" `T.isInfixOf` st || "seeding" `T.isInfixOf` st -> DSCompleted
           | "error" `T.isInfixOf` st || "missing" `T.isInfixOf` st -> DSFailed
           | otherwise -> DSQueued
  in DownloadInfo
    { diClientId = qbtHash
    , diStatus = status
    , diProgress = qbtProgress
    , diDownloadPath = qbtSavePath
    , diErrorMessage = if status == DSFailed then Just qbtState else Nothing
    , diSizeBytes = qbtSize
    , diDownloadedBytes = qbtDownloaded
    , diUploadRatio = qbtRatio
    , diSeeders = qbtNumSeeds
    , diPeers = qbtNumLeechs
    , diCategory = Nothing  -- TODO: Fetch category from QBittorrent API
    }
