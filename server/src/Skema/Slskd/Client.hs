{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | HTTP client for the slskd REST API.
--
-- Provides functions to interact with slskd for searching,
-- downloading, and monitoring transfers.
module Skema.Slskd.Client
  ( -- * Client
    SlskdClient (..)
  , createSlskdClient
    -- * Connection Test
  , testConnection
    -- * Search Operations
  , initiateSearch
  , getSearchStatus
  , getSearchResponses
  , deleteSearch
    -- * Transfer Operations
  , queueDownload
  , queueDownloads
  , getTransfers
  , getTransfersByUsername
  , cancelTransfer
  ) where

import Control.Exception (try)
import Data.Aeson (FromJSON, ToJSON, Value, decode, encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
  ( Manager
  , Request
  , RequestBody (..)
  , httpLbs
  , method
  , parseRequest
  , requestBody
  , requestHeaders
  , responseBody
  , responseStatus
  )
import Network.HTTP.Types.Status (statusCode)

import Skema.Config.Types (SlskdConfig (..))
import Skema.HTTP.Client (HttpClient, getManager)
import Skema.Slskd.Types

-- | slskd client instance.
data SlskdClient = SlskdClient
  { scConfig :: SlskdConfig
    -- ^ Client configuration
  , scManager :: Manager
    -- ^ HTTP manager for requests
  }
  deriving (Generic)

-- | Create a new slskd client from config and HTTP client.
createSlskdClient :: SlskdConfig -> HttpClient -> SlskdClient
createSlskdClient config httpClient =
  SlskdClient
    { scConfig = config
    , scManager = getManager httpClient
    }

-- | Make an authenticated request to slskd API.
makeSlskdRequest ::
  (FromJSON a) =>
  SlskdClient ->
  Text -> -- HTTP method
  Text -> -- API path (e.g., "/api/v0/searches")
  Maybe LBS.ByteString -> -- Request body
  IO (Either Text a)
makeSlskdRequest SlskdClient {..} httpMethod apiPath maybeBody = do
  let baseUrl = T.dropWhileEnd (== '/') (slskdUrl scConfig)
      fullUrl = baseUrl <> apiPath
      apiKey = slskdApiKey scConfig

  case parseRequest (toString fullUrl) of
    Nothing -> pure $ Left $ "Invalid URL: " <> fullUrl
    Just req -> do
      let req' =
            req
              { method = encodeUtf8 httpMethod
              , requestHeaders =
                  [ ("X-API-Key", encodeUtf8 apiKey)
                  , ("Content-Type", "application/json")
                  , ("Accept", "application/json")
                  ]
              , requestBody = maybe mempty RequestBodyLBS maybeBody
              }

      result <- try $ httpLbs req' scManager
      case result of
        Left (err :: SomeException) ->
          pure $ Left $ "HTTP error: " <> show err
        Right response -> do
          let status = statusCode $ responseStatus response
              body = responseBody response
          if status >= 200 && status < 300
            then case decode body of
              Nothing ->
                -- Some endpoints return empty body on success
                if LBS.null body || body == "null"
                  then
                    -- Try to decode as the expected type, or fail
                    pure $ Left "Empty response body"
                  else
                    pure $
                      Left $
                        "Failed to decode response: "
                          <> decodeUtf8 (LBS.toStrict body)
              Just val -> pure $ Right val
            else
              pure $
                Left $
                  "HTTP "
                    <> show status
                    <> ": "
                    <> decodeUtf8 (LBS.toStrict body)

-- | Make a request that returns no meaningful body.
makeSlskdRequestNoBody ::
  SlskdClient ->
  Text -> -- HTTP method
  Text -> -- API path
  Maybe LBS.ByteString -> -- Request body
  IO (Either Text ())
makeSlskdRequestNoBody SlskdClient {..} httpMethod apiPath maybeBody = do
  let baseUrl = T.dropWhileEnd (== '/') (slskdUrl scConfig)
      fullUrl = baseUrl <> apiPath
      apiKey = slskdApiKey scConfig

  case parseRequest (toString fullUrl) of
    Nothing -> pure $ Left $ "Invalid URL: " <> fullUrl
    Just req -> do
      let req' =
            req
              { method = encodeUtf8 httpMethod
              , requestHeaders =
                  [ ("X-API-Key", encodeUtf8 apiKey)
                  , ("Content-Type", "application/json")
                  , ("Accept", "application/json")
                  ]
              , requestBody = maybe mempty RequestBodyLBS maybeBody
              }

      result <- try $ httpLbs req' scManager
      case result of
        Left (err :: SomeException) ->
          pure $ Left $ "HTTP error: " <> show err
        Right response -> do
          let status = statusCode $ responseStatus response
              body = responseBody response
          if status >= 200 && status < 300
            then pure $ Right ()
            else
              pure $
                Left $
                  "HTTP "
                    <> show status
                    <> ": "
                    <> decodeUtf8 (LBS.toStrict body)

-- | Test connection to slskd API.
--
-- Returns Right () if connection successful, Left error otherwise.
testConnection :: SlskdClient -> IO (Either Text ())
testConnection client = do
  -- Try to get application info to verify connectivity
  result <- makeSlskdRequest client "GET" "/api/v0/application" Nothing
  case result of
    Left err -> pure $ Left err
    Right (_ :: Value) -> pure $ Right ()

-- | Initiate a new search.
--
-- POST /api/v0/searches
-- Returns the search ID on success.
initiateSearch :: SlskdClient -> Text -> IO (Either Text Text)
initiateSearch client searchText = do
  let body =
        encode $
          object
            [ "searchText" .= searchText
            ]
  result <- makeSlskdRequest client "POST" "/api/v0/searches" (Just body)
  case result of
    Left err -> pure $ Left err
    Right (response :: SlskdSearchResponse) -> pure $ Right $ ssId response

-- | Get status of a search.
--
-- GET /api/v0/searches/{id}
getSearchStatus :: SlskdClient -> Text -> IO (Either Text SlskdSearchResponse)
getSearchStatus client searchId = do
  let path = "/api/v0/searches/" <> searchId
  makeSlskdRequest client "GET" path Nothing

-- | Get search responses (detailed results with files).
--
-- GET /api/v0/searches/{id}/responses
-- This endpoint returns the actual user responses with file lists,
-- unlike getSearchStatus which only returns counts.
getSearchResponses :: SlskdClient -> Text -> IO (Either Text [SlskdSearchResult])
getSearchResponses client searchId = do
  let path = "/api/v0/searches/" <> searchId <> "/responses"
  makeSlskdRequest client "GET" path Nothing

-- | Delete a search.
--
-- DELETE /api/v0/searches/{id}
deleteSearch :: SlskdClient -> Text -> IO (Either Text ())
deleteSearch client searchId = do
  let path = "/api/v0/searches/" <> searchId
  makeSlskdRequestNoBody client "DELETE" path Nothing

-- | Queue a single file for download from a user.
--
-- POST /api/v0/transfers/downloads/{username}
queueDownload ::
  SlskdClient ->
  Text -> -- Username
  Text -> -- Filename (full path)
  Integer -> -- Size in bytes
  IO (Either Text ())
queueDownload client username filename size = do
  let path = "/api/v0/transfers/downloads/" <> username
      body =
        encode
          [ object
              [ "filename" .= filename
              , "size" .= size
              ]
          ]
  makeSlskdRequestNoBody client "POST" path (Just body)

-- | Queue multiple files for download from a user.
--
-- POST /api/v0/transfers/downloads/{username}
queueDownloads ::
  SlskdClient ->
  Text -> -- Username
  [SlskdFile] -> -- Files to download
  IO (Either Text ())
queueDownloads client username files = do
  let path = "/api/v0/transfers/downloads/" <> username
      body =
        encode $
          map
            ( \f ->
                object
                  [ "filename" .= sfFilename f
                  , "size" .= sfSize f
                  ]
            )
            files
  makeSlskdRequestNoBody client "POST" path (Just body)

-- | Get all downloads.
--
-- GET /api/v0/transfers/downloads
-- The API returns a nested structure: [{ username, directories: [{ directory, files: [...] }] }]
-- We flatten it to a list of transfers.
getTransfers :: SlskdClient -> IO (Either Text [SlskdTransfer])
getTransfers client = do
  result <- makeSlskdRequest client "GET" "/api/v0/transfers/downloads" Nothing
  case result of
    Left err -> pure $ Left err
    Right (userTransfers :: [SlskdUserTransfers]) ->
      pure $ Right $ flattenTransfers userTransfers

-- | Get downloads for a specific user.
--
-- GET /api/v0/transfers/downloads/{username}
-- The API returns a nested structure: [{ username, directories: [{ directory, files: [...] }] }]
-- We flatten it to a list of transfers.
getTransfersByUsername ::
  SlskdClient ->
  Text -> -- Username
  IO (Either Text [SlskdTransfer])
getTransfersByUsername client username = do
  let path = "/api/v0/transfers/downloads/" <> username
  result <- makeSlskdRequest client "GET" path Nothing
  case result of
    Left err -> pure $ Left err
    Right (userTransfers :: [SlskdUserTransfers]) ->
      pure $ Right $ flattenTransfers userTransfers

-- | Flatten nested user/directory/file structure to a flat list of transfers.
flattenTransfers :: [SlskdUserTransfers] -> [SlskdTransfer]
flattenTransfers userTransfers =
  [ transfer
  | user <- userTransfers
  , dir <- sutDirectories user
  , transfer <- sdtFiles dir
  ]

-- | Cancel a transfer.
--
-- DELETE /api/v0/transfers/downloads/{username}/{id}
cancelTransfer ::
  SlskdClient ->
  Text -> -- Username
  Text -> -- Transfer ID
  Bool -> -- Remove from list
  IO (Either Text ())
cancelTransfer client username transferId removeFromList = do
  let path =
        "/api/v0/transfers/downloads/"
          <> username
          <> "/"
          <> transferId
          <> "?remove="
          <> (if removeFromList then "true" else "false")
  makeSlskdRequestNoBody client "DELETE" path Nothing
