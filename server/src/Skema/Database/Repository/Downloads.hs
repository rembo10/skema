{-# LANGUAGE OverloadedStrings #-}

-- | Download repository operations.
module Skema.Database.Repository.Downloads
  ( DownloadInsert(..)
  , insertDownload
  ) where

import Skema.Database.Connection
import Data.Time (UTCTime)
import Database.SQLite.Simple (Only(..))
import qualified Database.SQLite.Simple.ToRow as SQLite
import qualified Database.SQLite.Simple.ToField as SQLite
import qualified Database.SQLite.Simple as SQLite

-- | Helper type for inserting downloads with more than 10 fields
data DownloadInsert = DownloadInsert
  { diCatalogAlbumId :: Maybe Int64
  , diIndexerName :: Text
  , diDownloadUrl :: Text
  , diDownloadClient :: Text
  , diDownloadClientId :: Maybe Text
  , diStatus :: Text
  , diDownloadPath :: Maybe Text
  , diTitle :: Text
  , diSizeBytes :: Maybe Integer
  , diQuality :: Maybe Text
  , diFormat :: Maybe Text
  , diSeeders :: Maybe Int
  , diProgress :: Double
  , diErrorMessage :: Maybe Text
  , diQueuedAt :: UTCTime
  }

-- | SQLite ToRow instance for DownloadInsert
instance SQLite.ToRow DownloadInsert where
  toRow d = [ SQLite.toField (diCatalogAlbumId d)
            , SQLite.toField (diIndexerName d)
            , SQLite.toField (diDownloadUrl d)
            , SQLite.toField (diDownloadClient d)
            , SQLite.toField (diDownloadClientId d)
            , SQLite.toField (diStatus d)
            , SQLite.toField (diDownloadPath d)
            , SQLite.toField (diTitle d)
            , SQLite.toField (diSizeBytes d)
            , SQLite.toField (diQuality d)
            , SQLite.toField (diFormat d)
            , SQLite.toField (diSeeders d)
            , SQLite.toField (diProgress d)
            , SQLite.toField (diErrorMessage d)
            , SQLite.toField (diQueuedAt d)
            ]

-- | Insert a download record.
insertDownload :: SQLite.Connection
               -> Maybe Int64         -- catalog_album_id
               -> Text               -- indexer_name
               -> Text               -- download_url
               -> Text               -- download_client
               -> Maybe Text         -- download_client_id
               -> Text               -- status
               -> Maybe Text         -- download_path
               -> Text               -- title
               -> Maybe Integer      -- size_bytes
               -> Maybe Text         -- quality
               -> Maybe Text         -- format
               -> Maybe Int          -- seeders
               -> Double             -- progress
               -> Maybe Text         -- error_message
               -> UTCTime            -- queued_at
               -> IO Int64
insertDownload conn catalogAlbumId indexerName downloadUrl downloadClient downloadClientId status downloadPath title sizeBytes quality format seeders progress errorMessage queuedAt = do
  let downloadInsert = DownloadInsert
        { diCatalogAlbumId = catalogAlbumId
        , diIndexerName = indexerName
        , diDownloadUrl = downloadUrl
        , diDownloadClient = downloadClient
        , diDownloadClientId = downloadClientId
        , diStatus = status
        , diDownloadPath = downloadPath
        , diTitle = title
        , diSizeBytes = sizeBytes
        , diQuality = quality
        , diFormat = format
        , diSeeders = seeders
        , diProgress = progress
        , diErrorMessage = errorMessage
        , diQueuedAt = queuedAt
        }
  results <- queryRows conn
    "INSERT INTO downloads (catalog_album_id, indexer_name, download_url, download_client, download_client_id, status, download_path, title, size_bytes, quality, format, seeders, progress, error_message, queued_at) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING id"
    downloadInsert :: IO [Only Int64]
  case viaNonEmpty head results of
    Just (Only did) -> pure did
    Nothing -> fail "Failed to insert download"
