{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Repository functions for search history.
module Skema.Database.Repository.SearchHistory
  ( insertSearchHistory
  , insertSearchHistoryResult
  , insertSearchHistoryResults
  , getSearchHistoryByAlbum
  , getSearchHistoryResults
  , getLatestSearchForAlbum
  ) where

import Data.Time (UTCTime)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple ((:.)(..), Only(..))
import Skema.Database.Types
import Skema.Database.Connection (queryRows)
import Skema.Database.Utils (insertReturningId)

-- | Insert a search history record and return its ID.
insertSearchHistory :: SQLite.Connection
                    -> Int64           -- ^ catalog_album_id
                    -> UTCTime         -- ^ searched_at
                    -> Int             -- ^ total_results
                    -> Maybe Int       -- ^ duration_ms
                    -> Maybe Text      -- ^ selected_release_title
                    -> Maybe Text      -- ^ selected_release_indexer
                    -> Maybe Int       -- ^ selected_release_score
                    -> SearchOutcome   -- ^ outcome
                    -> IO Int64
insertSearchHistory conn albumId searchedAt totalResults durationMs selectedTitle selectedIndexer selectedScore outcome = do
  insertReturningId conn
    "INSERT INTO search_history (catalog_album_id, searched_at, total_results, search_duration_ms, \
    \selected_release_title, selected_release_indexer, selected_release_score, outcome) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?) RETURNING id"
    (albumId, searchedAt, totalResults, durationMs, selectedTitle, selectedIndexer, selectedScore, searchOutcomeToText outcome)

-- | Insert a single search history result.
insertSearchHistoryResult :: SQLite.Connection
                          -> Int64           -- ^ search_history_id
                          -> Text            -- ^ indexer_name
                          -> Text            -- ^ title
                          -> Text            -- ^ download_url
                          -> Maybe Text      -- ^ info_url
                          -> Maybe Integer   -- ^ size_bytes
                          -> Maybe UTCTime   -- ^ publish_date
                          -> Maybe Int       -- ^ seeders
                          -> Maybe Int       -- ^ peers
                          -> Maybe Int       -- ^ grabs
                          -> Text            -- ^ download_type
                          -> Maybe Text      -- ^ quality
                          -> Int             -- ^ score
                          -> Int             -- ^ rank
                          -> IO ()
insertSearchHistoryResult conn historyId indexerName title downloadUrl infoUrl sizeBytes publishDate seeders peers grabs downloadType quality score rank = do
  -- Use nested tuples to work around ToRow instance limit
  SQLite.execute conn
    "INSERT INTO search_history_results (search_history_id, indexer_name, title, download_url, info_url, \
    \size_bytes, publish_date, seeders, peers, grabs, download_type, quality, score, rank) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    ((historyId, indexerName, title, downloadUrl, infoUrl, sizeBytes, publishDate) SQLite.:. (seeders, peers, grabs, downloadType, quality, score, rank))

-- | Insert multiple search history results efficiently.
insertSearchHistoryResults :: SQLite.Connection -> Int64 -> [(Text, Text, Text, Maybe Text, Maybe Integer, Maybe UTCTime, Maybe Int, Maybe Int, Maybe Int, Text, Maybe Text, Int, Int)] -> IO ()
insertSearchHistoryResults conn historyId results = do
  forM_ results $ \(indexerName, title, downloadUrl, infoUrl, sizeBytes, publishDate, seeders, peers, grabs, downloadType, quality, score, rank) ->
    insertSearchHistoryResult conn historyId indexerName title downloadUrl infoUrl sizeBytes publishDate seeders peers grabs downloadType quality score rank

-- | Get search history for an album, ordered by most recent first.
getSearchHistoryByAlbum :: SQLite.Connection -> Int64 -> IO [SearchHistoryRecord]
getSearchHistoryByAlbum conn albumId =
  queryRows conn
    "SELECT id, catalog_album_id, searched_at, total_results, search_duration_ms, \
    \selected_release_title, selected_release_indexer, selected_release_score, outcome \
    \FROM search_history WHERE catalog_album_id = ? ORDER BY searched_at DESC"
    (SQLite.Only albumId)

-- | Get search results for a specific search history entry.
getSearchHistoryResults :: SQLite.Connection -> Int64 -> IO [SearchHistoryResultRecord]
getSearchHistoryResults conn historyId =
  queryRows conn
    "SELECT id, search_history_id, indexer_name, title, download_url, info_url, \
    \size_bytes, publish_date, seeders, peers, grabs, download_type, quality, score, rank \
    \FROM search_history_results WHERE search_history_id = ? ORDER BY rank ASC"
    (SQLite.Only historyId)

-- | Get the most recent search for an album.
getLatestSearchForAlbum :: SQLite.Connection -> Int64 -> IO (Maybe SearchHistoryRecord)
getLatestSearchForAlbum conn albumId = do
  results <- queryRows conn
    "SELECT id, catalog_album_id, searched_at, total_results, search_duration_ms, \
    \selected_release_title, selected_release_indexer, selected_release_score, outcome \
    \FROM search_history WHERE catalog_album_id = ? ORDER BY searched_at DESC LIMIT 1"
    (SQLite.Only albumId)
  pure $ listToMaybe results
