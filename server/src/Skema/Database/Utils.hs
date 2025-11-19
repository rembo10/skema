{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions for database types and operations.
--
-- NOTE: This module now re-exports from Database.Types to eliminate
-- the circular dependency that previously required a .hs-boot file.
module Skema.Database.Utils
  ( sourceTypeToText
  , textToSourceType
  , albumStatusToText
  , textToAlbumStatus
  , downloadStatusToText
  , textToDownloadStatus
  , insertReturningId
  , insertReturningIdMaybe
  ) where

import Skema.Database.Types
  ( sourceTypeToText
  , textToSourceType
  , albumStatusToText
  , textToAlbumStatus
  , downloadStatusToText
  , textToDownloadStatus
  )
import Skema.Database.Connection (queryRows)
import Database.SQLite.Simple (Only(..), ToRow)
import qualified Database.SQLite.Simple as SQLite

-- * Database operation helpers

-- | Execute an INSERT query with RETURNING id and extract the returned ID.
-- This eliminates the repetitive pattern of querying for [Only Int64] and extracting the head.
-- Throws an error if the insert fails to return an ID.
insertReturningId
  :: ToRow q
  => SQLite.Connection  -- ^ Database connection
  -> Text               -- ^ SQL query (must end with RETURNING id)
  -> q                  -- ^ Query parameters
  -> IO Int64           -- ^ The returned ID
insertReturningId conn sql params = do
  results <- queryRows conn sql params :: IO [Only Int64]
  case viaNonEmpty head results of
    Just (Only idValue) -> pure idValue
    Nothing -> error $ toText $ "Failed to get ID from query: " <> toString sql

-- | Execute an INSERT query with RETURNING id and extract the returned ID as Maybe.
-- Returns Nothing if the insert returns no rows.
insertReturningIdMaybe
  :: ToRow q
  => SQLite.Connection  -- ^ Database connection
  -> Text               -- ^ SQL query (must end with RETURNING id)
  -> q                  -- ^ Query parameters
  -> IO (Maybe Int64)   -- ^ The returned ID, or Nothing
insertReturningIdMaybe conn sql params = do
  results <- queryRows conn sql params :: IO [Only Int64]
  pure $ fmap (\(Only idValue) -> idValue) (viaNonEmpty head results)
