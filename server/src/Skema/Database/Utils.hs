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
  , qualityRankSql
  , albumWantedSql
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
import Skema.Domain.Quality (Quality, qualityToText)
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

-- * Quality SQL helpers

-- | SQL expression mapping a quality text column to its intrinsic rank,
-- generated from the Quality enum in Skema.Domain.Quality so the SQL and
-- Haskell orderings cannot drift. Unrecognized values rank below Unknown,
-- so they always count as upgradeable.
qualityRankSql :: Text -> Text
qualityRankSql col =
  "CASE " <> col <> " "
    <> mconcat
        [ "WHEN '" <> qualityToText q <> "' THEN " <> show (fromEnum q) <> " "
        | q <- [minBound .. maxBound] :: [Quality]
        ]
    <> "ELSE -1 END"

-- | SQL CASE computing whether an album is wanted (1) or not (0).
-- Expects the aliases ca (catalog_albums), qp (quality_profiles), and
-- c (the album's cluster) to be in scope. An album is wanted when it has
-- a quality profile and either no cluster on disk or a cluster whose
-- quality is below the profile's cutoff.
albumWantedSql :: Text
albumWantedSql =
  "CASE \
  \WHEN ca.quality_profile_id IS NULL THEN 0 \
  \WHEN c.quality IS NULL THEN 1 \
  \WHEN qp.cutoff_quality IS NULL THEN 0 \
  \WHEN (" <> qualityRankSql "c.quality" <> ") >= (" <> qualityRankSql "qp.cutoff_quality" <> ") THEN 0 \
  \ELSE 1 END"
