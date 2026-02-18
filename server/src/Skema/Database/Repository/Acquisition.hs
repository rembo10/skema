{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Acquisition repository operations (acquisition rules).
module Skema.Database.Repository.Acquisition
  ( -- * Acquisition rule operations
    createAcquisitionRule
  , getAllAcquisitionRules
  , getEnabledAcquisitionRules
  , updateAcquisitionRule
  , deleteAcquisitionRule
  , getDefaultLibraryArtistsRule
  , getAcquisitionSummary
  ) where

import Skema.Database.Connection
import Skema.Database.Types
import Skema.Database.Utils (insertReturningId)
import qualified Skema.Database.Utils as Utils
import Database.SQLite.Simple (Only(..))
import qualified Database.SQLite.Simple as SQLite
import qualified Data.Map.Strict as Map

-- * Acquisition rule operations

-- | Create a new acquisition source (table still named acquisition_rules for backward compat).
createAcquisitionRule :: SQLite.Connection -> Text -> Maybe Text -> SourceType -> Bool -> Maybe Text -> IO Int64
createAcquisitionRule conn name description sType enabled filters =
  insertReturningId conn
    "INSERT INTO acquisition_rules (name, description, rule_type, enabled, filters) \
    \VALUES (?, ?, ?, ?, ?) RETURNING id"
    (name, description, Utils.sourceTypeToText sType, enabled, filters)

-- | Get all acquisition sources.
getAllAcquisitionRules :: SQLite.Connection -> IO [AcquisitionSourceRecord]
getAllAcquisitionRules conn =
  queryRows_ conn
    "SELECT id, name, description, rule_type, artist_mbid, enabled, priority, filters, created_at, updated_at \
    \FROM acquisition_rules ORDER BY created_at ASC"

-- | Get only enabled acquisition sources.
getEnabledAcquisitionRules :: SQLite.Connection -> IO [AcquisitionSourceRecord]
getEnabledAcquisitionRules conn =
  queryRows conn
    "SELECT id, name, description, rule_type, artist_mbid, enabled, priority, filters, created_at, updated_at \
    \FROM acquisition_rules WHERE enabled = ? ORDER BY created_at ASC"
    (Only True)

-- | Update an acquisition source.
updateAcquisitionRule :: SQLite.Connection -> Int64 -> Text -> Maybe Text -> SourceType -> Bool -> Maybe Text -> IO ()
updateAcquisitionRule conn sid name description sType enabled filters =
  executeQuery conn
    "UPDATE acquisition_rules SET name = ?, description = ?, rule_type = ?, enabled = ?, filters = ?, updated_at = CURRENT_TIMESTAMP \
    \WHERE id = ?"
    (name, description, Utils.sourceTypeToText sType, enabled, filters, sid)

-- | Delete an acquisition source.
deleteAcquisitionRule :: SQLite.Connection -> Int64 -> IO ()
deleteAcquisitionRule conn sid =
  executeQuery conn "DELETE FROM acquisition_rules WHERE id = ?" (Only sid)

-- | Get the default "library artists" source (if it exists).
getDefaultLibraryArtistsRule :: SQLite.Connection -> IO (Maybe AcquisitionSourceRecord)
getDefaultLibraryArtistsRule conn = do
  results <- queryRows conn
    "SELECT id, name, description, rule_type, artist_mbid, enabled, priority, filters, created_at, updated_at \
    \FROM acquisition_rules WHERE rule_type = ? ORDER BY created_at ASC LIMIT 1"
    (Only (Utils.sourceTypeToText LibraryArtists))
  pure $ viaNonEmpty head results

-- | Get acquisition summary: per-source artist/album counts and totals.
-- Returns (per-source stats, total followed artists, total wanted albums).
getAcquisitionSummary :: SQLite.Connection -> IO ([(Int64, Int, Int)], Int, Int)
getAcquisitionSummary conn = do
  -- Per-source artist counts
  artistCounts :: [(Int64, Int)] <- queryRows_ conn
    "SELECT added_by_rule_id, COUNT(*) \
    \FROM catalog_artists \
    \WHERE followed = 1 AND added_by_rule_id IS NOT NULL \
    \GROUP BY added_by_rule_id"

  -- Per-source album counts (albums whose artist was added by a given source)
  albumCounts :: [(Int64, Int)] <- queryRows_ conn
    "SELECT art.added_by_rule_id, COUNT(*) \
    \FROM catalog_albums alb \
    \JOIN catalog_artists art ON alb.artist_id = art.id \
    \WHERE art.followed = 1 AND art.added_by_rule_id IS NOT NULL \
    \  AND alb.quality_profile_id IS NOT NULL \
    \GROUP BY art.added_by_rule_id"

  -- Total followed artists
  [Only totalArtists] <- queryRows_ conn
    "SELECT COUNT(*) FROM catalog_artists WHERE followed = 1"

  -- Total wanted albums (quality_profile_id set and quality not yet at cutoff)
  [Only totalWanted] <- queryRows_ conn
    "SELECT COUNT(*) FROM catalog_albums ca \
    \LEFT JOIN quality_profiles qp ON ca.quality_profile_id = qp.id \
    \LEFT JOIN ( \
    \  SELECT mb_release_group_id, MIN(id) as id \
    \  FROM clusters \
    \  WHERE mb_release_group_id IS NOT NULL \
    \  GROUP BY mb_release_group_id \
    \) c ON ca.release_group_mbid = c.mb_release_group_id \
    \WHERE ca.quality_profile_id IS NOT NULL \
    \  AND CASE \
    \    WHEN c.id IS NULL THEN 1 \
    \    WHEN ca.current_quality IS NULL THEN 1 \
    \    WHEN qp.cutoff_quality IS NULL THEN 0 \
    \    ELSE CASE \
    \      WHEN ca.current_quality = 'FLAC' AND qp.cutoff_quality IN ('MP3', 'V0', 'FLAC') THEN 0 \
    \      WHEN ca.current_quality = 'V0' AND qp.cutoff_quality IN ('MP3', 'V0') THEN 0 \
    \      WHEN ca.current_quality = 'MP3' AND qp.cutoff_quality = 'MP3' THEN 0 \
    \      ELSE 1 \
    \    END \
    \  END = 1"

  -- Merge artist and album counts per source
  let albumCountMap = Map.fromList albumCounts
      perSource = map (\(sid, ac) ->
        (sid, ac, fromMaybe 0 (Map.lookup sid albumCountMap))
        ) artistCounts

  pure (perSource, totalArtists, totalWanted)
