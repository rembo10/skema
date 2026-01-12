{-# LANGUAGE OverloadedStrings #-}

-- | Acquisition repository operations (acquisition rules).
module Skema.Database.Repository.Acquisition
  ( -- * Acquisition rule operations
    createAcquisitionRule
  , getAllAcquisitionRules
  , getEnabledAcquisitionRules
  , updateAcquisitionRule
  , deleteAcquisitionRule
  , getDefaultLibraryArtistsRule
  ) where

import Skema.Database.Connection
import Skema.Database.Types
import Skema.Database.Utils (insertReturningId)
import qualified Skema.Database.Utils as Utils
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (Only(..))
import qualified Database.SQLite.Simple as SQLite

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
