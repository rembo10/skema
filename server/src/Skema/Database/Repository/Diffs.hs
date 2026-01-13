{-# LANGUAGE OverloadedStrings #-}

-- | Metadata diff and change repository operations.
module Skema.Database.Repository.Diffs
  ( -- * Metadata diff operations
    insertMetadataDiff
  , getMetadataDiffsForTrack
  , deleteMetadataDiffsForTrack
  , getAllMetadataDiffs
  , applyMetadataChange
  , applyGroupedMetadataDiff
  , computeMetadataDiffs
  , updateMatchInDatabase
    -- * Metadata change operations (for undo)
  , applyMetadataChanges
  , getMetadataChanges
  , revertMetadataChange
  , applyFieldToUpdate
  ) where

import Skema.Database.Connection
import Skema.Database.Types
import Skema.Database.Utils (insertReturningIdMaybe)
import Skema.Database.Repository.Tracks (stringToOsPath)
import Skema.MusicBrainz.Types (ReleaseMatch(..), TrackMatch(..), MBRelease(..), MBTrack(..), MBID(..), unMBID, FileGroup(..))
import Monatone.Metadata (Metadata(..))
import qualified Monatone.Metadata as M
import qualified Monatone.Writer as MW
import Control.Monad.Except ()
import qualified Control.Monad.Except as Except
import System.OsPath (OsPath)
import qualified System.OsPath as OP
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple (Only(..))
import qualified Database.SQLite.Simple as SQLite
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

-- * Metadata diff operations

-- | Insert a metadata diff record.
insertMetadataDiff :: SQLite.Connection -> Int64 -> Text -> Maybe Text -> Maybe Text -> IO ()
insertMetadataDiff conn tid fieldName fileValue mbValue =
  executeQuery conn
    "INSERT INTO metadata_diffs (track_id, field_name, file_value, mb_value) VALUES (?, ?, ?, ?)"
    (tid, fieldName, fileValue, mbValue)

-- | Get all metadata diffs for a track.
getMetadataDiffsForTrack :: SQLite.Connection -> Int64 -> IO [MetadataDiffRecord]
getMetadataDiffsForTrack conn tid =
  queryRows conn
    "SELECT id, track_id, field_name, file_value, mb_value, created_at FROM metadata_diffs WHERE track_id = ? ORDER BY field_name"
    (Only tid)

-- | Delete all metadata diffs for a track (e.g., when re-matching).
deleteMetadataDiffsForTrack :: SQLite.Connection -> Int64 -> IO ()
deleteMetadataDiffsForTrack conn tid =
  executeQuery conn "DELETE FROM metadata_diffs WHERE track_id = ?" (Only tid)

-- | Get all metadata diffs with track paths.
getAllMetadataDiffs :: SQLite.Connection -> IO [(MetadataDiffRecord, OsPath)]
getAllMetadataDiffs conn = do
  results <- queryRows_ conn
    "SELECT d.id, d.track_id, d.field_name, d.file_value, d.mb_value, d.created_at, t.path \
    \FROM metadata_diffs d \
    \JOIN library_tracks t ON d.track_id = t.id \
    \ORDER BY d.field_name, d.file_value, d.mb_value" :: IO [(Int64, Int64, Text, Maybe Text, Maybe Text, UTCTime, String)]

  forM results $ \(did, tid, fieldName, fileValue, mbValue, createdAt, pathStr) -> do
    path <- stringToOsPath pathStr
    let diff = MetadataDiffRecord
          { diffId = Just did
          , diffTrackId = tid
          , diffFieldName = fieldName
          , diffFileValue = fileValue
          , diffMBValue = mbValue
          , diffCreatedAt = Just createdAt
          }
    pure (diff, path)

-- | Apply a metadata change to a track.
applyMetadataChange :: SQLite.Connection -> Int64 -> Text -> Maybe Text -> IO ()
applyMetadataChange conn tid fieldName newValue = do
  -- Update the metadata field
  let updateSql = "UPDATE library_track_metadata SET " <> fieldName <> " = ?, updated_at = CURRENT_TIMESTAMP WHERE track_id = ?"
  executeQuery conn updateSql (newValue, tid)

  -- Delete the diff record
  executeQuery conn
    "DELETE FROM metadata_diffs WHERE track_id = ? AND field_name = ?"
    (tid, fieldName)

-- | Apply a grouped metadata diff to all matching tracks.
applyGroupedMetadataDiff :: SQLite.Connection -> Text -> Maybe Text -> Maybe Text -> IO Int
applyGroupedMetadataDiff conn fieldName fileValue mbValue = do
  -- Find all diffs matching this group
  diffs <- queryRows conn
    "SELECT track_id FROM metadata_diffs WHERE field_name = ? AND file_value IS ? AND mb_value IS ?"
    (fieldName, fileValue, mbValue) :: IO [Only Int64]

  -- Apply to each track
  forM_ diffs $ \(Only tid) -> do
    applyMetadataChange conn tid fieldName mbValue

  pure $ length diffs

-- | Compute metadata differences between file and MusicBrainz data.
computeMetadataDiffs :: Metadata -> MBTrack -> MBRelease -> [(Text, Maybe Text, Maybe Text)]
computeMetadataDiffs fileMeta mbTrack mbRelease =
  let
    -- Helper to normalize empty strings to Nothing
    -- This ensures that "" and Nothing are treated as equivalent (both empty)
    normalizeEmpty :: Maybe Text -> Maybe Text
    normalizeEmpty Nothing = Nothing
    normalizeEmpty (Just "") = Nothing
    normalizeEmpty (Just t) = Just (T.strip t)  -- Also strip whitespace

    -- Helper to compare values and create diff if different
    -- Skip if both values are Nothing (empty-to-empty should not create a diff)
    mkDiff :: Eq a => Text -> Maybe a -> Maybe a -> (a -> Text) -> Maybe (Text, Maybe Text, Maybe Text)
    mkDiff fieldName fileVal mbVal showFn =
      case (fileVal, mbVal) of
        (Nothing, Nothing) -> Nothing  -- Both empty, no diff
        _ | fileVal == mbVal -> Nothing  -- Same value, no diff
        _ -> Just (fieldName, fmap showFn fileVal, fmap showFn mbVal)  -- Different, create diff

    -- Version of mkDiff for Text fields that normalizes empty strings
    mkTextDiff :: Text -> Maybe Text -> Maybe Text -> Maybe (Text, Maybe Text, Maybe Text)
    mkTextDiff fieldName fileVal mbVal =
      let fileVal' = normalizeEmpty fileVal
          mbVal' = normalizeEmpty mbVal
      in case (fileVal', mbVal') of
        (Nothing, Nothing) -> Nothing  -- Both empty, no diff
        _ | fileVal' == mbVal' -> Nothing  -- Same value, no diff
        _ -> Just (fieldName, fileVal', mbVal')  -- Different, create diff

    -- Track-level comparisons
    titleDiff = mkTextDiff "title" (M.title fileMeta) (Just $ mbTrackTitle mbTrack)
    trackNumDiff = mkDiff "track_number" (M.trackNumber fileMeta) (Just $ mbTrackPosition mbTrack) show
    trackArtistDiff = mkTextDiff "artist" (M.artist fileMeta) (mbTrackArtist mbTrack)

    -- Release-level comparisons
    albumDiff = mkTextDiff "album" (M.album fileMeta) (Just $ mbReleaseTitle mbRelease)
    albumArtistDiff = mkTextDiff "album_artist" (M.albumArtist fileMeta) (Just $ mbReleaseArtist mbRelease)
    dateDiff = mkTextDiff "date" (M.date fileMeta) (mbReleaseDate mbRelease)
    yearDiff = mkDiff "year" (M.year fileMeta) (mbReleaseYear mbRelease) show
    countryDiff = mkTextDiff "country" (M.releaseCountry fileMeta) (mbReleaseCountry mbRelease)
    labelDiff = mkTextDiff "label" (M.recordLabel fileMeta) (mbReleaseLabel mbRelease)
    catalogDiff = mkTextDiff "catalog_number" (M.catalogNumber fileMeta) (mbReleaseCatalogNumber mbRelease)
    barcodeDiff = mkTextDiff "barcode" (M.barcode fileMeta) (mbReleaseBarcode mbRelease)
    -- Genre comparison (MusicBrainz provides a list, join with semicolons for comparison)
    genreDiff = mkTextDiff "genre" (M.genre fileMeta)
      (if null (mbReleaseGenres mbRelease) then Nothing else Just (T.intercalate "; " (mbReleaseGenres mbRelease)))

  in catMaybes
    [ titleDiff
    , trackNumDiff
    , trackArtistDiff
    , albumDiff
    , albumArtistDiff
    , dateDiff
    , yearDiff
    , countryDiff
    , labelDiff
    , catalogDiff
    , barcodeDiff
    , genreDiff
    ]

-- | Update database with matched MusicBrainz IDs from a ReleaseMatch.
-- Also computes and stores metadata differences.
updateMatchInDatabase :: ConnectionPool -> ReleaseMatch -> IO Int
updateMatchInDatabase pool match = do
  let trackMatches = rmTrackMatches match
      release = rmRelease match
      releaseId = unMBID (mbReleaseId release)
      releaseGroupId = mbReleaseGroupId release >>= Just . unMBID
      fileGroup = rmFileGroup match

  withConnection pool $ \conn -> do
    forM_ trackMatches $ \tm -> do
      let filePath = tmFilePath tm
          track = tmTrack tm
          mbRecordingIdText = unMBID (mbTrackRecordingId track)
          confidence = tmConfidence tm

      -- Update MusicBrainz IDs
      pathStr <- OP.decodeUtf filePath
      executeQuery conn
        "UPDATE library_tracks SET \
        \mb_recording_id = ?, mb_track_id = ?, mb_release_id = ?, \
        \mb_release_group_id = ?, mb_artist_id = ?, mb_confidence = ?, \
        \updated_at = CURRENT_TIMESTAMP \
        \WHERE path = ?"
        ( Just mbRecordingIdText :: Maybe Text
        , Nothing :: Maybe Text  -- Track ID (not available from current data)
        , Just releaseId
        , releaseGroupId
        , Nothing :: Maybe Text  -- Artist ID (not available from current data)
        , Just confidence
        , pathStr :: String
        )

      -- Get track ID for storing diffs
      results <- queryRows conn
        "SELECT id, path, size, modified_at, cluster_id, mb_recording_id, mb_track_id, mb_release_id, mb_confidence, added_at, updated_at FROM library_tracks WHERE path = ?"
        (Only (pathStr :: String))
      let mTrackRecord = viaNonEmpty head results

      case mTrackRecord of
        Just trackRecord -> case trackId trackRecord of
          Just tid -> do
            -- Find the metadata for this track in the file group
            case find (\(p, _) -> p == filePath) (fgFiles fileGroup) of
              Just (_, fileMeta) -> do
                -- Compute metadata diffs
                let diffs = computeMetadataDiffs fileMeta track release

                -- Delete old diffs for this track
                deleteMetadataDiffsForTrack conn tid

                -- Insert new diffs
                forM_ diffs $ \(fieldName, fileVal, mbVal) ->
                  insertMetadataDiff conn tid fieldName fileVal mbVal

              Nothing -> pure ()  -- Track not in group (shouldn't happen)
          Nothing -> pure ()  -- Track has no ID (shouldn't happen)
        Nothing -> pure ()  -- Track not found (shouldn't happen)

    pure $ length trackMatches

-- * Metadata change operations (for undo)

-- | Apply a single metadata field update to a Monatone update.
-- Maps field names to their corresponding Monatone writer functions.
applyFieldToUpdate :: Text -> Maybe Text -> MW.MetadataUpdate -> MW.MetadataUpdate
applyFieldToUpdate fieldName maybeValue update = case fieldName of
  "title" -> maybe update (\v -> MW.setTitle v update) maybeValue
  "artist" -> maybe update (\v -> MW.setArtist v update) maybeValue
  "track_artist" -> maybe update (\v -> MW.setArtist v update) maybeValue
  "album" -> maybe update (\v -> MW.setAlbum v update) maybeValue
  "album_artist" -> maybe update (\v -> MW.setAlbumArtist v update) maybeValue
  "track_number" -> maybe update (\v -> maybe update (\i -> MW.setTrackNumber i update) (readMaybe (toString v))) maybeValue
  "disc_number" -> maybe update (\v -> maybe update (\i -> MW.setDiscNumber i update) (readMaybe (toString v))) maybeValue
  "year" -> maybe update (\v -> maybe update (\i -> MW.setYear i update) (readMaybe (toString v))) maybeValue
  "date" -> maybe update (\v -> MW.setDate v update) maybeValue
  "genre" -> maybe update (\v -> MW.setGenre v update) maybeValue
  "barcode" -> maybe update (\v -> MW.setBarcode v update) maybeValue
  "catalog_number" -> maybe update (\v -> MW.setCatalogNumber v update) maybeValue
  "label" -> maybe update (\v -> MW.setLabel v update) maybeValue
  "country" -> maybe update (\v -> MW.setReleaseCountry v update) maybeValue
  _ -> update  -- Unsupported field, skip

-- | Apply metadata changes by writing diffs to actual audio files using Monatone.
-- Creates change history records for undo functionality.
-- Returns Either an error message or list of created change records.
applyMetadataChanges :: ConnectionPool -> [Int64] -> IO (Either Text [MetadataChangeRecord])
applyMetadataChanges pool diffIds = withConnection pool $ \conn -> do
  -- Get all diffs with track paths
  diffs <- forM diffIds $ \did -> do
    results <- queryRows conn
      "SELECT d.id, d.track_id, d.field_name, d.file_value, d.mb_value, d.created_at, t.path \
      \FROM metadata_diffs d \
      \JOIN library_tracks t ON d.track_id = t.id \
      \WHERE d.id = ?"
      (Only did) :: IO [(Int64, Int64, Text, Maybe Text, Maybe Text, UTCTime, String)]
    pure $ viaNonEmpty head results

  let validDiffs = catMaybes diffs

  if null validDiffs
    then pure $ Left "No valid diffs found"
    else do
      -- Group diffs by track to batch write operations
      let diffsByTrack = Map.fromListWith (<>)
            [(tid, [(fieldName, oldVal, newVal)]) | (_, tid, fieldName, oldVal, newVal, _, _) <- validDiffs]

      results <- forM (Map.toList diffsByTrack) $ \(tid, trackDiffs) -> do
        -- Get track path
        trackResult <- queryRows conn
          "SELECT path FROM library_tracks WHERE id = ?"
          (Only tid) :: IO [Only String]

        case viaNonEmpty head trackResult of
          Nothing -> pure $ Left $ "Track not found for ID: " <> show tid
          Just (Only pathStr) -> do
            path <- OP.encodeFS pathStr

            -- Build Monatone update using helper function
            let buildUpdate = foldl' (\acc (field, _, newVal) ->
                  applyFieldToUpdate field newVal acc
                  ) MW.emptyUpdate trackDiffs

            -- Apply update to file using Monatone
            writeResult <- Except.runExceptT $ MW.updateMetadata path buildUpdate

            case writeResult of
              Left err -> pure $ Left $ "Failed to write metadata: " <> show err
              Right () -> do
                -- Update database and create change history
                changes <- forM trackDiffs $ \(fieldName, oldValue, newValue) -> do
                  -- Update metadata in database
                  applyMetadataChange conn tid fieldName newValue

                  -- Create change history record with RETURNING clause
                  now <- getCurrentTime
                  cid <- insertReturningIdMaybe conn
                    "INSERT INTO metadata_change_history (track_id, field_name, old_value, new_value, applied_at) \
                    \VALUES (?, ?, ?, ?, ?) RETURNING id"
                    (tid, fieldName, oldValue, newValue, now)

                  pure $ MetadataChangeRecord
                    { changeId = cid
                    , changeTrackId = tid
                    , changeFieldName = fieldName
                    , changeOldValue = oldValue
                    , changeNewValue = newValue
                    , changeAppliedAt = Just now
                    , changeRevertedAt = Nothing
                    }

                pure $ Right changes

      -- Collect results
      let (errors, successLists) = partitionEithers results
      if not (null errors)
        then pure $ Left $ T.intercalate "; " errors
        else pure $ Right $ concat successLists

-- | Get metadata change history.
-- If activeOnly is True, only return non-reverted changes.
getMetadataChanges :: SQLite.Connection -> Bool -> IO [(MetadataChangeRecord, OsPath)]
getMetadataChanges conn activeOnly = do
  let sql = if activeOnly
        then "SELECT h.id, h.track_id, h.field_name, h.old_value, h.new_value, h.applied_at, h.reverted_at, t.path \
             \FROM metadata_change_history h \
             \JOIN library_tracks t ON h.track_id = t.id \
             \WHERE h.reverted_at IS NULL \
             \ORDER BY h.applied_at DESC"
        else "SELECT h.id, h.track_id, h.field_name, h.old_value, h.new_value, h.applied_at, h.reverted_at, t.path \
             \FROM metadata_change_history h \
             \JOIN library_tracks t ON h.track_id = t.id \
             \ORDER BY h.applied_at DESC"

  results <- queryRows_ conn sql :: IO [(Int64, Int64, Text, Maybe Text, Maybe Text, UTCTime, Maybe UTCTime, String)]

  forM results $ \(cid, tid, fieldName, oldValue, newValue, appliedAt, revertedAt, pathStr) -> do
    path <- stringToOsPath pathStr
    let change = MetadataChangeRecord
          { changeId = Just cid
          , changeTrackId = tid
          , changeFieldName = fieldName
          , changeOldValue = oldValue
          , changeNewValue = newValue
          , changeAppliedAt = Just appliedAt
          , changeRevertedAt = revertedAt
          }
    pure (change, path)

-- | Revert a metadata change by writing the old value back to the file.
revertMetadataChange :: ConnectionPool -> Int64 -> IO (Either Text ())
revertMetadataChange pool cid = withConnection pool $ \conn -> do
  -- Get the change record
  results <- queryRows conn
    "SELECT h.id, h.track_id, h.field_name, h.old_value, h.new_value, h.applied_at, h.reverted_at, t.path \
    \FROM metadata_change_history h \
    \JOIN library_tracks t ON h.track_id = t.id \
    \WHERE h.id = ?"
    (Only cid) :: IO [(Int64, Int64, Text, Maybe Text, Maybe Text, UTCTime, Maybe UTCTime, String)]

  case viaNonEmpty head results of
    Nothing -> pure $ Left "Change not found"
    Just (_, tid, fieldName, oldValue, _, _, revertedAt, pathStr) -> do
      -- Check if already reverted
      if isJust revertedAt
        then pure $ Left "Change already reverted"
        else do
          path <- OP.encodeFS pathStr

          -- Build Monatone update with old value using helper function
          let buildUpdate = applyFieldToUpdate fieldName oldValue MW.emptyUpdate

          -- Write old value back to file
          writeResult <- Except.runExceptT $ MW.updateMetadata path buildUpdate

          case writeResult of
            Left err -> pure $ Left $ "Failed to revert metadata: " <> show err
            Right () -> do
              -- Update database with old value
              let updateSql = "UPDATE library_track_metadata SET " <> fieldName <> " = ?, updated_at = CURRENT_TIMESTAMP WHERE track_id = ?"
              executeQuery conn updateSql (oldValue, tid)

              -- Mark change as reverted
              now <- getCurrentTime
              executeQuery conn
                "UPDATE metadata_change_history SET reverted_at = ? WHERE id = ?"
                (now, cid)

              pure $ Right ()
