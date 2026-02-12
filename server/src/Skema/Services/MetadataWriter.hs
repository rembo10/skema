{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | MetadataWriter service - writes metadata changes to audio files.
--
-- This service listens for MetadataWriteRequested events and performs
-- the actual file writes asynchronously, emitting progress events.
module Skema.Services.MetadataWriter
  ( startMetadataWriterService
  ) where

import Skema.Services.Dependencies (MetadataWriterDeps(..))
import Skema.Events.Bus
import Skema.Events.Types
import Skema.Database.Connection
import qualified Skema.Database.Repository as DB
import qualified Skema.Database.Types as DBTypes
import Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (readTChan)
import Control.Exception (try)
import Katip
import Database.SQLite.Simple (Only(..))
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

-- | Start the metadata writer service.
--
-- This service listens for MetadataWriteRequested events and writes
-- metadata changes to audio files asynchronously.
startMetadataWriterService :: MetadataWriterDeps -> IO (Async ())
startMetadataWriterService deps = do
  chan <- STM.atomically $ subscribe (writerEventBus deps)

  async $ forever $ do
    envelope <- STM.atomically $ readTChan chan
    case envelopeEvent envelope of
      MetadataWriteRequested{..} -> do
        result <- try $ processMetadataWrite deps writeDiffIds
        case result of
          Left (e :: SomeException) -> do
            let le = writerLogEnv deps
            runKatipContextT le () "writer.error" $ do
              $(logTM) ErrorS $ logStr $ ("Exception in metadata writer: " <> show e :: Text)

            -- Emit failure event
            let bus = writerEventBus deps
            publishAndLog bus le "writer" $ MetadataWriteFailed
              { writeErrorMessage = show e
              }
          Right () -> pure ()
      _ -> pure ()  -- Ignore other events

-- | Process a metadata write request.
-- Groups diffs by file and processes them in batches, emitting progress events as each file is written.
processMetadataWrite :: MetadataWriterDeps -> [Int64] -> IO ()
processMetadataWrite MetadataWriterDeps{..} diffIds = do
  let le = writerLogEnv
  let pool = writerDbPool
  let bus = writerEventBus
  let initialContext = ()
  let initialNamespace = "services.metadata-writer"

  runKatipContextT le initialContext initialNamespace $ do
    $(logTM) InfoS $ logStr $ ("Processing metadata write request for " <> show (length diffIds) <> " diffs" :: Text)

    -- Emit started event
    liftIO $ publishAndLog bus le "writer" $ MetadataWriteStarted
      { writeTotalChanges = length diffIds
      }

    -- Group diffs by file (track_id)
    diffsByFile <- liftIO $ groupDiffsByFile pool diffIds

    -- Process each file's diffs as a batch
    let fileGroups = Map.toList diffsByFile
    results <- liftIO $ forM (zip [1..] fileGroups) $ \(idx :: Int, (_trackId, (filePath, fileDiffIds))) -> do
      -- Emit progress event BEFORE writing
      publishAndLog bus le "writer" $ MetadataWriteProgress
        { writeCurrentFile = toText filePath
        , writeChangesProcessed = sum [length diffs | (_, (_, diffs)) <- take (idx - 1) fileGroups] + 1
        , writeTotalChanges = length diffIds
        }

      -- Apply all diffs for this file at once
      result <- DB.applyMetadataChanges pool fileDiffIds
      case result of
        Left err -> pure $ Left err
        Right changes -> do
          -- Emit applied event for each change
          forM_ changes $ \change ->
            case DBTypes.changeId change of
              Just cid -> publishAndLog bus le "writer" $ MetadataDiffApplied
                { diffId = cid
                }
              Nothing -> pure ()
          pure $ Right changes

    -- Collect results
    let (errors, successLists) = partitionEithers results

    if not (null errors) then do
      let errorMsg = T.intercalate "; " errors
      $(logTM) ErrorS $ logStr $ ("Failed to apply some metadata changes: " <> errorMsg :: Text)
      liftIO $ publishAndLog bus le "writer" $ MetadataWriteFailed
        { writeErrorMessage = errorMsg
        }
    else do
      let totalChanges = sum (length <$> successLists)
      $(logTM) InfoS $ logStr $ ("Successfully applied " <> show totalChanges <> " metadata changes" :: Text)

      -- Emit completed event
      liftIO $ publishAndLog bus le "writer" $ MetadataWriteCompleted
        { writeChangesApplied = totalChanges
        , writeErrors = 0
        }

-- | Group diffs by their file (track_id) for batched processing.
-- Returns a map from track_id to (file_path, [diff_ids])
groupDiffsByFile :: ConnectionPool -> [Int64] -> IO (Map.Map Int64 (String, [Int64]))
groupDiffsByFile pool diffIds = withConnection pool $ \conn -> do
  -- Get all diff info with track paths
  diffInfo <- forM diffIds $ \diffId -> do
    results <- queryRows conn
      "SELECT d.id, d.track_id, t.path \
      \FROM metadata_diffs d \
      \JOIN library_tracks t ON d.track_id = t.id \
      \WHERE d.id = ?"
      (Only diffId) :: IO [(Int64, Int64, String)]
    pure $ viaNonEmpty head results

  -- Group by track_id, collecting both the path and diff IDs
  let validDiffs = catMaybes diffInfo
  let grouped = Map.fromListWith (\(p1, ds1) (_, ds2) -> (p1, ds1 <> ds2))
                  [(trackId, (path, [diffId])) | (diffId, trackId, path) <- validDiffs]

  pure grouped
