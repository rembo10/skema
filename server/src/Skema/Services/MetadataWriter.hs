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
-- Processes diffs one at a time, emitting progress events as each file is written.
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

    -- Process diffs one at a time, emitting progress as we go
    results <- liftIO $ forM (zip [1..] diffIds) $ \(idx :: Int, diffId) -> do
      -- Get diff info with track path
      diffInfo <- withConnection pool $ \conn ->
        queryRows conn
          "SELECT d.id, d.track_id, d.field_name, d.file_value, d.mb_value, t.path \
          \FROM metadata_diffs d \
          \JOIN library_tracks t ON d.track_id = t.id \
          \WHERE d.id = ?"
          (Only diffId) :: IO [(Int64, Int64, Text, Maybe Text, Maybe Text, String)]

      case viaNonEmpty head diffInfo of
        Nothing -> pure $ Left ("Diff not found: " <> show diffId)
        Just (_, _trackId, _fieldName, _oldValue, _newValue, pathStr) -> do
          -- Emit progress event BEFORE writing
          publishAndLog bus le "writer" $ MetadataWriteProgress
            { writeCurrentFile = toText pathStr
            , writeChangesProcessed = idx
            , writeTotalChanges = length diffIds
            }

          -- Apply this single diff
          result <- DB.applyMetadataChanges pool [diffId]
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
