{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Scanner service - handles file system scanning.
module Skema.Services.Scanner
  ( startScannerService
  ) where

import Skema.Services.Dependencies (ScannerDeps(..))
import Skema.Events.Bus
import Skema.Events.Types
import Skema.Core.Library (FileSystemDiff(..), LibrarySnapshot(..))
import Skema.FileSystem.Scanner
import Monatone.Common (parseMetadata)
import Skema.Database.Connection
import Skema.Database.Repository (upsertTrackWithMetadata, getLibrarySnapshot, getTrackByPath, deleteTrack)
import qualified Skema.Database.Types as DBTypes
import qualified System.OsPath as OP
import Control.Concurrent.Async (Async, async, mapConcurrently)
import Control.Concurrent.QSem (QSem, newQSem, waitQSem, signalQSem)
import qualified Control.Concurrent.STM as STM
import Control.Exception (try, bracket_)
import GHC.Conc (getNumCapabilities)
import Katip

-- | Map a function over a list with bounded concurrency.
-- Uses a semaphore to limit the number of concurrent operations.
pooledMapConcurrently :: Int -> (a -> IO b) -> [a] -> IO [b]
pooledMapConcurrently maxWorkers f xs = do
  sem <- newQSem maxWorkers
  mapConcurrently (withSem sem . f) xs
  where
    withSem sem action = bracket_ (waitQSem sem) (signalQSem sem) action

-- | Start the scanner service.
--
-- This service listens for LIBRARY_SCAN_REQUESTED events and processes them.
-- Returns the async handle for graceful shutdown.
startScannerService :: ScannerDeps -> IO (Async ())
startScannerService deps = do
  chan <- STM.atomically $ subscribe (scanEventBus deps)
  async $ forever $ do
    envelope <- STM.atomically $ STM.readTChan chan
    case envelopeEvent envelope of
      LibraryScanRequested{..} -> do
        -- Process each scan request asynchronously with error handling
        _ <- async $ do
          result <- try $ handleScanRequest deps scanPath forceRescan
          case result of
            Left (e :: SomeException) -> do
              let le = scanLogEnv deps
              runKatipContextT le () "scanner.error" $ do
                $(logTM) ErrorS $ logStr $ ("Exception during scan: " <> show e :: Text)
            Right () -> pure ()
        pure ()
      _ -> pure ()  -- Ignore other events

-- | Handle a library scan request.
handleScanRequest :: ScannerDeps -> Text -> Bool -> IO ()
handleScanRequest ScannerDeps{..} scanPathText forceRescan = do
  let le = scanLogEnv
  let pool = scanDbPool
  let bus = scanEventBus
  let initialContext = ()
  let initialNamespace = "services.scanner"

  runKatipContextT le initialContext initialNamespace $ do
    if forceRescan
      then $(logTM) InfoS $ logStr ("Starting FORCE re-scan (treating all files as new)" :: Text)
      else $(logTM) InfoS $ logStr ("Starting file system scan" :: Text)

    -- Convert scan path to OsPath
    scanPath <- liftIO $ OP.encodeFS (toString scanPathText)

    -- Check if library is available
    libAvailable <- liftIO $ isLibraryAvailable scanPath
    if not libAvailable
      then do
        $(logTM) ErrorS $ logStr ("Library directory not available" :: Text)
      else do
        -- Load snapshot from database (or use empty snapshot if force rescan)
        dbSnapshot <- if forceRescan
          then do
            $(logTM) InfoS $ logStr ("Using empty snapshot for force re-scan" :: Text)
            pure $ LibrarySnapshot mempty  -- Empty snapshot means all files will be treated as added
          else liftIO $ withConnection pool getLibrarySnapshot

        -- Scan for changes
        diff <- liftIO $ scanDirectoryForChanges scanPath dbSnapshot

        let addedCount = length (diffAdded diff)
        let modifiedCount = length (diffModified diff)
        let deletedCount = length (diffDeleted diff)

        $(logTM) InfoS $ logStr $
          ("FileSystem changes: " <> show addedCount <> " added, " <>
           show modifiedCount <> " modified, " <> show deletedCount <> " deleted" :: Text)

        -- Emit FileSystemDiffGenerated event
        liftIO $ publishAndLog bus le "scanner" $ FileSystemDiffGenerated
          { filesAdded = addedCount
          , filesModified = modifiedCount
          , filesDeleted = deletedCount
          }

        -- Process deleted files
        when (deletedCount > 0) $ do
          $(logTM) InfoS $ logStr ("Removing deleted files from database..." :: Text)
          liftIO $ withConnection pool $ \conn -> do
            forM_ (diffDeleted diff) $ \path -> do
              -- Get track ID and delete
              maybeTrack <- getTrackByPath conn path
              forM_ maybeTrack $ \track ->
                forM_ (DBTypes.trackId track) $ \tid ->
                  deleteTrack conn tid

        -- Process added and modified files
        let filesToRead = diffAdded diff <> diffModified diff
        let totalToRead = length filesToRead

        -- Create counters for tracking progress (outside when block so we can read them later)
        progressCounter <- liftIO $ STM.newTVarIO (0 :: Int)
        errorCounter <- liftIO $ STM.newTVarIO (0 :: Int)

        when (totalToRead > 0) $ do
          -- Emit MetadataReadStarted event
          liftIO $ publishAndLog bus le "scanner" $ MetadataReadStarted
            { filesToRead = totalToRead
            }

          -- Get number of CPU cores for bounded concurrency
          maxWorkers <- liftIO getNumCapabilities
          $(logTM) InfoS $ logStr $ ("Processing " <> show totalToRead <> " files with " <> show maxWorkers <> " concurrent workers" :: Text)

          -- Process all files with bounded concurrency (no chunking needed)
          liftIO $ do
            _ <- pooledMapConcurrently maxWorkers (\path -> do
              -- Read metadata
              pathStr <- OP.decodeUtf path
              runKatipContextT le initialContext initialNamespace $ do
                $(logTM) DebugS $ logStr $ ("Reading metadata: " <> toText pathStr :: Text)

              metaResult <- parseMetadata path

              case metaResult of
                Left err -> do
                  -- Track error
                  STM.atomically $ STM.modifyTVar' errorCounter (+1)
                  runKatipContextT le initialContext initialNamespace $ do
                    $(logTM) WarningS $ logStr $ ("Failed to read metadata for " <> toText pathStr <> ": " <> show err :: Text)

                Right metadata -> do
                  -- Persist immediately after successful read
                  withConnection pool $ \conn -> do
                    _ <- upsertTrackWithMetadata conn path metadata
                    pure ()

              -- Update progress counter
              processed <- STM.atomically $ do
                STM.modifyTVar' progressCounter (+1)
                STM.readTVar progressCounter

              -- Emit progress event for every file (for frontend status bar)
              publishAndLog bus le "scanner" $ MetadataReadProgress
                { currentFile = toText pathStr
                , filesProcessed = processed
                , filesToRead = totalToRead
                }

              -- Log at INFO level every 10% of files
              let percentComplete = (processed * 100) `div` totalToRead
              let prevPercentComplete = ((processed - 1) * 100) `div` totalToRead
              when (percentComplete /= prevPercentComplete && percentComplete `mod` 10 == 0) $ do
                runKatipContextT le initialContext initialNamespace $ do
                  $(logTM) InfoS $ logStr $ ("Metadata read progress: " <> show percentComplete <> "% (" <> show processed <> "/" <> show totalToRead <> ")" :: Text)
              ) filesToRead  -- End of pooledMapConcurrently
            pure ()

          -- Get final error count
          parseErrors <- liftIO $ STM.readTVarIO errorCounter
          let successCount = totalToRead - parseErrors

          when (parseErrors > 0) $
            $(logTM) WarningS $ logStr $ (show parseErrors <> " files had metadata parsing errors" :: Text)

          $(logTM) InfoS $ logStr $ ("Successfully stored " <> show successCount <> " files in database" :: Text)

        -- Emit MetadataReadComplete event
        parseErrors <- liftIO $ STM.readTVarIO errorCounter
        liftIO $ publishAndLog bus le "scanner" $ MetadataReadComplete
          { filesProcessed = addedCount + modifiedCount
          , readErrors = parseErrors
          }
