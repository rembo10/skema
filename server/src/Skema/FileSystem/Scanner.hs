{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | File system scanning and metadata reading.
--
-- This module handles scanning directories for audio files and reading
-- their metadata using the monatone library.
module Skema.FileSystem.Scanner
  ( -- * Scanning
    scanDirectory
  , scanDirectoryForChanges
  , fullScan
  , fullScanWithLogging
  , isLibraryAvailable
    -- * Results
  , ScanResult (..)
  ) where

import Skema.Core.Library
import Skema.FileSystem.Utils (osPathToString)
import Monatone.Common (parseMetadata)
import Monatone.Metadata (Metadata)
import System.OsPath (OsPath, (</>))
import System.Directory.OsPath (listDirectory, doesDirectoryExist, getModificationTime, getFileSize)
import qualified Data.Map.Strict as Map
import Control.Concurrent.Async (forConcurrently)
import Data.List (isSuffixOf)
import Control.Monad (foldM)
import Katip

-- | Check if a library directory is available.
--
-- Returns False if the directory doesn't exist (e.g., network mount is down).
isLibraryAvailable :: OsPath -> IO Bool
isLibraryAvailable = doesDirectoryExist

-- | Scan a directory and create a snapshot of audio files.
--
-- This only reads basic file information (modification time, size),
-- not full metadata. Use 'fullScan' to also read metadata.
--
-- Returns an empty snapshot if the directory doesn't exist (e.g., network mount unavailable).
-- Use 'isLibraryAvailable' to check if a directory exists before scanning.
scanDirectory :: OsPath -> IO LibrarySnapshot
scanDirectory root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure emptySnapshot
    else do
      audioFiles <- listAudioFilesRecursive root
      fileInfos <- traverse getFileInfo audioFiles
      pure $ LibrarySnapshot (Map.fromList $ zip audioFiles fileInfos)

-- | Scan a directory and compute diff against a previous snapshot.
--
-- This compares the filesystem state with a previous snapshot (typically from
-- the database) and returns a FileSystemDiff showing what changed.
--
-- This is more efficient than fullScan when you only need to process changes,
-- as it doesn't read metadata for unchanged files.
--
-- Example workflow:
-- @
-- oldSnapshot <- getLibrarySnapshot dbConn  -- Load from database
-- diff <- scanDirectoryForChanges root oldSnapshot
-- -- Process only the added/modified files in the diff
-- @
scanDirectoryForChanges :: OsPath -> LibrarySnapshot -> IO FileSystemDiff
scanDirectoryForChanges root previousSnapshot = do
  currentSnapshot <- scanDirectory root
  pure $ computeDiff previousSnapshot currentSnapshot

-- | List all audio files in a directory recursively.
listAudioFilesRecursive :: OsPath -> IO [OsPath]
listAudioFilesRecursive root = go root
  where
    go dir = do
      entries <- listDirectory dir
      let fullPaths = map (dir </>) entries

      (dirs, files) <- partitionM doesDirectoryExist fullPaths

      audioFiles <- filterM isAudioFile files
      subDirFiles <- concat <$> traverse go dirs

      pure $ audioFiles <> subDirFiles

-- | Check if a path is an audio file based on extension.
isAudioFile :: OsPath -> IO Bool
isAudioFile path = do
  pathStr <- osPathToString path
  pure $ any (`isSuffixOf` pathStr) audioExtensions
  where
    audioExtensions = [".mp3", ".flac", ".m4a", ".aac", ".alac", ".ogg", ".opus"]

-- | Get basic file information for change detection.
getFileInfo :: OsPath -> IO FileInfo
getFileInfo path = do
  modTime <- getModificationTime path
  size <- getFileSize path
  pure $ FileInfo modTime size

-- | Result of a full scan including metadata.
data ScanResult = ScanResult
  { scanSnapshot :: LibrarySnapshot
    -- ^ Basic file snapshot
  , scanMetadata :: Map.Map OsPath Metadata
    -- ^ Successfully parsed metadata
  , scanErrors :: Map.Map OsPath Text
    -- ^ Files that failed to parse with error messages
  , scanLibraryAvailable :: Bool
    -- ^ Whether the library directory was available during scan
  } deriving (Show)

-- | Perform a full scan: snapshot + metadata for all audio files.
--
-- Metadata reading happens concurrently for better performance.
-- Files that fail to parse are captured in 'scanErrors'.
-- If the library directory doesn't exist, returns an empty result with
-- 'scanLibraryAvailable' set to False.
fullScan :: OsPath -> IO ScanResult
fullScan root = fullScanWithLogging root Nothing

-- | Perform a full scan with optional DEBUG logging for each file.
--
-- This is the same as 'fullScan' but with optional logging support.
-- When a log environment is provided, each file scanned will be logged
-- at DEBUG level.
fullScanWithLogging :: OsPath -> Maybe LogEnv -> IO ScanResult
fullScanWithLogging root mLogEnv = do
  exists <- doesDirectoryExist root

  if not exists
    then pure $ ScanResult
      { scanSnapshot = emptySnapshot
      , scanMetadata = Map.empty
      , scanErrors = Map.empty
      , scanLibraryAvailable = False
      }
    else do
      snapshot <- scanDirectory root

      let paths = Map.keys (snapshotFiles snapshot)

      -- Read metadata concurrently
      results <- forConcurrently paths $ \path -> do
        -- Log at DEBUG level if log environment provided
        forM_ mLogEnv $ \le -> do
          pathStr <- osPathToString path
          let initialContext = ()
          let initialNamespace = "scanner"
          runKatipContextT le initialContext initialNamespace $ do
            $(logTM) DebugS $ logStr $ ("Scanning file: " <> toText pathStr :: Text)

        meta <- parseMetadata path
        pure (path, meta)

      -- Partition successes and failures
      let (errors, metadata) = partitionEithers
            [ case result of
                Left err -> Left (path, toText (show err :: String))
                Right meta -> Right (path, meta)
            | (path, result) <- results
            ]

      pure $ ScanResult
        { scanSnapshot = snapshot
        , scanMetadata = fromList metadata
        , scanErrors = fromList errors
        , scanLibraryAvailable = True
        }

-- | Helper to partition with a monadic predicate.
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f xs = do
  (ts, fs) <- foldM go ([], []) xs
  pure (reverse ts, reverse fs)
  where
    go (ts, fs) x = do
      test <- f x
      pure $ if test then (x:ts, fs) else (ts, x:fs)
