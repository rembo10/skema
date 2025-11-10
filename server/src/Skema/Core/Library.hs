{-# LANGUAGE DeriveGeneric #-}

-- | Core library types and pure business logic for file system diffing.
--
-- This module contains pure functions with no IO, making them easy to test
-- and reason about. All file system interaction happens in the FileSystem
-- adapters.
module Skema.Core.Library
  ( -- * Types
    LibrarySnapshot (..)
  , FileInfo (..)
  , FileSystemDiff (..)
    -- * Pure logic
  , computeDiff
  , emptySnapshot
  , emptyDiff
  , isEmpty
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (UTCTime)
import System.OsPath (OsPath)

-- | A snapshot of the library's file system state at a point in time.
--
-- This contains just enough information to detect changes without
-- reading full metadata from every file.
data LibrarySnapshot = LibrarySnapshot
  { snapshotFiles :: Map.Map OsPath FileInfo
  } deriving (Eq, Show, Generic)

-- | Basic file information for change detection.
--
-- We track modification time and size to detect when files have changed.
-- This is cheaper than reading full metadata.
data FileInfo = FileInfo
  { fileModifiedTime :: UTCTime
  , fileSize :: Integer
  } deriving (Eq, Show, Generic)

-- | The difference between two library snapshots.
--
-- This represents what changed: files added, deleted, or modified.
-- Both initial scans and file watcher events produce these diffs.
data FileSystemDiff = FileSystemDiff
  { diffAdded :: [OsPath]
  , diffDeleted :: [OsPath]
  , diffModified :: [OsPath]
  } deriving (Eq, Show, Generic)

-- | Compute the difference between two snapshots.
--
-- This is a pure function that compares two snapshots and determines
-- what files were added, deleted, or modified.
--
-- For an initial library scan, pass 'emptySnapshot' as the old snapshot.
--
-- Examples:
--
-- >>> let old = LibrarySnapshot (Map.singleton "/music/a.mp3" (FileInfo time1 1000))
-- >>> let new = LibrarySnapshot (Map.singleton "/music/b.mp3" (FileInfo time2 1000))
-- >>> computeDiff old new
-- FileSystemDiff {diffAdded = ["/music/b.mp3"], diffDeleted = ["/music/a.mp3"], diffModified = []}
computeDiff :: LibrarySnapshot -> LibrarySnapshot -> FileSystemDiff
computeDiff (LibrarySnapshot oldFiles) (LibrarySnapshot newFiles) =
  let oldPaths = Map.keysSet oldFiles
      newPaths = Map.keysSet newFiles

      -- Files in new but not in old
      added = Set.toList $ newPaths `Set.difference` oldPaths

      -- Files in old but not in new
      deleted = Set.toList $ oldPaths `Set.difference` newPaths

      -- Files present in both - check if modified
      commonPaths = Set.toList $ oldPaths `Set.intersection` newPaths
      modified = filter (hasChanged oldFiles newFiles) commonPaths

  in FileSystemDiff
       { diffAdded = added
       , diffDeleted = deleted
       , diffModified = modified
       }

-- | Check if a file has been modified by comparing metadata.
hasChanged :: Map.Map OsPath FileInfo -> Map.Map OsPath FileInfo -> OsPath -> Bool
hasChanged oldFiles newFiles path =
  case (Map.lookup path oldFiles, Map.lookup path newFiles) of
    (Just oldInfo, Just newInfo) ->
      -- File changed if modification time or size differs
      fileModifiedTime oldInfo /= fileModifiedTime newInfo ||
      fileSize oldInfo /= fileSize newInfo
    _ -> False

-- | An empty snapshot with no files.
emptySnapshot :: LibrarySnapshot
emptySnapshot = LibrarySnapshot mempty

-- | An empty diff with no changes.
emptyDiff :: FileSystemDiff
emptyDiff = FileSystemDiff [] [] []

-- | Check if a diff contains any changes.
isEmpty :: FileSystemDiff -> Bool
isEmpty (FileSystemDiff [] [] []) = True
isEmpty _ = False
