{-# LANGUAGE OverloadedStrings #-}

-- | Trash/recycle bin functionality for deleted files.
--
-- This module provides a trash system for files deleted during upgrades,
-- allowing recovery if something goes wrong. Files are moved to a trash
-- directory with timestamps and can be automatically cleaned up after
-- a configurable retention period.
module Skema.FileSystem.Trash
  ( moveToTrash
  , cleanupOldTrashFiles
  , getTrashDirectory
  ) where

import Data.Time (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime)
import System.OsPath ((</>), OsPath)
import qualified System.OsPath as OP
import qualified System.Directory as Dir
import qualified Data.Text as T
import Control.Monad (foldM)

-- | Get the trash directory path.
--
-- Creates a .trash subdirectory in the library path.
getTrashDirectory :: OsPath -> IO OsPath
getTrashDirectory libraryPath = do
  trashName <- OP.encodeFS ".trash"
  pure $ libraryPath </> trashName

-- | Move a file to the trash directory with a timestamp.
--
-- The file is moved to the trash directory with its relative path preserved
-- and a timestamp appended to avoid conflicts.
--
-- Example:
--   Original: /music/Artist/Album/track.flac
--   Trash:    /music/.trash/Artist/Album/track.flac.2024-01-06T12-30-00Z
moveToTrash
  :: OsPath       -- ^ Library root path
  -> OsPath       -- ^ File path to move to trash
  -> IO ()
moveToTrash libraryPath filePath = do
  -- Get trash directory
  trashDir <- getTrashDirectory libraryPath
  trashDirStr <- OP.decodeUtf trashDir
  Dir.createDirectoryIfMissing True trashDirStr

  -- Get relative path from library root
  filePathStr <- OP.decodeUtf filePath
  libraryPathStr <- OP.decodeUtf libraryPath

  let relativePath = if libraryPathStr `isPrefixOf` filePathStr
        then drop (length libraryPathStr) filePathStr
        else filePathStr

  -- Remove leading slash if present
  let cleanRelativePath = case relativePath of
        ('/':rest) -> rest
        path -> path

  -- Add timestamp to filename to avoid conflicts
  now <- getCurrentTime
  let timestamp = T.replace ":" "-" $ T.replace "." "-" $ show now
  let trashedFileName = cleanRelativePath <> "." <> toString timestamp

  -- Create target path in trash
  trashedFileOsPath <- OP.encodeFS trashedFileName
  let targetPath = trashDir </> trashedFileOsPath

  -- Ensure parent directory exists in trash
  let targetDir = OP.takeDirectory targetPath
  targetDirStr <- OP.decodeUtf targetDir
  Dir.createDirectoryIfMissing True targetDirStr

  -- Move file to trash
  targetPathStr <- OP.decodeUtf targetPath
  Dir.renameFile filePathStr targetPathStr

-- | Clean up trash files older than the specified retention period.
--
-- Scans the trash directory and deletes files that are older than
-- the retention period (in days).
cleanupOldTrashFiles
  :: OsPath           -- ^ Library root path
  -> Int              -- ^ Retention period in days
  -> IO Int           -- ^ Number of files deleted
cleanupOldTrashFiles libraryPath retentionDays = do
  trashDir <- getTrashDirectory libraryPath
  trashDirStr <- OP.decodeUtf trashDir

  -- Check if trash directory exists
  exists <- Dir.doesDirectoryExist trashDirStr
  if not exists
    then pure 0
    else do
      -- Get current time
      now <- getCurrentTime
      let maxAge = fromIntegral retentionDays * 24 * 3600 :: NominalDiffTime

      -- Find and delete old files
      deleteOldFiles trashDir now maxAge 0

-- | Recursively find and delete files older than maxAge.
deleteOldFiles :: OsPath -> UTCTime -> NominalDiffTime -> Int -> IO Int
deleteOldFiles dirPath now maxAge count = do
  dirPathStr <- OP.decodeUtf dirPath
  entries <- Dir.listDirectory dirPathStr

  foldM (\acc entry -> do
    let entryStr = dirPathStr <> "/" <> entry
    isDir <- Dir.doesDirectoryExist entryStr

    if isDir
      then do
        -- Recursively process subdirectory
        entryOsPath <- OP.encodeFS entryStr
        deleteOldFiles entryOsPath now maxAge acc
      else do
        -- Check file age
        modTime <- Dir.getModificationTime entryStr
        let age = diffUTCTime now modTime

        if age > maxAge
          then do
            -- Delete old file
            Dir.removeFile entryStr
            pure (acc + 1)
          else pure acc
    ) count entries
