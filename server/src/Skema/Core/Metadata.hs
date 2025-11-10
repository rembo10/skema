{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Core metadata operations shared between Scanner and Importer.
--
-- This module provides common functionality for:
-- - Scanning directories and parsing audio metadata
-- - Grouping files by album
-- - Handling metadata parsing errors
--
-- These operations are used by both:
-- - Scanner: For files already in the music library
-- - Importer: For files being imported from downloads
module Skema.Core.Metadata
  ( -- * Types
    MetadataResult(..)
  , ParsedFile
  , GroupedFiles(..)
    -- * Operations
  , scanAndParseMetadata
  , groupParsedFiles
  ) where

import Monatone.Common (parseMetadata)
import qualified Monatone.Metadata as M
import Skema.FileSystem.Scanner (scanDirectory)
import Skema.Core.Library (LibrarySnapshot(..))
import Skema.MusicBrainz.Matching (groupFilesByRelease)
import Skema.MusicBrainz.Types (FileGroup(..))
import System.OsPath (OsPath)
import qualified Data.Map.Strict as Map

-- | A file path paired with its parsed metadata
type ParsedFile = (OsPath, M.Metadata)

-- | Result of scanning and parsing files in a directory
data MetadataResult = MetadataResult
  { mrValidFiles :: [ParsedFile]
    -- ^ Files with successfully parsed metadata
  , mrFailedFiles :: [(OsPath, String)]
    -- ^ Files that failed to parse (with error messages)
  , mrTotalFiles :: Int
    -- ^ Total number of files scanned
  } deriving (Show)

-- | Result of grouping parsed files into albums
data GroupedFiles = GroupedFiles
  { gfGroups :: [FileGroup]
    -- ^ File groups (each represents a potential album)
  , gfParsedFiles :: [ParsedFile]
    -- ^ The original parsed files
  } deriving (Show)

-- | Scan a directory and parse metadata from all audio files.
--
-- This operation:
-- 1. Scans the directory recursively for files
-- 2. Attempts to parse metadata from each file
-- 3. Collects both successes and failures
--
-- Example:
-- @
--   result <- scanAndParseMetadata downloadPath
--   when (null (mrValidFiles result)) $
--     throwIO $ ImportException "No valid audio files found"
-- @
scanAndParseMetadata :: OsPath -> IO MetadataResult
scanAndParseMetadata path = do
  -- Scan directory for all files
  snapshot <- scanDirectory path
  let allPaths = Map.keys (snapshotFiles snapshot)

  -- Parse metadata from each file
  filesWithMeta <- forM allPaths $ \filePath -> do
    metaResult <- parseMetadata filePath
    case metaResult of
      Right meta -> pure $ Right (filePath, meta)
      Left err -> do
        -- Return the file path and error for reporting
        pure $ Left (filePath, show err)

  -- Partition successes and failures
  let (failures, successes) = partitionEithers filesWithMeta
      validFiles = successes
      failedFiles = failures
      totalFiles = length allPaths

  pure MetadataResult
    { mrValidFiles = validFiles
    , mrFailedFiles = failedFiles
    , mrTotalFiles = totalFiles
    }

-- | Group parsed files into albums based on their metadata.
--
-- Uses album title, album artist, and track grouping heuristics
-- to determine which files belong together.
--
-- Example:
-- @
--   grouped <- groupParsedFiles validFiles
--   case viaNonEmpty head (gfGroups grouped) of
--     Nothing -> error "Could not group files into albums"
--     Just fileGroup -> identifyAlbum fileGroup
-- @
groupParsedFiles :: [ParsedFile] -> IO GroupedFiles
groupParsedFiles parsedFiles = do
  let fileGroups = groupFilesByRelease parsedFiles
  pure GroupedFiles
    { gfGroups = fileGroups
    , gfParsedFiles = parsedFiles
    }
