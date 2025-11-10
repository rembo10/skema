{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Filesystem browsing utilities for the API.
--
-- This module provides functions for browsing the filesystem,
-- expanding paths, and listing directory contents with metadata.
module Skema.FileSystem.Browser
  ( browsePath
  , parentPath
  , listDirectoryWithInfo
  ) where

import Skema.API.Types.Filesystem (FilesystemBrowseResponse(..), FilesystemEntry(..))
import System.FilePath ((</>), takeDirectory)
import System.Directory
  ( listDirectory
  , doesDirectoryExist
  , doesPathExist
  , getPermissions
  , getModificationTime
  , getFileSize
  , readable
  , writable
  )
import System.Environment (getEnv)
import Control.Monad.Catch (catch)
import qualified Control.Exception as E

-- | Browse filesystem at the given path.
--
-- Handles home directory expansion, path validation, and directory listing.
-- Returns a response with entries or error messages.
browsePath :: FilePath -> IO FilesystemBrowseResponse
browsePath path = do
  -- Handle home directory expansion
  expandedPath <- if path == "~" || "~/" `isPrefixOf` path
    then do
      homeDir <- getEnv "HOME"
      pure $ homeDir <> drop 1 path
    else pure path

  -- Check if path exists and is readable
  pathExists <- doesPathExist expandedPath
  if not pathExists
    then pure $ FilesystemBrowseResponse
      { filesystemBrowseResponsePath = toText expandedPath
      , filesystemBrowseResponseParent = parentPath expandedPath
      , filesystemBrowseResponseEntries = []
      , filesystemBrowseResponseError = Just $ "Path does not exist: " <> toText expandedPath
      }
    else do
      isDir <- doesDirectoryExist expandedPath
      if not isDir
        then pure $ FilesystemBrowseResponse
          { filesystemBrowseResponsePath = toText expandedPath
          , filesystemBrowseResponseParent = parentPath expandedPath
          , filesystemBrowseResponseEntries = []
          , filesystemBrowseResponseError = Just $ "Path is not a directory: " <> toText expandedPath
          }
        else do
          -- List directory contents
          result <- catch (listDirectoryWithInfo expandedPath) handleIOError
          case result of
            Left err -> pure $ FilesystemBrowseResponse
              { filesystemBrowseResponsePath = toText expandedPath
              , filesystemBrowseResponseParent = parentPath expandedPath
              , filesystemBrowseResponseEntries = []
              , filesystemBrowseResponseError = Just err
              }
            Right entries -> pure $ FilesystemBrowseResponse
              { filesystemBrowseResponsePath = toText expandedPath
              , filesystemBrowseResponseParent = parentPath expandedPath
              , filesystemBrowseResponseEntries = entries
              , filesystemBrowseResponseError = Nothing
              }
  where
    handleIOError :: E.IOException -> IO (Either Text a)
    handleIOError e = pure $ Left $ "Error reading directory: " <> show e

-- | Get parent directory path.
--
-- Returns Nothing if the path has no parent (root directory).
parentPath :: FilePath -> Maybe Text
parentPath path =
  let parent = takeDirectory path
  in if parent == path || null parent
     then Nothing
     else Just (toText parent)

-- | List directory contents with metadata.
--
-- Returns a sorted list of entries with permissions, size, and modification time.
-- Directories are sorted before files, then alphabetically by name.
-- Entries that cause errors (e.g., permission denied) are skipped.
listDirectoryWithInfo :: FilePath -> IO (Either Text [FilesystemEntry])
listDirectoryWithInfo path = do
  names <- listDirectory path
  -- Use mapMaybe to skip entries that cause errors
  entries <- fmap catMaybes $ forM names $ \name -> do
    let fullPath = path </> name
    -- Catch errors for individual entries
    catch (Just <$> getEntryInfo fullPath name) $ \(_ :: E.IOException) ->
      pure Nothing  -- Skip problematic entries

  -- Sort: directories first, then by name
  let sorted = sortBy compareEntries entries
  pure $ Right sorted
  where
    getEntryInfo :: FilePath -> String -> IO FilesystemEntry
    getEntryInfo fullPath name = do
      isDir <- doesDirectoryExist fullPath

      -- Get file permissions and metadata
      perms <- getPermissions fullPath
      let readable' = readable perms
      let writable' = writable perms

      -- Get file size and modification time
      (size, modTime) <- if isDir
        then pure (Nothing, Nothing)
        else do
          fileSize <- getFileSize fullPath
          modTimeUTC <- getModificationTime fullPath
          pure (Just fileSize, Just $ show modTimeUTC)

      pure $ FilesystemEntry
        { filesystemEntryName = toText name
        , filesystemEntryPath = toText fullPath
        , filesystemEntryIsDirectory = isDir
        , filesystemEntrySize = size
        , filesystemEntryModifiedAt = modTime
        , filesystemEntryReadable = readable'
        , filesystemEntryWritable = writable'
        }

    compareEntries a b =
      case (filesystemEntryIsDirectory a, filesystemEntryIsDirectory b) of
        (True, False) -> LT
        (False, True) -> GT
        _ -> compare (filesystemEntryName a) (filesystemEntryName b)
