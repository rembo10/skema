{-# LANGUAGE OverloadedStrings #-}

-- | Filesystem API handlers.
module Skema.API.Handlers.Filesystem
  ( filesystemServer
  ) where

import Skema.API.Types.Filesystem (FilesystemAPI, FilesystemBrowseResponse)
import Skema.FileSystem.Browser (browsePath)
import Servant
import Skema.API.App (AppM)

-- | Filesystem API handlers.
filesystemServer :: ServerT FilesystemAPI AppM
filesystemServer =
  browseHandler
  where
    browseHandler :: Maybe Text -> AppM FilesystemBrowseResponse
    browseHandler maybePath = do
      let path = fromMaybe "/" maybePath

      -- Convert Text path to FilePath
      let filePath = toString path

      liftIO $ browsePath filePath
