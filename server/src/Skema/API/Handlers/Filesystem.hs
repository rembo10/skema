{-# LANGUAGE OverloadedStrings #-}

-- | Filesystem API handlers.
module Skema.API.Handlers.Filesystem
  ( filesystemServer
  ) where

import Skema.API.Types.Filesystem (FilesystemAPI, FilesystemBrowseResponse)
import qualified Skema.Config.Types as Cfg
import Skema.FileSystem.Browser (browsePath)
import Servant

-- | Filesystem API handlers.
filesystemServer :: Cfg.ServerConfig -> Server FilesystemAPI
filesystemServer _serverCfg =
  browseHandler
  where
    browseHandler :: Maybe Text -> Handler FilesystemBrowseResponse
    browseHandler maybePath = do
      let path = fromMaybe "/" maybePath

      -- Convert Text path to FilePath
      let filePath = toString path

      liftIO $ browsePath filePath
