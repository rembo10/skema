{-# LANGUAGE OverloadedStrings #-}

-- | Filesystem API handlers.
module Skema.API.Handlers.Filesystem
  ( filesystemServer
  ) where

import Skema.API.Types.Filesystem (FilesystemAPI, FilesystemBrowseResponse)
import Skema.Auth (requireAuth)
import Skema.Auth.JWT (JWTSecret)
import qualified Skema.Config.Types as Cfg
import Skema.FileSystem.Browser (browsePath)
import Servant

-- | Filesystem API handlers.
filesystemServer :: Cfg.ServerConfig -> JWTSecret -> TVar Cfg.Config -> Server FilesystemAPI
filesystemServer _serverCfg jwtSecret configVar = \maybeAuthHeader ->
  browseHandler maybeAuthHeader
  where
    browseHandler :: Maybe Text -> Maybe Text -> Handler FilesystemBrowseResponse
    browseHandler authHeader maybePath = do
      _ <- requireAuth configVar jwtSecret authHeader

      let path = fromMaybe "/" maybePath

      -- Convert Text path to FilePath
      let filePath = toString path

      liftIO $ browsePath filePath
