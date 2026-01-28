{-# LANGUAGE OverloadedStrings #-}

-- | Static file serving handlers.
module Skema.API.Handlers.Static
  ( staticFileServer
  , frontendServer
  ) where

import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import WaiAppStatic.Types (StaticSettings(..), MaxAge(..), LookupResult(..), toPiece)
import Servant
import System.FilePath ((</>))
import qualified System.Environment as Env
import Skema.Config.Types (Config, ServerConfig, serverWebRoot, server)
import Control.Concurrent.STM (TVar)
import qualified Control.Concurrent.STM as STM
import Network.Wai (responseFile, responseLBS, Middleware, rawPathInfo)
import Network.HTTP.Types (status200, hContentType)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

-- | Static file server for images.
-- Serves images from the cache directory with proper caching headers.
staticFileServer :: FilePath -> Server Raw
staticFileServer cacheDir = Tagged $ app
  where
    imagesDir = cacheDir </> "images"

    -- Create static file serving application with caching
    app = staticApp $ settings imagesDir

    settings dir =
      let baseSettings = defaultFileServerSettings dir
      in baseSettings
           { ssMaxAge = MaxAgeSeconds 86400  -- Cache for 24 hours
           , ssAddTrailingSlash = False
           }

-- | Frontend SPA server.
-- Serves the built frontend with fallback to index.html for client-side routing.
-- Injects web_root configuration into HTML at runtime.
-- Checks multiple locations in order:
--   1. SKEMA_FRONTEND_DIR environment variable (for nix/docker builds)
--   2. ../web/dist (for development)
frontendServer :: TVar Config -> Server Raw
frontendServer configVar = Tagged $ \req sendResponse -> do
  -- Determine frontend directory at request time
  -- This allows the path to be resolved dynamically
  maybeFrontendRoot <- Env.lookupEnv "SKEMA_FRONTEND_DIR"
  let frontendDir = case maybeFrontendRoot of
        Just root -> root
        Nothing -> ".." </> "web" </> "dist"  -- Development fallback

  -- Read web_root from config
  cfg <- STM.readTVarIO configVar
  let webRoot = serverWebRoot (server cfg)

  -- Apply middleware that injects web_root into index.html
  injectWebRootMiddleware webRoot frontendDir (staticApp (settings frontendDir)) req sendResponse
  where
    settings dir =
      let baseSettings = defaultFileServerSettings dir
          -- Get the default lookup function
          defaultLookup = ssLookupFile baseSettings
          -- Safely construct the index.html piece
          indexPiece = case toPiece "index.html" of
            Just piece -> [piece]
            Nothing -> []  -- Should never happen for "index.html", but handle gracefully
      in baseSettings
           { ssMaxAge = MaxAgeSeconds 3600  -- Cache for 1 hour
           , ssAddTrailingSlash = False
           -- Custom lookup with SPA fallback to index.html
           , ssLookupFile = \pieces -> do
               result <- defaultLookup pieces
               case result of
                 -- If file not found, serve index.html for client-side routing
                 LRNotFound ->
                   if null indexPiece
                     then pure LRNotFound  -- Fallback failed, return not found
                     else defaultLookup indexPiece
                 _ -> pure result
           }

    -- Middleware to inject web_root into index.html
    -- The web_root is determined from config, allowing users to host at subpaths
    injectWebRootMiddleware :: T.Text -> FilePath -> Middleware
    injectWebRootMiddleware webRoot dir app req sendResponse' = do
      let path = decodeUtf8 (rawPathInfo req)
      -- Check if this is a request for index.html or a SPA route
      if path == "/" || path == "/index.html" || not ("." `T.isInfixOf` path)
        then do
          -- Read and modify index.html
          let indexPath = dir </> "index.html"
          content <- BS.readFile indexPath
          let contentText = TE.decodeUtf8 content
          -- Normalize web_root: ensure it starts with / and doesn't end with /
          let normalizedWebRoot = case webRoot of
                "" -> "/"
                "/" -> "/"
                path' ->
                  let withLeading = if T.head path' == '/' then path' else "/" <> path'
                  in T.dropWhileEnd (== '/') withLeading
          -- Inject web_root by setting the window variable before the script runs
          let injection = "\n    <script>window.SKEMA_BASE_PATH = '" <> normalizedWebRoot <> "';</script>"
          let injected = T.replace "<script>" (injection <> "\n    <script>") contentText
          sendResponse' $ responseLBS
            status200
            [(hContentType, "text/html; charset=utf-8")]
            (LBS.fromStrict $ TE.encodeUtf8 injected)
        else
          -- For all other files, pass through to static app
          app req sendResponse'
