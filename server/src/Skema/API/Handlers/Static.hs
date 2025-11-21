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
-- Checks multiple locations in order:
--   1. SKEMA_WEB_ROOT environment variable (for nix builds)
--   2. ../web/dist (for development)
frontendServer :: Server Raw
frontendServer = Tagged $ \req sendResponse -> do
  -- Determine frontend directory at request time
  -- This allows the path to be resolved dynamically
  maybeFrontendRoot <- Env.lookupEnv "SKEMA_WEB_ROOT"
  let frontendDir = case maybeFrontendRoot of
        Just root -> root
        Nothing -> ".." </> "web" </> "dist"  -- Development fallback

  staticApp (settings frontendDir) req sendResponse
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
