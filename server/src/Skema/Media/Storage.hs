{-# LANGUAGE OverloadedStrings #-}

-- | Local image storage for artist and album artwork.
--
-- Downloads images from external sources and stores them in the cache directory.
-- Returns local paths for database storage.
module Skema.Media.Storage
  ( downloadAndStoreImage
  , downloadAndStoreImages
  , getImagePath
  , ensureImageDirectory
  ) where

import Skema.HTTP.Client (HttpClient)
import qualified Skema.HTTP.Client as HTTP
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeExtension, pathSeparator)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T

-- | Download an image from a URL and store it locally in the cache directory.
--
-- Returns a local path relative to the server root (e.g., "/images/artists/abc123.jpg")
-- that can be stored in the database and served via static file endpoint.
downloadAndStoreImage
  :: HttpClient      -- ^ HTTP client for downloading
  -> FilePath        -- ^ Cache directory base path
  -> Text            -- ^ Image category ("artists" or "albums")
  -> Text            -- ^ Unique identifier (MBID)
  -> Text            -- ^ Source URL
  -> IO (Either Text Text)
downloadAndStoreImage httpClient cacheDir category identifier sourceUrl = do
  -- Ensure the images directory exists
  let imagesDir = cacheDir </> "images" </> toString category
  createDirectoryIfMissing True imagesDir

  -- Extract file extension from URL (default to .jpg)
  let ext = if T.null (toText $ takeExtension $ toString sourceUrl)
            then ".jpg"
            else toText $ takeExtension $ toString sourceUrl

  -- Create filename from identifier
  let filename = toString identifier <> toString ext
  let localPath = imagesDir </> filename

  -- Check if file already exists
  exists <- doesFileExist localPath

  if exists
    then do
      -- Already cached, return the URL path
      let urlPath = "/images/" <> category <> "/" <> toText filename
      pure $ Right urlPath
    else do
      -- Download the image
      result <- HTTP.get httpClient sourceUrl
      case result of
        Left err -> pure $ Left $ "Failed to download image: " <> show err
        Right imageData -> do
          -- Save to disk
          BSL.writeFile localPath imageData
          -- Return the URL path
          let urlPath = "/images/" <> category <> "/" <> toText filename
          pure $ Right urlPath

-- | Get the local file path for an image URL path.
--
-- Converts a URL path like "/images/artists/abc123.jpg" to an absolute file path.
getImagePath :: FilePath -> Text -> FilePath
getImagePath cacheDir urlPath =
  let pathWithoutLeadingSlash = T.dropWhile (== '/') urlPath
      relativePath = T.replace "/" (toText [pathSeparator]) pathWithoutLeadingSlash
  in cacheDir </> toString relativePath

-- | Download both full-size and thumbnail images and store them locally.
--
-- Returns (fullSizePath, maybeThumbnailPath)
downloadAndStoreImages
  :: HttpClient      -- ^ HTTP client for downloading
  -> FilePath        -- ^ Cache directory base path
  -> Text            -- ^ Image category ("artists" or "albums")
  -> Text            -- ^ Unique identifier (MBID)
  -> Text            -- ^ Full-size source URL
  -> Maybe Text      -- ^ Optional thumbnail source URL
  -> IO (Either Text (Text, Maybe Text))
downloadAndStoreImages httpClient cacheDir category identifier fullSizeUrl maybeThumbnailUrl = do
  -- Download full-size image
  fullSizeResult <- downloadAndStoreImage httpClient cacheDir category identifier fullSizeUrl

  case fullSizeResult of
    Left err -> pure $ Left err
    Right fullSizePath -> do
      -- Download thumbnail if provided
      thumbnailResult <- case maybeThumbnailUrl of
        Nothing -> pure Nothing
        Just thumbUrl -> do
          result <- downloadAndStoreThumbnail httpClient cacheDir category identifier thumbUrl
          case result of
            Left _ -> pure Nothing  -- Thumbnail download failed, but that's okay
            Right thumbPath -> pure $ Just thumbPath

      pure $ Right (fullSizePath, thumbnailResult)

-- | Download a thumbnail image and store it with "_thumb" suffix.
downloadAndStoreThumbnail
  :: HttpClient      -- ^ HTTP client for downloading
  -> FilePath        -- ^ Cache directory base path
  -> Text            -- ^ Image category ("artists" or "albums")
  -> Text            -- ^ Unique identifier (MBID)
  -> Text            -- ^ Source URL
  -> IO (Either Text Text)
downloadAndStoreThumbnail httpClient cacheDir category identifier sourceUrl = do
  -- Ensure the images directory exists
  let imagesDir = cacheDir </> "images" </> toString category
  createDirectoryIfMissing True imagesDir

  -- Extract file extension from URL (default to .jpg)
  let ext = if T.null (toText $ takeExtension $ toString sourceUrl)
            then ".jpg"
            else toText $ takeExtension $ toString sourceUrl

  -- Create filename with "_thumb" suffix
  let filename = toString identifier <> "_thumb" <> toString ext
  let localPath = imagesDir </> filename

  -- Check if file already exists
  exists <- doesFileExist localPath

  if exists
    then do
      -- Already cached, return the URL path
      let urlPath = "/images/" <> category <> "/" <> toText filename
      pure $ Right urlPath
    else do
      -- Download the image
      result <- HTTP.get httpClient sourceUrl
      case result of
        Left err -> pure $ Left $ "Failed to download thumbnail: " <> show err
        Right imageData -> do
          -- Save to disk
          BSL.writeFile localPath imageData
          -- Return the URL path
          let urlPath = "/images/" <> category <> "/" <> toText filename
          pure $ Right urlPath

-- | Ensure the images directory structure exists.
ensureImageDirectory :: FilePath -> IO ()
ensureImageDirectory cacheDir = do
  createDirectoryIfMissing True (cacheDir </> "images" </> "artists")
  createDirectoryIfMissing True (cacheDir </> "images" </> "albums")
