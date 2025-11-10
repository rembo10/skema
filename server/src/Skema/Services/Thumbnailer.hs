{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Thumbnailer service - generates thumbnails from full-size images.
module Skema.Services.Thumbnailer
  ( startThumbnailerService
  ) where

import Skema.Services.Dependencies (ThumbnailerDeps(..))
import Skema.Events.Bus
import Skema.Events.Types
import Skema.Database.Connection
import Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (readTChan)
import Control.Exception (try)
import qualified Data.Text as T
import Database.SQLite.Simple (Only(..))
import Katip
import System.FilePath ((</>), takeExtension, dropExtension)
import System.Directory (doesFileExist)
import Codec.Picture

-- | Thumbnail size in pixels (square).
thumbnailSize :: Int
thumbnailSize = 256

-- | Start the thumbnailer service.
--
-- This service listens for image events and generates thumbnails as needed.
startThumbnailerService :: ThumbnailerDeps -> IO (Async ())
startThumbnailerService deps = do
  chan <- STM.atomically $ subscribe (thumbEventBus deps)
  async $ forever $ do
    envelope <- STM.atomically $ readTChan chan
    case envelopeEvent envelope of
      ArtistImageFetched{..} -> do
        -- Generate thumbnail for artist image if not already present
        when (isNothing artistImageThumbnailUrl) $ do
          -- Run asynchronously to avoid blocking other thumbnail generation
          _ <- async $ do
            result <- try $ handleThumbnailGeneration
              deps
              "artists"
              artistImageMBID
              artistImageUrl
              "catalog_artists"
              "artist_mbid"
              "thumbnail_url"
            case result of
              Left (e :: SomeException) -> do
                let le = thumbLogEnv deps
                runKatipContextT le () "thumbnailer.error" $ do
                  $(logTM) ErrorS $ logStr $ ("Exception generating artist thumbnail: " <> show e :: Text)
              Right () -> pure ()
          pure ()

      AlbumCoverFetched{..} -> do
        -- Generate thumbnail for album cover if not already present
        when (isNothing albumCoverThumbnailUrl) $ do
          _ <- async $ do
            result <- try $ handleThumbnailGeneration
              deps
              "albums"
              albumCoverReleaseGroupMBID
              albumCoverUrl
              "catalog_albums"
              "release_group_mbid"
              "album_cover_thumbnail_url"
            case result of
              Left (e :: SomeException) -> do
                let le = thumbLogEnv deps
                runKatipContextT le () "thumbnailer.error" $ do
                  $(logTM) ErrorS $ logStr $ ("Exception generating album thumbnail: " <> show e :: Text)
              Right () -> pure ()
          pure ()

      _ -> pure ()  -- Ignore other events

-- | Handle thumbnail generation for an image.
handleThumbnailGeneration
  :: ThumbnailerDeps
  -> Text           -- ^ Image category ("artists" or "albums")
  -> Text           -- ^ Identifier (MBID)
  -> Text           -- ^ Full-size image URL path (e.g., "/images/artists/abc.jpg")
  -> Text           -- ^ Database table name
  -> Text           -- ^ Database ID column name
  -> Text           -- ^ Database thumbnail URL column name
  -> IO ()
handleThumbnailGeneration ThumbnailerDeps{..} category identifier imageUrlPath tableName idColumn thumbnailColumn = do
  let le = thumbLogEnv
  let pool = thumbDbPool
  let bus = thumbEventBus
  let initialContext = ()
  let initialNamespace = "services.thumbnailer"

  runKatipContextT le initialContext initialNamespace $ do
    katipAddContext (sl "category" category) $ do
      katipAddContext (sl "identifier" identifier) $ do
        $(logTM) InfoS $ logStr $ ("Generating thumbnail for: " <> identifier :: Text)

        -- Convert URL path to filesystem path
        let fullImagePath = thumbCacheDir </> toString (T.drop 1 imageUrlPath)  -- Drop leading '/'
        $(logTM) InfoS $ logStr $ ("Looking for image at: " <> toText fullImagePath :: Text)

        -- Check if full image exists
        exists <- liftIO $ doesFileExist fullImagePath

        unless exists $ do
          $(logTM) WarningS $ logStr $ ("Full-size image not found: " <> toText fullImagePath :: Text)
          return ()

        when exists $ do
          -- Determine thumbnail path
          let ext = takeExtension fullImagePath
          let baseName = dropExtension fullImagePath
          let thumbnailPath = baseName <> "_thumb" <> ext
          let thumbnailUrlPath = "/images/" <> category <> "/" <> identifier <> "_thumb" <> toText ext

          -- Check if thumbnail already exists
          thumbExists <- liftIO $ doesFileExist thumbnailPath
          if thumbExists
            then do
              $(logTM) DebugS $ logStr $ ("Thumbnail already exists: " <> toText thumbnailPath :: Text)
              -- Update database with existing thumbnail path
              liftIO $ withConnection pool $ \conn -> do
                let updateSql = "UPDATE " <> tableName <> " SET " <> thumbnailColumn <> " = ? WHERE " <> idColumn <> " = ?"
                executeQuery conn updateSql (Just thumbnailUrlPath, identifier)

              -- Emit event even if thumbnail already exists
              case category of
                "artists" -> do
                  -- Get artist ID from database
                  maybeArtistId <- liftIO $ withConnection pool $ \conn -> do
                    let selectSql = "SELECT id FROM " <> tableName <> " WHERE " <> idColumn <> " = ?"
                    results <- queryRows conn selectSql (Only identifier)
                    pure $ case results of
                      [(Only aid)] -> Just (aid :: Int64)
                      _ -> Nothing

                  case maybeArtistId of
                    Just artistId ->
                      liftIO $ publishAndLog bus le "thumbnailer" $ ArtistImageFetched
                        { artistImageId = artistId
                        , artistImageMBID = identifier
                        , artistImageUrl = imageUrlPath
                        , artistImageThumbnailUrl = Just thumbnailUrlPath
                        , artistImageSource = "thumbnail-cached"
                        }
                    Nothing ->
                      $(logTM) WarningS $ logStr $ ("Could not find artist ID for MBID: " <> identifier :: Text)

                "albums" -> do
                  liftIO $ publishAndLog bus le "thumbnailer" $ AlbumCoverFetched
                    { albumCoverReleaseGroupMBID = identifier
                    , albumCoverUrl = imageUrlPath
                    , albumCoverThumbnailUrl = Just thumbnailUrlPath
                    , albumCoverSource = "thumbnail-cached"
                    }
                _ -> pure ()
            else do
              -- Generate thumbnail
              $(logTM) InfoS $ logStr $ ("Generating thumbnail at: " <> toText thumbnailPath :: Text)
              result <- liftIO $ generateThumbnail fullImagePath thumbnailPath thumbnailSize
              case result of
                Left err -> do
                  $(logTM) ErrorS $ logStr $ ("Failed to generate thumbnail: " <> err :: Text)
                  $(logTM) ErrorS $ logStr $ ("Source: " <> toText fullImagePath <> " -> Dest: " <> toText thumbnailPath :: Text)
                Right () -> do
                  $(logTM) InfoS $ logStr $ ("Thumbnail generated successfully: " <> toText thumbnailPath :: Text)

                  -- Update database with thumbnail path
                  liftIO $ withConnection pool $ \conn -> do
                    let updateSql = "UPDATE " <> tableName <> " SET " <> thumbnailColumn <> " = ? WHERE " <> idColumn <> " = ?"
                    executeQuery conn updateSql (Just thumbnailUrlPath, identifier)

                  -- Emit event that thumbnail is ready
                  case category of
                    "artists" -> do
                      -- Get artist ID from database
                      maybeArtistId <- liftIO $ withConnection pool $ \conn -> do
                        let selectSql = "SELECT id FROM " <> tableName <> " WHERE " <> idColumn <> " = ?"
                        results <- queryRows conn selectSql (Only identifier)
                        pure $ case results of
                          [(Only aid)] -> Just (aid :: Int64)
                          _ -> Nothing

                      case maybeArtistId of
                        Just artistId -> do
                          liftIO $ publishAndLog bus le "thumbnailer" $ ArtistImageFetched
                            { artistImageId = artistId
                            , artistImageMBID = identifier
                            , artistImageUrl = imageUrlPath
                            , artistImageThumbnailUrl = Just thumbnailUrlPath
                            , artistImageSource = "thumbnail-generated"
                            }
                        Nothing ->
                          $(logTM) WarningS $ logStr $ ("Could not find artist ID for MBID: " <> identifier :: Text)

                    "albums" -> do
                      liftIO $ publishAndLog bus le "thumbnailer" $ AlbumCoverFetched
                        { albumCoverReleaseGroupMBID = identifier
                        , albumCoverUrl = imageUrlPath
                        , albumCoverThumbnailUrl = Just thumbnailUrlPath
                        , albumCoverSource = "thumbnail-generated"
                        }
                    _ -> pure ()

-- | Generate a thumbnail from a source image.
generateThumbnail
  :: FilePath  -- ^ Source image path
  -> FilePath  -- ^ Destination thumbnail path
  -> Int       -- ^ Thumbnail size (width/height in pixels)
  -> IO (Either Text ())
generateThumbnail sourcePath destPath size = do
  -- Read the source image
  imageResult <- readImage sourcePath
  case imageResult of
    Left err -> pure $ Left $ "Failed to read image: " <> toText err
    Right dynImage -> do
      -- Convert to RGB8 for consistent processing
      let rgb8Image = convertRGB8 dynImage

      -- Calculate dimensions for scaling (maintain aspect ratio)
      let (srcWidth, srcHeight) = (imageWidth rgb8Image, imageHeight rgb8Image)
      let scaleFactor :: Double = min
            (fromIntegral size / fromIntegral srcWidth)
            (fromIntegral size / fromIntegral srcHeight)
      let newWidth = round (fromIntegral srcWidth * scaleFactor)
      let newHeight = round (fromIntegral srcHeight * scaleFactor)

      -- Resize using nearest-neighbor (simple and fast for thumbnails)
      let scaled = scaleNearestNeighbor rgb8Image newWidth newHeight

      -- Save the thumbnail (preserve original format if JPEG, otherwise PNG)
      saveResult <- try $ case takeExtension sourcePath of
        ".jpg" -> saveJpgImage 90 destPath (ImageRGB8 scaled)
        ".jpeg" -> saveJpgImage 90 destPath (ImageRGB8 scaled)
        _ -> savePngImage destPath (ImageRGB8 scaled)
      case saveResult of
        Left (e :: SomeException) -> pure $ Left $ "Failed to save thumbnail: " <> show e
        Right () -> pure $ Right ()

-- | Simple nearest-neighbor scaling for thumbnails.
scaleNearestNeighbor :: Image PixelRGB8 -> Int -> Int -> Image PixelRGB8
scaleNearestNeighbor img newWidth newHeight =
  generateImage getPixel newWidth newHeight
  where
    srcWidth = imageWidth img
    srcHeight = imageHeight img
    scaleX :: Double = fromIntegral srcWidth / fromIntegral newWidth
    scaleY :: Double = fromIntegral srcHeight / fromIntegral newHeight

    getPixel x y =
      let srcX = floor (fromIntegral x * scaleX)
          srcY = floor (fromIntegral y * scaleY)
          -- Clamp to image bounds
          srcX' = min (srcWidth - 1) (max 0 srcX)
          srcY' = min (srcHeight - 1) (max 0 srcY)
      in pixelAt img srcX' srcY'
