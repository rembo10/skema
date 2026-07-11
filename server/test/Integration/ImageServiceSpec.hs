{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Integration tests for the image service's self-healing repair path.
--
-- When the on-disk image cache is deleted the database still holds a non-null
-- @image_url@ that points at a now-missing file. 'repairArtistImage' /
-- 'repairAlbumCover' reconcile that: they null the stale URLs, re-fetch the
-- artwork from the (mocked) providers, download it into the cache, write the
-- fresh local paths back to the database, and emit the usual
-- 'ArtistImageFetched' / 'AlbumCoverFetched' event so the UI updates over SSE.
--
-- HTTP is mocked, so no network is touched: the provider lookup and the image
-- download both resolve against canned responses.
module Integration.ImageServiceSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Control.Concurrent.STM as STM
import Data.Aeson (Value, object, (.=))
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import qualified Database.SQLite.Simple as SQLite

import Helpers.TestEnv (TestEnv(..), withTestEnvMock)
import Helpers.EventAssertions (waitForEventWithTimeout)
import Helpers.MockHttp (RequestMatcher, respond, hostIs, jsonResponse, statusResponse)

import Skema.Database.Connection (withConnection)
import Skema.Database.Repository
  ( upsertCatalogArtist
  , upsertCatalogAlbum
  , getCatalogArtistByMBID
  , getCatalogAlbumByReleaseGroupMBID
  )
import qualified Skema.Database.Types as DB
import Skema.Events.Bus (subscribe)
import Skema.Events.Types (Event(..))
import Skema.Services.Dependencies (ImageDeps(..))
import Skema.Services.Image (repairArtistImage, repairAlbumCover)

tests :: TestTree
tests = testGroup "Integration.ImageService"
  [ testCase "repairs a missing artist image: nulls stale URL, re-fetches, emits event" testRepairArtist
  , testCase "repairs a missing album cover: re-fetches full + thumbnail" testRepairAlbum
  ]

-- Fixtures ------------------------------------------------------------------

artistMbid :: Text
artistMbid = "artist-mbid-1"

rgMbid :: Text
rgMbid = "rg-mbid-1"

-- | Build image deps from a test env (mirrors 'ImageDeps' field-for-field).
mkImageDeps :: TestEnv -> ImageDeps
mkImageDeps env = ImageDeps
  { imgEventBus = teEventBus env
  , imgLogEnv = teLogEnv env
  , imgDbPool = tePool env
  , imgConfigVar = teConfigVar env
  , imgHttpClient = teHttpClient env
  , imgCacheDir = teCacheDir env
  }

isArtistImageFetched :: Event -> Bool
isArtistImageFetched = \case
  ArtistImageFetched{} -> True
  _ -> False

isAlbumCoverFetched :: Event -> Bool
isAlbumCoverFetched = \case
  AlbumCoverFetched{} -> True
  _ -> False

-- | fanart.tv artist response pointing at a CDN thumbnail we also mock.
fanartArtistJson :: Value
fanartArtistJson = object
  [ "artistthumb" .= [ object ["url" .= ("https://assets.fanart.tv/artistthumb/img.jpg" :: Text)] ]
  ]

-- | Cover Art Archive response with a front cover and a large thumbnail.
coverArtJson :: Value
coverArtJson = object
  [ "images" .=
      [ object
          [ "front" .= True
          , "image" .= ("https://ia.coverartarchive.org/front.jpg" :: Text)
          , "thumbnails" .= object
              [ "large" .= ("https://ia.coverartarchive.org/large.jpg" :: Text) ]
          ]
      ]
  ]

-- fanart.tv is the first artist-image source in the waterfall; a match there
-- means the rest (Last.fm, MusicBrainz) are never contacted. The CDN matcher
-- serves the image bytes for the subsequent download.
artistMatchers :: [RequestMatcher]
artistMatchers =
  [ respond (hostIs "webservice.fanart.tv") (jsonResponse fanartArtistJson)
  , respond (hostIs "assets.fanart.tv") (statusResponse 200 "fake-image-bytes")
  ]

-- Cover Art Archive is the first album-cover source; the ia.coverartarchive.org
-- matcher serves both the full-size and thumbnail downloads.
albumMatchers :: [RequestMatcher]
albumMatchers =
  [ respond (hostIs "coverartarchive.org") (jsonResponse coverArtJson)
  , respond (hostIs "ia.coverartarchive.org") (statusResponse 200 "fake-image-bytes")
  ]

-- Tests ---------------------------------------------------------------------

testRepairArtist :: IO ()
testRepairArtist = withTestEnvMock artistMatchers $ \env -> do
  -- Seed a followed artist whose image_url points at a file that isn't on disk.
  withConnection (tePool env) $ \conn -> do
    _ <- upsertCatalogArtist conn artistMbid "Artist"
      Nothing (Just "/images/artists/stale.jpg") (Just "/images/artists/stale_thumb.jpg")
      True Nothing Nothing Nothing
    pure ()

  eventChan <- STM.atomically $ subscribe (teEventBus env)
  repairArtistImage (mkImageDeps env) artistMbid

  -- Database now holds the freshly-fetched local path (thumbnail was not
  -- provided by fanart.tv, so it is cleared to Nothing).
  Just artist <- withConnection (tePool env) $ \conn ->
    getCatalogArtistByMBID conn artistMbid
  DB.catalogArtistImageUrl artist @?= Just ("/images/artists/" <> artistMbid <> ".jpg")
  DB.catalogArtistThumbnailUrl artist @?= Nothing

  -- The image was actually written into the cache directory.
  onDisk <- doesFileExist (teCacheDir env </> "images" </> "artists" </> toString artistMbid <> ".jpg")
  assertBool "artist image file exists in the cache" onDisk

  fetched <- waitForEventWithTimeout eventChan isArtistImageFetched (500 * 1000)
  assertBool "ArtistImageFetched should be emitted after repair" (isJust fetched)

testRepairAlbum :: IO ()
testRepairAlbum = withTestEnvMock albumMatchers $ \env -> do
  -- Seed an album with stale cover URLs pointing at missing files.
  withConnection (tePool env) $ \conn -> do
    artistId <- upsertCatalogArtist conn artistMbid "Artist"
      Nothing Nothing Nothing True Nothing Nothing Nothing
    albumId <- upsertCatalogAlbum conn rgMbid "Album" artistId artistMbid "Artist" Nothing Nothing
    SQLite.execute conn
      "UPDATE catalog_albums SET album_cover_url = ?, album_cover_thumbnail_url = ? WHERE id = ?"
      ("/images/albums/stale.jpg" :: Text, "/images/albums/stale_thumb.jpg" :: Text, albumId)

  eventChan <- STM.atomically $ subscribe (teEventBus env)
  repairAlbumCover (mkImageDeps env) rgMbid

  Just album <- withConnection (tePool env) $ \conn ->
    getCatalogAlbumByReleaseGroupMBID conn rgMbid
  DB.catalogAlbumCoverUrl album @?= Just ("/images/albums/" <> rgMbid <> ".jpg")
  DB.catalogAlbumCoverThumbnailUrl album @?= Just ("/images/albums/" <> rgMbid <> "_thumb.jpg")

  fullOnDisk <- doesFileExist (teCacheDir env </> "images" </> "albums" </> toString rgMbid <> ".jpg")
  thumbOnDisk <- doesFileExist (teCacheDir env </> "images" </> "albums" </> toString rgMbid <> "_thumb.jpg")
  assertBool "album cover file exists in the cache" fullOnDisk
  assertBool "album thumbnail file exists in the cache" thumbOnDisk

  fetched <- waitForEventWithTimeout eventChan isAlbumCoverFetched (500 * 1000)
  assertBool "AlbumCoverFetched should be emitted after repair" (isJust fetched)
