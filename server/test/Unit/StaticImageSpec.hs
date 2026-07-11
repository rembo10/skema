{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for 'missingImageEvent', the pure path parser behind the
-- static image server's "missing file → re-fetch" hook.
--
-- The static server serves images from paths like @artists/<mbid>.jpg@ and
-- @albums/<mbid>_thumb.jpg@. When a file is missing it maps the path back to
-- the artist/album it belongs to so the image service can re-fetch it. The
-- fiddly bits — stripping the @_thumb@ suffix and the extension, ignoring
-- unrelated paths — are exactly what these tests pin down.
module Unit.StaticImageSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Skema.API.Handlers.Static (missingImageEvent)
import Skema.Events.Types (Event(..))

tests :: TestTree
tests = testGroup "Unit.StaticImage"
  [ testCase "artist full-size image maps to ArtistImageMissing" $
      missingImageEvent ["artists", "abc-123.jpg"]
        @?= Just (ArtistImageMissing "abc-123")

  , testCase "artist thumbnail strips the _thumb suffix" $
      missingImageEvent ["artists", "abc-123_thumb.jpg"]
        @?= Just (ArtistImageMissing "abc-123")

  , testCase "album cover maps to AlbumCoverMissing" $
      missingImageEvent ["albums", "rg-9.png"]
        @?= Just (AlbumCoverMissing "rg-9")

  , testCase "album thumbnail strips the _thumb suffix" $
      missingImageEvent ["albums", "rg-9_thumb.jpg"]
        @?= Just (AlbumCoverMissing "rg-9")

  , testCase "extra path segments after the file name are ignored" $
      missingImageEvent ["artists", "abc.jpg", "extra"]
        @?= Just (ArtistImageMissing "abc")

  , testCase "unrecognised category yields no event" $
      missingImageEvent ["logos", "abc.jpg"]
        @?= Nothing

  , testCase "missing file-name segment yields no event" $
      missingImageEvent ["artists"]
        @?= Nothing

  , testCase "empty path yields no event" $
      missingImageEvent []
        @?= Nothing

  , testCase "empty MBID (dotfile) yields no event" $
      missingImageEvent ["artists", ".jpg"]
        @?= Nothing
  ]
