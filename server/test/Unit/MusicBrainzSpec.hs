{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the MusicBrainz client driven by a mock HTTP transport.
--
-- These tests exercise the full client stack (rate limiting, retries,
-- auth, JSON decoding) without hitting the network. They also serve as
-- a proof that the 'HttpTransport' seam works end-to-end.
module Unit.MusicBrainzSpec
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), object)

import qualified Skema.MusicBrainz.Client as MB
import Skema.MusicBrainz.Types (MBReleaseSearch(..), MBRelease(..), MBID(..))
import qualified Skema.HTTP.Client as HTTP
import Helpers.TestEnv (TestEnv(..), withTestEnvMock)
import Helpers.MockHttp (respond, hostIs, pathIs, pathStartsWith, (.&.), jsonResponse, statusResponse)

tests :: TestTree
tests = testGroup "Unit.MusicBrainz"
  [ searchReleasesTests
  , releaseRoundTripTests
  ]

releaseRoundTripTests :: TestTree
releaseRoundTripTests = testGroup "MBRelease JSON round-trip"
  [ testCase "multi-artist credit survives encode/decode" $ do
      let creditJson = object
            [ "id" .= ("release-1" :: Text)
            , "title" .= ("Watch the Throne" :: Text)
            , "artist-credit" .=
                [ object
                    [ "name" .= ("JAY-Z" :: Text)
                    , "joinphrase" .= (" & " :: Text)
                    , "artist" .= object [ "id" .= ("artist-jz" :: Text) ]
                    ]
                , object
                    [ "name" .= ("Kanye West" :: Text)
                    , "joinphrase" .= ("" :: Text)
                    , "artist" .= object [ "id" .= ("artist-kw" :: Text) ]
                    ]
                ]
            , "media" .= ([] :: [Aeson.Value])
            ]
      case Aeson.fromJSON creditJson :: Aeson.Result MBRelease of
        Aeson.Error err -> assertFailure $ "decode failed: " <> err
        Aeson.Success release -> do
          assertEqual "joined artist" "JAY-Z & Kanye West" (mbReleaseArtist release)
          assertEqual "credit count" 2 (length (mbReleaseArtists release))
          -- Re-encode and decode: nothing may be lost
          case Aeson.fromJSON (Aeson.toJSON release) :: Aeson.Result MBRelease of
            Aeson.Error err -> assertFailure $ "re-decode failed: " <> err
            Aeson.Success roundTripped -> assertEqual "round-trip" release roundTripped
  ]

searchReleasesTests :: TestTree
searchReleasesTests = testGroup "searchReleases"
  [ testCase "decodes a successful search response" $ do
      let mockBody = object
            [ "count" .= (1 :: Int)
            , "releases" .=
                [ object
                    [ "id" .= ("abcd-efgh" :: Text)
                    , "title" .= ("OK Computer" :: Text)
                    , "status" .= Aeson.Null
                    , "date" .= ("1997-06-16" :: Text)
                    , "artist-credit" .=
                        [ object
                            [ "name" .= ("Radiohead" :: Text)
                            , "artist" .= object [ "id" .= ("bcde" :: Text), "name" .= ("Radiohead" :: Text) ]
                            ]
                        ]
                    , "media" .= ([] :: [Aeson.Value])
                    ]
                ]
            ]
          matchers =
            [ respond (hostIs "musicbrainz.org" .&. pathStartsWith "/ws/2/release") (jsonResponse mockBody)
            ]
      withTestEnvMock matchers $ \env -> do
        result <- MB.searchReleases (teMBClientEnv env) "release:\"OK Computer\" AND artist:Radiohead" (Just 1) Nothing False
        case result of
          Left err -> assertFailure $ "Expected success, got: " <> show err
          Right search -> do
            assertEqual "count" 1 (mbSearchCount search)
            case mbSearchReleases search of
              [release] -> do
                assertEqual "title" "OK Computer" (mbReleaseTitle release)
                assertEqual "release id" (MBID "abcd-efgh") (mbReleaseId release)
              _ -> assertFailure "expected exactly one release"

  , testCase "maps a 404 to an MBHttpError" $ do
      let matchers =
            [ respond (hostIs "musicbrainz.org" .&. pathIs "/ws/2/release")
                (statusResponse 404 "not found")
            ]
      withTestEnvMock matchers $ \env -> do
        result <- MB.searchReleases (teMBClientEnv env) "query" Nothing Nothing False
        case result of
          Right _ -> assertFailure "Expected failure on 404"
          Left (MB.MBHttpError (HTTP.HttpStatusError code _ _)) ->
            assertEqual "status code" 404 code
          Left err -> assertFailure $ "Expected HttpStatusError 404, got: " <> show err
  ]
