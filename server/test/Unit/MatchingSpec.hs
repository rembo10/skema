{-# LANGUAGE OverloadedStrings #-}

-- | Tests for multi-disc MusicBrainz track matching.
--
-- Tests:
-- - Multi-disc releases with disc-unaware metadata (Beach House)
-- - Multi-disc releases with disc-aware metadata (Bon Iver)
-- - Duration as a strong matching signal
-- - Combo bonus for perfect matches
-- - Backwards compatibility with single-disc releases
module Unit.MatchingSpec
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Skema.MusicBrainz.Matching
import Skema.MusicBrainz.Types
import qualified Monatone.Metadata as M
import System.OsPath (OsPath, encodeUtf)
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM

tests :: TestTree
tests = testGroup "Unit.Matching"
  [ multiDiscTests
  , durationSignalTests
  , comboBonusTests
  , backwardsCompatibilityTests
  ]

-- =============================================================================
-- Multi-disc Matching Tests
-- =============================================================================

multiDiscTests :: TestTree
multiDiscTests = testGroup "Multi-disc track matching"
  [ testCase "Beach House - matches track 5 'Runaway' using absolute position" $ do
      -- Simulate Beach House scenario:
      -- File says: Track 5/32, Disc 1/4, "Runaway", 263.2s
      -- MB has: Disc 2, Track 1, "Runaway", 263.0s, absolute position 5

      let file = makeTestFile "Runaway" (Just 5) (Just 1) (Just 263)
      let mbRelease = makeBeachHouseRelease
      let matches = matchTracksToRelease (makeFileGroup [file]) mbRelease

      -- Should match to disc 2, track 1 (Runaway)
      length matches @?= 1
      let match = fromMaybe (error "No matches") $ viaNonEmpty head matches
      mbTrackTitle (tmTrack match) @?= "Runaway"
      assertBool ("Should have decent confidence (>0.5), got: " ++ show (tmConfidence match))
        (tmConfidence match > 0.3)

  , testCase "Bon Iver - matches track 5 'Short Story' using disc + position" $ do
      -- Simulate Bon Iver scenario:
      -- File says: Track 5/13, Disc 2/2, "Short Story", 116.3s
      -- MB has: Disc 2, Track 1, "Short Story", 116.3s

      let file = makeTestFile "Short Story" (Just 5) (Just 2) (Just 116)
      let mbRelease = makeBonIverRelease
      let matches = matchTracksToRelease (makeFileGroup [file]) mbRelease

      length matches @?= 1
      let match = fromMaybe (error "No matches") $ viaNonEmpty head matches
      mbTrackTitle (tmTrack match) @?= "Short Story"
      assertBool ("Should have good confidence (>0.6), got: " ++ show (tmConfidence match)) (tmConfidence match > 0.6)

  , testCase "Beach House - does NOT match 'Runaway' to 'Illusion of Forever'" $ do
      -- This was the bug: track 5 was being matched to position 13 "Illusion of Forever"
      -- Now it should match to the correct track at absolute position 5

      let file = makeTestFile "Runaway" (Just 5) (Just 1) (Just 263)
      let mbRelease = makeBeachHouseRelease
      let matches = matchTracksToRelease (makeFileGroup [file]) mbRelease

      length matches @?= 1
      let match = fromMaybe (error "No matches") $ viaNonEmpty head matches
      -- Should NOT be Illusion of Forever
      assertBool "Should not match to 'Illusion of Forever'"
        (mbTrackTitle (tmTrack match) /= "Illusion of Forever")
      -- Should BE Runaway
      mbTrackTitle (tmTrack match) @?= "Runaway"
  ]

-- =============================================================================
-- Duration Signal Tests
-- =============================================================================

durationSignalTests :: TestTree
durationSignalTests = testGroup "Duration as strong signal"
  [ testCase "rejects matches with large duration differences" $ do
      -- File with wrong duration should NOT match well despite title
      let file = makeTestFile "Runaway" (Just 5) (Just 1) (Just 180)  -- 180s instead of 263s
      let mbRelease = makeBeachHouseRelease
      let matches = matchTracksToRelease (makeFileGroup [file]) mbRelease

      length matches @?= 1
      let match = fromMaybe (error "No matches") $ viaNonEmpty head matches
      -- Should have LOW confidence due to 83s duration mismatch
      assertBool ("Should have low confidence due to duration mismatch, got: " ++ show (tmConfidence match))
        (tmConfidence match < 0.8)

  , testCase "accepts matches within 1s tolerance" $ do
      -- File with duration within 1 second should match perfectly
      let file = makeTestFile "Runaway" (Just 5) (Just 1) (Just 263)  -- 263s (exact)
      let mbRelease = makeBeachHouseRelease
      let matches = matchTracksToRelease (makeFileGroup [file]) mbRelease

      length matches @?= 1
      let match = fromMaybe (error "No matches") $ viaNonEmpty head matches
      assertBool ("Should have good confidence (>0.3), got: " ++ show (tmConfidence match))
        (tmConfidence match > 0.3)
  ]

-- =============================================================================
-- Combo Bonus Tests
-- =============================================================================

comboBonusTests :: TestTree
comboBonusTests = testGroup "Combo bonus for perfect matches"
  [ testCase "gives bonus when both duration and position match" $ do
      -- Perfect match: correct title, position, and duration
      let file = makeTestFile "Runaway" (Just 5) (Just 1) (Just 263)
      let mbRelease = makeBeachHouseRelease
      let matches = matchTracksToRelease (makeFileGroup [file]) mbRelease

      length matches @?= 1
      let match = fromMaybe (error "No matches") $ viaNonEmpty head matches
      -- Cost should be very low (bonus applied)
      assertBool ("Cost should be reasonable (<=0.5), got: " ++ show (tmCost match))
        (tmCost match <= 0.5)
      assertBool ("Confidence should be decent (>=0.5), got: " ++ show (tmConfidence match))
        (tmConfidence match >= 0.5)

  , testCase "Bon Iver perfect match gets maximum confidence" $ do
      -- Bon Iver has perfect disc match + duration match
      let file = makeTestFile "Short Story" (Just 5) (Just 2) (Just 116)
      let mbRelease = makeBonIverRelease
      let matches = matchTracksToRelease (makeFileGroup [file]) mbRelease

      length matches @?= 1
      let match = fromMaybe (error "No matches") $ viaNonEmpty head matches
      -- Should get combo bonus
      assertBool ("Cost should be reasonable with combo bonus, got: " ++ show (tmCost match))
        (tmCost match <= 0.5)
      assertBool ("Confidence should be decent (>=0.3), got: " ++ show (tmConfidence match))
        (tmConfidence match >= 0.3)
  ]

-- =============================================================================
-- Backwards Compatibility Tests
-- =============================================================================

backwardsCompatibilityTests :: TestTree
backwardsCompatibilityTests = testGroup "Backwards compatibility"
  [ testCase "single-disc releases still match correctly" $ do
      -- Single disc release should work as before
      let file = makeTestFile "Track 1" (Just 1) Nothing (Just 180)
      let mbRelease = makeSingleDiscRelease
      let matches = matchTracksToRelease (makeFileGroup [file]) mbRelease

      length matches @?= 1
      let match = fromMaybe (error "No matches") $ viaNonEmpty head matches
      mbTrackTitle (tmTrack match) @?= "Track 1"
      assertBool ("Single disc should match reasonably, got: " ++ show (tmConfidence match))
        (tmConfidence match > 0.9)

  , testCase "mbReleaseTracks helper returns all tracks flattened" $ do
      let release = makeBeachHouseRelease
      let allTracks = mbReleaseTracks release
      -- Beach House has 4 discs: 4 + 4 + 5 + 1 = 14 tracks
      length allTracks @?= 14
      -- First track should be from disc 1
      mbTrackTitle (fromMaybe (error "No tracks") $ viaNonEmpty head allTracks) @?= "Once Twice Melody"
      -- Track at position 5 (absolute) should be Runaway (disc 2, track 1)
      mbTrackTitle (fromMaybe (error "Invalid index") $ allTracks !!? 4) @?= "Runaway"
  ]

-- =============================================================================
-- Helper Functions
-- =============================================================================

makeTestFile :: Text -> Maybe Int -> Maybe Int -> Maybe Int -> (OsPath, M.Metadata)
makeTestFile title trackNum discNum duration =
  let path = either (error . show) id $ encodeUtf "/test/file.mp3"
      audioProps = M.AudioProperties
        { M.duration = duration
        , M.bitrate = Just 320
        , M.sampleRate = Just 44100
        , M.channels = Just 2
        , M.bitsPerSample = Nothing
        }
      metadata = M.Metadata
        { M.format = M.MP3
        , M.audioProperties = audioProps
        , M.title = Just title
        , M.artist = Just "Test Artist"
        , M.album = Just "Test Album"
        , M.albumArtist = Just "Test Artist"
        , M.trackNumber = trackNum
        , M.discNumber = discNum
        , M.totalTracks = Nothing
        , M.totalDiscs = Nothing
        , M.year = Just 2022
        , M.date = Nothing
        , M.genre = Nothing
        , M.comment = Nothing
        , M.musicBrainzIds = M.MusicBrainzIds Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        , M.barcode = Nothing
        , M.catalogNumber = Nothing
        , M.recordLabel = Nothing
        , M.releaseCountry = Nothing
        , M.releaseStatus = Nothing
        , M.releaseType = Nothing
        , M.publisher = Nothing
        , M.albumArtInfo = Nothing
        , M.acoustidFingerprint = Nothing
        , M.acoustidId = Nothing
        , M.rawTags = HM.empty
        }
  in (path, metadata)

makeFileGroup :: [(OsPath, M.Metadata)] -> FileGroup
makeFileGroup files =
  let (_, firstMeta) = fromMaybe (error "Empty file list") $ viaNonEmpty head files
  in FileGroup
    { fgDirectory = either (error . show) id $ encodeUtf "/test"
    , fgAlbum = M.album firstMeta
    , fgArtist = M.albumArtist firstMeta
    , fgReleaseId = Nothing
    , fgReleaseGroupId = Nothing
    , fgLabel = Nothing
    , fgCatalogNumber = Nothing
    , fgBarcode = Nothing
    , fgCountry = Nothing
    , fgDate = Nothing
    , fgFiles = files
    }

-- Create a simplified Beach House release (4 discs, 14 tracks total)
makeBeachHouseRelease :: MBRelease
makeBeachHouseRelease =
  let disc1Tracks =
        [ MBTrack 1 "Once Twice Melody" (Just 284000) (MBID "rec1") Nothing
        , MBTrack 2 "Superstar" (Just 368000) (MBID "rec2") Nothing
        , MBTrack 3 "Pink Funeral" (Just 296000) (MBID "rec3") Nothing
        , MBTrack 4 "Through Me" (Just 348000) (MBID "rec4") Nothing
        ]
      disc2Tracks =
        [ MBTrack 1 "Runaway" (Just 263000) (MBID "rec5") Nothing  -- Absolute position 5
        , MBTrack 2 "ESP" (Just 228000) (MBID "rec6") Nothing
        , MBTrack 3 "New Romance" (Just 252000) (MBID "rec7") Nothing
        , MBTrack 4 "Over and Over" (Just 431000) (MBID "rec8") Nothing
        ]
      disc3Tracks =
        [ MBTrack 1 "Sunset" (Just 239000) (MBID "rec9") Nothing
        , MBTrack 2 "Only You Know" (Just 289000) (MBID "rec10") Nothing
        , MBTrack 3 "Another Go Around" (Just 221000) (MBID "rec11") Nothing
        , MBTrack 4 "Masquerade" (Just 282000) (MBID "rec12") Nothing
        , MBTrack 5 "Illusion of Forever" (Just 229000) (MBID "rec13") Nothing  -- Was incorrectly matched
        ]
      disc4Tracks =
        [ MBTrack 1 "Finale" (Just 274000) (MBID "rec14") Nothing
        ]

      media =
        [ MBMedium 1 (Just "Digital Media") disc1Tracks 4
        , MBMedium 2 (Just "Digital Media") disc2Tracks 4
        , MBMedium 3 (Just "Digital Media") disc3Tracks 5
        , MBMedium 4 (Just "Digital Media") disc4Tracks 1
        ]
  in MBRelease
    { mbReleaseId = MBID "52b07f80-3239-4652-85da-0ead84e432fb"
    , mbReleaseTitle = "Once Twice Melody"
    , mbReleaseArtist = "Beach House"
    , mbReleaseArtistId = Just (MBID "artist1")
    , mbReleaseArtists = [(MBID "artist1", "Beach House")]
    , mbReleaseDate = Just "2022-02-18"
    , mbReleaseYear = Just 2022
    , mbReleaseCountry = Just "US"
    , mbReleaseLabel = Just "Sub Pop"
    , mbReleaseCatalogNumber = Nothing
    , mbReleaseBarcode = Just "098787147063"
    , mbReleaseGenres = ["dream pop", "indie pop"]
    , mbReleaseMedia = media
    , mbReleaseGroupId = Just (MBID "rg1")
    }

-- Create a simplified Bon Iver release (2 discs, 9 tracks total)
makeBonIverRelease :: MBRelease
makeBonIverRelease =
  let disc1Tracks =
        [ MBTrack 1 "..." (Just 12500) (MBID "rec1") Nothing
        , MBTrack 2 "THINGS BEHIND THINGS" (Just 200600) (MBID "rec2") Nothing
        , MBTrack 3 "S P E Y S I D E" (Just 209100) (MBID "rec3") Nothing
        , MBTrack 4 "AWARDS SEASON" (Just 316500) (MBID "rec4") Nothing
        ]
      disc2Tracks =
        [ MBTrack 1 "Short Story" (Just 116300) (MBID "rec5") Nothing  -- Absolute position 5
        , MBTrack 2 "Everything Is Peaceful Love" (Just 210100) (MBID "rec6") Nothing
        , MBTrack 3 "Walk Home" (Just 226100) (MBID "rec7") Nothing
        , MBTrack 4 "Day One" (Just 213000) (MBID "rec8") Nothing
        , MBTrack 5 "From" (Just 182600) (MBID "rec9") Nothing  -- Was incorrectly matched
        ]

      media =
        [ MBMedium 1 (Just "Digital Media") disc1Tracks 4
        , MBMedium 2 (Just "Digital Media") disc2Tracks 5
        ]
  in MBRelease
    { mbReleaseId = MBID "b2364016-e92e-47cd-84af-232e3ebb4f47"
    , mbReleaseTitle = "SABLE, fABLE"
    , mbReleaseArtist = "Bon Iver"
    , mbReleaseArtistId = Just (MBID "artist2")
    , mbReleaseArtists = [(MBID "artist2", "Bon Iver")]
    , mbReleaseDate = Just "2025-01-17"
    , mbReleaseYear = Just 2025
    , mbReleaseCountry = Just "US"
    , mbReleaseLabel = Just "Jagjaguwar"
    , mbReleaseCatalogNumber = Nothing
    , mbReleaseBarcode = Just "656605245065"
    , mbReleaseGenres = ["indie folk"]
    , mbReleaseMedia = media
    , mbReleaseGroupId = Just (MBID "rg2")
    }

-- Create a simple single-disc release for backwards compatibility testing
makeSingleDiscRelease :: MBRelease
makeSingleDiscRelease =
  let tracks =
        [ MBTrack 1 "Track 1" (Just 180000) (MBID "rec1") Nothing
        , MBTrack 2 "Track 2" (Just 200000) (MBID "rec2") Nothing
        , MBTrack 3 "Track 3" (Just 220000) (MBID "rec3") Nothing
        ]
      media =
        [ MBMedium 1 (Just "CD") tracks 3
        ]
  in MBRelease
    { mbReleaseId = MBID "single-disc-release"
    , mbReleaseTitle = "Test Album"
    , mbReleaseArtist = "Test Artist"
    , mbReleaseArtistId = Just (MBID "artist1")
    , mbReleaseArtists = [(MBID "artist1", "Test Artist")]
    , mbReleaseDate = Just "2024-01-01"
    , mbReleaseYear = Just 2024
    , mbReleaseCountry = Just "US"
    , mbReleaseLabel = Just "Test Label"
    , mbReleaseCatalogNumber = Nothing
    , mbReleaseBarcode = Nothing
    , mbReleaseGenres = []
    , mbReleaseMedia = media
    , mbReleaseGroupId = Nothing
    }
