{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Skema.Domain.Quality pure quality profile logic.
--
-- Tests:
-- - parseQuality: text to quality parsing
-- - parseQualityFromTitle: quality extraction from release titles
-- - meetsProfile: quality against profile validation
-- - needsUpgrade: quality below cutoff detection
-- - shouldUpgrade: upgrade decision with auto-upgrade flag
-- - selectBestQuality: best quality selection from candidates
module Unit.Domain.QualitySpec
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Skema.Domain.Quality

-- =============================================================================
-- Test Profile Helper
-- =============================================================================

-- | A standard quality profile for testing.
--
-- Cutoff: Lossless
-- Enabled: MP3_320 (rank 1), Lossless (rank 2), HiResLossless (rank 3)
-- Upgrade: enabled
testProfile :: QualityProfile
testProfile = QualityProfile
  { qfId = Just 1
  , qfName = "Test Profile"
  , qfQualityPreferences =
      [ QualityPreference MP3_320 1 True
      , QualityPreference Lossless 2 True
      , QualityPreference HiResLossless 3 True
      ]
  , qfCutoffQuality = Lossless
  , qfUpgradeAutomatically = True
  }

-- | Profile with upgrade disabled.
noUpgradeProfile :: QualityProfile
noUpgradeProfile = testProfile { qfUpgradeAutomatically = False }

-- =============================================================================
-- Tests
-- =============================================================================

tests :: TestTree
tests = testGroup "Unit.Domain.Quality"
  [ parseQualityTests
  , parseQualityFromTitleTests
  , meetsProfileTests
  , needsUpgradeTests
  , shouldUpgradeTests
  , selectBestQualityTests
  ]

-- =============================================================================
-- parseQuality Tests
-- =============================================================================

parseQualityTests :: TestTree
parseQualityTests = testGroup "parseQuality"
  [ testCase "FLAC -> Just Lossless" $
      parseQuality "FLAC" @?= Just Lossless

  , testCase "320 -> Just MP3_320" $
      parseQuality "320" @?= Just MP3_320

  , testCase "V0 -> Just VBR0" $
      parseQuality "V0" @?= Just VBR0

  , testCase "24bit FLAC -> Just HiResLossless" $
      parseQuality "24bit FLAC" @?= Just HiResLossless

  , testCase "random text -> Nothing" $
      parseQuality "random text" @?= Nothing
  ]

-- =============================================================================
-- parseQualityFromTitle Tests
-- =============================================================================

parseQualityFromTitleTests :: TestTree
parseQualityFromTitleTests = testGroup "parseQualityFromTitle"
  [ testCase "Artist - Album [FLAC] -> Lossless" $
      parseQualityFromTitle "Artist - Album [FLAC]" @?= Lossless

  , testCase "Artist - Album [320] -> MP3_320" $
      parseQualityFromTitle "Artist - Album [320]" @?= MP3_320

  , testCase "Artist - Album (no quality tag) -> Unknown" $
      parseQualityFromTitle "Artist - Album" @?= Unknown
  ]

-- =============================================================================
-- meetsProfile Tests
-- =============================================================================

meetsProfileTests :: TestTree
meetsProfileTests = testGroup "meetsProfile"
  [ testCase "enabled quality -> True" $
      meetsProfile Lossless testProfile @?= True

  , testCase "disabled quality -> False" $ do
      let profile = testProfile
            { qfQualityPreferences =
                [ QualityPreference MP3_320 1 True
                , QualityPreference Lossless 2 False  -- Disabled
                , QualityPreference HiResLossless 3 True
                ]
            }
      meetsProfile Lossless profile @?= False

  , testCase "quality not in profile -> False" $
      meetsProfile VBR0 testProfile @?= False
  ]

-- =============================================================================
-- needsUpgrade Tests
-- =============================================================================

needsUpgradeTests :: TestTree
needsUpgradeTests = testGroup "needsUpgrade"
  [ testCase "quality below cutoff -> True" $
      needsUpgrade MP3_320 testProfile @?= True

  , testCase "quality at cutoff -> False" $
      needsUpgrade Lossless testProfile @?= False

  , testCase "quality above cutoff -> False" $
      needsUpgrade HiResLossless testProfile @?= False
  ]

-- =============================================================================
-- shouldUpgrade Tests
-- =============================================================================

shouldUpgradeTests :: TestTree
shouldUpgradeTests = testGroup "shouldUpgrade"
  [ testCase "upgrade enabled + below cutoff -> True" $
      shouldUpgrade MP3_320 testProfile @?= True

  , testCase "upgrade disabled -> False" $
      shouldUpgrade MP3_320 noUpgradeProfile @?= False
  ]

-- =============================================================================
-- selectBestQuality Tests
-- =============================================================================

selectBestQualityTests :: TestTree
selectBestQualityTests = testGroup "selectBestQuality"
  [ testCase "picks highest ranked enabled quality" $ do
      let candidates = [MP3_320, Lossless, HiResLossless]
      selectBestQuality candidates testProfile @?= Just HiResLossless

  , testCase "ignores qualities not in profile" $ do
      let candidates = [VBR0, MP3_320]
      selectBestQuality candidates testProfile @?= Just MP3_320

  , testCase "returns Nothing when no candidates meet profile" $ do
      let candidates = [VBR0, MP3_192]
      selectBestQuality candidates testProfile @?= Nothing
  ]
