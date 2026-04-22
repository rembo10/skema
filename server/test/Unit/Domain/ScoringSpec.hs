{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Skema.Domain.Scoring pure ranking logic.
module Unit.Domain.ScoringSpec
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Skema.Domain.Scoring
import Skema.Indexer.Types (ReleaseInfo(..), DownloadType(..))
import qualified Skema.Domain.Quality as Q

-- =============================================================================
-- Builders
-- =============================================================================

-- | A bare release with neutral scoring values.
baseRelease :: ReleaseInfo
baseRelease = ReleaseInfo
  { riTitle = "Artist - Album"
  , riGuid = Nothing
  , riDownloadUrl = "http://example.com/x"
  , riInfoUrl = Nothing
  , riSize = Nothing
  , riPublishDate = Nothing
  , riCategory = Nothing
  , riSeeders = Nothing
  , riPeers = Nothing
  , riGrabs = Nothing
  , riDownloadType = NZB
  , riQuality = Q.Unknown
  , riSlskdUsername = Nothing
  , riSlskdFiles = Nothing
  }

-- =============================================================================
-- Tests
-- =============================================================================

tests :: TestTree
tests = testGroup "Unit.Domain.Scoring"
  [ formatScoreTests
  , qualityBonusTests
  , seedScoreTests
  , sizeScoreTests
  , grabScoreTests
  , titleMatchTests
  , rankTests
  ]

-- =============================================================================
-- Format (download type) score
-- =============================================================================

formatScoreTests :: TestTree
formatScoreTests = testGroup "format score (by download type)"
  [ testCase "NZB = 500" $
      scoreRelease Nothing baseRelease @?= 500

  , testCase "Torrent with >10 seeders = 400 + seed score" $
      -- 400 (format) + min 500 (20*10=200) seedScore = 600
      scoreRelease Nothing baseRelease { riDownloadType = Torrent, riSeeders = Just 20 }
        @?= 600

  , testCase "Torrent with 1-10 seeders = 200 + seed score" $
      -- 200 + 10 = 210
      scoreRelease Nothing baseRelease { riDownloadType = Torrent, riSeeders = Just 1 }
        @?= 210

  , testCase "Torrent with no seeders = 50" $
      scoreRelease Nothing baseRelease { riDownloadType = Torrent } @?= 50

  , testCase "Slskd with >5 seeders = 350 + seed score" $
      -- 350 + 60 = 410
      scoreRelease Nothing baseRelease { riDownloadType = Slskd, riSeeders = Just 6 }
        @?= 410

  , testCase "Slskd with 1-5 seeders = 150 + seed score" $
      -- 150 + 10 = 160
      scoreRelease Nothing baseRelease { riDownloadType = Slskd, riSeeders = Just 1 }
        @?= 160

  , testCase "Slskd baseline (no seeder info) = 100" $
      scoreRelease Nothing baseRelease { riDownloadType = Slskd } @?= 100
  ]

-- =============================================================================
-- Quality bonus (from title tags)
-- =============================================================================

qualityBonusTests :: TestTree
qualityBonusTests = testGroup "quality bonus from title"
  [ testCase "FLAC in title adds 1000" $
      scoreRelease Nothing baseRelease { riTitle = "Artist - Album [FLAC]" }
        @?= 500 + 1000

  , testCase "ALAC in title adds 1000" $
      scoreRelease Nothing baseRelease { riTitle = "Artist - Album [ALAC]" }
        @?= 500 + 1000

  , testCase "320 in title adds 600 (and not lossless)" $
      scoreRelease Nothing baseRelease { riTitle = "Artist - Album [320]" }
        @?= 500 + 600

  , testCase "V0 with leading space adds 400" $
      scoreRelease Nothing baseRelease { riTitle = "Artist - Album V0" }
        @?= 500 + 400

  , testCase "V0 with leading dash adds 400" $
      scoreRelease Nothing baseRelease { riTitle = "Artist-Album-V0" }
        @?= 500 + 400

  , testCase "Lossless wins over 320 if both present" $
      scoreRelease Nothing baseRelease { riTitle = "Artist - Album [FLAC 320kbps source]" }
        @?= 500 + 1000

  , testCase "no quality tag = no bonus" $
      scoreRelease Nothing baseRelease { riTitle = "Artist - Album" } @?= 500
  ]

-- =============================================================================
-- Seed score
-- =============================================================================

seedScoreTests :: TestTree
seedScoreTests = testGroup "seed score"
  [ testCase "seeders capped at 500 (50 seeders → +500)" $
      -- NZB=500 + seeders min(500, 50*10)=500 = 1000
      scoreRelease Nothing baseRelease { riSeeders = Just 50 } @?= 500 + 500

  , testCase "seeders capped at 500 (100 seeders still +500)" $
      scoreRelease Nothing baseRelease { riSeeders = Just 100 } @?= 500 + 500

  , testCase "no seeder info = 0" $
      scoreRelease Nothing baseRelease @?= 500
  ]

-- =============================================================================
-- Size score
-- =============================================================================

sizeScoreTests :: TestTree
sizeScoreTests = testGroup "size score"
  [ testCase "50MB - 2GB range = +100" $
      -- 100MB
      scoreRelease Nothing baseRelease { riSize = Just (100 * 1024 * 1024) } @?= 500 + 100

  , testCase "2GB - 5GB range = +50" $
      -- 3GB
      scoreRelease Nothing baseRelease { riSize = Just (3 * 1024 * 1024 * 1024) } @?= 500 + 50

  , testCase "under 50MB = 0" $
      -- 10MB
      scoreRelease Nothing baseRelease { riSize = Just (10 * 1024 * 1024) } @?= 500

  , testCase "over 5GB = 0" $
      -- 6GB
      scoreRelease Nothing baseRelease { riSize = Just (6 * 1024 * 1024 * 1024) } @?= 500

  , testCase "no size info = 0" $
      scoreRelease Nothing baseRelease @?= 500
  ]

-- =============================================================================
-- Grab score
-- =============================================================================

grabScoreTests :: TestTree
grabScoreTests = testGroup "grab score"
  [ testCase "grabs capped at 200 (200 grabs → +200)" $
      scoreRelease Nothing baseRelease { riGrabs = Just 200 } @?= 500 + 200

  , testCase "small grabs scale 2x" $
      scoreRelease Nothing baseRelease { riGrabs = Just 10 } @?= 500 + 20

  , testCase "no grabs = 0" $
      scoreRelease Nothing baseRelease @?= 500
  ]

-- =============================================================================
-- Title match
-- =============================================================================

titleMatchTests :: TestTree
titleMatchTests = testGroup "title match score"
  [ testCase "exact album match = +3000" $
      -- 500 (NZB) + 3000 (exact match)
      scoreRelease (Just "Blueprint") baseRelease { riTitle = "Jay-Z - Blueprint [FLAC]" }
        @?= 500 + 1000 + 3000

  , testCase "sequel number penalty: searching 'Blueprint' finds 'Blueprint 3'" $
      -- 500 + 1000 (FLAC) + 3000 (exact match on "blueprint") + (-2000) sequel penalty
      scoreRelease (Just "Blueprint") baseRelease { riTitle = "Jay-Z - Blueprint 3 [FLAC]" }
        @?= 500 + 1000 + 3000 - 2000

  , testCase "searching 'Blueprint 3' finds 'Blueprint 3' = no penalty" $
      -- "3" is in search words so not treated as extra number
      scoreRelease (Just "Blueprint 3") baseRelease { riTitle = "Jay-Z - Blueprint 3 [FLAC]" }
        @?= 500 + 1000 + 3000

  , testCase "no match = 0 titleMatch" $
      scoreRelease (Just "Something Else") baseRelease { riTitle = "Jay-Z - Blueprint" }
        @?= 500

  , testCase "Nothing search title = 0 titleMatch" $
      scoreRelease Nothing baseRelease { riTitle = "Jay-Z - Blueprint" } @?= 500

  , testCase "dots and dashes normalize to spaces" $
      -- "blueprint" found in "jay.z.blueprint.flac"
      scoreRelease (Just "Blueprint") baseRelease { riTitle = "Jay.Z.Blueprint.FLAC" }
        @?= 500 + 1000 + 3000
  ]

-- =============================================================================
-- Ranking
-- =============================================================================

rankTests :: TestTree
rankTests = testGroup "rankReleases"
  [ testCase "ranks higher-scoring release first" $ do
      let worse = baseRelease { riTitle = "a", riDownloadType = Torrent }  -- 50
          better = baseRelease { riTitle = "b" }                           -- 500 (NZB)
          ranked = rankReleases Nothing [worse, better]
      map riTitle ranked @?= ["b", "a"]

  , testCase "exact title match outranks quality" $ do
      let noMatch = baseRelease { riTitle = "Other Album [FLAC]" }         -- 500 + 1000
          match   = baseRelease { riTitle = "Target Album" }               -- 500 + 3000
          ranked = rankReleases (Just "Target Album") [noMatch, match]
      map riTitle ranked @?= ["Target Album", "Other Album [FLAC]"]

  , testCase "empty input returns empty" $
      rankReleases Nothing [] @?= []
  ]
