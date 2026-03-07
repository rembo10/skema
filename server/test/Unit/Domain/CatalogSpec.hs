{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Skema.Domain.Catalog pure business logic.
--
-- Tests:
-- - isAlbumWanted: profile/library/cutoff combinations
-- - computeAlbumState: all derived album states
-- - makeDownloadDecision: initiate, allow, cancel decisions
-- - artistQualityProfileChanged: artist profile inheritance propagation
module Unit.Domain.CatalogSpec
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Skema.Domain.Catalog
import Skema.Domain.Quality (Quality(..), QualityProfile(..), QualityPreference(..))

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

-- =============================================================================
-- Tests
-- =============================================================================

tests :: TestTree
tests = testGroup "Unit.Domain.Catalog"
  [ isAlbumWantedTests
  , computeAlbumStateTests
  , makeDownloadDecisionTests
  , artistQualityProfileChangedTests
  ]

-- =============================================================================
-- isAlbumWanted Tests
-- =============================================================================

isAlbumWantedTests :: TestTree
isAlbumWantedTests = testGroup "isAlbumWanted"
  [ testCase "no profile -> False" $ do
      let ctx = AlbumContext
            { acQualityProfile = Nothing
            , acCurrentQuality = Nothing
            , acInLibrary = False
            , acActiveDownloadStatus = Nothing
            }
      isAlbumWanted ctx @?= False

  , testCase "has profile + not in library -> True" $ do
      let ctx = AlbumContext
            { acQualityProfile = Just testProfile
            , acCurrentQuality = Nothing
            , acInLibrary = False
            , acActiveDownloadStatus = Nothing
            }
      isAlbumWanted ctx @?= True

  , testCase "in library + meets cutoff -> False" $ do
      let ctx = AlbumContext
            { acQualityProfile = Just testProfile
            , acCurrentQuality = Just Lossless  -- Meets cutoff
            , acInLibrary = True
            , acActiveDownloadStatus = Nothing
            }
      isAlbumWanted ctx @?= False

  , testCase "in library + below cutoff -> True" $ do
      let ctx = AlbumContext
            { acQualityProfile = Just testProfile
            , acCurrentQuality = Just MP3_320  -- Below Lossless cutoff
            , acInLibrary = True
            , acActiveDownloadStatus = Nothing
            }
      isAlbumWanted ctx @?= True
  ]

-- =============================================================================
-- computeAlbumState Tests
-- =============================================================================

computeAlbumStateTests :: TestTree
computeAlbumStateTests = testGroup "computeAlbumState"
  [ testCase "NotWanted: no profile, not in library" $ do
      let ctx = AlbumContext
            { acQualityProfile = Nothing
            , acCurrentQuality = Nothing
            , acInLibrary = False
            , acActiveDownloadStatus = Nothing
            }
      computeAlbumState ctx @?= NotWanted

  , testCase "InLibrary: no profile, in library" $ do
      let ctx = AlbumContext
            { acQualityProfile = Nothing
            , acCurrentQuality = Just Lossless
            , acInLibrary = True
            , acActiveDownloadStatus = Nothing
            }
      computeAlbumState ctx @?= InLibrary

  , testCase "Wanted: has profile, not in library, no download" $ do
      let ctx = AlbumContext
            { acQualityProfile = Just testProfile
            , acCurrentQuality = Nothing
            , acInLibrary = False
            , acActiveDownloadStatus = Nothing
            }
      computeAlbumState ctx @?= Wanted

  , testCase "Searching: download status is queued" $ do
      let ctx = AlbumContext
            { acQualityProfile = Just testProfile
            , acCurrentQuality = Nothing
            , acInLibrary = False
            , acActiveDownloadStatus = Just "queued"
            }
      computeAlbumState ctx @?= Searching

  , testCase "Downloading: download status is downloading" $ do
      let ctx = AlbumContext
            { acQualityProfile = Just testProfile
            , acCurrentQuality = Nothing
            , acInLibrary = False
            , acActiveDownloadStatus = Just "downloading"
            }
      computeAlbumState ctx @?= Downloading

  , testCase "Failed: download status is failed" $ do
      let ctx = AlbumContext
            { acQualityProfile = Just testProfile
            , acCurrentQuality = Nothing
            , acInLibrary = False
            , acActiveDownloadStatus = Just "failed"
            }
      computeAlbumState ctx @?= Failed

  , testCase "Monitored: in library, wanted, no download" $ do
      let ctx = AlbumContext
            { acQualityProfile = Just testProfile
            , acCurrentQuality = Just MP3_320  -- Below cutoff, so wanted
            , acInLibrary = True
            , acActiveDownloadStatus = Nothing
            }
      computeAlbumState ctx @?= Monitored

  , testCase "Upgrading: in library, wanted, has active download" $ do
      let ctx = AlbumContext
            { acQualityProfile = Just testProfile
            , acCurrentQuality = Just MP3_320  -- Below cutoff, so wanted
            , acInLibrary = True
            , acActiveDownloadStatus = Just "downloading"
            }
      computeAlbumState ctx @?= Upgrading
  ]

-- =============================================================================
-- makeDownloadDecision Tests
-- =============================================================================

makeDownloadDecisionTests :: TestTree
makeDownloadDecisionTests = testGroup "makeDownloadDecision"
  [ testCase "InitiateDownload: wanted + no download + quality meets profile" $ do
      let albumCtx = AlbumContext
            { acQualityProfile = Just testProfile
            , acCurrentQuality = Nothing
            , acInLibrary = False
            , acActiveDownloadStatus = Nothing
            }
          ctx = DownloadContext
            { dcAlbumContext = albumCtx
            , dcDownloadQuality = Lossless
            , dcNewProfile = Nothing
            }
      makeDownloadDecision ctx @?= InitiateDownload

  , testCase "AllowContinue: already downloading, quality does not improve" $ do
      let albumCtx = AlbumContext
            { acQualityProfile = Just testProfile
            , acCurrentQuality = Nothing
            , acInLibrary = False
            , acActiveDownloadStatus = Just "downloading"  -- Already has active download
            }
          ctx = DownloadContext
            { dcAlbumContext = albumCtx
            , dcDownloadQuality = Lossless
            , dcNewProfile = Nothing
            }
      makeDownloadDecision ctx @?= AllowContinue

  , testCase "CancelDownload: profile removed" $ do
      let albumCtx = AlbumContext
            { acQualityProfile = Nothing
            , acCurrentQuality = Nothing
            , acInLibrary = False
            , acActiveDownloadStatus = Just "downloading"
            }
          ctx = DownloadContext
            { dcAlbumContext = albumCtx
            , dcDownloadQuality = Lossless
            , dcNewProfile = Nothing
            }
      case makeDownloadDecision ctx of
        CancelDownload _ -> pure ()
        other -> assertFailure $ "Expected CancelDownload, got: " <> show other
  ]

-- =============================================================================
-- artistQualityProfileChanged Tests
-- =============================================================================

artistQualityProfileChangedTests :: TestTree
artistQualityProfileChangedTests = testGroup "artistQualityProfileChanged"
  [ testCase "album added from search: no own profile + artist has profile -> wanted" $ do
      -- Bug report: albums added through universal search don't get marked as wanted.
      -- When an album is created from search, it has no quality profile of its own.
      -- The artist's quality profile should be inherited, making the album wanted.
      let (wanted, _shouldCancel, shouldSearch) =
            artistQualityProfileChanged
              Nothing           -- album has no own quality profile
              Nothing           -- not in library (just added from search)
              Nothing           -- no current quality
              Nothing           -- no active download
              Nothing           -- no active download quality
              (Just testProfile) -- artist has a quality profile
      wanted @?= True
      shouldSearch @?= True

  , testCase "album has own profile -> artist profile ignored" $ do
      let (wanted, _, _) =
            artistQualityProfileChanged
              (Just 99)          -- album has its own quality profile
              Nothing            -- not in library
              Nothing            -- no current quality
              Nothing            -- no active download
              Nothing            -- no active download quality
              (Just testProfile) -- artist also has a profile
      -- Album's own profile takes precedence; artist change doesn't affect it
      wanted @?= False

  , testCase "artist profile removed -> album no longer wanted" $ do
      let (wanted, _, shouldSearch) =
            artistQualityProfileChanged
              Nothing  -- album has no own profile
              Nothing  -- not in library
              Nothing  -- no current quality
              Nothing  -- no active download
              Nothing  -- no active download quality
              Nothing  -- artist profile removed
      wanted @?= False
      shouldSearch @?= False

  , testCase "album in library below cutoff + artist has profile -> wanted" $ do
      let (wanted, _, shouldSearch) =
            artistQualityProfileChanged
              Nothing            -- album has no own profile
              (Just 1)           -- in library (cluster ID)
              (Just MP3_320)     -- current quality below Lossless cutoff
              Nothing            -- no active download
              Nothing            -- no active download quality
              (Just testProfile) -- artist has a quality profile
      wanted @?= True
      shouldSearch @?= True
  ]
