{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Skema.Domain.Acquisition pure filter evaluation.
module Unit.Domain.AcquisitionSpec
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)

import Skema.Domain.Acquisition
import Skema.Database.Types (CatalogAlbumRecord(..))

tests :: TestTree
tests = testGroup "Unit.Domain.Acquisition"
  [ metacriticFiltersTests
  , pitchforkFiltersTests
  , libraryArtistsFiltersTests
  ]

-- =============================================================================
-- matchesMetacriticFilters
-- =============================================================================

noMCFilters :: MetacriticFilters
noMCFilters = MetacriticFilters
  { mcGenres = Nothing
  , mcMinCriticScore = Nothing
  , mcMinUserScore = Nothing
  }

metacriticFiltersTests :: TestTree
metacriticFiltersTests = testGroup "matchesMetacriticFilters"
  [ testCase "no thresholds: passes regardless of scores" $ do
      matchesMetacriticFilters noMCFilters Nothing Nothing @?= True
      matchesMetacriticFilters noMCFilters (Just 10) (Just 1.0) @?= True

  , testCase "critic threshold: score >= threshold passes" $ do
      let f = noMCFilters { mcMinCriticScore = Just 80 }
      matchesMetacriticFilters f (Just 80) Nothing @?= True
      matchesMetacriticFilters f (Just 90) Nothing @?= True

  , testCase "critic threshold: score < threshold fails" $ do
      let f = noMCFilters { mcMinCriticScore = Just 80 }
      matchesMetacriticFilters f (Just 79) Nothing @?= False

  , testCase "critic threshold with missing critic score fails" $ do
      let f = noMCFilters { mcMinCriticScore = Just 80 }
      matchesMetacriticFilters f Nothing Nothing @?= False

  , testCase "user threshold: score >= threshold passes" $ do
      let f = noMCFilters { mcMinUserScore = Just 7.5 }
      matchesMetacriticFilters f Nothing (Just 7.5) @?= True
      matchesMetacriticFilters f Nothing (Just 8.0) @?= True

  , testCase "user threshold with missing user score fails" $ do
      let f = noMCFilters { mcMinUserScore = Just 7.5 }
      matchesMetacriticFilters f Nothing Nothing @?= False

  , testCase "both thresholds use AND semantics" $ do
      let f = noMCFilters { mcMinCriticScore = Just 80, mcMinUserScore = Just 7.5 }
      matchesMetacriticFilters f (Just 85) (Just 8.0) @?= True
      matchesMetacriticFilters f (Just 85) (Just 7.0) @?= False
      matchesMetacriticFilters f (Just 75) (Just 8.0) @?= False
      matchesMetacriticFilters f (Just 85) Nothing @?= False
  ]

-- =============================================================================
-- matchesPitchforkFilters
-- =============================================================================

noPFFilters :: PitchforkFilters
noPFFilters = PitchforkFilters
  { pfGenres = Nothing
  , pfMinScore = Nothing
  }

pitchforkFiltersTests :: TestTree
pitchforkFiltersTests = testGroup "matchesPitchforkFilters"
  [ testCase "no threshold: passes regardless" $ do
      matchesPitchforkFilters noPFFilters Nothing @?= True
      matchesPitchforkFilters noPFFilters (Just 5.0) @?= True

  , testCase "threshold: score >= threshold passes" $ do
      let f = noPFFilters { pfMinScore = Just 7.5 }
      matchesPitchforkFilters f (Just 7.5) @?= True
      matchesPitchforkFilters f (Just 9.0) @?= True

  , testCase "threshold: score < threshold fails" $ do
      let f = noPFFilters { pfMinScore = Just 7.5 }
      matchesPitchforkFilters f (Just 7.4) @?= False

  , testCase "threshold with missing score fails" $ do
      let f = noPFFilters { pfMinScore = Just 7.5 }
      matchesPitchforkFilters f Nothing @?= False
  ]

-- =============================================================================
-- evaluateLibraryArtistsAlbumFilters
-- =============================================================================

-- | "Now" used for all release-status comparisons below.
referenceNow :: UTCTime
referenceNow = UTCTime (fromGregorian 2025 6 1) (secondsToDiffTime 0)

mkFilters :: Maybe [Text] -> Maybe ReleaseStatusFilter -> LibraryArtistsFilters
mkFilters albumTypes releaseStatus = LibraryArtistsFilters
  { lafIncludeArtistIds = Nothing
  , lafExcludeArtistIds = Nothing
  , lafAlbumTypes = albumTypes
  , lafReleaseStatus = releaseStatus
  }

mkAlbum :: Maybe Text -> Maybe Text -> CatalogAlbumRecord
mkAlbum albumType firstReleaseDate = CatalogAlbumRecord
  { catalogAlbumId = Just 1
  , catalogAlbumReleaseGroupMBID = "rg"
  , catalogAlbumTitle = "Album"
  , catalogAlbumArtistId = Just 1
  , catalogAlbumArtistMBID = "artist"
  , catalogAlbumArtistName = "Artist"
  , catalogAlbumType = albumType
  , catalogAlbumFirstReleaseDate = firstReleaseDate
  , catalogAlbumCoverUrl = Nothing
  , catalogAlbumCoverThumbnailUrl = Nothing
  , catalogAlbumQualityProfileId = Nothing
  , catalogAlbumCurrentQuality = Nothing
  , catalogAlbumCreatedAt = Nothing
  , catalogAlbumUpdatedAt = Nothing
  }

libraryArtistsFiltersTests :: TestTree
libraryArtistsFiltersTests = testGroup "evaluateLibraryArtistsAlbumFilters"
  [ testCase "default status (no filter) wants only upcoming albums" $ do
      eval (mkFilters Nothing Nothing) (album future) @?= True
      eval (mkFilters Nothing Nothing) (album past) @?= False
      eval (mkFilters Nothing Nothing) (album noDate) @?= False

  , testCase "UpcomingOnly mirrors the default behaviour" $ do
      eval (mkFilters Nothing (Just UpcomingOnly)) (album future) @?= True
      eval (mkFilters Nothing (Just UpcomingOnly)) (album past) @?= False

  , testCase "ReleasedOnly wants only already-released albums" $ do
      eval (mkFilters Nothing (Just ReleasedOnly)) (album past) @?= True
      eval (mkFilters Nothing (Just ReleasedOnly)) (album future) @?= False

  , testCase "UpcomingAndReleased accepts any release status" $ do
      eval (mkFilters Nothing (Just UpcomingAndReleased)) (album future) @?= True
      eval (mkFilters Nothing (Just UpcomingAndReleased)) (album past) @?= True
      eval (mkFilters Nothing (Just UpcomingAndReleased)) (album noDate) @?= True

  , testCase "album type filter excludes non-matching types" $ do
      eval (mkFilters (Just ["Album"]) (Just UpcomingAndReleased)) (typed "Album" past) @?= True
      eval (mkFilters (Just ["Album"]) (Just UpcomingAndReleased)) (typed "EP" past) @?= False

  , testCase "empty album type list includes all types" $
      eval (mkFilters (Just []) (Just UpcomingAndReleased)) (typed "EP" past) @?= True

  , testCase "album with no type is included by default under a type filter" $
      eval (mkFilters (Just ["Album"]) (Just UpcomingAndReleased)) (album past) @?= True
  ]
  where
    eval f a = evaluateLibraryArtistsAlbumFilters f referenceNow a
    future = Just "2025-12-01"
    past = Just "2025-01-01"
    noDate = Nothing
    album date = mkAlbum Nothing date
    typed t date = mkAlbum (Just t) date
