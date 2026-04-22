{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Skema.Domain.Acquisition pure filter evaluation.
module Unit.Domain.AcquisitionSpec
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Skema.Domain.Acquisition

tests :: TestTree
tests = testGroup "Unit.Domain.Acquisition"
  [ metacriticFiltersTests
  , pitchforkFiltersTests
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
