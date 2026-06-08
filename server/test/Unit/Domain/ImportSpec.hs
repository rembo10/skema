{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Skema.Domain.Import pure import logic.
--
-- Tests:
-- - extractYear: year extraction from date strings
module Unit.Domain.ImportSpec
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Skema.Domain.Import
  ( extractYear
  , validateDownloadPath
  , MatchDecision(..)
  , confidenceDecision
  , noMatchMessage
  )

-- =============================================================================
-- Tests
-- =============================================================================

tests :: TestTree
tests = testGroup "Unit.Domain.Import"
  [ extractYearTests
  , validateDownloadPathTests
  , confidenceDecisionTests
  , noMatchMessageTests
  ]

-- =============================================================================
-- extractYear Tests
-- =============================================================================

extractYearTests :: TestTree
extractYearTests = testGroup "extractYear"
  [ testCase "Nothing -> Unknown" $
      extractYear Nothing @?= "Unknown"

  , testCase "Just full date -> year portion" $
      extractYear (Just "2024-01-15") @?= "2024"

  , testCase "Just year only -> year" $
      extractYear (Just "2024") @?= "2024"

  , testCase "Just non-date text -> first 4 chars" $
      extractYear (Just "abcd") @?= "abcd"

  , testCase "Just empty string -> Unknown" $
      extractYear (Just "") @?= "Unknown"
  ]

-- =============================================================================
-- validateDownloadPath Tests
-- =============================================================================

validateDownloadPathTests :: TestTree
validateDownloadPathTests = testGroup "validateDownloadPath"
  [ testCase "no path -> failure" $
      validateDownloadPath Nothing @?= Left "No download path"

  , testCase "empty path -> failure" $
      validateDownloadPath (Just "") @?= Left "Download path is empty"

  , testCase "whitespace-only path -> failure" $
      validateDownloadPath (Just "   ") @?= Left "Download path is empty"

  , testCase "real path -> success" $
      validateDownloadPath (Just "/downloads/album") @?= Right "/downloads/album"

  , testCase "path is returned unstripped" $
      validateDownloadPath (Just " /downloads/album ") @?= Right " /downloads/album "
  ]

-- =============================================================================
-- confidenceDecision Tests
-- =============================================================================

confidenceDecisionTests :: TestTree
confidenceDecisionTests = testGroup "confidenceDecision"
  [ testCase "confidence above threshold is accepted" $
      confidenceDecision 0.9 0.8 @?= AcceptMatch

  , testCase "confidence equal to threshold is accepted" $
      confidenceDecision 0.8 0.8 @?= AcceptMatch

  , testCase "confidence below threshold is rejected with a formatted message" $
      confidenceDecision 0.7 0.8
        @?= RejectLowConfidence "Best match confidence (70%) is below threshold (80%)."
  ]

-- =============================================================================
-- noMatchMessage Tests
-- =============================================================================

noMatchMessageTests :: TestTree
noMatchMessageTests = testGroup "noMatchMessage"
  [ testCase "includes the candidate count" $
      noMatchMessage 5
        @?= "No MusicBrainz match found (5 candidates searched). \
            \The release may not be in MusicBrainz database, or no candidate details could be fetched."
  ]
