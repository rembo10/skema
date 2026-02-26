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
import Skema.Domain.Import (extractYear)

-- =============================================================================
-- Tests
-- =============================================================================

tests :: TestTree
tests = testGroup "Unit.Domain.Import"
  [ extractYearTests
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
