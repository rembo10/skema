{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty
import qualified Integration.GrouperSpec
import qualified Integration.ScanGroupIdentifySpec
import qualified Integration.EndToEndSpec
import qualified Unit.ConfigSpec
import qualified Unit.IndexerSpec
import qualified Unit.MatchingSpec
import qualified Unit.PathFormatterSpec
import qualified Unit.Domain.CatalogSpec
import qualified Unit.Domain.QualitySpec
import qualified Unit.Domain.MetadataSpec
import qualified Unit.Domain.ImportSpec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Skema Tests"
  [ Unit.ConfigSpec.tests
  , Unit.IndexerSpec.tests
  , Unit.MatchingSpec.tests
  , Unit.PathFormatterSpec.tests
  , Unit.Domain.CatalogSpec.tests
  , Unit.Domain.QualitySpec.tests
  , Unit.Domain.MetadataSpec.tests
  , Unit.Domain.ImportSpec.tests
  , Integration.GrouperSpec.tests
  , Integration.ScanGroupIdentifySpec.tests
  , Integration.EndToEndSpec.tests
  ]
