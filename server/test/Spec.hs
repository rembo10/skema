{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty
import qualified Integration.GrouperSpec
import qualified Integration.ScanGroupIdentifySpec
import qualified Integration.EndToEndSpec
import qualified Unit.ConfigSpec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Skema Tests"
  [ Unit.ConfigSpec.tests
  , Integration.GrouperSpec.tests
  , Integration.ScanGroupIdentifySpec.tests
  , Integration.EndToEndSpec.tests
  ]
