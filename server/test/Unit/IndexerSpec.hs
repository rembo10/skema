{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the Indexer JSON parsing.
--
-- Tests:
-- - Parsing numeric size/length fields
-- - Parsing string size/length fields
-- - Handling missing size/length fields
module Unit.IndexerSpec
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL

import Skema.Indexer.Client (NewznabResponse(..), NewznabChannel(..), NewznabItem(..), NewznabEnclosure(..))

tests :: TestTree
tests = testGroup "Unit.Indexer"
  [ jsonParsingTests
  ]

-- =============================================================================
-- JSON Parsing Tests
-- =============================================================================

jsonParsingTests :: TestTree
jsonParsingTests = testGroup "JSON Parsing"
  [ testCase "parses numeric size and length fields" $ do
      let json = BSL.unlines
            [ "{"
            , "  \"channel\": {"
            , "    \"item\": {"
            , "      \"title\": \"Test Release\","
            , "      \"link\": \"http://example.com/download\","
            , "      \"size\": 1234567890,"
            , "      \"enclosure\": {"
            , "        \"@attributes\": {"
            , "          \"url\": \"http://example.com/file.nzb\","
            , "          \"length\": 9876543210,"
            , "          \"type\": \"application/x-nzb\""
            , "        }"
            , "      }"
            , "    }"
            , "  }"
            , "}"
            ]

      case eitherDecode json of
        Left err -> assertFailure $ "Failed to parse numeric JSON: " <> err
        Right (NewznabResponse channel) -> do
          let items = ncItem channel
          assertEqual "Should have 1 item" 1 (length items)
          case viaNonEmpty Prelude.head items of
            Nothing -> assertFailure "Should have at least one item"
            Just item -> do
              assertEqual "Size should be parsed" (Just 1234567890) (niSize item)
              case niEnclosure item of
                Nothing -> assertFailure "Enclosure should be present"
                Just enc -> assertEqual "Length should be parsed" (Just 9876543210) (neLength enc)

  , testCase "parses string size and length fields" $ do
      let json = BSL.unlines
            [ "{"
            , "  \"channel\": {"
            , "    \"item\": {"
            , "      \"title\": \"Test Release\","
            , "      \"link\": \"http://example.com/download\","
            , "      \"size\": \"1234567890\","
            , "      \"enclosure\": {"
            , "        \"@attributes\": {"
            , "          \"url\": \"http://example.com/file.nzb\","
            , "          \"length\": \"9876543210\","
            , "          \"type\": \"application/x-nzb\""
            , "        }"
            , "      }"
            , "    }"
            , "  }"
            , "}"
            ]

      case eitherDecode json of
        Left err -> assertFailure $ "Failed to parse string JSON: " <> err
        Right (NewznabResponse channel) -> do
          let items = ncItem channel
          assertEqual "Should have 1 item" 1 (length items)
          case viaNonEmpty Prelude.head items of
            Nothing -> assertFailure "Should have at least one item"
            Just item -> do
              assertEqual "Size should be parsed from string" (Just 1234567890) (niSize item)
              case niEnclosure item of
                Nothing -> assertFailure "Enclosure should be present"
                Just enc -> assertEqual "Length should be parsed from string" (Just 9876543210) (neLength enc)

  , testCase "handles missing size and length fields" $ do
      let json = BSL.unlines
            [ "{"
            , "  \"channel\": {"
            , "    \"item\": {"
            , "      \"title\": \"Test Release\","
            , "      \"link\": \"http://example.com/download\""
            , "    }"
            , "  }"
            , "}"
            ]

      case eitherDecode json of
        Left err -> assertFailure $ "Failed to parse minimal JSON: " <> err
        Right (NewznabResponse channel) -> do
          let items = ncItem channel
          assertEqual "Should have 1 item" 1 (length items)
          case viaNonEmpty Prelude.head items of
            Nothing -> assertFailure "Should have at least one item"
            Just item -> do
              assertEqual "Size should be Nothing when missing" Nothing (niSize item)
              assertBool "Enclosure should be Nothing when missing" (isNothing $ niEnclosure item)

  , testCase "handles invalid string values gracefully" $ do
      let json = BSL.unlines
            [ "{"
            , "  \"channel\": {"
            , "    \"item\": {"
            , "      \"title\": \"Test Release\","
            , "      \"link\": \"http://example.com/download\","
            , "      \"size\": \"not-a-number\""
            , "    }"
            , "  }"
            , "}"
            ]

      case eitherDecode json of
        Left err -> assertFailure $ "Failed to parse JSON with invalid size: " <> err
        Right (NewznabResponse channel) -> do
          let items = ncItem channel
          assertEqual "Should have 1 item" 1 (length items)
          case viaNonEmpty Prelude.head items of
            Nothing -> assertFailure "Should have at least one item"
            Just item ->
              -- Invalid string should result in Nothing
              assertEqual "Invalid string size should be Nothing" Nothing (niSize item)
  ]
