{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the Indexer JSON parsing and URL encoding.
--
-- Tests:
-- - Parsing numeric size/length fields
-- - Parsing string size/length fields
-- - Handling missing size/length fields
-- - URL encoding query parameters
module Unit.IndexerSpec
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL

import Skema.Indexer.Client (NewznabResponse(..), NewznabChannel(..), NewznabItem(..), NewznabEnclosure(..), encodeQueryParam, normalizeQuery)

tests :: TestTree
tests = testGroup "Unit.Indexer"
  [ jsonParsingTests
  , urlEncodingTests
  , queryNormalizationTests
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

-- =============================================================================
-- URL Encoding Tests
-- =============================================================================

urlEncodingTests :: TestTree
urlEncodingTests = testGroup "URL Encoding"
  [ testCase "encodes spaces as %20" $ do
      let input = "The Beatles"
          expected = "The%20Beatles"
      assertEqual "Spaces should be encoded" expected (encodeQueryParam input)

  , testCase "encodes forward slash" $ do
      let input = "AC/DC"
          expected = "AC%2FDC"
      assertEqual "Forward slash should be encoded" expected (encodeQueryParam input)

  , testCase "encodes ampersand" $ do
      let input = "artist & album"
          expected = "artist%20%26%20album"
      assertEqual "Ampersand should be encoded" expected (encodeQueryParam input)

  , testCase "encodes question mark" $ do
      let input = "What?"
          expected = "What%3F"
      assertEqual "Question mark should be encoded" expected (encodeQueryParam input)

  , testCase "encodes equals sign" $ do
      let input = "a=b"
          expected = "a%3Db"
      assertEqual "Equals sign should be encoded" expected (encodeQueryParam input)

  , testCase "encodes hash/pound sign" $ do
      let input = "#hashtag"
          expected = "%23hashtag"
      assertEqual "Hash sign should be encoded" expected (encodeQueryParam input)

  , testCase "encodes non-ASCII characters (French)" $ do
      let input = "Françoise Hardy"
          -- ç is encoded as %C3%A7 in UTF-8
          expected = "Fran%C3%A7oise%20Hardy"
      assertEqual "French characters should be encoded" expected (encodeQueryParam input)

  , testCase "encodes non-ASCII characters (Icelandic)" $ do
      let input = "Sigur Rós"
          -- ó is encoded as %C3%B3 in UTF-8
          expected = "Sigur%20R%C3%B3s"
      assertEqual "Icelandic characters should be encoded" expected (encodeQueryParam input)

  , testCase "encodes non-ASCII characters (Japanese)" $ do
      let input = "中島美嘉"
          -- Each Japanese character is encoded as multiple bytes in UTF-8
          -- 中 = E4 B8 AD, 島 = E5 B3 B6, 美 = E7 BE 8E, 嘉 = E5 98 89
          expected = "%E4%B8%AD%E5%B3%B6%E7%BE%8E%E5%98%89"
      assertEqual "Japanese characters should be encoded" expected (encodeQueryParam input)

  , testCase "preserves unreserved characters" $ do
      let input = "ABCabc123-_.~"
          expected = "ABCabc123-_.~"
      assertEqual "Unreserved characters should not be encoded" expected (encodeQueryParam input)

  , testCase "encodes mixed ASCII and non-ASCII" $ do
      let input = "Ágætis byrjun"
          -- Á = C3 81, æ = C3 A6
          expected = "%C3%81g%C3%A6tis%20byrjun"
      assertEqual "Mixed characters should be properly encoded" expected (encodeQueryParam input)

  , testCase "handles empty string" $ do
      let input = ""
          expected = ""
      assertEqual "Empty string should remain empty" expected (encodeQueryParam input)

  , testCase "encodes parentheses" $ do
      let input = "Song (Live)"
          expected = "Song%20%28Live%29"
      assertEqual "Parentheses should be encoded" expected (encodeQueryParam input)

  , testCase "encodes square brackets" $ do
      let input = "[2024]"
          expected = "%5B2024%5D"
      assertEqual "Square brackets should be encoded" expected (encodeQueryParam input)
  ]

-- =============================================================================
-- Query Normalization Tests
-- =============================================================================

queryNormalizationTests :: TestTree
queryNormalizationTests = testGroup "Query Normalization"
  [ testCase "removes forward slash" $ do
      let input = "AC/DC"
          expected = "ACDC"
      assertEqual "Forward slash should be removed" expected (normalizeQuery input)

  , testCase "removes special characters" $ do
      let input = "Panic! at the Disco"
          expected = "Panic at the Disco"
      assertEqual "Exclamation mark should be removed" expected (normalizeQuery input)

  , testCase "removes parentheses" $ do
      let input = "Song (Live)"
          expected = "Song Live"
      assertEqual "Parentheses should be removed" expected (normalizeQuery input)

  , testCase "removes ampersand" $ do
      let input = "Artist & Band"
          expected = "Artist  Band"
      assertEqual "Ampersand should be removed" expected (normalizeQuery input)

  , testCase "normalizes accented characters (French)" $ do
      let input = "Françoise Hardy"
          expected = "Francoise Hardy"
      assertEqual "French accents should be normalized" expected (normalizeQuery input)

  , testCase "normalizes accented characters (Icelandic)" $ do
      let input = "Sigur Rós"
          expected = "Sigur Ros"
      assertEqual "Icelandic accents should be normalized" expected (normalizeQuery input)

  , testCase "normalizes accented characters (mixed case)" $ do
      let input = "Ágætis byrjun"
          expected = "Agaetis byrjun"
      assertEqual "Mixed case accents should be normalized" expected (normalizeQuery input)

  , testCase "preserves spaces" $ do
      let input = "The Beatles"
          expected = "The Beatles"
      assertEqual "Spaces should be preserved" expected (normalizeQuery input)

  , testCase "preserves alphanumeric characters" $ do
      let input = "Blink-182"
          expected = "Blink182"
      assertEqual "Numbers should be preserved, hyphens removed" expected (normalizeQuery input)

  , testCase "handles empty string" $ do
      let input = ""
          expected = ""
      assertEqual "Empty string should remain empty" expected (normalizeQuery input)

  , testCase "handles multiple special characters" $ do
      let input = "AC/DC - Back in Black (1980)"
          expected = "ACDC  Back in Black 1980"
      assertEqual "Multiple special characters should be removed" expected (normalizeQuery input)

  , testCase "removes all non-alphanumeric except spaces" $ do
      let input = "Artist's \"Best\" Album #1!"
          expected = "Artists Best Album 1"
      assertEqual "All special characters should be removed" expected (normalizeQuery input)

  , testCase "handles Japanese characters" $ do
      let input = "中島美嘉"
          -- Japanese characters are considered alphanumeric by Unicode, so they're preserved
          expected = "中島美嘉"
      assertEqual "Japanese characters are preserved as alphanumeric" expected (normalizeQuery input)
  ]
