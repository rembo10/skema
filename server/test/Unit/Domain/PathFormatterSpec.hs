{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Skema.Domain.PathFormatter pure template engine.
module Unit.Domain.PathFormatterSpec
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Skema.Domain.PathFormatter

tests :: TestTree
tests = testGroup "Unit.Domain.PathFormatter"
  [ parseTemplateTests
  , formatPathTests
  , functionTests
  , conditionalTests
  , sanitizationTests
  , paddingTests
  , truncateFileNameTests
  ]

-- =============================================================================
-- Builders
-- =============================================================================

baseCtx :: PathContext
baseCtx = PathContext
  { pcAlbumArtist = "Radiohead"
  , pcAlbum = "OK Computer"
  , pcYear = "1997"
  , pcTotalDiscs = 1
  , pcTotalTracks = 12
  , pcCountry = Nothing
  , pcLabel = Nothing
  , pcCatalogNumber = Nothing
  , pcArtist = Nothing
  , pcTitle = Just "Paranoid Android"
  , pcDisc = Just 1
  , pcTrack = Just 2
  , pcFormat = Just "FLAC"
  , pcBitrate = Just 1000
  , pcSampleRate = Just 44100
  , pcBitDepth = Just 16
  , pcExtension = Just ".flac"
  }

-- =============================================================================
-- parseTemplate
-- =============================================================================

parseTemplateTests :: TestTree
parseTemplateTests = testGroup "parseTemplate"
  [ testCase "literal only" $
      parseTemplate "Music/" @?= [Literal "Music/"]

  , testCase "single variable" $
      parseTemplate "{album}" @?= [Variable "album"]

  , testCase "variable surrounded by literals" $
      parseTemplate "a/{album}/b" @?= [Literal "a/", Variable "album", Literal "/b"]

  , testCase "function" $
      parseTemplate "{lower:artist}" @?= [Function "lower" "artist"]

  , testCase "conditional with then and else" $
      parseTemplate "{if:multidisc|MULTI|SINGLE}" @?=
        [Conditional "multidisc" "MULTI" (Just "SINGLE")]

  , testCase "conditional with only then" $
      parseTemplate "{if:multidisc|MULTI}" @?=
        [Conditional "multidisc" "MULTI" Nothing]

  , testCase "unclosed brace treated as literal" $
      parseTemplate "abc{unclosed" @?= [Literal "abc{unclosed"]

  , testCase "empty template -> empty list" $
      parseTemplate "" @?= []
  ]

-- =============================================================================
-- formatPath end-to-end
-- =============================================================================

formatPathTests :: TestTree
formatPathTests = testGroup "formatPath"
  [ testCase "album directory template" $
      formatPath "{album_artist}/{year} - {album}" baseCtx @?=
        "Radiohead/1997 - OK Computer"

  , testCase "artist falls back to album_artist when unset" $
      formatPath "{artist}" baseCtx @?= "Radiohead"

  , testCase "artist uses track artist when set" $
      formatPath "{artist}" baseCtx { pcArtist = Just "Thom Yorke" } @?=
        "Thom Yorke"

  , testCase "unknown variable expands to empty" $
      formatPath "a/{bogus}/b" baseCtx @?= "a//b"

  , testCase "missing numeric field -> empty" $
      formatPath "{disc}" baseCtx { pcDisc = Nothing } @?= ""

  , testCase "missing title falls back to \"Unknown\"" $
      formatPath "{title}" baseCtx { pcTitle = Nothing } @?= "Unknown"
  ]

-- =============================================================================
-- Functions
-- =============================================================================

functionTests :: TestTree
functionTests = testGroup "functions"
  [ testCase "lower" $
      formatPath "{lower:album}" baseCtx @?= "ok computer"

  , testCase "upper" $
      formatPath "{upper:album_artist}" baseCtx @?= "RADIOHEAD"

  , testCase "sanitize removes illegal chars" $
      formatPath "{sanitize:album}" baseCtx { pcAlbum = "a/b:c" } @?= "a_b_c"

  , testCase "capitalize" $
      formatPath "{capitalize:album}" baseCtx { pcAlbum = "hello world" } @?=
        "Hello World"

  , testCase "trim strips whitespace" $
      formatPath "{trim:album}" baseCtx { pcAlbum = "  padded  " } @?= "padded"

  , testCase "unknown function returns value unchanged" $
      formatPath "{bogus:album}" baseCtx @?= "OK Computer"
  ]

-- =============================================================================
-- Conditionals
-- =============================================================================

conditionalTests :: TestTree
conditionalTests = testGroup "conditionals"
  [ testCase "multidisc false (1 disc) -> else branch" $
      formatPath "{if:multidisc|MULTI|SINGLE}" baseCtx @?= "SINGLE"

  , testCase "multidisc true (>1 disc) -> then branch" $
      formatPath "{if:multidisc|MULTI|SINGLE}" baseCtx { pcTotalDiscs = 2 } @?=
        "MULTI"

  , testCase "lossless true for FLAC" $
      formatPath "{if:lossless|LL/|Lossy/}" baseCtx @?= "LL/"

  , testCase "lossless true for ALAC (case-insensitive)" $
      formatPath "{if:lossless|LL/|Lossy/}" baseCtx { pcFormat = Just "alac" } @?=
        "LL/"

  , testCase "lossless false for MP3" $
      formatPath "{if:lossless|LL/|Lossy/}" baseCtx { pcFormat = Just "MP3" } @?=
        "Lossy/"

  , testCase "lossless false when format missing" $
      formatPath "{if:lossless|LL/|Lossy/}" baseCtx { pcFormat = Nothing } @?=
        "Lossy/"

  , testCase "unknown condition is always false" $
      formatPath "{if:bogus|T|F}" baseCtx @?= "F"
  ]

-- =============================================================================
-- Path component sanitization
-- =============================================================================

sanitizationTests :: TestTree
sanitizationTests = testGroup "path sanitization"
  [ testCase "slashes in album title are replaced" $
      formatPath "{album}" baseCtx { pcAlbum = "AC/DC" } @?= "AC_DC"

  , testCase "all illegal chars replaced with underscore" $
      formatPath "{album}" baseCtx { pcAlbum = "a:b*c?d\"e<f>g|h\\i" } @?=
        "a_b_c_d_e_f_g_h_i"

  , testCase "leading/trailing dots and spaces stripped" $
      formatPath "{album}" baseCtx { pcAlbum = "  .title.  " } @?= "title"

  , testCase "empty sanitized value becomes \"Unknown\"" $
      formatPath "{album}" baseCtx { pcAlbum = "   " } @?= "Unknown"

  , testCase "collapses multiple internal spaces" $
      formatPath "{album}" baseCtx { pcAlbum = "a    b" } @?= "a b"
  ]

-- =============================================================================
-- Padded numbers
-- =============================================================================

paddingTests :: TestTree
paddingTests = testGroup "padded track/disc numbers"
  [ testCase "track:02 pads single digit" $
      formatPath "{track:02}" baseCtx { pcTrack = Just 5 } @?= "05"

  , testCase "track:02 leaves two-digit number unchanged" $
      formatPath "{track:02}" baseCtx { pcTrack = Just 12 } @?= "12"

  , testCase "track:03 pads to three digits" $
      formatPath "{track:03}" baseCtx { pcTrack = Just 7 } @?= "007"

  , testCase "disc:02 pads disc number" $
      formatPath "{disc:02}" baseCtx { pcDisc = Just 3 } @?= "03"

  , testCase "missing number -> empty" $
      formatPath "{track:02}" baseCtx { pcTrack = Nothing } @?= ""

  , testCase "invalid padding spec returns raw number" $
      formatPath "{track:abc}" baseCtx { pcTrack = Just 5 } @?= "5"
  ]

-- =============================================================================
-- truncateFileName
-- =============================================================================

truncateFileNameTests :: TestTree
truncateFileNameTests = testGroup "truncateFileName"
  [ testCase "short filename is unchanged" $ do
      let name = "01 Artist - Album 2005 - Title.flac"
      truncateFileName name @?= name

  , testCase "exactly 255 bytes is unchanged" $ do
      let base = T.replicate 250 "a"
          name = base <> ".flac"
      BS.length (encodeUtf8 name) @?= 255
      truncateFileName name @?= name

  , testCase "256 bytes gets truncated" $ do
      let base = T.replicate 251 "a"
          name = base <> ".flac"
      BS.length (encodeUtf8 name) @?= 256
      let result = truncateFileName name
      BS.length (encodeUtf8 result) @?= 255
      assertBool "extension preserved" (".flac" `T.isSuffixOf` result)

  , testCase "preserves extension on very long filename" $ do
      let name = T.replicate 300 "x" <> ".mp3"
          result = truncateFileName name
      BS.length (encodeUtf8 result) @<= 255
      assertBool "extension preserved" (".mp3" `T.isSuffixOf` result)

  , testCase "handles multi-byte UTF-8 characters without breaking" $ do
      let base = T.replicate 85 "あ"
          name = base <> ".flac"
      assertBool "input exceeds 255 bytes" (BS.length (encodeUtf8 name) > 255)
      let result = truncateFileName name
      BS.length (encodeUtf8 result) @<= 255
      assertBool "extension preserved" (".flac" `T.isSuffixOf` result)
      assertBool "valid text" (T.length result > 0)

  , testCase "filename without extension gets truncated" $ do
      let name = T.replicate 300 "a"
          result = truncateFileName name
      BS.length (encodeUtf8 result) @<= 255

  , testCase "empty filename is unchanged" $
      truncateFileName "" @?= ""
  ]

-- | Assert that actual is less than or equal to expected.
(@<=) :: (Ord a, Show a) => a -> a -> Assertion
actual @<= limit =
  assertBool (show actual <> " is not <= " <> show limit) (actual <= limit)
infix 1 @<=
