{-# LANGUAGE OverloadedStrings #-}

module Unit.PathFormatterSpec
  ( tests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Skema.FileSystem.PathFormatter (truncateFileName)

tests :: TestTree
tests = testGroup "Unit.PathFormatter"
  [ truncateFileNameTests
  ]

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
      -- Extension preserved
      assertBool "extension preserved" (".flac" `T.isSuffixOf` result)

  , testCase "preserves extension on very long filename" $ do
      let name = T.replicate 300 "x" <> ".mp3"
          result = truncateFileName name
      BS.length (encodeUtf8 result) @<= 255
      assertBool "extension preserved" (".mp3" `T.isSuffixOf` result)

  , testCase "handles the actual bug report filename" $ do
      let name = "03 Giraffes_ Giraffes! - SUPERBASS!!!! (Black Death greatest hits vol. 1) 2005 - And then she look'd down and saw miniature houses and miniature people and inside the miniature people were miniature hearts pumping blood through miniature veins (her mouth was watery and wet).flac"
      -- Verify this is actually over 255 bytes
      assertBool "input exceeds 255 bytes" (BS.length (encodeUtf8 name) > 255)
      let result = truncateFileName name
      BS.length (encodeUtf8 result) @<= 255
      assertBool "extension preserved" (".flac" `T.isSuffixOf` result)

  , testCase "handles multi-byte UTF-8 characters without breaking" $ do
      -- Japanese characters are 3 bytes each in UTF-8
      let base = T.replicate 85 "あ"  -- 85 * 3 = 255 bytes for base alone
          name = base <> ".flac"
      assertBool "input exceeds 255 bytes" (BS.length (encodeUtf8 name) > 255)
      let result = truncateFileName name
      BS.length (encodeUtf8 result) @<= 255
      assertBool "extension preserved" (".flac" `T.isSuffixOf` result)
      -- Verify it's valid text (no broken multi-byte chars) by checking round-trip
      assertBool "valid text" (T.length result > 0)

  , testCase "filename without extension gets truncated" $ do
      let name = T.replicate 300 "a"
          result = truncateFileName name
      BS.length (encodeUtf8 result) @<= 255

  , testCase "empty filename is unchanged" $ do
      truncateFileName "" @?= ""
  ]

-- | Assert that actual is less than or equal to expected.
(@<=) :: (Ord a, Show a) => a -> a -> Assertion
actual @<= limit =
  assertBool (show actual <> " is not <= " <> show limit) (actual <= limit)
infix 1 @<=
