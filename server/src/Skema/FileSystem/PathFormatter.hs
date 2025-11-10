{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Path formatter - formats library paths using templates.
--
-- Supports:
-- - Variables: {artist}, {album_artist}, {album}, {year}, {format}, etc.
-- - Conditionals: {if:multidisc|Disc {disc}/}, {if:lossless|FLAC/|MP3/}
-- - Functions: {lower:artist}, {upper:format}, {capitalize:title}
--
-- All text variables are automatically sanitized for filesystem safety
-- (removes characters like /, \, :, *, ?, ", <, >, |).
--
-- Example templates:
-- - "{album_artist}/{year} - {album}/"
-- - "{album_artist}/{year} - {album}/{if:multidisc|Disc {disc}/}{track:02} - {title}.{ext}"
-- - "{if:lossless|Lossless/|Lossy/}{album_artist}/{album}/"
module Skema.FileSystem.PathFormatter
  ( PathContext(..)
  , formatPath
  , formatTrackPath
  , parseTemplate
  , TemplatePart(..)
  ) where

import qualified Data.Text as T
import Data.Char (isSpace)
import Text.Read ()

-- | Context data for path formatting.
-- Contains all available variables that can be used in templates.
data PathContext = PathContext
  { -- Album-level metadata
    pcAlbumArtist :: Text
  , pcAlbum :: Text
  , pcYear :: Text
  , pcTotalDiscs :: Int
  , pcTotalTracks :: Int
  , pcCountry :: Maybe Text
  , pcLabel :: Maybe Text
  , pcCatalogNumber :: Maybe Text

  -- Track-level metadata (for individual file paths)
  , pcArtist :: Maybe Text
  , pcTitle :: Maybe Text
  , pcDisc :: Maybe Int
  , pcTrack :: Maybe Int
  , pcFormat :: Maybe Text  -- FLAC, MP3, AAC, etc.
  , pcBitrate :: Maybe Int
  , pcSampleRate :: Maybe Int
  , pcBitDepth :: Maybe Int
  , pcExtension :: Maybe Text  -- File extension
  } deriving (Show)

-- | A part of a parsed template.
data TemplatePart
  = Literal Text
  | Variable Text  -- {artist}
  | Function Text Text  -- {lower:artist}, {sanitize:album}
  | Conditional Text Text (Maybe Text)  -- {if:multidisc|Disc {disc}/|}, second arg is then-part, third is else-part
  deriving (Show, Eq)

-- | Parse a template string into parts.
parseTemplate :: Text -> [TemplatePart]
parseTemplate template = parseTemplateLoop template []

parseTemplateLoop :: Text -> [TemplatePart] -> [TemplatePart]
parseTemplateLoop "" acc = reverse acc
parseTemplateLoop text acc =
  case T.breakOn "{" text of
    (before, "") ->
      -- No more variables
      if T.null before
        then reverse acc
        else reverse (Literal before : acc)

    (before, rest) ->
      -- Found a variable/function/conditional
      case T.breakOn "}" rest of
        (_, "") ->
          -- No closing brace, treat as literal
          reverse (Literal text : acc)

        (varContent, after) ->
          let varText = T.drop 1 varContent  -- Remove opening {
              afterBrace = T.drop 1 after    -- Remove closing }
              beforePart = if T.null before then [] else [Literal before]

              varPart = parseVariablePart varText

          in parseTemplateLoop afterBrace (varPart : (beforePart <> acc))

parseVariablePart :: Text -> TemplatePart
parseVariablePart text
  | "if:" `T.isPrefixOf` text = parseConditional text
  | ":" `T.isInfixOf` text = parseFunction text
  | otherwise = Variable text

parseFunction :: Text -> TemplatePart
parseFunction text =
  case T.breakOn ":" text of
    (funcName, varPart) ->
      let varName = T.drop 1 varPart
      in Function funcName varName

parseConditional :: Text -> TemplatePart
parseConditional text =
  -- Format: if:condition|then-part|else-part
  -- or: if:condition|then-part
  let withoutIf = T.drop 3 text  -- Remove "if:"
      parts = T.splitOn "|" withoutIf
  in case parts of
    [condition, thenPart] -> Conditional condition thenPart Nothing
    [condition, thenPart, elsePart] -> Conditional condition thenPart (Just elsePart)
    [condition] -> Conditional condition "" Nothing
    _ -> Literal ("{" <> text <> "}")  -- Invalid, return as literal

-- | Format a path using a template and context.
-- This is for directory paths (album-level).
formatPath :: Text -> PathContext -> Text
formatPath template ctx =
  let parts = parseTemplate template
  in T.concat $ map (evaluatePart ctx) parts

-- | Format a full track path (directory + filename).
formatTrackPath :: Text -> Text -> PathContext -> Text
formatTrackPath dirTemplate fileTemplate ctx =
  let dirPath = formatPath dirTemplate ctx
      fileName = formatPath fileTemplate ctx
  in if T.null dirPath
       then fileName
       else dirPath <> "/" <> fileName

-- | Evaluate a template part to text.
evaluatePart :: PathContext -> TemplatePart -> Text
evaluatePart _ (Literal text) = text

evaluatePart ctx (Variable name) =
  lookupVariable ctx name

evaluatePart ctx (Function funcName varName) =
  let value = lookupVariable ctx varName
  in applyFunction funcName value

evaluatePart ctx (Conditional condName thenPart elsePart) =
  let condition = evaluateCondition ctx condName
      selectedPart = if condition then thenPart else fromMaybe "" elsePart
  in formatPath selectedPart ctx

-- | Look up a variable value in the context.
-- All text values are sanitized by default to ensure filesystem safety.
lookupVariable :: PathContext -> Text -> Text
lookupVariable PathContext{..} name = case name of
  "album_artist" -> sanitizePathComponent pcAlbumArtist
  "artist" -> sanitizePathComponent (fromMaybe pcAlbumArtist pcArtist)
  "album" -> sanitizePathComponent pcAlbum
  "year" -> pcYear  -- Year is numeric, no sanitization needed
  "disc" -> maybe "" (T.pack . show) pcDisc  -- Numeric
  "track" -> maybe "" (T.pack . show) pcTrack  -- Numeric
  "title" -> sanitizePathComponent (fromMaybe "Unknown" pcTitle)
  "format" -> sanitizePathComponent (fromMaybe "" pcFormat)
  "bitrate" -> maybe "" (T.pack . show) pcBitrate  -- Numeric
  "sample_rate" -> maybe "" (T.pack . show) pcSampleRate  -- Numeric
  "bit_depth" -> maybe "" (T.pack . show) pcBitDepth  -- Numeric
  "ext" -> fromMaybe "" pcExtension  -- Extension is already safe
  "total_discs" -> T.pack $ show pcTotalDiscs  -- Numeric
  "total_tracks" -> T.pack $ show pcTotalTracks  -- Numeric
  "country" -> sanitizePathComponent (fromMaybe "" pcCountry)
  "label" -> sanitizePathComponent (fromMaybe "" pcLabel)
  "catalog_number" -> sanitizePathComponent (fromMaybe "" pcCatalogNumber)

  -- Padded track/disc numbers (numeric, no sanitization needed)
  _ | "track:" `T.isPrefixOf` name -> formatPadded (T.drop 6 name) pcTrack
    | "disc:" `T.isPrefixOf` name -> formatPadded (T.drop 5 name) pcDisc
    | otherwise -> ""

-- | Format a number with padding (e.g., "track:02" formats 5 as "05")
formatPadded :: Text -> Maybe Int -> Text
formatPadded paddingSpec maybeNum =
  case maybeNum of
    Nothing -> ""
    Just num ->
      case readMaybe (toString paddingSpec) :: Maybe Int of
        Nothing -> T.pack $ show num
        Just width ->
          let numStr = show num
              padding = max 0 (width - length numStr)
          in T.pack $ replicate padding '0' <> numStr

-- | Apply a function to a value.
applyFunction :: Text -> Text -> Text
applyFunction "lower" value = T.toLower value
applyFunction "upper" value = T.toUpper value
applyFunction "sanitize" value = sanitizePathComponent value
applyFunction "capitalize" value = capitalize value
applyFunction "trim" value = T.strip value
applyFunction _ value = value  -- Unknown function, return value as-is

-- | Evaluate a boolean condition.
evaluateCondition :: PathContext -> Text -> Bool
evaluateCondition PathContext{..} name = case name of
  "multidisc" -> pcTotalDiscs > 1
  "lossless" -> isLosslessFormat pcFormat
  "compilation" -> False  -- TODO: Add compilation flag to context
  _ -> False

-- | Check if a format is lossless.
isLosslessFormat :: Maybe Text -> Bool
isLosslessFormat Nothing = False
isLosslessFormat (Just fmt) =
  let fmt' = T.toLower fmt
  in fmt' `elem` ["flac", "alac", "ape", "wav", "aiff", "wv", "tta", "dff", "dsf"]

-- | Sanitize a path component by removing/replacing invalid characters.
sanitizePathComponent :: Text -> Text
sanitizePathComponent text =
  let -- Replace problematic characters
      replaced = T.map replaceChar text
      -- Remove leading/trailing dots and spaces
      trimmed = T.dropWhile (\c -> c == '.' || isSpace c) replaced
      trimmed2 = T.dropWhileEnd (\c -> c == '.' || isSpace c) trimmed
      -- Collapse multiple spaces
      collapsed = T.unwords $ T.words trimmed2
  in if T.null collapsed then "Unknown" else collapsed

replaceChar :: Char -> Char
replaceChar c
  | c `elem` ['/', '\\', ':', '*', '?', '"', '<', '>', '|'] = '_'
  | c == '\0' = '_'
  | otherwise = c

-- | Capitalize first letter of each word.
capitalize :: Text -> Text
capitalize text =
  T.unwords $ map capitalizeWord (T.words text)
  where
    capitalizeWord "" = ""
    capitalizeWord w =
      let (firstChar, rest) = T.splitAt 1 w
      in T.toUpper firstChar <> T.toLower rest
