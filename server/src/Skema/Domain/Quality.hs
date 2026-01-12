{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Quality profile system for managing download quality preferences.
--
-- This module provides types and functions for:
-- - Defining audio quality levels (FLAC, 320k, V0, etc.)
-- - Creating quality profiles with custom rankings
-- - Parsing quality from release titles
-- - Comparing qualities based on profile preferences
module Skema.Domain.Quality
  ( -- * Quality Types
    Quality (..)
  , QualityPreference (..)
  , QualityProfile (..)
  , QualityProfileRecord (..)
    -- * Parsing
  , parseQuality
  , parseQualityFromTitle
  , detectQualityFromAudio
    -- * Profile Operations
  , meetsProfile
  , needsUpgrade
  , compareQualities
  , getQualityRank
  , isBetterQuality
    -- * Conversion
  , qualityToText
  , textToQuality
  , qualityPreferencesToJSON
  , qualityPreferencesFromJSON
    -- * Upgrade Detection
  , shouldUpgrade
  , selectBestQuality
  , meetsOrExceedsCutoff
  ) where

import GHC.Generics ()
import qualified Data.Text as T
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, (.:), withText)
import qualified Data.Aeson as Aeson
import qualified Monatone.Metadata as Monatone

-- | Audio quality levels, in rough order of file size/quality.
data Quality
  = Unknown           -- ^ Quality could not be determined
  | MP3_192          -- ^ MP3 at 192 kbps
  | VBR2             -- ^ MP3 VBR V2 (roughly 190 kbps average)
  | MP3_256          -- ^ MP3 at 256 kbps
  | VBR0             -- ^ MP3 VBR V0 (roughly 245 kbps average)
  | MP3_320          -- ^ MP3 at 320 kbps
  | Lossless         -- ^ FLAC, ALAC, APE (16-bit/44.1kHz)
  | HiResLossless    -- ^ 24-bit FLAC or higher sample rates (96kHz, 192kHz, etc.)
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

instance ToJSON Quality where
  toJSON = toJSON . qualityToText

instance FromJSON Quality where
  parseJSON = withText "Quality" $ \t ->
    case textToQuality t of
      Just q -> pure q
      Nothing -> fail $ "Unknown quality: " <> toString t

-- | Quality preference for a specific quality level.
data QualityPreference = QualityPreference
  { qpQuality :: Quality
    -- ^ The quality level
  , qpRank :: Int
    -- ^ Custom ranking (higher = more preferred)
  , qpEnabled :: Bool
    -- ^ Whether this quality is acceptable
  } deriving (Show, Eq, Generic)

instance ToJSON QualityPreference where
  toJSON (QualityPreference quality rank enabled) = object
    [ "quality" .= quality
    , "rank" .= rank
    , "enabled" .= enabled
    ]

instance FromJSON QualityPreference where
  parseJSON = withObject "QualityPreference" $ \o ->
    QualityPreference
      <$> o .: "quality"
      <*> o .: "rank"
      <*> o .: "enabled"

-- | Quality profile defining acceptable qualities and preferences.
data QualityProfile = QualityProfile
  { qfId :: Maybe Int64
    -- ^ Database ID (Nothing for new profiles)
  , qfName :: Text
    -- ^ Profile name
  , qfQualityPreferences :: [QualityPreference]
    -- ^ List of qualities with ranks
  , qfCutoffQuality :: Quality
    -- ^ Once you have this quality or better, stop upgrading
  , qfUpgradeAutomatically :: Bool
    -- ^ Whether to automatically search for upgrades
  } deriving (Show, Eq, Generic)

instance ToJSON QualityProfile where
  toJSON (QualityProfile id' name prefs cutoff upgrade) = object
    [ "id" .= id'
    , "name" .= name
    , "quality_preferences" .= prefs
    , "cutoff_quality" .= cutoff
    , "upgrade_automatically" .= upgrade
    ]

instance FromJSON QualityProfile where
  parseJSON = withObject "QualityProfile" $ \o ->
    QualityProfile
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "quality_preferences"
      <*> o .: "cutoff_quality"
      <*> o .: "upgrade_automatically"

-- | Quality profile record as stored in database.
data QualityProfileRecord = QualityProfileRecord
  { qprId :: Maybe Int64
  , qprName :: Text
  , qprCutoffQuality :: Text
  , qprQualityPreferencesJSON :: Text
  , qprUpgradeAutomatically :: Bool
  } deriving (Show, Eq, Generic)

-- | Convert Quality to Text for database storage.
qualityToText :: Quality -> Text
qualityToText Unknown = "unknown"
qualityToText MP3_192 = "mp3_192"
qualityToText VBR2 = "vbr2"
qualityToText MP3_256 = "mp3_256"
qualityToText VBR0 = "vbr0"
qualityToText MP3_320 = "mp3_320"
qualityToText Lossless = "lossless"
qualityToText HiResLossless = "hires_lossless"

-- | Convert Text to Quality.
textToQuality :: Text -> Maybe Quality
textToQuality "unknown" = Just Unknown
textToQuality "mp3_192" = Just MP3_192
textToQuality "vbr2" = Just VBR2
textToQuality "mp3_256" = Just MP3_256
textToQuality "vbr0" = Just VBR0
textToQuality "mp3_320" = Just MP3_320
textToQuality "lossless" = Just Lossless
textToQuality "hires_lossless" = Just HiResLossless
textToQuality _ = Nothing

-- | Encode quality preferences to JSON for database storage.
qualityPreferencesToJSON :: [QualityPreference] -> Text
qualityPreferencesToJSON prefs =
  decodeUtf8 $ Aeson.encode prefs

-- | Decode quality preferences from JSON.
qualityPreferencesFromJSON :: Text -> Maybe [QualityPreference]
qualityPreferencesFromJSON jsonText =
  Aeson.decode (encodeUtf8 jsonText)

-- | Parse quality from a release title.
--
-- Examples:
--   "Artist - Album (2020) [FLAC]" -> Just Lossless
--   "Artist - Album [320]" -> Just MP3_320
--   "Artist - Album [24bit FLAC]" -> Just HiResLossless
--   "Artist - Album V0" -> Just VBR0
parseQualityFromTitle :: Text -> Quality
parseQualityFromTitle title =
  fromMaybe Unknown $ parseQuality title

-- | Parse quality from text (release title, format string, etc.).
parseQuality :: Text -> Maybe Quality
parseQuality text =
  let lower = T.toLower text
      hasWord w = w `T.isInfixOf` lower

      -- Check for hi-res indicators
      isHiRes = hasWord "24bit" || hasWord "24-bit" || hasWord "24 bit"
             || hasWord "96khz" || hasWord "192khz" || hasWord "dsd"
             || hasWord "sacd" || hasWord "24/96" || hasWord "24/192"

      -- Check for lossless formats
      isFlac = hasWord "flac"
      isAlac = hasWord "alac"
      isApe = hasWord "ape" || hasWord "monkey"
      isLossless = isFlac || isAlac || isApe || hasWord "lossless"

      -- Check for MP3 bitrates
      is320 = hasWord "320" || hasWord "cbr320"
      is256 = hasWord "256" || hasWord "cbr256"
      is192 = hasWord "192" || hasWord "cbr192"

      -- Check for VBR
      isV0 = (hasWord "v0" || hasWord "vbr0" || hasWord "vbr 0")
          && not (hasWord "vbr2")  -- Exclude VBR2
      isV2 = hasWord "v2" || hasWord "vbr2" || hasWord "vbr 2"

  in case () of
       _ | isHiRes && isLossless -> Just HiResLossless
         | isLossless -> Just Lossless
         | is320 -> Just MP3_320
         | isV0 -> Just VBR0
         | is256 -> Just MP3_256
         | isV2 -> Just VBR2
         | is192 -> Just MP3_192
         | otherwise -> Nothing

-- | Detect quality from audio file metadata (codec, bitrate, bit depth, sample rate).
-- This is used to determine the quality of files already in the library.
detectQualityFromAudio :: Monatone.Metadata -> Quality
detectQualityFromAudio metadata =
  let audioProps = Monatone.audioProperties metadata
      fmt = Monatone.format metadata
      bitrate = Monatone.bitrate audioProps
      bitsPerSample = Monatone.bitsPerSample audioProps
      sampleRate = Monatone.sampleRate audioProps

      -- Check if this is hi-res (24-bit or high sample rate)
      isHiRes = case (bitsPerSample, sampleRate) of
        (Just bits, _) | bits >= 24 -> True
        (_, Just rate) | rate >= 88200 -> True  -- 88.2kHz, 96kHz, 192kHz, etc.
        _ -> False

  in case fmt of
       -- Lossless formats
       Monatone.FLAC | isHiRes -> HiResLossless
                     | otherwise -> Lossless
       Monatone.M4A | isHiRes -> HiResLossless  -- Assume ALAC for now
                    | otherwise -> Lossless

       -- Lossy formats (MP3, OGG, Opus)
       _ -> case bitrate of
         Just br
           | br >= 310 -> MP3_320  -- 310+ kbps -> MP3 320
           | br >= 245 -> VBR0     -- 245-309 kbps -> VBR V0 (~245 kbps average)
           | br >= 225 -> MP3_256  -- 225-244 kbps -> MP3 256
           | br >= 180 -> VBR2     -- 180-224 kbps -> VBR V2 (~190 kbps average)
           | br >= 160 -> MP3_192  -- 160-179 kbps -> MP3 192
         _ -> Unknown

-- | Check if a quality meets the profile requirements.
meetsProfile :: Quality -> QualityProfile -> Bool
meetsProfile quality profile =
  case find (\pref -> qpQuality pref == quality) (qfQualityPreferences profile) of
    Nothing -> False
    Just pref -> qpEnabled pref

-- | Check if a quality needs to be upgraded according to the profile.
--
-- Returns True if the current quality is below the cutoff quality.
needsUpgrade :: Quality -> QualityProfile -> Bool
needsUpgrade currentQuality profile =
  let currentRank = getQualityRank currentQuality profile
      cutoffRank = getQualityRank (qfCutoffQuality profile) profile
  in currentRank < cutoffRank

-- | Get the rank for a quality from the profile.
--
-- Returns 0 if quality is not in profile or disabled.
getQualityRank :: Quality -> QualityProfile -> Int
getQualityRank quality profile =
  case find (\pref -> qpQuality pref == quality) (qfQualityPreferences profile) of
    Just pref | qpEnabled pref -> qpRank pref
    _ -> 0

-- | Compare two qualities according to a profile's preferences.
--
-- Returns GT if quality1 is better than quality2, LT if worse, EQ if equal.
compareQualities :: QualityProfile -> Quality -> Quality -> Ordering
compareQualities profile q1 q2 =
  compare (getQualityRank q1 profile) (getQualityRank q2 profile)

-- | Check if quality1 is better than quality2 according to the profile.
isBetterQuality :: QualityProfile -> Quality -> Quality -> Bool
isBetterQuality profile q1 q2 =
  compareQualities profile q1 q2 == GT

-- | Check if an album should be upgraded based on current quality and profile.
--
-- Returns True if:
-- 1. Upgrade is enabled in the profile
-- 2. Current quality is below cutoff
-- 3. Current quality is enabled in profile
shouldUpgrade :: Quality -> QualityProfile -> Bool
shouldUpgrade currentQuality profile =
  qfUpgradeAutomatically profile
  && needsUpgrade currentQuality profile
  && meetsProfile currentQuality profile

-- | Select the best quality from a list according to the profile.
--
-- Filters to only enabled qualities, then picks the highest ranked one.
selectBestQuality :: [Quality] -> QualityProfile -> Maybe Quality
selectBestQuality qualities profile =
  let enabledQualities = filter (`meetsProfile` profile) qualities
      sortedByRank = sortBy (\q1 q2 -> compareQualities profile q2 q1) enabledQualities
  in viaNonEmpty head sortedByRank

-- | Check if a quality meets or exceeds the cutoff quality.
meetsOrExceedsCutoff :: Quality -> QualityProfile -> Bool
meetsOrExceedsCutoff quality profile =
  let cutoff = qfCutoffQuality profile
  in quality == cutoff || isBetterQuality profile quality cutoff
