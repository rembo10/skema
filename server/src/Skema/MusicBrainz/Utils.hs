{-# LANGUAGE OverloadedStrings #-}

-- | MusicBrainz utility functions for data normalization.
module Skema.MusicBrainz.Utils
  ( normalizeJoinPhrase
  , normalizeReleaseJoinPhrases
  , normalizeTrackJoinPhrases
  ) where

import qualified Data.Text as T
import Skema.MusicBrainz.Types (MBRelease (..), MBMedium (..), MBTrack (..))

-- | Normalize join phrases in artist credits.
--
-- This function performs case-insensitive replacement of variations of "featuring"
-- with a normalized form (e.g., "feat.").
--
-- Examples:
-- - "JAY-Z featuring Gloria Carter" -> "JAY-Z feat. Gloria Carter"
-- - "Artist 1 Featuring Artist 2" -> "Artist 1 feat. Artist 2"
-- - "Band ft. Guest" -> "Band feat. Guest"
normalizeJoinPhrase :: Text -> Text -> Text
normalizeJoinPhrase normalizeTo text =
  let -- Common variations of "featuring" found in MusicBrainz join phrases
      variations =
        [ " featuring "
        , " Featuring "
        , " FEATURING "
        , " feat. "
        , " Feat. "
        , " feat "
        , " Feat "
        , " ft. "
        , " Ft. "
        , " ft "
        , " Ft "
        ]
      -- Replace each variation with the normalized form
      normalize t var = T.replace var (" " <> normalizeTo <> " ") t
  in foldl' normalize text variations

-- | Normalize join phrases in a release's artist field and all tracks.
normalizeReleaseJoinPhrases :: Text -> MBRelease -> MBRelease
normalizeReleaseJoinPhrases normalizeTo release =
  release { mbReleaseArtist = normalizeJoinPhrase normalizeTo (mbReleaseArtist release)
          , mbReleaseMedia = map normalizeMedium (mbReleaseMedia release)
          }
  where
    normalizeMedium medium = medium { mbMediumTracks = map (normalizeTrackJoinPhrases normalizeTo) (mbMediumTracks medium) }

-- | Normalize join phrases in a track's artist field.
normalizeTrackJoinPhrases :: Text -> MBTrack -> MBTrack
normalizeTrackJoinPhrases normalizeTo track =
  track { mbTrackArtist = fmap (normalizeJoinPhrase normalizeTo) (mbTrackArtist track) }
