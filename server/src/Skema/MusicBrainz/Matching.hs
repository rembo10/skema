{-# LANGUAGE OverloadedStrings #-}

-- | MusicBrainz matching using the Hungarian algorithm.
--
-- This module implements optimal matching between local files and MusicBrainz
-- releases/tracks using cost matrices and the Munkres (Hungarian) algorithm.
module Skema.MusicBrainz.Matching
  ( -- * Release Matching
    matchReleases
  , releaseCost
    -- * Track Matching
  , matchTracksToRelease
  , trackCost
    -- * Grouping
  , groupFilesByRelease
  ) where

import Skema.MusicBrainz.Types
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Algorithm.Assignment (assign)
import Text.EditDistance (defaultEditCosts, levenshteinDistance)
import System.OsPath (OsPath)
import qualified System.OsPath as OP
import Monatone.Metadata (Metadata(..), AudioProperties(..))
import qualified Monatone.Metadata as M

-- | Group scanned files by probable release (directory + album tag).
-- Returns Nothing for empty input or if grouping produces empty groups.
groupFilesByRelease :: [(OsPath, Metadata)] -> [FileGroup]
groupFilesByRelease files =
  let grouped = Map.fromListWith (<>)
        [ (groupKey meta, [(path, meta)])
        | (path, meta) <- files
        ]
  in mapMaybe (uncurry makeFileGroup) (Map.toList grouped)
  where
    groupKey meta = (album meta, albumArtist meta <|> artist meta)

    -- Returns Nothing if the file list is empty (defensive, shouldn't happen)
    makeFileGroup :: (Maybe Text, Maybe Text) -> [(OsPath, Metadata)] -> Maybe FileGroup
    makeFileGroup _ [] = Nothing  -- Empty file list - skip this group
    makeFileGroup (albumName, artistName) fileList@((firstPath, firstMeta):_) =
      let directory = OP.takeDirectory firstPath
          mbIds = musicBrainzIds firstMeta
      in Just FileGroup
        { fgDirectory = directory
        , fgAlbum = albumName
        , fgArtist = artistName
        , fgReleaseId = MBID <$> mfilter (not . T.null) (M.mbReleaseId mbIds)
        , fgReleaseGroupId = MBID <$> mfilter (not . T.null) (M.mbReleaseGroupId mbIds)
        , fgLabel = recordLabel firstMeta
        , fgCatalogNumber = catalogNumber firstMeta
        , fgBarcode = barcode firstMeta
        , fgCountry = releaseCountry firstMeta
        , fgDate = date firstMeta
        , fgFiles = fileList
        }

-- | Check if a release passes filter rules (hard requirements)
-- Returns True if the release could be a valid match
passesFilterRules :: FileGroup -> MBRelease -> Bool
passesFilterRules fg mbr =
  let
    -- Rule 1: Release must have at least as many tracks as user has
    fileCount = length (fgFiles fg)
    mbCount = length (mbReleaseTracks mbr)
    trackCountRule = mbCount >= fileCount

    -- Rule 2: If user has country tag, release must match (or be unknown)
    countryRule = case (fgCountry fg, mbReleaseCountry mbr) of
      (Just userCountry, Just mbCountry) ->
        -- Exact match or normalize common variants
        normalizeText userCountry == normalizeText mbCountry
      _ -> True  -- If either is missing, don't filter

    -- Rule 3: If user has barcode, it must match exactly
    barcodeRule = case (fgBarcode fg, mbReleaseBarcode mbr) of
      (Just userBarcode, Just mbBarcode) -> userBarcode == mbBarcode
      _ -> True  -- If either is missing, don't filter

  in trackCountRule && countryRule && barcodeRule
  -- Note: Label is NOT used as a filter because it's often incorrectly tagged
  -- (e.g., artist name instead of record label). It's used in scoring instead.

-- | Calculate cost between a file group and a MusicBrainz release.
-- Lower cost = better match. Returns value between 0.0 and 1.0.
-- This is only called on releases that pass filter rules.
releaseCost :: FileGroup -> MBRelease -> Double
releaseCost fg mbr =
  let
    -- If we have an exact MusicBrainz ID match, give perfect score
    exactMatch = case fgReleaseId fg of
      Just rid | rid == mbReleaseId mbr -> True
      _ -> False

    -- Track count: penalize if release has fewer tracks, small penalty for more
    fileCount = length (fgFiles fg)
    mbCount = length (mbReleaseTracks mbr)
    countCost = if mbCount == fileCount
                then 0.0  -- Exact match - perfect
                else if mbCount > fileCount
                  then 0.1  -- Release has more tracks - minor penalty (bonus tracks/deluxe edition)
                  else 0.8  -- Release has fewer tracks - major issue

    -- Album title similarity (Levenshtein distance)
    titleCost = case fgAlbum fg of
      Just albumName -> textSimilarityCost albumName (mbReleaseTitle mbr)
      Nothing -> 0.3  -- Unknown = small cost (don't penalize too much)

    -- Artist similarity
    artistCost = case fgArtist fg of
      Just artistName -> textSimilarityCost artistName (mbReleaseArtist mbr)
      Nothing -> 0.3  -- Unknown = small cost

    -- Bonus for matching optional fields (already filtered, so just bonus points)
    catalogCost = compareMaybe (fgCatalogNumber fg) (mbReleaseCatalogNumber mbr) textSimilarityCost
    dateCost = compareMaybe (fgDate fg) (mbReleaseDate mbr) textSimilarityCost
    labelCost = compareMaybe (fgLabel fg) (mbReleaseLabel mbr) textSimilarityCost

    -- Weighted combination
    -- When we have an exact ID match, return near-zero cost
    -- Otherwise, weight title and artist heavily
    weights = if exactMatch
      then [(1.0, 0.0)]  -- Perfect match
      else
        [ (10.0, titleCost)     -- Album title most important
        , (8.0, artistCost)     -- Artist very important
        , (3.0, countCost)      -- Track count matters
        , (1.0, catalogCost)    -- Catalog number nice to have
        , (0.5, labelCost)      -- Label nice to have (but often incorrectly tagged)
        , (0.5, dateCost)       -- Date minor factor
        ]

    totalWeight = sum (map fst weights)
    weightedSum = sum [w * c | (w, c) <- weights]
  in
    weightedSum / totalWeight

-- | Calculate cost between a file and a MusicBrainz track with disc/position info.
-- Lower cost = better match. Returns value between 0.0 and 1.0.
--
-- Takes enriched track information:
-- - Int: Disc number (1-based)
-- - Int: Absolute position in release (1-based, counting across all discs)
-- - MBTrack: The track data
trackCost :: (OsPath, Metadata) -> (Int, Int, MBTrack) -> Double
trackCost (_, meta) (discNum, absolutePos, mbTrack) =
  let
    -- Title similarity (still important)
    titleCost = case title meta of
      Just t -> textSimilarityCost t (mbTrackTitle mbTrack)
      Nothing -> 0.8  -- No title = high cost

    -- Duration difference (STRONGER signal now)
    -- Duration in Metadata is in seconds (Int), need to convert to milliseconds
    durationCost = case (duration (audioProperties meta), mbTrackLength mbTrack) of
      (Just fileDur, Just mbDur) ->
        let fileDurMs = fileDur * 1000  -- Convert seconds to milliseconds
            diff = abs (fileDurMs - mbDur)
            -- Tightened tolerance: ±1 second for perfect match
            tolerance = 1000
        in if diff <= tolerance
          then 0.0  -- Perfect match within 1 second
          else if diff <= 2000
            then 0.2  -- Close match within 2 seconds
            else min 1.0 (fromIntegral diff / fromIntegral (mbDur + tolerance))
      (Nothing, Nothing) -> 0.0  -- Both unknown
      _ -> 0.3  -- One unknown = medium cost

    -- Position matching: prefer disc+position, fall back to absolute
    -- This handles both disc-aware files (Bon Iver) and disc-unaware (Beach House)
    positionCost = case (discNumber meta, trackNumber meta) of
      (Just fileDisc, Just fileTrack) ->
        -- File has disc metadata - match BOTH disc AND per-disc position
        if fileDisc == discNum && fileTrack == mbTrackPosition mbTrack
          then 0.0  -- Perfect match: correct disc and position
          else if fileDisc == discNum
            then 0.5  -- Right disc, wrong track
            else 1.0  -- Wrong disc
      (Nothing, Just fileTrack) ->
        -- File only has track number (no disc) - try absolute position
        if fileTrack == absolutePos
          then 0.0  -- Matches absolute position
          else if fileTrack == mbTrackPosition mbTrack
            then 0.3  -- Matches per-disc position (might be disc-unaware metadata)
            else 0.6  -- Neither matches well
      _ -> 0.2  -- No track info - small penalty

    -- Check if duration matches well (within 2 seconds)
    durationMatches = case (duration (audioProperties meta), mbTrackLength mbTrack) of
      (Just fileDur, Just mbDur) -> abs (fileDur * 1000 - mbDur) <= 2000
      _ -> False

    -- Check if position matches perfectly
    positionMatches = case (discNumber meta, trackNumber meta) of
      (Just fileDisc, Just fileTrack) -> fileDisc == discNum && fileTrack == mbTrackPosition mbTrack
      (Nothing, Just fileTrack) -> fileTrack == absolutePos
      _ -> False

    -- Combo bonus: reward perfect duration + position matches
    comboBonus = if durationMatches && positionMatches
                 then -0.3  -- Negative cost = bonus (reduces total cost)
                 else 0.0

    -- Weighted combination
    weights =
      [ (5.0, titleCost)      -- Title still important
      , (5.0, durationCost)   -- INCREASED from 3.0 - duration is now equal to title
      , (4.0, positionCost)   -- INCREASED from 1.0 - position is critical
      , (2.0, comboBonus)     -- NEW: bonus for perfect matches
      ]

    totalWeight = sum (map fst weights)
    weightedSum = sum [w * c | (w, c) <- weights]
  in
    weightedSum / totalWeight

-- | Match a list of candidate releases to a file group.
-- Returns releases sorted by cost (best match first).
-- First filters candidates using hard rules, then scores remaining candidates.
matchReleases :: FileGroup -> [MBRelease] -> [(MBRelease, Double)]
matchReleases fg candidates =
  let -- Step 1: Filter candidates using hard rules
      filtered = filter (passesFilterRules fg) candidates
      -- Step 2: Score remaining candidates
      costs = [(mbr, releaseCost fg mbr) | mbr <- filtered]
  in sortOn snd costs

-- | Match tracks in a file group to tracks in a MusicBrainz release
-- using the Hungarian algorithm for optimal assignment.
--
-- This function now properly handles multi-disc releases by tracking both
-- disc number and absolute position for each track.
matchTracksToRelease :: FileGroup -> MBRelease -> [TrackMatch]
matchTracksToRelease fg mbr =
  let
    files = fgFiles fg

    -- Create enriched track list with disc number and absolute position
    -- For each track, we need (discNum, absolutePos, track)
    enrichedTracks =
      [ (mbMediumPosition medium, absPos, track)
      | medium <- mbReleaseMedia mbr
      , let tracksBeforeThisDisc = sum $ map mbMediumTrackCount $
              filter (\m -> mbMediumPosition m < mbMediumPosition medium) (mbReleaseMedia mbr)
      , (localIdx, track) <- zip [1..] (mbMediumTracks medium)
      , let absPos = tracksBeforeThisDisc + localIdx
      ]

    -- Cost function for assignment algorithm
    -- Returns cost scaled to Int (0-1000 range)
    costFn :: (OsPath, Metadata) -> (Int, Int, MBTrack) -> Int
    costFn file enrichedTrack = round (trackCost file enrichedTrack * 1000.0)

    -- Run Hungarian algorithm with enriched track data
    -- Returns list of (file, enrichedTrack) pairs
    assignment = assign costFn files enrichedTracks

    -- Build matches from assignment
    matches =
      [ TrackMatch
          { tmFilePath = fst file
          , tmTrack = track  -- Just the track, not the enriched data
          , tmCost = trackCost file enrichedTrack
          , tmConfidence = 1.0 - trackCost file enrichedTrack
          }
      | (file, enrichedTrack@(_, _, track)) <- assignment
      ]
  in
    matches

-- * Helper functions

-- | Compare two Maybe values using a cost function.
-- Returns 0.0 if either value is Nothing (field is not used in matching),
-- or the cost if both are Just.
compareMaybe :: Maybe Text -> Maybe Text -> (Text -> Text -> Double) -> Double
compareMaybe Nothing _ _ = 0.0  -- File doesn't have this field - don't use it in matching
compareMaybe _ Nothing _ = 0.0  -- MB release doesn't have this field - don't use it in matching
compareMaybe (Just a) (Just b) f = f a b

-- | Calculate text similarity cost using Levenshtein distance.
-- Returns 0.0 for identical strings, 1.0 for completely different.
textSimilarityCost :: Text -> Text -> Double
textSimilarityCost a b =
  let
    -- Normalize Unicode punctuation and case
    a' = T.toLower $ normalizeText a
    b' = T.toLower $ normalizeText b
    dist = levenshteinDistance defaultEditCosts (toString a') (toString b')
    maxLen = max (T.length a) (T.length b)
  in
    if maxLen == 0
      then 0.0
      else fromIntegral dist / fromIntegral maxLen

-- | Normalize text by converting Unicode punctuation variants to ASCII equivalents
-- This helps match text with different Unicode characters (e.g., "alt‐J" vs "alt-J")
normalizeText :: Text -> Text
normalizeText =
  -- Normalize various Unicode hyphens/dashes to ASCII hyphen-minus
  T.replace "\x2010" "-"  -- HYPHEN
  . T.replace "\x2011" "-"  -- NON-BREAKING HYPHEN
  . T.replace "\x2012" "-"  -- FIGURE DASH
  . T.replace "\x2013" "-"  -- EN DASH
  . T.replace "\x2014" "-"  -- EM DASH
  . T.replace "\x2015" "-"  -- HORIZONTAL BAR
  -- Normalize various Unicode quotes to ASCII
  . T.replace "\x2018" "'"  -- LEFT SINGLE QUOTATION MARK
  . T.replace "\x2019" "'"  -- RIGHT SINGLE QUOTATION MARK
  . T.replace "\x201C" "\""  -- LEFT DOUBLE QUOTATION MARK
  . T.replace "\x201D" "\""  -- RIGHT DOUBLE QUOTATION MARK
