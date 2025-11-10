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
groupFilesByRelease :: [(OsPath, Metadata)] -> [FileGroup]
groupFilesByRelease files =
  let grouped = Map.fromListWith (<>)
        [ (groupKey meta, [(path, meta)])
        | (path, meta) <- files
        ]
  in map (uncurry makeFileGroup) (Map.toList grouped)
  where
    groupKey meta = (album meta, albumArtist meta <|> artist meta)

    makeFileGroup (albumName, artistName) fileList =
      let -- Get first metadata (list should never be empty due to grouping)
          firstMeta = case viaNonEmpty head fileList of
            Just (_, meta) -> meta
            Nothing -> error "Empty file list in makeFileGroup"

          -- Get directory from first file
          directory = case viaNonEmpty head fileList of
            Just (path, _) -> OP.takeDirectory path
            Nothing -> error "Empty file list in makeFileGroup"

          mbIds = musicBrainzIds firstMeta
      in FileGroup
        { fgDirectory = directory
        , fgAlbum = albumName
        , fgArtist = artistName
        , fgReleaseId = MBID <$> M.mbReleaseId mbIds
        , fgReleaseGroupId = MBID <$> M.mbReleaseGroupId mbIds
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

-- | Calculate cost between a file and a MusicBrainz track.
-- Lower cost = better match. Returns value between 0.0 and 1.0.
trackCost :: (OsPath, Metadata) -> MBTrack -> Double
trackCost (_, meta) mbTrack =
  let
    -- Title similarity (most important)
    titleCost = case title meta of
      Just t -> textSimilarityCost t (mbTrackTitle mbTrack)
      Nothing -> 0.8  -- No title = high cost

    -- Duration difference (if available)
    -- Duration in Metadata is in seconds (Int), need to convert to milliseconds
    durationCost = case (duration (audioProperties meta), mbTrackLength mbTrack) of
      (Just fileDur, Just mbDur) ->
        let fileDurMs = fileDur * 1000  -- Convert seconds to milliseconds
            diff = abs (fileDurMs - mbDur)
            -- Allow ±2 seconds tolerance
            tolerance = 2000
        in if diff <= tolerance
          then 0.0
          else min 1.0 (fromIntegral diff / fromIntegral (mbDur + tolerance))
      (Nothing, Nothing) -> 0.0  -- Both unknown
      _ -> 0.3  -- One unknown = medium cost

    -- Track number hint
    trackNumCost = case trackNumber meta of
      Just trackNum ->
        if trackNum == mbTrackPosition mbTrack
          then 0.0
          else 0.5  -- Wrong position = medium penalty
      Nothing -> 0.1  -- Unknown = small penalty

    -- Weighted combination
    weights =
      [ (5.0, titleCost)
      , (3.0, durationCost)
      , (1.0, trackNumCost)
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
matchTracksToRelease :: FileGroup -> MBRelease -> [TrackMatch]
matchTracksToRelease fg mbr =
  let
    files = fgFiles fg
    mbTracks = mbReleaseTracks mbr

    -- Cost function for assignment algorithm
    -- Returns cost scaled to Int (0-1000 range)
    costFn :: (OsPath, Metadata) -> MBTrack -> Int
    costFn file track = round (trackCost file track * 1000.0)

    -- Run Hungarian algorithm
    -- Returns list of (file, track) pairs
    assignment = assign costFn files mbTracks

    -- Build matches from assignment
    matches =
      [ TrackMatch
          { tmFilePath = fst file
          , tmTrack = track
          , tmCost = trackCost file track
          , tmConfidence = 1.0 - trackCost file track
          }
      | (file, track) <- assignment
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
