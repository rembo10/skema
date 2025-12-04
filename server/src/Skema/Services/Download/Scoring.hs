{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Release scoring and ranking logic.
--
-- This module provides utilities for scoring and ranking releases based on
-- quality metrics like format, seeders, size, and popularity.
module Skema.Services.Download.Scoring
  ( scoreRelease
  ) where

import Skema.Indexer.Types (ReleaseInfo(..), DownloadType(..))

-- ============================================================================
-- SCORING
-- ============================================================================

-- | Calculate quality score for a release.
scoreRelease :: ReleaseInfo -> Int
scoreRelease ReleaseInfo{..} =
  formatScore + seedScore + sizeScore + grabScore
  where
    -- Format score (prefer lossless)
    formatScore = case riDownloadType of
      NZB -> 500  -- NZBs generally more reliable
      Torrent -> case riSeeders of
        Just s | s > 10 -> 400  -- Well-seeded torrents
        Just s | s > 0  -> 200  -- Some seeders
        _               -> 50   -- No seeder info or dead

    -- Seeder score (for torrents)
    seedScore = case riSeeders of
      Just s -> min 500 (s * 10)  -- Cap at 500
      Nothing -> 0

    -- Size score (prefer reasonable sizes, penalize too small or too large)
    sizeScore = case riSize of
      Just size ->
        let mb = fromIntegral size / (1024.0 * 1024.0) :: Double
        in if mb > 50 && mb < 2000  -- 50MB - 2GB range
           then 100
           else if mb > 2000 && mb < 5000
                then 50  -- Large but acceptable
                else 0   -- Too small or too large
      Nothing -> 0

    -- Grab score (popularity)
    grabScore = case riGrabs of
      Just g -> min 200 (g * 2)  -- Cap at 200
      Nothing -> 0
