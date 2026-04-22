{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Pure release scoring and ranking logic.
--
-- Scoring combines: download-type format score, title-tag quality bonus
-- (FLAC/320/V0), seeder score, size score, popularity, and optional
-- album-title match bonus/penalty.
module Skema.Domain.Scoring
  ( scoreRelease
  , rankReleases
  ) where

import qualified Data.Text as T

import Skema.Indexer.Types (ReleaseInfo(..), DownloadType(..))

-- | Rank releases by descending score. Pass the target album title to
-- enable title-match scoring (exact-match bonus and sequel-number penalty).
rankReleases :: Maybe Text -> [ReleaseInfo] -> [ReleaseInfo]
rankReleases maybeAlbumTitle = sortOn (Down . scoreRelease maybeAlbumTitle)

-- | Calculate quality score for a release.
--
-- Higher score = better. The optional album title enables title matching
-- (exact match is +3000, sequel-number mismatch like "Blueprint 3" when
-- searching "Blueprint" is -2000).
scoreRelease :: Maybe Text -> ReleaseInfo -> Int
scoreRelease maybeAlbumTitle ReleaseInfo{..} =
  formatScore + qualityBonus + seedScore + sizeScore + grabScore + titleMatchScore
  where
    formatScore = case riDownloadType of
      NZB -> 500
      Torrent -> case riSeeders of
        Just s | s > (10 :: Int) -> 400
        Just s | s > 0           -> 200
        _                        -> 50
      Slskd -> case riSeeders of
        Just s | s > 5 -> 350
        Just s | s > 0 -> 150
        _              -> 100

    titleLower = T.toLower riTitle
    hasFlac = "flac" `T.isInfixOf` titleLower
    hasLossless = hasFlac || "alac" `T.isInfixOf` titleLower || "ape" `T.isInfixOf` titleLower
    has320 = "320" `T.isInfixOf` titleLower
    hasV0 = " v0" `T.isInfixOf` titleLower || "-v0" `T.isInfixOf` titleLower

    qualityBonus
      | hasLossless = 1000
      | has320      = 600
      | hasV0       = 400
      | otherwise   = 0

    seedScore = case riSeeders of
      Just s -> min 500 (s * 10)
      Nothing -> 0

    sizeScore = case riSize of
      Just size ->
        let mb = fromIntegral size / (1024.0 * 1024.0 :: Double)
        in if mb > 50 && mb < 2000
           then 100
           else if mb > 2000 && mb < 5000
                then 50
                else 0
      Nothing -> 0

    grabScore = case riGrabs of
      Just g -> min 200 (g * 2)
      Nothing -> 0

    titleMatchScore = case maybeAlbumTitle of
      Nothing -> 0
      Just searchTitle ->
        let titleLowerClean = T.toLower $ T.replace "." " " $ T.replace "-" " " riTitle
            searchLowerClean = T.toLower $ T.replace "." " " $ T.replace "-" " " searchTitle
            cleanWord w = T.filter (\c -> c /= '(' && c /= ')' && c /= '[' && c /= ']') w
            titleWords = map cleanWord $ T.words titleLowerClean
            searchWords = map cleanWord $ T.words searchLowerClean
            hasExactMatch = searchLowerClean `T.isInfixOf` titleLowerClean
            hasExtraNumbers = any (\w -> T.all (\c -> c >= '0' && c <= '9') w && T.length w <= 2 && w `notElem` searchWords) titleWords
            extraNumberPenalty = if hasExtraNumbers then -2000 else 0
            exactMatchBonus = if hasExactMatch then 3000 else 0
        in exactMatchBonus + extraNumberPenalty
