{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skema.Indexer.Search
  ( searchAllIndexers
  , searchForAlbum
  , rankResults
  , rankResultsWithContext
  , rankResultsWithProfile
  ) where

import Control.Concurrent.Async (mapConcurrently)
import qualified Data.Text as T

import Skema.Config.Types (Indexer(..), IndexerConfig(..))
import Skema.Database.Types (CatalogAlbumRecord(..))
import Skema.Indexer.Types
import Skema.Indexer.Client
import Skema.HTTP.Client (HttpClient)
import qualified Skema.Domain.Quality as Quality

-- | Search all enabled indexers for a query
searchAllIndexers :: HttpClient -> IndexerConfig -> SearchQuery -> IO SearchResult
searchAllIndexers client IndexerConfig{..} query = do
  -- Filter enabled indexers and sort by priority
  let enabledIndexers = filter indexerEnabled indexerList
      sortedIndexers = sortOn indexerPriority enabledIndexers

  -- Search all indexers concurrently
  results <- mapConcurrently (\indexer -> searchIndexer client indexer query) sortedIndexers

  -- Partition results and errors
  let (errors, successes) = partitionEithers results
      totalReleases = sum $ map (length . irReleases) successes

  pure SearchResult
    { srResults = successes
    , srErrors = errors
    , srTotalReleases = totalReleases
    }

-- | Search for a specific catalog album
searchForAlbum :: HttpClient -> IndexerConfig -> CatalogAlbumRecord -> IO SearchResult
searchForAlbum client indexerConfig CatalogAlbumRecord{..} = do
  -- Extract year from first release date (format: YYYY-MM-DD)
  let year = catalogAlbumFirstReleaseDate >>= \date ->
        case T.take 4 date of
          y | T.length y == 4 -> readMaybe (T.unpack y)
          _ -> Nothing

      query = SearchQuery
        { sqArtist = Just catalogAlbumArtistName
        , sqAlbum = Just catalogAlbumTitle
        , sqYear = year
        , sqQuery = Nothing
        , sqCategories = [3000, 3010, 3020]  -- Audio, MP3, FLAC
        , sqLimit = 100
        , sqOffset = 0
        }

  searchAllIndexers client indexerConfig query

-- | Rank search results by quality score
-- Higher score = better
rankResults :: [ReleaseInfo] -> [ReleaseInfo]
rankResults = rankResultsWithContext Nothing

-- | Rank results with album title context for better matching
rankResultsWithContext :: Maybe Text -> [ReleaseInfo] -> [ReleaseInfo]
rankResultsWithContext maybeAlbumTitle releases =
  sortOn (Down . scoreReleaseWithContext maybeAlbumTitle) releases

-- | Rank results with quality profile consideration
-- This is the quality-aware version that uses the profile to score quality
rankResultsWithProfile :: Maybe Text -> Maybe Quality.QualityProfile -> [ReleaseInfo] -> [ReleaseInfo]
rankResultsWithProfile maybeAlbumTitle maybeProfile releases =
  sortOn (Down . scoreReleaseWithProfile maybeAlbumTitle maybeProfile) releases

-- | Calculate quality score with optional album title context
scoreReleaseWithContext :: Maybe Text -> ReleaseInfo -> Int
scoreReleaseWithContext maybeAlbumTitle ReleaseInfo{..} =
  formatScoreTotal + seedScore + sizeScore + grabScore + titleMatchScore
  where
    -- Format score (prefer lossless)
    formatScore = case riDownloadType of
      NZB -> 500  -- NZBs generally more reliable
      Torrent -> case riSeeders of
        Just s | s > (10 :: Int) -> 400  -- Well-seeded torrents
        Just s | s > 0  -> 200  -- Some seeders
        _               -> 50   -- No seeder info or dead
      Slskd -> case riSeeders of
        Just s | s > 5 -> 350   -- Good slskd availability
        Just s | s > 0 -> 150   -- Some availability
        _              -> 100   -- slskd baseline

    -- Title-based format detection
    titleLower = T.toLower riTitle
    hasFlac = "flac" `T.isInfixOf` titleLower
    hasLossless = hasFlac || "alac" `T.isInfixOf` titleLower || "ape" `T.isInfixOf` titleLower
    has320 = "320" `T.isInfixOf` titleLower
    hasV0 = " v0" `T.isInfixOf` titleLower || "-v0" `T.isInfixOf` titleLower

    -- Prefer: FLAC > 320kbps > V0 > others
    qualityBonus
      | hasLossless = 1000
      | has320      = 600
      | hasV0       = 400
      | otherwise   = 0

    -- Seeder score (for torrents)
    seedScore = case riSeeders of
      Just s -> min 500 (s * 10)  -- Cap at 500
      Nothing -> 0

    -- Size score (prefer reasonable sizes, penalize too small or too large)
    sizeScore = case riSize of
      Just size ->
        let mb = fromIntegral size / (1024 * 1024 :: Double)
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

    -- Title matching score (very important!)
    titleMatchScore = case maybeAlbumTitle of
      Nothing -> 0
      Just searchTitle ->
        let titleLowerClean = T.toLower $ T.replace "." " " $ T.replace "-" " " riTitle
            searchLowerClean = T.toLower $ T.replace "." " " $ T.replace "-" " " searchTitle
            -- Remove common words that might differ
            cleanWord w = T.filter (\c -> c /= '(' && c /= ')' && c /= '[' && c /= ']') w
            titleWords = map cleanWord $ T.words titleLowerClean
            searchWords = map cleanWord $ T.words searchLowerClean

            -- Check if title contains the search term
            hasExactMatch = searchLowerClean `T.isInfixOf` titleLowerClean

            -- Check if there are extra numbers (like "3" in "blueprint 3")
            hasExtraNumbers = any (\w -> T.all (\c -> c >= '0' && c <= '9') w && T.length w <= 2 && w `notElem` searchWords) titleWords

            -- Penalize if there are extra album indicators (2, 3, etc)
            extraNumberPenalty = if hasExtraNumbers then -2000 else 0

            -- Reward exact matches heavily
            exactMatchBonus = if hasExactMatch then 3000 else 0
        in exactMatchBonus + extraNumberPenalty

    formatScoreTotal = formatScore + qualityBonus

-- | Calculate quality score with quality profile support
scoreReleaseWithProfile :: Maybe Text -> Maybe Quality.QualityProfile -> ReleaseInfo -> Int
scoreReleaseWithProfile maybeAlbumTitle maybeProfile ReleaseInfo{..} =
  formatScoreTotal + seedScore + sizeScore + grabScore + titleMatchScore + profileQualityScore
  where
    -- Format score (NZB vs Torrent vs Slskd)
    formatScore = case riDownloadType of
      NZB -> 500  -- NZBs generally more reliable
      Torrent -> case riSeeders of
        Just s | s > (10 :: Int) -> 400  -- Well-seeded torrents
        Just s | s > 0  -> 200  -- Some seeders
        _               -> 50   -- No seeder info or dead
      Slskd -> case riSeeders of
        Just s | s > 5 -> 350   -- Good slskd availability
        Just s | s > 0 -> 150   -- Some availability
        _              -> 100   -- slskd baseline

    -- Quality score based on profile (if provided)
    profileQualityScore = case maybeProfile of
      Nothing -> 0  -- No profile, no quality scoring
      Just profile ->
        let rank = Quality.getQualityRank riQuality profile
            -- Scale rank to points (0-2000 range)
            -- Higher rank = better quality = more points
            qualityPoints = rank * 100

            -- Check if quality meets profile requirements
            meetsProfile = Quality.meetsProfile riQuality profile

            -- Bonus for meeting profile
            profileBonus = if meetsProfile then 500 else 0

            -- Check if quality meets or exceeds cutoff
            cutoffQuality = Quality.qfCutoffQuality profile
            meetsCutoff = Quality.isBetterQuality profile riQuality cutoffQuality
                       || riQuality == cutoffQuality

            -- Big bonus for meeting cutoff (this is what we want!)
            cutoffBonus = if meetsCutoff then 1500 else 0
        in qualityPoints + profileBonus + cutoffBonus

    -- Seeder score (for torrents)
    seedScore = case riSeeders of
      Just s -> min 500 (s * 10)  -- Cap at 500
      Nothing -> 0

    -- Size score (prefer reasonable sizes, penalize too small or too large)
    sizeScore = case riSize of
      Just size ->
        let mb = fromIntegral size / (1024 * 1024 :: Double)
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

    -- Title matching score (very important!)
    titleMatchScore = case maybeAlbumTitle of
      Nothing -> 0
      Just searchTitle ->
        let titleLowerClean = T.toLower $ T.replace "." " " $ T.replace "-" " " riTitle
            searchLowerClean = T.toLower $ T.replace "." " " $ T.replace "-" " " searchTitle
            -- Remove common words that might differ
            cleanWord w = T.filter (\c -> c /= '(' && c /= ')' && c /= '[' && c /= ']') w
            titleWords = map cleanWord $ T.words titleLowerClean
            searchWords = map cleanWord $ T.words searchLowerClean

            -- Check if title contains the search term
            hasExactMatch = searchLowerClean `T.isInfixOf` titleLowerClean

            -- Check if there are extra numbers (like "3" in "blueprint 3")
            hasExtraNumbers = any (\w -> T.all (\c -> c >= '0' && c <= '9') w && T.length w <= 2 && w `notElem` searchWords) titleWords

            -- Penalize if there are extra album indicators (2, 3, etc)
            extraNumberPenalty = if hasExtraNumbers then -2000 else 0

            -- Reward exact matches heavily
            exactMatchBonus = if hasExactMatch then 3000 else 0
        in exactMatchBonus + extraNumberPenalty

    formatScoreTotal = formatScore
