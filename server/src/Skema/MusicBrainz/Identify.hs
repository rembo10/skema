{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | MusicBrainz identification workflow (IO orchestration).
--
-- This module coordinates IO operations (HTTP API calls) with pure domain logic
-- to identify audio files using MusicBrainz metadata.
--
-- All pure business logic has been extracted to Skema.Domain.Identification.
-- This module only handles IO orchestration.
module Skema.MusicBrainz.Identify
  ( -- * Identification
    identifyFileGroups
  , identifyFileGroup
    -- * Re-exports
  , module Skema.Domain.Identification
  ) where

import Skema.MusicBrainz.Types
import Skema.MusicBrainz.Client
import Skema.MusicBrainz.Utils
import Skema.Domain.Identification
import Katip

-- | Identify multiple file groups (IO orchestration).
--
-- Coordinates HTTP calls to MusicBrainz with pure domain logic.
-- This is the main entry point for batch identification.
identifyFileGroups :: LogEnv
                   -> MBClientEnv
                   -> IdentifyConfig
                   -> [FileGroup]
                   -> IO [(FileGroup, Either MBClientError IdentificationResult)]
identifyFileGroups le mbEnv config groups = do
  let initialContext = ()
      initialNamespace = "musicbrainz.identify"

  runKatipContextT le initialContext initialNamespace $ do
    $(logTM) InfoS $ logStr $ ("Identifying " <> show (length groups) <> " album groups" :: Text)

    liftIO $ forM (zip [1..] groups) $ \(idx :: Int, fg) -> do
      let albumName = fromMaybe "<no album>" (fgAlbum fg)
          artistName = fromMaybe "<no artist>" (fgArtist fg)
          trackCount = length (fgFiles fg)

      runKatipContextT le initialContext initialNamespace $ do
        $(logTM) InfoS $ logStr $ ("[" <> show idx <> "/" <> show (length groups) <> "] Looking up: " <>
          albumName <> " by " <> artistName <> " (" <> show trackCount <> " tracks)" :: Text)

      result <- identifyFileGroup le mbEnv config fg

      runKatipContextT le initialContext initialNamespace $ do
        case result of
          Left err ->
            $(logTM) ErrorS $ logStr $ ("[" <> show idx <> "] API error for '" <> albumName <> "': " <> prettyClientError err :: Text)
          Right (IdentificationResult Nothing candidates) ->
            $(logTM) WarningS $ logStr $ ("[" <> show idx <> "] No match found for '" <> albumName <> "' (" <> show (length candidates) <> " candidates below threshold)" :: Text)
          Right (IdentificationResult (Just match) _) ->
            $(logTM) InfoS $ logStr $ ("[" <> show idx <> "] Matched '" <> albumName <> "' to '" <>
              mbReleaseTitle (rmRelease match) <> "' (confidence: " <>
              show (round (rmConfidence match * 100) :: Int) <> "%)" :: Text)

      pure (fg, result)

-- | Identify a single file group (IO orchestration).
--
-- Orchestrates HTTP calls with pure domain logic:
-- 1. Checks if the file group already has a MusicBrainz release ID
-- 2. If yes, fetches that specific release (HTTP IO)
-- 3. If no, searches MusicBrainz for candidates (HTTP IO)
-- 4. Uses pure domain logic to match and select best candidate
-- 5. Returns the match if confidence is above threshold
identifyFileGroup :: LogEnv
                  -> MBClientEnv
                  -> IdentifyConfig
                  -> FileGroup
                  -> IO (Either MBClientError IdentificationResult)
identifyFileGroup le mbEnv config fg = do
  -- Skip if there's no searchable metadata at all (no album, no artist, no release ID)
  if isNothing (fgAlbum fg) && isNothing (fgArtist fg) && isNothing (fgReleaseId fg)
    then do
      runKatipContextT le () "musicbrainz.identify" $
        $(logTM) InfoS "Skipping identification: no album, artist, or release ID"
      pure $ Right $ IdentificationResult Nothing []
    else
      -- First, try direct lookup if we have a release ID in tags
      case fgReleaseId fg of
        Just releaseId -> identifyByReleaseId le mbEnv config fg releaseId
        Nothing -> identifyBySearch le mbEnv config fg

-- | Apply join phrase normalization to a release if configured
normalizeRelease :: IdentifyConfig -> MBRelease -> MBRelease
normalizeRelease config release =
  if cfgNormalizeFeaturing config
    then normalizeReleaseJoinPhrases (cfgNormalizeFeaturingTo config) release
    else release

-- | Identify by existing MusicBrainz release ID in tags (IO orchestration).
--
-- Fetches the specific release AND searches for alternatives so users can
-- easily switch if the tagged release isn't the one they want.
identifyByReleaseId :: LogEnv
                    -> MBClientEnv
                    -> IdentifyConfig
                    -> FileGroup
                    -> ReleaseMBID
                    -> IO (Either MBClientError IdentificationResult)
identifyByReleaseId le mbEnv config fg releaseId = do
  -- IO: Fetch the specific release from MusicBrainz
  result <- getRelease mbEnv releaseId
  case result of
    Left err -> do
      runKatipContextT le () "musicbrainz.identify" $
        $(logTM) WarningS $ logStr $ ("Release lookup failed for " <> unMBID releaseId <> ": " <> prettyClientError err <> "; falling back to search" :: Text)
      identifyBySearch le mbEnv config fg
    Right release -> do
      -- Also search for alternative releases so users have options
      -- Build search query and fetch alternatives
      let query = buildSearchQuery fg
      searchResult <- searchReleases mbEnv query (Just $ cfgSearchLimit config) Nothing False

      case searchResult of
        Left _searchErr -> do
          -- If search fails, still return the direct lookup result
          let normalizedRelease = normalizeRelease config release
          let match = computeReleaseMatch fg [normalizedRelease] (normalizedRelease, 0.0)
          let bestMatch = selectBestMatch (cfgMinConfidence config) [match]
          pure $ Right $ IdentificationResult bestMatch [(normalizedRelease, rmConfidence match)]

        Right mbSearchResult -> do
          -- Got search results - fetch details for alternatives
          let searchCandidates = map (normalizeRelease config) $ mbSearchReleases mbSearchResult

          if null searchCandidates
            then do
              -- No search results, use direct lookup only
              let normalizedRelease = normalizeRelease config release
              let match = computeReleaseMatch fg [normalizedRelease] (normalizedRelease, 0.0)
              let bestMatch = selectBestMatch (cfgMinConfidence config) [match]
              pure $ Right $ IdentificationResult bestMatch [(normalizedRelease, rmConfidence match)]
            else do
              -- Fetch full details for top candidates (including the tagged release)
              candidateDetails <- fetchCandidateDetails le mbEnv config searchCandidates (cfgMaxCandidates config)

              -- Make sure the tagged release is in the candidate list
              let normalizedRelease = normalizeRelease config release
              let allCandidates = if normalizedRelease `elem` candidateDetails
                                  then candidateDetails
                                  else normalizedRelease : take (cfgMaxCandidates config - 1) candidateDetails

              -- Rank and match all candidates
              let rankedCandidates = rankReleaseCandidates (length allCandidates) fg allCandidates
              let detailedMatches = map (computeReleaseMatch fg allCandidates) rankedCandidates

              -- Select best match and extract candidates with scores
              let bestMatch = selectBestMatch (cfgMinConfidence config) detailedMatches
              let candidatesWithScores = map (\m -> (rmRelease m, rmConfidence m)) detailedMatches
              pure $ Right $ IdentificationResult bestMatch candidatesWithScores

-- | Identify by searching MusicBrainz (IO orchestration).
--
-- Searches and fetches releases via HTTP, then uses pure domain logic for matching.
identifyBySearch :: LogEnv
                 -> MBClientEnv
                 -> IdentifyConfig
                 -> FileGroup
                 -> IO (Either MBClientError IdentificationResult)
identifyBySearch le mbEnv config fg = do
  -- Pure: Build search query from file group metadata
  let query = buildSearchQuery fg

  -- IO: Search MusicBrainz for candidates (structured query, no dismax)
  result <- searchReleases mbEnv query (Just $ cfgSearchLimit config) Nothing False
  case result of
    Left err -> pure $ Left err
    Right searchResult -> do
      -- Pure: Apply normalization to search results
      let candidates = map (normalizeRelease config) $ mbSearchReleases searchResult

      -- If no candidates, return empty result
      if null candidates
        then pure $ Right $ IdentificationResult Nothing []
        else do
          -- IO: Fetch full details for candidates (including tracks)
          -- This is the expensive part (MusicBrainz API calls), so we limit to maxCandidates
          -- We then run Hungarian algorithm on ALL fetched candidates to pick the best
          candidateDetails <- fetchCandidateDetails le mbEnv config candidates (cfgMaxCandidates config)

          -- Check if we successfully fetched any candidate details
          if null candidateDetails
            then do
              runKatipContextT le () "musicbrainz.identify" $
                $(logTM) WarningS $ logStr $ ("All " <> show (length candidates) <> " candidate fetches failed - no releases to score" :: Text)
              pure $ Right $ IdentificationResult Nothing []
            else do
              -- Log what we fetched vs what we have
              let userFileCount = length (fgFiles fg)
              runKatipContextT le () "musicbrainz.identify" $ do
                $(logTM) InfoS $ logStr $ ("Fetched " <> show (length candidateDetails) <> " releases, user has " <> show userFileCount <> " files" :: Text)
                forM_ candidateDetails $ \release -> do
                  let trackCount = length (mbReleaseTracks release)
                      title = mbReleaseTitle release
                      passesFilter = trackCount >= userFileCount
                  $(logTM) InfoS $ logStr $ ("  \"" <> title <> "\": " <> show trackCount <> " tracks" <>
                                             (if passesFilter then " (passes)" else " (FILTERED - needs >= " <> show userFileCount <> ")") :: Text)

              -- Pure: Rank candidates by metadata only (no limit - we'll use Hungarian to pick best)
              -- We already fetched full details for all candidates, so run Hungarian on all of them
              let rankedCandidates = rankReleaseCandidates (length candidateDetails) fg candidateDetails

              -- Pure: Compute detailed matches for ALL fetched candidates
              -- This runs Hungarian algorithm on each to do track-by-track matching
              let detailedMatches = map (computeReleaseMatch fg candidateDetails) rankedCandidates

              -- Log confidence scores for all candidates (helps diagnose matching issues)
              runKatipContextT le () "musicbrainz.identify" $ do
                $(logTM) InfoS $ logStr $ ("Scoring " <> show (length detailedMatches) <> " candidates after filtering:" :: Text)
                forM_ (zip [(1::Int)..] detailedMatches) $ \(idx, match) -> do
                  let confidence = rmConfidence match
                      release = rmRelease match
                      title = mbReleaseTitle release
                      trackCount = length (mbReleaseTracks release)
                  $(logTM) InfoS $ logStr $ ("  " <> show idx <> ". \"" <> title <> "\" (" <> show trackCount <> " tracks) - confidence: " <> show (round (confidence * 100) :: Int) <> "%" :: Text)

              -- Pure: Select best match if it meets threshold
              let bestMatch = selectBestMatch (cfgMinConfidence config) detailedMatches
              -- Extract candidates with their confidence scores
              let candidatesWithScores = map (\m -> (rmRelease m, rmConfidence m)) detailedMatches
              -- Return result with match (if any) AND all candidates with scores
              pure $ Right $ IdentificationResult bestMatch candidatesWithScores

-- | Fetch full details for release candidates (IO operation).
fetchCandidateDetails :: LogEnv -> MBClientEnv -> IdentifyConfig -> [MBRelease] -> Int -> IO [MBRelease]
fetchCandidateDetails le mbEnv config candidates maxCandidates = do
  let releaseIds = take maxCandidates $ map mbReleaseId candidates
  results <- traverse (\releaseId -> getRelease mbEnv releaseId) releaseIds
  let successful = rights results
      failed = lefts results

  -- Log each failure with details
  unless (null failed) $
    runKatipContextT le () "musicbrainz.identify" $ do
      $(logTM) ErrorS $ logStr $ ("Failed to fetch details for " <> show (length failed) <> " out of " <> show (length releaseIds) <> " candidates" :: Text)
      forM_ (zip releaseIds failed) $ \(releaseId, err) -> do
        $(logTM) ErrorS $ logStr $ ("  Release " <> unMBID releaseId <> ": " <> prettyClientError err :: Text)

  when (length successful < length releaseIds && not (null failed)) $
    runKatipContextT le () "musicbrainz.identify" $
      $(logTM) WarningS $ logStr $ ("Only fetched " <> show (length successful) <> " out of " <> show (length releaseIds) <> " candidates successfully" :: Text)

  pure $ map (normalizeRelease config) successful
