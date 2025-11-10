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
                   -> IO [(FileGroup, Either MBClientError (Maybe ReleaseMatch))]
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
          Right Nothing ->
            $(logTM) WarningS $ logStr $ ("[" <> show idx <> "] No match found for '" <> albumName <> "'" :: Text)
          Right (Just match) ->
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
                  -> IO (Either MBClientError (Maybe ReleaseMatch))
identifyFileGroup le mbEnv config fg = do
  -- First, try direct lookup if we have a release ID in tags
  case fgReleaseId fg of
    Just releaseId -> identifyByReleaseId mbEnv config fg releaseId
    Nothing -> identifyBySearch le mbEnv config fg

-- | Apply join phrase normalization to a release if configured
normalizeRelease :: IdentifyConfig -> MBRelease -> MBRelease
normalizeRelease config release =
  if cfgNormalizeFeaturing config
    then normalizeReleaseJoinPhrases (cfgNormalizeFeaturingTo config) release
    else release

-- | Identify by existing MusicBrainz release ID in tags (IO orchestration).
--
-- Fetches release via HTTP, then uses pure domain logic for matching.
identifyByReleaseId :: MBClientEnv
                    -> IdentifyConfig
                    -> FileGroup
                    -> ReleaseMBID
                    -> IO (Either MBClientError (Maybe ReleaseMatch))
identifyByReleaseId mbEnv config fg releaseId = do
  -- IO: Fetch release from MusicBrainz
  result <- getRelease mbEnv releaseId
  case result of
    Left err -> pure $ Left err
    Right release -> do
      -- Pure: Apply normalization
      let normalizedRelease = normalizeRelease config release

      -- Pure: Compute match using domain logic
      let match = computeReleaseMatch fg [] (normalizedRelease, 0.0)

      -- Pure: Select match if it meets threshold
      pure $ Right $ selectBestMatch (cfgMinConfidence config) [match]

-- | Identify by searching MusicBrainz (IO orchestration).
--
-- Searches and fetches releases via HTTP, then uses pure domain logic for matching.
identifyBySearch :: LogEnv
                 -> MBClientEnv
                 -> IdentifyConfig
                 -> FileGroup
                 -> IO (Either MBClientError (Maybe ReleaseMatch))
identifyBySearch le mbEnv config fg = do
  -- Pure: Build search query from file group metadata
  let query = buildSearchQuery fg

  -- IO: Search MusicBrainz for candidates
  result <- searchReleases mbEnv query (Just $ cfgSearchLimit config) Nothing
  case result of
    Left err -> pure $ Left err
    Right searchResult -> do
      -- Pure: Apply normalization to search results
      let candidates = map (normalizeRelease config) $ mbSearchReleases searchResult

      -- If no candidates, return Nothing
      if null candidates
        then pure $ Right Nothing
        else do
          -- IO: Fetch full details for candidates (including tracks)
          -- This is the expensive part (MusicBrainz API calls), so we limit to maxCandidates
          -- We then run Hungarian algorithm on ALL fetched candidates to pick the best
          candidateDetails <- fetchCandidateDetails le mbEnv config candidates (cfgMaxCandidates config)

          -- Check if we successfully fetched any candidate details
          if null candidateDetails
            then pure $ Right Nothing
            else do
              -- Pure: Rank candidates by metadata only (no limit - we'll use Hungarian to pick best)
              -- We already fetched full details for all candidates, so run Hungarian on all of them
              let rankedCandidates = rankReleaseCandidates (length candidateDetails) fg candidateDetails

              -- Pure: Compute detailed matches for ALL fetched candidates
              -- This runs Hungarian algorithm on each to do track-by-track matching
              let detailedMatches = map (computeReleaseMatch fg candidateDetails) rankedCandidates

              -- Log confidence scores when we have multiple candidates (helps diagnose close matches)
              when (length detailedMatches > 1) $
                runKatipContextT le () "musicbrainz.identify" $ do
                  $(logTM) DebugS $ logStr $ ("Evaluating " <> show (length detailedMatches) <> " candidates:" :: Text)
                  forM_ (zip [(1::Int)..] detailedMatches) $ \(idx, match) -> do
                    let confidence = rmConfidence match
                        release = rmRelease match
                        title = mbReleaseTitle release
                    $(logTM) DebugS $ logStr $ ("  " <> show idx <> ". \"" <> title <> "\" - confidence: " <> show (round (confidence * 100) :: Int) <> "%" :: Text)

              -- Pure: Select best match if it meets threshold
              pure $ Right $ selectBestMatch (cfgMinConfidence config) detailedMatches

-- | Fetch full details for release candidates (IO operation).
fetchCandidateDetails :: LogEnv -> MBClientEnv -> IdentifyConfig -> [MBRelease] -> Int -> IO [MBRelease]
fetchCandidateDetails le mbEnv config candidates maxCandidates = do
  let releaseIds = take maxCandidates $ map mbReleaseId candidates
  results <- traverse (\releaseId -> getRelease mbEnv releaseId) releaseIds
  let successful = rights results
  when (length successful < length releaseIds) $
    runKatipContextT le () "musicbrainz.identify" $
      $(logTM) WarningS $ logStr $ ("Only fetched " <> show (length successful) <> " out of " <> show (length releaseIds) <> " candidates" :: Text)
  pure $ map (normalizeRelease config) successful
