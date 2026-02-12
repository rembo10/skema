{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Pure business logic for catalog album states and download decisions.
--
-- This module contains pure functions that determine:
-- - Whether an album is "wanted" (should be monitored/downloaded)
-- - Whether a download should be initiated, cancelled, or upgraded
-- - Album state derivation for UI purposes
--
-- All logic is centralized here rather than scattered across handlers.
module Skema.Core.Catalog
  ( -- * Types
    AlbumState(..)
  , DownloadDecision(..)
  , AlbumContext(..)
  , DownloadContext(..)
    -- * Album State
  , isAlbumWanted
  , computeAlbumState
  , albumStateToText
    -- * Download Decisions
  , shouldInitiateDownload
  , shouldCancelDownload
  , shouldUpgradeDownload
  , makeDownloadDecision
    -- * Quality Profile Reactions
  , qualityProfileChanged
  , artistQualityProfileChanged
  ) where

import Skema.Domain.Quality
  ( Quality
  , QualityProfile
  , meetsProfile
  , isBetterQuality
  , needsUpgrade
  )
import Data.Aeson (ToJSON(..), FromJSON(..))

-- ============================================================================
-- TYPES
-- ============================================================================

-- | Album state for UI presentation.
-- This is a **derived** state, not stored in the database.
data AlbumState
  = NotWanted              -- ^ No quality profile assigned
  | Wanted                 -- ^ Has profile, not in library, no download
  | Searching              -- ^ Has profile, not in library, download queued
  | Downloading            -- ^ Has profile, not in library, download in progress
  | Failed                 -- ^ Has profile, download failed
  | IdentificationFailed   -- ^ Has profile, download succeeded but couldn't identify
  | InLibrary              -- ^ No profile (or satisfied), exists in library
  | Monitored              -- ^ Has profile, in library, monitoring for upgrades
  | Upgrading              -- ^ Has profile, in library, upgrade download active
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON AlbumState
instance FromJSON AlbumState

-- | Convert AlbumState to Text representation.
albumStateToText :: AlbumState -> Text
albumStateToText NotWanted = "not_wanted"
albumStateToText Wanted = "wanted"
albumStateToText Searching = "searching"
albumStateToText Downloading = "downloading"
albumStateToText Failed = "failed"
albumStateToText IdentificationFailed = "identification_failed"
albumStateToText InLibrary = "in_library"
albumStateToText Monitored = "monitored"
albumStateToText Upgrading = "upgrading"

-- | Decision about what action to take for downloads.
data DownloadDecision
  = NoAction                   -- ^ No change needed
  | InitiateDownload           -- ^ Start a new download
  | CancelDownload Text        -- ^ Cancel existing download (with reason)
  | AllowContinue              -- ^ Download should continue
  deriving (Show, Eq)

-- | Context needed to determine album wanted status and state.
data AlbumContext = AlbumContext
  { acQualityProfile :: Maybe QualityProfile
    -- ^ Quality profile assigned to this album (if any)
  , acCurrentQuality :: Maybe Quality
    -- ^ Current quality of album in library (if in library)
  , acInLibrary :: Bool
    -- ^ Whether the album exists in the library (has matched cluster)
  , acActiveDownloadStatus :: Maybe Text
    -- ^ Status of active download (if any): "queued", "downloading", "failed", etc.
  } deriving (Show, Eq)

-- | Context needed to make download decisions.
data DownloadContext = DownloadContext
  { dcAlbumContext :: AlbumContext
    -- ^ Album state context
  , dcDownloadQuality :: Quality
    -- ^ Quality of the download in question
  , dcNewProfile :: Maybe QualityProfile
    -- ^ New quality profile (if profile was changed)
  } deriving (Show, Eq)

-- ============================================================================
-- ALBUM WANTED STATUS
-- ============================================================================

-- | Determine if an album is "wanted" based purely on its profile and current state.
--
-- An album is wanted if:
-- 1. It has a quality profile assigned, AND
-- 2. Either:
--    a. It's not in the library yet, OR
--    b. It's in the library but current quality doesn't meet the cutoff
--
-- This is the **single source of truth** for "wanted" status.
-- The database should not store this - it should be computed on demand.
isAlbumWanted :: AlbumContext -> Bool
isAlbumWanted AlbumContext{..} =
  case acQualityProfile of
    Nothing -> False  -- No profile = not monitoring = not wanted
    Just profile ->
      if not acInLibrary
        then True  -- Has profile but not in library = wanted
        else
          -- In library: wanted if current quality needs upgrade
          case acCurrentQuality of
            Nothing -> True  -- Unknown quality, assume needs acquisition
            Just currentQual -> needsUpgrade currentQual profile

-- | Compute the album state for UI display.
--
-- This is a pure function that derives state from context.
-- The state is NOT stored in the database.
computeAlbumState :: AlbumContext -> AlbumState
computeAlbumState ctx@AlbumContext{..} =
  let wanted = isAlbumWanted ctx
  in case (wanted, acInLibrary, acActiveDownloadStatus) of
       -- Not wanted cases
       (False, False, _) -> NotWanted
       (False, True, _) -> InLibrary

       -- Wanted, not in library
       (True, False, Nothing) -> Wanted
       (True, False, Just status)
         | status == "queued" -> Searching
         | status == "downloading" -> Downloading
         | status == "processing" -> Downloading
         | status == "failed" -> Failed
         | status == "identification_failed" -> IdentificationFailed
         | otherwise -> Wanted

       -- Wanted, in library (monitoring for upgrades)
       (True, True, Nothing) -> Monitored
       (True, True, Just _) -> Upgrading

-- ============================================================================
-- DOWNLOAD DECISIONS
-- ============================================================================

-- | Determine if a new download should be initiated.
--
-- A download should be initiated if:
-- 1. Album is wanted
-- 2. Album is not already being downloaded
-- 3. Download quality meets the profile requirements
-- 4. Download quality is better than current quality (if in library)
shouldInitiateDownload :: AlbumContext -> Quality -> Bool
shouldInitiateDownload ctx@AlbumContext{..} downloadQuality =
  let wanted = isAlbumWanted ctx
      noActiveDownload = isNothing acActiveDownloadStatus
      meetsRequirements = case acQualityProfile of
        Nothing -> False
        Just profile -> meetsProfile downloadQuality profile
      isBetter = case (acQualityProfile, acCurrentQuality) of
        (Just profile, Just currentQual) -> isBetterQuality profile downloadQuality currentQual
        (Just _, Nothing) -> True  -- No current quality, any quality is better
        _ -> False
  in wanted && noActiveDownload && meetsRequirements && isBetter

-- | Determine if an existing download should be cancelled.
--
-- A download should be cancelled if:
-- 1. Album is no longer wanted (profile removed), OR
-- 2. Download quality no longer meets profile requirements (profile changed), OR
-- 3. Album is now in library with quality that meets/exceeds cutoff
shouldCancelDownload :: DownloadContext -> Maybe Text
shouldCancelDownload DownloadContext{..} =
  let ctx = dcAlbumContext
      wanted = isAlbumWanted ctx
      profile = acQualityProfile ctx
      newProfile = dcNewProfile
  in case () of
       -- Album no longer wanted (profile removed)
       _ | not wanted && isNothing profile ->
           Just "Album no longer wanted (quality profile removed)"

       -- Quality profile changed and download no longer meets requirements
       _ | isJust newProfile ->
           case newProfile of
             Just prof | not (meetsProfile dcDownloadQuality prof) ->
               Just "Download quality no longer meets quality profile requirements"
             _ -> Nothing

       -- Album now in library with sufficient quality
       _ | acInLibrary ctx && not wanted ->
           Just "Album now in library with sufficient quality"

       -- Download should continue
       _ -> Nothing

-- | Determine if an upgrade download should be initiated.
--
-- An upgrade download should be initiated if:
-- 1. Album is in library
-- 2. Current quality is below cutoff
-- 3. Upgrade quality meets profile and is better than current
-- 4. No active download already running
shouldUpgradeDownload :: AlbumContext -> Quality -> Bool
shouldUpgradeDownload ctx@AlbumContext{..} upgradeQuality =
  let wanted = isAlbumWanted ctx
      noActiveDownload = isNothing acActiveDownloadStatus
      meetsRequirements = case acQualityProfile of
        Nothing -> False
        Just profile -> meetsProfile upgradeQuality profile
      isBetter = case (acQualityProfile, acCurrentQuality) of
        (Just profile, Just currentQual) -> isBetterQuality profile upgradeQuality currentQual
        _ -> False
  in acInLibrary && wanted && noActiveDownload && meetsRequirements && isBetter

-- | Make a complete download decision based on context.
--
-- This is the main decision function that handlers should call.
makeDownloadDecision :: DownloadContext -> DownloadDecision
makeDownloadDecision ctx@DownloadContext{..} =
  case shouldCancelDownload ctx of
    Just reason -> CancelDownload reason
    Nothing ->
      if shouldInitiateDownload dcAlbumContext dcDownloadQuality
        then InitiateDownload
        else if shouldUpgradeDownload dcAlbumContext dcDownloadQuality
          then InitiateDownload
          else AllowContinue

-- ============================================================================
-- QUALITY PROFILE CHANGE REACTIONS
-- ============================================================================

-- | Determine what actions to take when an album's quality profile changes.
--
-- Returns (newWantedStatus, shouldCancelActiveDownload, shouldSearchForDownload)
qualityProfileChanged
  :: Maybe Int64               -- ^ Matched cluster ID (if in library)
  -> Maybe Quality             -- ^ Current quality (if in library)
  -> Maybe Int64               -- ^ Active download ID (if any)
  -> Maybe Quality             -- ^ Active download quality (if any)
  -> Maybe QualityProfile      -- ^ Old quality profile
  -> Maybe QualityProfile      -- ^ New quality profile
  -> (Bool, Bool, Bool)        -- ^ (isWanted, shouldCancelDownload, shouldSearch)
qualityProfileChanged maybeClusterId maybeCurrentQuality maybeActiveDownloadId maybeActiveDownloadQuality oldProfile newProfile =
  let
    ctx = AlbumContext
      { acQualityProfile = newProfile
      , acCurrentQuality = maybeCurrentQuality
      , acInLibrary = isJust maybeClusterId
      , acActiveDownloadStatus = if isJust maybeActiveDownloadId then Just "downloading" else Nothing
      }

    wanted = isAlbumWanted ctx

    -- Should cancel active download if it no longer meets new profile
    shouldCancel = case (maybeActiveDownloadId, maybeActiveDownloadQuality, newProfile) of
      (Just _, Just downloadQual, Just prof) -> not (meetsProfile downloadQual prof)
      (Just _, _, Nothing) -> True  -- Profile removed, cancel download
      _ -> False

    -- Should search for new download if:
    -- - Now wanted but wasn't before (profile added)
    -- - Still wanted and no active download (might find better quality)
    shouldSearch = case (oldProfile, newProfile) of
      (Nothing, Just _) -> wanted && isNothing maybeActiveDownloadId
      (Just _, Just _) -> wanted && isNothing maybeActiveDownloadId && not shouldCancel
      _ -> False

  in (wanted, shouldCancel, shouldSearch)

-- | Determine what actions to take when an artist's quality profile changes.
--
-- This affects all albums by that artist that don't have their own profile.
-- Returns same triple as qualityProfileChanged.
artistQualityProfileChanged
  :: Maybe Int64               -- ^ Album's own quality profile (overrides artist)
  -> Maybe Int64               -- ^ Matched cluster ID (if in library)
  -> Maybe Quality             -- ^ Current quality (if in library)
  -> Maybe Int64               -- ^ Active download ID (if any)
  -> Maybe Quality             -- ^ Active download quality (if any)
  -> Maybe QualityProfile      -- ^ New artist quality profile
  -> (Bool, Bool, Bool)        -- ^ (isWanted, shouldCancelDownload, shouldSearch)
artistQualityProfileChanged albumProfileId maybeClusterId maybeCurrentQuality maybeActiveDownloadId maybeActiveDownloadQuality artistProfile =
  -- If album has its own profile, artist profile change doesn't affect it
  if isJust albumProfileId
    then (False, False, False)  -- No change needed
    else qualityProfileChanged maybeClusterId maybeCurrentQuality maybeActiveDownloadId maybeActiveDownloadQuality Nothing artistProfile
