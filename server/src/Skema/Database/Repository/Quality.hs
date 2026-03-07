{-# LANGUAGE OverloadedStrings #-}

-- | Quality profile repository operations.
module Skema.Database.Repository.Quality
  ( insertQualityProfile
  , updateQualityProfile
  , deleteQualityProfile
  , getQualityProfile
  , getAllQualityProfiles
  , getEffectiveQualityProfile
  , updateAlbumQuality
  , qualityProfileRecordToDomain
  , resolveQualityProfileId
  ) where

import Skema.Database.Connection
import Skema.Database.Types
import Skema.Database.Utils (insertReturningId)
import Skema.Domain.Quality (Quality, QualityProfile(..), QualityPreference(..), qualityToText, textToQuality, qualityPreferencesToJSON, qualityPreferencesFromJSON)
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (Only(..))
import qualified Database.SQLite.Simple as SQLite

-- | Convert QualityProfileRecord to QualityProfile domain type.
qualityProfileRecordToDomain :: QualityProfileRecord -> Maybe QualityProfile
qualityProfileRecordToDomain record = do
  cutoffQuality <- textToQuality (qualityProfileCutoffQuality record)
  qualityPrefs <- qualityPreferencesFromJSON (qualityProfileQualityPreferences record)
  pure $ QualityProfile
    { qfId = qualityProfileId record
    , qfName = qualityProfileName record
    , qfQualityPreferences = qualityPrefs
    , qfCutoffQuality = cutoffQuality
    , qfUpgradeAutomatically = qualityProfileUpgradeAutomatically record
    }

-- | Insert a new quality profile.
insertQualityProfile :: SQLite.Connection -> Text -> Quality -> [QualityPreference] -> Bool -> IO Int64
insertQualityProfile conn name cutoffQuality qualityPrefs upgradeAuto = do
  let cutoffText = qualityToText cutoffQuality
      prefsJson = qualityPreferencesToJSON qualityPrefs
  insertReturningId conn
    "INSERT INTO quality_profiles (name, cutoff_quality, quality_preferences, upgrade_automatically) \
    \VALUES (?, ?, ?, ?) RETURNING id"
    (name, cutoffText, prefsJson, upgradeAuto)

-- | Update an existing quality profile.
updateQualityProfile :: SQLite.Connection -> Int64 -> Text -> Quality -> [QualityPreference] -> Bool -> IO ()
updateQualityProfile conn profileId name cutoffQuality qualityPrefs upgradeAuto = do
  let cutoffText = qualityToText cutoffQuality
      prefsJson = qualityPreferencesToJSON qualityPrefs
  now <- getCurrentTime
  executeQuery conn
    "UPDATE quality_profiles SET name = ?, cutoff_quality = ?, quality_preferences = ?, upgrade_automatically = ?, updated_at = ? \
    \WHERE id = ?"
    (name, cutoffText, prefsJson, upgradeAuto, now, profileId)

-- | Delete a quality profile.
deleteQualityProfile :: SQLite.Connection -> Int64 -> IO ()
deleteQualityProfile conn profileId =
  executeQuery conn "DELETE FROM quality_profiles WHERE id = ?" (Only profileId)

-- | Get a quality profile by ID.
getQualityProfile :: SQLite.Connection -> Int64 -> IO (Maybe QualityProfile)
getQualityProfile conn profileId = do
  results <- queryRows conn
    "SELECT id, name, cutoff_quality, quality_preferences, upgrade_automatically, created_at, updated_at \
    \FROM quality_profiles WHERE id = ?"
    (Only profileId) :: IO [QualityProfileRecord]
  case viaNonEmpty head results of
    Just record -> pure $ qualityProfileRecordToDomain record
    Nothing -> pure Nothing

-- | Get all quality profiles.
getAllQualityProfiles :: SQLite.Connection -> IO [QualityProfile]
getAllQualityProfiles conn = do
  results <- queryRows_ conn
    "SELECT id, name, cutoff_quality, quality_preferences, upgrade_automatically, created_at, updated_at \
    \FROM quality_profiles ORDER BY name ASC" :: IO [QualityProfileRecord]
  pure $ mapMaybe qualityProfileRecordToDomain results

-- | Get the effective quality profile for an album.
-- Checks album → artist → global default in that order.
-- Returns Nothing if no profile is set at any level.
getEffectiveQualityProfile :: ConnectionPool -> Int64 -> IO (Maybe QualityProfile)
getEffectiveQualityProfile pool albumId = withConnection pool $ \conn -> do
  -- Get album record to check for profile ID and artist ID
  albumResults <- queryRows conn
    "SELECT quality_profile_id, artist_id FROM catalog_albums WHERE id = ?"
    (Only albumId) :: IO [(Maybe Int64, Maybe Int64)]

  case viaNonEmpty head albumResults of
    Nothing -> pure Nothing  -- Album not found
    Just (albumProfileId, maybeArtistId) -> do
      -- If album has a profile, use it
      case albumProfileId of
        Just profileId -> getQualityProfile conn profileId
        Nothing -> do
          -- Check artist profile
          case maybeArtistId of
            Nothing -> pure Nothing  -- No artist ID, no inherited profile
            Just artistId -> do
              artistResults <- queryRows conn
                "SELECT quality_profile_id FROM catalog_artists WHERE id = ?"
                (Only artistId) :: IO [Only (Maybe Int64)]

              case viaNonEmpty head artistResults of
                Just (Only (Just artistProfileId)) -> getQualityProfile conn artistProfileId
                _ -> do
                  -- Fall back to global default quality profile
                  defaultResults <- queryRows conn
                    "SELECT default_quality_profile_id FROM settings WHERE id = 1"
                    () :: IO [Only (Maybe Int64)]
                  case viaNonEmpty head defaultResults of
                    Just (Only (Just defaultProfileId)) -> getQualityProfile conn defaultProfileId
                    _ -> pure Nothing

-- | Update album quality and status based on new quality.
-- This function determines the album status based on the quality profile.
updateAlbumQuality :: ConnectionPool -> Int64 -> Quality -> IO ()
updateAlbumQuality pool albumId newQuality = withConnection pool $ \conn -> do
  now <- getCurrentTime
  let qualityText = qualityToText newQuality

  -- Get the effective quality profile for this album
  maybeProfile <- getEffectiveQualityProfile pool albumId

  case maybeProfile of
    Nothing -> do
      -- No profile, just update the quality without changing status
      executeQuery conn
        "UPDATE catalog_albums SET current_quality = ?, updated_at = ? WHERE id = ?"
        (qualityText, now, albumId)

    Just _profile -> do
      -- Just update the quality - the wanted status is now derived from
      -- quality_profile_id + current_quality + matched_cluster_id
      executeQuery conn
        "UPDATE catalog_albums SET current_quality = ?, updated_at = ? WHERE id = ?"
        (qualityText, now, albumId)

-- | Resolve quality profile for a new album.
-- Priority: explicit > artist > source > default
resolveQualityProfileId
  :: SQLite.Connection
  -> Maybe Int64    -- ^ Explicitly chosen profile (from search picker)
  -> Maybe Int64    -- ^ Artist ID (to look up artist's profile)
  -> Maybe Int64    -- ^ Source ID (to look up source's profile)
  -> IO (Maybe Int64)
resolveQualityProfileId conn explicitProfileId maybeArtistId maybeSourceId =
  case explicitProfileId of
    Just pid -> pure (Just pid)
    Nothing -> do
      -- Try artist profile
      artistProfile <- case maybeArtistId of
        Nothing -> pure Nothing
        Just artistId -> do
          results <- queryRows conn
            "SELECT quality_profile_id FROM catalog_artists WHERE id = ?"
            (Only artistId) :: IO [Only (Maybe Int64)]
          pure $ case viaNonEmpty head results of
            Just (Only pid) -> pid
            _ -> Nothing
      case artistProfile of
        Just pid -> pure (Just pid)
        Nothing -> do
          -- Try source profile
          sourceProfile <- case maybeSourceId of
            Nothing -> pure Nothing
            Just sid -> do
              results <- queryRows conn
                "SELECT quality_profile_id FROM acquisition_rules WHERE id = ?"
                (Only sid) :: IO [Only (Maybe Int64)]
              pure $ case viaNonEmpty head results of
                Just (Only pid) -> pid
                _ -> Nothing
          case sourceProfile of
            Just pid -> pure (Just pid)
            Nothing -> do
              -- Fall back to global default
              results <- queryRows conn
                "SELECT default_quality_profile_id FROM settings WHERE id = 1"
                () :: IO [Only (Maybe Int64)]
              pure $ case viaNonEmpty head results of
                Just (Only pid) -> pid
                _ -> Nothing
