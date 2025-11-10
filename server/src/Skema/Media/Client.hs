{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Media fetching client with waterfall fallback orchestration.
--
-- This module provides the main API for fetching artist images and album covers
-- from multiple sources with automatic fallback.
module Skema.Media.Client
  ( -- * Client functions
    fetchArtistImage
  , fetchAlbumCover
  , fetchArtistImageByName
  , fetchAlbumCoverByNames
    -- * Configuration
  , MediaConfig(..)
  , defaultMediaConfig
  ) where

import Skema.Media.Types
import Skema.MusicBrainz.Types (MBID)
import Skema.HTTP.Client (HttpClient)
import qualified Skema.Media.Providers.FanartTV as FanartTV
import qualified Skema.Media.Providers.CoverArtArchive as CoverArtArchive
import qualified Skema.Media.Providers.LastFM as LastFM
import qualified Skema.Media.Providers.ITunes as ITunes
import qualified Skema.Media.Providers.MusicBrainz as MusicBrainzProvider
import Katip

-- | Default media configuration.
--
-- Uses hardcoded app-level API keys and sensible defaults.
defaultMediaConfig :: MediaConfig
defaultMediaConfig = MediaConfig
  { mcFanartApiKey = Just FanartTV.defaultApiKey
  , mcLastFmApiKey = Just LastFM.defaultApiKey  -- App-level key, user can override
  , mcCacheDir = Nothing  -- Will use system cache dir
  , mcArtistImageSources = [FanartTV, LastFM, MusicBrainz]  -- CoverArtArchive doesn't support artist images
  , mcAlbumCoverSources = [CoverArtArchive, FanartTV, LastFM, ITunes]
  , mcTimeout = 10  -- 10 seconds per provider
  }

-- | Fetch artist image using MBID.
--
-- Tries sources in priority order (waterfall approach) until one succeeds.
fetchArtistImage :: LogEnv
                 -> HttpClient
                 -> MediaConfig
                 -> MBID
                 -> IO (Maybe MediaResult)
fetchArtistImage le httpClient config mbid = do
  runKatipContextT le () "media.artist_image" $ do
    $(logTM) DebugS $ logStr ("Fetching artist image for MBID..." :: Text)

    result <- liftIO $ trySourcesWaterfall
      (mcArtistImageSources config)
      (tryArtistImageSource httpClient config mbid)

    case result of
      Just res -> do
        $(logTM) InfoS $ logStr $
          ("Found artist image from " <> show (mrSource res) :: Text)
        pure $ Just res
      Nothing -> do
        $(logTM) DebugS $ logStr ("No artist image found from any source" :: Text)
        pure Nothing

-- | Fetch album cover using release MBID.
--
-- Tries sources in priority order (waterfall approach) until one succeeds.
fetchAlbumCover :: LogEnv
                -> HttpClient
                -> MediaConfig
                -> MBID
                -> IO (Maybe MediaResult)
fetchAlbumCover le httpClient config mbid = do
  runKatipContextT le () "media.album_cover" $ do
    $(logTM) DebugS $ logStr ("Fetching album cover for release MBID..." :: Text)

    result <- liftIO $ trySourcesWaterfall
      (mcAlbumCoverSources config)
      (tryAlbumCoverSource httpClient config mbid)

    case result of
      Just res -> do
        $(logTM) InfoS $ logStr $
          ("Found album cover from " <> show (mrSource res) :: Text)
        pure $ Just res
      Nothing -> do
        $(logTM) DebugS $ logStr ("No album cover found from any source" :: Text)
        pure Nothing

-- | Fetch artist image by name (fallback when MBID is not available).
--
-- Only works with providers that support name-based lookup.
fetchArtistImageByName :: LogEnv
                       -> HttpClient
                       -> MediaConfig
                       -> Text
                       -> IO (Maybe MediaResult)
fetchArtistImageByName le _httpClient _config _artistName = do
  runKatipContextT le () "media.artist_image" $ do
    $(logTM) DebugS $ logStr ("Artist image fetch by name not yet implemented" :: Text)
    pure Nothing
  -- TODO: Implement name-based lookups for Last.fm, etc.

-- | Fetch album cover by artist and album name (fallback when MBID is not available).
--
-- Useful for providers like iTunes that don't support MBID lookup.
fetchAlbumCoverByNames :: LogEnv
                       -> HttpClient
                       -> MediaConfig
                       -> Text
                       -> Text
                       -> IO (Maybe MediaResult)
fetchAlbumCoverByNames le httpClient _config _artistName _albumTitle = do
  runKatipContextT le () "media.album_cover" $ do
    $(logTM) DebugS $ logStr ("Fetching album cover by names..." :: Text)

    -- Only iTunes supports name-based lookup currently
    result <- liftIO $ ITunes.fetchAlbumCover httpClient _artistName _albumTitle

    case result of
      Right res -> do
        $(logTM) InfoS $ logStr ("Found album cover from iTunes" :: Text)
        pure $ Just res
      Left _ -> do
        $(logTM) DebugS $ logStr ("No album cover found from iTunes" :: Text)
        pure Nothing

-- | Try sources in waterfall fashion until one succeeds.
trySourcesWaterfall :: [MediaSource]
                    -> (MediaSource -> IO (Either MediaError MediaResult))
                    -> IO (Maybe MediaResult)
trySourcesWaterfall [] _ = pure Nothing
trySourcesWaterfall (source:rest) trySource = do
  result <- trySource source
  case result of
    Right res -> pure $ Just res
    Left _err -> trySourcesWaterfall rest trySource

-- | Try to fetch artist image from a specific source.
tryArtistImageSource :: HttpClient
                     -> MediaConfig
                     -> MBID
                     -> MediaSource
                     -> IO (Either MediaError MediaResult)
tryArtistImageSource httpClient config mbid source =
  case source of
    FanartTV ->
      case mcFanartApiKey config of
        Just apiKey -> FanartTV.fetchArtistImage httpClient apiKey mbid
        Nothing -> pure $ Left $ ConfigError "fanart.tv API key not configured"

    LastFM ->
      case mcLastFmApiKey config of
        Just apiKey -> LastFM.fetchArtistImage httpClient apiKey mbid
        Nothing -> pure $ Left $ ConfigError "Last.fm API key not configured"

    MusicBrainz ->
      MusicBrainzProvider.fetchArtistImage httpClient mbid

    -- Providers that don't support artist images
    CoverArtArchive ->
      pure $ Left NoMediaFound  -- CoverArtArchive doesn't support artist images
    ITunes ->
      pure $ Left NoMediaFound  -- iTunes doesn't support artist images
    Deezer ->
      pure $ Left NoMediaFound  -- Not implemented yet

-- | Try to fetch album cover from a specific source.
tryAlbumCoverSource :: HttpClient
                    -> MediaConfig
                    -> MBID
                    -> MediaSource
                    -> IO (Either MediaError MediaResult)
tryAlbumCoverSource httpClient config mbid source =
  case source of
    FanartTV ->
      case mcFanartApiKey config of
        Just apiKey -> FanartTV.fetchAlbumCover httpClient apiKey mbid
        Nothing -> pure $ Left $ ConfigError "fanart.tv API key not configured"

    CoverArtArchive ->
      CoverArtArchive.fetchAlbumCover httpClient mbid

    LastFM ->
      case mcLastFmApiKey config of
        Just apiKey -> LastFM.fetchAlbumCover httpClient apiKey mbid
        Nothing -> pure $ Left $ ConfigError "Last.fm API key not configured"

    MusicBrainz ->
      MusicBrainzProvider.fetchAlbumCover httpClient mbid

    ITunes ->
      pure $ Left NoMediaFound  -- Need artist/album names for iTunes

    Deezer ->
      pure $ Left NoMediaFound  -- Not implemented yet
