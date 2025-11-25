{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | MusicBrainz API client using centralized HTTP client.
--
-- This module provides functions to query the MusicBrainz API for releases.
-- It uses the centralized HTTP client for automatic rate limiting, retries,
-- and proper user agent formatting.
module Skema.MusicBrainz.Client
  ( -- * API Client
    searchReleases
  , getRelease
  , getArtist
  , searchArtists
  , searchReleaseGroups
  , MBClientEnv(..)
  , newMBClientEnv
  , MBClientError(..)
  , prettyClientError
  ) where

import Skema.MusicBrainz.Types
import Skema.HTTP.Client (HttpClient, HttpError, getJSON, getJSONWithBasicAuth, prettyHttpError)
import Skema.Config.Types (MusicBrainzConfig, MusicBrainzServer(..), getMusicBrainzServerUrl, mbServer, mbUsername, mbPassword)
import Data.Aeson (FromJSON)
import qualified Data.Text as T
import Network.HTTP.Types.URI (urlEncode)

-- | MusicBrainz client error
data MBClientError
  = MBHttpError HttpError      -- ^ HTTP error from centralized client
  | MBInvalidResponse Text     -- ^ Response parsing failed or was unexpected
  deriving (Show, Eq)

-- | Pretty print a MusicBrainz client error
prettyClientError :: MBClientError -> Text
prettyClientError (MBHttpError err) = prettyHttpError err
prettyClientError (MBInvalidResponse msg) = "Invalid MusicBrainz response: " <> msg

-- | Client environment for MusicBrainz API
data MBClientEnv = MBClientEnv
  { mbHttpClient :: HttpClient
    -- ^ Centralized HTTP client (handles rate limiting, retries, auth)
  , mbBaseUrl :: Text
    -- ^ Base URL (official musicbrainz.org or VIP mirror)
  , mbConfig :: MusicBrainzConfig
    -- ^ MusicBrainz configuration (for per-request auth)
  }

-- | Create a new MusicBrainz client environment
newMBClientEnv :: HttpClient -> MusicBrainzConfig -> MBClientEnv
newMBClientEnv httpClient config =
  let baseUrl = getMusicBrainzServerUrl config <> "/ws/2"
  in MBClientEnv httpClient baseUrl config

-- | URL encode a query parameter
encodeParam :: Text -> Text
encodeParam = decodeUtf8 . urlEncode False . encodeUtf8

-- | Build a MusicBrainz API URL with query parameters
buildMBUrl :: Text -> Text -> [(Text, Text)] -> Text
buildMBUrl baseUrl endpoint params =
  let paramStr = T.intercalate "&" $ map (\(k, v) -> k <> "=" <> encodeParam v) params
      separator = if null params then "" else "?"
  in baseUrl <> "/" <> endpoint <> separator <> paramStr

-- | Make a request with appropriate auth based on config
--
-- Applies Basic Auth if using Headphones VIP with credentials configured.
-- Otherwise uses regular getJSON (which may apply domain-level auth if configured).
mbGetJSON :: FromJSON a => MBClientEnv -> Text -> IO (Either HttpError a)
mbGetJSON MBClientEnv{..} url =
  case (mbServer mbConfig, mbUsername mbConfig, mbPassword mbConfig) of
    (HeadphonesVIP, Just user, Just pass) ->
      getJSONWithBasicAuth mbHttpClient url user pass
    _ ->
      getJSON mbHttpClient url

-- | Search for releases matching a query.
--
-- The query follows Lucene syntax. Examples:
-- - "artist:Radiohead AND release:OK Computer"
-- - "release:Nevermind AND artist:Nirvana"
-- - "barcode:724384260552"
--
-- Automatically retries with exponential backoff via centralized HTTP client.
searchReleases :: MBClientEnv -> Text -> Maybe Int -> Maybe Int -> IO (Either MBClientError MBReleaseSearch)
searchReleases env@MBClientEnv{..} query limit offset = do
  let params = [("query", query), ("fmt", "json")]
             <> maybe [] (\l -> [("limit", show l)]) limit
             <> maybe [] (\o -> [("offset", show o)]) offset
      url = buildMBUrl mbBaseUrl "release" params

  result <- mbGetJSON env url
  pure $ case result of
    Left err -> Left $ MBHttpError err
    Right releases -> Right releases

-- | Get a specific release by MusicBrainz ID with full details.
--
-- Includes tracks, recordings, and other related data.
--
-- Automatically retries with exponential backoff via centralized HTTP client.
getRelease :: MBClientEnv -> ReleaseMBID -> IO (Either MBClientError MBRelease)
getRelease env@MBClientEnv{..} (MBID mbid) = do
  -- Include comprehensive metadata for accurate identification and diff checking
  let includes = "recordings+artist-credits+labels+genres+media+release-groups"
      params = [("inc", includes), ("fmt", "json")]
      url = buildMBUrl mbBaseUrl ("release/" <> mbid) params

  result <- mbGetJSON env url
  pure $ case result of
    Left err -> Left $ MBHttpError err
    Right release -> Right release

-- | Get an artist with their release groups.
--
-- Automatically retries with exponential backoff via centralized HTTP client.
getArtist :: MBClientEnv -> ArtistMBID -> IO (Either MBClientError MBArtist)
getArtist env@MBClientEnv{..} (MBID mbid) = do
  -- Include release groups to see the artist's discography
  let includes = "release-groups"
      params = [("inc", includes), ("fmt", "json")]
      url = buildMBUrl mbBaseUrl ("artist/" <> mbid) params

  result <- mbGetJSON env url
  pure $ case result of
    Left err -> Left $ MBHttpError err
    Right artist -> Right artist

-- | Search for artists matching a query.
--
-- The query follows Lucene syntax. Examples:
-- - "artist:Radiohead"
-- - "Nirvana"
--
-- Automatically retries with exponential backoff via centralized HTTP client.
searchArtists :: MBClientEnv -> Text -> Maybe Int -> Maybe Int -> IO (Either MBClientError MBArtistSearch)
searchArtists env@MBClientEnv{..} query limit offset = do
  let params = [("query", query), ("fmt", "json")]
             <> maybe [] (\l -> [("limit", show l)]) limit
             <> maybe [] (\o -> [("offset", show o)]) offset
      url = buildMBUrl mbBaseUrl "artist" params

  result <- mbGetJSON env url
  pure $ case result of
    Left err -> Left $ MBHttpError err
    Right artists -> Right artists

-- | Search for release groups matching a query.
--
-- The query follows Lucene syntax. Examples:
-- - "releasegroup:OK Computer AND artist:Radiohead"
-- - "In Rainbows"
--
-- Automatically retries with exponential backoff via centralized HTTP client.
searchReleaseGroups :: MBClientEnv -> Text -> Maybe Int -> Maybe Int -> IO (Either MBClientError MBReleaseGroupSearch)
searchReleaseGroups env@MBClientEnv{..} query limit offset = do
  let params = [("query", query), ("fmt", "json")]
             <> maybe [] (\l -> [("limit", show l)]) limit
             <> maybe [] (\o -> [("offset", show o)]) offset
      url = buildMBUrl mbBaseUrl "release-group" params

  result <- mbGetJSON env url
  pure $ case result of
    Left err -> Left $ MBHttpError err
    Right releaseGroups -> Right releaseGroups
