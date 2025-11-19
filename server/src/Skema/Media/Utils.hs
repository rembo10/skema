{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions for media fetching.
module Skema.Media.Utils
  ( upgradeToHttps
  , prettyMediaError
  , fetchAndDecode
  ) where

import Skema.Media.Types (MediaError(..), MediaSource)
import Skema.HTTP.Client (HttpClient)
import qualified Skema.HTTP.Client as HTTP
import Data.Aeson (FromJSON, decode)
import qualified Data.Text as T

-- | Upgrade HTTP URLs to HTTPS for known image CDN domains.
--
-- Many APIs return HTTP URLs but support HTTPS. This function upgrades
-- HTTP URLs to HTTPS for security, covering all known media providers.
upgradeToHttps :: Text -> Text
upgradeToHttps url
  -- Cover Art Archive
  | "http://coverartarchive.org" `T.isPrefixOf` url =
      T.replace "http://coverartarchive.org" "https://coverartarchive.org" url
  -- MusicBrainz
  | "http://musicbrainz.org" `T.isPrefixOf` url =
      T.replace "http://musicbrainz.org" "https://musicbrainz.org" url
  -- Wikimedia Commons (used by MusicBrainz)
  | "http://commons.wikimedia.org" `T.isPrefixOf` url =
      T.replace "http://commons.wikimedia.org" "https://commons.wikimedia.org" url
  | "http://upload.wikimedia.org" `T.isPrefixOf` url =
      T.replace "http://upload.wikimedia.org" "https://upload.wikimedia.org" url
  -- Wikipedia (used by MusicBrainz)
  | "http://en.wikipedia.org" `T.isPrefixOf` url =
      T.replace "http://en.wikipedia.org" "https://en.wikipedia.org" url
  | "http://wikipedia.org" `T.isPrefixOf` url =
      T.replace "http://wikipedia.org" "https://wikipedia.org" url
  -- Last.fm CDN
  | "http://lastfm.freetls.fastly.net" `T.isPrefixOf` url =
      T.replace "http://lastfm.freetls.fastly.net" "https://lastfm.freetls.fastly.net" url
  | "http://lastfm-img2.akamaized.net" `T.isPrefixOf` url =
      T.replace "http://lastfm-img2.akamaized.net" "https://lastfm-img2.akamaized.net" url
  | "http://img2.lastfm.com" `T.isPrefixOf` url =
      T.replace "http://img2.lastfm.com" "https://img2.lastfm.com" url
  -- fanart.tv CDN
  | "http://assets.fanart.tv" `T.isPrefixOf` url =
      T.replace "http://assets.fanart.tv" "https://assets.fanart.tv" url
  -- iTunes/Apple CDN (should already be HTTPS but just in case)
  | "http://is1.mzstatic.com" `T.isPrefixOf` url =
      T.replace "http://is1.mzstatic.com" "https://is1.mzstatic.com" url
  | otherwise = url

-- | Pretty-print a media error.
prettyMediaError :: MediaError -> Text
prettyMediaError NoMediaFound = "No media found from any source"
prettyMediaError (ProviderError source msg) =
  "Error from " <> show source <> ": " <> msg
prettyMediaError (NetworkError msg) = "Network error: " <> msg
prettyMediaError (ParseError msg) = "Parse error: " <> msg
prettyMediaError (ConfigError msg) = "Configuration error: " <> msg

-- | Fetch a URL and decode the JSON response.
-- This abstracts the common pattern of HTTP fetch + JSON decode + error handling
-- used by all media providers.
--
-- The extractor function receives the decoded JSON and should return Nothing
-- if the desired data is not found, triggering a NoMediaFound error.
fetchAndDecode
  :: FromJSON a
  => HttpClient         -- ^ HTTP client
  -> MediaSource        -- ^ Media source (for error reporting)
  -> Text               -- ^ URL to fetch
  -> (a -> Maybe b)     -- ^ Extractor function (Nothing means no media found)
  -> IO (Either MediaError b)
fetchAndDecode httpClient source url extractor = do
  result <- HTTP.get httpClient url
  case result of
    Left err ->
      pure $ Left $ ProviderError source (show err)
    Right body ->
      case decode body of
        Nothing ->
          pure $ Left $ ParseError $ "Failed to parse " <> show source <> " response"
        Just json ->
          case extractor json of
            Nothing ->
              pure $ Left NoMediaFound
            Just extracted ->
              pure $ Right extracted
