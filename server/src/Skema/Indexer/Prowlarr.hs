{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Prowlarr integration for indexer aggregation.
--
-- Prowlarr provides a unified API to search multiple indexers at once.
-- This module implements:
-- 1. Fetching indexers from Prowlarr to sync with local config
-- 2. Searching through Prowlarr's unified search API
module Skema.Indexer.Prowlarr
  ( searchProwlarr
  , fetchProwlarrIndexers
  , testProwlarrConnection
  , grabRelease
  , ProwlarrRelease(..)
  ) where

import Control.Applicative ((<|>))
import Control.Exception (try, SomeException)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), withObject, object, (.=), encode)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import GHC.Generics (Generic)

import Skema.Config.Types (ProwlarrConfig(..), Indexer(..))
import Skema.Indexer.Types
import Skema.HTTP.Client (HttpClient, prettyHttpError)
import qualified Skema.HTTP.Client as HTTP
import qualified Skema.Domain.Quality as Quality

-- | Prowlarr search result from /api/v1/search
data ProwlarrSearchResult = ProwlarrSearchResult
  { psrGuid :: Text
  , psrTitle :: Text
  , psrDownloadUrl :: Maybe Text
  , psrInfoUrl :: Maybe Text
  , psrSize :: Maybe Integer
  , psrPublishDate :: Maybe Text
  , psrIndexer :: Text
  , psrIndexerId :: Int
  , psrSeeders :: Maybe Int
  , psrLeechers :: Maybe Int
  , psrGrabs :: Maybe Int
  , psrProtocol :: Text  -- "torrent" or "usenet"
  } deriving (Show, Generic)

instance FromJSON ProwlarrSearchResult where
  parseJSON = withObject "ProwlarrSearchResult" $ \v -> do
    psrGuid <- v .: "guid"
    psrTitle <- v .: "title"
    psrDownloadUrl <- v .:? "downloadUrl"
    psrInfoUrl <- v .:? "infoUrl"
    psrSize <- v .:? "size"
    psrPublishDate <- v .:? "publishDate"
    psrIndexer <- v .: "indexer"
    psrIndexerId <- v .: "indexerId"
    psrSeeders <- v .:? "seeders"
    psrLeechers <- v .:? "leechers"
    psrGrabs <- v .:? "grabs"
    psrProtocol <- v .: "protocol"
    pure ProwlarrSearchResult{..}

-- | Prowlarr release info for grabbing - includes guid and indexerId needed for grab API
data ProwlarrRelease = ProwlarrRelease
  { prGuid :: Text
  , prIndexerId :: Int
  , prTitle :: Text
  } deriving (Show, Eq)

-- | Prowlarr indexer from /api/v1/indexer
data ProwlarrIndexer = ProwlarrIndexer
  { piId :: Int
  , piName :: Text
  , piProtocol :: Text  -- "torrent" or "usenet"
  , piEnabled :: Bool
  , piPriority :: Int
  } deriving (Show, Generic)

instance FromJSON ProwlarrIndexer where
  parseJSON = withObject "ProwlarrIndexer" $ \v -> do
    piId <- v .: "id"
    piName <- v .: "name"
    piProtocol <- v .: "protocol"
    piEnabled <- v .: "enable"
    piPriority <- v .: "priority"
    pure ProwlarrIndexer{..}

-- | Search through Prowlarr's unified search API
searchProwlarr :: HttpClient -> ProwlarrConfig -> SearchQuery -> IO (Either IndexerError IndexerResult)
searchProwlarr client config query = do
  startTime <- getCurrentTime
  result <- try $ searchProwlarrImpl client config query
  endTime <- getCurrentTime
  let searchTime = realToFrac $ diffUTCTime endTime startTime

  case result of
    Left (err :: SomeException) ->
      pure $ Left IndexerError
        { ieIndexerName = "Prowlarr"
        , ieError = T.pack $ show err
        }
    Right releases ->
      pure $ Right IndexerResult
        { irIndexerName = "Prowlarr"
        , irReleases = releases
        , irSearchTime = searchTime
        }

-- | Internal implementation of Prowlarr search
searchProwlarrImpl :: HttpClient -> ProwlarrConfig -> SearchQuery -> IO [ReleaseInfo]
searchProwlarrImpl client ProwlarrConfig{..} SearchQuery{..} = do
  -- Build search query string
  let queryStr = case (sqArtist, sqAlbum) of
        (Just artist, Just album) -> artist <> " " <> album
        (Just artist, Nothing) -> artist
        (Nothing, Just album) -> album
        (Nothing, Nothing) -> maybe "" id sqQuery

      -- Prowlarr expects categories as repeated query params: categories=3000&categories=3010
      -- Use audio category 3000 (Music) for Prowlarr
      catParams = T.concat $ map (\c -> "&categories=" <> T.pack (show c)) sqCategories

      -- Build the search URL
      -- Prowlarr v1 API: /api/v1/search?query=...&categories=...
      searchUrl = prowlarrUrl <> "/api/v1/search?query=" <> HTTP.urlEncode queryStr
                  <> catParams
                  <> "&limit=" <> T.pack (show sqLimit)
                  <> "&type=search"  -- Explicitly set search type

      headers = [("X-Api-Key", prowlarrApiKey)]

  -- Make request with API key header
  result <- HTTP.getJSONWithHeaders client searchUrl headers

  case result of
    Left err -> fail $ "HTTP error: " <> T.unpack (prettyHttpError err)
    Right results -> pure $ mapMaybe parseSearchResult results

-- | Parse a Prowlarr search result into ReleaseInfo
parseSearchResult :: ProwlarrSearchResult -> Maybe ReleaseInfo
parseSearchResult ProwlarrSearchResult{..} = do
  downloadUrl <- psrDownloadUrl

  let downloadType = if psrProtocol == "torrent" then Torrent else NZB
      publishDate = psrPublishDate >>= parseISO8601
      quality = Quality.parseQualityFromTitle psrTitle

  pure ReleaseInfo
    { riTitle = psrTitle <> " [" <> psrIndexer <> "]"  -- Include indexer name in title
    , riDownloadUrl = downloadUrl
    , riInfoUrl = psrInfoUrl
    , riSize = psrSize
    , riPublishDate = publishDate
    , riCategory = Nothing  -- Prowlarr doesn't return category in same format
    , riSeeders = psrSeeders
    , riPeers = psrLeechers
    , riGrabs = psrGrabs
    , riDownloadType = downloadType
    , riQuality = quality
    , riProwlarrGuid = Just psrGuid
    , riProwlarrIndexerId = Just psrIndexerId
    }

-- | Fetch list of indexers from Prowlarr
fetchProwlarrIndexers :: HttpClient -> ProwlarrConfig -> IO (Either Text [Indexer])
fetchProwlarrIndexers client ProwlarrConfig{..} = do
  let url = prowlarrUrl <> "/api/v1/indexer"
      headers = [("X-Api-Key", prowlarrApiKey)]

  result <- HTTP.getJSONWithHeaders client url headers

  case result of
    Left err -> pure $ Left $ prettyHttpError err
    Right indexers -> pure $ Right $ map convertIndexer indexers

-- | Convert Prowlarr indexer to our Indexer type
convertIndexer :: ProwlarrIndexer -> Indexer
convertIndexer ProwlarrIndexer{..} = Indexer
  { indexerName = piName <> " (Prowlarr)"
  , indexerUrl = ""  -- Prowlarr handles the URL internally
  , indexerApiKey = Nothing
  , indexerUsername = Nothing
  , indexerPassword = Nothing
  , indexerEnabled = piEnabled
  , indexerPriority = piPriority
  , indexerCategories = [3000, 3010]  -- Default audio categories
  }

-- | Test connection to Prowlarr
testProwlarrConnection :: HttpClient -> ProwlarrConfig -> IO (Either Text ())
testProwlarrConnection client ProwlarrConfig{..} = do
  let url = prowlarrUrl <> "/api/v1/health"
      headers = [("X-Api-Key", prowlarrApiKey)]

  result <- try $ HTTP.getWithHeaders client url headers

  case result of
    Left (err :: SomeException) -> pure $ Left $ T.pack $ show err
    Right (Left httpErr) -> pure $ Left $ prettyHttpError httpErr
    Right (Right _) -> pure $ Right ()

-- | Parse ISO8601 date format (Prowlarr uses this)
parseISO8601 :: Text -> Maybe UTCTime
parseISO8601 t = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (T.unpack t)
             <|> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" (T.unpack t)

-- | Grab a release through Prowlarr - this tells Prowlarr to send it to the configured download client
grabRelease :: HttpClient -> ProwlarrConfig -> ProwlarrRelease -> IO (Either Text ())
grabRelease client ProwlarrConfig{..} ProwlarrRelease{..} = do
  let url = prowlarrUrl <> "/api/v1/search"
      -- Prowlarr expects the release object with guid and indexerId
      body = object
        [ "guid" .= prGuid
        , "indexerId" .= prIndexerId
        ]
  
  result <- try $ HTTP.post client url (encode body) 
              [("X-Api-Key", prowlarrApiKey), ("Content-Type", "application/json")]
  
  case result of
    Left (err :: SomeException) -> pure $ Left $ "Prowlarr grab failed: " <> T.pack (show err)
    Right (Left httpErr) -> 
      -- Check if the error is "already in session" - this is actually a success
      let errText = prettyHttpError httpErr
      in if "already in session" `T.isInfixOf` T.toLower errText || 
            "Torrent already" `T.isInfixOf` errText
         then pure $ Right ()  -- Treat as success
         else pure $ Left $ "Prowlarr grab failed: " <> errText
    Right (Right _) -> pure $ Right ()
