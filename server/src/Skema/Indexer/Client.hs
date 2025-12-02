{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Skema.Indexer.Client
  ( searchIndexer
  , testIndexerConnection
  ) where

import Control.Exception (try)
import Data.Aeson (FromJSON(..), (.:), (.:?), withObject, Value(..), withText)
import Data.Aeson.Types (Parser, explicitParseFieldMaybe)
import Text.Read (readMaybe)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Skema.Config.Types (Indexer(..))
import Skema.Indexer.Types
import Skema.Indexer.Utils (downloadTypeFromMimeType)
import Skema.HTTP.Client (HttpClient, getJSON, prettyHttpError)
import qualified Skema.HTTP.Client as HTTP
import qualified Skema.Domain.Quality as Quality

-- | Newznab JSON response types
data NewznabResponse = NewznabResponse
  { nrChannel :: NewznabChannel
  } deriving (Show, Generic)

data NewznabChannel = NewznabChannel
  { ncItem :: [NewznabItem]
  } deriving (Show, Generic)

data NewznabItem = NewznabItem
  { niTitle :: Text
  , niLink :: Text
  , niComments :: Maybe Text
  , niPubDate :: Maybe Text
  , niSize :: Maybe Integer
  , niEnclosure :: Maybe NewznabEnclosure
  , niAttr :: Maybe [NewznabAttr]
  } deriving (Show, Generic)

data NewznabEnclosure = NewznabEnclosure
  { neUrl :: Text
  , neLength :: Maybe Integer
  , neType :: Maybe Text
  } deriving (Show, Generic)

data NewznabAttr = NewznabAttr
  { naName :: Text
  , naValue :: Text
  } deriving (Show, Generic)

instance FromJSON NewznabResponse where
  parseJSON = withObject "NewznabResponse" $ \v ->
    NewznabResponse <$> v .: "channel"

instance FromJSON NewznabChannel where
  parseJSON = withObject "NewznabChannel" $ \v -> do
    -- Handle both single item (object), multiple items (array), and no items (missing field)
    maybeItems <- v .:? "item"
    items <- case maybeItems of
      Nothing -> pure []  -- No items field = empty results
      Just val -> parseItems val
    pure $ NewznabChannel items
    where
      parseItems :: Value -> Parser [NewznabItem]
      parseItems val = case val of
        Array _ -> parseJSON val  -- Already an array
        Object _ -> (:[]) <$> parseJSON val  -- Single object, wrap in list
        _ -> fail "Expected item to be object or array"

instance FromJSON NewznabItem where
  parseJSON = withObject "NewznabItem" $ \v -> do
    niTitle <- v .: "title"
    niLink <- v .: "link"
    niComments <- v .:? "comments"
    niPubDate <- v .:? "pubDate"
    niSize <- explicitParseFieldMaybe parseIntegerOrString v "size"
    niEnclosure <- v .:? "enclosure"
    niAttr <- v .:? "attr"
    pure NewznabItem{..}

instance FromJSON NewznabEnclosure where
  parseJSON = withObject "NewznabEnclosure" $ \v -> do
    -- Enclosure attributes are nested in @attributes wrapper
    attrs <- v .: "@attributes"
    neUrl <- attrs .: "url"
    neLength <- explicitParseFieldMaybe parseIntegerOrString attrs "length"
    neType <- attrs .:? "type"
    pure NewznabEnclosure{..}

-- | Parse a value that could be either a JSON number or a string containing a number
parseIntegerOrString :: Value -> Parser Integer
parseIntegerOrString (Number n) = pure $ round n
parseIntegerOrString (String s) = case readMaybe (T.unpack s) of
  Just i -> pure i
  Nothing -> fail $ "Cannot parse integer from string: " <> T.unpack s
parseIntegerOrString _ = fail "Expected number or string for integer field"

instance FromJSON NewznabAttr where
  parseJSON = withObject "NewznabAttr" $ \v -> do
    -- Attributes are nested in @attributes wrapper
    attrs <- v .: "@attributes"
    naName <- attrs .: "name"
    naValue <- attrs .: "value"
    pure NewznabAttr{..}

-- | Search a single indexer using Newznab/Torznab API
searchIndexer :: HttpClient -> Indexer -> SearchQuery -> IO (Either IndexerError IndexerResult)
searchIndexer client indexer query = do
  startTime <- getCurrentTime
  result <- try $ searchIndexerImpl client indexer query
  endTime <- getCurrentTime
  let searchTime = realToFrac $ diffUTCTime endTime startTime

  case result of
    Left (err :: SomeException) ->
      pure $ Left $ IndexerError (indexerName indexer) (T.pack $ show err)
    Right releases ->
      pure $ Right $ IndexerResult
        { irIndexerName = indexerName indexer
        , irReleases = releases
        , irSearchTime = searchTime
        }

-- | Internal implementation of indexer search
searchIndexerImpl :: HttpClient -> Indexer -> SearchQuery -> IO [ReleaseInfo]
searchIndexerImpl client Indexer{..} SearchQuery{..} = do
  -- Build search URL with JSON output
  let apiKeyParam = case indexerApiKey of
        Just key -> "&apikey=" <> key
        Nothing -> ""

      -- Build query string
      queryStr = case (sqArtist, sqAlbum) of
        (Just artist, Just album) -> artist <> " " <> album
        (Just artist, Nothing) -> artist
        (Nothing, Just album) -> album
        (Nothing, Nothing) -> maybe "" id sqQuery

      -- Build category string
      catStr = if null sqCategories
        then ""
        else "&cat=" <> T.intercalate "," (map (T.pack . show) sqCategories)

      searchUrl = indexerUrl <> "/api?t=search&o=json&q=" <> queryStr
                  <> apiKeyParam
                  <> catStr
                  <> "&limit=" <> T.pack (show sqLimit)
                  <> "&offset=" <> T.pack (show sqOffset)

  -- Make request with indexer-specific auth (username/password)
  result <- case (indexerUsername, indexerPassword) of
    (Just user, Just pass) -> HTTP.getJSONWithBasicAuth client searchUrl user pass
    _ -> HTTP.getJSON client searchUrl

  case result of
    Left err -> fail $ "HTTP error: " <> T.unpack (prettyHttpError err)
    Right (NewznabResponse channel) ->
      pure $ mapMaybe parseItem (ncItem channel)

-- | Parse a single item from JSON response
parseItem :: NewznabItem -> Maybe ReleaseInfo
parseItem NewznabItem{..} = do
  -- Determine download type from enclosure or default to NZB
  let downloadType = case niEnclosure of
        Just enc -> case neType enc of
          Just t -> maybe NZB id (downloadTypeFromMimeType t)
          Nothing -> NZB
        Nothing -> NZB

  -- Get download URL (prefer enclosure URL over link)
  let downloadUrl = case niEnclosure of
        Just enc -> neUrl enc
        Nothing -> niLink

  -- Parse publish date
  let publishDate = niPubDate >>= parseRFC822 . T.unpack

  -- Get size (prefer enclosure length over direct size field)
  let size = case niEnclosure of
        Just enc -> neLength enc <|> niSize
        Nothing -> niSize

  -- Get attributes
  let attrs = maybe [] id niAttr
      getAttr name = listToMaybe [naValue a | a <- attrs, naName a == name]

      category = getAttr "category" >>= readMaybe . T.unpack
      seeders = getAttr "seeders" >>= readMaybe . T.unpack
      peers = getAttr "peers" >>= readMaybe . T.unpack
      grabs = getAttr "grabs" >>= readMaybe . T.unpack

  -- Parse quality from title
  let quality = Quality.parseQualityFromTitle niTitle

  pure ReleaseInfo
    { riTitle = niTitle
    , riDownloadUrl = downloadUrl
    , riInfoUrl = niComments
    , riSize = size
    , riPublishDate = publishDate
    , riCategory = category
    , riSeeders = seeders
    , riPeers = peers
    , riGrabs = grabs
    , riDownloadType = downloadType
    , riQuality = quality
    }

-- | Test connection to an indexer
testIndexerConnection :: HttpClient -> Indexer -> IO (Either Text ())
testIndexerConnection client indexer = do
  result <- try $ testIndexerImpl client indexer
  case result of
    Left (err :: SomeException) -> pure $ Left $ T.pack $ show err
    Right () -> pure $ Right ()

-- | Internal implementation of connection test
testIndexerImpl :: HttpClient -> Indexer -> IO ()
testIndexerImpl client Indexer{..} = do
  let apiKeyParam = case indexerApiKey of
        Just key -> "?apikey=" <> key
        Nothing -> ""
      capsUrl = indexerUrl <> "/api?t=caps" <> apiKeyParam

  -- Make request with indexer-specific auth (username/password)
  result <- case (indexerUsername, indexerPassword) of
    (Just user, Just pass) -> HTTP.getWithBasicAuth client capsUrl user pass
    _ -> HTTP.get client capsUrl

  case result of
    Left err -> fail $ "HTTP error: " <> T.unpack (prettyHttpError err)
    Right _body -> pure ()  -- Success if we got a 2xx response

-- Parse RFC822 date format
parseRFC822 :: String -> Maybe UTCTime
parseRFC822 = parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z"
