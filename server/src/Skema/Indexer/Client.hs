{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Skema.Indexer.Client
  ( searchIndexer
  , testIndexerConnection
  -- Exported for testing
  , NewznabResponse(..)
  , NewznabChannel(..)
  , NewznabItem(..)
  , NewznabEnclosure(..)
  , encodeQueryParam
  , normalizeQuery
  ) where

import Control.Exception (try)
import Data.Aeson (FromJSON(..), (.:), (.:?), withObject, Value(..), Object, eitherDecode)
import Data.Aeson.Types (Parser)
import Data.Aeson.Key (fromText)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Read as TR
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.Time (UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Network.URI (escapeURIString, isUnescapedInURIComponent)
import Text.XML (parseText, def)
import Text.XML.Cursor (Cursor, fromDocument, element, content, attribute, ($//), (&/), ($|), ($/))

import Skema.Config.Types (Indexer(..))
import Skema.Indexer.Types
import Skema.Indexer.Utils (downloadTypeFromMimeType)
import Skema.HTTP.Client (HttpClient, prettyHttpError)
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
  , niGuid :: Maybe Text
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

-- | Parse a field that might be an integer or a string containing an integer
-- Some indexers return numeric fields as strings, so we handle both cases
parseIntegerField :: Object -> Text -> Parser (Maybe Integer)
parseIntegerField obj field = do
  maybeVal <- obj .:? fromText field
  case maybeVal of
    Nothing -> pure Nothing
    Just val -> case val of
      Number n -> pure $ Just (round n)
      String s -> pure $ readMaybe (T.unpack s)
      _ -> fail $ "Expected number or string for " <> T.unpack field

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
    niGuid <- v .:? "guid"
    niLink <- v .: "link"
    niComments <- v .:? "comments"
    niPubDate <- v .:? "pubDate"
    niSize <- parseIntegerField v "size"
    niEnclosure <- v .:? "enclosure"
    niAttr <- v .:? "attr"
    pure NewznabItem{..}

instance FromJSON NewznabEnclosure where
  parseJSON = withObject "NewznabEnclosure" $ \v -> do
    -- Enclosure attributes are nested in @attributes wrapper
    attrs <- v .: "@attributes"
    neUrl <- attrs .: "url"
    neLength <- parseIntegerField attrs "length"
    neType <- attrs .:? "type"
    pure NewznabEnclosure{..}

instance FromJSON NewznabAttr where
  parseJSON = withObject "NewznabAttr" $ \v -> do
    -- Attributes are nested in @attributes wrapper
    attrs <- v .: "@attributes"
    naName <- attrs .: "name"
    naValue <- attrs .: "value"
    pure NewznabAttr{..}

-- | Parse XML Newznab/Torznab response to our data types
parseXmlResponse :: LBS.ByteString -> Either Text [ReleaseInfo]
parseXmlResponse xmlBytes = do
  -- Parse XML document
  doc <- first (T.pack . show) $ parseText def (TLE.decodeUtf8 xmlBytes)
  let cursor = fromDocument doc

  -- Get all <item> elements from the RSS feed
  let items = cursor $// element "item"

  -- Parse each item
  pure $ mapMaybe parseXmlItem items

-- | Parse a single <item> element from XML
parseXmlItem :: Cursor -> Maybe ReleaseInfo
parseXmlItem cursor = do
  -- Required fields
  title <- listToMaybe $ cursor $/ element "title" &/ content
  link <- listToMaybe $ cursor $/ element "link" &/ content

  -- Optional fields
  let guid = listToMaybe $ cursor $/ element "guid" &/ content
      comments = listToMaybe $ cursor $/ element "comments" &/ content
      pubDate = listToMaybe $ cursor $/ element "pubDate" &/ content
      sizeText = listToMaybe $ cursor $/ element "size" &/ content
      size = sizeText >>= parseIntegerText

  -- Parse enclosure
  let enclosures = cursor $/ element "enclosure"
      enclosure = listToMaybe enclosures
      enclosureUrl = enclosure >>= (\e -> listToMaybe $ e $| attribute "url")
      enclosureLength = enclosure >>= (\e -> listToMaybe $ e $| attribute "length") >>= parseIntegerText
      enclosureType = enclosure >>= (\e -> listToMaybe $ e $| attribute "type")

  -- Parse newznab/torznab attributes
  let attrElements = cursor $/ element "attr"
      attrs = mapMaybe parseAttr attrElements
      getAttr name = listToMaybe [value | (n, value) <- attrs, n == name]

      category = getAttr "category" >>= parseInteger
      seeders = getAttr "seeders" >>= parseInteger
      peers = getAttr "peers" >>= parseInteger
      grabs = getAttr "grabs" >>= parseInteger

  -- Determine download type
  let downloadType = case enclosureType of
        Just t -> maybe NZB id (downloadTypeFromMimeType t)
        Nothing -> NZB

  -- Get download URL (prefer enclosure URL over link)
  let downloadUrl = fromMaybe link enclosureUrl

  -- Parse publish date
  let publishDate = pubDate >>= parseRFC822 . T.unpack

  -- Get size (prefer enclosure length over direct size field)
  let finalSize = enclosureLength <|> size

  -- Parse quality from title
  let quality = Quality.parseQualityFromTitle title

  pure ReleaseInfo
    { riTitle = title
    , riGuid = guid
    , riDownloadUrl = downloadUrl
    , riInfoUrl = comments
    , riSize = finalSize
    , riPublishDate = publishDate
    , riCategory = category
    , riSeeders = seeders
    , riPeers = peers
    , riGrabs = grabs
    , riDownloadType = downloadType
    , riQuality = quality
    , riSlskdUsername = Nothing
    , riSlskdFiles = Nothing
    }

-- | Parse newznab:attr or torznab:attr element
parseAttr :: Cursor -> Maybe (Text, Text)
parseAttr cursor = do
  name <- listToMaybe $ cursor $| attribute "name"
  value <- listToMaybe $ cursor $| attribute "value"
  pure (name, value)

-- | Parse Integer from Text
parseIntegerText :: Text -> Maybe Integer
parseIntegerText t = case TR.decimal t of
  Right (n, "") -> Just n
  _ -> Nothing

-- | Parse Int from Text
parseInteger :: Text -> Maybe Int
parseInteger t = fmap fromInteger (parseIntegerText t)

-- | Detect if response is JSON or XML based on content
-- JSON starts with '{' or '[', XML starts with '<'
isJsonResponse :: LBS.ByteString -> Bool
isJsonResponse body =
  case LBS.uncons body of
    Nothing -> False
    Just _ ->
      -- Skip whitespace and check first real character
      let trimmed = LBS.dropWhile (\b -> b == 32 || b == 9 || b == 10 || b == 13) body
      in case LBS.uncons trimmed of
           Nothing -> False
           Just (c, _) -> c == 123 || c == 91  -- '{' or '['

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

-- | Normalize a search query by removing special characters
-- This helps with older indexers that don't handle special characters well
-- Examples: "AC/DC" -> "ACDC", "Sigur Rós" -> "Sigur Ros"
normalizeQuery :: Text -> Text
normalizeQuery = T.filter isAlphaNumOrSpace . removeAccents
  where
    -- Keep only alphanumeric characters and spaces
    isAlphaNumOrSpace c = isAlphaNum c || c == ' '

    -- Remove common accents by replacing with base characters
    -- This is a simplified version - for full Unicode normalization, use unicode-transforms
    removeAccents = T.replace "á" "a"
                  . T.replace "à" "a"
                  . T.replace "â" "a"
                  . T.replace "ä" "a"
                  . T.replace "å" "a"
                  . T.replace "ã" "a"
                  . T.replace "æ" "ae"
                  . T.replace "ç" "c"
                  . T.replace "é" "e"
                  . T.replace "è" "e"
                  . T.replace "ê" "e"
                  . T.replace "ë" "e"
                  . T.replace "í" "i"
                  . T.replace "ì" "i"
                  . T.replace "î" "i"
                  . T.replace "ï" "i"
                  . T.replace "ñ" "n"
                  . T.replace "ó" "o"
                  . T.replace "ò" "o"
                  . T.replace "ô" "o"
                  . T.replace "ö" "o"
                  . T.replace "õ" "o"
                  . T.replace "ø" "o"
                  . T.replace "ú" "u"
                  . T.replace "ù" "u"
                  . T.replace "û" "u"
                  . T.replace "ü" "u"
                  . T.replace "ý" "y"
                  . T.replace "ÿ" "y"
                  -- Uppercase versions
                  . T.replace "Á" "A"
                  . T.replace "À" "A"
                  . T.replace "Â" "A"
                  . T.replace "Ä" "A"
                  . T.replace "Å" "A"
                  . T.replace "Ã" "A"
                  . T.replace "Æ" "AE"
                  . T.replace "Ç" "C"
                  . T.replace "É" "E"
                  . T.replace "È" "E"
                  . T.replace "Ê" "E"
                  . T.replace "Ë" "E"
                  . T.replace "Í" "I"
                  . T.replace "Ì" "I"
                  . T.replace "Î" "I"
                  . T.replace "Ï" "I"
                  . T.replace "Ñ" "N"
                  . T.replace "Ó" "O"
                  . T.replace "Ò" "O"
                  . T.replace "Ô" "O"
                  . T.replace "Ö" "O"
                  . T.replace "Õ" "O"
                  . T.replace "Ø" "O"
                  . T.replace "Ú" "U"
                  . T.replace "Ù" "U"
                  . T.replace "Û" "U"
                  . T.replace "Ü" "U"
                  . T.replace "Ý" "Y"
                  . T.replace "Ÿ" "Y"

-- | URL encode a query parameter value
-- Uses UTF-8 encoding and percent-encodes all characters except unreserved ones
encodeQueryParam :: Text -> Text
encodeQueryParam = T.pack . escapeURIString isUnescapedInURIComponent . T.unpack

-- | Internal implementation of indexer search
searchIndexerImpl :: HttpClient -> Indexer -> SearchQuery -> IO [ReleaseInfo]
searchIndexerImpl client Indexer{..} SearchQuery{..} = do
  -- Build search URL with JSON output
  let apiKeyParam = case indexerApiKey of
        Just key -> "&apikey=" <> encodeQueryParam key
        Nothing -> ""

      -- Build query string
      rawQueryStr = case (sqArtist, sqAlbum) of
        (Just artist, Just album) -> artist <> " " <> album
        (Just artist, Nothing) -> artist
        (Nothing, Just album) -> album
        (Nothing, Nothing) -> maybe "" id sqQuery

      -- Optionally normalize query based on indexer settings
      queryStr = if indexerNormalizeQuery
                 then normalizeQuery rawQueryStr
                 else rawQueryStr

      -- Build category string
      catStr = if null sqCategories
        then ""
        else "&cat=" <> T.intercalate "," (map (T.pack . show) sqCategories)

      -- Build output format parameter based on configuration
      -- "auto" mode omits the parameter to get the indexer's default (usually XML)
      outputParam = case T.toLower indexerResponseFormat of
        "xml" -> "&o=xml"
        "json" -> "&o=json"
        _ -> ""  -- "auto" mode: no parameter, accept whatever the indexer sends

      searchUrl = indexerUrl <> "/api?t=search&q=" <> encodeQueryParam queryStr
                  <> apiKeyParam
                  <> catStr
                  <> outputParam
                  <> "&limit=" <> T.pack (show sqLimit)
                  <> "&offset=" <> T.pack (show sqOffset)

  -- Make request with indexer-specific auth (username/password)
  -- Get raw response bytes instead of parsed JSON
  responseBytes <- case (indexerUsername, indexerPassword) of
    (Just user, Just password) -> HTTP.getWithBasicAuth client searchUrl user password
    _ -> HTTP.get client searchUrl

  case responseBytes of
    Left err -> fail $ "HTTP error: " <> T.unpack (prettyHttpError err)
    Right body ->
      -- Parse based on configured format or auto-detect from content
      case T.toLower indexerResponseFormat of
        "json" ->
          -- User explicitly requested JSON
          case eitherDecode body of
            Left parseErr -> fail $ "JSON parse error: " <> parseErr
            Right (NewznabResponse channel) -> pure $ mapMaybe parseItem (ncItem channel)
        "xml" ->
          -- User explicitly requested XML
          case parseXmlResponse body of
            Left parseErr -> fail $ "XML parse error: " <> T.unpack parseErr
            Right releases -> pure releases
        _ -> -- Auto-detect based on response content
          if isJsonResponse body
            then case eitherDecode body of
              Left parseErr -> fail $ "JSON parse error: " <> parseErr
              Right (NewznabResponse channel) -> pure $ mapMaybe parseItem (ncItem channel)
            else case parseXmlResponse body of
              Left parseErr -> fail $ "XML parse error: " <> T.unpack parseErr
              Right releases -> pure releases

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
    , riGuid = niGuid
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
    , riSlskdUsername = Nothing
    , riSlskdFiles = Nothing
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
        Just key -> "?apikey=" <> encodeQueryParam key
        Nothing -> ""
      capsUrl = indexerUrl <> "/api?t=caps" <> apiKeyParam

  -- Make request with indexer-specific auth (username/password)
  result <- case (indexerUsername, indexerPassword) of
    (Just user, Just password) -> HTTP.getWithBasicAuth client capsUrl user password
    _ -> HTTP.get client capsUrl

  case result of
    Left err -> fail $ "HTTP error: " <> T.unpack (prettyHttpError err)
    Right _body -> pure ()  -- Success if we got a 2xx response

-- Parse RFC822 date format
parseRFC822 :: String -> Maybe UTCTime
parseRFC822 = parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z"
