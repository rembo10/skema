{-# LANGUAGE OverloadedStrings #-}

-- | Shared HTTP utilities for web scrapers.
module Skema.Scraper.Http
  ( fetchPage
  ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client (parseRequest, requestHeaders)

import Skema.HTTP.Client (HttpClient, makeRequestWithRetry, prettyHttpError)
import qualified Network.HTTP.Client as HTTP

-- | Fetch a page with browser-like headers.
-- Routes through the shared HttpClient so requests are rate-limited,
-- logged, and mockable in tests.
fetchPage :: HttpClient -> Text -> IO (Either String LBS.ByteString)
fetchPage httpClient url = do
  req <- parseRequest (T.unpack url)
  let reqWithHeaders = req
        { requestHeaders =
            [ ("User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
            , ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8")
            , ("Accept-Language", "en-US,en;q=0.5")
            ]
        }
  result <- makeRequestWithRetry httpClient reqWithHeaders
  pure $ case result of
    Left err -> Left $ T.unpack (prettyHttpError err)
    Right response -> Right (HTTP.responseBody response)
