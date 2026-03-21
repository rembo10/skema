{-# LANGUAGE OverloadedStrings #-}

-- | Shared HTTP utilities for web scrapers.
module Skema.Scraper.Http
  ( fetchPage
  ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- | Fetch a page with browser-like headers.
-- Returns the response body or an error message.
fetchPage :: Text -> IO (Either String LBS.ByteString)
fetchPage url = do
  manager <- newManager tlsManagerSettings
  req <- parseRequest (T.unpack url)
  let reqWithHeaders = req
        { requestHeaders =
            [ ("User-Agent", "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
            , ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8")
            , ("Accept-Language", "en-US,en;q=0.5")
            ]
        }

  response <- httpLbs reqWithHeaders manager
  pure $ Right (responseBody response)
