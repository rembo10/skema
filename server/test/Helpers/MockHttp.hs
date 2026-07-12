{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | HTTP mocking for tests.
--
-- Mock transports substitute for a real network connection in tests. A
-- mock is a list of request matchers. When a request arrives, each matcher
-- is tried in order; the first one to return 'Just' a response wins.
-- Requests that match no matcher throw 'UnexpectedHttpRequest' - tests
-- fail loudly when production code starts talking to a new endpoint.
module Helpers.MockHttp
  ( -- * Building mock transports
    mockTransport
  , RequestMatcher
  , respond
    -- * Predicates
  , methodIs
  , hostIs
  , pathIs
  , pathStartsWith
  , (.&.)
    -- * Response builders
  , jsonResponse
  , textResponse
  , statusResponse
  , statusResponseWithHeaders
    -- * Exceptions
  , UnexpectedHttpRequest(..)
  ) where

import Control.Exception (throwIO)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client.Internal
  ( Response(..)
  , ResponseClose(..)
  )
import Network.HTTP.Client (Request, host, method, path, queryString, secure)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (Status, ResponseHeaders, http11, mkStatus, ok200)

import Skema.HTTP.Client (HttpTransport(..))

-- | A request matcher: given a request, optionally produce a response.
-- Matchers are checked in order, first Just wins.
type RequestMatcher = Request -> Maybe (Response LBS.ByteString)

-- | Build a mock transport from a list of matchers.
-- Unmatched requests throw 'UnexpectedHttpRequest' so tests fail loudly
-- when production code reaches an unexpected endpoint.
mockTransport :: [RequestMatcher] -> HttpTransport
mockTransport matchers = HttpTransport $ \req ->
  case firstJust (map ($ req) matchers) of
    Just resp -> pure $ resp { responseOriginalRequest = req }
    Nothing -> throwIO $ UnexpectedHttpRequest
      { uhrMethod = TE.decodeUtf8 (method req)
      , uhrUrl = renderUrl req
      }
  where
    firstJust = foldr (\x acc -> maybe acc Just x) Nothing

-- | Combine a predicate and a canned response into a matcher.
respond :: (Request -> Bool) -> Response LBS.ByteString -> RequestMatcher
respond p resp req = if p req then Just resp else Nothing

-- | Combine two predicates with logical AND.
(.&.) :: (Request -> Bool) -> (Request -> Bool) -> (Request -> Bool)
p .&. q = \r -> p r && q r
infixr 3 .&.

-- | Match on HTTP method (e.g. "GET", "POST").
methodIs :: ByteString -> Request -> Bool
methodIs m req = method req == m

-- | Match on exact host.
hostIs :: Text -> Request -> Bool
hostIs h req = TE.decodeUtf8 (host req) == h

-- | Match on exact request path.
pathIs :: Text -> Request -> Bool
pathIs p req = TE.decodeUtf8 (path req) == p

-- | Match when the request path starts with a given prefix.
pathStartsWith :: Text -> Request -> Bool
pathStartsWith prefix req = prefix `T.isPrefixOf` TE.decodeUtf8 (path req)

-- | Build a 200 response carrying a JSON-encoded value.
jsonResponse :: ToJSON a => a -> Response LBS.ByteString
jsonResponse val = buildResponse ok200 [("Content-Type", "application/json")] (encode val)

-- | Build a 200 response with a plain text body.
textResponse :: Text -> Response LBS.ByteString
textResponse txt = buildResponse ok200 [("Content-Type", "text/plain")] (LBS.fromStrict (TE.encodeUtf8 txt))

-- | Build a response with an arbitrary status code and body.
statusResponse :: Int -> LBS.ByteString -> Response LBS.ByteString
statusResponse code body =
  buildResponse (mkStatus code (TE.encodeUtf8 ("status " <> T.pack (show code)))) [] body

-- | Build a response with an arbitrary status code, response headers, and body.
-- Used to simulate handshakes that carry state in a header (e.g. Transmission's
-- @X-Transmission-Session-Id@ on a 409).
statusResponseWithHeaders :: Int -> ResponseHeaders -> LBS.ByteString -> Response LBS.ByteString
statusResponseWithHeaders code headers body =
  buildResponse (mkStatus code (TE.encodeUtf8 ("status " <> T.pack (show code)))) headers body

buildResponse :: Status -> ResponseHeaders -> LBS.ByteString -> Response LBS.ByteString
buildResponse status headers body = Response
  { responseStatus = status
  , responseVersion = http11
  , responseHeaders = headers
  , responseBody = body
  , responseCookieJar = mempty
  , responseClose' = ResponseClose (pure ())
  , responseOriginalRequest = HTTP.defaultRequest
  , responseEarlyHints = []
  }

-- | Thrown when the mock transport receives a request no matcher handles.
-- The message includes the method and URL so test output points at the
-- surprise call.
data UnexpectedHttpRequest = UnexpectedHttpRequest
  { uhrMethod :: Text
  , uhrUrl :: Text
  } deriving (Show, Exception)

renderUrl :: Request -> Text
renderUrl req =
  let scheme = if secure req then "https://" else "http://"
  in scheme
       <> TE.decodeUtf8 (host req)
       <> TE.decodeUtf8 (path req)
       <> TE.decodeUtf8 (queryString req)
