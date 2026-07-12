{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit tests for the Transmission download client.
--
-- Transmission speaks JSON-RPC over @POST /transmission/rpc@ and guards it with
-- a session-id handshake: an unauthenticated request gets a 409 carrying
-- @X-Transmission-Session-Id@, which the client caches and replays. Tests cover
-- both the happy path (server answers 200 directly) and that handshake, plus
-- the status-int → 'DownloadInfo' mapper.
module Unit.DownloadClient.TransmissionSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson (Value, object, toJSON, (.=))
import Network.HTTP.Client (Request, requestHeaders)

import Helpers.MockClient (withMockHttpClient)
import Helpers.MockHttp
  ( RequestMatcher, respond, methodIs, pathIs, jsonResponse, statusResponseWithHeaders, (.&.) )

import Skema.DownloadClient.Transmission (TransmissionClient, createTransmissionClient)
import Skema.DownloadClient.Types

tests :: TestTree
tests = testGroup "Unit.DownloadClient.Transmission"
  [ testCase "addDownload returns the added torrent id" testAdd
  , testCase "getAllDownloads maps torrent status codes" testGetAll
  , testCase "getDownloadStatus rejects a non-numeric id" testBadId
  , testCase "pauseDownload succeeds" testPause
  , testCase "performs the 409 session-id handshake and retries" testHandshake
  ]

rpcPath :: Request -> Bool
rpcPath = methodIs "POST" .&. pathIs "/transmission/rpc"

-- | A client whose transport answers @/transmission/rpc@ with @body@ (status 200,
-- so no session handshake is triggered).
withClient :: Value -> (TransmissionClient -> IO a) -> IO a
withClient body action =
  withMockHttpClient [respond rpcPath (jsonResponse body)] $ \http -> do
    c <- createTransmissionClient "http://localhost:9091" Nothing Nothing http
    action c

success :: Value -> Value
success args = object [ "result" .= ("success" :: Text), "arguments" .= args ]

transTorrent :: Int -> Int -> Value
transTorrent tid status = object
  [ "id" .= tid
  , "hashString" .= ("hash" <> show tid :: Text)
  , "name" .= ("Album" :: Text)
  , "status" .= status
  , "percentDone" .= (0.5 :: Double)
  , "error" .= (0 :: Int)
  ]

-- Tests ---------------------------------------------------------------------

testAdd :: Assertion
testAdd = withClient (success args) $ \c -> do
  res <- addDownload c (AddDownloadRequest "http://x/a.torrent" "Album" Nothing Nothing)
  case res of
    Right r -> adrClientId r @?= "42"
    Left e -> assertFailure ("expected success, got: " <> show e)
  where
    args = object
      [ "torrent-added" .= object
          [ "id" .= (42 :: Int)
          , "hashString" .= ("deadbeef" :: Text)
          , "name" .= ("Album" :: Text)
          ]
      ]

-- The client decodes the @arguments@ value directly as a torrent list.
testGetAll :: Assertion
testGetAll = withClient (success (toJSON torrents)) $ \c -> do
  res <- getAllDownloads c
  case res of
    Right infos -> map diStatus infos @?= [DSPaused, DSDownloading, DSCompleted]
    Left e -> assertFailure ("expected downloads, got: " <> show e)
  where
    torrents = [transTorrent 1 0, transTorrent 2 4, transTorrent 3 6]

testBadId :: Assertion
testBadId = withClient (success (toJSON ([] :: [Value]))) $ \c -> do
  res <- getDownloadStatus c "not-a-number"
  assertBool "non-numeric id should fail" (isLeft res)

testPause :: Assertion
testPause = withClient (success (object [])) $ \c -> do
  res <- pauseDownload c "1"
  res @?= Right ()

-- | First request omits the session id → 409 with the header; the client caches
-- it and retries, and the retry (which carries the header) gets the real answer.
testHandshake :: Assertion
testHandshake =
  withMockHttpClient [challenge, answer] $ \http -> do
    c <- createTransmissionClient "http://localhost:9091" Nothing Nothing http
    res <- testConnection c
    res @?= Right ()
  where
    hasSession req = any ((== "X-Transmission-Session-Id") . fst) (requestHeaders req)
    challenge :: RequestMatcher
    challenge = respond (rpcPath .&. (not . hasSession))
      (statusResponseWithHeaders 409 [("X-Transmission-Session-Id", "sid-123")] "conflict")
    answer :: RequestMatcher
    answer = respond (rpcPath .&. hasSession) (jsonResponse (success (object [])))
