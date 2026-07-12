{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the qBittorrent download client.
--
-- qBittorrent authenticates with a cookie obtained from @/api/v2/auth/login@
-- before every operation, so each test provides a login matcher plus a matcher
-- for the operation's endpoint. This exercises the login handshake, the request
-- builders, the torrent parser, and the state-substring → 'DownloadInfo' mapper.
module Unit.DownloadClient.QBittorrentSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson (Value, object, toJSON, (.=))

import Helpers.MockClient (withMockHttpClient)
import Helpers.MockHttp (RequestMatcher, respond, methodIs, pathIs, jsonResponse, textResponse, (.&.))

import Skema.DownloadClient.QBittorrent (QBittorrentClient, createQBittorrentClient)
import Skema.DownloadClient.Types

tests :: TestTree
tests = testGroup "Unit.DownloadClient.QBittorrent"
  [ testCase "testConnection logs in successfully" testConn
  , testCase "addDownload reports success and returns the url as a temp id" testAdd
  , testCase "getAllDownloads maps every torrent state" testGetAll
  , testCase "getDownloadStatus finds a torrent by hash" testStatus
  , testCase "pauseDownload succeeds" testPause
  ]

loginMatcher :: RequestMatcher
loginMatcher = respond (methodIs "POST" .&. pathIs "/api/v2/auth/login") (textResponse "Ok.")

withClient :: [RequestMatcher] -> (QBittorrentClient -> IO a) -> IO a
withClient matchers action =
  withMockHttpClient (loginMatcher : matchers) $ \http -> do
    c <- createQBittorrentClient "http://localhost:8080" "admin" "pass" http
    action c

torrent :: Text -> Text -> Value
torrent hash stateStr = object
  [ "hash" .= hash
  , "name" .= ("Album" :: Text)
  , "state" .= stateStr
  , "progress" .= (0.5 :: Double)
  , "size" .= (100 :: Int)
  , "downloaded" .= (50 :: Int)
  ]

-- Tests ---------------------------------------------------------------------

testConn :: Assertion
testConn = withClient [] $ \c -> do
  res <- testConnection c
  res @?= Right ()

testAdd :: Assertion
testAdd = withClient [addM] $ \c -> do
  res <- addDownload c (AddDownloadRequest "magnet:?xt=urn:btih:abc" "Album" Nothing Nothing)
  case res of
    Right r -> do
      adrSuccess r @?= True
      adrClientId r @?= "magnet:?xt=urn:btih:abc"
    Left e -> assertFailure ("expected success, got: " <> show e)
  where
    addM = respond (methodIs "POST" .&. pathIs "/api/v2/torrents/add") (textResponse "Ok.")

testGetAll :: Assertion
testGetAll = withClient [infoM] $ \c -> do
  res <- getAllDownloads c
  case res of
    Right infos -> map diStatus infos @?=
      [DSDownloading, DSPaused, DSQueued, DSCompleted, DSFailed, DSQueued]
    Left e -> assertFailure ("expected downloads, got: " <> show e)
  where
    infoM = respond (methodIs "GET" .&. pathIs "/api/v2/torrents/info") (jsonResponse body)
    body = toJSON
      [ torrent "h1" "downloading"
      , torrent "h2" "pausedUP"
      , torrent "h3" "queuedDL"
      , torrent "h4" "completed"
      , torrent "h5" "error"
      , torrent "h6" "uploading"  -- no keyword → default DSQueued
      ]

testStatus :: Assertion
testStatus = withClient [infoM] $ \c -> do
  res <- getDownloadStatus c "h2"
  case res of
    Right info -> do
      diClientId info @?= "h2"
      diStatus info @?= DSDownloading
    Left e -> assertFailure ("expected status, got: " <> show e)
  where
    infoM = respond (methodIs "GET" .&. pathIs "/api/v2/torrents/info")
              (jsonResponse (toJSON [torrent "h1" "completed", torrent "h2" "downloading"]))

testPause :: Assertion
testPause = withClient [pauseM] $ \c -> do
  res <- pauseDownload c "h1"
  res @?= Right ()
  where
    pauseM = respond (methodIs "POST" .&. pathIs "/api/v2/torrents/pause") (textResponse "Ok.")
