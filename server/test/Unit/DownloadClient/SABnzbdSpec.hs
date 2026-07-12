{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the SABnzbd download client.
--
-- SABnzbd is a GET-based API where the operation is selected by a @mode@ query
-- parameter, so tests route canned responses by matching on the query string.
-- This exercises the request builders, the (string-or-number) slot parsers, and
-- the queue/history → 'DownloadInfo' status mappers.
module Unit.DownloadClient.SABnzbdSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson (Value, object, (.=))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (Request, queryString)

import Helpers.MockClient (withMockHttpClient)
import Helpers.MockHttp (RequestMatcher, respond, methodIs, pathIs, jsonResponse, (.&.))

import Skema.DownloadClient.SABnzbd (SABnzbdClient, createSABnzbdClient)
import Skema.DownloadClient.Types

tests :: TestTree
tests = testGroup "Unit.DownloadClient.SABnzbd"
  [ testCase "addDownload returns the first NZO id" testAdd
  , testCase "addDownload fails when no NZO id is returned" testAddEmpty
  , testCase "getDownloadStatus maps a queue slot" testQueueStatus
  , testCase "getAllDownloads maps queue and history slots" testGetAll
  , testCase "removeDownload succeeds" testRemove
  ]

-- | Match the SABnzbd @/api@ endpoint whose query string contains @needle@.
apiWithQuery :: Text -> Value -> RequestMatcher
apiWithQuery needle body =
  respond (methodIs "GET" .&. pathIs "/api" .&. queryHas needle) (jsonResponse body)

queryHas :: Text -> Request -> Bool
queryHas needle req = needle `T.isInfixOf` TE.decodeUtf8 (queryString req)

withClient :: [RequestMatcher] -> (SABnzbdClient -> IO a) -> IO a
withClient matchers action =
  withMockHttpClient matchers $ \http ->
    action (createSABnzbdClient "http://localhost:8080" "apikey" http (Just "/downloads") (Just "music"))

-- Tests ---------------------------------------------------------------------

testAdd :: Assertion
testAdd = withClient [apiWithQuery "mode=addurl" body] $ \c -> do
  res <- addDownload c (AddDownloadRequest "http://x/a.nzb" "Album" (Just "music") (Just 1))
  case res of
    Right r -> adrClientId r @?= "SABnzbd_nzo_abc"
    Left e -> assertFailure ("expected success, got: " <> show e)
  where
    body = object [ "nzo_ids" .= (["SABnzbd_nzo_abc"] :: [Text]) ]

testAddEmpty :: Assertion
testAddEmpty = withClient [apiWithQuery "mode=addurl" body] $ \c -> do
  res <- addDownload c (AddDownloadRequest "http://x/a.nzb" "Album" Nothing Nothing)
  assertBool "empty nzo_ids should fail" (isLeft res)
  where
    body = object [ "nzo_ids" .= ([] :: [Text]) ]

testQueueStatus :: Assertion
testQueueStatus = withClient [apiWithQuery "mode=queue" body] $ \c -> do
  res <- getDownloadStatus c "nzo1"
  case res of
    Right info -> do
      diStatus info @?= DSDownloading
      diProgress info @?= 0.5
    Left e -> assertFailure ("expected status, got: " <> show e)
  where
    body = object
      [ "queue" .= object
          [ "slots" .=
              [ object
                  [ "nzo_id" .= ("nzo1" :: Text)
                  , "filename" .= ("Album" :: Text)
                  , "status" .= ("Downloading" :: Text)
                  , "mbleft" .= ("50.0" :: Text)
                  , "mb" .= ("100.0" :: Text)
                  , "percentage" .= ("50" :: Text)
                  , "cat" .= ("music" :: Text)
                  ]
              ]
          ]
      ]

testGetAll :: Assertion
testGetAll = withClient [apiWithQuery "mode=history" histBody, apiWithQuery "mode=queue" queueBody] $ \c -> do
  res <- getAllDownloads c
  case res of
    Right infos -> do
      length infos @?= 2
      map diStatus infos @?= [DSPaused, DSCompleted]
    Left e -> assertFailure ("expected downloads, got: " <> show e)
  where
    queueBody = object
      [ "queue" .= object
          [ "slots" .=
              [ object
                  [ "nzo_id" .= ("q1" :: Text)
                  , "filename" .= ("A" :: Text)
                  , "status" .= ("Paused" :: Text)
                  , "mbleft" .= ("10" :: Text)
                  , "mb" .= ("10" :: Text)
                  , "percentage" .= ("0" :: Text)
                  ]
              ]
          ]
      ]
    histBody = object
      [ "history" .= object
          [ "slots" .=
              [ object
                  [ "nzo_id" .= ("h1" :: Text)
                  , "name" .= ("B" :: Text)
                  , "status" .= ("Completed" :: Text)
                  , "bytes" .= (12345 :: Int)
                  , "category" .= ("music" :: Text)
                  , "storage" .= ("/downloads/B" :: Text)
                  ]
              ]
          ]
      ]

testRemove :: Assertion
testRemove = withClient [apiWithQuery "mode=queue" (object [])] $ \c -> do
  res <- removeDownload c "nzo1" True
  res @?= Right ()
