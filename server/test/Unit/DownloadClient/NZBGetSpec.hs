{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the NZBGet download client.
--
-- Each test drives the 'DownloadClientAPI' methods against a mock HTTP
-- transport returning canned JSON-RPC responses, exercising the request
-- builders, the response parsers, and the group/history → 'DownloadInfo'
-- status mappers without touching the network. NZBGet posts every call to the
-- same @/jsonrpc@ endpoint, so a single matcher answers each test.
module Unit.DownloadClient.NZBGetSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Aeson (Value, object, toJSON, (.=))

import Helpers.MockClient (withMockHttpClient)
import Helpers.MockHttp (RequestMatcher, respond, methodIs, pathIs, jsonResponse, (.&.))

import Skema.DownloadClient.NZBGet (NZBGetClient, createNZBGetClient)
import Skema.DownloadClient.Types

tests :: TestTree
tests = testGroup "Unit.DownloadClient.NZBGet"
  [ testCase "addDownload returns the assigned NZB id on success" testAddSuccess
  , testCase "addDownload surfaces a non-positive result as an error" testAddBadId
  , testCase "addDownload surfaces a JSON-RPC error object" testAddRpcError
  , testCase "getDownloadStatus maps queue-group statuses" testStatusMapping
  , testCase "getAllDownloads maps both queue and history entries" testGetAll
  , testCase "pauseDownload rejects a non-numeric id" testPauseBadId
  , testCase "removeDownload succeeds on a boolean result" testRemoveOk
  ]

-- | An NZBGet client whose transport answers @POST /jsonrpc@ with @body@.
withClient :: Value -> (NZBGetClient -> IO a) -> IO a
withClient body action =
  withMockHttpClient [rpc body] $ \http ->
    action (createNZBGetClient "http://localhost:6789" "user" "pass" http (Just "music"))
  where
    rpc :: Value -> RequestMatcher
    rpc b = respond (methodIs "POST" .&. pathIs "/jsonrpc") (jsonResponse b)

-- | A JSON-RPC envelope carrying @result@.
rpcResult :: Value -> Value
rpcResult r = object [ "result" .= r, "id" .= (1 :: Int) ]

-- Tests ---------------------------------------------------------------------

testAddSuccess :: Assertion
testAddSuccess = withClient (rpcResult (toJSON (3 :: Int))) $ \c -> do
  res <- addDownload c req
  case res of
    Right r -> do
      adrClientId r @?= "3"
      adrSuccess r @?= True
    Left e -> assertFailure ("expected success, got: " <> show e)
  where
    req = AddDownloadRequest
      { adrUrl = "http://example.com/a.nzb"
      , adrTitle = "Album"
      , adrCategory = Nothing
      , adrPriority = Nothing
      }

testAddBadId :: Assertion
testAddBadId = withClient (rpcResult (toJSON (0 :: Int))) $ \c -> do
  res <- addDownload c req
  assertBool "non-positive id should fail" (isLeft res)
  where
    req = AddDownloadRequest "http://x/a.nzb" "Album" Nothing Nothing

testAddRpcError :: Assertion
testAddRpcError =
  withClient (object [ "error" .= object [ "code" .= (2 :: Int), "message" .= ("boom" :: Text) ]
                     , "id" .= (1 :: Int) ]) $ \c -> do
    res <- addDownload c (AddDownloadRequest "http://x/a.nzb" "Album" Nothing Nothing)
    res @?= Left "boom"

testStatusMapping :: Assertion
testStatusMapping = do
  check "DOWNLOADING" DSDownloading
  check "PAUSED" DSPaused
  check "QUEUED" DSQueued
  check "PP_FINISHED" DSCompleted
  where
    check statusStr expected =
      withClient (rpcResult (toJSON [grp statusStr])) $ \c -> do
        res <- getDownloadStatus c "7"
        case res of
          Right info -> diStatus info @?= expected
          Left e -> assertFailure ("expected status, got: " <> show e)
    grp status = object
      [ "NZBID" .= (7 :: Int)
      , "NZBName" .= ("Album" :: Text)
      , "Status" .= (status :: Text)
      , "FileSizeMB" .= (100 :: Int)
      , "RemainingSizeMB" .= (40 :: Int)
      , "DownloadedSizeMB" .= (60 :: Int)
      , "Category" .= ("music" :: Text)
      ]

-- | A single object carrying the union of queue-group and history fields
-- parses as both, so one canned response exercises both mappers.
testGetAll :: Assertion
testGetAll = withClient (rpcResult (toJSON [unionEntry])) $ \c -> do
  res <- getAllDownloads c
  case res of
    Right infos -> length infos @?= 2  -- one from listgroups, one from history
    Left e -> assertFailure ("expected downloads, got: " <> show e)
  where
    unionEntry = object
      [ "NZBID" .= (7 :: Int)
      , "NZBName" .= ("Album" :: Text)
      , "Name" .= ("Album" :: Text)
      , "Status" .= ("DOWNLOADING" :: Text)
      , "FileSizeMB" .= (100 :: Int)
      , "RemainingSizeMB" .= (0 :: Int)
      , "DownloadedSizeMB" .= (100 :: Int)
      , "Category" .= ("music" :: Text)
      , "DestDir" .= ("/dst" :: Text)
      , "FinalDir" .= ("/final" :: Text)
      ]

testPauseBadId :: Assertion
testPauseBadId = withClient (rpcResult (toJSON True)) $ \c -> do
  res <- pauseDownload c "not-a-number"
  assertBool "non-numeric id should fail" (isLeft res)

testRemoveOk :: Assertion
testRemoveOk = withClient (rpcResult (toJSON True)) $ \c -> do
  res <- removeDownload c "7" False
  res @?= Right ()
