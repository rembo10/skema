{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the pure download-submission decision logic in
-- "Skema.Domain.Download".
module Unit.Domain.DownloadSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Skema.Domain.Download
import Skema.Indexer.Types (ReleaseInfo(..), DownloadType(..), SlskdFile(..))
import Skema.Config.Types
  ( Config(download)
  , DownloadConfig(..)
  , DownloadClient(..)
  , DownloadClientType(..)
  , SlskdConfig(..)
  , defaultConfig
  , downloadClientTypeName
  )
import qualified Skema.Domain.Quality as Quality

tests :: TestTree
tests = testGroup "Unit.Domain.Download"
  [ planTests
  , slskdSubmissionTests
  , traditionalSubmissionTests
  ]

-- Builders -------------------------------------------------------------------

-- | A release of the given type with stable, easy-to-assert fields.
mkRelease :: DownloadType -> ReleaseInfo
mkRelease downloadType = ReleaseInfo
  { riTitle = "Album"
  , riGuid = Nothing
  , riDownloadUrl = "http://example/release"
  , riInfoUrl = Nothing
  , riSize = Just 1000
  , riPublishDate = Nothing
  , riCategory = Nothing
  , riSeeders = Just 5
  , riPeers = Nothing
  , riGrabs = Nothing
  , riDownloadType = downloadType
  , riQuality = Quality.Lossless
  , riSlskdUsername = Nothing
  , riSlskdFiles = Nothing
  }

mkSlskdConfig :: Bool -> SlskdConfig
mkSlskdConfig enabled = SlskdConfig
  { slskdUrl = "http://localhost:5030"
  , slskdApiKey = "key"
  , slskdEnabled = enabled
  , slskdDownloadDirectory = "/downloads"
  }

mkClient :: DownloadClientType -> Bool -> DownloadClient
mkClient clientType enabled = DownloadClient
  { dcType = clientType
  , dcUrl = "http://localhost:8080"
  , dcApiKey = Nothing
  , dcUsername = Nothing
  , dcPassword = Nothing
  , dcEnabled = enabled
  , dcDownloadDir = Nothing
  , dcCategory = Just "music"
  }

mkFile :: Text -> Integer -> SlskdFile
mkFile name size = SlskdFile
  { sfFilename = name
  , sfSize = size
  , sfBitRate = Nothing
  , sfSampleRate = Nothing
  , sfBitDepth = Nothing
  , sfLength = Nothing
  , sfIsLocked = False
  }

-- | The default download config with one field overridden.
cfgWith :: (DownloadConfig -> DownloadConfig) -> DownloadConfig
cfgWith f = f (download defaultConfig)

-- planSubmission -------------------------------------------------------------

planTests :: TestTree
planTests = testGroup "planSubmission"
  [ testCase "slskd download with no slskd client is rejected" $
      planSubmission (cfgWith (\c -> c { downloadSlskdClient = Nothing }))
                     (slskdRelease (Just "user") (Just [mkFile "a" 1]))
        @?= PlanReject NoSlskdClient

  , testCase "slskd download with a disabled client is rejected" $
      planSubmission (cfgWith (\c -> c { downloadSlskdClient = Just (mkSlskdConfig False) }))
                     (slskdRelease (Just "user") (Just [mkFile "a" 1]))
        @?= PlanReject SlskdDisabled

  , testCase "slskd download missing the username is rejected" $
      planSubmission slskdEnabledConfig (slskdRelease Nothing (Just [mkFile "a" 1]))
        @?= PlanReject MissingSlskdDetails

  , testCase "slskd download missing the file list is rejected" $
      planSubmission slskdEnabledConfig (slskdRelease (Just "user") Nothing)
        @?= PlanReject MissingSlskdDetails

  , testCase "slskd download with an empty file list is rejected" $
      planSubmission slskdEnabledConfig (slskdRelease (Just "user") (Just []))
        @?= PlanReject MissingSlskdDetails

  , testCase "valid slskd download is planned with its config, user, and files" $
      planSubmission slskdEnabledConfig (slskdRelease (Just "user") (Just files))
        @?= PlanSlskd (mkSlskdConfig True) "user" files

  , testCase "NZB download with no NZB client is rejected" $
      planSubmission (cfgWith (\c -> c { downloadNzbClient = Nothing })) (mkRelease NZB)
        @?= PlanReject (NoDownloadClient NZB)

  , testCase "NZB download with a disabled client is rejected by name" $
      planSubmission (cfgWith (\c -> c { downloadNzbClient = Just (mkClient SABnzbd False) }))
                     (mkRelease NZB)
        @?= PlanReject (DownloadClientDisabled (downloadClientTypeName SABnzbd))

  , testCase "enabled NZB client yields a traditional plan" $
      planSubmission (cfgWith (\c -> c { downloadNzbClient = Just nzbClient })) (mkRelease NZB)
        @?= PlanTraditional nzbClient

  , testCase "enabled Torrent client yields a traditional plan" $
      planSubmission (cfgWith (\c -> c { downloadTorrentClient = Just torrentClient })) (mkRelease Torrent)
        @?= PlanTraditional torrentClient
  ]
  where
    files = [mkFile "a.flac" 100, mkFile "b.flac" 200]
    slskdEnabledConfig = cfgWith (\c -> c { downloadSlskdClient = Just (mkSlskdConfig True) })
    nzbClient = mkClient SABnzbd True
    torrentClient = mkClient Transmission True
    slskdRelease user fs = (mkRelease Slskd) { riSlskdUsername = user, riSlskdFiles = fs }

-- slskdSubmission ------------------------------------------------------------

slskdSubmissionTests :: TestTree
slskdSubmissionTests = testGroup "slskdSubmission"
  [ testCase "success records a downloading row and a started+queued outcome" $ do
      let (row, outcome) = slskdSubmission 42 release "user" files (Right ())
      row @?= DownloadInsert
        { diCatalogAlbumId = 42
        , diIndexerName = "slskd"
        , diDownloadUrl = "user:Album"
        , diDownloadClient = "slskd"
        , diClientId = Just "user:Album"
        , diStatus = "downloading"
        , diTitle = "Album"
        , diSize = Just 1000
        , diQuality = Just (Quality.qualityToText Quality.Lossless)
        , diFormat = Just "Slskd"
        , diSeeders = Just 5
        , diError = Nothing
        }
      outcome @?= SubmissionSlskdStarted "user" 2 300

  , testCase "failure records a failed row carrying the error" $ do
      let (row, outcome) = slskdSubmission 42 release "user" files (Left "boom")
      row @?= DownloadInsert
        { diCatalogAlbumId = 42
        , diIndexerName = "slskd"
        , diDownloadUrl = "user:Album"
        , diDownloadClient = "slskd"
        , diClientId = Nothing
        , diStatus = "failed"
        , diTitle = "Album"
        , diSize = Just 1000
        , diQuality = Just (Quality.qualityToText Quality.Lossless)
        , diFormat = Just "Slskd"
        , diSeeders = Just 5
        , diError = Just "boom"
        }
      outcome @?= SubmissionFailed "boom"
  ]
  where
    release = mkRelease Slskd
    files = [mkFile "a.flac" 100, mkFile "b.flac" 200]

-- traditionalSubmission ------------------------------------------------------

traditionalSubmissionTests :: TestTree
traditionalSubmissionTests = testGroup "traditionalSubmission"
  [ testCase "success records a downloading row with the client id and NZB format" $ do
      let (row, outcome) = traditionalSubmission 7 "myindexer" (mkRelease NZB) (mkClient SABnzbd True) (Right "client-123")
      row @?= DownloadInsert
        { diCatalogAlbumId = 7
        , diIndexerName = "myindexer"
        , diDownloadUrl = "http://example/release"
        , diDownloadClient = downloadClientTypeName SABnzbd
        , diClientId = Just "client-123"
        , diStatus = "downloading"
        , diTitle = "Album"
        , diSize = Just 1000
        , diQuality = Just (Quality.qualityToText Quality.Lossless)
        , diFormat = Just "NZB"
        , diSeeders = Just 5
        , diError = Nothing
        }
      outcome @?= SubmissionStarted

  , testCase "Torrent downloads are labelled with the Torrent format" $ do
      let (row, _) = traditionalSubmission 7 "myindexer" (mkRelease Torrent) (mkClient Transmission True) (Right "abc")
      diFormat row @?= Just "Torrent"

  , testCase "failure records a failed row with no client id" $ do
      let (row, outcome) = traditionalSubmission 7 "myindexer" (mkRelease NZB) (mkClient SABnzbd True) (Left "nope")
      diStatus row @?= "failed"
      diClientId row @?= Nothing
      diError row @?= Just "nope"
      outcome @?= SubmissionFailed "nope"
  ]
