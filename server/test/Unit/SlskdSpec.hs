{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for slskd on-disk path resolution in "Skema.Services.Slskd".
--
-- These cover 'albumDirectoryCandidates', which derives the list of possible
-- on-disk locations for a completed download from the uploader's remote
-- filenames. The list must cover both slskd layouts (default leaf-only and
-- @${SOURCE_PATH}@ full-tree) so the caller can pick whichever exists on disk.
module Unit.SlskdSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Skema.Services.Slskd (albumDirectoryCandidates)
import Skema.Config.Types (SlskdConfig(..), defaultSlskdConfig)
import Skema.Slskd.Types (SlskdTransfer(..), SlskdTransferState(..))

tests :: TestTree
tests = testGroup "Unit.Slskd"
  [ candidateTests
  ]

-- Builders -------------------------------------------------------------------

-- | A config whose download directory is easy to assert against.
config :: SlskdConfig
config = defaultSlskdConfig { slskdDownloadDirectory = "/data/downloads" }

-- | A completed transfer with the given remote filename; other fields are
-- irrelevant to path resolution and get stable placeholder values.
mkTransfer :: Text -> SlskdTransfer
mkTransfer filename = SlskdTransfer
  { stId = "id"
  , stUsername = "uploader"
  , stFilename = filename
  , stState = TransferCompleted
  , stSize = 1000
  , stBytesTransferred = 1000
  , stPercentComplete = 100
  , stAverageSpeed = 0
  , stElapsedTime = Nothing
  , stRemainingTime = Nothing
  , stException = Nothing
  }

-- Tests ----------------------------------------------------------------------

candidateTests :: TestTree
candidateTests = testGroup "albumDirectoryCandidates"
  [ testCase "offers full-tree then leaf then base for a backslash remote path" $
      -- slskd reports the uploader's remote path with backslashes and an
      -- @@username prefix. Both layouts plus the flat fallback must appear.
      albumDirectoryCandidates config
        [ mkTransfer "@@uploader\\Music\\Artist\\Album\\01.flac"
        , mkTransfer "@@uploader\\Music\\Artist\\Album\\02.flac"
        ]
        @?=
        [ "/data/downloads/Music/Artist/Album" -- ${SOURCE_PATH} layout
        , "/data/downloads/Album"              -- default slskd layout
        , "/data/downloads"                    -- flat layout
        ]

  , testCase "handles forward-slash paths without an @@username prefix" $
      albumDirectoryCandidates config
        [ mkTransfer "Music/Artist/Album/01.flac" ]
        @?=
        [ "/data/downloads/Music/Artist/Album"
        , "/data/downloads/Album"
        , "/data/downloads"
        ]

  , testCase "uses the common prefix across differing remote directories" $
      -- Two files whose remote dirs diverge below Artist: the common album
      -- directory is Artist, and slskd's leaf for it is likewise Artist.
      albumDirectoryCandidates config
        [ mkTransfer "@@uploader\\Music\\Artist\\Disc 1\\01.flac"
        , mkTransfer "@@uploader\\Music\\Artist\\Disc 2\\01.flac"
        ]
        @?=
        [ "/data/downloads/Music/Artist"
        , "/data/downloads/Artist"
        , "/data/downloads"
        ]

  , testCase "dedups when the remote directory is a single segment" $
      -- fullPath and leaf coincide, so only one subdirectory candidate remains.
      albumDirectoryCandidates config
        [ mkTransfer "@@uploader\\Album\\01.flac" ]
        @?=
        [ "/data/downloads/Album"
        , "/data/downloads"
        ]

  , testCase "falls back to the base directory for a bare filename" $
      albumDirectoryCandidates config
        [ mkTransfer "01.flac" ]
        @?=
        [ "/data/downloads" ]

  , testCase "falls back to the base directory when there are no transfers" $
      albumDirectoryCandidates config []
        @?=
        [ "/data/downloads" ]
  ]
