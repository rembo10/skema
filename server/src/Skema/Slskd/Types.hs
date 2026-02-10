{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Types for the slskd (Soulseek) REST API.
--
-- slskd is a web-based Soulseek client that exposes a REST API
-- for searching, downloading, and monitoring transfers.
module Skema.Slskd.Types
  ( -- * Search Types
    SlskdSearchRequest (..)
  , SlskdSearchResponse (..)
  , SlskdSearchState (..)
  , SlskdSearchResult (..)
  , SlskdFile (..)
    -- * Transfer Types
  , SlskdTransfer (..)
  , SlskdTransferState (..)
  , SlskdUserTransfers (..)
  , SlskdDirectoryTransfers (..)
    -- * Album Grouping
  , SlskdAlbumCandidate (..)
  ) where

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , object
  , withObject
  , withText
  , (.:)
  , (.:?)
  , (.=)
  , (.!=)
  )
import qualified Data.Text as T

-- | Request to initiate a search.
-- POST /api/v0/searches
data SlskdSearchRequest = SlskdSearchRequest
  { ssrSearchText :: Text
    -- ^ Search query text
  , ssrId :: Maybe Text
    -- ^ Optional search ID (generated if not provided)
  }
  deriving (Show, Eq, Generic)

instance ToJSON SlskdSearchRequest where
  toJSON SlskdSearchRequest {..} =
    object
      [ "searchText" .= ssrSearchText
      , "id" .= ssrId
      ]

-- | Search state enum from slskd API.
data SlskdSearchState
  = SearchNone
  | SearchRequested
  | SearchInProgress
  | SearchCompleted
  | SearchCancelled
  | SearchTimedOut
  | SearchResponsesExhausted
  deriving (Show, Eq, Generic)

instance FromJSON SlskdSearchState where
  parseJSON = withText "SlskdSearchState" $ \t ->
    -- slskd can return comma-separated states like "Completed, TimedOut"
    -- Parse each part and return the most significant state
    let states = map (T.toLower . T.strip) $ T.splitOn "," t
        parseState s = case s of
          "none" -> Just SearchNone
          "requested" -> Just SearchRequested
          "inprogress" -> Just SearchInProgress
          "completed" -> Just SearchCompleted
          "cancelled" -> Just SearchCancelled
          "timedout" -> Just SearchTimedOut
          "responsesexhausted" -> Just SearchResponsesExhausted
          _ -> Nothing
        parsedStates = mapMaybe parseState states
    in case parsedStates of
         [] -> fail $ "Unknown search state: " <> toString t
         -- Prefer completed states over timed out
         (x:xs) | SearchCompleted `elem` (x:xs) -> pure SearchCompleted
                | SearchTimedOut `elem` (x:xs) -> pure SearchTimedOut
                | otherwise -> pure x

instance ToJSON SlskdSearchState where
  toJSON SearchNone = "None"
  toJSON SearchRequested = "Requested"
  toJSON SearchInProgress = "InProgress"
  toJSON SearchCompleted = "Completed"
  toJSON SearchCancelled = "Cancelled"
  toJSON SearchTimedOut = "TimedOut"
  toJSON SearchResponsesExhausted = "ResponsesExhausted"

-- | Search response from slskd API.
-- GET /api/v0/searches/{id}
data SlskdSearchResponse = SlskdSearchResponse
  { ssState :: SlskdSearchState
    -- ^ Current state of the search
  , ssId :: Text
    -- ^ Search ID
  , ssSearchText :: Text
    -- ^ Original search text
  , ssResponseCount :: Int
    -- ^ Number of user responses received
  , ssFileCount :: Int
    -- ^ Total file count across all responses
  , ssLockedFileCount :: Int
    -- ^ Number of locked (non-downloadable) files
  , ssResponses :: [SlskdSearchResult]
    -- ^ List of results from each responding user
  }
  deriving (Show, Eq, Generic)

instance FromJSON SlskdSearchResponse where
  parseJSON = withObject "SlskdSearchResponse" $ \o -> do
    ssState <- o .: "state"
    ssId <- o .: "id"
    ssSearchText <- o .: "searchText"
    ssResponseCount <- o .:? "responseCount" .!= 0
    ssFileCount <- o .:? "fileCount" .!= 0
    ssLockedFileCount <- o .:? "lockedFileCount" .!= 0
    ssResponses <- o .:? "responses" .!= []
    pure SlskdSearchResponse {..}

-- | Search result from a single user.
data SlskdSearchResult = SlskdSearchResult
  { ssrUsername :: Text
    -- ^ Soulseek username
  , ssrHasFreeUploadSlot :: Bool
    -- ^ Whether user has free upload slots
  , ssrUploadSpeed :: Int
    -- ^ User's upload speed in bytes/second
  , ssrQueueLength :: Int
    -- ^ Number of files in user's upload queue
  , ssrFileCount :: Int
    -- ^ Number of matching files from this user
  , ssrLockedFileCount :: Int
    -- ^ Number of locked files
  , ssrFiles :: [SlskdFile]
    -- ^ List of matching files
  }
  deriving (Show, Eq, Generic)

instance FromJSON SlskdSearchResult where
  parseJSON = withObject "SlskdSearchResult" $ \o -> do
    ssrUsername <- o .: "username"
    ssrHasFreeUploadSlot <- o .:? "hasFreeUploadSlot" .!= False
    ssrUploadSpeed <- o .:? "uploadSpeed" .!= 0
    ssrQueueLength <- o .:? "queueLength" .!= 0
    ssrFileCount <- o .:? "fileCount" .!= 0
    ssrLockedFileCount <- o .:? "lockedFileCount" .!= 0
    ssrFiles <- o .:? "files" .!= []
    pure SlskdSearchResult {..}

-- | File information from slskd search results.
data SlskdFile = SlskdFile
  { sfFilename :: Text
    -- ^ Full path including filename (e.g., "@@username/path/to/file.flac")
  , sfSize :: Integer
    -- ^ File size in bytes
  , sfBitRate :: Maybe Int
    -- ^ Audio bitrate (for MP3/lossy formats)
  , sfSampleRate :: Maybe Int
    -- ^ Sample rate in Hz
  , sfBitDepth :: Maybe Int
    -- ^ Bit depth (e.g., 16, 24)
  , sfLength :: Maybe Int
    -- ^ Duration in seconds
  , sfIsLocked :: Bool
    -- ^ Whether file is locked (non-downloadable)
  }
  deriving (Show, Eq, Generic)

instance FromJSON SlskdFile where
  parseJSON = withObject "SlskdFile" $ \o -> do
    sfFilename <- o .: "filename"
    sfSize <- o .: "size"
    sfBitRate <- o .:? "bitRate"
    sfSampleRate <- o .:? "sampleRate"
    sfBitDepth <- o .:? "bitDepth"
    sfLength <- o .:? "length"
    sfIsLocked <- o .:? "isLocked" .!= False
    pure SlskdFile {..}

instance ToJSON SlskdFile where
  toJSON SlskdFile {..} =
    object
      [ "filename" .= sfFilename
      , "size" .= sfSize
      , "bitRate" .= sfBitRate
      , "sampleRate" .= sfSampleRate
      , "bitDepth" .= sfBitDepth
      , "length" .= sfLength
      , "isLocked" .= sfIsLocked
      ]

-- | Transfer state enum from slskd API.
data SlskdTransferState
  = TransferNone
  | TransferRequested
  | TransferQueued
  | TransferInitializing
  | TransferInProgress
  | TransferCompleted
  | TransferRejected
  | TransferTimedOut
  | TransferCancelled
  | TransferErrored
  deriving (Show, Eq, Generic)

instance FromJSON SlskdTransferState where
  parseJSON = withText "SlskdTransferState" $ \t ->
    -- slskd returns comma-separated states like "Completed, Succeeded" or "Queued, Remotely"
    -- We split on comma and look for the primary state
    let states = map (T.toLower . T.strip) $ T.splitOn "," t
        parseState s = case s of
          "none" -> Just TransferNone
          "requested" -> Just TransferRequested
          "queued" -> Just TransferQueued
          "initializing" -> Just TransferInitializing
          "inprogress" -> Just TransferInProgress
          "completed" -> Just TransferCompleted
          "rejected" -> Just TransferRejected
          "timedout" -> Just TransferTimedOut
          "cancelled" -> Just TransferCancelled
          "errored" -> Just TransferErrored
          -- Secondary states that can be ignored (they're modifiers)
          "succeeded" -> Nothing
          "locally" -> Nothing
          "remotely" -> Nothing
          _ -> Nothing
        parsedStates = mapMaybe parseState states
    in case parsedStates of
         [] -> fail $ "Unknown transfer state: " <> toString t
         -- Priority: Error states first, then success states
         -- "Completed, Errored" should be Errored, not Completed
         (x:xs)
           | TransferErrored `elem` (x:xs) -> pure TransferErrored
           | TransferRejected `elem` (x:xs) -> pure TransferRejected
           | TransferCancelled `elem` (x:xs) -> pure TransferCancelled
           | TransferTimedOut `elem` (x:xs) -> pure TransferTimedOut
           | TransferCompleted `elem` (x:xs) -> pure TransferCompleted
           | TransferInProgress `elem` (x:xs) -> pure TransferInProgress
           | TransferInitializing `elem` (x:xs) -> pure TransferInitializing
           | TransferQueued `elem` (x:xs) -> pure TransferQueued
           | TransferRequested `elem` (x:xs) -> pure TransferRequested
           | otherwise -> pure x

instance ToJSON SlskdTransferState where
  toJSON TransferNone = "None"
  toJSON TransferRequested = "Requested"
  toJSON TransferQueued = "Queued"
  toJSON TransferInitializing = "Initializing"
  toJSON TransferInProgress = "InProgress"
  toJSON TransferCompleted = "Completed"
  toJSON TransferRejected = "Rejected"
  toJSON TransferTimedOut = "TimedOut"
  toJSON TransferCancelled = "Cancelled"
  toJSON TransferErrored = "Errored"

-- | Transfer (download) status from slskd API.
-- GET /api/v0/transfers/downloads/{username}/{id}
data SlskdTransfer = SlskdTransfer
  { stId :: Text
    -- ^ Transfer ID
  , stUsername :: Text
    -- ^ Remote user's username
  , stFilename :: Text
    -- ^ Remote file path
  , stState :: SlskdTransferState
    -- ^ Current transfer state
  , stSize :: Integer
    -- ^ Total file size in bytes
  , stBytesTransferred :: Integer
    -- ^ Bytes downloaded so far
  , stPercentComplete :: Double
    -- ^ Download progress (0.0 to 100.0)
  , stAverageSpeed :: Double
    -- ^ Average download speed in bytes/second
  , stElapsedTime :: Maybe Text
    -- ^ Elapsed time as string
  , stRemainingTime :: Maybe Text
    -- ^ Estimated remaining time as string
  , stException :: Maybe Text
    -- ^ Error message if transfer failed
  }
  deriving (Show, Eq, Generic)

instance FromJSON SlskdTransfer where
  parseJSON = withObject "SlskdTransfer" $ \o -> do
    stId <- o .: "id"
    stUsername <- o .: "username"
    stFilename <- o .: "filename"
    stState <- o .: "state"
    stSize <- o .: "size"
    stBytesTransferred <- o .:? "bytesTransferred" .!= 0
    stPercentComplete <- o .:? "percentComplete" .!= 0.0
    stAverageSpeed <- o .:? "averageSpeed" .!= 0.0
    stElapsedTime <- o .:? "elapsedTime"
    stRemainingTime <- o .:? "remainingTime"
    stException <- o .:? "exception"
    pure SlskdTransfer {..}

-- | Directory with transfer files from slskd API.
-- Part of the nested response from GET /api/v0/transfers/downloads
data SlskdDirectoryTransfers = SlskdDirectoryTransfers
  { sdtDirectory :: Text
    -- ^ Directory path
  , sdtFileCount :: Int
    -- ^ Number of files
  , sdtFiles :: [SlskdTransfer]
    -- ^ Transfer files
  }
  deriving (Show, Eq, Generic)

instance FromJSON SlskdDirectoryTransfers where
  parseJSON = withObject "SlskdDirectoryTransfers" $ \o -> do
    sdtDirectory <- o .: "directory"
    sdtFileCount <- o .:? "fileCount" .!= 0
    sdtFiles <- o .:? "files" .!= []
    pure SlskdDirectoryTransfers {..}

-- | User with directories from slskd API.
-- Part of the nested response from GET /api/v0/transfers/downloads
data SlskdUserTransfers = SlskdUserTransfers
  { sutUsername :: Text
    -- ^ Username
  , sutDirectories :: [SlskdDirectoryTransfers]
    -- ^ Directories with transfers
  }
  deriving (Show, Eq, Generic)

instance FromJSON SlskdUserTransfers where
  parseJSON = withObject "SlskdUserTransfers" $ \o -> do
    sutUsername <- o .: "username"
    sutDirectories <- o .:? "directories" .!= []
    pure SlskdUserTransfers {..}

-- | Grouped album candidate from search results.
--
-- Files from the same user in the same directory are grouped
-- together as a potential album download.
data SlskdAlbumCandidate = SlskdAlbumCandidate
  { sacUsername :: Text
    -- ^ Username providing this album
  , sacDirectory :: Text
    -- ^ Common directory path for all files
  , sacFiles :: [SlskdFile]
    -- ^ Files in this album
  , sacTotalSize :: Integer
    -- ^ Total size of all files in bytes
  , sacTrackCount :: Int
    -- ^ Number of audio tracks
  , sacHasFreeUploadSlot :: Bool
    -- ^ Whether user has free upload slots
  , sacQueueLength :: Int
    -- ^ User's upload queue length
  , sacUploadSpeed :: Int
    -- ^ User's upload speed in bytes/second
  }
  deriving (Show, Eq, Generic)

instance ToJSON SlskdAlbumCandidate where
  toJSON SlskdAlbumCandidate {..} =
    object
      [ "username" .= sacUsername
      , "directory" .= sacDirectory
      , "files" .= sacFiles
      , "total_size" .= sacTotalSize
      , "track_count" .= sacTrackCount
      , "has_free_upload_slot" .= sacHasFreeUploadSlot
      , "queue_length" .= sacQueueLength
      , "upload_speed" .= sacUploadSpeed
      ]
