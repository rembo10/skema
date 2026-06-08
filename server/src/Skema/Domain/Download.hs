{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Pure decision logic for submitting a release to a download client.
--
-- 'Skema.Services.Download.Submission.submitDownload' wires these pure
-- functions to IO: it routes via 'planSubmission', performs the HTTP call,
-- then turns the client's result into a 'DownloadInsert' (the row to
-- persist) and a 'SubmissionOutcome' (what happened, for event emission).
module Skema.Domain.Download
  ( -- * Planning
    SubmissionPlan(..)
  , SubmissionRejection(..)
  , planSubmission
    -- * Outcome of an attempt
  , DownloadInsert(..)
  , SubmissionOutcome(..)
  , slskdSubmission
  , traditionalSubmission
  ) where

import Skema.Indexer.Types (ReleaseInfo(..), DownloadType(..), SlskdFile(..))
import Skema.Config.Types
  ( DownloadConfig(..)
  , DownloadClient(..)
  , SlskdConfig(..)
  , downloadClientTypeName
  )
import Skema.Domain.Quality (qualityToText)

-- | Why a release cannot be submitted.
data SubmissionRejection
  = NoSlskdClient                 -- ^ slskd download but no slskd client configured
  | SlskdDisabled                 -- ^ slskd client present but disabled
  | MissingSlskdDetails           -- ^ slskd download missing username or files
  | NoDownloadClient DownloadType -- ^ no client configured for this download type
  | DownloadClientDisabled Text   -- ^ client configured but disabled (carries its type name)
  deriving (Show, Eq)

-- | How a release should be submitted, or why it cannot be.
data SubmissionPlan
  = PlanSlskd SlskdConfig Text [SlskdFile]  -- ^ slskd config, username, files
  | PlanTraditional DownloadClient          -- ^ enabled NZB/Torrent client
  | PlanReject SubmissionRejection
  deriving (Show, Eq)

-- | Decide how (if at all) a release should be submitted, given the
-- configured clients. slskd downloads route to the slskd client; NZB and
-- Torrent downloads route to their respective traditional clients.
planSubmission :: DownloadConfig -> ReleaseInfo -> SubmissionPlan
planSubmission config release = case riDownloadType release of
  Slskd -> case downloadSlskdClient config of
    Nothing -> PlanReject NoSlskdClient
    Just slskdConfig
      | not (slskdEnabled slskdConfig) -> PlanReject SlskdDisabled
      | otherwise -> case (riSlskdUsername release, riSlskdFiles release) of
          (Just username, Just files) | not (null files) -> PlanSlskd slskdConfig username files
          _ -> PlanReject MissingSlskdDetails
  downloadType -> case clientFor downloadType of
    Nothing -> PlanReject (NoDownloadClient downloadType)
    Just client
      | not (dcEnabled client) -> PlanReject (DownloadClientDisabled (downloadClientTypeName (dcType client)))
      | otherwise -> PlanTraditional client
  where
    clientFor NZB = downloadNzbClient config
    clientFor Torrent = downloadTorrentClient config
    clientFor Slskd = Nothing  -- handled above; here only for totality

-- | The fields of a download row to persist. The constant columns
-- (progress, download path) and the queued-at timestamp are supplied by
-- the IO layer at insert time.
data DownloadInsert = DownloadInsert
  { diCatalogAlbumId :: Int64
  , diIndexerName :: Text
  , diDownloadUrl :: Text
  , diDownloadClient :: Text
  , diClientId :: Maybe Text
  , diStatus :: Text
  , diTitle :: Text
  , diSize :: Maybe Integer
  , diQuality :: Maybe Text
  , diFormat :: Maybe Text
  , diSeeders :: Maybe Int
  , diError :: Maybe Text
  } deriving (Show, Eq)

-- | What happened, so the service can emit the right events once the row
-- has been assigned an id.
data SubmissionOutcome
  = SubmissionFailed Text                   -- ^ error message
  | SubmissionStarted                       -- ^ traditional download accepted
  | SubmissionSlskdStarted Text Int Integer -- ^ username, file count, total size
  deriving (Show, Eq)

-- | Build the row and outcome for a slskd submission from the result of
-- queueing the files.
slskdSubmission
  :: Int64           -- ^ catalog album id
  -> ReleaseInfo
  -> Text            -- ^ slskd username
  -> [SlskdFile]     -- ^ files queued
  -> Either Text ()  -- ^ result of queueing
  -> (DownloadInsert, SubmissionOutcome)
slskdSubmission albumId release username files = \case
  Left err ->
    (row "failed" Nothing (Just err), SubmissionFailed err)
  Right () ->
    ( row "downloading" (Just ref) Nothing
    , SubmissionSlskdStarted username (length files) (sum (map sfSize files))
    )
  where
    ref = username <> ":" <> riTitle release
    row status clientId mErr = DownloadInsert
      { diCatalogAlbumId = albumId
      , diIndexerName = "slskd"
      , diDownloadUrl = ref
      , diDownloadClient = "slskd"
      , diClientId = clientId
      , diStatus = status
      , diTitle = riTitle release
      , diSize = riSize release
      , diQuality = Just (qualityToText (riQuality release))
      , diFormat = Just "Slskd"
      , diSeeders = riSeeders release
      , diError = mErr
      }

-- | Build the row and outcome for a traditional (NZB/Torrent) submission
-- from the client's add-download result (Left error, or Right client id).
traditionalSubmission
  :: Int64             -- ^ catalog album id
  -> Text              -- ^ indexer name
  -> ReleaseInfo
  -> DownloadClient    -- ^ the client used (for its type name)
  -> Either Text Text  -- ^ Left error | Right client-assigned id
  -> (DownloadInsert, SubmissionOutcome)
traditionalSubmission albumId indexerName release client = \case
  Left err ->
    (row "failed" Nothing (Just err), SubmissionFailed err)
  Right clientId ->
    (row "downloading" (Just clientId) Nothing, SubmissionStarted)
  where
    row status clientId mErr = DownloadInsert
      { diCatalogAlbumId = albumId
      , diIndexerName = indexerName
      , diDownloadUrl = riDownloadUrl release
      , diDownloadClient = downloadClientTypeName (dcType client)
      , diClientId = clientId
      , diStatus = status
      , diTitle = riTitle release
      , diSize = riSize release
      , diQuality = Just (qualityToText (riQuality release))
      , diFormat = Just (formatLabel (riDownloadType release))
      , diSeeders = riSeeders release
      , diError = mErr
      }

formatLabel :: DownloadType -> Text
formatLabel NZB = "NZB"
formatLabel Torrent = "Torrent"
formatLabel Slskd = "Slskd"
