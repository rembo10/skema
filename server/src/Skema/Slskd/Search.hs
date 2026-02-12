{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Search and result processing for slskd.
--
-- This module handles searching slskd, grouping results by album,
-- and converting results to the common ReleaseInfo type for unified ranking.
module Skema.Slskd.Search
  ( -- * Search
    searchSlskd
  , searchSlskdWithTimeout
  , searchSlskdStreaming
    -- * Result Processing
  , groupResultsByAlbum
  , slskdCandidateToReleaseInfo
    -- * Quality Detection
  , detectQualityFromFiles
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.IORef as IORef
import System.FilePath.Posix (takeDirectory, takeExtension, takeFileName)

import Katip

import Skema.Domain.Quality (Quality (..), parseQuality)
import qualified Skema.Indexer.Types as Indexer
import Skema.Indexer.Types (DownloadType (..), ReleaseInfo (..))
import Skema.Slskd.Client
  ( SlskdClient
  , initiateSearch
  , getSearchStatus
  , getSearchResponses
  , deleteSearch
  )
import Skema.Slskd.Types

-- | Search slskd and return album candidates.
--
-- This function:
-- 1. Initiates a search
-- 2. Polls for completion
-- 3. Groups results by album (user + directory)
-- 4. Returns album candidates sorted by quality
searchSlskd ::
  LogEnv ->
  SlskdClient ->
  Text -> -- Artist name
  Text -> -- Album title
  IO (Either Text [SlskdAlbumCandidate])
searchSlskd le client artistName albumTitle = do
  -- Default 60 second timeout
  searchSlskdWithTimeout le client artistName albumTitle 60

-- | Search slskd with configurable timeout.
searchSlskdWithTimeout ::
  LogEnv ->
  SlskdClient ->
  Text -> -- Artist name
  Text -> -- Album title
  Int -> -- Timeout in seconds
  IO (Either Text [SlskdAlbumCandidate])
searchSlskdWithTimeout le client artistName albumTitle timeoutSecs = do
  let searchQuery = artistName <> " " <> albumTitle

  runKatipContextT le () "slskd.search" $ do
    $(logTM) InfoS $ logStr $ ("Initiating slskd search: " <> searchQuery :: Text)

  -- Initiate search
  searchResult <- initiateSearch client searchQuery
  case searchResult of
    Left err -> do
      runKatipContextT le () "slskd.search" $ do
        $(logTM) ErrorS $ logStr $ ("Failed to initiate search: " <> err :: Text)
      pure $ Left err
    Right searchId -> do
      runKatipContextT le () "slskd.search" $ do
        $(logTM) DebugS $ logStr $ ("Search initiated with ID: " <> searchId :: Text)

      -- Poll for completion with timeout
      let timeoutMicros = timeoutSecs * 1000000
      result <-
        race
          (threadDelay timeoutMicros)
          (pollSearchCompletion le client searchId)

      case result of
        Left () -> do
          -- Clean up search on timeout
          _ <- deleteSearch client searchId
          runKatipContextT le () "slskd.search" $ do
            $(logTM) WarningS $
              logStr $
                ("Search timed out after " <> show timeoutSecs <> " seconds" :: Text)
          pure $ Left "Search timed out"
        Right (Left err) -> do
          -- Clean up search on error
          _ <- deleteSearch client searchId
          pure $ Left err
        Right (Right response) -> do
          -- The status endpoint doesn't include file details, fetch them separately
          runKatipContextT le () "slskd.search" $ do
            $(logTM) DebugS $
              logStr $
                ( "Search completed with "
                    <> show (ssResponseCount response)
                    <> " users and "
                    <> show (ssFileCount response)
                    <> " files, fetching details..."
                    :: Text
                )

          responsesResult <- getSearchResponses client searchId

          -- Clean up search after fetching responses
          _ <- deleteSearch client searchId

          case responsesResult of
            Left err -> do
              runKatipContextT le () "slskd.search" $ do
                $(logTM) ErrorS $
                  logStr $ ("Failed to fetch search responses: " <> err :: Text)
              pure $ Left err
            Right responses -> do
              -- Create a response with the actual file data
              let responseWithFiles = response { ssResponses = responses }
              let candidates = groupResultsByAlbum responseWithFiles
              runKatipContextT le () "slskd.search" $ do
                $(logTM) InfoS $
                  logStr $
                    ( "Found "
                        <> show (length candidates)
                        <> " album candidates from "
                        <> show (length responses)
                        <> " users"
                        :: Text
                    )
              pure $ Right candidates

-- | Search slskd with streaming results.
--
-- This version calls the callback with new album candidates as they're
-- discovered during the search, rather than waiting for completion.
-- Returns all candidates at the end for completeness.
searchSlskdStreaming ::
  LogEnv ->
  SlskdClient ->
  Text -> -- Artist name
  Text -> -- Album title
  Int -> -- Timeout in seconds
  (SlskdAlbumCandidate -> IO ()) -> -- Callback for each new candidate
  IO (Either Text [SlskdAlbumCandidate])
searchSlskdStreaming le client artistName albumTitle timeoutSecs onCandidate = do
  let searchQuery = artistName <> " " <> albumTitle

  runKatipContextT le () "slskd.search" $ do
    $(logTM) InfoS $ logStr $ ("Initiating streaming slskd search: " <> searchQuery :: Text)

  -- Initiate search
  searchResult <- initiateSearch client searchQuery
  case searchResult of
    Left err -> do
      runKatipContextT le () "slskd.search" $ do
        $(logTM) ErrorS $ logStr $ ("Failed to initiate search: " <> err :: Text)
      pure $ Left err
    Right searchId -> do
      runKatipContextT le () "slskd.search" $ do
        $(logTM) DebugS $ logStr $ ("Search initiated with ID: " <> searchId :: Text)

      -- Track emitted candidates by (username, directory) to avoid duplicates
      emittedRef <- IORef.newIORef (Set.empty :: Set.Set (Text, Text))
      allCandidatesRef <- IORef.newIORef ([] :: [SlskdAlbumCandidate])

      -- Poll for completion with timeout, emitting results as we go
      let timeoutMicros = timeoutSecs * 1000000
      result <-
        race
          (threadDelay timeoutMicros)
          (pollAndEmit le client searchId emittedRef allCandidatesRef onCandidate)

      -- Clean up search
      _ <- deleteSearch client searchId

      case result of
        Left () -> do
          runKatipContextT le () "slskd.search" $ do
            $(logTM) WarningS $
              logStr $
                ("Search timed out after " <> show timeoutSecs <> " seconds" :: Text)
          -- Return whatever we found so far
          allCandidates <- IORef.readIORef allCandidatesRef
          pure $ Right allCandidates
        Right (Left err) -> pure $ Left err
        Right (Right _) -> do
          allCandidates <- IORef.readIORef allCandidatesRef
          runKatipContextT le () "slskd.search" $ do
            $(logTM) InfoS $
              logStr $
                ( "Streaming search complete: "
                    <> show (length allCandidates)
                    <> " album candidates"
                    :: Text
                )
          pure $ Right allCandidates

-- | Poll search and emit new candidates as they're found.
pollAndEmit ::
  LogEnv ->
  SlskdClient ->
  Text -> -- Search ID
  IORef (Set.Set (Text, Text)) -> -- Emitted candidates (username, directory)
  IORef [SlskdAlbumCandidate] -> -- All candidates found
  (SlskdAlbumCandidate -> IO ()) -> -- Callback
  IO (Either Text ())
pollAndEmit le client searchId emittedRef allCandidatesRef onCandidate = go 0
  where
    -- Poll interval: 1.5 seconds
    pollIntervalMicros = 1500000
    -- Max polls before giving up (60 seconds / 1.5 = 40 polls)
    maxPolls = 40 :: Int

    go pollCount
      | pollCount >= maxPolls = pure $ Left "Search polling exceeded max attempts"
      | otherwise = do
          -- First check status to see if search is done
          statusResult <- getSearchStatus client searchId
          case statusResult of
            Left err -> pure $ Left err
            Right statusResponse -> do
              runKatipContextT le () "slskd.search" $ do
                $(logTM) DebugS $
                  logStr $
                    ( "Poll "
                        <> show pollCount
                        <> ": state="
                        <> show (ssState statusResponse)
                        <> ", users="
                        <> show (ssResponseCount statusResponse)
                        <> ", files="
                        <> show (ssFileCount statusResponse)
                        :: Text
                    )

              -- Fetch current responses (even if search is still in progress)
              responsesResult <- getSearchResponses client searchId
              case responsesResult of
                Left err -> do
                  runKatipContextT le () "slskd.search" $ do
                    $(logTM) DebugS $ logStr $ ("Failed to fetch responses: " <> err :: Text)
                Right responses -> do
                  runKatipContextT le () "slskd.search" $ do
                    $(logTM) DebugS $
                      logStr $ ("Fetched " <> show (length responses) <> " user responses" :: Text)

                  -- Group into candidates
                  let dummyResponse = SlskdSearchResponse
                        { ssState = SearchInProgress
                        , ssId = searchId
                        , ssSearchText = ""
                        , ssResponseCount = length responses
                        , ssFileCount = 0
                        , ssLockedFileCount = 0
                        , ssResponses = responses
                        }
                  let candidates = groupResultsByAlbum dummyResponse

                  -- Emit new candidates
                  emitted <- IORef.readIORef emittedRef
                  let newCandidates = filter (\c -> not $ Set.member (sacUsername c, sacDirectory c) emitted) candidates

                  forM_ newCandidates $ \candidate -> do
                    onCandidate candidate
                    -- Track as emitted
                    emittedSet <- IORef.readIORef emittedRef
                    IORef.writeIORef emittedRef (Set.insert (sacUsername candidate, sacDirectory candidate) emittedSet)
                    -- Add to all candidates
                    allCandidates <- IORef.readIORef allCandidatesRef
                    IORef.writeIORef allCandidatesRef (candidate : allCandidates)

                  when (not (null newCandidates)) $ do
                    runKatipContextT le () "slskd.search" $ do
                      $(logTM) InfoS $
                        logStr $
                          ( "Emitted "
                              <> show (length newCandidates)
                              <> " new slskd candidates"
                              :: Text
                          )

              -- Check if search is complete
              case ssState statusResponse of
                SearchCompleted -> pure $ Right ()
                SearchTimedOut -> pure $ Right ()
                SearchResponsesExhausted -> pure $ Right ()
                SearchCancelled -> pure $ Left "Search was cancelled"
                _ -> do
                  -- Wait before next poll
                  threadDelay pollIntervalMicros
                  go (pollCount + 1)

-- | Poll search status until completion.
pollSearchCompletion ::
  LogEnv ->
  SlskdClient ->
  Text -> -- Search ID
  IO (Either Text SlskdSearchResponse)
pollSearchCompletion le client searchId = go 0
  where
    -- Poll interval: 500ms
    pollIntervalMicros = 500000
    -- Max polls before giving up (60 seconds / 0.5 = 120 polls)
    maxPolls = 120 :: Int

    go pollCount
      | pollCount >= maxPolls = pure $ Left "Search polling exceeded max attempts"
      | otherwise = do
          threadDelay pollIntervalMicros
          result <- getSearchStatus client searchId
          case result of
            Left err -> pure $ Left err
            Right response -> do
              case ssState response of
                SearchCompleted -> pure $ Right response
                SearchTimedOut -> pure $ Right response
                SearchResponsesExhausted -> pure $ Right response
                SearchCancelled -> pure $ Left "Search was cancelled"
                SearchInProgress -> do
                  when (pollCount `mod` 10 == 0) $ do
                    runKatipContextT le () "slskd.search" $ do
                      $(logTM) DebugS $
                        logStr $
                          ( "Search in progress: "
                              <> show (ssFileCount response)
                              <> " files from "
                              <> show (ssResponseCount response)
                              <> " users"
                              :: Text
                          )
                  go (pollCount + 1)
                _ -> go (pollCount + 1)

-- | Group search results by album (user + directory).
--
-- Files from the same user in the same directory are considered
-- to be part of the same album.
groupResultsByAlbum :: SlskdSearchResponse -> [SlskdAlbumCandidate]
groupResultsByAlbum response =
  let allUserResults = ssResponses response

      -- For each user, group their files by directory
      userCandidates = concatMap groupUserFiles allUserResults

      -- Filter out candidates with too few tracks (< 3)
      -- and sort by quality score
      validCandidates = filter (\c -> sacTrackCount c >= 3) userCandidates
   in sortBy compareCandidates validCandidates

-- | Group a single user's files by directory.
groupUserFiles :: SlskdSearchResult -> [SlskdAlbumCandidate]
groupUserFiles result =
  let username = ssrUsername result
      files = filter (not . sfIsLocked) (ssrFiles result)

      -- Group files by directory
      filesByDir =
        Map.toList $
          Map.fromListWith (++) $
            map (\f -> (getDirectory (sfFilename f), [f])) files

      -- Create a candidate for each directory with audio files
      candidates = mapMaybe (mkCandidate username result) filesByDir
   in candidates
  where
    getDirectory :: Text -> Text
    getDirectory path =
      -- Normalize Windows backslashes to forward slashes
      let normalizedPath = T.replace "\\" "/" path
      in toText $ takeDirectory $ toString normalizedPath

    mkCandidate :: Text -> SlskdSearchResult -> (Text, [SlskdFile]) -> Maybe SlskdAlbumCandidate
    mkCandidate username userResult (dir, dirFiles) =
      let audioFiles = filter isAudioFile dirFiles
          trackCount = length audioFiles
       in if trackCount >= 3
            then
              Just
                SlskdAlbumCandidate
                  { sacUsername = username
                  , sacDirectory = dir
                  , sacFiles = audioFiles
                  , sacTotalSize = sum $ map sfSize audioFiles
                  , sacTrackCount = trackCount
                  , sacHasFreeUploadSlot = ssrHasFreeUploadSlot userResult
                  , sacQueueLength = ssrQueueLength userResult
                  , sacUploadSpeed = ssrUploadSpeed userResult
                  }
            else Nothing

-- | Check if a file is an audio file based on extension.
isAudioFile :: SlskdFile -> Bool
isAudioFile file =
  let ext = T.toLower $ toText $ takeExtension $ toString $ sfFilename file
   in ext `elem` audioExtensions

audioExtensions :: [Text]
audioExtensions =
  [ ".flac"
  , ".mp3"
  , ".m4a"
  , ".ogg"
  , ".opus"
  , ".wav"
  , ".ape"
  , ".wv" -- WavPack
  , ".alac"
  ]

-- | Compare album candidates for sorting.
--
-- Prefer:
-- 1. Higher quality (FLAC > MP3)
-- 2. Users with free upload slots
-- 3. Lower queue length
-- 4. Higher upload speed
compareCandidates :: SlskdAlbumCandidate -> SlskdAlbumCandidate -> Ordering
compareCandidates a b =
  -- First compare by quality (better quality = higher rank)
  let qualA = detectQualityFromFiles (sacFiles a)
      qualB = detectQualityFromFiles (sacFiles b)
      qualOrd = compare qualB qualA -- Reversed for descending
   in case qualOrd of
        EQ ->
          -- Then prefer users with free slots
          case (sacHasFreeUploadSlot a, sacHasFreeUploadSlot b) of
            (True, False) -> LT
            (False, True) -> GT
            _ ->
              -- Then prefer lower queue length
              case compare (sacQueueLength a) (sacQueueLength b) of
                EQ ->
                  -- Then prefer higher upload speed
                  compare (sacUploadSpeed b) (sacUploadSpeed a)
                other -> other
        other -> other

-- | Detect quality from a list of files.
--
-- Returns the lowest quality found (pessimistic approach).
detectQualityFromFiles :: [SlskdFile] -> Quality
detectQualityFromFiles [] = Unknown
detectQualityFromFiles files =
  let qualities = map detectFileQuality files
   in fromMaybe Unknown (viaNonEmpty List.minimum qualities)

-- | Detect quality from a single file.
detectFileQuality :: SlskdFile -> Quality
detectFileQuality file =
  let filename = T.toLower $ sfFilename file
      ext = toText $ takeExtension $ toString filename
   in case ext of
        ".flac" ->
          -- Check for hi-res indicators
          case sfBitDepth file of
            Just bd | bd >= 24 -> HiResLossless
            _ -> case sfSampleRate file of
              Just sr | sr >= 88200 -> HiResLossless
              _ -> Lossless
        ".ape" -> Lossless
        ".wav" -> Lossless
        ".wv" -> Lossless
        ".alac" -> Lossless
        ".m4a" ->
          -- Could be ALAC or AAC
          case sfBitRate file of
            Just br | br > 1000 -> Lossless -- Likely ALAC
            Just br | br >= 320 -> MP3_320
            Just br | br >= 256 -> MP3_256
            Just br | br >= 192 -> MP3_192
            _ -> Unknown
        ".mp3" ->
          case sfBitRate file of
            Just br | br >= 310 -> MP3_320
            Just br | br >= 245 -> VBR0
            Just br | br >= 225 -> MP3_256
            Just br | br >= 180 -> VBR2
            Just br | br >= 160 -> MP3_192
            _ ->
              -- Try to parse from filename
              fromMaybe Unknown $ parseQuality filename
        ".ogg" ->
          case sfBitRate file of
            Just br | br >= 320 -> MP3_320
            Just br | br >= 256 -> MP3_256
            Just br | br >= 192 -> MP3_192
            _ -> Unknown
        ".opus" ->
          case sfBitRate file of
            Just br | br >= 256 -> MP3_320 -- Opus is efficient
            Just br | br >= 128 -> MP3_256
            _ -> Unknown
        _ -> Unknown

-- | Convert a slskd album candidate to a ReleaseInfo for unified ranking.
--
-- This allows slskd results to be compared against indexer results
-- using the same scoring and ranking logic.
slskdCandidateToReleaseInfo :: SlskdAlbumCandidate -> ReleaseInfo
slskdCandidateToReleaseInfo candidate =
  let quality = detectQualityFromFiles (sacFiles candidate)

      -- Create a title from directory name, with fallbacks
      -- Normalize Windows backslashes first
      normalizedDir = T.replace "\\" "/" (sacDirectory candidate)
      dirName = toText $ takeFileName $ toString normalizedDir
      title = if T.null dirName || dirName == "."
                then sacUsername candidate <> " - " <> show (sacTrackCount candidate) <> " tracks"
                else dirName <> " [" <> sacUsername candidate <> "]"

      -- Calculate seeders equivalent based on upload characteristics
      -- Users with free slots and low queues are like well-seeded torrents
      seedersEquivalent =
        let slotBonus = if sacHasFreeUploadSlot candidate then 5 else 0
            queuePenalty = min 5 (sacQueueLength candidate `div` 10)
            speedBonus = min 5 (sacUploadSpeed candidate `div` 100000)
         in max 1 (slotBonus - queuePenalty + speedBonus)
      -- Convert SlskdFile from Slskd.Types to Indexer.Types
      convertFile :: SlskdFile -> Indexer.SlskdFile
      convertFile f = Indexer.SlskdFile
        { Indexer.sfFilename = sfFilename f
        , Indexer.sfSize = sfSize f
        , Indexer.sfBitRate = sfBitRate f
        , Indexer.sfSampleRate = sfSampleRate f
        , Indexer.sfBitDepth = sfBitDepth f
        , Indexer.sfLength = sfLength f
        , Indexer.sfIsLocked = sfIsLocked f
        }
   in ReleaseInfo
        { riTitle = title
        , riGuid = Just $ sacUsername candidate <> ":" <> sacDirectory candidate
        , riDownloadUrl = "" -- Will be populated during submission
        , riInfoUrl = Nothing
        , riSize = Just $ sacTotalSize candidate
        , riPublishDate = Nothing
        , riCategory = Just 3000 -- Audio category
        , riSeeders = Just seedersEquivalent
        , riPeers = Nothing
        , riGrabs = Nothing
        , riDownloadType = Slskd
        , riQuality = quality
        , riSlskdUsername = Just $ sacUsername candidate
        , riSlskdFiles = Just $ map convertFile (sacFiles candidate)
        }
