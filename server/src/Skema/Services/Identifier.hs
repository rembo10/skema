{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Identifier service - identifies albums using MusicBrainz.
module Skema.Services.Identifier
  ( startIdentifierService
  , clusterToFileGroup
  ) where

import Skema.Services.Dependencies (IdentifierDeps(..))
import Skema.Services.Common (metadataRecordToMonatone)
import Skema.Events.Bus
import Skema.Events.Types
import Skema.Database.Connection
import Skema.Database.Repository (getAllClusters, getClusterWithTracks, updateClusterWithMBData, updateClusterLastIdentified, updateClusterWithCandidates, updateTrackCluster, getTrackedArtistByMBID, getCatalogAlbumByReleaseGroupMBID)
import Skema.Database.Types (ClusterRecord(..), LibraryTrackMetadataRecord(..))
import qualified Skema.Database.Types as DBTypes
import Skema.MusicBrainz.Identify (identifyFileGroup)
import Skema.Domain.Identification (IdentifyConfig(..), shouldRetryIdentification)
import Skema.MusicBrainz.Types (FileGroup(..), ReleaseMatch(..), TrackMatch(..), IdentificationResult(..), MBID(..), unMBID, MBRelease(..), MBTrack(..))
import Skema.Config.Types (Config(..), LibraryConfig(..))
import qualified Monatone.Metadata as M
import System.OsPath (OsPath, takeDirectory)
import qualified System.OsPath as OP
import Control.Concurrent.Async (Async, async)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (readTChan)
import Control.Monad (foldM)
import Control.Exception (try)
import Data.Time (getCurrentTime, UTCTime)
import qualified Database.SQLite.Simple as SQLite
import Katip

-- | Start the identifier service.
--
-- This service listens for CLUSTERS_GENERATED events and identifies them with MusicBrainz.
-- Returns the async handle for graceful shutdown.
startIdentifierService :: IdentifierDeps -> IO (Async ())
startIdentifierService deps = do
  chan <- STM.atomically $ subscribe (identEventBus deps)
  async $ forever $ do
    envelope <- STM.atomically $ readTChan chan
    case envelopeEvent envelope of
      ClustersGenerated{ needsIdentification = count } -> do
        -- Process each event asynchronously with error handling
        _ <- async $ do
          result <- try $ handleClustersGenerated deps count
          case result of
            Left (e :: SomeException) -> do
              let le = identLogEnv deps
              runKatipContextT le () "identifier.error" $ do
                $(logTM) ErrorS $ logStr $ ("Exception identifying clusters: " <> show e :: Text)
            Right () -> pure ()
        pure ()
      _ -> pure ()  -- Ignore other events

-- | Handle a clusters generated event.
handleClustersGenerated :: IdentifierDeps -> Int -> IO ()
handleClustersGenerated IdentifierDeps{..} groupsNeedingId = do
  let le = identLogEnv
  let pool = identDbPool
  let bus = identEventBus
  let initialContext = ()
  let initialNamespace = "services.identifier"

  -- Read current config from TVar
  config <- STM.atomically $ STM.readTVar identConfigVar

  runKatipContextT le initialContext initialNamespace $ do
    if groupsNeedingId == 0
      then do
        $(logTM) InfoS $ logStr ("All albums already matched, skipping identification" :: Text)

        -- Emit IdentificationComplete with 0 matches
        liftIO $ publishAndLog bus le "identifier" $ IdentificationComplete
          { groupsProcessed = 0
          , matchesFound = 0
          }
      else do
        $(logTM) InfoS $ logStr $ ("Identifying " <> show groupsNeedingId <> " albums with MusicBrainz" :: Text)

        -- Configure identification (before using it to filter clusters)
        let libConfig = library config
        let identifyConfig = IdentifyConfig
              { cfgMaxCandidates = 5
              , cfgMinConfidence = 0.35  -- Lowered from 0.5 with improved scoring algorithm
              , cfgSearchLimit = 20
              , cfgNormalizeFeaturing = libraryNormalizeFeaturing libConfig
              , cfgNormalizeFeaturingTo = libraryNormalizeFeaturingTo libConfig
              , cfgRetryIntervalHours = 24  -- Retry after 24 hours
              }

        -- Emit IdentificationStarted
        liftIO $ publishAndLog bus le "identifier" $ IdentificationStarted
          { groupCount = groupsNeedingId
          }

        -- Get clusters that need identification (using pure domain logic)
        now <- liftIO getCurrentTime

        clusters <- liftIO $ withConnection pool $ \conn -> do
          allClusters <- getAllClusters conn
          -- Pure: Use domain logic to filter clusters needing identification
          -- Skip locked clusters (manually assigned matches)
          pure $ filter (\c -> not (clusterMatchLocked c) && clusterNeedsIdentification identifyConfig now c) allClusters

        -- Convert clusters to FileGroups
        fileGroups <- liftIO $ withConnection pool $ \conn -> do
          catMaybes <$> forM clusters (clusterToFileGroup conn)

        $(logTM) InfoS $ logStr $ ("Converted " <> show (length fileGroups) <> " clusters to file groups" :: Text)

        -- Use injected MusicBrainz client (shared for rate limiting)
        let mbEnv = identMBClient

        -- Process each file group individually and emit events immediately
        -- This allows downstream services to start processing while we're still identifying
        $(logTM) InfoS $ logStr ("Identifying and processing albums one at a time..." :: Text)

        matchCount <- liftIO $ foldM
          (\count (idx, (fg, cluster)) -> do
            let albumName = fromMaybe "<no album>" (fgAlbum fg)
            let artistName = fromMaybe "<no artist>" (fgArtist fg)
            let _trackCount = length (fgFiles fg)

            runKatipContextT le () "identifier" $ do
              $(logTM) InfoS $ logStr $ ("[" <> show idx <> "/" <> show (length fileGroups) <> "] Identifying: " <>
                albumName <> " by " <> artistName :: Text)

            -- Emit progress
            publishAndLog bus le "identifier" $ IdentificationProgress
              { currentAlbum = albumName
              , currentArtist = artistName
              , albumsProcessed = idx - 1
              , totalAlbums = length fileGroups
              }

            -- Identify this single album
            result <- identifyFileGroup le mbEnv identifyConfig fg

            runKatipContextT le () "identifier" $ do
              case result of
                Left err ->
                  $(logTM) ErrorS $ logStr $ ("[" <> show idx <> "] API error: " <> show err :: Text)
                Right (IdentificationResult Nothing candidates) ->
                  $(logTM) WarningS $ logStr $ ("[" <> show idx <> "] No match found (" <> show (length candidates) <> " candidates below threshold)" :: Text)
                Right (IdentificationResult (Just match) _) ->
                  $(logTM) InfoS $ logStr $ ("[" <> show idx <> "] Matched (confidence: " <>
                    show (round (rmConfidence match * 100) :: Integer) <> "%)" :: Text)

            -- Process this result immediately
            processed <- (case result of
                Right (IdentificationResult (Just match) _) -> do
                  -- Persist this match in its own connection
                  withConnection pool $ \conn -> do
                    -- Update cluster with MusicBrainz data (including cached release details)
                    case clusterId cluster of
                      Just cid -> do
                        let release = rmRelease match
                        let confidence = rmConfidence match
                        let candidates = rmCandidates match

                        updateClusterWithMBData conn cid release confidence candidates

                        -- Link catalog album to this cluster if it exists
                        case mbReleaseGroupId release of
                          Just rgId -> do
                            let releaseGroupMBID = unMBID rgId
                            maybeCatalogAlbum <- getCatalogAlbumByReleaseGroupMBID conn releaseGroupMBID
                            case maybeCatalogAlbum of
                              Just catalogAlbum -> do
                                -- Only update if not already linked
                                updateTime <- getCurrentTime
                                executeQuery conn
                                  "UPDATE catalog_albums SET matched_cluster_id = ?, updated_at = ? WHERE id = ? AND matched_cluster_id IS NULL"
                                  (cid, updateTime, DBTypes.catalogAlbumId catalogAlbum)
                              Nothing -> pure ()  -- No catalog album exists for this release group
                          Nothing -> pure ()  -- No release group ID in the match

                        -- Get track IDs for this cluster
                        matchResult <- getClusterWithTracks conn cid
                        let trackIds = case matchResult of
                              Just (_, trackList) -> map (\(tid, _, _, _, _) -> tid) trackList
                              Nothing -> []

                        -- Link tracks to cluster (already done, but ensure it's set)
                        updateTrackCluster conn cid trackIds

                        -- Save track→recording mappings from munkres matching
                        let trackMatches = rmTrackMatches match
                        forM_ trackMatches $ \tm -> do
                          let filePath = tmFilePath tm
                              recording = tmTrack tm
                              recordingId = unMBID (mbTrackRecordingId recording)
                              recordingTitle = mbTrackTitle recording
                          -- Update the track's mb_recording_id and mb_recording_title
                          pathStr <- OP.decodeUtf filePath
                          executeQuery conn
                            "UPDATE library_tracks SET mb_recording_id = ?, mb_recording_title = ? WHERE path = ?"
                            (Just recordingId, Just recordingTitle, toText pathStr)

                      Nothing -> pure ()

                  -- Emit events AFTER persistence (outside withConnection)
                  case clusterId cluster of
                    Just cid -> do
                      let release = rmRelease match
                      let releaseId = unMBID (mbReleaseId release)
                      let releaseGroupId = mbReleaseGroupId release >>= Just . unMBID
                      let confidence = case viaNonEmpty head (rmTrackMatches match) of
                            Just tm -> tmConfidence tm
                            Nothing -> 0.8
                      let numTracks = length (fgFiles fg)

                      -- Emit CLUSTER_IDENTIFIED event (will trigger diff generation)
                      publishAndLog bus le "identifier" $ ClusterIdentified
                        { identifiedClusterId = cid
                        , identifiedReleaseId = releaseId
                        , identifiedReleaseGroupId = releaseGroupId
                        , identifiedConfidence = confidence
                        , identifiedTrackCount = numTracks
                        }

                      -- Emit LIBRARY_ARTIST_FOUND events for NEW artists only
                      -- This handles collaborative albums (e.g., Jay-Z & Kanye West)
                      let artists = mbReleaseArtists release

                      -- Filter to only new artists (not already tracked)
                      newArtists <- withConnection pool $ \conn ->
                        filterM (\(artistId, _) -> do
                          let mbid = unMBID artistId
                          maybeTracked <- getTrackedArtistByMBID conn mbid
                          pure $ isNothing maybeTracked
                        ) artists

                      -- Emit event for each NEW artist
                      forM_ newArtists $ \(artistId, artName) -> do
                        publishAndLog bus le "identifier" $ LibraryArtistFound
                          { foundArtistMBID = unMBID artistId
                          , foundArtistName = artName
                          , foundClusterId = cid
                          , foundReleaseGroupId = releaseGroupId
                          }

                      pure True  -- Successfully processed
                    Nothing -> pure False

                Right (IdentificationResult Nothing candidatesWithScores) -> do
                  -- No match found above threshold - save candidates for manual selection
                  let candidates = map fst candidatesWithScores
                  withConnection pool $ \conn ->
                    case clusterId cluster of
                      Just cid -> updateClusterWithCandidates conn cid candidates
                      Nothing -> pure ()
                  pure False

                Left _ -> pure False)  -- Error case, skip

            pure $ if processed then count + 1 else count)
          (0 :: Int)
          (zip [1..] (zip fileGroups clusters))

        -- Emit IdentificationComplete event
        liftIO $ publishAndLog bus le "identifier" $ IdentificationComplete
          { groupsProcessed = length fileGroups
          , matchesFound = matchCount
          }

-- | Check if a cluster needs identification (delegates to pure domain logic).
clusterNeedsIdentification :: IdentifyConfig -> UTCTime -> ClusterRecord -> Bool
clusterNeedsIdentification config now cluster =
  shouldRetryIdentification config now (clusterMBReleaseId cluster) (clusterLastIdentifiedAt cluster)

-- | Convert a cluster record to a FileGroup.
-- Returns Nothing if the cluster has no tracks.
clusterToFileGroup :: SQLite.Connection -> ClusterRecord -> IO (Maybe FileGroup)
clusterToFileGroup conn cluster = do
  case clusterId cluster of
    Nothing -> pure Nothing
    Just cid -> do
      result <- getClusterWithTracks conn cid
      case result of
        Nothing -> pure Nothing
        Just (_, tracks) -> do
          if null tracks
            then pure Nothing
            else do
              -- Convert tracks to (OsPath, Metadata) format
              let filesWithMetadata = map trackToFileMetadata tracks

              -- Use first track for directory
              case viaNonEmpty head tracks of
                Nothing -> pure Nothing  -- Should never happen due to null check above
                Just (_, firstPath, _, _, _) -> do
                  let dir = takeDirectory firstPath

                  -- Extract metadata from all tracks - use first non-Nothing value for each field
                  -- This ensures we use available metadata even if it's only on one track
                  let allMeta = map (\(_, _, meta, _, _) -> meta) tracks

                  -- Find first non-Nothing value for each field
                  let findFirst f = viaNonEmpty head $ mapMaybe f allMeta
                  let findFirstMBId selector = do
                        meta <- viaNonEmpty head allMeta
                        let mbIds = M.musicBrainzIds (metadataRecordToMonatone meta)
                        mbid <- selector mbIds
                        pure (MBID mbid)

                  -- Get metadata from cluster and search through all tracks for additional context
                  pure $ Just FileGroup
                    { fgDirectory = dir
                    , fgAlbum = clusterAlbum cluster
                    , fgArtist = clusterAlbumArtist cluster
                    , fgReleaseId = findFirstMBId M.mbReleaseId
                    , fgReleaseGroupId = findFirstMBId M.mbReleaseGroupId
                    , fgLabel = findFirst metaLabel
                    , fgCatalogNumber = findFirst metaCatalogNumber
                    , fgBarcode = findFirst metaBarcode
                    , fgCountry = findFirst metaCountry
                    , fgDate = findFirst metaDate
                    , fgFiles = filesWithMetadata
                    }

-- | Convert a track with metadata to (OsPath, Metadata) format.
trackToFileMetadata :: (Int64, OsPath, LibraryTrackMetadataRecord, Maybe Text, Maybe Text) -> (OsPath, M.Metadata)
trackToFileMetadata (_, path, meta, _, _) = (path, metadataRecordToMonatone meta)
