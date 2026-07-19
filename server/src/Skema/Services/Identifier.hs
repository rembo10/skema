{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Identifier service - identifies albums using MusicBrainz.
module Skema.Services.Identifier
  ( startIdentifierService
  , identifyClusterNow
  , clusterToFileGroup
  ) where

import Skema.Services.Types (ServiceContext(..))
import Skema.Services.EventLoop (JobMode(..), runEventService)
import Skema.Events.Bus
import Skema.Events.Types
import Skema.Database.Connection
import Skema.Database.Repository (getAllClusters, getClusterById, getClusterWithTracks, updateClusterWithMBData, updateClusterWithCandidates, updateTrackCluster, clearClusterRelease, setTrackRecordingByPath, clusterNeedsIdentification, metadataRecordToMonatone)
import Skema.Database.Types (ClusterRecord(..))
import Skema.MusicBrainz.Identify (identifyFileGroup)
import Skema.Domain.Identification (mkIdentifyConfig, buildFileGroup)
import Skema.Domain.Matching (FileGroup(..), ReleaseMatch(..), TrackMatch(..), IdentificationResult(..))
import Skema.MusicBrainz.Types (MBID(..), unMBID, MBRelease(..), MBTrack(..))
import Skema.Config.Types (Config(..))
import Skema.FileSystem.Utils (osPathToString)
import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.STM as STM
import Control.Monad (foldM)
import Skema.Clock (getNow)
import qualified Data.Aeson as Aeson
import qualified Database.SQLite.Simple as SQLite
import Katip

-- | Start the identifier service.
--
-- This service listens for CLUSTERS_GENERATED events and identifies them with
-- MusicBrainz. Identification runs sequentially: concurrent runs would write
-- competing matches, and MusicBrainz is rate limited to one request per
-- second anyway.
-- Returns the async handle for graceful shutdown.
startIdentifierService :: ServiceContext -> IO (Async ())
startIdentifierService deps =
  runEventService (scLogEnv deps) (scEventBus deps) "services.identifier" Sequential $ \case
    ClustersGenerated{ affectedClusterIds = affectedIds } ->
      Just $ handleClustersGenerated deps affectedIds
    _ -> Nothing

-- | Handle a clusters generated event.
handleClustersGenerated :: ServiceContext -> [Int64] -> IO ()
handleClustersGenerated ServiceContext{..} affectedIds = do
  let le = scLogEnv
  let pool = scDbPool
  let bus = scEventBus
  let initialContext = ()
  let initialNamespace = "services.identifier"

  -- Read current config from TVar
  config <- STM.atomically $ STM.readTVar scConfigVar

  runKatipContextT le initialContext initialNamespace $ do
    if null affectedIds
      then do
        $(logTM) InfoS $ logStr ("All albums already matched, skipping identification" :: Text)

        -- Emit IdentificationComplete with 0 matches
        liftIO $ publishAndLog bus le "identifier" $ IdentificationComplete
          { groupsProcessed = 0
          , matchesFound = 0
          }
      else do
        $(logTM) InfoS $ logStr $ ("Identifying " <> show (length affectedIds) <> " albums with MusicBrainz" :: Text)

        -- Configure identification (before using it to filter clusters)
        let identifyConfig = mkIdentifyConfig (library config) (musicbrainz config)

        -- Emit IdentificationStarted
        liftIO $ publishAndLog bus le "identifier" $ IdentificationStarted
          { groupCount = length affectedIds
          }

        -- Get clusters that need identification, scoped to affected clusters
        now <- liftIO (getNow scClock)

        clusters <- liftIO $ withConnection pool $ \conn -> do
          scopedClusters <- if null affectedIds
            then getAllClusters conn  -- Fallback: load all (shouldn't happen normally)
            else catMaybes <$> mapM (getClusterById conn) affectedIds
          -- Pure: Use domain logic to filter clusters needing identification
          -- Skip locked clusters (manually assigned matches)
          pure $ filter (\c -> not (clusterMatchLocked c) && clusterNeedsIdentification identifyConfig now c) scopedClusters

        -- Convert clusters to FileGroups
        fileGroups <- liftIO $ withConnection pool $ \conn -> do
          catMaybes <$> forM clusters (clusterToFileGroup conn)

        $(logTM) InfoS $ logStr $ ("Converted " <> show (length fileGroups) <> " clusters to file groups" :: Text)

        -- Use injected MusicBrainz client (shared for rate limiting)
        let mbEnv = scMBClientEnv

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
                          Just _rgId -> pure ()  -- Catalog album linking is handled by migration/overview query
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
                          pathStr <- osPathToString filePath
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

                      -- Emit LIBRARY_ARTIST_FOUND event for the primary artist only
                      -- This prevents featured/secondary artists from being auto-followed
                      case mbReleaseArtistId release of
                        Just artistId -> do
                          publishAndLog bus le "identifier" $ LibraryArtistFound
                            { foundArtistMBID = unMBID artistId
                            , foundArtistName = mbReleaseArtist release
                            , foundClusterId = cid
                            , foundReleaseGroupId = releaseGroupId
                            }
                        Nothing -> pure ()  -- No primary artist (shouldn't happen)

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

-- | Convert a cluster record to a FileGroup.
-- Returns Nothing if the cluster has no id or no tracks.
--
-- IO responsibility is limited to fetching tracks from the database.
-- The aggregation logic lives in 'Skema.Domain.Identification.buildFileGroup'.
clusterToFileGroup :: SQLite.Connection -> ClusterRecord -> IO (Maybe FileGroup)
clusterToFileGroup conn cluster = case clusterId cluster of
  Nothing -> pure Nothing
  Just cid -> do
    result <- getClusterWithTracks conn cid
    pure $ case result of
      Nothing -> Nothing
      Just (_, tracks) ->
        buildFileGroup (clusterAlbum cluster) (clusterAlbumArtist cluster)
          (map (\(_, p, meta, _, _) -> (p, metadataRecordToMonatone meta)) tracks)

-- | Identify a single cluster on demand (the API's "identify" task).
--
-- Clears any existing match, re-runs identification against MusicBrainz,
-- persists either the match (plus per-track recording ids, emitting
-- TracksRematched for diff regeneration) or the candidate list for manual
-- selection. Progress is reported through the callback; the returned value
-- is a task-result payload, or a failure message.
identifyClusterNow
  :: ServiceContext
  -> Int64                        -- ^ Cluster id
  -> (Double -> Text -> IO ())    -- ^ Progress reporter
  -> IO (Either Text Aeson.Value)
identifyClusterNow ServiceContext{..} cid report = do
  let le = scLogEnv
      pool = scDbPool
      bus = scEventBus
  config <- readTVarIO scConfigVar

  maybeCluster <- withConnection pool $ \conn -> getClusterById conn cid
  case maybeCluster of
    Nothing -> pure $ Left $ "Cluster not found: " <> show cid
    Just cluster -> do
      let identifyConfig = mkIdentifyConfig (library config) (musicbrainz config)

      -- Remove current match to unlock
      when (isJust $ clusterMBReleaseId cluster) $
        withConnection pool $ \conn -> clearClusterRelease conn cid True

      report 0.2 "Converting cluster to file group..."
      maybeFileGroup <- withConnection pool $ \conn -> clusterToFileGroup conn cluster
      case maybeFileGroup of
        Nothing -> pure $ Left "Failed to convert cluster to file group"
        Just fileGroup -> do
          report 0.4 "Identifying with MusicBrainz..."
          result <- identifyFileGroup le scMBClientEnv identifyConfig fileGroup
          case result of
            Left err -> pure $ Left $ "MusicBrainz error: " <> show err

            Right (IdentificationResult Nothing candidatesWithScores) -> do
              report 0.8 "Saving candidates..."
              -- No match above the confidence threshold: save candidates for
              -- manual selection
              let candidates = map fst candidatesWithScores
              withConnection pool $ \conn ->
                updateClusterWithCandidates conn cid candidates
              pure $ Right $ Aeson.toJSON $ Aeson.object
                [ "message" Aeson..= ("No confident match found. Saved " <> show (length candidates) <> " candidates for manual selection." :: Text)
                , "candidates_count" Aeson..= length candidates
                ]

            Right (IdentificationResult (Just match) _) -> do
              report 0.7 "Saving match..."
              withConnection pool $ \conn -> do
                updateClusterWithMBData conn cid (rmRelease match) (rmConfidence match) (rmCandidates match)

                -- Save track matches
                forM_ (rmTrackMatches match) $ \tm -> do
                  let recording = tmTrack tm
                  pathStr <- osPathToString (tmFilePath tm)
                  setTrackRecordingByPath conn (toText pathStr)
                    (unMBID (mbTrackRecordingId recording)) (Just (mbTrackTitle recording))

              -- Emit TracksRematched event (for diff regeneration)
              let trackCount = length (rmTrackMatches match)
              publishAndLog bus le "identifier" $ TracksRematched
                { rematchedClusterId = cid
                , rematchedTrackCount = trackCount
                }

              pure $ Right $ Aeson.toJSON $ Aeson.object
                [ "message" Aeson..= ("Successfully identified cluster with " <> show trackCount <> " tracks" :: Text)
                , "release_id" Aeson..= unMBID (mbReleaseId (rmRelease match))
                , "confidence" Aeson..= rmConfidence match
                , "track_matches" Aeson..= trackCount
                ]
