{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Clusters API handlers.
module Skema.API.Handlers.Clusters
  ( clustersServer
  ) where

import Skema.API.Types.Clusters (ClustersAPI, ClusterResponse(..), ClusterWithTracksResponse(..), ClusterTrackInfo(..), MBTrackInfo(..), CandidateRelease(..), AssignReleaseRequest(..), UpdateTrackRecordingRequest(..), CreateClusterRequest(..))
import Skema.API.Handlers.Auth (throwJsonError)
import Skema.Auth (requireAuth)
import Skema.Auth.JWT (JWTSecret)
import Skema.Database.Connection
import qualified Skema.Database.Repository as DB
import qualified Skema.Database.Types as DBTypes
import qualified Skema.Config.Types as Cfg
import Skema.Domain.Converters (clusterToResponse)
import Skema.Services.Registry (ServiceRegistry(..))
import Skema.MusicBrainz.Client (getRelease, searchReleases, searchRecordings)
import Skema.MusicBrainz.Identify (identifyFileGroup)
import Skema.Services.Identifier (clusterToFileGroup)
import Skema.MusicBrainz.Types
import Skema.Domain.Identification (IdentifyConfig(..))
import Skema.Events.Bus (EventBus, publishAndLog)
import Skema.Events.Types (Event(..))
import qualified System.OsPath as OP
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson (eitherDecode)
import Data.Time (getCurrentTime)
import Database.SQLite.Simple (Only(..))
import qualified Control.Concurrent.STM as STM
import Servant
import Katip

-- | Throw a 404 Not Found error.
throw404 :: Text -> Handler a
throw404 = throwJsonError err404

-- | Throw a 500 Internal Server Error.
throw500 :: Text -> Handler a
throw500 = throwJsonError err500

-- | Clusters API handlers.
clustersServer :: LogEnv -> EventBus -> Cfg.ServerConfig -> JWTSecret -> ServiceRegistry -> ConnectionPool -> TVar Cfg.Config -> Server ClustersAPI
clustersServer le bus _serverCfg jwtSecret registry connPool configVar = \maybeAuthHeader ->
  getAllClustersHandler maybeAuthHeader
  :<|> getClusterHandler maybeAuthHeader
  :<|> getCandidatesHandler maybeAuthHeader
  :<|> assignReleaseHandler maybeAuthHeader
  :<|> removeReleaseHandler maybeAuthHeader
  :<|> updateTrackRecordingHandler maybeAuthHeader
  :<|> reidentifyClusterHandler maybeAuthHeader
  :<|> searchReleasesHandler maybeAuthHeader
  :<|> searchRecordingsHandler maybeAuthHeader
  :<|> createClusterHandler maybeAuthHeader
  :<|> deleteClusterHandler maybeAuthHeader
  where
    getAllClustersHandler :: Maybe Text -> Handler [ClusterResponse]
    getAllClustersHandler authHeader = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection connPool $ \conn -> do
        clusters <- DB.getAllClusters conn
        -- For each cluster, get the first track's metadata
        forM clusters $ \cluster -> do
          firstTrackMeta <- case DBTypes.clusterId cluster of
            Nothing -> pure Nothing
            Just cid -> do
              result <- DB.getClusterWithTracks conn cid
              pure $ case result of
                Nothing -> Nothing
                Just (_,tracks) -> case viaNonEmpty head tracks of
                  Nothing -> Nothing
                  Just (_, _, metadata, _, _) -> Just metadata
          pure $ clusterToResponse cluster firstTrackMeta

    getClusterHandler :: Maybe Text -> Int64 -> Handler ClusterWithTracksResponse
    getClusterHandler authHeader clusterId = do
      _ <- requireAuth configVar jwtSecret authHeader
      maybeResult <- liftIO $ withConnection connPool $ \conn ->
        DB.getClusterWithTracks conn clusterId
      case maybeResult of
        Nothing -> throw404 $ "Cluster not found: " <> show clusterId
        Just (cluster, tracks) -> do
          trackInfos <- liftIO $ forM tracks $ \(trackId, path, metadata, mbRecId, mbRecTitle) -> do
            pathStr <- OP.decodeUtf path
            pure $ ClusterTrackInfo
              { clusterTrackId = trackId
              , clusterTrackPath = toText pathStr
              , clusterTrackTitle = DBTypes.metaTitle metadata
              , clusterTrackArtist = DBTypes.metaArtist metadata
              , clusterTrackTrackNumber = DBTypes.metaTrackNumber metadata
              , clusterTrackDiscNumber = DBTypes.metaDiscNumber metadata
              , clusterTrackDuration = DBTypes.metaDurationSeconds metadata
              , clusterTrackMBRecordingId = mbRecId
              , clusterTrackMBRecordingTitle = mbRecTitle
              }

          -- Get first track's metadata
          -- Extract MusicBrainz tracks from cached release data
          let mbTracks = case DBTypes.clusterMBReleaseData cluster of
                Nothing -> Nothing
                Just jsonText -> case eitherDecode (BSL.fromStrict $ TE.encodeUtf8 jsonText) of
                  Left _ -> Nothing
                  Right (release :: MBRelease) ->
                    let allTracks = concatMap (\medium ->
                          map (\track -> MBTrackInfo
                                { mbTrackInfoPosition = mbTrackPosition track
                                , mbTrackInfoTitle = mbTrackTitle track
                                , mbTrackInfoLength = mbTrackLength track
                                , mbTrackInfoRecordingId = unMBID (mbTrackRecordingId track)
                                , mbTrackInfoArtist = mbTrackArtist track
                                , mbTrackInfoDiscNumber = mbMediumPosition medium
                                }) (mbMediumTracks medium)
                          ) (mbReleaseMedia release)
                    in Just allTracks

          let firstTrackMeta = case viaNonEmpty head tracks of
                Nothing -> Nothing
                Just (_, _, metadata, _, _) -> Just metadata
          let clusterResp = clusterToResponse cluster firstTrackMeta
          pure $ ClusterWithTracksResponse
            { clusterWithTracksCluster = clusterResp
            , clusterWithTracksTracks = trackInfos
            , clusterWithTracksMBTracks = mbTracks
            }

    getCandidatesHandler :: Maybe Text -> Int64 -> Handler [CandidateRelease]
    getCandidatesHandler authHeader clusterId = do
      _ <- requireAuth configVar jwtSecret authHeader
      maybeCluster <- liftIO $ withConnection connPool $ \conn ->
        DB.getClusterById conn clusterId
      case maybeCluster of
        Nothing -> throw404 $ "Cluster not found: " <> show clusterId
        Just cluster -> case DBTypes.clusterMBCandidates cluster of
          Nothing -> pure []  -- No candidates cached
          Just jsonText -> case eitherDecode (BSL.fromStrict $ TE.encodeUtf8 jsonText) of
            Left err -> do
              -- Log the error and return 500
              liftIO $ runKatipContextT le () "api" $ do
                $(logTM) ErrorS $ logStr $ ("Failed to decode candidates JSON: " <> show err <> ", JSON length: " <> show (T.length jsonText) :: Text)
              throw500 $ "Failed to decode candidates: " <> toText err
            Right releases ->
              -- Convert MBRelease to CandidateRelease format
              pure $ map mbReleaseToCandidateRelease releases
      where
        mbReleaseToCandidateRelease :: MBRelease -> CandidateRelease
        mbReleaseToCandidateRelease release = CandidateRelease
          { candidateReleaseId = unMBID (mbReleaseId release)
          , candidateTitle = mbReleaseTitle release
          , candidateArtist = mbReleaseArtist release
          , candidateDate = mbReleaseDate release
          , candidateCountry = mbReleaseCountry release
          , candidateTrackCount = length (mbReleaseTracks release)
          , candidateConfidence = 0.0  -- We don't have individual candidate confidence scores
          , candidateBarcode = mbReleaseBarcode release
          , candidateLabel = mbReleaseLabel release
          , candidateCatalogNumber = mbReleaseCatalogNumber release
          }

    assignReleaseHandler :: Maybe Text -> Int64 -> AssignReleaseRequest -> Handler ClusterResponse
    assignReleaseHandler authHeader clusterId req = do
      _ <- requireAuth configVar jwtSecret authHeader
      let mbEnv = srMBClientEnv registry
      let idText = assignReleaseId req

      liftIO $ runKatipContextT le () "api.assign_release" $ do
        $(logTM) InfoS $ logStr $ ("Assigning release " <> idText <> " to cluster " <> show clusterId :: Text)

      -- Fetch the release from MusicBrainz
      releaseResult <- liftIO $ getRelease mbEnv (MBID idText)
      release <- case releaseResult of
        Right r -> pure r
        Left err -> throw500 $ "Failed to fetch release from MusicBrainz: " <> show err

      -- Update cluster with the full release (will cache all details and LOCK the match)
      let confidence = fromMaybe 1.0 (assignConfidence req)  -- Manual assignments get high confidence
      liftIO $ withConnection connPool $ \conn ->
        DB.updateClusterWithMBDataManual conn clusterId release confidence

      -- Link catalog albums to this cluster based on release group ID
      -- First lookup catalog album by MBID, then update using internal ID
      case mbReleaseGroupId release of
        Just releaseGroupId -> do
          maybeAlbum <- liftIO $ withConnection connPool $ \conn ->
            DB.getCatalogAlbumByReleaseGroupMBID conn (unMBID releaseGroupId)
          case maybeAlbum of
            Just album | isNothing (DBTypes.catalogAlbumMatchedClusterId album) -> do
              -- Album exists and isn't already matched to another cluster
              case DBTypes.catalogAlbumId album of
                Just albumId -> liftIO $ withConnection connPool $ \conn -> do
                  now <- getCurrentTime
                  executeQuery conn
                    "UPDATE catalog_albums SET matched_cluster_id = ?, updated_at = ? WHERE id = ?"
                    (clusterId, now, albumId)
                Nothing -> pure ()  -- Album has no ID (shouldn't happen)
            _ -> pure ()  -- Album doesn't exist or already matched
        Nothing -> pure ()

      -- Get updated cluster with tracks
      maybeResult <- liftIO $ withConnection connPool $ \conn ->
        DB.getClusterWithTracks conn clusterId
      case maybeResult of
        Nothing -> throw404 $ "Cluster not found: " <> show clusterId
        Just (cluster, tracks) -> do
          -- Get first track's metadata
          let firstTrackMeta = case viaNonEmpty head tracks of
                Nothing -> Nothing
                Just (_, _, metadata, _, _) -> Just metadata
          pure $ clusterToResponse cluster firstTrackMeta

    removeReleaseHandler :: Maybe Text -> Int64 -> Handler NoContent
    removeReleaseHandler authHeader clusterId = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection connPool $ \conn -> do
        now <- getCurrentTime
        -- Remove release assignment by setting MB fields to NULL and unlocking
        executeQuery conn
          "UPDATE clusters SET mb_release_id = NULL, mb_release_group_id = NULL, mb_confidence = NULL, match_source = NULL, match_locked = 0, updated_at = ? WHERE id = ?"
          (now, clusterId)
      pure NoContent

    updateTrackRecordingHandler :: Maybe Text -> Int64 -> Int64 -> UpdateTrackRecordingRequest -> Handler NoContent
    updateTrackRecordingHandler authHeader clusterId trackId req = do
      _ <- requireAuth configVar jwtSecret authHeader
      let recordingId = updateRecordingId req
      let recordingTitle = updateRecordingTitle req
      trackCount <- liftIO $ withConnection connPool $ \conn -> do
        -- Update the track's recording mapping
        executeQuery conn
          "UPDATE library_tracks SET mb_recording_id = ?, mb_recording_title = ? WHERE id = ?"
          (Just recordingId, recordingTitle, trackId)
        -- Get track count for the cluster
        results <- queryRows conn "SELECT COUNT(*) FROM library_tracks WHERE cluster_id = ?" (Only clusterId)
        pure $ case results of
          ((Only count :: Only Int) : _) -> count
          [] -> 0

      -- Emit TracksRematched event
      liftIO $ publishAndLog bus le "api.clusters" $ TracksRematched
        { rematchedClusterId = clusterId
        , rematchedTrackCount = trackCount
        }

      pure NoContent

    reidentifyClusterHandler :: Maybe Text -> Int64 -> Handler ClusterResponse
    reidentifyClusterHandler authHeader clusterId = do
      _ <- requireAuth configVar jwtSecret authHeader

      -- Get the cluster
      maybeCluster <- liftIO $ withConnection connPool $ \conn ->
        DB.getClusterById conn clusterId
      cluster <- case maybeCluster of
        Nothing -> throw404 $ "Cluster not found: " <> show clusterId
        Just c -> pure c

      -- Read current config
      config <- liftIO $ STM.atomically $ STM.readTVar configVar
      let mbEnv = srMBClientEnv registry
      let libConfig = Cfg.library config

      -- Build identify config
      let identifyConfig = IdentifyConfig
            { cfgMaxCandidates = 5
            , cfgMinConfidence = 0.35
            , cfgSearchLimit = 20
            , cfgNormalizeFeaturing = Cfg.libraryNormalizeFeaturing libConfig
            , cfgNormalizeFeaturingTo = Cfg.libraryNormalizeFeaturingTo libConfig
            , cfgRetryIntervalHours = 24
            }

      -- Remove current match to unlock
      when (isJust $ DBTypes.clusterMBReleaseId cluster) $ do
        liftIO $ withConnection connPool $ \conn -> do
          now <- getCurrentTime
          executeQuery conn
            "UPDATE clusters SET mb_release_id = NULL, mb_release_group_id = NULL, mb_confidence = NULL, match_source = NULL, match_locked = 0, mb_candidates = NULL, updated_at = ? WHERE id = ?"
            (now, clusterId)

      -- Convert cluster to FileGroup
      maybeFileGroup <- liftIO $ withConnection connPool $ \conn ->
        clusterToFileGroup conn cluster
      fileGroup <- case maybeFileGroup of
        Nothing -> throw500 "Failed to convert cluster to file group"
        Just fg -> pure fg

      -- Identify the cluster
      result <- liftIO $ identifyFileGroup le mbEnv identifyConfig fileGroup

      -- Process result
      case result of
        Left err -> throw500 $ "MusicBrainz error: " <> show err
        Right (IdentificationResult Nothing candidatesWithScores) -> do
          -- No match found above confidence threshold - save candidates for manual selection
          let candidates = map fst candidatesWithScores
          liftIO $ withConnection connPool $ \conn ->
            DB.updateClusterWithCandidates conn clusterId candidates
          -- Return updated cluster
          getAllClustersHandler authHeader >>= \clusters ->
            case find (\c -> clusterResponseId c == clusterId) clusters of
              Nothing -> throw404 "Cluster not found after reidentification"
              Just c -> pure c

        Right (IdentificationResult (Just match) _) -> do
          -- Match found - save it
          liftIO $ withConnection connPool $ \conn -> do
            let release = rmRelease match
            let confidence = rmConfidence match
            let candidates = rmCandidates match
            DB.updateClusterWithMBData conn clusterId release confidence candidates

            -- Save track matches
            forM_ (rmTrackMatches match) $ \tm -> do
              let filePath = tmFilePath tm
                  recording = tmTrack tm
                  recordingId = unMBID (mbTrackRecordingId recording)
                  recordingTitle = mbTrackTitle recording
              pathStr <- OP.decodeUtf filePath
              executeQuery conn
                "UPDATE library_tracks SET mb_recording_id = ?, mb_recording_title = ? WHERE path = ?"
                (Just recordingId, Just recordingTitle, toText pathStr)

          -- Emit TracksRematched event (for diff regeneration)
          let trackCount = length (rmTrackMatches match)
          liftIO $ publishAndLog bus le "api.clusters" $ TracksRematched
            { rematchedClusterId = clusterId
            , rematchedTrackCount = trackCount
            }

          -- Emit ClusterIdentified event
          let releaseId = unMBID (mbReleaseId $ rmRelease match)
          let releaseGroupId = mbReleaseGroupId (rmRelease match) >>= Just . unMBID
          liftIO $ runKatipContextT le () "api.reidentify" $ do
            $(logTM) InfoS $ logStr $ ("Re-identified cluster " <> show clusterId <> " to release " <> releaseId :: Text)

          -- Return updated cluster
          getAllClustersHandler authHeader >>= \clusters ->
            case find (\c -> clusterResponseId c == clusterId) clusters of
              Nothing -> throw404 "Cluster not found after reidentification"
              Just c -> pure c

    searchReleasesHandler :: Maybe Text -> Text -> Maybe Int -> Handler [CandidateRelease]
    searchReleasesHandler authHeader query maybeLimit = do
      _ <- requireAuth configVar jwtSecret authHeader
      let mbEnv = srMBClientEnv registry
      let limit = fromMaybe 10 maybeLimit

      -- Search for releases
      searchResult <- liftIO $ searchReleases mbEnv query (Just limit) Nothing
      case searchResult of
        Left err -> throw500 $ "MusicBrainz search failed: " <> show err
        Right searchResp -> do
          -- Convert MBRelease to CandidateRelease
          pure $ map mbReleaseToCandidateRelease (mbSearchReleases searchResp)
      where
        mbReleaseToCandidateRelease :: MBRelease -> CandidateRelease
        mbReleaseToCandidateRelease release = CandidateRelease
          { candidateReleaseId = unMBID (mbReleaseId release)
          , candidateTitle = mbReleaseTitle release
          , candidateArtist = mbReleaseArtist release
          , candidateDate = mbReleaseDate release
          , candidateCountry = mbReleaseCountry release
          , candidateTrackCount = length (mbReleaseTracks release)
          , candidateConfidence = 0.0  -- Search results don't have confidence scores
          , candidateBarcode = mbReleaseBarcode release
          , candidateLabel = mbReleaseLabel release
          , candidateCatalogNumber = mbReleaseCatalogNumber release
          }

    searchRecordingsHandler :: Maybe Text -> Text -> Maybe Int -> Handler [MBTrackInfo]
    searchRecordingsHandler authHeader query maybeLimit = do
      _ <- requireAuth configVar jwtSecret authHeader
      let mbEnv = srMBClientEnv registry
      let limit = fromMaybe 25 maybeLimit

      -- Search for recordings
      searchResult <- liftIO $ searchRecordings mbEnv query (Just limit) Nothing
      case searchResult of
        Left err -> throw500 $ "MusicBrainz search failed: " <> show err
        Right searchResp -> do
          -- Convert MBRecording to MBTrackInfo
          pure $ zipWith mbRecordingToTrackInfo [1..] (mbSearchRecordings searchResp)
      where
        mbRecordingToTrackInfo :: Int -> MBRecording -> MBTrackInfo
        mbRecordingToTrackInfo idx recording = MBTrackInfo
          { mbTrackInfoPosition = idx
          , mbTrackInfoTitle = mbRecordingTitle recording
          , mbTrackInfoLength = mbRecordingLength recording
          , mbTrackInfoRecordingId = unMBID (mbRecordingId recording)
          , mbTrackInfoArtist = Nothing  -- Recording search doesn't include artist
          , mbTrackInfoDiscNumber = 1
          }

    createClusterHandler :: Maybe Text -> CreateClusterRequest -> Handler ClusterResponse
    createClusterHandler authHeader req = do
      _ <- requireAuth configVar jwtSecret authHeader
      let trackIds = createClusterTrackIds req
      let album = createClusterAlbum req
      let albumArtist = createClusterAlbumArtist req

      when (null trackIds) $
        throw500 "Cannot create cluster with no tracks"

      newClusterId <- liftIO $ withConnection connPool $ \conn -> do
        -- Compute cluster hash
        let hash = DB.computeClusterHash album albumArtist (length trackIds)

        -- Create new cluster
        cid <- DB.createCluster conn hash album albumArtist (length trackIds)

        -- Move tracks to new cluster
        DB.updateTrackCluster conn cid trackIds

        pure cid

      -- Return the new cluster
      getClusterHandler authHeader newClusterId >>= \resp ->
        pure (clusterWithTracksCluster resp)

    deleteClusterHandler :: Maybe Text -> Int64 -> Maybe Int64 -> Handler NoContent
    deleteClusterHandler authHeader clusterId maybeMergeInto = do
      _ <- requireAuth configVar jwtSecret authHeader

      -- Get all tracks in this cluster
      maybeResult <- liftIO $ withConnection connPool $ \conn ->
        DB.getClusterWithTracks conn clusterId

      case maybeResult of
        Nothing -> throwError err404
        Just (_, tracks) -> do
          let trackIds = map (\(tid, _, _, _, _) -> tid) tracks

          case maybeMergeInto of
            -- Merge into another cluster
            Just targetClusterId -> do
              -- Verify target cluster exists
              maybeTarget <- liftIO $ withConnection connPool $ \conn ->
                DB.getClusterById conn targetClusterId
              when (isNothing maybeTarget) $
                throwError err404

              -- Move all tracks and update counts
              liftIO $ withConnection connPool $ \conn -> do
                DB.updateTrackCluster conn targetClusterId trackIds
                now <- getCurrentTime
                executeQuery conn
                  "UPDATE clusters SET track_count = track_count + ?, updated_at = ? WHERE id = ?"
                  (length trackIds, now, targetClusterId)

            -- Unassign tracks from cluster
            Nothing ->
              liftIO $ withConnection connPool $ \conn ->
                forM_ trackIds $ \tid ->
                  executeQuery conn
                    "UPDATE library_tracks SET cluster_id = NULL WHERE id = ?"
                    (Only tid)

          -- Delete the cluster
          liftIO $ withConnection connPool $ \conn ->
            executeQuery conn
              "DELETE FROM clusters WHERE id = ?"
              (Only clusterId)

          pure NoContent
