{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Clusters API handlers.
module Skema.API.Handlers.Clusters
  ( clustersServer
  ) where

import Skema.API.Types.Clusters (ClustersAPI, ClusterResponse(..), ClustersResponse(..), ClustersPagination(..), ClustersStats(..), ClusterWithTracksResponse(..), ClusterTrackInfo(..), MBTrackInfo(..), CandidateRelease(..), AssignReleaseRequest(..), UpdateTrackRecordingRequest(..), CreateClusterRequest(..), SetLockRequest(..))
import Skema.API.Types.Tasks (TaskRequest(..), TaskResponse(..), TaskResource(..))
import qualified Skema.Services.TaskManager as TM
import Skema.API.Handlers.Utils (throw400, throw404, throw500, parsePagination)
import Skema.Database.Connection
import qualified Skema.Database.Repository as DB
import qualified Skema.Database.Types as DBTypes
import Skema.API.Converters (clusterToResponse)
import Skema.FileSystem.Utils (osPathToString)
import Skema.MusicBrainz.Client (getRelease, searchReleases, searchRecordings)
import Skema.Services.Identifier (identifyClusterNow)
import Skema.MusicBrainz.Types
import Skema.Events.Bus (publishAndLog)
import Skema.Events.Types (Event(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson (eitherDecode)
import Skema.Clock (getNow)
import Servant
import Skema.API.App (Env(..), AppM, toServiceContext)
import Katip

-- | Convert an MBRelease to a CandidateRelease for the API response.
mbReleaseToCandidateRelease :: MBRelease -> CandidateRelease
mbReleaseToCandidateRelease release = CandidateRelease
  { candidateReleaseId = unMBID (mbReleaseId release)
  , candidateTitle = mbReleaseTitle release
  , candidateArtist = mbReleaseArtist release
  , candidateDate = mbReleaseDate release
  , candidateCountry = mbReleaseCountry release
  , candidateTrackCount = sum (map mbMediumTrackCount (mbReleaseMedia release))
  , candidateConfidence = 0.0
  , candidateBarcode = mbReleaseBarcode release
  , candidateLabel = mbReleaseLabel release
  , candidateCatalogNumber = mbReleaseCatalogNumber release
  }

-- | Clusters API handlers.
clustersServer :: Env -> ServerT ClustersAPI AppM
clustersServer env@Env{envLogEnv = le, envEventBus = bus, envTaskManager = tm, envDbPool = connPool, envMBClient = mbClient, envClock = clock} =
  taskHandler
  :<|> getAllClustersHandler
  :<|> getClusterHandler
  :<|> getCandidatesHandler
  :<|> assignReleaseHandler
  :<|> removeReleaseHandler
  :<|> setLockHandler
  :<|> updateTrackRecordingHandler
  :<|> searchReleasesHandler
  :<|> searchRecordingsHandler
  :<|> createClusterHandler
  :<|> deleteClusterHandler
  where
    taskHandler :: TaskRequest -> AppM TaskResponse
    taskHandler req = do
      let clusterId = fromMaybe 0 (taskRequestResourceId req)
      case taskRequestType req of
        "identify" -> TM.startTask tm ClustersResource (Just clusterId) "identify" $ \taskId -> do
          result <- identifyClusterNow (toServiceContext env) clusterId $ \progress msg ->
            TM.updateTaskProgress tm taskId progress (Just msg)
          case result of
            Left err -> TM.failTask tm taskId err
            Right payload -> TM.completeTask tm taskId (Just payload)
        _ -> throw400 "Unknown task type"

    getAllClustersHandler :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> AppM ClustersResponse
    getAllClustersHandler maybeOffset maybeLimit maybeSearch maybeFilter maybeSortField maybeOrder = do
      let (offset, limit) = parsePagination maybeOffset maybeLimit
      let searchQuery = maybeSearch
      let filterStatus = maybeFilter  -- "all", "matched", "unmatched", or "locked"
      let sortField = fromMaybe "album" maybeSortField  -- "album", "artist", "confidence", "status", "track_count"
      let sortOrder = fromMaybe "asc" maybeOrder  -- "asc" or "desc"

      liftIO $ withConnection connPool $ \conn -> do
        allClusters <- DB.getAllClusters conn

        -- For each cluster, get the first track's metadata
        clusterResponses <- forM allClusters $ \cluster -> do
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

        -- Apply filtering
        let filtered = case filterStatus of
              Just "matched" -> filter (\c -> isJust (clusterResponseMBReleaseId c)) clusterResponses
              Just "unmatched" -> filter (\c -> isNothing (clusterResponseMBReleaseId c)) clusterResponses
              Just "locked" -> filter clusterResponseMatchLocked clusterResponses
              _ -> clusterResponses

        -- Apply search
        let searched = case searchQuery of
              Nothing -> filtered
              Just query -> let queryLower = T.toLower query
                            in filter (\c ->
                              matchText queryLower (clusterResponseAlbum c) ||
                              matchText queryLower (clusterResponseAlbumArtist c) ||
                              matchText queryLower (clusterResponseMBReleaseTitle c) ||
                              matchText queryLower (clusterResponseMBReleaseArtist c)
                            ) filtered

        -- Apply sorting
        let sorted = sortBy (compareClustersByField sortField sortOrder) searched

        let total = length sorted
        let paginated = take limit $ drop offset $ sorted

        -- Global stats over ALL clusters, independent of filter/search/page
        let matchedCount = length $ filter (isJust . clusterResponseMBReleaseId) clusterResponses
            lockedCount = length $ filter clusterResponseMatchLocked clusterResponses

        pure $ ClustersResponse
          { clustersResponsePagination = ClustersPagination
              { clustersPaginationTotal = total
              , clustersPaginationOffset = offset
              , clustersPaginationLimit = limit
              }
          , clustersResponseClusters = paginated
          , clustersResponseStats = ClustersStats
              { clustersStatsTotal = length clusterResponses
              , clustersStatsMatched = matchedCount
              , clustersStatsUnmatched = length clusterResponses - matchedCount
              , clustersStatsLocked = lockedCount
              }
          }
      where
        matchText :: Text -> Maybe Text -> Bool
        matchText query maybeText = case maybeText of
          Nothing -> False
          Just text -> query `T.isInfixOf` T.toLower text

        compareClustersByField :: Text -> Text -> ClusterResponse -> ClusterResponse -> Ordering
        compareClustersByField field order a b =
          let cmp = case field of
                "album" -> compare (clusterResponseAlbum a <|> clusterResponseMBReleaseTitle a)
                                   (clusterResponseAlbum b <|> clusterResponseMBReleaseTitle b)
                "artist" -> compare (clusterResponseAlbumArtist a <|> clusterResponseMBReleaseArtist a)
                                    (clusterResponseAlbumArtist b <|> clusterResponseMBReleaseArtist b)
                "confidence" -> compare (fromMaybe (-1) (clusterResponseMBConfidence a))
                                        (fromMaybe (-1) (clusterResponseMBConfidence b))
                "status" -> compare (if isJust (clusterResponseMBReleaseId a) then 1 :: Int else 0)
                                    (if isJust (clusterResponseMBReleaseId b) then 1 :: Int else 0)
                "track_count" -> compare (clusterResponseTrackCount a) (clusterResponseTrackCount b)
                _ -> EQ
          in if order == "desc" then case cmp of { LT -> GT; GT -> LT; EQ -> EQ } else cmp

    getClusterHandler :: Int64 -> AppM ClusterWithTracksResponse
    getClusterHandler clusterId = do
      maybeResult <- liftIO $ withConnection connPool $ \conn ->
        DB.getClusterWithTracks conn clusterId
      case maybeResult of
        Nothing -> throw404 $ "Cluster not found: " <> show clusterId
        Just (cluster, tracks) -> do
          trackInfos <- liftIO $ forM tracks $ \(trackId, path, metadata, mbRecId, mbRecTitle) -> do
            pathStr <- osPathToString path
            pure $ ClusterTrackInfo
              { clusterTrackId = trackId
              , clusterTrackPath = toText pathStr
              , clusterTrackTitle = DBTypes.metaTitle metadata
              , clusterTrackArtist = DBTypes.metaArtist metadata
              , clusterTrackTrackNumber = DBTypes.metaTrackNumber metadata
              , clusterTrackDiscNumber = DBTypes.metaDiscNumber metadata
              , clusterTrackDuration = DBTypes.metaDurationSeconds metadata
              , clusterTrackFormat = DBTypes.metaFormat metadata
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

    getCandidatesHandler :: Int64 -> AppM [CandidateRelease]
    getCandidatesHandler clusterId = do
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

    assignReleaseHandler :: Int64 -> AssignReleaseRequest -> AppM ClusterResponse
    assignReleaseHandler clusterId req = do
      let mbEnv = mbClient
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

      -- Note: Catalog albums are now linked to clusters implicitly via release_group_mbid
      -- No explicit linkage update needed - the JOIN happens at query time

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

    removeReleaseHandler :: Int64 -> AppM NoContent
    removeReleaseHandler clusterId = do
      liftIO $ withConnection connPool $ \conn ->
        DB.clearClusterRelease conn clusterId False
      pure NoContent

    setLockHandler :: Int64 -> SetLockRequest -> AppM ClusterResponse
    setLockHandler clusterId req = do
      let locked = setLockLocked req
      -- Verify the cluster exists (and, when locking, that it has a match to lock)
      maybeCluster <- liftIO $ withConnection connPool $ \conn ->
        DB.getClusterById conn clusterId
      case maybeCluster of
        Nothing -> throw404 $ "Cluster not found: " <> show clusterId
        Just cluster -> do
          when (locked && isNothing (DBTypes.clusterMBReleaseId cluster)) $
            throw500 "Cannot lock a cluster without a match"
          liftIO $ withConnection connPool $ \conn ->
            DB.setClusterMatchLock conn clusterId locked
          -- Return the updated cluster
          maybeResult <- liftIO $ withConnection connPool $ \conn ->
            DB.getClusterWithTracks conn clusterId
          case maybeResult of
            Nothing -> throw404 $ "Cluster not found: " <> show clusterId
            Just (updated, tracks) -> do
              let firstTrackMeta = case viaNonEmpty head tracks of
                    Nothing -> Nothing
                    Just (_, _, metadata, _, _) -> Just metadata
              pure $ clusterToResponse updated firstTrackMeta

    updateTrackRecordingHandler :: Int64 -> Int64 -> UpdateTrackRecordingRequest -> AppM NoContent
    updateTrackRecordingHandler clusterId trackId req = do
      let recordingId = updateRecordingId req
      let recordingTitle = updateRecordingTitle req
      trackCount <- liftIO $ withConnection connPool $ \conn -> do
        DB.setTrackRecording conn trackId recordingId recordingTitle
        DB.countClusterTracks conn clusterId

      -- Emit TracksRematched event
      liftIO $ publishAndLog bus le "api.clusters" $ TracksRematched
        { rematchedClusterId = clusterId
        , rematchedTrackCount = trackCount
        }

      pure NoContent

    searchReleasesHandler :: Text -> Maybe Int -> AppM [CandidateRelease]
    searchReleasesHandler query maybeLimit = do
      let mbEnv = mbClient
      let limit = fromMaybe 10 maybeLimit

      -- Search for releases (user input - use dismax for better relevance)
      searchResult <- liftIO $ searchReleases mbEnv query (Just limit) Nothing True
      case searchResult of
        Left err -> throw500 $ "MusicBrainz search failed: " <> show err
        Right searchResp -> do
          -- Convert MBRelease to CandidateRelease
          pure $ map mbReleaseToCandidateRelease (mbSearchReleases searchResp)

    searchRecordingsHandler :: Text -> Maybe Int -> AppM [MBTrackInfo]
    searchRecordingsHandler query maybeLimit = do
      let mbEnv = mbClient
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

    createClusterHandler :: CreateClusterRequest -> AppM ClusterResponse
    createClusterHandler req = do
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
      getClusterHandler newClusterId >>= \resp ->
        pure (clusterWithTracksCluster resp)

    deleteClusterHandler :: Int64 -> Maybe Int64 -> AppM NoContent
    deleteClusterHandler clusterId maybeMergeInto = do

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
                now <- getNow clock
                DB.moveTracksToCluster conn targetClusterId trackIds now

            -- Unassign tracks from cluster
            Nothing ->
              liftIO $ withConnection connPool $ \conn ->
                DB.unassignTracksFromCluster conn trackIds

          -- Delete the cluster
          liftIO $ withConnection connPool $ \conn ->
            DB.deleteCluster conn clusterId

          pure NoContent

