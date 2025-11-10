{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Clusters API handlers.
module Skema.API.Handlers.Clusters
  ( clustersServer
  ) where

import Skema.API.Types.Clusters (ClustersAPI, ClusterResponse(..), ClusterWithTracksResponse(..), ClusterTrackInfo(..), CandidateRelease(..), AssignReleaseRequest(..))
import Skema.API.Handlers.Auth (throwJsonError)
import Skema.Auth (requireAuth)
import Skema.Auth.JWT (JWTSecret)
import Skema.Database.Connection
import qualified Skema.Database.Repository as DB
import qualified Skema.Database.Types as DBTypes
import qualified Skema.Config.Types as Cfg
import Skema.Domain.Converters (clusterToResponse)
import Skema.Services.Registry (ServiceRegistry(..))
import Skema.MusicBrainz.Client (getRelease)
import Skema.MusicBrainz.Types
import qualified System.OsPath as OP
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson (eitherDecode)
import Data.Time (getCurrentTime)
import Servant
import Katip

-- | Throw a 404 Not Found error.
throw404 :: Text -> Handler a
throw404 = throwJsonError err404

-- | Throw a 500 Internal Server Error.
throw500 :: Text -> Handler a
throw500 = throwJsonError err500

-- | Clusters API handlers.
clustersServer :: LogEnv -> Cfg.ServerConfig -> JWTSecret -> ServiceRegistry -> ConnectionPool -> TVar Cfg.Config -> Server ClustersAPI
clustersServer le _serverCfg jwtSecret registry connPool configVar = \maybeAuthHeader ->
  getAllClustersHandler maybeAuthHeader
  :<|> getClusterHandler maybeAuthHeader
  :<|> getCandidatesHandler maybeAuthHeader
  :<|> assignReleaseHandler maybeAuthHeader
  :<|> removeReleaseHandler maybeAuthHeader
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
                  Just (_, _, metadata) -> Just metadata
          pure $ clusterToResponse cluster firstTrackMeta

    getClusterHandler :: Maybe Text -> Int64 -> Handler ClusterWithTracksResponse
    getClusterHandler authHeader clusterId = do
      _ <- requireAuth configVar jwtSecret authHeader
      maybeResult <- liftIO $ withConnection connPool $ \conn ->
        DB.getClusterWithTracks conn clusterId
      case maybeResult of
        Nothing -> throw404 $ "Cluster not found: " <> show clusterId
        Just (cluster, tracks) -> do
          trackInfos <- liftIO $ forM tracks $ \(trackId, path, metadata) -> do
            pathStr <- OP.decodeUtf path
            pure $ ClusterTrackInfo
              { clusterTrackId = trackId
              , clusterTrackPath = toText pathStr
              , clusterTrackTitle = DBTypes.metaTitle metadata
              , clusterTrackArtist = DBTypes.metaArtist metadata
              , clusterTrackTrackNumber = DBTypes.metaTrackNumber metadata
              , clusterTrackDiscNumber = DBTypes.metaDiscNumber metadata
              , clusterTrackDuration = DBTypes.metaDurationSeconds metadata
              }

          -- Get first track's metadata
          let firstTrackMeta = case viaNonEmpty head tracks of
                Nothing -> Nothing
                Just (_, _, metadata) -> Just metadata
          let clusterResp = clusterToResponse cluster firstTrackMeta
          pure $ ClusterWithTracksResponse
            { clusterWithTracksCluster = clusterResp
            , clusterWithTracksTracks = trackInfos
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
          }

    assignReleaseHandler :: Maybe Text -> Int64 -> AssignReleaseRequest -> Handler ClusterResponse
    assignReleaseHandler authHeader clusterId req = do
      _ <- requireAuth configVar jwtSecret authHeader
      let mbEnv = srMBClientEnv registry

      -- Fetch the release from MusicBrainz to get full details for caching
      let releaseId = MBID (assignReleaseId req)
      releaseResult <- liftIO $ getRelease mbEnv releaseId
      release <- case releaseResult of
        Left err -> throw500 $ "Failed to fetch release from MusicBrainz: " <> show err
        Right r -> pure r

      -- Update cluster with the full release (will cache all details in database)
      let confidence = fromMaybe 0.8 (assignConfidence req)
      liftIO $ withConnection connPool $ \conn ->
        DB.updateClusterWithMBData conn clusterId release confidence []  -- No candidates when manually assigning

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
                Just (_, _, metadata) -> Just metadata
          pure $ clusterToResponse cluster firstTrackMeta

    removeReleaseHandler :: Maybe Text -> Int64 -> Handler NoContent
    removeReleaseHandler authHeader clusterId = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection connPool $ \conn -> do
        now <- getCurrentTime
        -- Remove release assignment by setting MB fields to NULL
        executeQuery conn
          "UPDATE clusters SET mb_release_id = NULL, mb_release_group_id = NULL, mb_confidence = NULL, updated_at = ? WHERE id = ?"
          (now, clusterId)
      pure NoContent
