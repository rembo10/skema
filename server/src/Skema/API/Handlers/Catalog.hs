{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Catalog API handlers.
module Skema.API.Handlers.Catalog
  ( catalogServer
  ) where

import Skema.API.Types.Catalog (CatalogAPI, CatalogQueryRequest(..), CatalogQueryResponse(..), CatalogArtistResponse(..), ArtistsPagination(..), ArtistsResponse(..), CatalogAlbumResponse(..), CreateCatalogArtistRequest(..), UpdateCatalogArtistRequest(..), CreateCatalogAlbumRequest(..), UpdateCatalogAlbumRequest(..), CatalogTaskRequest(..), CatalogAlbumOverviewResponse(..), AlbumOverviewPagination(..), AlbumOverviewStats(..), AlbumOverviewResponse(..), BulkAlbumActionRequest(..), BulkAlbumAction(..), AlbumState(..), ActiveDownloadInfo(..), AlbumReleasesResponse(..), ReleaseResponse(..), SlskdFileResponse(..), ReleaseStreamEvent(..))
import Skema.API.Types.Common (SourceIO)
import Skema.Auth (checkAuthEnabled)
import Skema.Auth.JWT (validateJWT)
import Skema.API.Types.Tasks (TaskResponse(..), TaskResource(..))
import Skema.Core.TaskManager (TaskManager)
import qualified Skema.Core.TaskManager as TM
import qualified Skema.Core.Catalog as Core
import Skema.API.Handlers.Auth (throwJsonError)
import Skema.Auth (requireAuth)
import Skema.Auth.JWT (JWTSecret)
import Skema.Database.Connection
import qualified Skema.Database.Repository as DB
import qualified Skema.Database.Types as DBTypes
import Database.SQLite.Simple (Only(..))
import qualified Skema.Config.Types as Cfg
import Skema.Services.Registry (ServiceRegistry(..))
import Skema.MusicBrainz.Client (searchArtists, searchReleaseGroups, prettyClientError)
import Skema.MusicBrainz.Types (mbasArtists, mbasArtistId, mbrgsReleaseGroups, mbrgsReleaseGroupId, unMBID)
import Skema.Domain.Converters (mbArtistSearchToCatalogResponse, mbReleaseGroupSearchToCatalogResponse)
import Skema.Events.Bus (EventBus)
import qualified Skema.Events.Bus as EventBus
import qualified Skema.Events.Types as Events
import Skema.Indexer.Client (searchIndexer)
import Skema.Indexer.Types (SearchQuery(..), ReleaseInfo(..), IndexerResult(..), DownloadType(..))
import qualified Skema.Indexer.Types as Indexer
import Skema.Slskd.Client (createSlskdClient)
import Skema.Slskd.Search (searchSlskd, searchSlskdStreaming, slskdCandidateToReleaseInfo)
import Skema.Domain.Quality (qualityToText, textToQuality)
import qualified Skema.Domain.Quality as Qual
import Control.Concurrent.Async (async, mapConcurrently, race, concurrently)
import Control.Concurrent (threadDelay)
import Data.Aeson (toJSON, object, (.=))
import Servant hiding (SourceIO)
import Servant.Types.SourceT (SourceT(..), StepT(..))
import Katip
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan, readTChan)
import qualified Data.IORef as IORef
import Data.Time (UTCTime, getCurrentTime, diffUTCTime)
import qualified Data.Text as Text
import qualified Control.Concurrent.STM as STM

-- | Throw a 400 Bad Request error.
throw400 :: Text -> Handler a
throw400 = throwJsonError err400

-- | Throw a 404 Not Found error.
throw404 :: Text -> Handler a
throw404 = throwJsonError err404

-- | Throw a 500 Internal Server Error.
throw500 :: Text -> Handler a
throw500 = throwJsonError err500

-- | Catalog API handlers.
catalogServer :: LogEnv -> EventBus -> Cfg.ServerConfig -> JWTSecret -> ServiceRegistry -> TaskManager -> ConnectionPool -> FilePath -> TVar Cfg.Config -> Server CatalogAPI
catalogServer le bus _serverCfg jwtSecret registry tm connPool _cacheDir configVar = \maybeAuthHeader ->
  taskHandler maybeAuthHeader
  :<|> queryHandler maybeAuthHeader
  :<|> getArtistsHandler maybeAuthHeader
  :<|> createArtistHandler maybeAuthHeader
  :<|> updateArtistHandler maybeAuthHeader
  :<|> deleteArtistHandler maybeAuthHeader
  :<|> albumOverviewHandler maybeAuthHeader
  :<|> createAlbumHandler maybeAuthHeader
  :<|> updateAlbumHandler maybeAuthHeader
  :<|> deleteAlbumHandler maybeAuthHeader
  :<|> searchAlbumReleasesHandler maybeAuthHeader
  :<|> streamAlbumReleasesHandler
  :<|> bulkActionHandler maybeAuthHeader
  where
    taskHandler :: Maybe Text -> CatalogTaskRequest -> Handler TaskResponse
    taskHandler authHeader req = do
      _ <- requireAuth configVar jwtSecret authHeader

      -- Create task based on request type
      case catalogTaskType req of
        "refresh" -> case catalogTaskArtistId req of
          Nothing -> throwError err400 { errBody = "Artist ID required for refresh task" }
          Just artistId -> liftIO $ do
            -- Create the task
            taskResp <- TM.createTask tm CatalogResource (Just artistId) "refresh"
            let taskId = taskResponseId taskResp

            -- Spawn async worker to execute the refresh
            _ <- async $ do
              -- Get artist by ID to find MBID
              maybeArtist <- withConnection connPool $ \conn -> do
                artists <- DB.getCatalogArtists conn Nothing Nothing Nothing Nothing
                pure $ find (\a -> DBTypes.catalogArtistId a == Just artistId) artists
              case maybeArtist of
                Nothing -> do
                  TM.failTask tm taskId $ "Artist not found: " <> show artistId
                Just artist -> do
                  TM.updateTaskProgress tm taskId 0.3 (Just "Emitting refresh event...")
                  -- Emit refresh event
                  EventBus.publishAndLog bus le "api.catalog.task" $ Events.CatalogArtistRefreshRequested
                    { Events.refreshArtistMBID = DBTypes.catalogArtistMBID artist
                    }
                  TM.completeTask tm taskId (Just $ toJSON $ object
                    [ "message" .= ("Artist catalog refresh requested for: " <> DBTypes.catalogArtistName artist :: Text)
                    , "artist_mbid" .= DBTypes.catalogArtistMBID artist
                    ])

            pure taskResp

        "refresh_all" -> liftIO $ do
          -- Create the task
          taskResp <- TM.createTask tm CatalogResource Nothing "refresh_all"
          let taskId = taskResponseId taskResp

          -- Spawn async worker to execute the refresh
          _ <- async $ do
            TM.updateTaskProgress tm taskId 0.2 (Just "Fetching followed artists...")
            -- Get all followed artists
            followedArtists <- withConnection connPool $ \conn ->
              DB.getCatalogArtists conn (Just True) Nothing Nothing Nothing

            let totalArtists = length followedArtists
            TM.updateTaskProgress tm taskId 0.4 (Just $ "Emitting refresh events for " <> show totalArtists <> " artists...")

            -- Emit refresh event for each
            forM_ followedArtists $ \artist ->
              EventBus.publishAndLog bus le "api.catalog.task" $ Events.CatalogArtistRefreshRequested
                { Events.refreshArtistMBID = DBTypes.catalogArtistMBID artist
                }

            TM.completeTask tm taskId (Just $ toJSON $ object
              [ "message" .= ("Catalog refresh requested for " <> show totalArtists <> " artists" :: Text)
              , "artists_count" .= totalArtists
              ])

          pure taskResp

        _ -> throwError err400 { errBody = "Unknown task type" }
    -- Universal search handler - searches both artists and albums
    queryHandler :: Maybe Text -> CatalogQueryRequest -> Handler CatalogQueryResponse
    queryHandler authHeader req = do
      _ <- requireAuth configVar jwtSecret authHeader
      let mbEnv = srMBClientEnv registry
      let query = catalogQueryQuery req
      let limit = fromMaybe 10 (catalogQueryLimit req)

      -- Search artists and albums in parallel
      (artistResults, albumResults) <- liftIO $ do
        artistSearchResult <- searchArtists mbEnv query (Just limit) Nothing
        albumSearchResult <- searchReleaseGroups mbEnv query (Just limit) Nothing
        pure (artistSearchResult, albumSearchResult)

      -- Convert search results to catalog responses
      artists <- case artistResults of
        Left err -> throw500 $ "MusicBrainz artist search failed: " <> prettyClientError err
        Right searchResp -> do
          -- Check which artists are already in catalog
          catalogArtists <- liftIO $ withConnection connPool $ \conn ->
            mapM (\result -> DB.getCatalogArtistByMBID conn (unMBID $ mbasArtistId result))
                 (mbasArtists searchResp)

          pure $ zipWith mbArtistSearchToCatalogResponse (mbasArtists searchResp) catalogArtists

      albums <- case albumResults of
        Left err -> throw500 $ "MusicBrainz release-group search failed: " <> prettyClientError err
        Right searchResp -> do
          -- Check which albums are already in catalog
          catalogAlbums <- liftIO $ withConnection connPool $ \conn ->
            mapM (\result -> DB.getCatalogAlbumByReleaseGroupMBID conn (unMBID $ mbrgsReleaseGroupId result))
                 (mbrgsReleaseGroups searchResp)

          pure $ zipWith mbReleaseGroupSearchToCatalogResponse (mbrgsReleaseGroups searchResp) catalogAlbums

      pure $ CatalogQueryResponse
        { catalogQueryResponseArtists = artists
        , catalogQueryResponseAlbums = albums
        }

    -- Get catalog artists
    getArtistsHandler :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Text -> Handler ArtistsResponse
    getArtistsHandler authHeader maybeOffset maybeLimit maybeFollowed maybeSearch maybeSort maybeOrder = do
      _ <- requireAuth configVar jwtSecret authHeader

      let offset = fromMaybe 0 maybeOffset
      let limit = fromMaybe 50 maybeLimit

      (allArtists', responses) <- liftIO $ withConnection connPool $ \conn -> do
        allArtists' <- DB.getCatalogArtists conn maybeFollowed maybeSearch maybeSort maybeOrder
        let paginated = take limit $ drop offset allArtists'
        responses <- forM paginated $ \artist -> do
          -- For followed artists, include recent albums
          maybeAlbums <- case (DBTypes.catalogArtistFollowed artist, DBTypes.catalogArtistId artist) of
            (True, Just artistId) -> do
              -- Get all albums for this artist (sorted by release date desc)
              albumRows <- DB.getCatalogAlbumsByArtistOverview conn artistId
              pure $ Just $ map rowToResponse albumRows
            _ -> pure Nothing

          pure $ CatalogArtistResponse
            { catalogArtistResponseId = DBTypes.catalogArtistId artist
            , catalogArtistResponseMBID = DBTypes.catalogArtistMBID artist
            , catalogArtistResponseName = DBTypes.catalogArtistName artist
            , catalogArtistResponseType = DBTypes.catalogArtistType artist
            , catalogArtistResponseImageUrl = DBTypes.catalogArtistImageUrl artist
            , catalogArtistResponseThumbnailUrl = DBTypes.catalogArtistThumbnailUrl artist
            , catalogArtistResponseFollowed = DBTypes.catalogArtistFollowed artist
            , catalogArtistResponseQualityProfileId = DBTypes.catalogArtistQualityProfileId artist
            , catalogArtistResponseScore = Nothing  -- No score in database records
            , catalogArtistResponseCreatedAt = fmap (show :: UTCTime -> Text) (DBTypes.catalogArtistCreatedAt artist)
            , catalogArtistResponseUpdatedAt = fmap (show :: UTCTime -> Text) (DBTypes.catalogArtistUpdatedAt artist)
            , catalogArtistResponseAlbums = maybeAlbums
            }
        pure (allArtists', responses)

      let total = length allArtists'
      let pagination = ArtistsPagination
            { artistsPaginationTotal = total
            , artistsPaginationOffset = offset
            , artistsPaginationLimit = limit
            }

      pure ArtistsResponse
        { artistsResponsePagination = pagination
        , artistsResponseArtists = responses
        }

    -- Create/upsert catalog artist
    createArtistHandler :: Maybe Text -> CreateCatalogArtistRequest -> Handler CatalogArtistResponse
    createArtistHandler authHeader req = do
      _ <- requireAuth configVar jwtSecret authHeader

      -- Skip "Various Artists" - it's not a trackable artist
      when (DBTypes.isVariousArtists (createCatalogArtistMBID req)) $
        throw400 "Cannot add Various Artists to catalog (special compilation artist)"

      _ <- liftIO $ withConnection connPool $ \conn ->
        DB.upsertCatalogArtist
          conn
          (createCatalogArtistMBID req)
          (createCatalogArtistName req)
          (createCatalogArtistType req)
          (createCatalogArtistImageUrl req)
          Nothing  -- thumbnailUrl (will be set by image service)
          (createCatalogArtistFollowed req)
          Nothing  -- addedByRuleId (manual follow, not by rule)
          Nothing  -- sourceClusterId (not from library scan)
          Nothing  -- lastCheckedAt (not checked yet)

      -- Emit event for image fetching if artist is being followed and no image URL provided
      when (createCatalogArtistFollowed req && isNothing (createCatalogArtistImageUrl req)) $ do
        liftIO $ EventBus.publishAndLog bus le "api.catalog" $ Events.CatalogArtistFollowed
          { Events.catalogArtistMBID = createCatalogArtistMBID req
          , Events.catalogArtistName = createCatalogArtistName req
          }

      -- Fetch the created/updated artist
      maybeArtist <- liftIO $ withConnection connPool $ \conn ->
        DB.getCatalogArtistByMBID conn (createCatalogArtistMBID req)

      case maybeArtist of
        Nothing -> throw500 "Failed to retrieve created artist"
        Just artist -> pure $ CatalogArtistResponse
          { catalogArtistResponseId = DBTypes.catalogArtistId artist
          , catalogArtistResponseMBID = DBTypes.catalogArtistMBID artist
          , catalogArtistResponseName = DBTypes.catalogArtistName artist
          , catalogArtistResponseType = DBTypes.catalogArtistType artist
          , catalogArtistResponseImageUrl = DBTypes.catalogArtistImageUrl artist
          , catalogArtistResponseThumbnailUrl = DBTypes.catalogArtistThumbnailUrl artist
          , catalogArtistResponseFollowed = DBTypes.catalogArtistFollowed artist
          , catalogArtistResponseQualityProfileId = DBTypes.catalogArtistQualityProfileId artist
          , catalogArtistResponseScore = Nothing
          , catalogArtistResponseCreatedAt = fmap (show :: UTCTime -> Text) (DBTypes.catalogArtistCreatedAt artist)
          , catalogArtistResponseUpdatedAt = fmap (show :: UTCTime -> Text) (DBTypes.catalogArtistUpdatedAt artist)
          , catalogArtistResponseAlbums = Nothing  -- Not needed for create response
          }

    -- Update catalog artist (followed status and quality profile)
    updateArtistHandler :: Maybe Text -> Int64 -> UpdateCatalogArtistRequest -> Handler CatalogArtistResponse
    updateArtistHandler authHeader artistId req = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection connPool $ \conn ->
        DB.updateCatalogArtist conn artistId
          (updateCatalogArtistFollowed req)
          (updateCatalogArtistQualityProfileId req)

      -- Fetch the updated artist by ID
      maybeArtist <- liftIO $ withConnection connPool $ \conn -> do
        artists <- DB.getCatalogArtists conn Nothing Nothing Nothing Nothing
        pure $ find (\a -> DBTypes.catalogArtistId a == Just artistId) artists

      case maybeArtist of
        Nothing -> throw404 $ "Artist not found: " <> show artistId
        Just artist -> pure $ CatalogArtistResponse
          { catalogArtistResponseId = DBTypes.catalogArtistId artist
          , catalogArtistResponseMBID = DBTypes.catalogArtistMBID artist
          , catalogArtistResponseName = DBTypes.catalogArtistName artist
          , catalogArtistResponseType = DBTypes.catalogArtistType artist
          , catalogArtistResponseImageUrl = DBTypes.catalogArtistImageUrl artist
          , catalogArtistResponseThumbnailUrl = DBTypes.catalogArtistThumbnailUrl artist
          , catalogArtistResponseFollowed = DBTypes.catalogArtistFollowed artist
          , catalogArtistResponseQualityProfileId = DBTypes.catalogArtistQualityProfileId artist
          , catalogArtistResponseScore = Nothing
          , catalogArtistResponseCreatedAt = fmap (show :: UTCTime -> Text) (DBTypes.catalogArtistCreatedAt artist)
          , catalogArtistResponseUpdatedAt = fmap (show :: UTCTime -> Text) (DBTypes.catalogArtistUpdatedAt artist)
          , catalogArtistResponseAlbums = Nothing  -- Not needed for update response
          }

    -- Delete catalog artist
    deleteArtistHandler :: Maybe Text -> Int64 -> Handler NoContent
    deleteArtistHandler authHeader artistId = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection connPool $ \conn ->
        DB.deleteCatalogArtist conn artistId
      pure NoContent

    -- Create/upsert catalog album
    createAlbumHandler :: Maybe Text -> CreateCatalogAlbumRequest -> Handler CatalogAlbumResponse
    createAlbumHandler authHeader req = do
      _ <- requireAuth configVar jwtSecret authHeader

      -- First, ensure the artist exists in catalog_artists (or create it)
      -- Skip Various Artists - albums can still be created without artist entry
      when (DBTypes.isVariousArtists (createCatalogAlbumArtistMBID req)) $
        throw400 "Cannot create catalog album for Various Artists - use a compilation source instead"

      artistId <- liftIO $ withConnection connPool $ \conn -> do
        maybeArtist <- DB.getCatalogArtistByMBID conn (createCatalogAlbumArtistMBID req)
        case maybeArtist of
          Just artist -> case DBTypes.catalogArtistId artist of
            Just aid -> pure aid
            Nothing -> fail "Artist record has no ID"  -- Shouldn't happen, but fail safely
          Nothing -> do
            -- Artist doesn't exist, create it with followed=false
            DB.upsertCatalogArtist
              conn
              (createCatalogAlbumArtistMBID req)
              (createCatalogAlbumArtistName req)
              Nothing  -- artist_type
              Nothing  -- image_url
              Nothing  -- thumbnail_url
              False    -- followed (not automatically following)
              Nothing  -- added_by_rule_id
              Nothing  -- source_cluster_id
              Nothing  -- last_checked_at

      _ <- liftIO $ withConnection connPool $ \conn ->
        DB.upsertCatalogAlbum
          conn
          (createCatalogAlbumReleaseGroupMBID req)
          (createCatalogAlbumTitle req)
          artistId
          (createCatalogAlbumArtistMBID req)
          (createCatalogAlbumArtistName req)
          (createCatalogAlbumType req)
          (createCatalogAlbumFirstReleaseDate req)

      -- Fetch the created/updated album
      maybeAlbum <- liftIO $ withConnection connPool $ \conn ->
        DB.getCatalogAlbumByReleaseGroupMBID conn (createCatalogAlbumReleaseGroupMBID req)

      case maybeAlbum of
        Nothing -> throw500 "Failed to retrieve created album"
        Just album -> do
          -- Compute wanted status using Core.Catalog logic
          maybeProfile <- liftIO $ withConnection connPool $ \conn ->
            case DBTypes.catalogAlbumQualityProfileId album of
              Nothing -> pure Nothing
              Just profileId -> DB.getQualityProfile conn profileId

          let albumContext = Core.AlbumContext
                { Core.acQualityProfile = maybeProfile
                , Core.acCurrentQuality = DBTypes.catalogAlbumCurrentQuality album >>= textToQuality
                , Core.acInLibrary = isJust (DBTypes.catalogAlbumCurrentQuality album)
                , Core.acActiveDownloadStatus = Nothing
                }
              wanted = Core.isAlbumWanted albumContext

          pure $ CatalogAlbumResponse
            { catalogAlbumResponseId = DBTypes.catalogAlbumId album
            , catalogAlbumResponseReleaseGroupMBID = DBTypes.catalogAlbumReleaseGroupMBID album
            , catalogAlbumResponseTitle = DBTypes.catalogAlbumTitle album
            , catalogAlbumResponseArtistMBID = DBTypes.catalogAlbumArtistMBID album
            , catalogAlbumResponseArtistName = DBTypes.catalogAlbumArtistName album
            , catalogAlbumResponseType = DBTypes.catalogAlbumType album
            , catalogAlbumResponseFirstReleaseDate = DBTypes.catalogAlbumFirstReleaseDate album
            , catalogAlbumResponseCoverUrl = DBTypes.catalogAlbumCoverUrl album
            , catalogAlbumResponseCoverThumbnailUrl = DBTypes.catalogAlbumCoverThumbnailUrl album
            , catalogAlbumResponseWanted = wanted
            , catalogAlbumResponseMatchedClusterId = Nothing  -- No longer stored, derived from current_quality
            , catalogAlbumResponseQualityProfileId = DBTypes.catalogAlbumQualityProfileId album
            , catalogAlbumResponseScore = Nothing
            , catalogAlbumResponseCreatedAt = fmap (show :: UTCTime -> Text) (DBTypes.catalogAlbumCreatedAt album)
            , catalogAlbumResponseUpdatedAt = fmap (show :: UTCTime -> Text) (DBTypes.catalogAlbumUpdatedAt album)
            }

    -- Update catalog album (wanted status and quality profile)
    updateAlbumHandler :: Maybe Text -> Int64 -> UpdateCatalogAlbumRequest -> Handler CatalogAlbumResponse
    updateAlbumHandler authHeader albumId req = do
      _ <- requireAuth configVar jwtSecret authHeader

      -- Fetch the album BEFORE updating to get current status
      maybeAlbumBefore <- liftIO $ withConnection connPool $ \conn -> do
        albums <- DB.getCatalogAlbums conn
        pure $ find (\a -> DBTypes.catalogAlbumId a == Just albumId) albums

      case maybeAlbumBefore of
        Nothing -> throw404 $ "Album not found: " <> show albumId
        Just albumBefore -> do
          -- Determine the new quality profile ID
          -- updateCatalogAlbumQualityProfileId is Maybe (Maybe Int64):
          --   Nothing = no change (keep existing)
          --   Just Nothing = clear profile (set to null/unwant)
          --   Just (Just id) = set to specific profile ID
          let newProfileId = case updateCatalogAlbumQualityProfileId req of
                Nothing -> DBTypes.catalogAlbumQualityProfileId albumBefore  -- No change, keep existing
                Just maybeId -> maybeId  -- Use the new value (Nothing to clear, Just id to set)

          -- Get active download info from overview
          maybeAlbumWithDownload <- liftIO $ withConnection connPool $ \conn -> do
            overviews <- DB.getCatalogAlbumsOverview conn DB.defaultAlbumQuery { DB.aqLimit = 1 }
            pure $ find (\row -> DB.caorAlbumId row == albumId) overviews

          let maybeActiveDownloadId = maybeAlbumWithDownload >>= DB.caorActiveDownloadId
              maybeActiveDownloadQuality = maybeAlbumWithDownload >>= DB.caorActiveDownloadQuality >>= textToQuality

          -- Compute wanted status using Core.Catalog logic
          (wasWanted, newWantedStatus, shouldCancel) <- liftIO $ withConnection connPool $ \conn -> do
            -- Get profiles
            maybeOldProfile <- case DBTypes.catalogAlbumQualityProfileId albumBefore of
              Nothing -> pure Nothing
              Just profileId -> DB.getQualityProfile conn profileId

            maybeNewProfile <- case newProfileId of
              Nothing -> pure Nothing
              Just profileId -> DB.getQualityProfile conn profileId

            -- Compute old wanted status
            let oldContext = Core.AlbumContext
                  { Core.acQualityProfile = maybeOldProfile
                  , Core.acCurrentQuality = DBTypes.catalogAlbumCurrentQuality albumBefore >>= textToQuality
                  , Core.acInLibrary = isJust (DBTypes.catalogAlbumCurrentQuality albumBefore)
                  , Core.acActiveDownloadStatus = Nothing
                  }
                wasWanted' = Core.isAlbumWanted oldContext

            -- Compute new wanted status
            let newContext = Core.AlbumContext
                  { Core.acQualityProfile = maybeNewProfile
                  , Core.acCurrentQuality = DBTypes.catalogAlbumCurrentQuality albumBefore >>= textToQuality
                  , Core.acInLibrary = isJust (DBTypes.catalogAlbumCurrentQuality albumBefore)
                  , Core.acActiveDownloadStatus = Nothing
                  }
                newWanted = Core.isAlbumWanted newContext

            -- Check if we should cancel active download using Core logic
            let downloadCtx = Core.DownloadContext
                  { Core.dcAlbumContext = newContext
                  , Core.dcDownloadQuality = fromMaybe Qual.Unknown maybeActiveDownloadQuality
                  , Core.dcNewProfile = maybeNewProfile
                  }
                shouldCancel' = isJust (Core.shouldCancelDownload downloadCtx)

            pure (wasWanted', newWanted, shouldCancel')

          -- Update the quality profile (wanted is now computed, not stored)
          -- Pass Just newProfileId to indicate we want to update the field
          liftIO $ withConnection connPool $ \conn ->
            DB.updateCatalogAlbum conn albumId (Just newProfileId)

          -- Cancel active download if Core logic says we should
          case (shouldCancel, maybeActiveDownloadId) of
            (True, Just downloadId) ->
              liftIO $ withConnection connPool $ \conn ->
                executeQuery conn "DELETE FROM downloads WHERE id = ?" (Only downloadId)
            _ -> pure ()

          -- Emit WantedAlbumAdded event if album is being marked as wanted (was not wanted before)
          when (newWantedStatus && not wasWanted) $ do
            liftIO $ EventBus.publishAndLog bus le "api.catalog" $ Events.WantedAlbumAdded
              { Events.wantedCatalogAlbumId = albumId
              , Events.wantedReleaseGroupId = DBTypes.catalogAlbumReleaseGroupMBID albumBefore
              , Events.wantedAlbumTitle = DBTypes.catalogAlbumTitle albumBefore
              , Events.wantedArtistName = DBTypes.catalogAlbumArtistName albumBefore
              }

          -- Fetch the updated album
          maybeAlbum <- liftIO $ withConnection connPool $ \conn -> do
            albums <- DB.getCatalogAlbums conn
            pure $ find (\a -> DBTypes.catalogAlbumId a == Just albumId) albums

          case maybeAlbum of
            Nothing -> throw404 $ "Album not found: " <> show albumId
            Just album -> do
              -- Fetch overview data for complete state information
              overviewRow <- liftIO $ withConnection connPool $ \conn -> do
                overviews <- DB.getCatalogAlbumsOverview conn DB.defaultAlbumQuery { DB.aqLimit = 1 }
                pure $ find (\row -> DB.caorAlbumId row == albumId) overviews

              -- Re-compute wanted status for the response
              maybeProfile <- liftIO $ withConnection connPool $ \conn ->
                case DBTypes.catalogAlbumQualityProfileId album of
                  Nothing -> pure Nothing
                  Just profileId -> DB.getQualityProfile conn profileId

              let albumContext = Core.AlbumContext
                    { Core.acQualityProfile = maybeProfile
                    , Core.acCurrentQuality = DBTypes.catalogAlbumCurrentQuality album >>= textToQuality
                    , Core.acInLibrary = isJust (DBTypes.catalogAlbumCurrentQuality album)
                    , Core.acActiveDownloadStatus = Nothing
                    }
                  wanted = Core.isAlbumWanted albumContext

              -- Compute album state
              let albumState = case overviewRow of
                    Just row -> rowToAlbumState row
                    Nothing -> if wanted then Wanted else NotWanted

              -- Emit CatalogAlbumUpdated event
              liftIO $ EventBus.publishAndLog bus le "api.catalog" $ Events.CatalogAlbumUpdated
                { Events.updatedAlbumId = albumId
                , Events.updatedAlbumReleaseGroupMBID = DBTypes.catalogAlbumReleaseGroupMBID album
                , Events.updatedAlbumTitle = DBTypes.catalogAlbumTitle album
                , Events.updatedAlbumArtistMBID = DBTypes.catalogAlbumArtistMBID album
                , Events.updatedAlbumArtistName = DBTypes.catalogAlbumArtistName album
                , Events.updatedAlbumType = DBTypes.catalogAlbumType album
                , Events.updatedAlbumFirstReleaseDate = DBTypes.catalogAlbumFirstReleaseDate album
                , Events.updatedAlbumState = show albumState
                , Events.updatedAlbumCurrentQuality = DBTypes.catalogAlbumCurrentQuality album
                , Events.updatedAlbumQualityProfileId = DBTypes.catalogAlbumQualityProfileId album
                , Events.updatedAlbumQualityProfileName = fmap Qual.qfName maybeProfile
                , Events.updatedAlbumCoverUrl = DBTypes.catalogAlbumCoverUrl album
                , Events.updatedAlbumCoverThumbnailUrl = DBTypes.catalogAlbumCoverThumbnailUrl album
                }

              pure $ CatalogAlbumResponse
                { catalogAlbumResponseId = DBTypes.catalogAlbumId album
                , catalogAlbumResponseReleaseGroupMBID = DBTypes.catalogAlbumReleaseGroupMBID album
                , catalogAlbumResponseTitle = DBTypes.catalogAlbumTitle album
                , catalogAlbumResponseArtistMBID = DBTypes.catalogAlbumArtistMBID album
                , catalogAlbumResponseArtistName = DBTypes.catalogAlbumArtistName album
                , catalogAlbumResponseType = DBTypes.catalogAlbumType album
                , catalogAlbumResponseFirstReleaseDate = DBTypes.catalogAlbumFirstReleaseDate album
                , catalogAlbumResponseCoverUrl = DBTypes.catalogAlbumCoverUrl album
                , catalogAlbumResponseCoverThumbnailUrl = DBTypes.catalogAlbumCoverThumbnailUrl album
                , catalogAlbumResponseWanted = wanted
                , catalogAlbumResponseMatchedClusterId = Nothing  -- No longer stored, derived from current_quality
                , catalogAlbumResponseQualityProfileId = DBTypes.catalogAlbumQualityProfileId album
                , catalogAlbumResponseScore = Nothing
                , catalogAlbumResponseCreatedAt = fmap (show :: UTCTime -> Text) (DBTypes.catalogAlbumCreatedAt album)
                , catalogAlbumResponseUpdatedAt = fmap (show :: UTCTime -> Text) (DBTypes.catalogAlbumUpdatedAt album)
                }

    -- Delete catalog album
    deleteAlbumHandler :: Maybe Text -> Int64 -> Handler NoContent
    deleteAlbumHandler authHeader albumId = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection connPool $ \conn ->
        DB.deleteCatalogAlbum conn albumId
      pure NoContent

    -- Get catalog albums with enhanced state information (replaces simple GET /albums)
    albumOverviewHandler :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Int64 -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Handler AlbumOverviewResponse
    albumOverviewHandler authHeader maybeOffset maybeLimit _maybeWanted maybeArtistId maybeSearch maybeSort maybeOrder maybeStateFilter maybeQualityFilter maybeReleaseDateAfter maybeReleaseDateBefore = do
      _ <- requireAuth configVar jwtSecret authHeader

      let offset = fromMaybe 0 maybeOffset
      let limit = fromMaybe 50 maybeLimit

      -- Parse comma-separated filters
      let maybeStates = fmap (map Text.strip . Text.splitOn ",") maybeStateFilter
      let maybeQualities = fmap (map Text.strip . Text.splitOn ",") maybeQualityFilter

      -- Query database for albums with joined data
      -- State filtering now happens at the database level
      let albumQuery = DB.AlbumQuery
            { DB.aqLimit = limit
            , DB.aqOffset = offset
            , DB.aqStates = maybeStates
            , DB.aqQualities = maybeQualities
            , DB.aqArtistId = maybeArtistId
            , DB.aqSearch = maybeSearch
            , DB.aqSort = maybeSort
            , DB.aqOrder = maybeOrder
            , DB.aqReleaseDateAfter = maybeReleaseDateAfter
            , DB.aqReleaseDateBefore = maybeReleaseDateBefore
            }
      (overviewRows, totalCount, statsData) <- liftIO $ withConnection connPool $ \conn -> do
        rows <- DB.getCatalogAlbumsOverview conn albumQuery
        count <- DB.getCatalogAlbumsOverviewCount conn albumQuery
        stats <- DB.getCatalogAlbumsOverviewStats conn
        pure (rows, count, stats)

      -- Convert rows to response objects
      let responses = map rowToResponse overviewRows

      -- Build pagination
      let pagination = AlbumOverviewPagination
            { albumOverviewPaginationTotal = totalCount
            , albumOverviewPaginationOffset = offset
            , albumOverviewPaginationLimit = limit
            }

      -- Compute state stats using SQL-side aggregation (ignoring state and quality filters for global stats)
      stateStatsCounts <- liftIO $ withConnection connPool $ \conn ->
        DB.getCatalogAlbumsStatsByState conn maybeArtistId maybeSearch maybeReleaseDateAfter maybeReleaseDateBefore

      -- Convert Text state names to AlbumState
      let statsByState = mapMaybe (\(stateText, count) ->
            case textToAlbumState stateText of
              Just albumState -> Just (albumState, count)
              Nothing -> Nothing
            ) stateStatsCounts

      -- Build stats
      let stats = AlbumOverviewStats
            { albumOverviewStatsByState = statsByState
            , albumOverviewStatsByQuality = statsData
            }

      pure AlbumOverviewResponse
        { albumOverviewResponsePagination = pagination
        , albumOverviewResponseStats = stats
        , albumOverviewResponseAlbums = responses
        }

    -- Helper function to compute album state from database row
    rowToAlbumState :: DB.CatalogAlbumOverviewRow -> AlbumState
    rowToAlbumState row =
      let maybeProfile = case (DB.caorQualityProfileId row, DB.caorQualityProfileName row, DB.caorQualityProfileCutoff row) of
            (Just profileId, Just name, Just cutoff) -> Just $ Qual.QualityProfile
              { Qual.qfId = Just profileId
              , Qual.qfName = name
              , Qual.qfCutoffQuality = fromMaybe Qual.Unknown (textToQuality cutoff)
              , Qual.qfQualityPreferences = []  -- Not needed for state computation
              , Qual.qfUpgradeAutomatically = False  -- Not needed for state computation
              }
            _ -> Nothing
          context = Core.AlbumContext
            { Core.acQualityProfile = maybeProfile
            , Core.acCurrentQuality = DB.caorCurrentQuality row >>= textToQuality
            , Core.acInLibrary = isJust (DB.caorMatchedClusterId row)
            , Core.acActiveDownloadStatus = DB.caorActiveDownloadStatus row
            }
      in Core.computeAlbumState context

    -- Helper function to convert a database row to a response object
    rowToResponse :: DB.CatalogAlbumOverviewRow -> CatalogAlbumOverviewResponse
    rowToResponse row =
      let albumState = rowToAlbumState row

          activeDownload = case DB.caorActiveDownloadId row of
            Nothing -> Nothing
            Just downloadId -> Just $ ActiveDownloadInfo
              { activeDownloadId = downloadId
              , activeDownloadStatus = fromMaybe "unknown" (DB.caorActiveDownloadStatus row)
              , activeDownloadProgress = fromMaybe 0.0 (DB.caorActiveDownloadProgress row)
              , activeDownloadQuality = DB.caorActiveDownloadQuality row
              , activeDownloadTitle = fromMaybe "" (DB.caorActiveDownloadTitle row)
              , activeDownloadSizeBytes = DB.caorActiveDownloadSizeBytes row
              , activeDownloadStartedAt = DB.caorActiveDownloadStartedAt row
              , activeDownloadErrorMessage = DB.caorActiveDownloadErrorMessage row
              }

      in CatalogAlbumOverviewResponse
        { catalogAlbumOverviewId = DB.caorAlbumId row
        , catalogAlbumOverviewReleaseGroupMBID = DB.caorReleaseGroupMBID row
        , catalogAlbumOverviewTitle = DB.caorTitle row
        , catalogAlbumOverviewArtistId = DB.caorArtistId row
        , catalogAlbumOverviewArtistMBID = DB.caorArtistMBID row
        , catalogAlbumOverviewArtistName = DB.caorArtistName row
        , catalogAlbumOverviewType = DB.caorAlbumType row
        , catalogAlbumOverviewFirstReleaseDate = DB.caorFirstReleaseDate row
        , catalogAlbumOverviewCoverUrl = DB.caorCoverUrl row
        , catalogAlbumOverviewCoverThumbnailUrl = DB.caorCoverThumbnailUrl row
        , catalogAlbumOverviewState = albumState
        , catalogAlbumOverviewWanted = DB.caorWanted row
        , catalogAlbumOverviewHasCluster = isJust (DB.caorMatchedClusterId row)
        , catalogAlbumOverviewMatchedClusterId = DB.caorMatchedClusterId row
        , catalogAlbumOverviewCurrentQuality = DB.caorCurrentQuality row
        , catalogAlbumOverviewQualityProfileId = DB.caorQualityProfileId row
        , catalogAlbumOverviewQualityProfileName = DB.caorQualityProfileName row
        , catalogAlbumOverviewActiveDownload = activeDownload
        , catalogAlbumOverviewDownloadCount = DB.caorDownloadCount row
        , catalogAlbumOverviewLastDownloadAt = DB.caorLastDownloadAt row
        , catalogAlbumOverviewCreatedAt = DB.caorCreatedAt row
        , catalogAlbumOverviewUpdatedAt = DB.caorUpdatedAt row
        , catalogAlbumOverviewImportedAt = DB.caorImportedAt row
        }


    -- Helper function to parse AlbumState from Text
    textToAlbumState :: Text -> Maybe AlbumState
    textToAlbumState txt = case Text.toLower txt of
      "notwanted" -> Just NotWanted
      "not_wanted" -> Just NotWanted
      "wanted" -> Just Wanted
      "searching" -> Just Searching
      "downloading" -> Just Downloading
      "failed" -> Just Failed
      "identificationfailed" -> Just IdentificationFailed
      "identification_failed" -> Just IdentificationFailed
      "inlibrary" -> Just InLibrary
      "in_library" -> Just InLibrary
      "monitored" -> Just Monitored
      "upgrading" -> Just Upgrading
      _ -> Nothing

    -- Bulk operations on catalog albums
    bulkActionHandler :: Maybe Text -> BulkAlbumActionRequest -> Handler NoContent
    bulkActionHandler authHeader req = do
      _ <- requireAuth configVar jwtSecret authHeader

      let albumIds = bulkAlbumActionAlbumIds req
      let action = bulkAlbumActionAction req

      case action of
        SetQualityProfile profileId -> do
          -- Update quality profile for all albums
          liftIO $ withConnection connPool $ \conn ->
            forM_ albumIds $ \albumId ->
              DB.updateCatalogAlbum conn albumId (Just (Just profileId))

        SetWanted wanted -> do
          -- Update wanted status for all albums
          liftIO $ withConnection connPool $ \conn ->
            forM_ albumIds $ \albumId ->
              DB.updateCatalogAlbum conn albumId Nothing

          -- If setting to wanted, emit WantedAlbumAdded events for each
          when wanted $ do
            albums <- liftIO $ withConnection connPool $ \conn ->
              DB.getCatalogAlbums conn
            let targetAlbums = filter (\a -> DBTypes.catalogAlbumId a `elem` map Just albumIds) albums
            forM_ targetAlbums $ \album -> do
              case DBTypes.catalogAlbumId album of
                Nothing -> pure ()
                Just albumId -> do
                  liftIO $ EventBus.publishAndLog bus le "api.catalog.bulk" $ Events.WantedAlbumAdded
                    { Events.wantedCatalogAlbumId = albumId
                    , Events.wantedReleaseGroupId = DBTypes.catalogAlbumReleaseGroupMBID album
                    , Events.wantedAlbumTitle = DBTypes.catalogAlbumTitle album
                    , Events.wantedArtistName = DBTypes.catalogAlbumArtistName album
                    }

        TriggerSearch -> do
          -- Emit WantedAlbumAdded events to trigger searches
          albums <- liftIO $ withConnection connPool $ \conn ->
            DB.getCatalogAlbums conn
          let targetAlbums = filter (\a -> DBTypes.catalogAlbumId a `elem` map Just albumIds) albums
          forM_ targetAlbums $ \album -> do
            case DBTypes.catalogAlbumId album of
              Nothing -> pure ()
              Just albumId -> do
                liftIO $ EventBus.publishAndLog bus le "api.catalog.bulk" $ Events.WantedAlbumAdded
                  { Events.wantedCatalogAlbumId = albumId
                  , Events.wantedReleaseGroupId = DBTypes.catalogAlbumReleaseGroupMBID album
                  , Events.wantedAlbumTitle = DBTypes.catalogAlbumTitle album
                  , Events.wantedArtistName = DBTypes.catalogAlbumArtistName album
                  }

        DeleteFromCatalog -> do
          -- Delete albums from catalog
          liftIO $ withConnection connPool $ \conn ->
            forM_ albumIds $ \albumId ->
              DB.deleteCatalogAlbum conn albumId

      pure NoContent

    -- Search for available releases from indexers for a specific album
    searchAlbumReleasesHandler :: Maybe Text -> Int64 -> Handler AlbumReleasesResponse
    searchAlbumReleasesHandler authHeader albumId = do
      _ <- requireAuth configVar jwtSecret authHeader

      -- Get the album from the database
      maybeAlbum <- liftIO $ withConnection connPool $ \conn -> do
        albums <- DB.getCatalogAlbums conn
        pure $ find (\a -> DBTypes.catalogAlbumId a == Just albumId) albums

      case maybeAlbum of
        Nothing -> throw404 $ "Album not found: " <> show albumId
        Just album -> do
          let albumTitle = DBTypes.catalogAlbumTitle album
          let artistName = DBTypes.catalogAlbumArtistName album

          -- Read current config to get indexers
          config <- liftIO $ STM.atomically $ STM.readTVar configVar
          let indexerCfg = Cfg.indexers config
          let enabledIndexers = filter Cfg.indexerEnabled (Cfg.indexerList indexerCfg)

          when (null enabledIndexers) $ do
            throw400 "No enabled indexers configured"

          -- Build search query
          let searchQuery = SearchQuery
                { sqArtist = Just artistName
                , sqAlbum = Just albumTitle
                , sqYear = Nothing
                , sqQuery = Nothing
                , sqCategories = [3000, 3010, 3020]  -- Audio categories
                , sqLimit = 50
                , sqOffset = 0
                }

          -- Search all indexers concurrently with 30s timeout
          startTime <- liftIO getCurrentTime

          searchResult <- liftIO $ race
            (threadDelay (90 * 1000000))  -- 90 seconds timeout (slskd searches can take 60+ seconds)
            (searchAllIndexers enabledIndexers searchQuery)

          endTime <- liftIO getCurrentTime
          let searchDuration = realToFrac $ diffUTCTime endTime startTime :: Double

          case searchResult of
            Left () -> do
              -- Timeout occurred
              throw500 "Search timed out after 90 seconds"

            Right results -> do
              -- Convert results to response format
              let releases = concatMap convertIndexerResult results

              -- Compute wanted status using Core.Catalog logic
              maybeProfile <- liftIO $ withConnection connPool $ \conn ->
                case DBTypes.catalogAlbumQualityProfileId album of
                  Nothing -> pure Nothing
                  Just profileId -> DB.getQualityProfile conn profileId

              let albumContext = Core.AlbumContext
                    { Core.acQualityProfile = maybeProfile
                    , Core.acCurrentQuality = DBTypes.catalogAlbumCurrentQuality album >>= textToQuality
                    , Core.acInLibrary = isJust (DBTypes.catalogAlbumCurrentQuality album)
                    , Core.acActiveDownloadStatus = Nothing
                    }
                  wanted = Core.isAlbumWanted albumContext

              pure AlbumReleasesResponse
                { albumReleasesAlbum = CatalogAlbumResponse
                    { catalogAlbumResponseId = DBTypes.catalogAlbumId album
                    , catalogAlbumResponseReleaseGroupMBID = DBTypes.catalogAlbumReleaseGroupMBID album
                    , catalogAlbumResponseTitle = albumTitle
                    , catalogAlbumResponseArtistMBID = DBTypes.catalogAlbumArtistMBID album
                    , catalogAlbumResponseArtistName = artistName
                    , catalogAlbumResponseType = DBTypes.catalogAlbumType album
                    , catalogAlbumResponseFirstReleaseDate = DBTypes.catalogAlbumFirstReleaseDate album
                    , catalogAlbumResponseCoverUrl = DBTypes.catalogAlbumCoverUrl album
                    , catalogAlbumResponseCoverThumbnailUrl = DBTypes.catalogAlbumCoverThumbnailUrl album
                    , catalogAlbumResponseWanted = wanted
                    , catalogAlbumResponseMatchedClusterId = Nothing  -- No longer stored, derived from current_quality
                    , catalogAlbumResponseQualityProfileId = DBTypes.catalogAlbumQualityProfileId album
                    , catalogAlbumResponseScore = Nothing
                    , catalogAlbumResponseCreatedAt = fmap (show :: UTCTime -> Text) (DBTypes.catalogAlbumCreatedAt album)
                    , catalogAlbumResponseUpdatedAt = fmap (show :: UTCTime -> Text) (DBTypes.catalogAlbumUpdatedAt album)
                    }
                , albumReleasesReleases = releases
                , albumReleasesSearchTime = searchDuration
                }

    -- Stream album releases via SSE (results sent as they arrive)
    streamAlbumReleasesHandler :: Int64 -> Maybe Text -> Handler (SourceIO ReleaseStreamEvent)
    streamAlbumReleasesHandler albumId maybeToken = do
      -- Validate JWT from query parameter (since EventSource doesn't support custom headers)
      currentCfg <- liftIO $ STM.atomically $ STM.readTVar configVar
      let cfg = Cfg.server currentCfg
      authEnabled <- liftIO $ checkAuthEnabled cfg

      when authEnabled $ do
        case maybeToken of
          Nothing -> throwError err401 { errBody = "JWT token required in query parameter" }
          Just token -> do
            result <- liftIO $ validateJWT jwtSecret token
            case result of
              Left _err -> throwError err401 { errBody = "Invalid or expired JWT" }
              Right _claims -> pure ()

      -- Get the album from the database
      maybeAlbum <- liftIO $ withConnection connPool $ \conn -> do
        albums <- DB.getCatalogAlbums conn
        pure $ find (\a -> DBTypes.catalogAlbumId a == Just albumId) albums

      case maybeAlbum of
        Nothing -> throwError err404 { errBody = "Album not found" }
        Just album -> do
          let albumTitle = DBTypes.catalogAlbumTitle album
          let artistName = DBTypes.catalogAlbumArtistName album

          -- Read current config to get indexers and slskd
          config <- liftIO $ STM.atomically $ STM.readTVar configVar
          let indexerCfg = Cfg.indexers config
          let enabledIndexers = filter Cfg.indexerEnabled (Cfg.indexerList indexerCfg)
          let maybeSlskdConfig = Cfg.downloadSlskdClient (Cfg.download config)

          -- Build search query
          let searchQuery = SearchQuery
                { sqArtist = Just artistName
                , sqAlbum = Just albumTitle
                , sqYear = Nothing
                , sqQuery = Nothing
                , sqCategories = [3000, 3010, 3020]
                , sqLimit = 50
                , sqOffset = 0
                }

          -- Return the streaming source
          pure $ streamSearchResults le registry configVar enabledIndexers maybeSlskdConfig searchQuery

    -- Stream search results from all sources
    streamSearchResults ::
      LogEnv ->
      ServiceRegistry ->
      TVar Cfg.Config ->
      [Cfg.Indexer] ->
      Maybe Cfg.SlskdConfig ->
      SearchQuery ->
      SourceIO ReleaseStreamEvent
    streamSearchResults logEnv svcRegistry _cfgVar indexers maybeSlskd query = SourceT $ \k -> k $ Effect $ do
      -- Create a channel for results
      chan <- newTChanIO

      startTime <- getCurrentTime

      -- Start all searches concurrently, writing results to channel
      _ <- async $ do
        let httpClient = srHttpClient svcRegistry

        -- Search indexers concurrently
        forM_ indexers $ \indexer -> async $ do
          let indexerName = Cfg.indexerName indexer
          STM.atomically $ writeTChan chan (SearchStarted indexerName)
          result <- searchIndexer httpClient indexer query
          case result of
            Left err -> do
              STM.atomically $ writeTChan chan (SearchError indexerName (show err))
            Right indexerResult -> do
              let releases = irReleases indexerResult
              forM_ releases $ \release -> do
                let response = releaseInfoToResponse indexerName release
                STM.atomically $ writeTChan chan (ReleaseFound response indexerName)
              STM.atomically $ writeTChan chan (SearchCompleted indexerName (length releases))

        -- Search slskd if enabled (streaming version - emits results as found)
        case maybeSlskd of
          Just slskdConfig | Cfg.slskdEnabled slskdConfig -> do
            let indexerName = "slskd"
            STM.atomically $ writeTChan chan (SearchStarted indexerName)
            let client = createSlskdClient slskdConfig httpClient
            let artistName = fromMaybe "" (sqArtist query)
            let albumName = fromMaybe "" (sqAlbum query)

            -- Use streaming search - emit each candidate as it's found
            resultCountRef <- IORef.newIORef (0 :: Int)
            result <- searchSlskdStreaming logEnv client artistName albumName 60 $ \candidate -> do
              let release = slskdCandidateToReleaseInfo candidate
              let response = releaseInfoToResponse indexerName release
              STM.atomically $ writeTChan chan (ReleaseFound response indexerName)
              IORef.modifyIORef' resultCountRef (+1)

            finalCount <- IORef.readIORef resultCountRef
            case result of
              Left err -> do
                STM.atomically $ writeTChan chan (SearchError indexerName err)
              Right _ -> do
                STM.atomically $ writeTChan chan (SearchCompleted indexerName finalCount)
          _ -> pure ()

        -- Signal completion
        endTime <- getCurrentTime
        let totalTime = realToFrac $ diffUTCTime endTime startTime :: Double
        STM.atomically $ writeTChan chan (SearchDone totalTime)

      -- Return the source that reads from the channel
      pure $ eventLoop chan

    eventLoop :: TChan ReleaseStreamEvent -> StepT IO ReleaseStreamEvent
    eventLoop chan = Effect $ do
      evt <- STM.atomically $ readTChan chan
      case evt of
        SearchDone _ -> pure $ Yield evt Stop
        _ -> pure $ Yield evt (eventLoop chan)

    -- Search all indexers and slskd concurrently
    searchAllIndexers :: [Cfg.Indexer] -> SearchQuery -> IO [IndexerResult]
    searchAllIndexers indexers query = do
      let httpClient = srHttpClient registry

      -- Get slskd config
      config <- STM.atomically $ STM.readTVar configVar
      let maybeSlskdConfig = Cfg.downloadSlskdClient (Cfg.download config)

      -- Search indexers
      let searchIndexersIO = do
            results <- mapConcurrently (\indexer -> searchIndexer httpClient indexer query) indexers
            pure $ rights results

      -- Search slskd if configured and enabled
      let searchSlskdIO = case maybeSlskdConfig of
            Just slskdConfig | Cfg.slskdEnabled slskdConfig -> do
              runKatipContextT le () "catalog.search" $ do
                $(logTM) InfoS $ logStr $ ("Searching slskd at: " <> Cfg.slskdUrl slskdConfig :: Text)
              let client = createSlskdClient slskdConfig httpClient
              let artistName = fromMaybe "" (sqArtist query)
              let albumName = fromMaybe "" (sqAlbum query)
              result <- searchSlskd le client artistName albumName
              case result of
                Left err -> do
                  runKatipContextT le () "catalog.search" $ do
                    $(logTM) ErrorS $ logStr $ ("slskd search failed: " <> err :: Text)
                  pure []
                Right candidates -> do
                  runKatipContextT le () "catalog.search" $ do
                    $(logTM) InfoS $ logStr $ ("slskd returned " <> show (length candidates) <> " candidates" :: Text)
                  -- Convert slskd candidates to ReleaseInfo, then wrap as IndexerResult
                  let releases = map slskdCandidateToReleaseInfo candidates
                  pure [IndexerResult
                    { irIndexerName = "slskd"
                    , irReleases = releases
                    , irSearchTime = 0  -- Not tracked here
                    }]
            _ -> pure []

      -- Run both searches in parallel
      (indexerResults, slskdResults) <- concurrently searchIndexersIO searchSlskdIO
      pure $ indexerResults ++ slskdResults

    -- Convert IndexerResult to list of ReleaseResponse
    convertIndexerResult :: IndexerResult -> [ReleaseResponse]
    convertIndexerResult indexerResult =
      map (releaseInfoToResponse (irIndexerName indexerResult)) (irReleases indexerResult)

    -- Convert ReleaseInfo to ReleaseResponse
    releaseInfoToResponse :: Text -> ReleaseInfo -> ReleaseResponse
    releaseInfoToResponse source release = ReleaseResponse
      { releaseResponseTitle = riTitle release
      , releaseResponseSource = source
      , releaseResponseQuality = qualityToText (riQuality release)
      , releaseResponseSize = fmap fromIntegral (riSize release)
      , releaseResponseSeeders = riSeeders release
      , releaseResponsePeers = riPeers release
      , releaseResponseDownloadType = case riDownloadType release of
          NZB -> "nzb"
          Torrent -> "torrent"
          Slskd -> "slskd"
      , releaseResponseDownloadUrl = riDownloadUrl release
      , releaseResponsePublishDate = fmap show (riPublishDate release)
      , releaseResponseSlskdUsername = riSlskdUsername release
      , releaseResponseSlskdFiles = fmap (map toSlskdFileResponse) (riSlskdFiles release)
      }

    -- Convert SlskdFile to SlskdFileResponse
    toSlskdFileResponse :: Indexer.SlskdFile -> SlskdFileResponse
    toSlskdFileResponse f = SlskdFileResponse
      { slskdFileFilename = Indexer.sfFilename f
      , slskdFileSize = Indexer.sfSize f
      }
