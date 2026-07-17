{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Catalog API handlers.
module Skema.API.Handlers.Catalog
  ( catalogServer
  ) where

import Skema.API.Types.Catalog (CatalogAPI, CatalogQueryResponse(..), CatalogArtistResponse(..), ArtistsPagination(..), ArtistsResponse(..), CatalogAlbumResponse(..), CreateCatalogArtistRequest(..), UpdateCatalogArtistRequest(..), CreateCatalogAlbumRequest(..), UpdateCatalogAlbumRequest(..), CatalogAlbumOverviewResponse(..), AlbumOverviewPagination(..), AlbumOverviewStats(..), AlbumOverviewResponse(..), BulkAlbumActionRequest(..), BulkAlbumAction(..), AlbumState(..), ActiveDownloadInfo(..), AlbumReleasesResponse(..), ReleaseResponse(..), SlskdFileResponse(..), ReleaseStreamEvent(..), AlbumTrackResponse(..), AlbumTracksResponse(..))
import Skema.API.Types.Common (SourceIO)
import Skema.Auth (checkAuthEnabled)
import Skema.Auth.JWT (validateJWT)
import Skema.API.Types.Tasks (TaskRequest(..), TaskResponse(..), TaskResource(..))
import qualified Skema.Services.TaskManager as TM
import qualified Skema.Domain.Catalog as Core
import Skema.API.Handlers.Utils (throw400, throw404, throw500, readConfig, parsePagination)
import Skema.Database.Connection
import qualified Skema.Database.Repository as DB
import qualified Skema.Database.Types as DBTypes
import Database.SQLite.Simple (Only(..))
import qualified Skema.Config.Types as Cfg
import Skema.Services.Download.Search (StreamEvent(..), streamReleaseSearch)
import Skema.Services.Catalog (updateAlbumQualityProfile)
import Skema.MusicBrainz.Client (searchArtists, searchReleaseGroups, getRelease, getReleaseGroup, getReleaseGroupReleases, pickRepresentativeRelease, prettyClientError)
import Skema.MusicBrainz.Types (MBRelease(..), MBReleaseGroup(..), MBMedium(..), MBTrack(..), mbasArtists, mbasArtistId, mbrgsReleaseGroups, mbrgsReleaseGroupId, MBID(..), unMBID)
import Skema.API.Converters (mbArtistSearchToCatalogResponse, mbReleaseGroupSearchToCatalogResponse)
import Skema.API.Types.Common (timestampToText)
import qualified Skema.Events.Bus as EventBus
import qualified Skema.Events.Types as Events
import Skema.Indexer.Types (SearchQuery(..), ReleaseInfo(..), DownloadType(..))
import qualified Skema.Indexer.Types as Indexer
import Skema.Domain.Quality (qualityToText, textToQuality)
import Control.Concurrent.Async (async)
import Data.Aeson (toJSON, object, (.=), decodeStrict)
import Servant hiding (SourceIO)
import Skema.API.App (Env(..), AppM, toServiceContext)
import Servant.Types.SourceT (SourceT(..), StepT(..))
import Control.Concurrent.STM.TChan (TChan, newTChanIO, writeTChan, readTChan)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Control.Concurrent.STM as STM

-- | Catalog API handlers.
catalogServer :: Env -> ServerT CatalogAPI AppM
catalogServer env@Env{envLogEnv = le, envEventBus = bus, envJwtSecret = jwtSecret, envTaskManager = tm, envDbPool = connPool, envConfigVar = configVar, envMBClient = mbClient} =
  taskHandler
  :<|> queryHandler
  :<|> getArtistsHandler
  :<|> getArtistHandler
  :<|> createArtistHandler
  :<|> updateArtistHandler
  :<|> deleteArtistHandler
  :<|> albumOverviewHandler
  :<|> createAlbumHandler
  :<|> updateAlbumHandler
  :<|> deleteAlbumHandler
  :<|> searchAlbumReleasesHandler
  :<|> streamAlbumReleasesHandler
  :<|> getAlbumTracksHandler
  :<|> getAlbumHandler
  :<|> bulkActionHandler
  where
    taskHandler :: TaskRequest -> AppM TaskResponse
    taskHandler req = do

      -- Create task based on request type
      case taskRequestType req of
        "refresh" -> case taskRequestResourceId req of
          Nothing -> throw400 "Artist ID required for refresh task"
          Just artistId -> TM.startTask tm CatalogResource (Just artistId) "refresh" $ \taskId -> do
            -- Get artist by ID to find MBID
            maybeArtist <- withConnection connPool $ \conn ->
              DB.getCatalogArtistById conn artistId
            case maybeArtist of
              Nothing ->
                TM.failTask tm taskId $ "Artist not found: " <> show artistId
              Just artist -> do
                EventBus.publishAndLog bus le "api.catalog.task" $ Events.CatalogArtistRefreshRequested
                  { Events.refreshArtistMBID = DBTypes.catalogArtistMBID artist
                  }
                TM.completeTask tm taskId (Just $ toJSON $ object
                  [ "message" .= ("Artist catalog refresh requested for: " <> DBTypes.catalogArtistName artist :: Text)
                  , "artist_mbid" .= DBTypes.catalogArtistMBID artist
                  ])

        "refresh_all" -> TM.startTask tm CatalogResource Nothing "refresh_all" $ \taskId -> do
          TM.updateTaskProgress tm taskId 0.2 (Just "Fetching followed artists...")
          followedArtists <- withConnection connPool $ \conn ->
            DB.getCatalogArtists conn (Just True) Nothing Nothing Nothing

          let totalArtists = length followedArtists
          forM_ followedArtists $ \artist ->
            EventBus.publishAndLog bus le "api.catalog.task" $ Events.CatalogArtistRefreshRequested
              { Events.refreshArtistMBID = DBTypes.catalogArtistMBID artist
              }

          TM.completeTask tm taskId (Just $ toJSON $ object
            [ "message" .= ("Catalog refresh requested for " <> show totalArtists <> " artists" :: Text)
            , "artists_count" .= totalArtists
            ])

        _ -> throw400 "Unknown task type"
    -- Universal search handler - searches both artists and albums
    queryHandler :: Text -> Maybe Int -> AppM CatalogQueryResponse
    queryHandler query maybeLimit = do
      let mbEnv = mbClient
      let limit = fromMaybe 10 maybeLimit

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
          let releaseGroups = mbrgsReleaseGroups searchResp
          -- Check which albums are already in catalog and look up cluster matches
          (catalogAlbums, clusterIds) <- liftIO $ withConnection connPool $ \conn -> do
            cats <- mapM (\result -> DB.getCatalogAlbumByReleaseGroupMBID conn (unMBID $ mbrgsReleaseGroupId result))
                         releaseGroups
            -- Look up cluster IDs for in-library status
            clusters <- forM releaseGroups $ \result -> do
              rows <- queryRows conn
                "SELECT id FROM clusters WHERE mb_release_group_id = ? LIMIT 1"
                (Only (unMBID $ mbrgsReleaseGroupId result)) :: IO [Only Int64]
              pure $ case viaNonEmpty head rows of
                Just (Only cid) -> Just cid
                Nothing -> Nothing
            pure (cats, clusters)

          let responses = zipWith mbReleaseGroupSearchToCatalogResponse releaseGroups catalogAlbums
          -- Enrich with cluster IDs for in-library indicator
          pure $ zipWith (\resp cid -> resp { catalogAlbumResponseMatchedClusterId = cid }) responses clusterIds

      pure $ CatalogQueryResponse
        { catalogQueryResponseArtists = artists
        , catalogQueryResponseAlbums = albums
        }

    -- Get catalog artists
    getArtistsHandler :: Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Text -> Maybe Text -> Maybe Text -> AppM ArtistsResponse
    getArtistsHandler maybeOffset maybeLimit maybeFollowed maybeSearch maybeSort maybeOrder = do

      let (offset, limit) = parsePagination maybeOffset maybeLimit

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
            , catalogArtistResponseBio = DBTypes.catalogArtistBio artist
            , catalogArtistResponseFollowed = DBTypes.catalogArtistFollowed artist
            , catalogArtistResponseQualityProfileId = DBTypes.catalogArtistQualityProfileId artist
            , catalogArtistResponseScore = Nothing  -- No score in database records
            , catalogArtistResponseLastCheckedAt = fmap timestampToText (DBTypes.catalogArtistLastCheckedAt artist)
            , catalogArtistResponseCreatedAt = fmap timestampToText (DBTypes.catalogArtistCreatedAt artist)
            , catalogArtistResponseUpdatedAt = fmap timestampToText (DBTypes.catalogArtistUpdatedAt artist)
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

    -- Get single catalog artist by ID
    getArtistHandler :: Int64 -> AppM CatalogArtistResponse
    getArtistHandler artistId = do

      maybeArtist <- liftIO $ withConnection connPool $ \conn ->
        DB.getCatalogArtistById conn artistId

      case maybeArtist of
        Nothing -> throw404 $ "Artist not found: " <> show artistId
        Just artist -> do
          -- Include album overview data
          maybeAlbums <- liftIO $ case DBTypes.catalogArtistId artist of
            Just aid -> do
              albumRows <- withConnection connPool $ \conn ->
                DB.getCatalogAlbumsByArtistOverview conn aid
              pure $ Just $ map rowToResponse albumRows
            Nothing -> pure Nothing

          pure $ CatalogArtistResponse
            { catalogArtistResponseId = DBTypes.catalogArtistId artist
            , catalogArtistResponseMBID = DBTypes.catalogArtistMBID artist
            , catalogArtistResponseName = DBTypes.catalogArtistName artist
            , catalogArtistResponseType = DBTypes.catalogArtistType artist
            , catalogArtistResponseImageUrl = DBTypes.catalogArtistImageUrl artist
            , catalogArtistResponseThumbnailUrl = DBTypes.catalogArtistThumbnailUrl artist
            , catalogArtistResponseBio = DBTypes.catalogArtistBio artist
            , catalogArtistResponseFollowed = DBTypes.catalogArtistFollowed artist
            , catalogArtistResponseQualityProfileId = DBTypes.catalogArtistQualityProfileId artist
            , catalogArtistResponseScore = Nothing
            , catalogArtistResponseLastCheckedAt = fmap timestampToText (DBTypes.catalogArtistLastCheckedAt artist)
            , catalogArtistResponseCreatedAt = fmap timestampToText (DBTypes.catalogArtistCreatedAt artist)
            , catalogArtistResponseUpdatedAt = fmap timestampToText (DBTypes.catalogArtistUpdatedAt artist)
            , catalogArtistResponseAlbums = maybeAlbums
            }

    -- Create/upsert catalog artist
    createArtistHandler :: CreateCatalogArtistRequest -> AppM CatalogArtistResponse
    createArtistHandler req = do

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

      -- If quality_profile_id is provided, set it on the artist
      case createCatalogArtistQualityProfileId req of
        Just profileId -> do
          maybeArtistForProfile <- liftIO $ withConnection connPool $ \conn ->
            DB.getCatalogArtistByMBID conn (createCatalogArtistMBID req)
          case maybeArtistForProfile >>= DBTypes.catalogArtistId of
            Just artistId -> liftIO $ withConnection connPool $ \conn ->
              DB.updateCatalogArtist conn artistId
                (createCatalogArtistFollowed req)
                (Just (Just profileId))
            Nothing -> pure ()
        Nothing -> pure ()

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
          , catalogArtistResponseBio = DBTypes.catalogArtistBio artist
          , catalogArtistResponseFollowed = DBTypes.catalogArtistFollowed artist
          , catalogArtistResponseQualityProfileId = DBTypes.catalogArtistQualityProfileId artist
          , catalogArtistResponseScore = Nothing
          , catalogArtistResponseLastCheckedAt = fmap timestampToText (DBTypes.catalogArtistLastCheckedAt artist)
          , catalogArtistResponseCreatedAt = fmap timestampToText (DBTypes.catalogArtistCreatedAt artist)
          , catalogArtistResponseUpdatedAt = fmap timestampToText (DBTypes.catalogArtistUpdatedAt artist)
          , catalogArtistResponseAlbums = Nothing  -- Not needed for create response
          }

    -- Update catalog artist (followed status and quality profile)
    updateArtistHandler :: Int64 -> UpdateCatalogArtistRequest -> AppM CatalogArtistResponse
    updateArtistHandler artistId req = do
      liftIO $ withConnection connPool $ \conn ->
        DB.updateCatalogArtist conn artistId
          (updateCatalogArtistFollowed req)
          (updateCatalogArtistQualityProfileId req)

      -- Fetch the updated artist by ID
      maybeArtist <- liftIO $ withConnection connPool $ \conn ->
        DB.getCatalogArtistById conn artistId

      case maybeArtist of
        Nothing -> throw404 $ "Artist not found: " <> show artistId
        Just artist -> pure $ CatalogArtistResponse
          { catalogArtistResponseId = DBTypes.catalogArtistId artist
          , catalogArtistResponseMBID = DBTypes.catalogArtistMBID artist
          , catalogArtistResponseName = DBTypes.catalogArtistName artist
          , catalogArtistResponseType = DBTypes.catalogArtistType artist
          , catalogArtistResponseImageUrl = DBTypes.catalogArtistImageUrl artist
          , catalogArtistResponseThumbnailUrl = DBTypes.catalogArtistThumbnailUrl artist
          , catalogArtistResponseBio = DBTypes.catalogArtistBio artist
          , catalogArtistResponseFollowed = DBTypes.catalogArtistFollowed artist
          , catalogArtistResponseQualityProfileId = DBTypes.catalogArtistQualityProfileId artist
          , catalogArtistResponseScore = Nothing
          , catalogArtistResponseLastCheckedAt = fmap timestampToText (DBTypes.catalogArtistLastCheckedAt artist)
          , catalogArtistResponseCreatedAt = fmap timestampToText (DBTypes.catalogArtistCreatedAt artist)
          , catalogArtistResponseUpdatedAt = fmap timestampToText (DBTypes.catalogArtistUpdatedAt artist)
          , catalogArtistResponseAlbums = Nothing  -- Not needed for update response
          }

    -- Delete catalog artist
    deleteArtistHandler :: Int64 -> AppM NoContent
    deleteArtistHandler artistId = do
      liftIO $ withConnection connPool $ \conn ->
        DB.deleteCatalogArtist conn artistId
      pure NoContent

    -- Create/upsert catalog album
    createAlbumHandler :: CreateCatalogAlbumRequest -> AppM CatalogAlbumResponse
    createAlbumHandler req = do
      let mbEnv = mbClient

      -- Resolve metadata: either from request fields or by looking up MusicBrainz
      (rgMbid, title, artistMbid, artistName, albumType, firstReleaseDate) <- do
        case (createCatalogAlbumReleaseGroupMBID req, createCatalogAlbumReleaseMBID req) of
          (Nothing, Nothing) ->
            throw400 "Either release_group_mbid or release_mbid must be provided"
          (Just rgId, _)
            | Just t <- createCatalogAlbumTitle req
            , Just aMbid <- createCatalogAlbumArtistMBID req
            , Just aName <- createCatalogAlbumArtistName req ->
            -- All metadata provided (frontend flow) — use directly
            pure (rgId, t, aMbid, aName, createCatalogAlbumType req, createCatalogAlbumFirstReleaseDate req)
          (Just rgId, _) -> do
            -- Only release group MBID — resolve metadata from MusicBrainz
            rgResult <- liftIO $ getReleaseGroup mbEnv (MBID rgId)
            case rgResult of
              Left err -> throw500 $ "Failed to look up release group: " <> prettyClientError err
              Right rg -> case (mbrgArtistName rg, mbrgArtistId rg) of
                (Just aName, Just aId) ->
                  pure (rgId, mbrgTitle rg, unMBID aId, aName, mbrgType rg, mbrgFirstReleaseDate rg)
                _ -> throw500 "MusicBrainz release group has no artist information"
          (Nothing, Just relId) -> do
            -- Release MBID (ListenBrainz) — resolve release group from release
            relResult <- liftIO $ getRelease mbEnv (MBID relId)
            case relResult of
              Left err -> throw500 $ "Failed to look up release: " <> prettyClientError err
              Right rel -> case mbReleaseGroupId rel of
                Nothing -> throw500 "Release has no associated release group"
                Just (MBID rgId) -> case (mbReleaseArtistId rel, mbReleaseArtist rel) of
                  (Just aId, aName) | not (Text.null aName) ->
                    pure (rgId, mbReleaseTitle rel, unMBID aId, aName, Nothing, mbReleaseDate rel)
                  _ -> throw500 "Release has no artist information"

      let wantedFlag = fromMaybe True (createCatalogAlbumWanted req)

      -- Skip Various Artists
      when (DBTypes.isVariousArtists artistMbid) $
        throw400 "Cannot create catalog album for Various Artists - use a compilation source instead"

      artistId <- liftIO $ withConnection connPool $ \conn -> do
        maybeArtist <- DB.getCatalogArtistByMBID conn artistMbid
        case maybeArtist of
          Just artist -> case DBTypes.catalogArtistId artist of
            Just aid -> pure aid
            Nothing -> fail "Artist record has no ID"
          Nothing -> do
            DB.upsertCatalogArtist
              conn
              artistMbid
              artistName
              Nothing  -- artist_type
              Nothing  -- image_url
              Nothing  -- thumbnail_url
              False    -- followed (not automatically following)
              Nothing  -- added_by_rule_id
              Nothing  -- source_cluster_id
              Nothing  -- last_checked_at

      albumId' <- liftIO $ withConnection connPool $ \conn ->
        DB.upsertCatalogAlbum
          conn
          rgMbid
          title
          artistId
          artistMbid
          artistName
          albumType
          firstReleaseDate

      -- Resolve quality profile: explicit from request > artist > default
      resolvedProfileId <- liftIO $ withConnection connPool $ \conn ->
        DB.resolveQualityProfileId conn
          (createCatalogAlbumQualityProfileId req)
          (Just artistId)
          Nothing  -- No source for manual search

      -- Set the resolved profile on the album
      when (isJust resolvedProfileId && wantedFlag) $
        liftIO $ withConnection connPool $ \conn ->
          DB.updateCatalogAlbum conn albumId' (Just resolvedProfileId)

      -- Fetch the created/updated album
      maybeAlbum <- liftIO $ withConnection connPool $ \conn ->
        DB.getCatalogAlbumByReleaseGroupMBID conn rgMbid

      case maybeAlbum of
        Nothing -> throw500 "Failed to retrieve created album"
        Just album -> do
          -- Compute wanted status using Core.Catalog logic
          maybeProfile <- liftIO $ case DBTypes.catalogAlbumId album of
            Nothing -> pure Nothing
            Just albumId -> DB.getEffectiveQualityProfile connPool albumId

          let albumContext = Core.AlbumContext
                { Core.acQualityProfile = maybeProfile
                , Core.acCurrentQuality = DBTypes.catalogAlbumCurrentQuality album >>= textToQuality
                , Core.acInLibrary = isJust (DBTypes.catalogAlbumCurrentQuality album)
                , Core.acActiveDownloadStatus = Nothing
                }
              wanted = Core.isAlbumWanted albumContext

          -- Emit WantedAlbumAdded event if album is wanted
          when wanted $ do
            case DBTypes.catalogAlbumId album of
              Nothing -> pure ()
              Just albumId ->
                liftIO $ EventBus.publishAndLog bus le "api.catalog" $ Events.WantedAlbumAdded
                  { Events.wantedCatalogAlbumId = albumId
                  , Events.wantedReleaseGroupId = DBTypes.catalogAlbumReleaseGroupMBID album
                  , Events.wantedAlbumTitle = DBTypes.catalogAlbumTitle album
                  , Events.wantedArtistName = DBTypes.catalogAlbumArtistName album
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
            , catalogAlbumResponseMatchedClusterId = Nothing
            , catalogAlbumResponseCurrentQuality = DBTypes.catalogAlbumCurrentQuality album
            , catalogAlbumResponseQualityProfileId = DBTypes.catalogAlbumQualityProfileId album
            , catalogAlbumResponseScore = Nothing
            , catalogAlbumResponseCreatedAt = fmap timestampToText (DBTypes.catalogAlbumCreatedAt album)
            , catalogAlbumResponseUpdatedAt = fmap timestampToText (DBTypes.catalogAlbumUpdatedAt album)
            }

    -- Update catalog album (wanted status and quality profile)
    updateAlbumHandler :: Int64 -> UpdateCatalogAlbumRequest -> AppM CatalogAlbumResponse
    updateAlbumHandler albumId req = do
      result <- liftIO $ updateAlbumQualityProfile (toServiceContext env) albumId
        (updateCatalogAlbumQualityProfileId req)
      case result of
        Left err -> throw404 err
        Right (album, wanted) -> pure $ CatalogAlbumResponse
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
          , catalogAlbumResponseMatchedClusterId = Nothing
          , catalogAlbumResponseCurrentQuality = DBTypes.catalogAlbumCurrentQuality album
          , catalogAlbumResponseQualityProfileId = DBTypes.catalogAlbumQualityProfileId album
          , catalogAlbumResponseScore = Nothing
          , catalogAlbumResponseCreatedAt = fmap timestampToText (DBTypes.catalogAlbumCreatedAt album)
          , catalogAlbumResponseUpdatedAt = fmap timestampToText (DBTypes.catalogAlbumUpdatedAt album)
          }

    -- Delete catalog album
    deleteAlbumHandler :: Int64 -> AppM NoContent
    deleteAlbumHandler albumId = do
      liftIO $ withConnection connPool $ \conn ->
        DB.deleteCatalogAlbum conn albumId
      pure NoContent

    -- Get catalog albums with enhanced state information (replaces simple GET /albums)
    albumOverviewHandler :: Maybe Int -> Maybe Int -> Maybe Bool -> Maybe Int64 -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> AppM AlbumOverviewResponse
    albumOverviewHandler maybeOffset maybeLimit _maybeWanted maybeArtistId maybeSearch maybeSort maybeOrder maybeStateFilter maybeQualityFilter maybeReleaseDateAfter maybeReleaseDateBefore maybeMBID = do

      let (offset, limit) = parsePagination maybeOffset maybeLimit

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
            , DB.aqAlbumId = Nothing
            , DB.aqArtistId = maybeArtistId
            , DB.aqSearch = maybeSearch
            , DB.aqSort = maybeSort
            , DB.aqOrder = maybeOrder
            , DB.aqReleaseDateAfter = maybeReleaseDateAfter
            , DB.aqReleaseDateBefore = maybeReleaseDateBefore
            , DB.aqReleaseGroupMBID = maybeMBID
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
            case Core.textToAlbumState stateText of
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

    -- The state is computed once, in the overview SQL, so the state shown
    -- always matches the state filtered and aggregated on
    rowToAlbumState :: DB.CatalogAlbumOverviewRow -> AlbumState
    rowToAlbumState row = fromMaybe Wanted (Core.textToAlbumState (DB.caorState row))

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


    -- Get a single catalog album's overview by ID
    getAlbumHandler :: Int64 -> AppM CatalogAlbumOverviewResponse
    getAlbumHandler albumId = do
      maybeRow <- liftIO $ withConnection connPool $ \conn -> do
        rows <- DB.getCatalogAlbumsOverview conn DB.defaultAlbumQuery { DB.aqAlbumId = Just albumId, DB.aqLimit = 1 }
        pure $ viaNonEmpty head rows
      case maybeRow of
        Nothing -> throw404 $ "Album not found: " <> show albumId
        Just row -> pure $ rowToResponse row

    -- Get an album's tracklist from MusicBrainz.
    --
    -- Uses the release actually matched in the library when available,
    -- otherwise a representative release chosen from the release group.
    getAlbumTracksHandler :: Int64 -> AppM AlbumTracksResponse
    getAlbumTracksHandler albumId = do
      let mbEnv = mbClient

      maybeAlbum <- liftIO $ withConnection connPool $ \conn ->
        DB.getCatalogAlbumById conn albumId

      case maybeAlbum of
        Nothing -> throw404 $ "Album not found: " <> show albumId
        Just album -> do
          let rgMbid = DBTypes.catalogAlbumReleaseGroupMBID album
              firstReleaseDate = DBTypes.catalogAlbumFirstReleaseDate album

          -- Prefer the release actually matched in the library
          matchedReleaseId <- liftIO $ withConnection connPool $ \conn -> do
            rows <- queryRows conn
              "SELECT mb_release_id FROM clusters WHERE catalog_album_id = ? AND mb_release_id IS NOT NULL LIMIT 1"
              (Only albumId) :: IO [Only Text]
            pure $ case viaNonEmpty head rows of
              Just (Only rid) -> Just rid
              Nothing -> Nothing

          -- Resolve which release (edition) to show the tracklist for
          chosen <- case matchedReleaseId of
            Just rid -> pure $ Just (rid, True)
            Nothing -> do
              releasesResult <- liftIO $ getReleaseGroupReleases mbEnv (MBID rgMbid)
              case releasesResult of
                Left _ -> pure Nothing
                Right releases ->
                  pure $ fmap (\r -> (unMBID (mbReleaseId r), False))
                              (pickRepresentativeRelease firstReleaseDate releases)

          case chosen of
            Nothing -> pure $ AlbumTracksResponse
              { albumTracksReleaseMbid = Nothing
              , albumTracksReleaseTitle = Nothing
              , albumTracksIsMatchedRelease = False
              , albumTracksTracks = []
              }
            Just (releaseMbid, isMatched) -> do
              relResult <- liftIO $ getRelease mbEnv (MBID releaseMbid)
              case relResult of
                Left err -> throw500 $ "Failed to fetch release tracklist: " <> prettyClientError err
                Right rel -> do
                  let tracks =
                        [ AlbumTrackResponse
                            { albumTrackPosition = mbTrackPosition t
                            , albumTrackDiscNumber = mbMediumPosition m
                            , albumTrackTitle = mbTrackTitle t
                            , albumTrackArtist = mbTrackArtist t
                            , albumTrackLengthMs = mbTrackLength t
                            , albumTrackRecordingMbid = unMBID (mbTrackRecordingId t)
                            }
                        | m <- mbReleaseMedia rel, t <- mbMediumTracks m
                        ]
                  pure $ AlbumTracksResponse
                    { albumTracksReleaseMbid = Just releaseMbid
                    , albumTracksReleaseTitle = Just (mbReleaseTitle rel)
                    , albumTracksIsMatchedRelease = isMatched
                    , albumTracksTracks = tracks
                    }

    -- Bulk operations on catalog albums
    bulkActionHandler :: BulkAlbumActionRequest -> AppM NoContent
    bulkActionHandler req = do

      let albumIds = bulkAlbumActionAlbumIds req
      let action = bulkAlbumActionAction req

      case action of
        SetQualityProfile profileId -> do
          -- Update quality profile for all albums
          liftIO $ withConnection connPool $ \conn ->
            forM_ albumIds $ \albumId ->
              DB.updateCatalogAlbum conn albumId (Just (Just profileId))

        SetWanted wanted -> do
          -- "Wanted" is derived from the quality profile: assign the default
          -- profile to make albums wanted, clear it to unwant them.
          if wanted
            then do
              maybeProfileId <- liftIO $ withConnection connPool DB.getDefaultQualityProfileId
              case maybeProfileId of
                Nothing -> throw400 "No default quality profile configured"
                Just profileId ->
                  liftIO $ withConnection connPool $ \conn ->
                    forM_ albumIds $ \albumId ->
                      DB.updateCatalogAlbum conn albumId (Just (Just profileId))
            else
              liftIO $ withConnection connPool $ \conn ->
                forM_ albumIds $ \albumId ->
                  DB.updateCatalogAlbum conn albumId (Just Nothing)

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
    searchAlbumReleasesHandler :: Int64 -> AppM AlbumReleasesResponse
    searchAlbumReleasesHandler albumId = do

      -- Get the album from the database
      maybeAlbum <- liftIO $ withConnection connPool $ \conn ->
        DB.getCatalogAlbumById conn albumId

      case maybeAlbum of
        Nothing -> throw404 $ "Album not found: " <> show albumId
        Just album -> do
          let albumTitle = DBTypes.catalogAlbumTitle album
          let artistName = DBTypes.catalogAlbumArtistName album

          -- Read cached releases from the database. The cache is populated by
          -- the streaming search endpoint (on-demand) and the RSS monitor, so
          -- this returns instantly without hitting any indexer.
          cached <- liftIO $ withConnection connPool $ \conn ->
            DB.getCachedReleasesForAlbum conn albumId
          let releases = map cachedReleaseToResponse cached

          -- Compute wanted status using Core.Catalog logic
          -- Use effective profile (album's own, or inherited from artist)
          maybeProfile <- liftIO $ case DBTypes.catalogAlbumId album of
            Nothing -> pure Nothing
            Just albumId' -> DB.getEffectiveQualityProfile connPool albumId'

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
                , catalogAlbumResponseMatchedClusterId = Nothing
                , catalogAlbumResponseCurrentQuality = DBTypes.catalogAlbumCurrentQuality album
                , catalogAlbumResponseQualityProfileId = DBTypes.catalogAlbumQualityProfileId album
                , catalogAlbumResponseScore = Nothing
                , catalogAlbumResponseCreatedAt = fmap timestampToText (DBTypes.catalogAlbumCreatedAt album)
                , catalogAlbumResponseUpdatedAt = fmap timestampToText (DBTypes.catalogAlbumUpdatedAt album)
                }
            , albumReleasesReleases = releases
            , albumReleasesSearchTime = 0
            }

    -- Stream album releases via SSE (results sent as they arrive)
    streamAlbumReleasesHandler :: Int64 -> Maybe Text -> AppM (SourceIO ReleaseStreamEvent)
    streamAlbumReleasesHandler albumId maybeToken = do
      -- Validate JWT from query parameter (since EventSource doesn't support custom headers)
      currentCfg <- liftIO $ readConfig configVar
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
      maybeAlbum <- liftIO $ withConnection connPool $ \conn ->
        DB.getCatalogAlbumById conn albumId

      case maybeAlbum of
        Nothing -> throwError err404 { errBody = "Album not found" }
        Just album -> do
          let albumTitle = DBTypes.catalogAlbumTitle album
          let artistName = DBTypes.catalogAlbumArtistName album

          -- Read current config to get indexers and slskd
          config <- liftIO $ readConfig configVar
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
          pure $ streamSearchResults enabledIndexers maybeSlskdConfig searchQuery albumId

    -- Stream search results from all sources
    streamSearchResults ::
      [Cfg.Indexer] ->
      Maybe Cfg.SlskdConfig ->
      SearchQuery ->
      Int64 ->
      SourceIO ReleaseStreamEvent
    streamSearchResults indexers maybeSlskd query albumId = SourceT $ \k -> k $ Effect $ do
      -- The search engine lives in the download service; this handler only
      -- bridges its events into the SSE stream as response DTOs
      chan <- newTChanIO
      _ <- async $
        streamReleaseSearch (toServiceContext env) indexers maybeSlskd query albumId $ \ev ->
          STM.atomically $ writeTChan chan (toStreamDto ev)
      pure $ eventLoop chan

    toStreamDto :: StreamEvent -> ReleaseStreamEvent
    toStreamDto = \case
      StreamSearchStarted name -> SearchStarted name
      StreamReleaseFound name release -> ReleaseFound (releaseInfoToResponse name release) name
      StreamSearchCompleted name count -> SearchCompleted name count
      StreamSearchError name err -> SearchError name err
      StreamDone totalTime -> SearchDone totalTime

    eventLoop :: TChan ReleaseStreamEvent -> StepT IO ReleaseStreamEvent
    eventLoop chan = Effect $ do
      evt <- STM.atomically $ readTChan chan
      case evt of
        SearchDone _ -> pure $ Yield evt Stop
        _ -> pure $ Yield evt (eventLoop chan)

    -- Convert a cached release row (catalog_releases) to a ReleaseResponse
    cachedReleaseToResponse :: DBTypes.CachedReleaseRecord -> ReleaseResponse
    cachedReleaseToResponse r = ReleaseResponse
      { releaseResponseTitle = DBTypes.cachedReleaseTitle r
      , releaseResponseSource = DBTypes.cachedReleaseIndexerName r
      , releaseResponseQuality = DBTypes.cachedReleaseQuality r
      , releaseResponseSize = DBTypes.cachedReleaseSizeBytes r
      , releaseResponseSeeders = DBTypes.cachedReleaseSeeders r
      , releaseResponsePeers = DBTypes.cachedReleasePeers r
      , releaseResponseDownloadType = DBTypes.cachedReleaseDownloadType r
      , releaseResponseDownloadUrl = DBTypes.cachedReleaseDownloadUrl r
      , releaseResponsePublishDate = fmap timestampToText (DBTypes.cachedReleasePublishDate r)
      , releaseResponseSlskdUsername = DBTypes.cachedReleaseSlskdUsername r
      , releaseResponseSlskdFiles = decodeCachedSlskdFiles (DBTypes.cachedReleaseSlskdFiles r)
      }
      where
        decodeCachedSlskdFiles Nothing = Nothing
        decodeCachedSlskdFiles (Just json) =
          fmap (map toSlskdFileResponse)
            (decodeStrict (TE.encodeUtf8 json) :: Maybe [Indexer.SlskdFile])

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
