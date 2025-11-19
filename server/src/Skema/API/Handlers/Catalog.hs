{-# LANGUAGE OverloadedStrings #-}

-- | Catalog API handlers.
module Skema.API.Handlers.Catalog
  ( catalogServer
  ) where

import Skema.API.Types.Catalog (CatalogAPI, CatalogQueryRequest(..), CatalogQueryResponse(..), CatalogArtistResponse(..), CatalogAlbumResponse(..), CreateCatalogArtistRequest(..), UpdateCatalogArtistRequest(..), CreateCatalogAlbumRequest(..), UpdateCatalogAlbumRequest(..))
import Skema.API.Types.Events (EventResponse(..))
import Skema.API.Handlers.Auth (throwJsonError)
import Skema.API.Handlers.Utils (withAuthDB)
import Skema.Auth (requireAuth)
import Skema.Auth.JWT (JWTSecret)
import Skema.Database.Connection
import qualified Skema.Database.Repository as DB
import qualified Skema.Database.Types as DBTypes
import qualified Skema.Config.Types as Cfg
import Skema.Services.Registry (ServiceRegistry(..))
import Skema.MusicBrainz.Client (searchArtists, searchReleaseGroups, prettyClientError)
import Skema.MusicBrainz.Types (mbasArtists, mbasArtistId, mbrgsReleaseGroups, mbrgsReleaseGroupId, unMBID)
import Skema.Domain.Converters (mbArtistSearchToCatalogResponse, mbReleaseGroupSearchToCatalogResponse)
import Skema.Events.Bus (EventBus)
import qualified Skema.Events.Bus as EventBus
import qualified Skema.Events.Types as Events
import Servant
import Katip
import Data.Time (UTCTime)

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
catalogServer :: LogEnv -> EventBus -> Cfg.ServerConfig -> JWTSecret -> ServiceRegistry -> ConnectionPool -> FilePath -> TVar Cfg.Config -> Server CatalogAPI
catalogServer le bus _serverCfg jwtSecret registry connPool _cacheDir configVar = \maybeAuthHeader ->
  queryHandler maybeAuthHeader
  :<|> getArtistsHandler maybeAuthHeader
  :<|> createArtistHandler maybeAuthHeader
  :<|> updateArtistHandler maybeAuthHeader
  :<|> deleteArtistHandler maybeAuthHeader
  :<|> refreshArtistHandler maybeAuthHeader
  :<|> refreshAllHandler maybeAuthHeader
  :<|> getAlbumsHandler maybeAuthHeader
  :<|> createAlbumHandler maybeAuthHeader
  :<|> updateAlbumHandler maybeAuthHeader
  :<|> deleteAlbumHandler maybeAuthHeader
  where
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
    getArtistsHandler :: Maybe Text -> Maybe Bool -> Handler [CatalogArtistResponse]
    getArtistsHandler authHeader maybeFollowed =
      withAuthDB configVar jwtSecret connPool authHeader $ \conn -> do
        artists <- DB.getCatalogArtists conn maybeFollowed
        forM artists $ \artist -> pure $ CatalogArtistResponse
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
        artists <- DB.getCatalogArtists conn Nothing
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
          }

    -- Delete catalog artist
    deleteArtistHandler :: Maybe Text -> Int64 -> Handler NoContent
    deleteArtistHandler authHeader artistId = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection connPool $ \conn ->
        DB.deleteCatalogArtist conn artistId
      pure NoContent

    -- Refresh single artist's catalog
    refreshArtistHandler :: Maybe Text -> Int64 -> Handler EventResponse
    refreshArtistHandler authHeader artistId = do
      _ <- requireAuth configVar jwtSecret authHeader
      -- Get artist by ID to find MBID
      maybeArtist <- liftIO $ withConnection connPool $ \conn -> do
        artists <- DB.getCatalogArtists conn Nothing
        pure $ find (\a -> DBTypes.catalogArtistId a == Just artistId) artists
      case maybeArtist of
        Nothing -> throw404 $ "Artist not found: " <> show artistId
        Just artist -> do
          -- Emit refresh event
          liftIO $ EventBus.publishAndLog bus le "api.catalog" $ Events.CatalogArtistRefreshRequested
            { Events.refreshArtistMBID = DBTypes.catalogArtistMBID artist
            }
          pure $ EventResponse
            { eventResponseSuccess = True
            , eventResponseMessage = "Artist catalog refresh requested"
            }

    -- Refresh all followed artists' catalogs
    refreshAllHandler :: Maybe Text -> Handler EventResponse
    refreshAllHandler authHeader = do
      _ <- requireAuth configVar jwtSecret authHeader
      -- Get all followed artists
      followedArtists <- liftIO $ withConnection connPool $ \conn ->
        DB.getCatalogArtists conn (Just True)
      -- Emit refresh event for each
      liftIO $ forM_ followedArtists $ \artist ->
        EventBus.publishAndLog bus le "api.catalog" $ Events.CatalogArtistRefreshRequested
          { Events.refreshArtistMBID = DBTypes.catalogArtistMBID artist
          }
      pure $ EventResponse
        { eventResponseSuccess = True
        , eventResponseMessage = "Catalog refresh requested for " <> show (length followedArtists) <> " artists"
        }

    -- Get catalog albums
    getAlbumsHandler :: Maybe Text -> Maybe Bool -> Maybe Int64 -> Handler [CatalogAlbumResponse]
    getAlbumsHandler authHeader maybeWanted maybeArtistId = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection connPool $ \conn -> do
        albums <- case maybeArtistId of
          Nothing -> DB.getCatalogAlbums conn maybeWanted
          Just artistId -> DB.getCatalogAlbumsByArtistId conn artistId maybeWanted
        forM albums $ \album -> pure $ CatalogAlbumResponse
          { catalogAlbumResponseId = DBTypes.catalogAlbumId album
          , catalogAlbumResponseReleaseGroupMBID = DBTypes.catalogAlbumReleaseGroupMBID album
          , catalogAlbumResponseTitle = DBTypes.catalogAlbumTitle album
          , catalogAlbumResponseArtistMBID = DBTypes.catalogAlbumArtistMBID album
          , catalogAlbumResponseArtistName = DBTypes.catalogAlbumArtistName album
          , catalogAlbumResponseType = DBTypes.catalogAlbumType album
          , catalogAlbumResponseFirstReleaseDate = DBTypes.catalogAlbumFirstReleaseDate album
          , catalogAlbumResponseCoverUrl = DBTypes.catalogAlbumCoverUrl album
          , catalogAlbumResponseCoverThumbnailUrl = DBTypes.catalogAlbumCoverThumbnailUrl album
          , catalogAlbumResponseWanted = DBTypes.catalogAlbumWanted album
          , catalogAlbumResponseMatchedClusterId = DBTypes.catalogAlbumMatchedClusterId album
          , catalogAlbumResponseQualityProfileId = DBTypes.catalogAlbumQualityProfileId album
          , catalogAlbumResponseScore = Nothing  -- No score in database records
          , catalogAlbumResponseCreatedAt = fmap (show :: UTCTime -> Text) (DBTypes.catalogAlbumCreatedAt album)
          , catalogAlbumResponseUpdatedAt = fmap (show :: UTCTime -> Text) (DBTypes.catalogAlbumUpdatedAt album)
          }

    -- Create/upsert catalog album
    createAlbumHandler :: Maybe Text -> CreateCatalogAlbumRequest -> Handler CatalogAlbumResponse
    createAlbumHandler authHeader req = do
      _ <- requireAuth configVar jwtSecret authHeader

      -- First, ensure the artist exists in catalog_artists (or create it)
      -- Skip Various Artists - albums can still be created without artist entry
      artistId <- liftIO $ withConnection connPool $ \conn -> do
        if DBTypes.isVariousArtists (createCatalogAlbumArtistMBID req)
          then do
            -- For Various Artists, we'll use a sentinel ID of 0 (or create a special entry)
            -- Actually, let's just return error - Various Artists albums shouldn't have artist_id
            error "Cannot create catalog album for Various Artists - use a compilation source instead"
          else do
            maybeArtist <- DB.getCatalogArtistByMBID conn (createCatalogAlbumArtistMBID req)
            case maybeArtist of
              Just artist -> case DBTypes.catalogArtistId artist of
                Just aid -> pure aid
                Nothing -> error "Artist record has no ID"
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
          (createCatalogAlbumWanted req)
          Nothing  -- matched_cluster_id is not set on creation

      -- Fetch the created/updated album
      maybeAlbum <- liftIO $ withConnection connPool $ \conn ->
        DB.getCatalogAlbumByReleaseGroupMBID conn (createCatalogAlbumReleaseGroupMBID req)

      case maybeAlbum of
        Nothing -> throw500 "Failed to retrieve created album"
        Just album -> pure $ CatalogAlbumResponse
          { catalogAlbumResponseId = DBTypes.catalogAlbumId album
          , catalogAlbumResponseReleaseGroupMBID = DBTypes.catalogAlbumReleaseGroupMBID album
          , catalogAlbumResponseTitle = DBTypes.catalogAlbumTitle album
          , catalogAlbumResponseArtistMBID = DBTypes.catalogAlbumArtistMBID album
          , catalogAlbumResponseArtistName = DBTypes.catalogAlbumArtistName album
          , catalogAlbumResponseType = DBTypes.catalogAlbumType album
          , catalogAlbumResponseFirstReleaseDate = DBTypes.catalogAlbumFirstReleaseDate album
          , catalogAlbumResponseCoverUrl = DBTypes.catalogAlbumCoverUrl album
          , catalogAlbumResponseCoverThumbnailUrl = DBTypes.catalogAlbumCoverThumbnailUrl album
          , catalogAlbumResponseWanted = DBTypes.catalogAlbumWanted album
          , catalogAlbumResponseMatchedClusterId = DBTypes.catalogAlbumMatchedClusterId album
          , catalogAlbumResponseQualityProfileId = DBTypes.catalogAlbumQualityProfileId album
          , catalogAlbumResponseScore = Nothing
          , catalogAlbumResponseCreatedAt = fmap (show :: UTCTime -> Text) (DBTypes.catalogAlbumCreatedAt album)
          , catalogAlbumResponseUpdatedAt = fmap (show :: UTCTime -> Text) (DBTypes.catalogAlbumUpdatedAt album)
          }

    -- Update catalog album (wanted status and quality profile)
    updateAlbumHandler :: Maybe Text -> Int64 -> UpdateCatalogAlbumRequest -> Handler CatalogAlbumResponse
    updateAlbumHandler authHeader albumId req = do
      _ <- requireAuth configVar jwtSecret authHeader

      let newWantedStatus = updateCatalogAlbumWanted req

      -- Fetch the album BEFORE updating to get current status
      maybeAlbumBefore <- liftIO $ withConnection connPool $ \conn -> do
        albums <- DB.getCatalogAlbums conn Nothing
        pure $ find (\a -> DBTypes.catalogAlbumId a == Just albumId) albums

      case maybeAlbumBefore of
        Nothing -> throw404 $ "Album not found: " <> show albumId
        Just albumBefore -> do
          let wasWanted = DBTypes.catalogAlbumWanted albumBefore

          -- Update the wanted status and quality profile
          liftIO $ withConnection connPool $ \conn ->
            DB.updateCatalogAlbum conn albumId newWantedStatus
              (updateCatalogAlbumQualityProfileId req)

          -- Emit WantedAlbumAdded event if album is being marked as wanted (not unwanted)
          when (newWantedStatus && not wasWanted) $ do
            liftIO $ EventBus.publishAndLog bus le "api.catalog" $ Events.WantedAlbumAdded
              { Events.wantedCatalogAlbumId = albumId
              , Events.wantedReleaseGroupId = DBTypes.catalogAlbumReleaseGroupMBID albumBefore
              , Events.wantedAlbumTitle = DBTypes.catalogAlbumTitle albumBefore
              , Events.wantedArtistName = DBTypes.catalogAlbumArtistName albumBefore
              }

          -- Fetch the updated album
          maybeAlbum <- liftIO $ withConnection connPool $ \conn -> do
            albums <- DB.getCatalogAlbums conn Nothing
            pure $ find (\a -> DBTypes.catalogAlbumId a == Just albumId) albums

          case maybeAlbum of
            Nothing -> throw404 $ "Album not found: " <> show albumId
            Just album -> pure $ CatalogAlbumResponse
              { catalogAlbumResponseId = DBTypes.catalogAlbumId album
              , catalogAlbumResponseReleaseGroupMBID = DBTypes.catalogAlbumReleaseGroupMBID album
              , catalogAlbumResponseTitle = DBTypes.catalogAlbumTitle album
              , catalogAlbumResponseArtistMBID = DBTypes.catalogAlbumArtistMBID album
              , catalogAlbumResponseArtistName = DBTypes.catalogAlbumArtistName album
              , catalogAlbumResponseType = DBTypes.catalogAlbumType album
              , catalogAlbumResponseFirstReleaseDate = DBTypes.catalogAlbumFirstReleaseDate album
              , catalogAlbumResponseCoverUrl = DBTypes.catalogAlbumCoverUrl album
              , catalogAlbumResponseCoverThumbnailUrl = DBTypes.catalogAlbumCoverThumbnailUrl album
              , catalogAlbumResponseWanted = DBTypes.catalogAlbumWanted album
              , catalogAlbumResponseMatchedClusterId = DBTypes.catalogAlbumMatchedClusterId album
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
