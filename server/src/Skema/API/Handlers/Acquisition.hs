{-# LANGUAGE OverloadedStrings #-}

-- | Acquisition API handlers.
module Skema.API.Handlers.Acquisition
  ( acquisitionServer
  ) where

import Skema.API.Types.Acquisition (AcquisitionAPI, AcquisitionRuleResponse(..), CreateRuleRequest(..), UpdateRuleRequest(..), SyncResponse(..))
import Skema.API.Handlers.Auth (throwJsonError)
import Skema.Auth (requireAuth)
import Skema.Auth.JWT (JWTSecret)
import Skema.Database.Connection
import qualified Skema.Database.Repository as DB
import qualified Skema.Database.Types as DBTypes
import qualified Skema.Database.Utils as DBUtils
import qualified Skema.Config.Types as Cfg
import Skema.Events.Bus (EventBus)
import Skema.Services.Registry (ServiceRegistry(..))
import Skema.Services.SourceEvaluator (evaluateSingleSource)
import Servant
import Data.Time (UTCTime, getCurrentTime)

-- | Throw a 400 Bad Request error.
throw400 :: Text -> Handler a
throw400 = throwJsonError err400

-- | Acquisition API handlers.
acquisitionServer :: EventBus -> Cfg.ServerConfig -> JWTSecret -> ServiceRegistry -> ConnectionPool -> TVar Cfg.Config -> Server AcquisitionAPI
acquisitionServer bus _serverCfg jwtSecret registry connPool configVar = \maybeAuthHeader ->
  getRulesHandler maybeAuthHeader
  :<|> createRuleHandler maybeAuthHeader
  :<|> updateRuleHandler maybeAuthHeader
  :<|> deleteRuleHandler maybeAuthHeader
  :<|> enableRuleHandler maybeAuthHeader
  :<|> disableRuleHandler maybeAuthHeader
  :<|> syncRuleHandler maybeAuthHeader
  where
    getRulesHandler :: Maybe Text -> Handler [AcquisitionRuleResponse]
    getRulesHandler authHeader = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection connPool $ \conn -> do
        sources <- DB.getAllAcquisitionRules conn
        forM sources $ \source -> pure $ AcquisitionRuleResponse
          { acquisitionRuleResponseId = fromMaybe 0 (DBTypes.sourceId source)
          , acquisitionRuleResponseName = DBTypes.sourceName source
          , acquisitionRuleResponseDescription = DBTypes.sourceDescription source
          , acquisitionRuleResponseSourceType = DBUtils.sourceTypeToText (DBTypes.sourceType source)
          , acquisitionRuleResponseEnabled = DBTypes.sourceEnabled source
          , acquisitionRuleResponseFilters = DBTypes.sourceFilters source
          , acquisitionRuleResponseCreatedAt = maybe "" (show :: UTCTime -> Text) (DBTypes.sourceCreatedAt source)
          , acquisitionRuleResponseUpdatedAt = maybe "" (show :: UTCTime -> Text) (DBTypes.sourceUpdatedAt source)
          }

    createRuleHandler :: Maybe Text -> CreateRuleRequest -> Handler AcquisitionRuleResponse
    createRuleHandler authHeader req = do
      _ <- requireAuth configVar jwtSecret authHeader
      -- Parse and validate source type
      sourceType <- case DBUtils.textToSourceType (createRuleType req) of
        Nothing -> throw400 $ "Invalid source type: " <> createRuleType req
        Just st -> pure st

      -- Create the source in the database
      newSourceId <- liftIO $ withConnection connPool $ \conn ->
        DB.createAcquisitionRule
          conn
          (createRuleName req)
          (createRuleDescription req)
          sourceType
          (createRuleEnabled req)
          (createRuleFilters req)

      -- Return the created source as a response
      pure $ AcquisitionRuleResponse
        { acquisitionRuleResponseId = newSourceId
        , acquisitionRuleResponseName = createRuleName req
        , acquisitionRuleResponseDescription = createRuleDescription req
        , acquisitionRuleResponseSourceType = createRuleType req
        , acquisitionRuleResponseEnabled = createRuleEnabled req
        , acquisitionRuleResponseFilters = createRuleFilters req
        , acquisitionRuleResponseCreatedAt = ""  -- Will be set by database
        , acquisitionRuleResponseUpdatedAt = ""  -- Will be set by database
        }

    updateRuleHandler :: Maybe Text -> Int64 -> UpdateRuleRequest -> Handler AcquisitionRuleResponse
    updateRuleHandler authHeader sourceId req = do
      _ <- requireAuth configVar jwtSecret authHeader
      -- Parse and validate source type
      sourceType <- case DBUtils.textToSourceType (updateRuleType req) of
        Nothing -> throw400 $ "Invalid source type: " <> updateRuleType req
        Just st -> pure st

      -- Update the source in the database
      liftIO $ withConnection connPool $ \conn ->
        DB.updateAcquisitionRule
          conn
          sourceId
          (updateRuleName req)
          (updateRuleDescription req)
          sourceType
          (updateRuleEnabled req)
          (updateRuleFilters req)

      -- Return the updated source as a response
      pure $ AcquisitionRuleResponse
        { acquisitionRuleResponseId = sourceId
        , acquisitionRuleResponseName = updateRuleName req
        , acquisitionRuleResponseDescription = updateRuleDescription req
        , acquisitionRuleResponseSourceType = updateRuleType req
        , acquisitionRuleResponseEnabled = updateRuleEnabled req
        , acquisitionRuleResponseFilters = updateRuleFilters req
        , acquisitionRuleResponseCreatedAt = ""  -- Preserve existing
        , acquisitionRuleResponseUpdatedAt = ""  -- Will be set by database
        }

    deleteRuleHandler :: Maybe Text -> Int64 -> Handler NoContent
    deleteRuleHandler authHeader sourceId = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection connPool $ \conn ->
        DB.deleteAcquisitionRule conn sourceId
      pure NoContent

    enableRuleHandler :: Maybe Text -> Int64 -> Handler NoContent
    enableRuleHandler authHeader sourceId = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection connPool $ \conn -> do
        now <- getCurrentTime
        executeQuery conn
          "UPDATE acquisition_rules SET enabled = ?, updated_at = ? WHERE id = ?"
          (True, now, sourceId)
      pure NoContent

    disableRuleHandler :: Maybe Text -> Int64 -> Handler NoContent
    disableRuleHandler authHeader sourceId = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection connPool $ \conn -> do
        now <- getCurrentTime
        executeQuery conn
          "UPDATE acquisition_rules SET enabled = ?, updated_at = ? WHERE id = ?"
          (False, now, sourceId)
      pure NoContent

    syncRuleHandler :: Maybe Text -> Int64 -> Handler SyncResponse
    syncRuleHandler authHeader sourceId = do
      _ <- requireAuth configVar jwtSecret authHeader
      let mbEnv = srMBClientEnv registry
      result <- liftIO $ evaluateSingleSource connPool bus mbEnv sourceId
      case result of
        Left err -> pure $ SyncResponse False err 0
        Right count -> pure $ SyncResponse True ("Sync completed successfully") count
