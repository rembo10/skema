{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Acquisition API handlers.
module Skema.API.Handlers.Acquisition
  ( acquisitionServer
  ) where

import Skema.API.Types.Acquisition (AcquisitionAPI, AcquisitionRuleResponse(..), AcquisitionSummaryResponse(..), SourceStatsResponse(..), CreateRuleRequest(..), UpdateRuleRequest(..))
import Skema.API.Types.Tasks (TaskRequest(..), TaskResponse(..), TaskResource(..))
import Skema.API.Handlers.Utils (throw400, withAuthDB)
import Skema.Auth (requireAuth)
import Skema.Auth.JWT (JWTSecret)
import Skema.Database.Connection
import qualified Skema.Database.Repository as DB
import qualified Skema.Database.Types as DBTypes
import qualified Skema.Database.Utils as DBUtils
import qualified Skema.Config.Types as Cfg
import Skema.Events.Bus (EventBus)
import Skema.MusicBrainz.Client (MBClientEnv)
import Skema.Services.SourceEvaluator (evaluateSource)
import Skema.Services.TaskManager (TaskManager)
import qualified Skema.Services.TaskManager as TM
import Data.Aeson (toJSON, object, (.=))
import Control.Concurrent.Async (async)
import Control.Exception (try)
import Servant
import Katip (LogEnv)
import Data.Time (UTCTime)

-- | Acquisition API handlers.
acquisitionServer :: LogEnv -> EventBus -> Cfg.ServerConfig -> JWTSecret -> ConnectionPool -> TVar Cfg.Config -> MBClientEnv -> TaskManager -> Server AcquisitionAPI
acquisitionServer le bus _serverCfg jwtSecret connPool configVar mbClient tm = \maybeAuthHeader ->
  getRulesHandler maybeAuthHeader
  :<|> createRuleHandler maybeAuthHeader
  :<|> updateRuleHandler maybeAuthHeader
  :<|> deleteRuleHandler maybeAuthHeader
  :<|> taskHandler maybeAuthHeader
  :<|> getSummaryHandler maybeAuthHeader
  where
    getRulesHandler :: Maybe Text -> Handler [AcquisitionRuleResponse]
    getRulesHandler authHeader =
      withAuthDB configVar jwtSecret connPool authHeader $ \conn -> do
        sources <- DB.getAllAcquisitionRules conn
        forM sources $ \source -> pure $ AcquisitionRuleResponse
          { acquisitionRuleResponseId = fromMaybe 0 (DBTypes.sourceId source)
          , acquisitionRuleResponseName = DBTypes.sourceName source
          , acquisitionRuleResponseDescription = DBTypes.sourceDescription source
          , acquisitionRuleResponseSourceType = DBUtils.sourceTypeToText (DBTypes.sourceType source)
          , acquisitionRuleResponseEnabled = DBTypes.sourceEnabled source
          , acquisitionRuleResponseFilters = DBTypes.sourceFilters source
          , acquisitionRuleResponseQualityProfileId = DBTypes.sourceQualityProfileId source
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
          (createRuleQualityProfileId req)

      -- Return the created source as a response
      pure $ AcquisitionRuleResponse
        { acquisitionRuleResponseId = newSourceId
        , acquisitionRuleResponseName = createRuleName req
        , acquisitionRuleResponseDescription = createRuleDescription req
        , acquisitionRuleResponseSourceType = createRuleType req
        , acquisitionRuleResponseEnabled = createRuleEnabled req
        , acquisitionRuleResponseFilters = createRuleFilters req
        , acquisitionRuleResponseQualityProfileId = createRuleQualityProfileId req
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
          (updateRuleQualityProfileId req)

      -- Return the updated source as a response
      pure $ AcquisitionRuleResponse
        { acquisitionRuleResponseId = sourceId
        , acquisitionRuleResponseName = updateRuleName req
        , acquisitionRuleResponseDescription = updateRuleDescription req
        , acquisitionRuleResponseSourceType = updateRuleType req
        , acquisitionRuleResponseEnabled = updateRuleEnabled req
        , acquisitionRuleResponseFilters = updateRuleFilters req
        , acquisitionRuleResponseQualityProfileId = updateRuleQualityProfileId req
        , acquisitionRuleResponseCreatedAt = ""  -- Preserve existing
        , acquisitionRuleResponseUpdatedAt = ""  -- Will be set by database
        }

    deleteRuleHandler :: Maybe Text -> Int64 -> Handler NoContent
    deleteRuleHandler authHeader sourceId = do
      withAuthDB configVar jwtSecret connPool authHeader $ \conn ->
        DB.deleteAcquisitionRule conn sourceId
      pure NoContent

    taskHandler :: Maybe Text -> TaskRequest -> Handler TaskResponse
    taskHandler authHeader req = do
      _ <- requireAuth configVar jwtSecret authHeader

      case taskRequestType req of
        "evaluate" -> liftIO $ do
          let sid = fromMaybe 0 (taskRequestResourceId req)
          taskResp <- TM.createTask tm AcquisitionResource (Just sid) "evaluate"
          let tid = taskResponseId taskResp

          _ <- async $ do
            TM.updateTaskProgress tm tid 0.1 (Just "Looking up source...")
            source <- withConnection connPool $ \conn ->
              DB.getAcquisitionRuleById conn sid
            case source of
              Nothing ->
                TM.failTask tm tid $ "Source not found: " <> show sid
              Just s -> do
                TM.updateTaskProgress tm tid 0.2 (Just $ "Evaluating source: " <> DBTypes.sourceName s)
                result <- try $ evaluateSource connPool bus le mbClient s
                case result of
                  Left (e :: SomeException) ->
                    TM.failTask tm tid $ "Evaluation failed: " <> show e
                  Right albumCount ->
                    TM.completeTask tm tid (Just $ toJSON $ object
                      [ "albums_added" .= albumCount
                      , "source_name" .= DBTypes.sourceName s
                      ])

          pure taskResp

        _ -> throwError err400 { errBody = "Unknown task type" }

    getSummaryHandler :: Maybe Text -> Handler AcquisitionSummaryResponse
    getSummaryHandler authHeader =
      withAuthDB configVar jwtSecret connPool authHeader $ \conn -> do
        (perSource, totalArtists, totalWanted) <- DB.getAcquisitionSummary conn
        pure $ AcquisitionSummaryResponse
          { acquisitionSummaryResponseSources = map (\(sid, ac, alc) ->
              SourceStatsResponse
                { sourceStatsResponseSourceId = sid
                , sourceStatsResponseArtistCount = ac
                , sourceStatsResponseAlbumCount = alc
                }
            ) perSource
          , acquisitionSummaryResponseTotalArtistsFollowed = totalArtists
          , acquisitionSummaryResponseTotalAlbumsWanted = totalWanted
          }
