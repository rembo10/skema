{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Acquisition API handlers.
module Skema.API.Handlers.Acquisition
  ( acquisitionServer
  ) where

import Skema.API.Types.Acquisition (AcquisitionAPI, AcquisitionRuleResponse(..), AcquisitionSummaryResponse(..), SourceStatsResponse(..), CreateRuleRequest(..), UpdateRuleRequest(..))
import Skema.API.Types.Tasks (TaskRequest(..), TaskResponse(..), TaskResource(..))
import Skema.API.Handlers.Utils (throw400, withDB)
import Skema.Database.Connection
import qualified Skema.Database.Repository as DB
import qualified Skema.Database.Types as DBTypes
import qualified Skema.Database.Utils as DBUtils
import Skema.Services.SourceEvaluator (evaluateSource)
import qualified Skema.Services.TaskManager as TM
import Data.Aeson (toJSON, object, (.=))
import Servant
import Skema.API.App (Env(..), AppM)
import Skema.API.Types.Common (timestampToText)

-- | Acquisition API handlers.
acquisitionServer :: Env -> ServerT AcquisitionAPI AppM
acquisitionServer Env{envLogEnv = le, envEventBus = bus, envDbPool = connPool, envMBClient = mbClient, envHttpClient = httpClient, envTaskManager = tm} =
  getRulesHandler
  :<|> createRuleHandler
  :<|> updateRuleHandler
  :<|> deleteRuleHandler
  :<|> taskHandler
  :<|> getSummaryHandler
  where
    getRulesHandler :: AppM [AcquisitionRuleResponse]
    getRulesHandler =
      withDB connPool $ \conn -> do
        sources <- DB.getAllAcquisitionRules conn
        forM sources $ \source -> pure $ AcquisitionRuleResponse
          { acquisitionRuleResponseId = fromMaybe 0 (DBTypes.sourceId source)
          , acquisitionRuleResponseName = DBTypes.sourceName source
          , acquisitionRuleResponseDescription = DBTypes.sourceDescription source
          , acquisitionRuleResponseSourceType = DBUtils.sourceTypeToText (DBTypes.sourceType source)
          , acquisitionRuleResponseEnabled = DBTypes.sourceEnabled source
          , acquisitionRuleResponseFilters = DBTypes.sourceFilters source
          , acquisitionRuleResponseQualityProfileId = DBTypes.sourceQualityProfileId source
          , acquisitionRuleResponseCreatedAt = maybe "" timestampToText (DBTypes.sourceCreatedAt source)
          , acquisitionRuleResponseUpdatedAt = maybe "" timestampToText (DBTypes.sourceUpdatedAt source)
          }

    createRuleHandler :: CreateRuleRequest -> AppM AcquisitionRuleResponse
    createRuleHandler req = do
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

    updateRuleHandler :: Int64 -> UpdateRuleRequest -> AppM AcquisitionRuleResponse
    updateRuleHandler sourceId req = do
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

    deleteRuleHandler :: Int64 -> AppM NoContent
    deleteRuleHandler sourceId = do
      withDB connPool $ \conn ->
        DB.deleteAcquisitionRule conn sourceId
      pure NoContent

    taskHandler :: TaskRequest -> AppM TaskResponse
    taskHandler req = do
      case taskRequestType req of
        "evaluate" -> do
          let sid = fromMaybe 0 (taskRequestResourceId req)
          TM.startTask tm AcquisitionResource (Just sid) "evaluate" $ \tid -> do
            TM.updateTaskProgress tm tid 0.1 (Just "Looking up source...")
            source <- withConnection connPool $ \conn ->
              DB.getAcquisitionRuleById conn sid
            case source of
              Nothing ->
                TM.failTask tm tid $ "Source not found: " <> show sid
              Just s -> do
                TM.updateTaskProgress tm tid 0.2 (Just $ "Evaluating source: " <> DBTypes.sourceName s)
                albumCount <- evaluateSource connPool bus le mbClient httpClient s
                TM.completeTask tm tid (Just $ toJSON $ object
                  [ "albums_added" .= albumCount
                  , "source_name" .= DBTypes.sourceName s
                  ])

        _ -> throw400 "Unknown task type"

    getSummaryHandler :: AppM AcquisitionSummaryResponse
    getSummaryHandler =
      withDB connPool $ \conn -> do
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
