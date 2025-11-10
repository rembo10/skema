{-# LANGUAGE OverloadedStrings #-}

-- | Diffs API handlers (includes both diffs and metadata-changes endpoints).
module Skema.API.Handlers.Diffs
  ( diffsServer
  ) where

import Skema.API.Types.Diffs (DiffsAPI, MetadataDiffResponse(..), GroupedDiffResponse(..), ApplyGroupedDiffRequest(..), ApplyToFileRequest(..), ApplyChangesRequest(..), MetadataChangeResponse(..))
import Skema.API.Handlers.Auth (throwJsonError)
import Skema.Auth (requireAuth)
import Skema.Auth.JWT (JWTSecret)
import Skema.Database.Connection
import qualified Skema.Database.Repository as DB
import qualified Skema.Database.Types as DBTypes
import qualified Skema.Config.Types as Cfg
import Skema.Services.Registry (ServiceRegistry)
import Skema.Events.Bus (EventBus)
import qualified Skema.Events.Bus as EventBus
import qualified Skema.Events.Types as Events
import qualified System.OsPath as OP
import qualified Data.Map.Strict as Map
import Servant
import Katip
import Data.Time (UTCTime)

-- | Throw a 500 Internal Server Error.
throw500 :: Text -> Handler a
throw500 = throwJsonError err500

-- | Diffs API handlers (includes both diffs and metadata-changes endpoints).
diffsServer :: LogEnv -> EventBus -> Cfg.ServerConfig -> JWTSecret -> ServiceRegistry -> ConnectionPool -> TVar Cfg.Config -> Server DiffsAPI
diffsServer le bus _serverCfg jwtSecret _registry connPool configVar =
  (\maybeAuthHeader ->
    getAllDiffsHandler maybeAuthHeader
    :<|> getGroupedDiffsHandler maybeAuthHeader
    :<|> applyGroupedHandler maybeAuthHeader
    :<|> applyToFileHandler maybeAuthHeader)
  :<|>
  (\maybeAuthHeader ->
    applyChangesHandler maybeAuthHeader
    :<|> getChangesHandler maybeAuthHeader
    :<|> revertChangeHandler maybeAuthHeader)
  where
    -- Diffs handlers
    getAllDiffsHandler :: Maybe Text -> Handler [MetadataDiffResponse]
    getAllDiffsHandler authHeader = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection connPool $ \conn -> do
        diffs <- DB.getAllMetadataDiffs conn
        forM diffs $ \(diff, path) -> do
          pathStr <- OP.decodeUtf path
          pure $ MetadataDiffResponse
            { diffId = fromMaybe 0 (DBTypes.diffId diff)
            , diffTrackId = DBTypes.diffTrackId diff
            , diffFilePath = toText pathStr
            , diffFieldName = DBTypes.diffFieldName diff
            , diffFileValue = DBTypes.diffFileValue diff
            , diffMBValue = DBTypes.diffMBValue diff
            , diffCreatedAt = maybe "" (show :: UTCTime -> Text) (DBTypes.diffCreatedAt diff)
            }

    getGroupedDiffsHandler :: Maybe Text -> Handler [GroupedDiffResponse]
    getGroupedDiffsHandler authHeader = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection connPool $ \conn -> do
        diffs <- DB.getAllMetadataDiffs conn
        diffResponses <- forM diffs $ \(diff, path) -> do
          pathStr <- OP.decodeUtf path
          pure $ MetadataDiffResponse
            { diffId = fromMaybe 0 (DBTypes.diffId diff)
            , diffTrackId = DBTypes.diffTrackId diff
            , diffFilePath = toText pathStr
            , diffFieldName = DBTypes.diffFieldName diff
            , diffFileValue = DBTypes.diffFileValue diff
            , diffMBValue = DBTypes.diffMBValue diff
            , diffCreatedAt = maybe "" (show :: UTCTime -> Text) (DBTypes.diffCreatedAt diff)
            }

        -- Group diffs by (fieldName, fileValue, mbValue)
        let grouped = Map.fromListWith (<>)
              [ ((diffFieldName dr, diffFileValue dr, diffMBValue dr), [dr])
              | dr <- diffResponses
              ]

        pure $ map (\((fieldName, fileValue, mbValue), drs) ->
          GroupedDiffResponse
            { groupedFieldName = fieldName
            , groupedFileValue = fileValue
            , groupedMBValue = mbValue
            , groupedCount = length drs
            , groupedTrackIds = map diffTrackId drs
            , groupedDiffs = drs
            }) (Map.toList grouped)

    applyGroupedHandler :: Maybe Text -> ApplyGroupedDiffRequest -> Handler ()
    applyGroupedHandler authHeader req = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection connPool $ \conn -> do
        _ <- DB.applyGroupedMetadataDiff conn (applyFieldName req) (applyFileValue req) (applyMBValue req)
        pure ()

    applyToFileHandler :: Maybe Text -> ApplyToFileRequest -> Handler ()
    applyToFileHandler authHeader req = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection connPool $ \conn -> do
        DB.applyMetadataChange conn (applyToTrackId req) (applyToFieldName req) (applyToValue req)

    -- Metadata changes handlers
    applyChangesHandler :: Maybe Text -> ApplyChangesRequest -> Handler [MetadataChangeResponse]
    applyChangesHandler authHeader req = do
      _ <- requireAuth configVar jwtSecret authHeader
      let diffIds = applyChangesDiffIds req

      -- Publish MetadataWriteRequested event for async processing
      liftIO $ EventBus.publishAndLog bus le "api" $ Events.MetadataWriteRequested
        { Events.writeDiffIds = diffIds
        }

      -- Return empty list immediately - client will receive updates via SSE
      pure []

    getChangesHandler :: Maybe Text -> Handler [MetadataChangeResponse]
    getChangesHandler authHeader = do
      _ <- requireAuth configVar jwtSecret authHeader
      liftIO $ withConnection connPool $ \conn -> do
        changes <- DB.getMetadataChanges conn False  -- False = get all changes, not just active
        forM changes $ \(change, path) -> do
          pathStr <- OP.decodeUtf path
          pure $ MetadataChangeResponse
            { changeResponseId = fromMaybe 0 (DBTypes.changeId change)
            , changeResponseTrackId = DBTypes.changeTrackId change
            , changeResponseFilePath = toText pathStr
            , changeResponseFieldName = DBTypes.changeFieldName change
            , changeResponseOldValue = DBTypes.changeOldValue change
            , changeResponseNewValue = DBTypes.changeNewValue change
            , changeResponseAppliedAt = maybe "" (show :: UTCTime -> Text) (DBTypes.changeAppliedAt change)
            , changeResponseReverted = isJust (DBTypes.changeRevertedAt change)
            }

    revertChangeHandler :: Maybe Text -> Int64 -> Handler NoContent
    revertChangeHandler authHeader changeId = do
      _ <- requireAuth configVar jwtSecret authHeader
      result <- liftIO $ DB.revertMetadataChange connPool changeId
      case result of
        Left err -> throw500 $ "Failed to revert change: " <> err
        Right () -> pure NoContent
