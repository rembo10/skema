{-# LANGUAGE OverloadedStrings #-}

-- | Diffs API handlers (includes both diffs and metadata-changes endpoints).
module Skema.API.Handlers.Diffs
  ( diffsServer
  ) where

import Skema.API.Types.Diffs (DiffsAPI, MetadataDiffResponse(..), GroupedDiffResponse(..), GroupedDiffsResponse(..), DiffsPagination(..), ApplyChangesRequest(..), MetadataChangeResponse(..), MetadataChangesResponse(..))
import Skema.API.Handlers.Utils (throw500, parsePagination, withDB)
import Skema.Database.Connection
import qualified Skema.Database.Repository as DB
import qualified Skema.Database.Types as DBTypes
import Skema.FileSystem.Utils (osPathToString)
import qualified Skema.Events.Bus as EventBus
import qualified Skema.Events.Types as Events
import qualified Data.Map.Strict as Map
import Servant
import Skema.API.App (Env(..), AppM)
import Skema.API.Types.Common (timestampToText)

-- | Diffs API handlers (includes both diffs and metadata-changes endpoints).
diffsServer :: Env -> ServerT DiffsAPI AppM
diffsServer Env{envLogEnv = le, envEventBus = bus, envDbPool = connPool} =
  ( getAllDiffsHandler
    :<|> getGroupedDiffsHandler)
  :<|>
  ( applyChangesHandler
    :<|> getChangesHandler
    :<|> revertChangeHandler)
  where
    -- Diffs handlers
    getAllDiffsHandler :: AppM [MetadataDiffResponse]
    getAllDiffsHandler =
      withDB connPool $ \conn -> do
        diffs <- DB.getAllMetadataDiffs conn
        forM diffs $ \(diff, path) -> do
          pathStr <- osPathToString path
          pure $ MetadataDiffResponse
            { diffId = fromMaybe 0 (DBTypes.diffId diff)
            , diffTrackId = DBTypes.diffTrackId diff
            , diffFilePath = toText pathStr
            , diffFieldName = DBTypes.diffFieldName diff
            , diffFileValue = DBTypes.diffFileValue diff
            , diffMBValue = DBTypes.diffMBValue diff
            , diffCreatedAt = maybe "" timestampToText (DBTypes.diffCreatedAt diff)
            }

    getGroupedDiffsHandler :: Maybe Int -> Maybe Int -> AppM GroupedDiffsResponse
    getGroupedDiffsHandler maybeOffset maybeLimit = do
      let (offset, limit) = parsePagination maybeOffset maybeLimit

      liftIO $ withConnection connPool $ \conn -> do
        diffs <- DB.getAllMetadataDiffs conn
        diffResponses <- forM diffs $ \(diff, path) -> do
          pathStr <- osPathToString path
          pure $ MetadataDiffResponse
            { diffId = fromMaybe 0 (DBTypes.diffId diff)
            , diffTrackId = DBTypes.diffTrackId diff
            , diffFilePath = toText pathStr
            , diffFieldName = DBTypes.diffFieldName diff
            , diffFileValue = DBTypes.diffFileValue diff
            , diffMBValue = DBTypes.diffMBValue diff
            , diffCreatedAt = maybe "" timestampToText (DBTypes.diffCreatedAt diff)
            }

        -- Group diffs by (fieldName, fileValue, mbValue)
        let grouped = Map.fromListWith (<>)
              [ ((diffFieldName dr, diffFileValue dr, diffMBValue dr), [dr])
              | dr <- diffResponses
              ]

        let allGrouped = map (\((fieldName, fileValue, mbValue), drs) ->
              GroupedDiffResponse
                { groupedFieldName = fieldName
                , groupedFileValue = fileValue
                , groupedMBValue = mbValue
                , groupedCount = length drs
                , groupedTrackIds = map diffTrackId drs
                , groupedDiffs = drs
                }) (Map.toList grouped)

        let total = length allGrouped
        let paginated = take limit $ drop offset $ allGrouped

        pure $ GroupedDiffsResponse
          { groupedDiffsResponsePagination = DiffsPagination
              { diffsPaginationTotal = total
              , diffsPaginationOffset = offset
              , diffsPaginationLimit = limit
              }
          , groupedDiffsResponseDiffs = paginated
          }

    -- Metadata changes handlers
    applyChangesHandler :: ApplyChangesRequest -> AppM [MetadataChangeResponse]
    applyChangesHandler req = do
      let diffIds = applyChangesDiffIds req

      -- Publish MetadataWriteRequested event for async processing
      liftIO $ EventBus.publishAndLog bus le "api" $ Events.MetadataWriteRequested
        { Events.writeDiffIds = diffIds
        }

      -- Return empty list immediately - client will receive updates via SSE
      pure []

    getChangesHandler :: Maybe Int -> Maybe Int -> AppM MetadataChangesResponse
    getChangesHandler maybeOffset maybeLimit = do
      let (offset, limit) = parsePagination maybeOffset maybeLimit

      liftIO $ withConnection connPool $ \conn -> do
        changes <- DB.getMetadataChanges conn False  -- False = get all changes, not just active
        changeResponses <- forM changes $ \(change, path) -> do
          pathStr <- osPathToString path
          pure $ MetadataChangeResponse
            { changeResponseId = fromMaybe 0 (DBTypes.changeId change)
            , changeResponseTrackId = DBTypes.changeTrackId change
            , changeResponseFilePath = toText pathStr
            , changeResponseFieldName = DBTypes.changeFieldName change
            , changeResponseOldValue = DBTypes.changeOldValue change
            , changeResponseNewValue = DBTypes.changeNewValue change
            , changeResponseAppliedAt = maybe "" timestampToText (DBTypes.changeAppliedAt change)
            , changeResponseReverted = isJust (DBTypes.changeRevertedAt change)
            }

        let total = length changeResponses
        let paginated = take limit $ drop offset $ changeResponses

        pure $ MetadataChangesResponse
          { metadataChangesResponsePagination = DiffsPagination
              { diffsPaginationTotal = total
              , diffsPaginationOffset = offset
              , diffsPaginationLimit = limit
              }
          , metadataChangesResponseChanges = paginated
          }

    revertChangeHandler :: Int64 -> AppM NoContent
    revertChangeHandler changeId = do
      result <- liftIO $ DB.revertMetadataChange connPool changeId
      case result of
        Left err -> throw500 $ "Failed to revert change: " <> err
        Right () -> pure NoContent
