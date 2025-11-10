{-# LANGUAGE OverloadedStrings #-}

-- | Events API handler (Server-Sent Events).
-- GET: Streams events from the event bus to clients in real-time.
-- POST: Accepts event submissions from clients.
module Skema.API.Handlers.Events
  ( eventsServer
  , streamEvents
  , loadStats
  , convertEvent
  ) where

import Skema.API.Types.Events (EventsAPI, ServerEvent(..), EventRequest(..), EventResponse(..))
import Skema.API.Types.Common (SourceIO)
import Skema.API.Types.Library (LibraryStats(..))
import Skema.Auth (checkAuthEnabled, requireAuth)
import Skema.Auth.JWT (JWTSecret, validateJWT)
import Skema.Database.Connection
import Skema.Database.Repository (getLibraryStats)
import qualified Skema.Config.Types as Cfg
import Skema.Events.Bus (EventBus)
import qualified Skema.Events.Bus as EventBus
import qualified Skema.Events.Types as Events
import qualified System.OsPath as OP
import Servant hiding (SourceIO)
import Servant.Types.SourceT (SourceT(..), StepT(..))
import Katip
import Data.Aeson ((.:), withObject, toJSON)
import Data.Aeson.Types (parseMaybe)
import Control.Concurrent.STM (readTChan)
import qualified Control.Concurrent.STM as STM

-- | Events API handler (Server-Sent Events).
eventsServer :: LogEnv -> EventBus -> Cfg.ServerConfig -> JWTSecret -> ConnectionPool -> Maybe Text -> TVar Cfg.Config -> Server EventsAPI
eventsServer le bus _serverCfg jwtSecret connPool libPath configVar =
  getEventsHandler :<|> postEventsHandler
  where
    -- GET /api/events - Stream events via SSE
    getEventsHandler :: Maybe Text -> Handler (SourceIO ServerEvent)
    getEventsHandler maybeToken = do
      -- Validate JWT from query parameter (since EventSource doesn't support custom headers)
      -- Read current config to check if auth is enabled
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

      pure $ streamEvents le bus connPool libPath

    -- POST /api/events - Submit events
    postEventsHandler :: Maybe Text -> EventRequest -> Handler EventResponse
    postEventsHandler authHeader req = do
      _ <- requireAuth configVar jwtSecret authHeader

      -- Parse and handle the event
      case eventRequestType req of
        "LibraryScanRequested" -> do
          config <- liftIO $ STM.atomically $ STM.readTVar configVar
          case Cfg.libraryPath (Cfg.library config) of
            Nothing -> pure $ EventResponse
              { eventResponseSuccess = False
              , eventResponseMessage = "Library path not configured"
              }
            Just libOsPath -> do
              libPathText <- liftIO $ OP.decodeUtf libOsPath

              -- Emit library scan requested event
              liftIO $ EventBus.publishAndLog bus le "api" $
                Events.LibraryScanRequested
                  { Events.scanPath = toText libPathText
                  }

              pure $ EventResponse
                { eventResponseSuccess = True
                , eventResponseMessage = "Library scan requested"
                }

        "ArtistDiscographyRequested" -> do
          -- Parse event data to get artist ID and MBID
          case eventRequestData req of
            Nothing -> pure $ EventResponse
              { eventResponseSuccess = False
              , eventResponseMessage = "Missing event data (need artist_id and artist_mbid)"
              }
            Just evtData -> do
              -- Parse JSON to extract artist_id and artist_mbid
              let parseResult = parseMaybe parseEventData evtData
                  parseEventData = withObject "ArtistDiscographyRequested" $ \o -> do
                    artistId <- o .: "artist_id"
                    artistMBID <- o .: "artist_mbid"
                    pure (artistId, artistMBID)

              case parseResult of
                Just (artistId, artistMBID) -> do
                  -- Emit ArtistDiscographyRequested event
                  liftIO $ EventBus.publishAndLog bus le "api" $
                    Events.ArtistDiscographyRequested
                      { Events.requestedArtistId = artistId
                      , Events.requestedArtistMBID = artistMBID
                      }

                  pure $ EventResponse
                    { eventResponseSuccess = True
                    , eventResponseMessage = "Artist discography image fetch requested"
                    }
                Nothing -> pure $ EventResponse
                  { eventResponseSuccess = False
                  , eventResponseMessage = "Invalid event data (need artist_id:number and artist_mbid:string)"
                  }

        unknownType -> pure $ EventResponse
          { eventResponseSuccess = False
          , eventResponseMessage = "Unsupported event type: " <> unknownType
          }

-- | Stream events from the event bus with heartbeat.
streamEvents :: LogEnv -> EventBus -> ConnectionPool -> Maybe Text -> SourceIO ServerEvent
streamEvents _le bus connPool libPath = SourceT $ \k -> k $ Effect $ do
  -- Subscribe to the event bus
  chan <- STM.atomically $ EventBus.subscribe bus

  -- Send initial stats
  initialStats <- loadStats connPool libPath
  pure $ Yield (ServerEvent "stats" (toJSON initialStats)) (eventLoop chan)
  where
    eventLoop chan = Effect $ do
      -- Read next event from bus (blocking)
      envelope <- STM.atomically $ readTChan chan

      -- Convert event envelope to SSE format
      let evt = convertEvent envelope
      pure $ Yield evt (eventLoop chan)

-- | Load current stats from database.
loadStats :: ConnectionPool -> Maybe Text -> IO LibraryStats
loadStats connPool libPath = withConnection connPool $ \conn -> do
  (totalFiles, totalAlbums, totalArtists, matchedFiles, unmatchedFiles, accuracy, totalDiffs, totalSize, totalRuntime) <- getLibraryStats conn
  pure $ LibraryStats
    { statsTotalFiles = totalFiles
    , statsTotalAlbums = totalAlbums
    , statsTotalArtists = totalArtists
    , statsMatchedFiles = matchedFiles
    , statsUnmatchedFiles = unmatchedFiles
    , statsMetadataAccuracy = accuracy
    , statsTotalDiffs = totalDiffs
    , statsLibrarySize = totalSize
    , statsTotalRuntime = totalRuntime
    , statsLibraryPath = libPath
    }

-- | Convert an EventEnvelope to a ServerEvent for SSE.
convertEvent :: Events.EventEnvelope -> ServerEvent
convertEvent envelope =
  let evt = Events.envelopeEvent envelope
      evtType = Events.eventType evt
      evtData = Events.eventToJSON evt
  in ServerEvent evtType evtData
