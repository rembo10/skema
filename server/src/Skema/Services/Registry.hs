{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

-- | Service registry - initializes and manages all services.
module Skema.Services.Registry
  ( ServiceRegistry(..)
  , startAllServices
  ) where

import Skema.Services.Types
import Skema.Services.Dependencies (ScannerDeps(..), GrouperDeps(..), IdentifierDeps(..), DiffGeneratorDeps(..), PersisterDeps(..), DownloadDeps(..), ThumbnailerDeps(..), ImageDeps(..), ImporterDeps(..), CatalogDeps(..), AcquisitionDeps(..), StatsDeps(..), SourceEvaluatorDeps(..), MetadataWriterDeps(..), NotificationDeps(..))
import Skema.Services.AsyncRegistry (AsyncRegistry, newAsyncRegistry, registerAsync)
import Skema.Services.Scanner (startScannerService)
import Skema.Services.Grouper (startGrouperService)
import Skema.Services.Identifier (startIdentifierService)
import Skema.Services.DiffGenerator (startDiffGeneratorService)
import Skema.Services.Persister (startPersisterService)
import Skema.Services.Catalog (startCatalogService)
import Skema.Services.Acquisition (startAcquisitionService)
import Skema.Services.Image (startImageService)
import Skema.Services.Thumbnailer (startThumbnailerService)
import Skema.Services.Download (startDownloadService)
import Skema.Services.Importer (startImporterService)
import Skema.Services.Stats (startStatsService)
import Skema.Services.SourceEvaluator (startSourceEvaluatorService)
import Skema.Services.MetadataWriter (startMetadataWriterService)
import Skema.Services.Notifications (startNotificationService)
import Skema.Events.Bus (EventBus, subscribe, publishAndLog)
import Skema.Events.Types (Event(..), EventEnvelope(..))
import Skema.Database.Connection (ConnectionPool)
import Skema.Config.Types
  ( Config(..)
  , LibraryConfig(..)
  , MusicBrainzServer(..)
  , MusicBrainzConfig(..)
  , Indexer(..)
  , IndexerConfig(..)
  )
import Skema.Config.Loader (loadConfigFromFile)
import Skema.MusicBrainz.Client (newMBClientEnv, MBClientEnv)
import Skema.HTTP.Client (HttpClient, newHttpClient, getManager, defaultHttpConfig, defaultUserAgentData, headphonesVIPDomainConfig, HttpConfig(..), DomainConfig(..), HttpAuth(..), defaultDomainConfig)
import Network.HTTP.Client (Manager)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified System.OsPath as OP
import Control.Concurrent.Async (async)
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.STM (readTChan)
import Control.Monad ()
import Katip

-- | Service registry holds all running services and shared resources.
data ServiceRegistry = ServiceRegistry
  { srMBClientEnv :: MBClientEnv
    -- ^ Shared MusicBrainz client (ensures rate limiting)
  , srHttpClient :: HttpClient
    -- ^ Shared HTTP client for all services
  , srHttpManager :: Manager
    -- ^ Raw HTTP manager (for notification providers)
  , srConfigVar :: TVar Config
    -- ^ Shared config TVar (for live updates)
  , srDownloadProgressMap :: TVar (Map.Map Int64 (Double, T.Text))
    -- ^ In-memory map of download progress and status (download_id -> (progress 0.0-1.0, display_status))
  }

-- | Extract domain from a URL
extractDomain :: Text -> Maybe Text
extractDomain url =
  case T.breakOn "//" url of
    (_, rest) ->
      let afterProtocol = T.drop 2 rest
          domain = T.takeWhile (\c -> c /= '/' && c /= ':') afterProtocol
      in if T.null domain then Nothing else Just domain

-- | Create a DomainConfig for an indexer
indexerToDomainConfig :: Indexer -> Maybe (Text, DomainConfig)
indexerToDomainConfig Indexer{..} = do
  domain <- extractDomain indexerUrl
  let auth = case (indexerUsername, indexerPassword) of
        (Just user, Just password) -> Just $ BasicAuth user password
        _ -> Nothing
      config = defaultDomainConfig
        { domainAuth = auth
        , domainRateLimit = 2.0  -- Default 2 req/sec for indexers
        }
  pure (domain, config)

-- | Start all services.
--
-- This initializes and starts all background services in the correct order.
-- Services will begin listening to their respective events immediately.
-- Returns both the ServiceRegistry (shared resources) and AsyncRegistry (async handles for shutdown).
startAllServices
  :: LogEnv
  -> EventBus
  -> ConnectionPool
  -> Config
  -> FilePath  -- ^ Cache directory
  -> FilePath  -- ^ Config file path
  -> IO (ServiceRegistry, AsyncRegistry)
startAllServices le bus pool config cacheDir configPath = do
  let initialContext = ()
  let initialNamespace = "services.registry"

  -- Create async registry for tracking all service handles
  asyncRegistry <- newAsyncRegistry

  runKatipContextT le initialContext initialNamespace $ do
    $(logTM) InfoS $ logStr ("Starting all services..." :: Text)

    -- Create shared HTTP client for all services (ensures rate limiting, retries, auth)
    $(logTM) InfoS $ logStr ("Initializing HTTP client..." :: Text)

    -- Configure HTTP client with MusicBrainz and indexer settings
    let mbConfig = musicbrainz config
        -- Add Headphones VIP config if using that server
        mbDomainConfigs = case (mbServer mbConfig, mbUsername mbConfig, mbPassword mbConfig) of
          (HeadphonesVIP, Just user, Just password) ->
            Map.singleton "musicbrainz.codeshy.com" (headphonesVIPDomainConfig user password)
          _ -> Map.empty

        -- Add all indexer domain configs
        indexerDomainConfigs = Map.fromList $ mapMaybe indexerToDomainConfig (indexerList $ indexers config)

        -- Combine all domain configs (indexers override defaults if there's a conflict)
        allDomainConfigs = httpDomainConfigs defaultHttpConfig
                           `Map.union` mbDomainConfigs
                           `Map.union` indexerDomainConfigs

        httpConfig = defaultHttpConfig
          { httpDomainConfigs = allDomainConfigs
          }

    httpClient <- liftIO $ newHttpClient le httpConfig defaultUserAgentData

    $(logTM) InfoS $ logStr ("Initializing MusicBrainz API client..." :: Text)
    let mbEnv = newMBClientEnv httpClient mbConfig

    -- Create TVar for config (allows live updates)
    configVar <- liftIO $ STM.newTVarIO config

    -- Start a background thread to listen for ConfigUpdated events and reload config
    $(logTM) InfoS $ logStr ("Starting config reload listener..." :: Text)
    chan <- liftIO $ STM.atomically $ subscribe bus
    configReloadHandle <- liftIO $ async $ forever $ do
      envelope <- STM.atomically $ readTChan chan
      case envelopeEvent envelope of
        ConfigUpdated _ -> do
          runKatipContextT le () "services.registry" $ do
            $(logTM) InfoS $ logStr ("ConfigUpdated event received, reloading config..." :: Text)
          result <- loadConfigFromFile configPath
          case result of
            Left err -> do
              runKatipContextT le () "services.registry" $ do
                $(logTM) ErrorS $ logStr $ ("Failed to reload config: " <> err :: Text)
              -- Emit ConfigReloadFailed event to notify users
              publishAndLog bus le "registry" $ ConfigReloadFailed
                { configReloadError = err
                }
            Right newConfig -> do
              STM.atomically $ STM.writeTVar configVar newConfig
              runKatipContextT le () "services.registry" $ do
                $(logTM) InfoS $ logStr ("Config reloaded successfully" :: Text)
        _ -> pure ()
    liftIO $ registerAsync asyncRegistry "ConfigReloader" configReloadHandle

    -- Create TVar for in-memory download progress tracking
    $(logTM) InfoS $ logStr ("Initializing download progress map..." :: Text)
    downloadProgressMap <- liftIO $ STM.newTVarIO Map.empty

    let ctx = ServiceContext
          { scEventBus = bus
          , scDbPool = pool
          , scConfigVar = configVar
          , scLogEnv = le
          , scHttpClient = httpClient
          , scMBClientEnv = mbEnv
          , scCacheDir = cacheDir
          , scDownloadProgressMap = downloadProgressMap
          }

    -- Start pipeline services
    $(logTM) InfoS $ logStr ("Starting Scanner service..." :: Text)
    let scannerDeps = ScannerDeps
          { scanEventBus = scEventBus ctx
          , scanLogEnv = scLogEnv ctx
          , scanDbPool = scDbPool ctx
          }
    scannerHandle <- liftIO $ startScannerService scannerDeps
    liftIO $ registerAsync asyncRegistry "Scanner" scannerHandle

    $(logTM) InfoS $ logStr ("Starting Grouper service..." :: Text)
    let grouperDeps = GrouperDeps
          { groupEventBus = scEventBus ctx
          , groupLogEnv = scLogEnv ctx
          , groupDbPool = scDbPool ctx
          , groupConfigVar = scConfigVar ctx
          }
    grouperHandle <- liftIO $ startGrouperService grouperDeps
    liftIO $ registerAsync asyncRegistry "Grouper" grouperHandle

    $(logTM) InfoS $ logStr ("Starting Identifier service..." :: Text)
    let identifierDeps = IdentifierDeps
          { identEventBus = scEventBus ctx
          , identLogEnv = scLogEnv ctx
          , identDbPool = scDbPool ctx
          , identConfigVar = scConfigVar ctx
          , identMBClient = scMBClientEnv ctx
          }
    identifierHandle <- liftIO $ startIdentifierService identifierDeps
    liftIO $ registerAsync asyncRegistry "Identifier" identifierHandle

    $(logTM) InfoS $ logStr ("Starting DiffGenerator service..." :: Text)
    let diffGeneratorDeps = DiffGeneratorDeps
          { diffEventBus = scEventBus ctx
          , diffLogEnv = scLogEnv ctx
          , diffDbPool = scDbPool ctx
          , diffMBClient = scMBClientEnv ctx
          }
    diffGeneratorHandle <- liftIO $ startDiffGeneratorService diffGeneratorDeps
    liftIO $ registerAsync asyncRegistry "DiffGenerator" diffGeneratorHandle

    $(logTM) InfoS $ logStr ("Starting Persister service..." :: Text)
    let persisterDeps = PersisterDeps
          { persistEventBus = scEventBus ctx
          , persistLogEnv = scLogEnv ctx
          , persistDbPool = scDbPool ctx
          }
    persisterHandle <- liftIO $ startPersisterService persisterDeps
    liftIO $ registerAsync asyncRegistry "Persister" persisterHandle

    $(logTM) InfoS $ logStr ("Starting Catalog service..." :: Text)
    let catalogDeps = CatalogDeps
          { catEventBus = scEventBus ctx
          , catLogEnv = scLogEnv ctx
          , catDbPool = scDbPool ctx
          , catConfigVar = scConfigVar ctx
          , catMBClient = scMBClientEnv ctx
          , catHttpClient = scHttpClient ctx
          , catCacheDir = scCacheDir ctx
          }
    catalogHandle <- liftIO $ startCatalogService catalogDeps
    liftIO $ registerAsync asyncRegistry "Catalog" catalogHandle

    $(logTM) InfoS $ logStr ("Starting Acquisition service..." :: Text)
    let acquisitionDeps = AcquisitionDeps
          { acqEventBus = scEventBus ctx
          , acqLogEnv = scLogEnv ctx
          , acqDbPool = scDbPool ctx
          , acqConfigVar = scConfigVar ctx
          , acqMBClient = scMBClientEnv ctx
          }
    acquisitionHandle <- liftIO $ startAcquisitionService acquisitionDeps
    liftIO $ registerAsync asyncRegistry "Acquisition" acquisitionHandle

    $(logTM) InfoS $ logStr ("Starting Image service..." :: Text)
    let imageDeps = ImageDeps
          { imgEventBus = scEventBus ctx
          , imgLogEnv = scLogEnv ctx
          , imgDbPool = scDbPool ctx
          , imgConfigVar = scConfigVar ctx
          , imgHttpClient = scHttpClient ctx
          , imgCacheDir = scCacheDir ctx
          }
    imageHandle <- liftIO $ startImageService imageDeps
    liftIO $ registerAsync asyncRegistry "Image" imageHandle

    $(logTM) InfoS $ logStr ("Starting Thumbnailer service..." :: Text)
    let thumbnailerDeps = ThumbnailerDeps
          { thumbEventBus = scEventBus ctx
          , thumbLogEnv = scLogEnv ctx
          , thumbDbPool = scDbPool ctx
          , thumbCacheDir = scCacheDir ctx
          }
    thumbnailerHandle <- liftIO $ startThumbnailerService thumbnailerDeps
    liftIO $ registerAsync asyncRegistry "Thumbnailer" thumbnailerHandle

    $(logTM) InfoS $ logStr ("Starting Download service..." :: Text)
    let downloadDeps = DownloadDeps
          { dlEventBus = scEventBus ctx
          , dlLogEnv = scLogEnv ctx
          , dlDbPool = scDbPool ctx
          , dlConfigVar = scConfigVar ctx
          , dlHttpClient = scHttpClient ctx
          , dlProgressMap = scDownloadProgressMap ctx
          }
    (downloadSearchHandle, downloadMonitorHandle) <- liftIO $ startDownloadService downloadDeps
    liftIO $ registerAsync asyncRegistry "Download.Search" downloadSearchHandle
    liftIO $ registerAsync asyncRegistry "Download.Monitor" downloadMonitorHandle

    $(logTM) InfoS $ logStr ("Starting Importer service..." :: Text)
    let importerDeps = ImporterDeps
          { impEventBus = scEventBus ctx
          , impLogEnv = scLogEnv ctx
          , impDbPool = scDbPool ctx
          , impConfigVar = scConfigVar ctx
          , impMBClient = scMBClientEnv ctx
          }
    importerHandle <- liftIO $ startImporterService importerDeps
    liftIO $ registerAsync asyncRegistry "Importer" importerHandle

    -- Start Stats service (computes and emits STATS_UPDATED events)
    $(logTM) InfoS $ logStr ("Starting Stats service..." :: Text)
    libraryPathText <- case libraryPath (library config) of
      Nothing -> pure Nothing
      Just osPath -> do
        pathStr <- liftIO $ OP.decodeUtf osPath
        pure $ Just (toText pathStr)
    let statsDeps = StatsDeps
          { statsEventBus = scEventBus ctx
          , statsLogEnv = scLogEnv ctx
          , statsDbPool = scDbPool ctx
          }
    (statsListenerHandle, statsUpdaterHandle) <- liftIO $ startStatsService statsDeps libraryPathText
    liftIO $ registerAsync asyncRegistry "Stats.Listener" statsListenerHandle
    liftIO $ registerAsync asyncRegistry "Stats.Updater" statsUpdaterHandle

    -- Start MetadataWriter service (writes metadata changes to audio files asynchronously)
    $(logTM) InfoS $ logStr ("Starting MetadataWriter service..." :: Text)
    let metadataWriterDeps = MetadataWriterDeps
          { writerEventBus = scEventBus ctx
          , writerLogEnv = scLogEnv ctx
          , writerDbPool = scDbPool ctx
          }
    metadataWriterHandle <- liftIO $ startMetadataWriterService metadataWriterDeps
    liftIO $ registerAsync asyncRegistry "MetadataWriter" metadataWriterHandle

    -- Start SourceEvaluator service (periodically evaluates acquisition sources)
    $(logTM) InfoS $ logStr ("Starting SourceEvaluator service..." :: Text)
    let sourceEvaluatorDeps = SourceEvaluatorDeps
          { sourceEvalEventBus = scEventBus ctx
          , sourceEvalLogEnv = scLogEnv ctx
          , sourceEvalDbPool = scDbPool ctx
          , sourceEvalMBClient = scMBClientEnv ctx
          }
    sourceEvaluatorHandle <- liftIO $ startSourceEvaluatorService sourceEvaluatorDeps
    liftIO $ registerAsync asyncRegistry "SourceEvaluator" sourceEvaluatorHandle

    -- Start Notification service (sends notifications via Pushover, etc.)
    $(logTM) InfoS $ logStr ("Starting Notification service..." :: Text)
    let notificationDeps = NotificationDeps
          { notifEventBus = scEventBus ctx
          , notifConfigVar = scConfigVar ctx
          , notifHttpManager = getManager (scHttpClient ctx)
          , notifLogEnv = scLogEnv ctx
          }
    notificationHandle <- liftIO $ startNotificationService notificationDeps
    liftIO $ registerAsync asyncRegistry "Notification" notificationHandle

    $(logTM) InfoS $ logStr ("All services started successfully" :: Text)

    let serviceRegistry = ServiceRegistry
          { srMBClientEnv = mbEnv
          , srHttpClient = httpClient
          , srHttpManager = getManager httpClient
          , srConfigVar = configVar
          , srDownloadProgressMap = downloadProgressMap
          }

    pure (serviceRegistry, asyncRegistry)
