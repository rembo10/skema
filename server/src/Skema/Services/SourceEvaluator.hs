{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SourceEvaluator service - periodically evaluates acquisition sources.
--
-- This service runs on a schedule (every 24 hours) to:
-- 1. Fetch all enabled acquisition sources
-- 2. For each source, scrape the relevant provider (Metacritic, Pitchfork)
-- 3. Evaluate scraped albums against source filters
-- 4. Match albums to MusicBrainz release groups
-- 5. Add matching albums to the wanted list
module Skema.Services.SourceEvaluator
  ( startSourceEvaluatorService
  , evaluateSingleSource
  ) where

import Skema.Services.Dependencies (SourceEvaluatorDeps(..))
import Skema.Events.Bus (EventBus, publish)
import Skema.Events.Types (Event(..), EventEnvelope(..), EventMetadata(..))
import Skema.Events.Utils (mkEventWithSequence)
import qualified Control.Concurrent.STM as STM
import Skema.Database.Connection
import Skema.Database.Repository (getEnabledAcquisitionRules, insertWantedAlbum)
import Skema.Database.Repository.Catalog (upsertCatalogArtist)
import Database.SQLite.Simple (Only(..))
import Skema.Database.Types (AcquisitionSourceRecord(..), SourceType(..), AlbumStatus(..))
import Skema.Services.Filters
  ( SourceFilters(..)
  , MetacriticFilters(..)
  , PitchforkFilters(..)
  , YouTubeMusicFilters(..)
  , MetacriticGenre(..)
  , PitchforkGenre(..)
  , parseSourceFilters
  , metacriticGenreToUrl
  , pitchforkGenreToUrl
  )
import Skema.Scraper.Metacritic (MetacriticAlbum(..), scrapeGenreUrl)
import Skema.Scraper.Pitchfork (PitchforkAlbum(..), scrapeUrl)
import Skema.YouTube.Client (fetchPlaylistItems, parseArtistFromVideo, YouTubeArtistTrack(..))
import Skema.MusicBrainz.Client (searchReleaseGroups, searchArtists, MBClientEnv)
import Skema.MusicBrainz.Types (MBReleaseGroupSearch(..), MBReleaseGroupSearchResult(..), MBID(..), MBArtistSearch(..), MBArtistSearchResult(..))
import Control.Concurrent.Async (Async, async)
import Control.Exception (try)
import Control.Concurrent (threadDelay)
import Katip

-- | Evaluation interval in microseconds (24 hours).
evaluationIntervalMicros :: Int
evaluationIntervalMicros = 24 * 60 * 60 * 1000000  -- 24 hours

-- | Initial delay before first evaluation (5 minutes).
-- This gives the system time to start up before running the first evaluation.
initialDelayMicros :: Int
initialDelayMicros = 5 * 60 * 1000000  -- 5 minutes

-- | Start the source evaluator service.
--
-- This service runs periodically to evaluate all enabled acquisition sources
-- against their respective providers (Metacritic, Pitchfork).
-- Exceptions are caught and logged to prevent the service from crashing.
startSourceEvaluatorService :: SourceEvaluatorDeps -> IO (Async ())
startSourceEvaluatorService deps = async $ do
  let le = sourceEvalLogEnv deps

  runKatipContextT le () "services.source_evaluator" $ do
    $(logTM) InfoS $ logStr ("SourceEvaluator service starting, waiting " <> show (initialDelayMicros `div` 1000000) <> "s before first evaluation" :: Text)

  -- Wait before first evaluation
  threadDelay initialDelayMicros

  -- Run evaluation loop
  forever $ do
    result <- try $ runEvaluation deps
    case result of
      Left (e :: SomeException) -> do
        runKatipContextT le () "services.source_evaluator.error" $ do
          $(logTM) ErrorS $ logStr $ ("Exception in source evaluator: " <> show e :: Text)
      Right () -> pure ()

    -- Wait until next evaluation
    threadDelay evaluationIntervalMicros

-- | Run a single evaluation cycle for all enabled sources.
runEvaluation :: SourceEvaluatorDeps -> IO ()
runEvaluation SourceEvaluatorDeps{..} = do
  let le = sourceEvalLogEnv
  let pool = sourceEvalDbPool
  let bus = sourceEvalEventBus
  let mbClient = sourceEvalMBClient

  runKatipContextT le () "services.source_evaluator" $ do
    $(logTM) InfoS $ logStr ("Starting source evaluation cycle" :: Text)

    -- Fetch all enabled sources
    sources <- liftIO $ withConnection pool getEnabledAcquisitionRules

    $(logTM) InfoS $ logStr $ ("Found " <> show (length sources) <> " enabled source(s)" :: Text)

    -- Evaluate each source
    forM_ sources $ \source -> do
      $(logTM) InfoS $ logStr $ ("Evaluating source: " <> sourceName source :: Text)
      result <- liftIO $ try $ evaluateSource pool bus mbClient source
      case result of
        Left (e :: SomeException) -> do
          $(logTM) ErrorS $ logStr $
            ("Failed to evaluate source '" <> sourceName source <> "': " <> show e :: Text)
        Right albumCount -> do
          $(logTM) InfoS $ logStr $
            ("Source '" <> sourceName source <> "' added " <> show albumCount <> " album(s)" :: Text)

    $(logTM) InfoS $ logStr ("Source evaluation cycle complete" :: Text)

-- | Evaluate a single source by ID (for manual sync).
-- Returns the number of albums/artists added.
evaluateSingleSource :: ConnectionPool -> EventBus -> MBClientEnv -> Int64 -> IO (Either Text Int)
evaluateSingleSource pool bus mbClient sourceId = do
  -- Fetch the source from database
  maybeSource <- withConnection pool $ \conn -> do
    sources <- queryRows conn
      "SELECT id, name, description, rule_type, artist_mbid, enabled, priority, filters, created_at, updated_at \
      \FROM acquisition_rules WHERE id = ?"
      (Only sourceId)
    pure $ listToMaybe sources
  
  case maybeSource of
    Nothing -> pure $ Left "Source not found"
    Just source -> do
      result <- try $ evaluateSource pool bus mbClient source
      case result of
        Left (e :: SomeException) -> pure $ Left $ "Evaluation failed: " <> show e
        Right count -> pure $ Right count

-- | Evaluate a single acquisition source.
-- Returns the number of albums added to the wanted list.
evaluateSource :: ConnectionPool -> EventBus -> MBClientEnv -> AcquisitionSourceRecord -> IO Int
evaluateSource pool bus mbClient source = do
  case sourceType source of
    LibraryArtists -> do
      -- Library Artists sources are event-driven, not periodically evaluated
      pure 0

    Metacritic -> evaluateMetacriticSource pool bus mbClient source
    Pitchfork -> evaluatePitchforkSource pool bus mbClient source
    YouTubeMusic -> evaluateYouTubeMusicSource pool bus mbClient source

-- | Evaluate a Metacritic source.
evaluateMetacriticSource :: ConnectionPool -> EventBus -> MBClientEnv -> AcquisitionSourceRecord -> IO Int
evaluateMetacriticSource pool _bus mbClient source = do
  -- Parse filters
  let filters = parseSourceFilters (sourceType source) (sourceFilters source)

  case filters of
    Just (MetacriticSourceFilters (MetacriticFilters genresFilter minCriticScore minUserScore)) -> do
      -- Get genres to scrape (or all if none specified)
      let genresToScrape = case genresFilter of
            Just gs -> gs
            Nothing -> [MCPop, MCRock, MCAlternative, MCRap, MCCountry, MCElectronic, MCRB, MCJazz, MCFolk, MCMetal]

      let mcFilters = MetacriticFilters genresFilter minCriticScore minUserScore

      -- Scrape each genre and collect results
      allAlbums <- fmap concat $ forM genresToScrape $ \genre -> do
        result <- scrapeGenreUrl
          ("https://www.metacritic.com/browse/albums/genre/date/" <> metacriticGenreToUrl genre)
          [genre]
        case result of
          Left _err -> pure []
          Right albums -> pure albums

      -- Filter albums by score thresholds
      let filteredAlbums = filter (matchesMetacriticFilters mcFilters) allAlbums

      -- Match and add each album
      addedCount <- fmap length $ forM filteredAlbums $ \album -> do
        matchAndAddAlbum pool mbClient source (mcArtistName album) (mcAlbumTitle album)

      pure addedCount

    _ -> pure 0  -- Wrong filter type

-- | Evaluate a Pitchfork source.
evaluatePitchforkSource :: ConnectionPool -> EventBus -> MBClientEnv -> AcquisitionSourceRecord -> IO Int
evaluatePitchforkSource pool _bus mbClient source = do
  -- Parse filters
  let filters = parseSourceFilters (sourceType source) (sourceFilters source)

  case filters of
    Just (PitchforkSourceFilters (PitchforkFilters genresFilter minScore)) -> do
      -- Get genres to scrape (or all if none specified)
      let genresToScrape = case genresFilter of
            Just gs -> gs
            Nothing -> [PFPop, PFRock, PFExperimental, PFElectronic, PFRap, PFJazz, PFMetal, PFFolkCountry]

      let pfFilters = PitchforkFilters genresFilter minScore

      -- Scrape each genre and collect results
      allAlbums <- fmap concat $ forM genresToScrape $ \genre -> do
        result <- scrapeUrl
          ("https://pitchfork.com/reviews/albums/?genre=" <> pitchforkGenreToUrl genre)
          [genre]
        case result of
          Left _err -> pure []
          Right albums -> pure albums

      -- Filter albums by score threshold
      let filteredAlbums = filter (matchesPitchforkFilters pfFilters) allAlbums

      -- Match and add each album
      addedCount <- fmap length $ forM filteredAlbums $ \album -> do
        matchAndAddAlbum pool mbClient source (pfArtistName album) (pfAlbumTitle album)

      pure addedCount

    _ -> pure 0  -- Wrong filter type

-- | Check if a Metacritic album matches the filter criteria.
matchesMetacriticFilters :: MetacriticFilters -> MetacriticAlbum -> Bool
matchesMetacriticFilters MetacriticFilters{..} album =
  let criticOk = case (mcMinCriticScore, mcCriticScore album) of
        (Just minScore, Just score) -> score >= minScore
        (Just _, Nothing) -> False  -- Filter requires score but album has none
        (Nothing, _) -> True  -- No filter
      userOk = case (mcMinUserScore, mcUserScore album) of
        (Just minScore, Just score) -> score >= minScore
        (Just _, Nothing) -> False
        (Nothing, _) -> True
  in criticOk && userOk

-- | Check if a Pitchfork album matches the filter criteria.
matchesPitchforkFilters :: PitchforkFilters -> PitchforkAlbum -> Bool
matchesPitchforkFilters PitchforkFilters{..} album =
  case (pfMinScore, pfScore album) of
    (Just minScore, Just score) -> score >= minScore
    (Just _, Nothing) -> False  -- Filter requires score but album has none
    (Nothing, _) -> True  -- No filter

-- | Match an album to MusicBrainz and add it to the wanted list.
-- Returns True if successfully added, False otherwise.
matchAndAddAlbum :: ConnectionPool -> MBClientEnv -> AcquisitionSourceRecord -> Text -> Text -> IO Bool
matchAndAddAlbum pool mbClient source artistName albumTitle = do
  -- Search MusicBrainz for the release group
  let query = "artist:\"" <> artistName <> "\" AND releasegroup:\"" <> albumTitle <> "\""
  searchResult <- searchReleaseGroups mbClient query (Just 5) Nothing

  case searchResult of
    Left _err -> pure False

    Right MBReleaseGroupSearch{..} -> do
      -- Take the first result (best match)
      case listToMaybe mbrgsReleaseGroups of
        Nothing -> pure False

        Just rg -> do
          -- Get the source ID
          let sid = case sourceId source of
                Just sourceIdVal -> sourceIdVal
                Nothing -> error "Source has no ID"  -- Should never happen for persisted sources

          -- Add to wanted albums
          withConnection pool $ \conn -> do
            _ <- insertWantedAlbum
              conn
              (unMBID $ mbrgsReleaseGroupId rg)
              (mbrgsTitle rg)
              (case mbrgsArtistId rg of
                Just aid -> unMBID aid
                Nothing -> "")  -- Fallback if no artist MBID
              (mbrgsArtistName rg)
              Wanted
              sid
              (mbrgsFirstReleaseDate rg)
            pure True

-- | Evaluate a YouTube Music playlist source.
-- Fetches playlist items, parses artist names, matches to MusicBrainz,
-- and optionally follows artists and marks their albums as wanted.
evaluateYouTubeMusicSource :: ConnectionPool -> EventBus -> MBClientEnv -> AcquisitionSourceRecord -> IO Int
evaluateYouTubeMusicSource pool bus mbClient source = do
  putStrLn $ "[YouTube] Starting evaluation for source: " <> toString (sourceName source)
  
  -- Parse filters
  let filters = parseSourceFilters (sourceType source) (sourceFilters source)

  case filters of
    Just (YouTubeMusicSourceFilters ytFilters) -> do
      putStrLn $ "[YouTube] Fetching playlist: " <> toString (ytPlaylistId ytFilters)
      
      -- Fetch playlist items from YouTube
      playlistResult <- fetchPlaylistItems (ytApiKey ytFilters) (ytPlaylistId ytFilters)

      case playlistResult of
        Left err -> do
          putStrLn $ "[YouTube] API error: " <> toString err
          pure 0

        Right items -> do
          putStrLn $ "[YouTube] Fetched " <> show (length items) <> " playlist items"
          
          -- Parse artist names from video metadata
          let artistTracks = mapMaybe parseArtistFromVideo items
              -- Get unique artist names
              uniqueArtists = ordNub $ map yatArtistName artistTracks

          putStrLn $ "[YouTube] Parsed " <> show (length artistTracks) <> " artist/track pairs"
          putStrLn $ "[YouTube] Found " <> show (length uniqueArtists) <> " unique artists"
          
          -- Log each parsed track
          forM_ (take 10 artistTracks) $ \track -> do
            putStrLn $ "[YouTube]   - " <> toString (yatArtistName track) <> " - " <> toString (yatTrackTitle track)
          when (length artistTracks > 10) $
            putStrLn $ "[YouTube]   ... and " <> show (length artistTracks - 10) <> " more"

          -- Match each artist to MusicBrainz and process
          addedCount <- fmap sum $ forM uniqueArtists $ \artistName -> do
            matchAndFollowArtist pool bus mbClient source ytFilters artistName

          putStrLn $ "[YouTube] Total artists added/matched: " <> show addedCount
          pure addedCount

    Nothing -> do
      putStrLn $ "[YouTube] Failed to parse filters for source"
      pure 0
    
    _ -> do
      putStrLn $ "[YouTube] Wrong filter type for source"
      pure 0

-- | Match an artist name to MusicBrainz and follow them.
-- If auto_want_albums is enabled, also marks their albums as wanted.
-- Emits CatalogArtistFollowed event to trigger full catalog sync.
matchAndFollowArtist :: ConnectionPool -> EventBus -> MBClientEnv -> AcquisitionSourceRecord -> YouTubeMusicFilters -> Text -> IO Int
matchAndFollowArtist pool bus mbClient source ytFilters artistName = do
  putStrLn $ "[YouTube] Searching MusicBrainz for artist: " <> toString artistName
  
  -- Search MusicBrainz for the artist
  let query = "artist:\"" <> artistName <> "\""
  searchResult <- searchArtists mbClient query (Just 5) Nothing

  case searchResult of
    Left err -> do
      putStrLn $ "[YouTube]   MusicBrainz search failed: " <> show err
      pure 0

    Right MBArtistSearch{..} -> do
      putStrLn $ "[YouTube]   Found " <> show (length mbasArtists) <> " MusicBrainz results"
      
      -- Take the first result (best match)
      case listToMaybe mbasArtists of
        Nothing -> do
          putStrLn $ "[YouTube]   No matching artist found in MusicBrainz"
          pure 0

        Just artist -> do
          putStrLn $ "[YouTube]   Best match: " <> toString (mbasArtistName artist) <> " (MBID: " <> toString (unMBID $ mbasArtistId artist) <> ")"
          
          -- Get the source ID
          let sid = case sourceId source of
                Just sourceIdVal -> sourceIdVal
                Nothing -> error "Source has no ID"

          -- If auto-follow is enabled, add the artist to catalog_artists (followed)
          when (ytAutoFollowArtists ytFilters) $ do
            putStrLn $ "[YouTube]   Adding artist to followed list..."
            withConnection pool $ \conn -> do
              -- Upsert catalog artist with followed=True
              _ <- upsertCatalogArtist
                conn
                (unMBID $ mbasArtistId artist)
                (mbasArtistName artist)
                (mbasArtistType artist)  -- Artist type from MusicBrainz
                Nothing  -- No image URL from search
                Nothing  -- No thumbnail
                True     -- followed = True
                (Just sid)  -- added_by_rule_id
                Nothing  -- No source cluster
                Nothing  -- No last_checked_at
              pure ()
            
            -- Emit CatalogArtistFollowed event to trigger full catalog sync
            -- This will fetch all albums from MusicBrainz and add them to the catalog
            putStrLn $ "[YouTube]   Emitting CatalogArtistFollowed event for full catalog sync..."
            let evt = CatalogArtistFollowed
                  { catalogArtistMBID = unMBID $ mbasArtistId artist
                  , catalogArtistName = mbasArtistName artist
                  }
            envelope <- mkEventWithSequence "youtube-sync" evt 0
            STM.atomically $ publish bus envelope
            putStrLn $ "[YouTube]   Artist added successfully"

          -- If auto-want albums is enabled, search and add their albums
          if ytAutoWantAlbums ytFilters
            then do
              putStrLn $ "[YouTube]   Searching for albums by this artist..."
              -- Search for release groups by this artist
              let rgQuery = "arid:" <> unMBID (mbasArtistId artist)
              rgResult <- searchReleaseGroups mbClient rgQuery (Just 25) Nothing

              case rgResult of
                Left err -> do
                  putStrLn $ "[YouTube]   Album search failed: " <> show err
                  pure 0
                Right MBReleaseGroupSearch{..} -> do
                  putStrLn $ "[YouTube]   Found " <> show (length mbrgsReleaseGroups) <> " albums"
                  -- Add each release group as wanted
                  addedCount <- fmap length $ forM mbrgsReleaseGroups $ \rg -> do
                    withConnection pool $ \conn -> do
                      _ <- insertWantedAlbum
                        conn
                        (unMBID $ mbrgsReleaseGroupId rg)
                        (mbrgsTitle rg)
                        (unMBID $ mbasArtistId artist)
                        (mbasArtistName artist)
                        Wanted
                        sid
                        (mbrgsFirstReleaseDate rg)
                      pure ()
                  putStrLn $ "[YouTube]   Added " <> show addedCount <> " albums as wanted"
                  pure addedCount
            else do
              putStrLn $ "[YouTube]   Auto-want albums disabled, returning 1 for followed artist"
              pure 1  -- Return 1 to count the followed artist

-- | Import insertTrackedArtist from Repository
-- (This is already available via the Repository module)
