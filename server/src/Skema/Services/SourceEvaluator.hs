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
  ) where

import Skema.Services.Dependencies (SourceEvaluatorDeps(..))
import Skema.Events.Bus
import Skema.Database.Connection
import Skema.Database.Repository (getEnabledAcquisitionRules, insertWantedAlbum)
import Skema.Database.Types (AcquisitionSourceRecord(..), SourceType(..), AlbumStatus(..))
import Skema.Services.Filters
  ( SourceFilters(..)
  , MetacriticFilters(..)
  , PitchforkFilters(..)
  , MetacriticGenre(..)
  , PitchforkGenre(..)
  , parseSourceFilters
  , metacriticGenreToUrl
  , pitchforkGenreToUrl
  )
import Skema.Scraper.Metacritic (MetacriticAlbum(..), scrapeGenreUrl)
import Skema.Scraper.Pitchfork (PitchforkAlbum(..), scrapeUrl)
import Skema.MusicBrainz.Client (searchReleaseGroups, MBClientEnv)
import Skema.MusicBrainz.Types (MBReleaseGroupSearch(..), MBReleaseGroupSearchResult(..), MBID(..))
import Control.Concurrent.Async (Async, async)
import Control.Exception (try)
import Control.Concurrent (threadDelay)
import qualified Data.Text as T
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
          Left err -> do
            putStrLn $ "Failed to scrape Metacritic " <> T.unpack (metacriticGenreToUrl genre) <> ": " <> err
            pure []
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
          Left err -> do
            putStrLn $ "Failed to scrape Pitchfork " <> T.unpack (pitchforkGenreToUrl genre) <> ": " <> err
            pure []
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
    Left err -> do
      putStrLn $ "MusicBrainz search failed for " <> T.unpack artistName <> " - " <> T.unpack albumTitle <> ": " <> show err
      pure False

    Right MBReleaseGroupSearch{..} -> do
      -- Take the first result (best match)
      case listToMaybe mbrgsReleaseGroups of
        Nothing -> do
          putStrLn $ "No MusicBrainz match found for " <> T.unpack artistName <> " - " <> T.unpack albumTitle
          pure False

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
