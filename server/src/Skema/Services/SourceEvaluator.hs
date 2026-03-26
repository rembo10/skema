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
  , evaluateSource
  ) where

import Skema.Services.Dependencies (SourceEvaluatorDeps(..))
import Skema.Events.Bus (EventBus, publishAndLog)
import Skema.Events.Types (Event(..))
import Skema.Database.Connection
import Skema.Database.Repository (getEnabledAcquisitionRules, upsertCatalogArtist, upsertCatalogAlbum, resolveQualityProfileId, updateCatalogAlbum, updateSourceLastSeenUrl)
import Skema.Database.Types (AcquisitionSourceRecord(..), SourceType(..))
import Skema.Domain.Acquisition
  ( SourceFilters(..)
  , MetacriticFilters(..)
  , PitchforkFilters(..)
  , MetacriticGenre(..)
  , parseSourceFilters
  , metacriticGenreToUrl
  )
import Skema.Scraper.Metacritic (MetacriticAlbum(..), scrapeGenreUrl)
import Skema.Scraper.Pitchfork (PitchforkAlbum(..), scrapeUrl, fetchReviewScore)
import qualified Skema.Scraper.Pitchfork as PF
import Skema.MusicBrainz.Client (searchReleaseGroups, MBClientEnv)
import Skema.MusicBrainz.Types (MBReleaseGroupSearch(..), MBReleaseGroupSearchResult(..), MBID(..))
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
      result <- liftIO $ try $ evaluateSource pool bus le mbClient source
      case result of
        Left (e :: SomeException) -> do
          $(logTM) ErrorS $ logStr $
            ("Failed to evaluate source '" <> sourceName source <> "': " <> show e :: Text)
        Right albumCount -> do
          $(logTM) InfoS $ logStr $
            ("Source '" <> sourceName source <> "' added " <> show albumCount <> " album(s)" :: Text)

    $(logTM) InfoS $ logStr ("Source evaluation cycle complete" :: Text)

-- | Log a message in the source evaluator context.
logEval :: LogEnv -> Text -> IO ()
logEval le msg =
  runKatipContextT le () "services.source_evaluator" $
    $(logTM) InfoS $ logStr msg

-- | Evaluate a single acquisition source.
-- Returns the number of albums added to the wanted list.
evaluateSource :: ConnectionPool -> EventBus -> LogEnv -> MBClientEnv -> AcquisitionSourceRecord -> IO Int
evaluateSource pool bus le mbClient source = do
  case sourceType source of
    LibraryArtists -> do
      -- Library Artists sources are event-driven, not periodically evaluated
      pure 0

    Metacritic -> evaluateMetacriticSource pool bus le mbClient source
    Pitchfork -> evaluatePitchforkSource pool bus le mbClient source

-- | Evaluate a Metacritic source.
evaluateMetacriticSource :: ConnectionPool -> EventBus -> LogEnv -> MBClientEnv -> AcquisitionSourceRecord -> IO Int
evaluateMetacriticSource pool bus le mbClient source = do
  -- Parse filters
  let filters = parseSourceFilters (sourceType source) (sourceFilters source)

  case filters of
    Just (MetacriticSourceFilters (MetacriticFilters genresFilter minCriticScore minUserScore)) -> do
      -- Get genres to scrape (or all if none specified)
      let genresToScrape = case genresFilter of
            Just gs -> gs
            Nothing -> [MCPop, MCRock, MCAlternative, MCRap, MCCountry, MCElectronic, MCRB, MCJazz, MCFolk, MCMetal]

      let mcFilters = MetacriticFilters genresFilter minCriticScore minUserScore

      logEval le $ "Scraping " <> show (length genresToScrape) <> " Metacritic genre(s)"
        <> " (min_critic: " <> maybe "none" show minCriticScore
        <> ", min_user: " <> maybe "none" show minUserScore <> ")"

      -- Scrape each genre and collect results
      allAlbums <- fmap concat $ forM genresToScrape $ \genre -> do
        let url = "https://www.metacritic.com/browse/albums/genre/date/" <> metacriticGenreToUrl genre
        result <- scrapeGenreUrl url [genre]
        case result of
          Left err -> do
            logEval le $ "Failed to scrape genre " <> show genre <> ": " <> show err
            pure []
          Right albums -> do
            logEval le $ "Scraped " <> show (length albums) <> " album(s) from " <> metacriticGenreToUrl genre
            pure albums

      logEval le $ "Total scraped: " <> show (length allAlbums) <> " album(s)"

      -- Filter albums by score thresholds
      let filteredAlbums = filter (matchesMetacriticFilters mcFilters) allAlbums
      let failedFilter = length allAlbums - length filteredAlbums
      logEval le $ "Score filter: " <> show (length filteredAlbums) <> " passed"
        <> (if failedFilter > 0
            then " (" <> show failedFilter <> " below threshold or missing score)"
            else "")

      -- Match and add each album
      logEval le $ "Looking up " <> show (length filteredAlbums) <> " album(s) on MusicBrainz..."
      results <- forM (zip [1::Int ..] filteredAlbums) $ \(i, album) -> do
        logEval le $ "[" <> show i <> "/" <> show (length filteredAlbums) <> "] "
          <> mcArtistName album <> " - " <> mcAlbumTitle album
        matched <- matchAndAddAlbum pool bus le mbClient source (mcArtistName album) (mcAlbumTitle album)
        unless matched $
          logEval le $ "  -> No MusicBrainz match"
        pure matched

      let addedCount = length (filter id results)
      let mbFailures = length (filter not results)
      logEval le $ "Done: " <> show addedCount <> " album(s) added"
        <> (if mbFailures > 0 then ", " <> show mbFailures <> " MusicBrainz lookup(s) failed" else "")
      pure addedCount

    _ -> do
      logEval le "Could not parse Metacritic filters"
      pure 0

-- | Evaluate a Pitchfork source.
-- Scrapes the main listing page (genre-filtered URLs return 404).
-- Genre filtering is done client-side by the parser.
-- Only processes entries newer than the last-seen URL to avoid redundant scraping.
-- Individual review pages are fetched for scores only when a min score filter is set.
evaluatePitchforkSource :: ConnectionPool -> EventBus -> LogEnv -> MBClientEnv -> AcquisitionSourceRecord -> IO Int
evaluatePitchforkSource pool bus le mbClient source = do
  let filters = parseSourceFilters (sourceType source) (sourceFilters source)

  case filters of
    Just (PitchforkSourceFilters pf@(PitchforkFilters genresFilter minScore)) -> do
      let genresToFilter = fromMaybe [] genresFilter

      logEval le $ "Scraping Pitchfork reviews listing (genres: "
        <> (if null genresToFilter then "all" else show genresToFilter)
        <> ", min_score: " <> maybe "none" show minScore <> ")"

      -- Scrape main listing page (no genre filter - we filter in the evaluator for clearer logging)
      result <- scrapeUrl "https://pitchfork.com/reviews/albums/" []
      case result of
        Left err -> do
          logEval le $ "Failed to scrape Pitchfork: " <> show err
          pure 0
        Right allAlbums -> do
          logEval le $ "Scraped " <> show (length allAlbums) <> " album(s) from Pitchfork"

          -- Apply genre filter
          let albums = if null genresToFilter
                then allAlbums
                else filter (\a -> any (`elem` genresToFilter) (PF.pfGenres a)) allAlbums
          unless (null genresToFilter) $
            logEval le $ "Genre filter: " <> show (length albums) <> " of "
              <> show (length allAlbums) <> " match genres " <> show genresToFilter

          -- Only process entries newer than the last-seen URL
          let lastSeen = sourceLastSeenUrl source
          let newAlbums = takeNewEntries lastSeen pfReviewUrl albums
          logEval le $ "Last seen URL: " <> fromMaybe "(none - first run)" lastSeen
          logEval le $ "New entries since last check: " <> show (length newAlbums)
            <> " of " <> show (length albums)

          -- Update last_seen_url to the first entry (newest) from the full list
          case (sourceId source, listToMaybe albums) of
            (Just sid, Just newest) -> do
              logEval le $ "Updating last_seen_url to: " <> pfReviewUrl newest
              withConnection pool $ \conn ->
                updateSourceLastSeenUrl conn sid (pfReviewUrl newest)
            _ -> pure ()

          -- Fetch scores for new entries if a min score filter is set
          scoredAlbums <- if isJust minScore
            then do
              logEval le $ "Fetching scores for " <> show (length newAlbums) <> " review(s)..."
              forM (zip [1::Int ..] newAlbums) $ \(i, album) -> do
                threadDelay 1000000  -- 1 second delay between requests
                score <- fetchReviewScore (pfReviewUrl album)
                logEval le $ "[" <> show i <> "/" <> show (length newAlbums) <> "] "
                  <> pfArtistName album <> " - " <> pfAlbumTitle album
                  <> " => " <> maybe "no score" (\s -> show s <> "/10") score
                pure album { pfScore = score }
            else do
              logEval le "No min_score filter set, skipping score fetching"
              pure newAlbums

          -- Filter by score threshold
          let filteredAlbums = filter (matchesPitchforkFilters pf) scoredAlbums
          let failedScore = length scoredAlbums - length filteredAlbums
          logEval le $ "Score filter: " <> show (length filteredAlbums) <> " passed"
            <> (if failedScore > 0
                then " (" <> show failedScore <> " below threshold or missing score)"
                else "")

          -- Match and add each album
          logEval le $ "Looking up " <> show (length filteredAlbums) <> " album(s) on MusicBrainz..."
          results <- forM (zip [1::Int ..] filteredAlbums) $ \(i, album) -> do
            logEval le $ "[" <> show i <> "/" <> show (length filteredAlbums) <> "] "
              <> pfArtistName album <> " - " <> pfAlbumTitle album
            matched <- matchAndAddAlbum pool bus le mbClient source (pfArtistName album) (pfAlbumTitle album)
            unless matched $
              logEval le $ "  -> No MusicBrainz match"
            pure matched

          let addedCount = length (filter id results)
          let mbFailures = length (filter not results)
          logEval le $ "Done: " <> show addedCount <> " album(s) added"
            <> (if mbFailures > 0 then ", " <> show mbFailures <> " MusicBrainz lookup(s) failed" else "")
          pure addedCount

    _ -> do
      logEval le "Could not parse Pitchfork filters"
      pure 0

-- | Take entries from the front of a list until reaching the last-seen entry.
-- If lastSeenUrl is Nothing (first run), returns all entries.
takeNewEntries :: Maybe Text -> (a -> Text) -> [a] -> [a]
takeNewEntries Nothing _ entries = entries
takeNewEntries (Just lastUrl) getUrl entries =
  takeWhile (\e -> getUrl e /= lastUrl) entries

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
matchAndAddAlbum :: ConnectionPool -> EventBus -> LogEnv -> MBClientEnv -> AcquisitionSourceRecord -> Text -> Text -> IO Bool
matchAndAddAlbum pool bus le mbClient source artistName albumTitle = do
  logEval le $ "Looking up: " <> artistName <> " - " <> albumTitle
  -- Search MusicBrainz for the release group
  let query = "artist:\"" <> artistName <> "\" AND releasegroup:\"" <> albumTitle <> "\""
  searchResult <- searchReleaseGroups mbClient query (Just 5) Nothing

  case searchResult of
    Left err -> do
      logEval le $ "MusicBrainz search failed for " <> artistName <> " - " <> albumTitle <> ": " <> show err
      pure False

    Right MBReleaseGroupSearch{..} -> do
      -- Take the first result (best match)
      case listToMaybe mbrgsReleaseGroups of
        Nothing -> pure False

        Just rg -> do
          let releaseGroupMBID = unMBID $ mbrgsReleaseGroupId rg
          let artistMBID = case mbrgsArtistId rg of
                Just aid -> unMBID aid
                Nothing -> ""  -- Fallback if no artist MBID

          -- Upsert catalog artist and album (like Catalog service does)
          withConnection pool $ \conn -> do
            -- Upsert artist (returns artist ID directly)
            artistId <- upsertCatalogArtist conn artistMBID (mbrgsArtistName rg) Nothing Nothing Nothing False Nothing Nothing Nothing

            -- Upsert album
            albumId <- upsertCatalogAlbum
              conn
              releaseGroupMBID
              (mbrgsTitle rg)
              artistId
              artistMBID
              (mbrgsArtistName rg)
              (mbrgsType rg)
              (mbrgsFirstReleaseDate rg)

            -- Resolve quality profile: artist > source > default
            resolvedProfileId <- resolveQualityProfileId conn Nothing (Just artistId) (sourceId source)

            -- Set the resolved profile on the album
            case resolvedProfileId of
              Just _pid -> updateCatalogAlbum conn albumId (Just resolvedProfileId)
              Nothing -> pure ()

            -- Emit WantedAlbumAdded event
            publishAndLog bus le "source_evaluator" $ WantedAlbumAdded
              { wantedCatalogAlbumId = albumId
              , wantedReleaseGroupId = releaseGroupMBID
              , wantedAlbumTitle = mbrgsTitle rg
              , wantedArtistName = mbrgsArtistName rg
              }
            logEval le $ "Added: " <> mbrgsArtistName rg <> " - " <> mbrgsTitle rg
            pure True
