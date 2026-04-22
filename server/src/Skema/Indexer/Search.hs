{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Skema.Indexer.Search
  ( searchAllIndexers
  , searchForAlbum
  ) where

import Control.Concurrent.Async (mapConcurrently)
import qualified Data.Text as T

import Skema.Config.Types (Indexer(..), IndexerConfig(..))
import Skema.Database.Types (CatalogAlbumRecord(..))
import Skema.Indexer.Types
import Skema.Indexer.Client
import Skema.HTTP.Client (HttpClient)

-- | Search all enabled indexers for a query
searchAllIndexers :: HttpClient -> IndexerConfig -> SearchQuery -> IO SearchResult
searchAllIndexers client IndexerConfig{..} query = do
  -- Filter enabled indexers and sort by priority
  let enabledIndexers = filter indexerEnabled indexerList
      sortedIndexers = sortOn indexerPriority enabledIndexers

  -- Search all indexers concurrently
  results <- mapConcurrently (\indexer -> searchIndexer client indexer query) sortedIndexers

  -- Partition results and errors
  let (errors, successes) = partitionEithers results
      totalReleases = sum $ map (length . irReleases) successes

  pure SearchResult
    { srResults = successes
    , srErrors = errors
    , srTotalReleases = totalReleases
    }

-- | Search for a specific catalog album
searchForAlbum :: HttpClient -> IndexerConfig -> CatalogAlbumRecord -> IO SearchResult
searchForAlbum client indexerConfig CatalogAlbumRecord{..} = do
  -- Extract year from first release date (format: YYYY-MM-DD)
  let year = catalogAlbumFirstReleaseDate >>= \date ->
        case T.take 4 date of
          y | T.length y == 4 -> readMaybe (T.unpack y)
          _ -> Nothing

      query = SearchQuery
        { sqArtist = Just catalogAlbumArtistName
        , sqAlbum = Just catalogAlbumTitle
        , sqYear = year
        , sqQuery = Nothing
        , sqCategories = [3000, 3010, 3020]  -- Audio, MP3, FLAC
        , sqLimit = 100
        , sqOffset = 0
        }

  searchAllIndexers client indexerConfig query
