{-# LANGUAGE OverloadedStrings #-}

-- | Cached indexer release operations.
--
-- Persists indexer/RSS search results per catalog album so the releases UI can
-- render instantly (with a manual refresh to re-search) and acquisition can
-- reuse them. Rows are deduplicated by (catalog_album_id, download_url).
module Skema.Database.Repository.Releases
  ( upsertCachedRelease
  , getCachedReleasesForAlbum
  ) where

import Skema.Database.Connection (queryRows)
import Skema.Database.Types (CachedReleaseRecord)
import Skema.Domain.Quality (qualityToText)
import Skema.Indexer.Types (ReleaseInfo (..), DownloadType (..))
import Data.Aeson (encode)
import Data.Time (UTCTime)
import Database.SQLite.Simple (Only (..))
import qualified Database.SQLite.Simple as SQLite
import qualified Database.SQLite.Simple.ToField as SQLite
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

-- | Row shape for inserting a cached release.
data CachedReleaseInsert = CachedReleaseInsert
  { criAlbumId :: Int64
  , criIndexerName :: Text
  , criGuid :: Maybe Text
  , criTitle :: Text
  , criDownloadUrl :: Text
  , criDownloadType :: Text
  , criQuality :: Text
  , criSizeBytes :: Maybe Int64
  , criSeeders :: Maybe Int
  , criPeers :: Maybe Int
  , criPublishDate :: Maybe UTCTime
  , criInfoUrl :: Maybe Text
  , criSlskdUsername :: Maybe Text
  , criSlskdFiles :: Maybe Text
  , criSource :: Text
  , criNow :: UTCTime
  }

instance SQLite.ToRow CachedReleaseInsert where
  toRow c =
    [ SQLite.toField (criAlbumId c)
    , SQLite.toField (criIndexerName c)
    , SQLite.toField (criGuid c)
    , SQLite.toField (criTitle c)
    , SQLite.toField (criDownloadUrl c)
    , SQLite.toField (criDownloadType c)
    , SQLite.toField (criQuality c)
    , SQLite.toField (criSizeBytes c)
    , SQLite.toField (criSeeders c)
    , SQLite.toField (criPeers c)
    , SQLite.toField (criPublishDate c)
    , SQLite.toField (criInfoUrl c)
    , SQLite.toField (criSlskdUsername c)
    , SQLite.toField (criSlskdFiles c)
    , SQLite.toField (criSource c)
    , SQLite.toField (criNow c)  -- first_seen_at (ignored on conflict)
    , SQLite.toField (criNow c)  -- last_seen_at
    ]

downloadTypeToText :: DownloadType -> Text
downloadTypeToText NZB = "nzb"
downloadTypeToText Torrent = "torrent"
downloadTypeToText Slskd = "slskd"

-- | Insert or refresh a cached release for an album, deduplicated by download URL.
-- On conflict the mutable fields (title, quality, seeders, source, …) and
-- last_seen_at are refreshed while first_seen_at is preserved.
upsertCachedRelease
  :: SQLite.Connection
  -> Int64    -- ^ catalog album id
  -> Text     -- ^ indexer name
  -> Text     -- ^ source ("search" or "rss")
  -> UTCTime  -- ^ now
  -> ReleaseInfo
  -> IO ()
upsertCachedRelease conn albumId indexerName source now release =
  SQLite.execute conn
    "INSERT INTO catalog_releases \
    \  (catalog_album_id, indexer_name, guid, title, download_url, download_type, \
    \   quality, size_bytes, seeders, peers, publish_date, info_url, \
    \   slskd_username, slskd_files, source, first_seen_at, last_seen_at) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) \
    \ON CONFLICT(catalog_album_id, download_url) DO UPDATE SET \
    \  indexer_name = excluded.indexer_name, \
    \  guid = excluded.guid, \
    \  title = excluded.title, \
    \  download_type = excluded.download_type, \
    \  quality = excluded.quality, \
    \  size_bytes = excluded.size_bytes, \
    \  seeders = excluded.seeders, \
    \  peers = excluded.peers, \
    \  publish_date = excluded.publish_date, \
    \  info_url = excluded.info_url, \
    \  slskd_username = excluded.slskd_username, \
    \  slskd_files = excluded.slskd_files, \
    \  source = excluded.source, \
    \  last_seen_at = excluded.last_seen_at"
    CachedReleaseInsert
      { criAlbumId = albumId
      , criIndexerName = indexerName
      , criGuid = riGuid release
      , criTitle = riTitle release
      , criDownloadUrl = riDownloadUrl release
      , criDownloadType = downloadTypeToText (riDownloadType release)
      , criQuality = qualityToText (riQuality release)
      , criSizeBytes = fmap fromIntegral (riSize release)
      , criSeeders = riSeeders release
      , criPeers = riPeers release
      , criPublishDate = riPublishDate release
      , criInfoUrl = riInfoUrl release
      , criSlskdUsername = riSlskdUsername release
      , criSlskdFiles = fmap encodeJsonText (riSlskdFiles release)
      , criSource = source
      , criNow = now
      }
  where
    encodeJsonText = TL.toStrict . TLE.decodeUtf8 . encode

-- | Standard SELECT column list for cached release queries (matches FromRow order).
cachedReleaseColumns :: Text
cachedReleaseColumns =
  "id, catalog_album_id, indexer_name, guid, title, download_url, download_type, \
  \quality, size_bytes, seeders, peers, publish_date, info_url, slskd_username, \
  \slskd_files, source, first_seen_at, last_seen_at"

-- | Get all cached releases for an album, most recently seen first.
getCachedReleasesForAlbum :: SQLite.Connection -> Int64 -> IO [CachedReleaseRecord]
getCachedReleasesForAlbum conn albumId =
  queryRows conn
    ("SELECT " <> cachedReleaseColumns <> " FROM catalog_releases \
     \WHERE catalog_album_id = ? ORDER BY last_seen_at DESC, id DESC")
    (Only albumId)
