{-# LANGUAGE OverloadedStrings #-}

-- | Catalog operations for artists and albums.
--
-- This module re-exports all catalog operations from submodules.
-- It handles catalog_artists and catalog_albums tables.
module Skema.Database.Repository.Catalog
  ( -- * Types
    module Skema.Database.Repository.Catalog.Types
  , CatalogAlbumOverviewRow(..)
    -- * Artist operations
  , upsertCatalogArtist
  , getCatalogArtists
  , getCatalogArtistByMBID
  , updateCatalogArtistFollowed
  , updateCatalogArtist
  , deleteCatalogArtist
    -- * Album operations
  , upsertCatalogAlbum
  , getCatalogAlbums
  , getCatalogAlbumsByArtistId
  , getCatalogAlbumByReleaseGroupMBID
  , updateCatalogAlbum
  , deleteCatalogAlbum
    -- * Overview operations
  , getCatalogAlbumsOverview
  , getCatalogAlbumsOverviewCount
  , getCatalogAlbumsOverviewStats
  , getCatalogAlbumsByArtistOverview
  , getCatalogAlbumsStatsByState
  ) where

import Skema.Database.Repository.Catalog.Types
import Skema.Database.Repository.Catalog.Artist
import Skema.Database.Repository.Catalog.Album
import Skema.Database.Repository.Catalog.Overview
