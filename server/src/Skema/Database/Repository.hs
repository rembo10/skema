{-# LANGUAGE OverloadedStrings #-}

-- | Database repository layer.
--
-- This module provides data access functions for library files and metadata.
module Skema.Database.Repository
  ( -- * Library track operations
    insertTrack
  , updateTrack
  , deleteTrack
  , getTrackByPath
  , getAllTracks
  , getLibrarySnapshot
    -- * Metadata operations
  , insertTrackMetadata
  , updateTrackMetadata
  , getMetadataForTrack
    , upsertTrackWithMetadata
    , applyMetadataChange
    -- * MusicBrainz operations
  , updateMusicBrainzIds
  , getMusicBrainzIdsByPath
  , updateMatchInDatabase
    -- * Metadata diff operations
  , insertMetadataDiff
  , getMetadataDiffsForTrack
  , deleteMetadataDiffsForTrack
  , getAllMetadataDiffs
  , applyGroupedMetadataDiff
  , computeMetadataDiffs
    -- * Metadata change operations (for undo)
  , applyMetadataChanges
  , getMetadataChanges
  , revertMetadataChange
    -- * Cluster operations
  , computeClusterHash
  , findClusterByHash
  , createCluster
  , updateClusterWithMBData
  , updateClusterWithMBDataManual
  , updateClusterLastIdentified
  , updateClusterWithCandidates
  , updateTrackCluster
  , getClusterById
  , getAllClusters
  , getClusterWithTracks
  , computeClusterQuality
    -- * Stats operations
  , getLibraryStats
    -- * Scan history
  , insertScanHistory
  , updateScanHistory
  , getRecentScans
    -- * Download operations
  , insertDownload
    -- * Acquisition rule operations
  , createAcquisitionRule
  , getAllAcquisitionRules
  , getEnabledAcquisitionRules
  , updateAcquisitionRule
  , deleteAcquisitionRule
  , getDefaultLibraryArtistsRule
    -- * Tracked artist operations
  , insertTrackedArtist
  , getTrackedArtistByMBID
  , getAllTrackedArtists
  , getRecentTrackedArtists
  , updateTrackedArtistLastChecked
  , deleteTrackedArtist
    -- * Wanted album operations
  , insertWantedAlbum
  , getWantedAlbumByReleaseGroupMBID
  , getAllWantedAlbums
  , getWantedAlbumsByStatus
  , updateWantedAlbumStatus
  , linkWantedAlbumToCluster
  , deleteWantedAlbum
    -- * Catalog operations
  , upsertCatalogArtist
  , getCatalogArtists
  , getCatalogArtistByMBID
  , updateCatalogArtistFollowed
  , updateCatalogArtist
  , deleteCatalogArtist
  , upsertCatalogAlbum
  , getCatalogAlbums
  , getCatalogAlbumsByArtistId
  , getCatalogAlbumByReleaseGroupMBID
  , updateCatalogAlbum
  , deleteCatalogAlbum
  , getCatalogAlbumsOverview
  , getCatalogAlbumsOverviewCount
  , getCatalogAlbumsOverviewStats
  , CatalogAlbumOverviewRow(..)
    -- * Quality profile operations
  , insertQualityProfile
  , updateQualityProfile
  , deleteQualityProfile
  , getQualityProfile
  , getAllQualityProfiles
  , getEffectiveQualityProfile
  , updateAlbumQuality
    -- * Settings operations
  , getDefaultQualityProfileId
  , setDefaultQualityProfileId
  ) where

-- Re-export track operations
import Skema.Database.Repository.Tracks
  ( insertTrack
  , updateTrack
  , deleteTrack
  , getTrackByPath
  , getAllTracks
  , getLibrarySnapshot
  , insertTrackMetadata
  , updateTrackMetadata
  , getMetadataForTrack
  , upsertTrackWithMetadata
  , updateMusicBrainzIds
  , getMusicBrainzIdsByPath
  , insertScanHistory
  , updateScanHistory
  , getRecentScans
  )

-- Re-export cluster operations
import Skema.Database.Repository.Clusters
  ( computeClusterHash
  , findClusterByHash
  , createCluster
  , updateClusterWithMBData
  , updateClusterWithMBDataManual
  , updateClusterLastIdentified
  , updateClusterWithCandidates
  , updateTrackCluster
  , getClusterById
  , getAllClusters
  , getClusterWithTracks
  , computeClusterQuality
  )

-- Re-export diff operations
import Skema.Database.Repository.Diffs
  ( insertMetadataDiff
  , getMetadataDiffsForTrack
  , deleteMetadataDiffsForTrack
  , getAllMetadataDiffs
  , applyGroupedMetadataDiff
  , computeMetadataDiffs
  , applyMetadataChange
  , applyMetadataChanges
  , getMetadataChanges
  , revertMetadataChange
  , updateMatchInDatabase
  )

-- Re-export acquisition operations
import Skema.Database.Repository.Acquisition
  ( createAcquisitionRule
  , getAllAcquisitionRules
  , getEnabledAcquisitionRules
  , updateAcquisitionRule
  , deleteAcquisitionRule
  , getDefaultLibraryArtistsRule
  , insertTrackedArtist
  , getTrackedArtistByMBID
  , getAllTrackedArtists
  , getRecentTrackedArtists
  , updateTrackedArtistLastChecked
  , deleteTrackedArtist
  , insertWantedAlbum
  , getWantedAlbumByReleaseGroupMBID
  , getAllWantedAlbums
  , getWantedAlbumsByStatus
  , updateWantedAlbumStatus
  , linkWantedAlbumToCluster
  , deleteWantedAlbum
  )

-- Re-export download operations
import Skema.Database.Repository.Downloads
  ( insertDownload
  )

-- Re-export quality profile operations
import Skema.Database.Repository.Quality
  ( insertQualityProfile
  , updateQualityProfile
  , deleteQualityProfile
  , getQualityProfile
  , getAllQualityProfiles
  , getEffectiveQualityProfile
  , updateAlbumQuality
  )

-- Re-export stats operations
import Skema.Database.Repository.Stats
  ( getLibraryStats
  )

-- Re-export settings operations
import Skema.Database.Repository.Settings
  ( getDefaultQualityProfileId
  , setDefaultQualityProfileId
  )

-- Re-export catalog operations
import Skema.Database.Repository.Catalog
  ( upsertCatalogArtist
  , getCatalogArtists
  , getCatalogArtistByMBID
  , updateCatalogArtistFollowed
  , updateCatalogArtist
  , deleteCatalogArtist
  , upsertCatalogAlbum
  , getCatalogAlbums
  , getCatalogAlbumsByArtistId
  , getCatalogAlbumByReleaseGroupMBID
  , updateCatalogAlbum
  , deleteCatalogAlbum
  , getCatalogAlbumsOverview
  , getCatalogAlbumsOverviewCount
  , getCatalogAlbumsOverviewStats
  , CatalogAlbumOverviewRow(..)
  )
