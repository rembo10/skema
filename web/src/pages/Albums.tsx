import { useEffect, useState, useRef } from 'react';
import { useSearchParams } from 'react-router-dom';
import { api } from '../lib/api';
import { formatDate } from '../lib/formatters';
import toast from 'react-hot-toast';
import { PaginationControls } from '../components/PaginationControls';
import { usePagination } from '../hooks/usePagination';
import { useAlbumsFilterState, QuickFilter } from '../hooks/useAlbumsFilterState';
import { TableRowSkeleton } from '../components/LoadingSkeleton';
import { LoadingState } from '../components/LoadingState';
import { AlbumReleasesModal } from '../components/AlbumReleasesModal';
import {
  AlbumState,
  CatalogAlbumOverview,
  AlbumOverviewRequest,
  QualityProfile,
} from '../types/api';
import {
  Music,
  Search as SearchIcon,
  CheckCircle2,
  XCircle,
  Loader2,
  AlertCircle,
  ArrowUpCircle,
  Filter,
  X,
  Disc,
  ArrowUpDown,
  ArrowUp,
  ArrowDown,
  List,
  Square,
  CheckSquare,
  Heart,
} from 'lucide-react';

const ITEMS_PER_PAGE = 50;

const stateConfig: Record<AlbumState, { label: string; icon: typeof Music; color: string }> = {
  NotWanted: { label: 'Not Wanted', icon: X, color: 'text-gray-400' },
  Wanted: { label: 'Wanted', icon: Music, color: 'text-blue-500' },
  Searching: { label: 'Searching', icon: SearchIcon, color: 'text-yellow-500' },
  Downloading: { label: 'Downloading', icon: Loader2, color: 'text-orange-500' },
  Failed: { label: 'Failed', icon: XCircle, color: 'text-red-500' },
  IdentificationFailed: { label: 'ID Failed', icon: AlertCircle, color: 'text-red-400' },
  InLibrary: { label: 'Finished', icon: CheckCircle2, color: 'text-green-500' },
  Monitored: { label: 'Upgrading', icon: ArrowUpCircle, color: 'text-purple-500' },
  Upgrading: { label: 'Upgrading', icon: ArrowUpCircle, color: 'text-purple-500' },
};

export default function Albums() {
  const [searchParams] = useSearchParams();

  const [albums, setAlbums] = useState<CatalogAlbumOverview[]>([]);
  const [loading, setLoading] = useState(true);
  const [tableLoading, setTableLoading] = useState(false);
  const initialLoadComplete = useRef(false);
  const urlParamsInitialized = useRef(false);
  const [searching, setSearching] = useState(false);
  const [searchInput, setSearchInput] = useState('');

  // Consolidated filter state using reducer
  const {
    state: filterState,
    toggleSort,
    toggleQuality,
    clearQualityFilter,
    toggleStatus,
    toggleUpgrading,
    clearStatusFilter,
    setQuickFilter,
    setSearchFilter,
    applyUrlParams,
  } = useAlbumsFilterState();

  const { sortBy, sortOrder, qualityFilter, statusFilter, upcomingFilter, quickFilter, searchFilter } = filterState;

  const pagination = usePagination(ITEMS_PER_PAGE);
  const [showReleasesModal, setShowReleasesModal] = useState(false);
  const [selectedAlbum, setSelectedAlbum] = useState<CatalogAlbumOverview | null>(null);
  const [qualityProfiles, setQualityProfiles] = useState<QualityProfile[]>([]);
  const [defaultProfile, setDefaultProfile] = useState<QualityProfile | null>(null);
  const [selectedAlbumIds, setSelectedAlbumIds] = useState<Set<number>>(new Set());
  // Track selected quality profile for unwanted albums (album.id -> 'default' | profileId)
  const [unwantedAlbumSelections, setUnwantedAlbumSelections] = useState<Map<number, string>>(new Map());

  // Initialize filters from URL parameters
  useEffect(() => {
    const stateParam = searchParams.get('state');
    const releaseDateAfter = searchParams.get('release_date_after');
    applyUrlParams(stateParam, releaseDateAfter);
    pagination.resetOffset();
  }, [searchParams, applyUrlParams]); // React to URL param changes

  // Set initialized flag after filters are updated
  useEffect(() => {
    urlParamsInitialized.current = true;
  }, [statusFilter, upcomingFilter]);

  // Debounce search input
  useEffect(() => {
    setSearching(true);
    const timer = setTimeout(() => {
      setSearchFilter(searchInput);
      pagination.resetOffset();
      setSearching(false);
    }, 500);
    return () => {
      clearTimeout(timer);
      setSearching(false);
    };
  }, [searchInput, setSearchFilter]);

  useEffect(() => {
    // Don't load until URL params have been initialized
    if (!urlParamsInitialized.current) return;

    loadAlbums();
    // Clear selections when filters change
    setSelectedAlbumIds(new Set());
  }, [searchParams, pagination.offset, searchFilter, sortBy, sortOrder, qualityFilter, statusFilter, upcomingFilter]);

  useEffect(() => {
    loadQualityProfiles();
  }, []);

  // Listen for SSE updates to individual albums
  useEffect(() => {
    const handleAlbumUpdate = (event: Event) => {
      const customEvent = event as CustomEvent;
      const data = customEvent.detail;

      // Update the album in the local state
      setAlbums(prevAlbums => {
        const albumIndex = prevAlbums.findIndex(a => a.id === data.album_id);
        if (albumIndex === -1) {
          // Album not in current view, ignore
          return prevAlbums;
        }

        // Create updated album object
        const updatedAlbum: CatalogAlbumOverview = {
          ...prevAlbums[albumIndex],
          state: data.state,
          current_quality: data.current_quality,
          quality_profile_id: data.quality_profile_id,
          quality_profile_name: data.quality_profile_name,
          cover_url: data.cover_url,
          cover_thumbnail_url: data.cover_thumbnail_url,
        };

        // Replace the album in the array
        const newAlbums = [...prevAlbums];
        newAlbums[albumIndex] = updatedAlbum;
        return newAlbums;
      });
    };

    window.addEventListener('catalog_album_updated', handleAlbumUpdate);
    return () => {
      window.removeEventListener('catalog_album_updated', handleAlbumUpdate);
    };
  }, []);

  const loadQualityProfiles = async () => {
    try {
      const [profiles, defaultProf] = await Promise.all([
        api.getQualityProfiles(),
        api.getDefaultQualityProfile(),
      ]);
      setQualityProfiles(profiles);
      setDefaultProfile(defaultProf);
    } catch (error) {
      console.error('Failed to load quality profiles:', error);
    }
  };

  const loadAlbums = async () => {
    // Only show full loading spinner on very first load
    if (!initialLoadComplete.current) {
      setLoading(true);
    } else {
      // Show table loading overlay for subsequent loads
      setTableLoading(true);
    }
    try {
      // Read filters from URL params directly to avoid state sync issues
      const stateParam = searchParams.get('state');
      const releaseDateAfter = searchParams.get('release_date_after');

      let effectiveStateFilter: AlbumState[] | undefined = undefined;
      let effectiveUpcomingFilter: string | undefined = undefined;

      if (stateParam) {
        effectiveStateFilter = stateParam.split(',') as AlbumState[];
      } else if (releaseDateAfter) {
        effectiveUpcomingFilter = 'today';
      } else if (statusFilter.length > 0) {
        // Use state filter if no URL params
        effectiveStateFilter = statusFilter;
      }

      if (upcomingFilter && !releaseDateAfter) {
        effectiveUpcomingFilter = 'today';
      }

      const request: AlbumOverviewRequest = {
        offset: pagination.offset,
        limit: ITEMS_PER_PAGE,
        search: searchFilter || undefined,
        sort: sortBy,
        order: sortOrder,
        state: effectiveStateFilter,
        quality: qualityFilter.length > 0 ? qualityFilter : undefined,
        release_date_after: effectiveUpcomingFilter,
      };

      const response = await api.getAlbumOverview(request);

      if (!response || !response.pagination) {
        console.error('Invalid response structure:', response);
        toast.error('Invalid response from server');
        return;
      }

      setAlbums(response.albums || []);
      pagination.setTotalCount(response.pagination.total);
      initialLoadComplete.current = true;
    } catch (error) {
      console.error('Failed to load albums:', error);
      toast.error('Failed to load albums');
    } finally {
      setLoading(false);
      setTableLoading(false);
    }
  };

  const handleUnwantAlbum = async (albumId: number) => {
    try {
      // Set quality_profile_id = null to unwant (remove from monitoring)
      await api.updateCatalogAlbum(albumId, null);
      toast.success('Album removed from wanted list');
      // SSE will update the album state automatically
    } catch (error) {
      console.error('Failed to unwant album:', error);
      toast.error('Failed to remove album from wanted list');
    }
  };

  const handleForceSearch = async (_albumId: number) => {
    try {
      // TODO: Implement force search API call
      toast.success('Search triggered for album');
    } catch (error) {
      console.error('Failed to trigger search:', error);
      toast.error('Failed to trigger search');
    }
  };

  const handleShowReleases = (album: CatalogAlbumOverview) => {
    setSelectedAlbum(album);
    setShowReleasesModal(true);
  };

  const handleCloseReleasesModal = () => {
    setShowReleasesModal(false);
    setSelectedAlbum(null);
  };

  const handleQualityProfileChange = async (albumId: number, profileId: number | null, isExisting: boolean = false) => {
    try {
      // "Existing" means: keep current quality, don't monitor for upgrades
      // This sets quality_profile_id = null (no profile = not monitored)
      // When there's a profile, the backend derives wanted status automatically
      // - If quality_profile_id IS NULL -> wanted = false (not monitored)
      // - If quality_profile_id IS NOT NULL -> wanted = true (monitored, based on cutoff)
      await api.updateCatalogAlbum(albumId, profileId);

      if (isExisting) {
        toast.success('Album set to keep existing quality - monitoring disabled');
      } else if (profileId) {
        toast.success('Quality profile updated');
      } else {
        toast.success('Album unwanted');
      }
      // SSE will update the album state automatically
    } catch (error) {
      console.error('Failed to update quality profile:', error);
      toast.error('Failed to update quality profile');
    }
  };

  const handleSort = (column: 'title' | 'artist' | 'date' | 'quality' | 'state') => {
    toggleSort(column);
    pagination.resetOffset();
  };

  // Bulk actions
  const toggleSelectAll = () => {
    if (selectedAlbumIds.size === albums.length) {
      setSelectedAlbumIds(new Set());
    } else {
      setSelectedAlbumIds(new Set(albums.map(a => a.id)));
    }
  };

  const toggleSelectAlbum = (albumId: number) => {
    const newSelection = new Set(selectedAlbumIds);
    if (newSelection.has(albumId)) {
      newSelection.delete(albumId);
    } else {
      newSelection.add(albumId);
    }
    setSelectedAlbumIds(newSelection);
  };

  const handleBulkUnwant = async () => {
    if (selectedAlbumIds.size === 0) return;

    try {
      await Promise.all(
        Array.from(selectedAlbumIds).map(id => api.updateCatalogAlbum(id, null))
      );
      toast.success(`Removed ${selectedAlbumIds.size} album(s) from wanted list`);
      setSelectedAlbumIds(new Set());
      // SSE will update the album states automatically
    } catch (error) {
      console.error('Failed to unwant albums:', error);
      toast.error('Failed to remove albums from wanted list');
    }
  };

  const handleBulkForceSearch = async () => {
    if (selectedAlbumIds.size === 0) return;

    toast(`Force search for ${selectedAlbumIds.size} album(s) - feature coming soon`);
    // TODO: Implement bulk force search API call
  };

  const handleBulkQualityChange = async (profileId: number | null, isExisting: boolean = false) => {
    if (selectedAlbumIds.size === 0) return;

    try {
      await Promise.all(
        Array.from(selectedAlbumIds).map(id => api.updateCatalogAlbum(id, profileId))
      );

      if (isExisting) {
        toast.success(`Updated ${selectedAlbumIds.size} album(s) to keep existing quality`);
      } else {
        toast.success(`Updated quality profile for ${selectedAlbumIds.size} album(s)`);
      }

      setSelectedAlbumIds(new Set());
      // SSE will update the album states automatically
    } catch (error) {
      console.error('Failed to update quality profiles:', error);
      toast.error('Failed to update quality profiles');
    }
  };

  const SortableHeader = ({ column, children }: { column: 'title' | 'artist' | 'date' | 'quality' | 'state'; children: React.ReactNode }) => {
    const isActive = sortBy === column;
    return (
      <th
        className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider cursor-pointer hover:text-dark-text-secondary transition-colors select-none"
        onClick={() => handleSort(column)}
      >
        <div className="flex items-center gap-2">
          {children}
          {isActive ? (
            sortOrder === 'asc' ? (
              <ArrowUp className="h-3 w-3" />
            ) : (
              <ArrowDown className="h-3 w-3" />
            )
          ) : (
            <ArrowUpDown className="h-3 w-3 opacity-30" />
          )}
        </div>
      </th>
    );
  };

  const formatQuality = (quality: string | null) => {
    if (!quality) return 'Unknown';
    // Convert quality strings to readable format
    const qualityMap: Record<string, string> = {
      'unknown': 'Unknown',
      'mp3_192': 'MP3 192',
      'vbr2': 'VBR V2',
      'mp3_256': 'MP3 256',
      'vbr0': 'VBR V0',
      'mp3_320': 'MP3 320',
      'lossless': 'FLAC',
      'hires_lossless': 'Hi-Res FLAC',
    };
    return qualityMap[quality] || quality;
  };

  const getQualityBadgeStyle = (quality: string | null): string => {
    if (!quality) return 'bg-gray-500/20 text-gray-400 border-gray-500/30';

    const styleMap: Record<string, string> = {
      'unknown': 'bg-gray-500/20 text-gray-400 border-gray-500/30',
      'mp3_192': 'bg-orange-500/20 text-orange-400 border-orange-500/30',
      'vbr2': 'bg-orange-500/20 text-orange-400 border-orange-500/30',
      'mp3_256': 'bg-yellow-500/20 text-yellow-400 border-yellow-500/30',
      'vbr0': 'bg-yellow-500/20 text-yellow-400 border-yellow-500/30',
      'mp3_320': 'bg-lime-500/20 text-lime-400 border-lime-500/30',
      'lossless': 'bg-green-500/20 text-green-400 border-green-500/30',
      'hires_lossless': 'bg-blue-500/20 text-blue-400 border-blue-500/30',
    };
    return styleMap[quality] || 'bg-gray-500/20 text-gray-400 border-gray-500/30';
  };

  const QualityBadge = ({ quality }: { quality: string | null }) => (
    <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium border ${getQualityBadgeStyle(quality)}`}>
      {formatQuality(quality)}
    </span>
  );

  const StateBadge = ({ state }: { state: AlbumState }) => {
    const config = stateConfig[state];
    const Icon = config.icon;
    return (
      <span className={`inline-flex items-center gap-1 px-2.5 py-0.5 rounded-full text-xs font-medium ${config.color}`}>
        <Icon className="h-3 w-3" />
        {config.label}
      </span>
    );
  };


  const handleQuickFilter = (filter: QuickFilter) => {
    setQuickFilter(filter);
    pagination.resetOffset();
  };

  return (
    <div className="space-y-6 animate-fade-in">
      {/* Header with Quick Filters */}
      <div>
        <h1 className="text-3xl font-bold text-dark-text mb-4">Albums</h1>
        <div className="flex flex-wrap gap-2">
          <button
            onClick={() => handleQuickFilter('all')}
            className={`px-4 py-2 rounded-lg font-medium transition-all ${
              quickFilter === 'all'
                ? 'bg-dark-accent text-white'
                : 'bg-dark-bg-subtle text-dark-text-secondary hover:bg-dark-bg-hover'
            }`}
          >
            All
          </button>
          <button
            onClick={() => handleQuickFilter('tracked')}
            className={`px-4 py-2 rounded-lg font-medium transition-all ${
              quickFilter === 'tracked'
                ? 'bg-dark-accent text-white'
                : 'bg-dark-bg-subtle text-dark-text-secondary hover:bg-dark-bg-hover'
            }`}
          >
            All Tracked
          </button>
          <button
            onClick={() => handleQuickFilter('unacquired')}
            className={`px-4 py-2 rounded-lg font-medium transition-all ${
              quickFilter === 'unacquired'
                ? 'bg-dark-accent text-white'
                : 'bg-dark-bg-subtle text-dark-text-secondary hover:bg-dark-bg-hover'
            }`}
          >
            Unacquired
          </button>
          <button
            onClick={() => handleQuickFilter('library')}
            className={`px-4 py-2 rounded-lg font-medium transition-all ${
              quickFilter === 'library'
                ? 'bg-dark-accent text-white'
                : 'bg-dark-bg-subtle text-dark-text-secondary hover:bg-dark-bg-hover'
            }`}
          >
            In Library
          </button>
          <button
            onClick={() => handleQuickFilter('upcoming')}
            className={`px-4 py-2 rounded-lg font-medium transition-all ${
              quickFilter === 'upcoming'
                ? 'bg-dark-accent text-white'
                : 'bg-dark-bg-subtle text-dark-text-secondary hover:bg-dark-bg-hover'
            }`}
          >
            Upcoming
          </button>
        </div>
      </div>

      {/* Filters */}
      <div className="sticky top-0 z-10 pt-4 pb-4 mb-4 -mx-4 sm:-mx-6 lg:-mx-8 px-4 sm:px-6 lg:px-8 bg-dark-bg">
        <div className="card p-4 space-y-4">
        <div className="flex items-center gap-2 text-sm font-medium text-dark-text">
          <Filter className="h-4 w-4" />
          <span>Filters</span>
        </div>

        <div className="space-y-4">
          {/* Search */}
          <div className="relative w-full max-w-md">
            <input
              type="text"
              placeholder="Search albums or artists..."
              value={searchInput}
              onChange={(e) => setSearchInput(e.target.value)}
              className="input w-full pr-10"
            />
            {searching && (
              <div className="absolute top-1/2 right-3 -translate-y-1/2 pointer-events-none">
                <Loader2 className="h-4 w-4 animate-spin text-dark-text-secondary" />
              </div>
            )}
          </div>

          {/* Quality Filter */}
          <div className="space-y-2">
            <label className="text-xs font-medium text-dark-text-secondary">Filter by Quality:</label>
              <div className="flex flex-wrap gap-3">
                {(['unknown', 'mp3_192', 'vbr2', 'mp3_256', 'vbr0', 'mp3_320', 'lossless', 'hires_lossless'] as const).map((quality) => {
                  const isSelected = qualityFilter.includes(quality);
                  return (
                    <button
                      key={quality}
                      onClick={() => {
                        toggleQuality(quality);
                        pagination.resetOffset();
                      }}
                      className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium transition-all ${getQualityBadgeStyle(quality)} ${
                        isSelected
                          ? 'ring-2 ring-current shadow-md'
                          : 'border hover:brightness-110 hover:-translate-y-0.5'
                      }`}
                    >
                      {formatQuality(quality)}
                    </button>
                  );
                })}
                {qualityFilter.length > 0 && (
                  <button
                    onClick={() => {
                      clearQualityFilter();
                      pagination.resetOffset();
                    }}
                    className="inline-flex items-center gap-1 px-2.5 py-0.5 rounded-full text-xs font-medium text-dark-text-tertiary hover:text-dark-error border border-dark-border hover:border-dark-error transition-colors"
                  >
                    <X className="h-3 w-3" />
                    Clear
                  </button>
                )}
              </div>
            </div>

          {/* Status Filter */}
          <div className="space-y-2">
            <label className="text-xs font-medium text-dark-text-secondary">Filter by Status:</label>
            <div className="flex flex-wrap gap-3">
              {(['Wanted', 'Failed', 'IdentificationFailed', 'InLibrary', 'upgrading-combined', 'NotWanted'] as (AlbumState | 'upgrading-combined')[]).map((status) => {
                // Special handling - combine Monitored and Upgrading into one "Upgrading" badge
                if (status === 'upgrading-combined') {
                  const isSelected = statusFilter.includes('Monitored') || statusFilter.includes('Upgrading');
                  const config = stateConfig['Upgrading'];
                  const Icon = config.icon;
                  return (
                    <button
                      key="upgrading-combined"
                      onClick={() => {
                        toggleUpgrading();
                        pagination.resetOffset();
                      }}
                      className={`inline-flex items-center gap-1 px-2.5 py-0.5 rounded-full text-xs font-medium transition-all ${config.color} ${
                        isSelected
                          ? 'ring-2 ring-current shadow-md'
                          : 'opacity-75 hover:opacity-100 hover:-translate-y-0.5'
                      }`}
                    >
                      <Icon className="h-3 w-3" />
                      {config.label}
                    </button>
                  );
                }

                const isSelected = statusFilter.includes(status);
                const config = stateConfig[status];
                const Icon = config.icon;
                return (
                  <button
                    key={status}
                    onClick={() => {
                      toggleStatus(status);
                      pagination.resetOffset();
                    }}
                    className={`inline-flex items-center gap-1 px-2.5 py-0.5 rounded-full text-xs font-medium transition-all ${config.color} ${
                      isSelected
                        ? 'ring-2 ring-current shadow-md'
                        : 'opacity-75 hover:opacity-100 hover:-translate-y-0.5'
                    }`}
                  >
                    <Icon className="h-3 w-3" />
                    {config.label}
                  </button>
                );
              })}
              {statusFilter.length > 0 && (
                <button
                  onClick={() => {
                    clearStatusFilter();
                    pagination.resetOffset();
                  }}
                  className="inline-flex items-center gap-1 px-2.5 py-0.5 rounded-full text-xs font-medium text-dark-text-tertiary hover:text-dark-error border border-dark-border hover:border-dark-error transition-colors"
                >
                  <X className="h-3 w-3" />
                  Clear
                </button>
              )}
            </div>
          </div>

        </div>
        </div>
      </div>

      {/* Bulk Actions Bar */}
      {selectedAlbumIds.size > 0 && (
        <div className="card p-4 bg-dark-accent-muted border-dark-accent">
          <div className="flex flex-wrap items-center gap-3">
            <div className="flex items-center gap-2 text-sm font-medium text-dark-text">
              <CheckSquare className="h-4 w-4 text-dark-accent" />
              <span>{selectedAlbumIds.size} album{selectedAlbumIds.size !== 1 ? 's' : ''} selected</span>
            </div>

            <div className="flex flex-wrap items-center gap-2">
              <button
                onClick={handleBulkUnwant}
                className="px-3 py-1.5 bg-dark-bg-hover hover:bg-dark-error-muted text-dark-text-secondary hover:text-dark-error rounded transition-colors flex items-center gap-1.5 text-sm"
              >
                <X className="h-4 w-4" />
                <span>Unwant</span>
              </button>
              <select
                onChange={(e) => {
                  const value = e.target.value;
                  if (value === 'default') {
                    if (defaultProfile?.id) {
                      handleBulkQualityChange(defaultProfile.id, false);
                    } else {
                      toast.error('No default quality profile configured');
                    }
                  } else if (value) {
                    handleBulkQualityChange(parseInt(value, 10));
                  }
                  e.target.value = '';
                }}
                className="text-sm bg-dark-bg-hover border border-dark-border rounded px-3 py-1.5 text-dark-text focus:outline-none focus:ring-2 focus:ring-dark-accent"
                defaultValue=""
              >
                <option value="" disabled>Change Target Quality...</option>
                <option value="default">
                  {defaultProfile ? `Default (${defaultProfile.name})` : 'Default'}
                </option>
                {qualityProfiles.map((profile) => (
                  <option key={profile.id} value={profile.id}>
                    {profile.name}
                  </option>
                ))}
              </select>

              <button
                onClick={handleBulkForceSearch}
                className="px-3 py-1.5 bg-dark-bg-hover hover:bg-dark-accent-muted text-dark-text-secondary hover:text-dark-accent rounded transition-colors flex items-center gap-1.5 text-sm"
              >
                <SearchIcon className="h-4 w-4" />
                <span>Force Search</span>
              </button>

              <button
                onClick={() => setSelectedAlbumIds(new Set())}
                className="px-3 py-1.5 bg-dark-bg-hover hover:bg-dark-bg-subtle text-dark-text-secondary hover:text-dark-text rounded transition-colors flex items-center gap-1.5 text-sm"
              >
                <X className="h-4 w-4" />
                <span>Clear Selection</span>
              </button>
            </div>
          </div>
        </div>
      )}

      {/* Albums Table */}
      <LoadingState
        loading={loading || tableLoading}
        empty={albums.length === 0}
        skeleton={
          <div className="card overflow-hidden relative">
            <div className="overflow-x-auto">
              <table className="min-w-full divide-y divide-dark-border">
                <thead className="bg-dark-bg-subtle">
                  <tr>
                    <th className="px-6 py-3 text-left"><Square className="h-4 w-4" /></th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">Album</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">Artist</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">Release Date</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">State</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">Current Quality</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">Target Quality</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">Actions</th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-dark-border">
                  {[...Array(10)].map((_, i) => (
                    <TableRowSkeleton key={i} columns={8} />
                  ))}
                </tbody>
              </table>
            </div>
          </div>
        }
        emptyState={
          <div className="card p-12 text-center">
            <Disc className="mx-auto h-12 w-12 text-dark-text-tertiary" />
            <h3 className="mt-4 text-sm font-medium text-dark-text">No albums found</h3>
            <p className="mt-2 text-sm text-dark-text-secondary">
              {searchFilter
                ? 'Try adjusting your search'
                : 'No albums in catalog'}
            </p>
          </div>
        }
      >
        <>
          <div className="card overflow-hidden relative">
            <div className="overflow-x-auto">
              <table className="min-w-full divide-y divide-dark-border">
                <thead className="bg-dark-bg-subtle">
                  <tr>
                    <th className="px-6 py-3 text-left">
                      <button
                        onClick={toggleSelectAll}
                        className="text-dark-text-tertiary hover:text-dark-text transition-colors"
                        title={selectedAlbumIds.size === albums.length ? 'Deselect all' : 'Select all'}
                      >
                        {selectedAlbumIds.size === albums.length && albums.length > 0 ? (
                          <CheckSquare className="h-4 w-4" />
                        ) : (
                          <Square className="h-4 w-4" />
                        )}
                      </button>
                    </th>
                    <SortableHeader column="title">Album</SortableHeader>
                    <SortableHeader column="artist">Artist</SortableHeader>
                    <SortableHeader column="date">Release Date</SortableHeader>
                    <SortableHeader column="state">State</SortableHeader>
                    <SortableHeader column="quality">Current Quality</SortableHeader>
                    <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">
                      Target Quality
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">
                      Actions
                    </th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-dark-border">
                  {albums.map((album) => (
                    <tr key={album.id} className="hover:bg-dark-bg-hover transition-colors duration-150">
                      <td className="px-6 py-4">
                        <button
                          onClick={() => toggleSelectAlbum(album.id)}
                          className="text-dark-text-tertiary hover:text-dark-text transition-colors"
                        >
                          {selectedAlbumIds.has(album.id) ? (
                            <CheckSquare className="h-4 w-4 text-dark-accent" />
                          ) : (
                            <Square className="h-4 w-4" />
                          )}
                        </button>
                      </td>
                      <td className="px-6 py-4">
                        <div className="flex items-center gap-3">
                          {album.cover_thumbnail_url ? (
                            <img
                              src={album.cover_thumbnail_url}
                              alt={album.title}
                              className="h-10 w-10 rounded object-cover flex-shrink-0"
                            />
                          ) : (
                            <div className="h-10 w-10 rounded bg-dark-bg-subtle flex items-center justify-center flex-shrink-0">
                              <Disc className="h-5 w-5 text-dark-text-tertiary" />
                            </div>
                          )}
                          <div>
                            <div className="text-sm font-medium text-dark-text">
                              {album.title}
                            </div>
                            {album.type && (
                              <div className="text-xs text-dark-text-tertiary">
                                {album.type}
                              </div>
                            )}
                          </div>
                        </div>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        <div className="text-sm text-dark-text-secondary">{album.artist_name}</div>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        <div className="text-sm text-dark-text-secondary">
                          {formatDate(album.first_release_date)}
                        </div>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        <StateBadge state={album.state} />
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        {album.current_quality ? (
                          <QualityBadge quality={album.current_quality} />
                        ) : (
                          <span className="text-sm text-dark-text-tertiary">—</span>
                        )}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        <select
                          value={album.quality_profile_id || ((album.state === 'InLibrary' || album.state === 'Monitored') ? 'existing' : (unwantedAlbumSelections.get(album.id) || 'default'))}
                          onChange={(e) => {
                            const value = e.target.value;
                            // Check if album is in library (has current_quality) rather than checking quality_profile_id
                            // because "Existing" albums have NULL quality_profile_id but are still tracked
                            const isTrackedAlbum = album.quality_profile_id !== null || album.current_quality !== null;

                            if (isTrackedAlbum) {
                              // Album is already tracked (either wanted or existing), apply the change immediately
                              if (value === 'existing') {
                                // Keep existing quality, no monitoring
                                handleQualityProfileChange(album.id, null, true);
                              } else if (value === 'default') {
                                if (defaultProfile?.id) {
                                  handleQualityProfileChange(album.id, defaultProfile.id, false);
                                } else {
                                  toast.error('No default quality profile configured');
                                }
                              } else {
                                const profileId = parseInt(value, 10);
                                handleQualityProfileChange(album.id, profileId, false);
                              }
                            } else {
                              // Album is not tracked at all, just update the selection state
                              setUnwantedAlbumSelections(new Map(unwantedAlbumSelections.set(album.id, value)));
                            }
                          }}
                          className="text-sm bg-dark-bg-hover border border-dark-border rounded px-2 py-1 text-dark-text focus:outline-none focus:ring-2 focus:ring-dark-accent"
                        >
                          {(album.state === 'InLibrary' || album.state === 'Monitored') && !album.quality_profile_id && (
                            <option value="existing">Existing</option>
                          )}
                          <option value="default">
                            {defaultProfile ? `Default (${defaultProfile.name})` : 'Default'}
                          </option>
                          {qualityProfiles.map((profile) => (
                            <option key={profile.id} value={profile.id}>
                              {profile.name}
                            </option>
                          ))}
                        </select>
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap">
                        <div className="flex items-center gap-2">
                          <button
                            onClick={() => handleShowReleases(album)}
                            className="px-3 py-1.5 bg-dark-bg-hover hover:bg-dark-accent-muted text-dark-text-secondary hover:text-dark-accent rounded transition-colors flex items-center gap-1.5"
                            title="Show available releases"
                          >
                            <List className="h-4 w-4" />
                            <span>Releases</span>
                          </button>
                          {album.quality_profile_id ? (
                            <>
                              <button
                                onClick={() => handleForceSearch(album.id)}
                                className="px-3 py-1.5 bg-dark-bg-hover hover:bg-dark-accent-muted text-dark-text-secondary hover:text-dark-accent rounded transition-colors flex items-center gap-1.5"
                                title="Force search for this album"
                              >
                                <SearchIcon className="h-4 w-4" />
                                <span>Search</span>
                              </button>
                              <button
                                onClick={() => handleUnwantAlbum(album.id)}
                                className="px-3 py-1.5 bg-dark-bg-hover hover:bg-dark-error-muted text-dark-text-secondary hover:text-dark-error rounded transition-colors flex items-center gap-1.5"
                                title="Remove from wanted list"
                              >
                                <X className="h-4 w-4" />
                                <span>Unwant</span>
                              </button>
                            </>
                          ) : (
                            <button
                              onClick={() => {
                                const selectedValue = unwantedAlbumSelections.get(album.id) || 'default';
                                if (selectedValue === 'existing') {
                                  handleQualityProfileChange(album.id, null, true);
                                } else if (selectedValue === 'default') {
                                  if (defaultProfile?.id) {
                                    handleQualityProfileChange(album.id, defaultProfile.id, false);
                                  } else {
                                    toast.error('No default quality profile configured');
                                  }
                                } else {
                                  const profileId = parseInt(selectedValue, 10);
                                  handleQualityProfileChange(album.id, profileId, false);
                                }
                              }}
                              className="px-3 py-1.5 bg-dark-accent hover:bg-dark-accent-muted text-dark-bg rounded transition-colors flex items-center gap-1.5"
                              title="Add to wanted list with selected quality profile"
                            >
                              <Heart className="h-4 w-4" />
                              <span>Want</span>
                            </button>
                          )}
                        </div>
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>

          <PaginationControls
            offset={pagination.offset}
            limit={ITEMS_PER_PAGE}
            total={pagination.totalCount}
            onPrevPage={pagination.prevPage}
            onNextPage={pagination.nextPage}
            itemName="albums"
          />
        </>
      </LoadingState>

      {/* Releases Modal */}
      {showReleasesModal && selectedAlbum && (
        <AlbumReleasesModal
          album={selectedAlbum}
          onClose={handleCloseReleasesModal}
        />
      )}
    </div>
  );
}
