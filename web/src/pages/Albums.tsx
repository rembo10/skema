import { useEffect, useState, useCallback } from 'react';
import { useLocation, useNavigate } from 'react-router-dom';
import { api } from '../lib/api';
import toast from 'react-hot-toast';
import { PaginationControls } from '../components/PaginationControls';
import { usePagination } from '../hooks/usePagination';
import {
  AlbumState,
  CatalogAlbumOverview,
  AlbumOverviewRequest,
  AlbumRelease,
  QualityProfile,
} from '../types/api';
import {
  Music,
  Download,
  Search as SearchIcon,
  CheckCircle2,
  XCircle,
  Loader2,
  AlertCircle,
  Eye,
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
} from 'lucide-react';

const ITEMS_PER_PAGE = 50;

const stateConfig: Record<AlbumState, { label: string; icon: typeof Music; color: string }> = {
  NotWanted: { label: 'Not Wanted', icon: X, color: 'text-gray-400' },
  Wanted: { label: 'Wanted', icon: Music, color: 'text-blue-500' },
  Searching: { label: 'Searching', icon: SearchIcon, color: 'text-yellow-500' },
  Downloading: { label: 'Downloading', icon: Download, color: 'text-orange-500' },
  Failed: { label: 'Failed', icon: XCircle, color: 'text-red-500' },
  IdentificationFailed: { label: 'ID Failed', icon: AlertCircle, color: 'text-red-400' },
  InLibrary: { label: 'Finished', icon: CheckCircle2, color: 'text-green-500' },
  Monitored: { label: 'Upgrading', icon: Eye, color: 'text-blue-400' },
  Upgrading: { label: 'Upgrading', icon: ArrowUpCircle, color: 'text-purple-500' },
};

type TabView = 'unacquired' | 'library';

export default function Albums() {
  const location = useLocation();
  const navigate = useNavigate();

  // Determine active tab from URL path
  const getTabFromPath = (pathname: string): TabView => {
    if (pathname === '/albums/library') return 'library';
    return 'unacquired';
  };

  const [activeTab, setActiveTab] = useState<TabView>(() => getTabFromPath(location.pathname));
  const [albums, setAlbums] = useState<CatalogAlbumOverview[]>([]);
  const [loading, setLoading] = useState(true);
  const [searching, setSearching] = useState(false);
  const [searchInput, setSearchInput] = useState('');
  const [searchFilter, setSearchFilter] = useState('');
  const [sortBy, setSortBy] = useState<'title' | 'artist' | 'date'>('date');
  const [sortOrder, setSortOrder] = useState<'asc' | 'desc'>('desc');
  const pagination = usePagination(ITEMS_PER_PAGE);
  const [showReleasesModal, setShowReleasesModal] = useState(false);
  const [selectedAlbum, setSelectedAlbum] = useState<CatalogAlbumOverview | null>(null);
  const [searchingReleases, setSearchingReleases] = useState(false);
  const [releases, setReleases] = useState<AlbumRelease[]>([]);
  const [qualityProfiles, setQualityProfiles] = useState<QualityProfile[]>([]);
  const [defaultProfile, setDefaultProfile] = useState<QualityProfile | null>(null);
  const [selectedAlbumIds, setSelectedAlbumIds] = useState<Set<number>>(new Set());

  // Sync active tab with URL changes
  useEffect(() => {
    const newTab = getTabFromPath(location.pathname);
    if (newTab !== activeTab) {
      setActiveTab(newTab);
    }
  }, [location.pathname]);

  // Redirect /albums to /albums/unacquired
  useEffect(() => {
    if (location.pathname === '/albums') {
      navigate('/albums/unacquired', { replace: true });
    }
  }, [location.pathname, navigate]);

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
  }, [searchInput]);

  useEffect(() => {
    loadAlbums();
    // Clear selections when tab changes
    setSelectedAlbumIds(new Set());
  }, [activeTab, pagination.offset, searchFilter, sortBy, sortOrder]);

  useEffect(() => {
    loadQualityProfiles();
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
    // Only show full loading spinner on initial load or tab change
    if (albums.length === 0) {
      setLoading(true);
    }
    try {
      // Filter by state based on active tab
      const stateFilter = activeTab === 'unacquired'
        ? ['Wanted', 'Searching', 'Downloading', 'Failed', 'IdentificationFailed'] as AlbumState[]
        : ['InLibrary', 'Monitored', 'Upgrading'] as AlbumState[];

      const request: AlbumOverviewRequest = {
        offset: pagination.offset,
        limit: ITEMS_PER_PAGE,
        search: searchFilter || undefined,
        sort: sortBy,
        order: sortOrder,
        state: stateFilter,
      };

      const response = await api.getAlbumOverview(request);

      if (!response || !response.pagination) {
        console.error('Invalid response structure:', response);
        toast.error('Invalid response from server');
        return;
      }

      setAlbums(response.albums || []);
      pagination.setTotalCount(response.pagination.total);
    } catch (error) {
      console.error('Failed to load albums:', error);
      toast.error('Failed to load albums');
    } finally {
      setLoading(false);
    }
  };

  const handleUnwantAlbum = async (albumId: number) => {
    try {
      // Set quality_profile_id = null to unwant (remove from monitoring)
      await api.updateCatalogAlbum(albumId, null);
      toast.success('Album removed from wanted list');
      loadAlbums(); // Reload to reflect changes
    } catch (error) {
      console.error('Failed to unwant album:', error);
      toast.error('Failed to remove album from wanted list');
    }
  };

  const handleForceSearch = async (albumId: number) => {
    try {
      // TODO: Implement force search API call
      toast.success('Search triggered for album');
    } catch (error) {
      console.error('Failed to trigger search:', error);
      toast.error('Failed to trigger search');
    }
  };

  const handleShowReleases = async (album: CatalogAlbumOverview) => {
    setSelectedAlbum(album);
    setShowReleasesModal(true);
    setSearchingReleases(true);
    setReleases([]);

    try {
      const response = await api.getAlbumReleases(album.id);
      setReleases(response.releases);

      if (response.releases.length === 0) {
        toast.info('No releases found from indexers');
      } else {
        toast.success(`Found ${response.releases.length} release(s) in ${response.search_time.toFixed(1)}s`);
      }
    } catch (error) {
      console.error('Failed to search for releases:', error);
      toast.error('Failed to search for releases');
    } finally {
      setSearchingReleases(false);
    }
  };

  const handleQualityProfileChange = async (albumId: number, profileId: number | null, isExisting: boolean = false) => {
    try {
      // "Existing" means: keep current quality, don't monitor for upgrades
      // This sets quality_profile_id = null (no profile = not monitored)
      // When there's a profile, the backend derives wanted status automatically
      // - If quality_profile_id IS NULL -> wanted = false (not monitored)
      // - If quality_profile_id IS NOT NULL -> wanted = true (monitored, based on cutoff)
      await api.updateCatalogAlbum(albumId, profileId);

      if (isExisting && activeTab === 'library') {
        toast.success('Album set to keep existing quality - monitoring disabled');
      } else if (activeTab === 'library') {
        toast.success('Desired quality updated - album will be monitored for upgrades');
      } else {
        toast.success('Quality profile updated');
      }
      loadAlbums(); // Reload to reflect changes
    } catch (error) {
      console.error('Failed to update quality profile:', error);
      toast.error('Failed to update quality profile');
    }
  };

  const handleSort = (column: 'title' | 'artist' | 'date') => {
    if (sortBy === column) {
      // Toggle sort order if clicking the same column
      setSortOrder(sortOrder === 'asc' ? 'desc' : 'asc');
    } else {
      // New column, set to descending by default
      setSortBy(column);
      setSortOrder('desc');
    }
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
      loadAlbums();
    } catch (error) {
      console.error('Failed to unwant albums:', error);
      toast.error('Failed to remove albums from wanted list');
    }
  };

  const handleBulkForceSearch = async () => {
    if (selectedAlbumIds.size === 0) return;

    toast.info(`Force search for ${selectedAlbumIds.size} album(s) - feature coming soon`);
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
      loadAlbums();
    } catch (error) {
      console.error('Failed to update quality profiles:', error);
      toast.error('Failed to update quality profiles');
    }
  };

  const SortableHeader = ({ column, children }: { column: 'title' | 'artist' | 'date'; children: React.ReactNode }) => {
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

  const formatDate = (dateString: string | null) => {
    if (!dateString) return 'Unknown';
    try {
      if (dateString.includes('-')) {
        return new Date(dateString).toLocaleDateString('en-US', {
          year: 'numeric',
          month: 'short',
          day: 'numeric',
        });
      }
      return dateString;
    } catch {
      return dateString;
    }
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


  if (loading) {
    return (
      <div className="flex items-center justify-center h-64">
        <Loader2 className="h-8 w-8 animate-spin text-dark-accent" />
      </div>
    );
  }

  return (
    <div className="space-y-6 animate-fade-in">
      {/* Tabs */}
      <div className="flex gap-2 border-b border-dark-border mb-6">
        <button
          onClick={() => {
            navigate('/albums/unacquired');
            pagination.resetOffset();
          }}
          className={`px-4 py-2 font-medium border-b-2 transition-colors ${
            activeTab === 'unacquired'
              ? 'border-dark-accent text-dark-accent'
              : 'border-transparent text-dark-text-secondary hover:text-dark-text'
          }`}
        >
          Unacquired Albums
        </button>
        <button
          onClick={() => {
            navigate('/albums/library');
            pagination.resetOffset();
          }}
          className={`px-4 py-2 font-medium border-b-2 transition-colors ${
            activeTab === 'library'
              ? 'border-dark-accent text-dark-accent'
              : 'border-transparent text-dark-text-secondary hover:text-dark-text'
          }`}
        >
          Library Albums
        </button>
      </div>

      {/* Header */}
      <div>
        <h1 className="text-3xl font-bold text-dark-text">Albums</h1>
        <p className="text-dark-text-secondary mt-2">
          Manage your wanted albums and quality settings for existing albums
        </p>
      </div>

      {/* Filters */}
      <div className="sticky top-0 z-10 pt-4 pb-4 mb-4 -mx-4 sm:-mx-6 lg:-mx-8 px-4 sm:px-6 lg:px-8 bg-dark-bg">
        <div className="card p-4 space-y-4">
        <div className="flex items-center gap-2 text-sm font-medium text-dark-text">
          <Filter className="h-4 w-4" />
          <span>Filters</span>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          {/* Search */}
          <div className="relative w-full">
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
              {activeTab === 'unacquired' && (
                <>
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
                      if (value) {
                        handleBulkQualityChange(parseInt(value, 10));
                        e.target.value = '';
                      }
                    }}
                    className="text-sm bg-dark-bg-hover border border-dark-border rounded px-3 py-1.5 text-dark-text focus:outline-none focus:ring-2 focus:ring-dark-accent"
                    defaultValue=""
                  >
                    <option value="" disabled>Change Quality...</option>
                    <option value="default">
                      {defaultProfile ? `Default (${defaultProfile.name})` : 'Default'}
                    </option>
                    {qualityProfiles.map((profile) => (
                      <option key={profile.id} value={profile.id}>
                        {profile.name}
                      </option>
                    ))}
                  </select>
                </>
              )}

              {activeTab === 'library' && (
                <select
                  onChange={(e) => {
                    const value = e.target.value;
                    if (value === 'existing') {
                      handleBulkQualityChange(null, true);
                    } else if (value) {
                      handleBulkQualityChange(parseInt(value, 10));
                    }
                    e.target.value = '';
                  }}
                  className="text-sm bg-dark-bg-hover border border-dark-border rounded px-3 py-1.5 text-dark-text focus:outline-none focus:ring-2 focus:ring-dark-accent"
                  defaultValue=""
                >
                  <option value="" disabled>Change Desired Quality...</option>
                  <option value="existing">Existing</option>
                  <option value="default">
                    {defaultProfile ? `Default (${defaultProfile.name})` : 'Default'}
                  </option>
                  {qualityProfiles.map((profile) => (
                    <option key={profile.id} value={profile.id}>
                      {profile.name}
                    </option>
                  ))}
                </select>
              )}

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
      {albums.length === 0 ? (
        <div className="card p-12 text-center">
          <Disc className="mx-auto h-12 w-12 text-dark-text-tertiary" />
          <h3 className="mt-4 text-sm font-medium text-dark-text">No albums found</h3>
          <p className="mt-2 text-sm text-dark-text-secondary">
            {searchFilter
              ? 'Try adjusting your search'
              : 'No albums in catalog'}
          </p>
        </div>
      ) : (
        <>
          <div className="card overflow-hidden">
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
                    {activeTab === 'unacquired' ? (
                      <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">
                        Quality Profile
                      </th>
                    ) : (
                      <>
                        <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">
                          Current Quality
                        </th>
                        <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">
                          Desired Quality
                        </th>
                        <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">
                          Status
                        </th>
                      </>
                    )}
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
                      {activeTab === 'unacquired' ? (
                        <td className="px-6 py-4 whitespace-nowrap">
                          <select
                            value={album.quality_profile_id || ''}
                            onChange={(e) => {
                              const profileId = e.target.value ? parseInt(e.target.value, 10) : null;
                              handleQualityProfileChange(album.id, profileId);
                            }}
                            className="text-sm bg-dark-bg-hover border border-dark-border rounded px-2 py-1 text-dark-text focus:outline-none focus:ring-2 focus:ring-dark-accent"
                          >
                            <option value="">
                              {defaultProfile ? `Default (${defaultProfile.name})` : 'Default'}
                            </option>
                            {qualityProfiles.map((profile) => (
                              <option key={profile.id} value={profile.id}>
                                {profile.name}
                              </option>
                            ))}
                          </select>
                        </td>
                      ) : (
                        <>
                          <td className="px-6 py-4 whitespace-nowrap">
                            <QualityBadge quality={album.current_quality} />
                          </td>
                          <td className="px-6 py-4 whitespace-nowrap">
                            <select
                              value={album.wanted ? (album.quality_profile_id || '') : 'existing'}
                              onChange={(e) => {
                                const value = e.target.value;
                                if (value === 'existing') {
                                  handleQualityProfileChange(album.id, null, true);
                                } else {
                                  const profileId = value ? parseInt(value, 10) : null;
                                  handleQualityProfileChange(album.id, profileId, false);
                                }
                              }}
                              className="text-sm bg-dark-bg-hover border border-dark-border rounded px-2 py-1 text-dark-text focus:outline-none focus:ring-2 focus:ring-dark-accent"
                            >
                              <option value="existing">Existing</option>
                              <option value="">
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
                            <StateBadge state={album.state} />
                          </td>
                        </>
                      )}
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
                          <button
                            onClick={() => handleForceSearch(album.id)}
                            className="px-3 py-1.5 bg-dark-bg-hover hover:bg-dark-accent-muted text-dark-text-secondary hover:text-dark-accent rounded transition-colors flex items-center gap-1.5"
                            title="Force search for this album"
                          >
                            <SearchIcon className="h-4 w-4" />
                            <span>Search</span>
                          </button>
                          {activeTab === 'unacquired' && (
                            <button
                              onClick={() => handleUnwantAlbum(album.id)}
                              className="px-3 py-1.5 bg-dark-bg-hover hover:bg-dark-error-muted text-dark-text-secondary hover:text-dark-error rounded transition-colors flex items-center gap-1.5"
                              title="Remove from wanted list"
                            >
                              <X className="h-4 w-4" />
                              <span>Unwant</span>
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
      )}

      {/* Releases Modal */}
      {showReleasesModal && selectedAlbum && (
        <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4">
          <div className="bg-dark-bg-elevated rounded-lg shadow-xl max-w-4xl w-full max-h-[80vh] overflow-hidden flex flex-col">
            {/* Modal Header */}
            <div className="px-6 py-4 border-b border-dark-border flex items-center justify-between">
              <div>
                <h2 className="text-xl font-bold text-dark-text">Available Releases</h2>
                <p className="text-sm text-dark-text-secondary mt-1">
                  {selectedAlbum.title} - {selectedAlbum.artist_name}
                </p>
              </div>
              <button
                onClick={() => setShowReleasesModal(false)}
                className="text-dark-text-secondary hover:text-dark-text transition-colors"
              >
                <X className="h-6 w-6" />
              </button>
            </div>

            {/* Modal Body */}
            <div className="flex-1 overflow-y-auto p-6">
              {searchingReleases ? (
                <div className="flex items-center justify-center h-64">
                  <Loader2 className="h-8 w-8 animate-spin text-dark-accent" />
                </div>
              ) : releases.length === 0 ? (
                <div className="text-center py-12">
                  <Disc className="mx-auto h-12 w-12 text-dark-text-tertiary" />
                  <h3 className="mt-4 text-sm font-medium text-dark-text">No releases found</h3>
                  <p className="mt-2 text-sm text-dark-text-secondary">
                    No releases available for this album at the moment
                  </p>
                </div>
              ) : (
                <div className="space-y-3">
                  {releases.map((release, index) => (
                    <div
                      key={index}
                      className="card p-4 hover:bg-dark-bg-hover transition-colors"
                    >
                      <div className="flex items-start justify-between gap-4">
                        <div className="flex-1 min-w-0">
                          <h3 className="text-sm font-medium text-dark-text mb-2">{release.title}</h3>
                          <div className="flex flex-wrap items-center gap-x-4 gap-y-1 text-xs text-dark-text-secondary">
                            <span className="flex items-center gap-1">
                              <span className="text-dark-text-tertiary">Source:</span>
                              <span className="font-medium">{release.source}</span>
                            </span>
                            <span className="flex items-center gap-1">
                              <span className="text-dark-text-tertiary">Quality:</span>
                              <span className="font-medium text-dark-accent">{release.quality}</span>
                            </span>
                            {release.size && (
                              <span className="flex items-center gap-1">
                                <span className="text-dark-text-tertiary">Size:</span>
                                <span className="font-medium">
                                  {release.size >= 1024 * 1024 * 1024
                                    ? `${(release.size / 1024 / 1024 / 1024).toFixed(2)} GB`
                                    : `${(release.size / 1024 / 1024).toFixed(0)} MB`}
                                </span>
                              </span>
                            )}
                            {release.download_type === 'torrent' && (
                              <>
                                {release.seeders !== null && release.seeders !== undefined && (
                                  <span className="flex items-center gap-1">
                                    <span className="text-dark-text-tertiary">Seeders:</span>
                                    <span className={`font-medium ${release.seeders > 0 ? 'text-green-400' : 'text-red-400'}`}>
                                      {release.seeders}
                                    </span>
                                  </span>
                                )}
                                {release.peers !== null && release.peers !== undefined && (
                                  <span className="flex items-center gap-1">
                                    <span className="text-dark-text-tertiary">Peers:</span>
                                    <span className="font-medium">{release.peers}</span>
                                  </span>
                                )}
                              </>
                            )}
                            <span className="flex items-center gap-1">
                              <span className="text-dark-text-tertiary">Type:</span>
                              <span className="uppercase text-xs font-mono font-medium">{release.download_type}</span>
                            </span>
                          </div>
                        </div>
                        <button
                          className="btn-primary px-4 py-2 flex-shrink-0"
                          onClick={async () => {
                            try {
                              const response = await api.queueDownload({
                                catalog_album_id: selectedAlbum.id,
                                indexer_name: release.source,
                                url: release.download_url,
                                title: release.title,
                                size_bytes: release.size,
                                quality: release.quality,
                                format: release.download_type.toUpperCase(),
                                seeders: release.seeders,
                              });

                              if (response.success) {
                                toast.success('Download queued successfully');
                                setShowReleasesModal(false);
                              } else {
                                toast.error(response.message || 'Failed to queue download');
                              }
                            } catch (error) {
                              console.error('Failed to queue download:', error);
                              toast.error('Failed to queue download');
                            }
                          }}
                        >
                          <Download className="h-4 w-4 mr-2" />
                          Download
                        </button>
                      </div>
                    </div>
                  ))}
                </div>
              )}
            </div>

            {/* Modal Footer */}
            <div className="px-6 py-4 border-t border-dark-border flex justify-end">
              <button
                onClick={() => setShowReleasesModal(false)}
                className="btn-secondary"
              >
                Close
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}
