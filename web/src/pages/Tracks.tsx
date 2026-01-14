import { useState, useEffect, useMemo } from 'react';
import { TrackWithCluster, Cluster, TracksStats } from '../types/api';
import { api } from '../lib/api';
import toast from 'react-hot-toast';
import { RematchModal } from '../components/identification/RematchModal';
import { TrackEditModal } from '../components/match-viz/TrackEditModal';
import { PaginationControls } from '../components/PaginationControls';
import { usePagination } from '../hooks/usePagination';
import {
  Loader2,
  Search,
  ArrowUpDown,
  CheckCircle2,
  AlertCircle,
  Lock,
  Zap,
  Edit2,
  RefreshCw,
  Filter,
  Layers,
  Info,
  Music
} from 'lucide-react';

type SortField = 'album' | 'artist' | 'track_title' | 'confidence' | 'status';
type SortDirection = 'asc' | 'desc';
type FilterStatus = 'all' | 'matched' | 'unmatched' | 'locked';

const ITEMS_PER_PAGE = 50;

export default function Tracks() {
  const [tracks, setTracks] = useState<TrackWithCluster[]>([]);
  const [stats, setStats] = useState<TracksStats | null>(null);
  const [loading, setLoading] = useState(true);
  const [searchQuery, setSearchQuery] = useState('');
  const [sortField, setSortField] = useState<SortField>('album');
  const [sortDirection, setSortDirection] = useState<SortDirection>('asc');
  const [filterStatus, setFilterStatus] = useState<FilterStatus>('all');
  const pagination = usePagination(ITEMS_PER_PAGE);
  const [selectedCluster, setSelectedCluster] = useState<Cluster | null>(null);
  const [editingTrack, setEditingTrack] = useState<{
    id: number;
    title: string | null;
    clusterId: number;
  } | null>(null);
  const [viewingTrack, setViewingTrack] = useState<TrackWithCluster | null>(null);
  const [recordingSearchQuery, setRecordingSearchQuery] = useState('');

  useEffect(() => {
    loadData();
  }, [pagination.offset, filterStatus]);

  const loadData = async () => {
    try {
      setLoading(true);
      const [tracksResponse, statsResponse] = await Promise.all([
        api.getAllTracks(pagination.offset, ITEMS_PER_PAGE, filterStatus),
        api.getTracksStats(),
      ]);
      setTracks(tracksResponse.tracks);
      setStats(statsResponse);
      pagination.setTotalCount(tracksResponse.pagination.total);
    } catch (error) {
      console.error('Failed to load data:', error);
      toast.error('Failed to load library data');
    } finally {
      setLoading(false);
    }
  };

  // Client-side search and sort (filter is now server-side)
  const filteredAndSortedRows = useMemo(() => {
    let filtered = tracks;

    // Apply search filter (client-side)
    if (searchQuery) {
      const query = searchQuery.toLowerCase();
      filtered = filtered.filter(track =>
        track.title?.toLowerCase().includes(query) ||
        track.artist?.toLowerCase().includes(query) ||
        track.cluster_album?.toLowerCase().includes(query) ||
        track.cluster_album_artist?.toLowerCase().includes(query) ||
        track.mb_recording_title?.toLowerCase().includes(query) ||
        track.path.toLowerCase().includes(query)
      );
    }

    // Apply sorting (client-side)
    filtered.sort((a, b) => {
      let aVal: any;
      let bVal: any;

      switch (sortField) {
        case 'album':
          aVal = a.cluster_album || '';
          bVal = b.cluster_album || '';
          break;
        case 'artist':
          aVal = a.artist || '';
          bVal = b.artist || '';
          break;
        case 'track_title':
          aVal = a.title || '';
          bVal = b.title || '';
          break;
        case 'confidence':
          aVal = a.mb_confidence ?? -1;
          bVal = b.mb_confidence ?? -1;
          break;
        case 'status':
          aVal = a.mb_release_id ? 1 : 0;
          bVal = b.mb_release_id ? 1 : 0;
          break;
      }

      if (aVal < bVal) return sortDirection === 'asc' ? -1 : 1;
      if (aVal > bVal) return sortDirection === 'asc' ? 1 : -1;
      return 0;
    });

    return filtered;
  }, [tracks, searchQuery, sortField, sortDirection]);

  const handleFilterChange = (newFilter: FilterStatus) => {
    setFilterStatus(newFilter);
    pagination.resetOffset();
  };

  const handleSort = (field: SortField) => {
    if (sortField === field) {
      setSortDirection(sortDirection === 'asc' ? 'desc' : 'asc');
    } else {
      setSortField(field);
      setSortDirection('asc');
    }
  };

  const getStatusDisplay = (track: TrackWithCluster) => {
    if (!track.mb_release_id) {
      return {
        icon: <AlertCircle className="h-4 w-4 text-red-400" />,
        text: 'Unmatched',
        color: 'text-red-400'
      };
    }
    if (track.match_locked) {
      return {
        icon: <Lock className="h-4 w-4 text-purple-400" />,
        text: 'Locked',
        color: 'text-purple-400'
      };
    }
    if (track.match_source === 'auto_fingerprint') {
      return {
        icon: <Zap className="h-4 w-4 text-blue-400" />,
        text: 'Fingerprint',
        color: 'text-blue-400'
      };
    }
    return {
      icon: <CheckCircle2 className="h-4 w-4 text-green-400" />,
      text: 'Matched',
      color: 'text-green-400'
    };
  };

  // Stats for current page only
  const pageStats = useMemo(() => {
    const pageTotal = tracks.length;
    const matched = tracks.filter(t => t.mb_release_id).length;
    const unmatched = pageTotal - matched;
    const locked = tracks.filter(t => t.match_locked).length;
    return { matched, unmatched, locked };
  }, [tracks]);

  const handleToggleLock = async (clusterId: number, currentLocked: boolean) => {
    try {
      await api.lockCluster(clusterId, !currentLocked);
      toast.success(currentLocked ? 'Match unlocked' : 'Match locked');
      loadData();
    } catch (error) {
      console.error('Failed to toggle lock:', error);
      toast.error('Failed to toggle lock');
    }
  };

  const handleOpenCluster = async (track: TrackWithCluster) => {
    if (!track.cluster_id) return;

    try {
      const clusterData = await api.getCluster(track.cluster_id);
      setSelectedCluster(clusterData.cluster);
    } catch (error) {
      console.error('Failed to load cluster:', error);
      toast.error('Failed to load cluster');
    }
  };

  return (
    <div className="h-full flex flex-col animate-fade-in">
      {/* Header */}
      <div className="mb-6">
        <h1 className="text-3xl font-bold text-dark-text">Tracks</h1>
        <p className="text-dark-text-secondary mt-2">
          View and manage MusicBrainz matches for individual tracks
        </p>
      </div>

      {/* Stats */}
      {stats && (
        <div className="grid grid-cols-4 gap-4 mb-6">
          <div className="card p-4">
            <div className="text-2xl font-bold text-dark-text">{stats.total}</div>
            <div className="text-sm text-dark-text-secondary">Total Tracks</div>
          </div>
          <div className="card p-4">
            <div className="text-2xl font-bold text-green-400">{stats.matched}</div>
            <div className="text-sm text-dark-text-secondary">Matched</div>
          </div>
          <div className="card p-4">
            <div className="text-2xl font-bold text-red-400">{stats.unmatched}</div>
            <div className="text-sm text-dark-text-secondary">Unmatched</div>
          </div>
          <div className="card p-4">
            <div className="text-2xl font-bold text-purple-400">{stats.locked}</div>
            <div className="text-sm text-dark-text-secondary">Locked</div>
          </div>
        </div>
      )}

      {/* Controls */}
      <div className="flex gap-3 mb-4">
        {/* Search */}
        <div className="relative flex-1">
          <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-dark-text-tertiary" />
          <input
            type="text"
            placeholder="Search tracks, albums, artists..."
            value={searchQuery}
            onChange={(e) => setSearchQuery(e.target.value)}
            className="w-full pl-9 pr-4 py-2 bg-dark-bg-elevated border border-dark-border rounded-lg text-sm text-dark-text placeholder-dark-text-tertiary focus:outline-none focus:border-dark-accent"
          />
        </div>

        {/* Filter */}
        <div className="flex gap-2 bg-dark-bg-elevated rounded-lg p-1 border border-dark-border">
          <button
            onClick={() => handleFilterChange('all')}
            className={`px-3 py-1.5 rounded text-sm font-medium transition-colors ${
              filterStatus === 'all'
                ? 'bg-dark-accent text-dark-bg'
                : 'text-dark-text-secondary hover:text-dark-text'
            }`}
          >
            All
          </button>
          <button
            onClick={() => handleFilterChange('matched')}
            className={`px-3 py-1.5 rounded text-sm font-medium transition-colors ${
              filterStatus === 'matched'
                ? 'bg-dark-accent text-dark-bg'
                : 'text-dark-text-secondary hover:text-dark-text'
            }`}
          >
            Matched
          </button>
          <button
            onClick={() => handleFilterChange('unmatched')}
            className={`px-3 py-1.5 rounded text-sm font-medium transition-colors ${
              filterStatus === 'unmatched'
                ? 'bg-dark-accent text-dark-bg'
                : 'text-dark-text-secondary hover:text-dark-text'
            }`}
          >
            Unmatched
          </button>
          <button
            onClick={() => handleFilterChange('locked')}
            className={`px-3 py-1.5 rounded text-sm font-medium transition-colors ${
              filterStatus === 'locked'
                ? 'bg-dark-accent text-dark-bg'
                : 'text-dark-text-secondary hover:text-dark-text'
            }`}
          >
            Locked
          </button>
        </div>

        <button
          onClick={loadData}
          className="btn-secondary flex items-center gap-2"
        >
          <RefreshCw className="h-4 w-4" />
          Refresh
        </button>
      </div>

      {/* Table */}
      <div className="flex-1 overflow-auto card relative">
        {loading && (
          <div className="absolute inset-0 bg-dark-bg/80 backdrop-blur-sm flex items-center justify-center z-10">
            <Loader2 className="w-12 h-12 text-dark-accent animate-spin" />
          </div>
        )}
        <table className="w-full">
          <thead className="sticky top-0 bg-dark-bg-elevated border-b border-dark-border">
            <tr>
              <th className="px-4 py-3 text-left">
                <button
                  onClick={() => handleSort('status')}
                  className="flex items-center gap-2 text-xs font-medium text-dark-text-secondary hover:text-dark-text uppercase"
                >
                  Status
                  <ArrowUpDown className="h-3 w-3" />
                </button>
              </th>
              <th className="px-4 py-3 text-left">
                <button
                  onClick={() => handleSort('album')}
                  className="flex items-center gap-2 text-xs font-medium text-dark-text-secondary hover:text-dark-text uppercase"
                >
                  Album / Artist
                  <ArrowUpDown className="h-3 w-3" />
                </button>
              </th>
              <th className="px-4 py-3 text-left">
                <button
                  onClick={() => handleSort('track_title')}
                  className="flex items-center gap-2 text-xs font-medium text-dark-text-secondary hover:text-dark-text uppercase"
                >
                  Your File
                  <ArrowUpDown className="h-3 w-3" />
                </button>
              </th>
              <th className="px-4 py-3 text-left text-xs font-medium text-dark-text-secondary uppercase">
                Matched MB Recording
              </th>
              <th className="px-4 py-3 text-left text-xs font-medium text-dark-text-secondary uppercase">
                Actions
              </th>
            </tr>
          </thead>
          <tbody>
            {filteredAndSortedRows.map((track, index) => (
              <tr
                key={`${track.cluster_id}-${track.id}`}
                className={`border-b border-dark-border hover:bg-dark-bg-elevated transition-colors ${
                  index % 2 === 0 ? 'bg-dark-bg' : 'bg-dark-bg/50'
                }`}
              >
                {/* Status */}
                <td className="px-4 py-3">
                  {(() => {
                    const status = getStatusDisplay(track);
                    return (
                      <div className="flex items-center gap-2">
                        {status.icon}
                        <span className={`text-sm ${status.color}`}>{status.text}</span>
                      </div>
                    );
                  })()}
                </td>

                {/* Album / Artist */}
                <td className="px-4 py-3">
                  <div className="text-sm text-dark-text font-medium">
                    {track.cluster_album || <span className="italic text-dark-text-tertiary">Unknown</span>}
                  </div>
                  <div className="text-xs text-dark-text-tertiary">
                    {track.cluster_album_artist || 'Unknown Artist'}
                  </div>
                </td>

                {/* Your File */}
                <td className="px-4 py-3">
                  <div className="flex items-center gap-2">
                    {track.track_number && (
                      <span className="text-xs font-mono text-dark-text-tertiary flex-shrink-0">
                        {track.disc_number ? `${track.disc_number}-` : ''}{track.track_number}
                      </span>
                    )}
                    <div className="text-sm text-dark-text">
                      {track.title || <span className="italic text-dark-text-tertiary">Unknown</span>}
                    </div>
                  </div>
                  {track.artist && (
                    <div className="text-xs text-dark-text-secondary ml-8">
                      {track.artist}
                    </div>
                  )}
                </td>

                {/* Matched MB Recording */}
                <td className="px-4 py-3">
                  {track.mb_recording_title ? (
                    <div className="text-sm text-dark-text">{track.mb_recording_title}</div>
                  ) : (
                    <span className="text-sm italic text-dark-text-tertiary">Not matched</span>
                  )}
                </td>

                {/* Actions */}
                <td className="px-4 py-3">
                  <div className="flex items-center gap-2">
                    <button
                      onClick={() => handleOpenCluster(track)}
                      disabled={!track.cluster_id}
                      className="text-dark-text-secondary hover:text-dark-accent transition-colors disabled:opacity-30"
                      title="Open cluster"
                    >
                      <Layers className="h-4 w-4" />
                    </button>
                    <button
                      onClick={() => setEditingTrack({
                        id: track.id,
                        title: track.title,
                        clusterId: track.cluster_id || 0,
                      })}
                      className="text-dark-text-secondary hover:text-dark-accent transition-colors"
                      title="Edit recording"
                    >
                      <Music className="h-4 w-4" />
                    </button>
                    <button
                      onClick={() => setViewingTrack(track)}
                      className="text-dark-text-secondary hover:text-dark-accent transition-colors"
                      title="View details"
                    >
                      <Info className="h-4 w-4" />
                    </button>
                  </div>
                </td>
              </tr>
            ))}
          </tbody>
        </table>

        {filteredAndSortedRows.length === 0 && (
          <div className="p-12 text-center">
            <Filter className="mx-auto h-12 w-12 text-dark-text-tertiary" />
            <h3 className="mt-4 text-sm font-medium text-dark-text">No tracks found</h3>
            <p className="mt-2 text-sm text-dark-text-secondary">
              {searchQuery ? 'Try a different search query' : 'No tracks match the current filter'}
            </p>
          </div>
        )}
      </div>

      <PaginationControls
        offset={pagination.offset}
        limit={ITEMS_PER_PAGE}
        total={pagination.totalCount}
        onPrevPage={pagination.prevPage}
        onNextPage={pagination.nextPage}
        itemName="tracks"
        filteredCount={filteredAndSortedRows.length < tracks.length ? filteredAndSortedRows.length : undefined}
      />

      {/* Modals */}
      {selectedCluster && (
        <RematchModal
          cluster={selectedCluster}
          onClose={() => setSelectedCluster(null)}
          onUpdate={loadData}
        />
      )}

      {editingTrack && (
        <TrackEditModal
          trackId={editingTrack.id}
          trackTitle={editingTrack.title}
          currentClusterId={editingTrack.clusterId}
          onClose={() => setEditingTrack(null)}
          onUpdate={loadData}
        />
      )}

      {viewingTrack && (
        <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4">
          <div className="bg-dark-bg-elevated rounded-lg shadow-xl max-w-3xl w-full max-h-[80vh] overflow-auto">
            <div className="sticky top-0 bg-dark-bg-elevated border-b border-dark-border px-6 py-4 flex items-center justify-between">
              <h2 className="text-xl font-semibold text-dark-text">Track Details</h2>
              <button
                onClick={() => setViewingTrack(null)}
                className="text-dark-text-secondary hover:text-dark-text transition-colors"
              >
                ✕
              </button>
            </div>
            <div className="p-6 space-y-4">
              <div>
                <h3 className="text-sm font-medium text-dark-text-secondary uppercase mb-3">File Information</h3>
                <div className="space-y-2 text-sm">
                  <div className="grid grid-cols-[120px_1fr] gap-2">
                    <span className="text-dark-text-tertiary">Path:</span>
                    <span className="text-dark-text font-mono break-all">{viewingTrack.path}</span>
                  </div>
                  <div className="grid grid-cols-[120px_1fr] gap-2">
                    <span className="text-dark-text-tertiary">Title:</span>
                    <span className="text-dark-text">{viewingTrack.title || '—'}</span>
                  </div>
                  <div className="grid grid-cols-[120px_1fr] gap-2">
                    <span className="text-dark-text-tertiary">Artist:</span>
                    <span className="text-dark-text">{viewingTrack.artist || '—'}</span>
                  </div>
                  <div className="grid grid-cols-[120px_1fr] gap-2">
                    <span className="text-dark-text-tertiary">Album:</span>
                    <span className="text-dark-text">{viewingTrack.cluster_album || '—'}</span>
                  </div>
                  <div className="grid grid-cols-[120px_1fr] gap-2">
                    <span className="text-dark-text-tertiary">Track Number:</span>
                    <span className="text-dark-text">
                      {viewingTrack.disc_number && `${viewingTrack.disc_number}-`}{viewingTrack.track_number || '—'}
                    </span>
                  </div>
                  <div className="grid grid-cols-[120px_1fr] gap-2">
                    <span className="text-dark-text-tertiary">Duration:</span>
                    <span className="text-dark-text">
                      {viewingTrack.duration ? `${Math.floor(viewingTrack.duration / 60)}:${Math.floor(viewingTrack.duration % 60).toString().padStart(2, '0')}` : '—'}
                    </span>
                  </div>
                </div>
              </div>

              <div>
                <h3 className="text-sm font-medium text-dark-text-secondary uppercase mb-3">MusicBrainz Recording</h3>
                <div className="space-y-2 text-sm">
                  <div className="grid grid-cols-[120px_1fr] gap-2">
                    <span className="text-dark-text-tertiary">Recording ID:</span>
                    <span className="text-dark-text font-mono">{viewingTrack.mb_recording_id || '—'}</span>
                  </div>
                  <div className="grid grid-cols-[120px_1fr] gap-2">
                    <span className="text-dark-text-tertiary">Title:</span>
                    <span className="text-dark-text">{viewingTrack.mb_recording_title || '—'}</span>
                  </div>
                </div>
              </div>

              <div>
                <h3 className="text-sm font-medium text-dark-text-secondary uppercase mb-3">MusicBrainz Release</h3>
                <div className="space-y-2 text-sm">
                  <div className="grid grid-cols-[120px_1fr] gap-2">
                    <span className="text-dark-text-tertiary">Release ID:</span>
                    <span className="text-dark-text font-mono">{viewingTrack.mb_release_id || '—'}</span>
                  </div>
                  <div className="grid grid-cols-[120px_1fr] gap-2">
                    <span className="text-dark-text-tertiary">Title:</span>
                    <span className="text-dark-text">{viewingTrack.mb_release_title || '—'}</span>
                  </div>
                  <div className="grid grid-cols-[120px_1fr] gap-2">
                    <span className="text-dark-text-tertiary">Artist:</span>
                    <span className="text-dark-text">{viewingTrack.mb_release_artist || '—'}</span>
                  </div>
                  <div className="grid grid-cols-[120px_1fr] gap-2">
                    <span className="text-dark-text-tertiary">Year:</span>
                    <span className="text-dark-text">{viewingTrack.cluster_year || '—'}</span>
                  </div>
                  <div className="grid grid-cols-[120px_1fr] gap-2">
                    <span className="text-dark-text-tertiary">Confidence:</span>
                    <span className="text-dark-text">
                      {viewingTrack.mb_confidence !== null ? `${Math.round(viewingTrack.mb_confidence * 100)}%` : '—'}
                    </span>
                  </div>
                  <div className="grid grid-cols-[120px_1fr] gap-2">
                    <span className="text-dark-text-tertiary">Match Source:</span>
                    <span className="text-dark-text">{viewingTrack.match_source || '—'}</span>
                  </div>
                  <div className="grid grid-cols-[120px_1fr] gap-2">
                    <span className="text-dark-text-tertiary">Match Locked:</span>
                    <span className="text-dark-text">{viewingTrack.match_locked ? 'Yes' : 'No'}</span>
                  </div>
                </div>
              </div>

              <div>
                <h3 className="text-sm font-medium text-dark-text-secondary uppercase mb-3">Cluster</h3>
                <div className="space-y-2 text-sm">
                  <div className="grid grid-cols-[120px_1fr] gap-2">
                    <span className="text-dark-text-tertiary">Album:</span>
                    <span className="text-dark-text">{viewingTrack.cluster_album || '—'}</span>
                  </div>
                  <div className="grid grid-cols-[120px_1fr] gap-2">
                    <span className="text-dark-text-tertiary">Album Artist:</span>
                    <span className="text-dark-text">{viewingTrack.cluster_album_artist || '—'}</span>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}
