import { useState, useEffect, useMemo } from 'react';
import { Cluster } from '../types/api';
import { api } from '../lib/api';
import toast from 'react-hot-toast';
import { IdentificationNav } from '../components/IdentificationNav';
import { RematchModal } from '../components/identification/RematchModal';
import { PaginationControls } from '../components/PaginationControls';
import { usePagination } from '../hooks/usePagination';
import { TableRowSkeleton, StatsGridSkeleton } from '../components/LoadingSkeleton';
import { LoadingState } from '../components/LoadingState';
import {
  Loader2,
  Search,
  ArrowUpDown,
  CheckCircle2,
  AlertCircle,
  Lock,
  Zap,
  RefreshCw,
  Filter,
  Edit2,
} from 'lucide-react';

type SortField = 'album' | 'artist' | 'confidence' | 'status' | 'track_count';
type SortDirection = 'asc' | 'desc';
type FilterStatus = 'all' | 'matched' | 'unmatched' | 'locked';

const ITEMS_PER_PAGE = 50;

export default function Clusters() {
  // Local state - no longer using store
  const [clusters, setClusters] = useState<Cluster[]>([]);
  const [initialLoading, setInitialLoading] = useState(true);
  const [loading, setLoading] = useState(false);
  const [searchQuery, setSearchQuery] = useState('');
  const [debouncedSearchQuery, setDebouncedSearchQuery] = useState('');
  const [sortField, setSortField] = useState<SortField>('album');
  const [sortDirection, setSortDirection] = useState<SortDirection>('asc');
  const [filterStatus, setFilterStatus] = useState<FilterStatus>('all');
  const [rematchingCluster, setRematchingCluster] = useState<Cluster | null>(null);
  const pagination = usePagination(ITEMS_PER_PAGE);

  // Debounce search query
  useEffect(() => {
    const timeoutId = setTimeout(() => {
      setDebouncedSearchQuery(searchQuery);
    }, 300); // 300ms debounce

    return () => clearTimeout(timeoutId);
  }, [searchQuery]);

  // Reset offset when filters change
  useEffect(() => {
    pagination.resetOffset();
  }, [debouncedSearchQuery, filterStatus, sortField, sortDirection]);

  useEffect(() => {
    loadData();
  }, [pagination.offset, debouncedSearchQuery, filterStatus, sortField, sortDirection]);

  const loadData = async () => {
    try {
      setLoading(true);
      const response = await api.getClusters(
        pagination.offset,
        ITEMS_PER_PAGE,
        debouncedSearchQuery || undefined,
        filterStatus === 'all' ? undefined : filterStatus,
        sortField,
        sortDirection
      );
      setClusters(response.clusters);
      pagination.setTotalCount(response.pagination.total);
    } catch (error) {
      console.error('Failed to load clusters:', error);
      toast.error('Failed to load clusters');
    } finally {
      setLoading(false);
      setInitialLoading(false);
    }
  };

  const handleSort = (field: SortField) => {
    if (sortField === field) {
      setSortDirection(sortDirection === 'asc' ? 'desc' : 'asc');
    } else {
      setSortField(field);
      setSortDirection('asc');
    }
  };

  const getStatusIcon = (cluster: Cluster) => {
    if (!cluster.mb_release_id) {
      return <AlertCircle className="h-4 w-4 text-red-400" />;
    }
    if (cluster.match_locked) {
      return <Lock className="h-4 w-4 text-purple-400" />;
    }
    if (cluster.match_source === 'auto_fingerprint') {
      return <Zap className="h-4 w-4 text-blue-400" />;
    }
    return <CheckCircle2 className="h-4 w-4 text-green-400" />;
  };

  // Stats - showing total count from pagination response
  // Note: matched/unmatched/locked counts are based on current page only
  const stats = useMemo(() => {
    const total = pagination.totalCount;
    const matched = clusters.filter(c => c.mb_release_id).length;
    const unmatched = clusters.filter(c => !c.mb_release_id).length;
    const locked = clusters.filter(c => c.match_locked).length;
    return { total, matched, unmatched, locked };
  }, [clusters, pagination.totalCount]);

  return (
    <div className="h-full flex flex-col animate-fade-in">
      <IdentificationNav />

      {/* Header */}
      <div className="mb-6">
        <h1 className="text-3xl font-bold text-dark-text">Clusters</h1>
        <p className="text-dark-text-secondary mt-2">
          Manage album clusters and their MusicBrainz matches
        </p>
      </div>

      {/* Stats */}
      <LoadingState
        loading={initialLoading || loading}
        empty={false}
        skeleton={<StatsGridSkeleton columns={4} />}
        emptyState={null}
      >
        <div className="grid grid-cols-4 gap-4 mb-6">
          <div className="card p-4">
            <div className="text-2xl font-bold text-dark-text">{stats.total}</div>
            <div className="text-sm text-dark-text-secondary">Total Clusters</div>
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
      </LoadingState>

      {/* Controls */}
      <div className="flex gap-3 mb-4">
        {/* Search */}
        <div className="relative flex-1">
          <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-dark-text-tertiary" />
          <input
            type="text"
            placeholder="Search clusters..."
            value={searchQuery}
            onChange={(e) => setSearchQuery(e.target.value)}
            className="w-full pl-9 pr-4 py-2 bg-dark-bg-elevated border border-dark-border rounded-lg text-sm text-dark-text placeholder-dark-text-tertiary focus:outline-none focus:border-dark-accent"
          />
        </div>

        {/* Filter */}
        <div className="flex gap-2 bg-dark-bg-elevated rounded-lg p-1 border border-dark-border">
          <button
            onClick={() => setFilterStatus('all')}
            className={`px-3 py-1.5 rounded text-sm font-medium transition-colors ${
              filterStatus === 'all'
                ? 'bg-dark-accent text-dark-bg'
                : 'text-dark-text-secondary hover:text-dark-text'
            }`}
          >
            All
          </button>
          <button
            onClick={() => setFilterStatus('matched')}
            className={`px-3 py-1.5 rounded text-sm font-medium transition-colors ${
              filterStatus === 'matched'
                ? 'bg-dark-accent text-dark-bg'
                : 'text-dark-text-secondary hover:text-dark-text'
            }`}
          >
            Matched
          </button>
          <button
            onClick={() => setFilterStatus('unmatched')}
            className={`px-3 py-1.5 rounded text-sm font-medium transition-colors ${
              filterStatus === 'unmatched'
                ? 'bg-dark-accent text-dark-bg'
                : 'text-dark-text-secondary hover:text-dark-text'
            }`}
          >
            Unmatched
          </button>
          <button
            onClick={() => setFilterStatus('locked')}
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
      <div className="card overflow-auto relative">
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
                  Album
                  <ArrowUpDown className="h-3 w-3" />
                </button>
              </th>
              <th className="px-4 py-3 text-left">
                <button
                  onClick={() => handleSort('artist')}
                  className="flex items-center gap-2 text-xs font-medium text-dark-text-secondary hover:text-dark-text uppercase"
                >
                  Artist
                  <ArrowUpDown className="h-3 w-3" />
                </button>
              </th>
              <th className="px-4 py-3 text-left">
                <button
                  onClick={() => handleSort('track_count')}
                  className="flex items-center gap-2 text-xs font-medium text-dark-text-secondary hover:text-dark-text uppercase"
                >
                  Tracks
                  <ArrowUpDown className="h-3 w-3" />
                </button>
              </th>
              <th className="px-4 py-3 text-left text-xs font-medium text-dark-text-secondary uppercase">
                Local Metadata
              </th>
              <th className="px-4 py-3 text-left text-xs font-medium text-dark-text-secondary uppercase">
                MusicBrainz Match
              </th>
              <th className="px-4 py-3 text-left">
                <button
                  onClick={() => handleSort('confidence')}
                  className="flex items-center gap-2 text-xs font-medium text-dark-text-secondary hover:text-dark-text uppercase"
                >
                  Confidence
                  <ArrowUpDown className="h-3 w-3" />
                </button>
              </th>
              <th className="px-4 py-3 text-left text-xs font-medium text-dark-text-secondary uppercase">
                Actions
              </th>
            </tr>
          </thead>
          <tbody>
            <LoadingState
              loading={loading || initialLoading}
              empty={clusters.length === 0}
              skeleton={
                <>
                  {[...Array(10)].map((_, i) => (
                    <TableRowSkeleton key={i} columns={6} />
                  ))}
                </>
              }
              emptyState={
                <tr>
                  <td colSpan={6}>
                    <div className="p-12 text-center">
                      <Filter className="mx-auto h-12 w-12 text-dark-text-tertiary" />
                      <h3 className="mt-4 text-sm font-medium text-dark-text">No clusters found</h3>
                      <p className="mt-2 text-sm text-dark-text-secondary">
                        {searchQuery ? 'Try a different search query' : 'No clusters match the current filter'}
                      </p>
                    </div>
                  </td>
                </tr>
              }
            >
              {clusters.map((cluster, index) => (
                <tr
                  key={cluster.id}
                  className={`hover:bg-dark-bg-elevated transition-colors border-b border-dark-border ${
                    index % 2 === 0 ? 'bg-dark-bg' : 'bg-dark-bg/50'
                  }`}
                >
                <td className="px-4 py-3">
                  <div className="flex items-center gap-2">
                    {getStatusIcon(cluster)}
                  </div>
                </td>
                <td className="px-4 py-3">
                  <div className="text-sm font-medium text-dark-text">
                    {cluster.album || cluster.mb_release_title || <span className="italic text-dark-text-tertiary">Unknown</span>}
                  </div>
                  {cluster.mb_release_title && cluster.album !== cluster.mb_release_title && (
                    <div className="text-xs text-dark-text-tertiary">
                      MB: {cluster.mb_release_title}
                    </div>
                  )}
                </td>
                <td className="px-4 py-3 text-sm text-dark-text-secondary">
                  {cluster.album_artist || cluster.mb_release_artist || <span className="italic text-dark-text-tertiary">Unknown</span>}
                </td>
                <td className="px-4 py-3 text-sm font-mono text-dark-text">
                  {cluster.track_count}
                </td>
                {/* Local Metadata Column */}
                <td className="px-4 py-3">
                  <div className="text-xs space-y-0.5">
                    {cluster.date && (
                      <div className="text-dark-text-secondary">
                        <span className="text-dark-text-tertiary">Date:</span> {cluster.date}
                      </div>
                    )}
                    {cluster.country && (
                      <div className="text-dark-text-secondary">
                        <span className="text-dark-text-tertiary">Country:</span> {cluster.country}
                      </div>
                    )}
                    {cluster.label && (
                      <div className="text-dark-text-secondary">
                        <span className="text-dark-text-tertiary">Label:</span> {cluster.label}
                      </div>
                    )}
                    {cluster.catalog_number && (
                      <div className="text-dark-text-secondary">
                        <span className="text-dark-text-tertiary">Cat#:</span> {cluster.catalog_number}
                      </div>
                    )}
                    {cluster.barcode && (
                      <div className="text-dark-text-secondary">
                        <span className="text-dark-text-tertiary">Barcode:</span> {cluster.barcode}
                      </div>
                    )}
                    {!cluster.date && !cluster.country && !cluster.label && !cluster.catalog_number && !cluster.barcode && (
                      <span className="text-dark-text-tertiary italic">No metadata</span>
                    )}
                  </div>
                </td>
                {/* MusicBrainz Match Column */}
                <td className="px-4 py-3">
                  {cluster.mb_release_id ? (
                    <div className="text-xs space-y-0.5">
                      {cluster.mb_release_date && (
                        <div className={`text-dark-text-secondary ${cluster.date && cluster.date !== cluster.mb_release_date ? 'text-yellow-400' : ''}`}>
                          <span className="text-dark-text-tertiary">Date:</span> {cluster.mb_release_date}
                        </div>
                      )}
                      {cluster.mb_release_country && (
                        <div className={`text-dark-text-secondary ${cluster.country && cluster.country !== cluster.mb_release_country ? 'text-yellow-400' : ''}`}>
                          <span className="text-dark-text-tertiary">Country:</span> {cluster.mb_release_country}
                        </div>
                      )}
                      {cluster.mb_release_label && (
                        <div className={`text-dark-text-secondary ${cluster.label && cluster.label !== cluster.mb_release_label ? 'text-yellow-400' : ''}`}>
                          <span className="text-dark-text-tertiary">Label:</span> {cluster.mb_release_label}
                        </div>
                      )}
                      {cluster.mb_release_barcode && (
                        <div className={`text-dark-text-secondary ${cluster.barcode && cluster.barcode !== cluster.mb_release_barcode ? 'text-yellow-400' : ''}`}>
                          <span className="text-dark-text-tertiary">Barcode:</span> {cluster.mb_release_barcode}
                        </div>
                      )}
                      {!cluster.mb_release_date && !cluster.mb_release_country && !cluster.mb_release_label && !cluster.mb_release_barcode && (
                        <span className="text-dark-text-tertiary italic">No metadata</span>
                      )}
                    </div>
                  ) : (
                    <span className="text-dark-text-tertiary italic">Not matched</span>
                  )}
                </td>
                <td className="px-4 py-3">
                  {cluster.mb_confidence !== null ? (
                    <span className={`text-sm font-mono ${
                      cluster.mb_confidence >= 0.8 ? 'text-green-400' :
                      cluster.mb_confidence >= 0.6 ? 'text-blue-400' :
                      cluster.mb_confidence >= 0.35 ? 'text-yellow-400' :
                      'text-red-400'
                    }`}>
                      {Math.round(cluster.mb_confidence * 100)}%
                    </span>
                  ) : (
                    <span className="text-sm italic text-dark-text-tertiary">—</span>
                  )}
                </td>
                <td className="px-4 py-3">
                  <button
                    onClick={() => setRematchingCluster(cluster)}
                    className="text-dark-text-secondary hover:text-dark-accent transition-colors"
                    title="Re-match cluster"
                  >
                    <Edit2 className="h-4 w-4" />
                  </button>
                </td>
              </tr>
              ))}
            </LoadingState>
          </tbody>
        </table>
      </div>

      <PaginationControls
        offset={pagination.offset}
        limit={ITEMS_PER_PAGE}
        total={pagination.totalCount}
        onPrevPage={pagination.prevPage}
        onNextPage={pagination.nextPage}
        itemName="clusters"
      />

      {/* Rematch Modal */}
      {rematchingCluster && (
        <RematchModal
          cluster={rematchingCluster}
          onClose={() => setRematchingCluster(null)}
          onUpdate={loadData}
        />
      )}
    </div>
  );
}
