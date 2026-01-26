import { useEffect, useState, useMemo, useCallback } from 'react';
import toast from 'react-hot-toast';
import { Link } from 'react-router-dom';
import { FileAudio, HardDrive, Clock, Disc, Mic, AlertTriangle, RefreshCw, RotateCcw } from 'lucide-react';
import { api } from '../lib/api';
import { useAppStore } from '../store';
import type { LibraryStats } from '../types/api';
import { RecentlyFollowedArtists } from '../components/RecentlyFollowedArtists';
import { RecentlyReleasedAlbums } from '../components/RecentlyReleasedAlbums';
import { DashboardSkeleton } from '../components/LoadingSkeleton';
import { UpcomingAlbums } from '../components/UpcomingAlbums';
import { WantedAlbumsSummary } from '../components/WantedAlbumsSummary';

// Format bytes to human-readable string
const formatBytes = (bytes: number): string => {
  if (bytes === 0) return '0 B';
  const k = 1024;
  const sizes = ['B', 'KB', 'MB', 'GB', 'TB'];
  const i = Math.floor(Math.log(bytes) / Math.log(k));
  return Math.round((bytes / Math.pow(k, i)) * 100) / 100 + ' ' + sizes[i];
};

// Format duration in seconds to human-readable string
const formatDuration = (seconds: number): string => {
  if (seconds === 0) return '0s';

  const days = Math.floor(seconds / 86400);
  const hours = Math.floor((seconds % 86400) / 3600);
  const minutes = Math.floor((seconds % 3600) / 60);
  const secs = Math.floor(seconds % 60);

  const parts: string[] = [];
  if (days > 0) parts.push(`${days}d`);
  if (hours > 0) parts.push(`${hours}h`);
  if (minutes > 0) parts.push(`${minutes}m`);
  if (secs > 0 && days === 0 && hours === 0) parts.push(`${secs}s`); // Only show seconds if less than an hour

  return parts.join(' ') || '0s';
};

export default function Dashboard() {
  // Read state from Zustand
  const stats = useAppStore((state) => state.stats);
  const followedArtists = useAppStore((state) => state.followedArtists);

  const [loading, setLoading] = useState(true);
  const [scanning, setScanning] = useState(false);
  const [forceScanning, setForceScanning] = useState(false);

  // Load stats and artists on mount
  useEffect(() => {
    async function loadData() {
      try {
        // Load stats and artists in parallel
        const [statsData, artistsResponse] = await Promise.all([
          api.getStats(),
          followedArtists.length === 0 ? api.getCatalogArtists(0, 1000, true) : Promise.resolve({ artists: [], pagination: { total: 0, offset: 0, limit: 0 } })
        ]);

        useAppStore.getState().setStats(statsData);

        // Only set artists if we loaded them (not already in store)
        if (artistsResponse.artists.length > 0) {
          useAppStore.getState().setFollowedArtists(artistsResponse.artists);
        }

        setLoading(false);
      } catch (error: any) {
        // Don't log 401 errors - they're expected when auth is required
        if (!error.isAuthError) {
          console.error('Failed to load data:', error);
        }
        setLoading(false);
        // 401 will trigger redirect via global unauthorized event handler
      }
    }
    loadData();
  }, []); // eslint-disable-line react-hooks/exhaustive-deps

  const startScan = useCallback(async () => {
    setScanning(true);
    try {
      const result = await api.scanLibrary();
      if (result.success) {
        toast.success('Library scan started');
      } else {
        toast.error(result.message || 'Failed to start library scan');
      }
    } catch (error) {
      console.error('Failed to start scan:', error);
      const errorMessage = error instanceof Error ? error.message : 'Failed to start library scan';
      toast.error(errorMessage);
    } finally {
      setScanning(false);
    }
  }, []);

  const startForceScan = useCallback(async () => {
    setForceScanning(true);
    try {
      const result = await api.forceScanLibrary();
      if (result.success) {
        toast.success('Force library re-scan started');
      } else {
        toast.error(result.message || 'Failed to start force re-scan');
      }
    } catch (error) {
      console.error('Failed to start force scan:', error);
      const errorMessage = error instanceof Error ? error.message : 'Failed to start force re-scan';
      toast.error(errorMessage);
    } finally {
      setForceScanning(false);
    }
  }, []);

  // Memoize formatted values to avoid recalculation on every render
  const formattedStats = useMemo(() => {
    if (!stats) return null;

    return {
      librarySize: formatBytes(stats.library_size),
      totalRuntime: formatDuration(stats.total_runtime),
      totalFiles: stats.total_files.toLocaleString(),
      totalAlbums: stats.total_albums.toLocaleString(),
      totalArtists: stats.total_artists.toLocaleString(),
      totalDiffs: stats.total_diffs.toLocaleString(),
      metadataAccuracy: stats.metadata_accuracy.toFixed(1),
      matchedFiles: stats.matched_files.toLocaleString(),
    };
  }, [stats]);

  if (loading) {
    return <DashboardSkeleton />;
  }

  return (
    <div className="space-y-6 animate-fade-in">
      {/* Header */}
      <div className="flex items-start justify-between">
        <div>
          <h1 className="text-3xl font-bold text-dark-text">Overview</h1>
          <p className="mt-2 text-dark-text-secondary">
            {stats?.library_path ? (
              <span className="font-mono text-sm">{stats.library_path}</span>
            ) : (
              'Configure your library path in settings'
            )}
          </p>
        </div>
        <button
          onClick={startScan}
          disabled={!stats?.library_path || scanning}
          className="btn-primary disabled:opacity-50 disabled:cursor-not-allowed"
          title={!stats?.library_path ? 'Configure library path in settings first' : ''}
        >
          {scanning ? 'Scanning...' : 'Scan Library'}
        </button>
      </div>

      {/* Primary stats grid */}
      {formattedStats && (
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div className="card-hover p-6">
            <div className="flex items-center justify-between">
              <div className="flex items-center gap-3">
                <div className="w-12 h-12 bg-dark-bg-subtle rounded-lg flex items-center justify-center border border-dark-border-bright">
                  <FileAudio className="w-6 h-6 text-dark-accent" />
                </div>
                <div>
                  <p className="text-sm text-dark-text-secondary">Files</p>
                  <p className="text-3xl font-bold text-dark-text tabular-nums">{formattedStats.totalFiles}</p>
                </div>
              </div>
            </div>
          </div>
          <div className="card-hover p-6">
            <div className="flex items-center justify-between">
              <div className="flex items-center gap-3">
                <div className="w-12 h-12 bg-dark-bg-subtle rounded-lg flex items-center justify-center border border-dark-border-bright">
                  <HardDrive className="w-6 h-6 text-dark-info" />
                </div>
                <div>
                  <p className="text-sm text-dark-text-secondary">Storage</p>
                  <p className="text-3xl font-bold text-dark-text tabular-nums">{formattedStats.librarySize}</p>
                </div>
              </div>
            </div>
          </div>
          <div className="card-hover p-6">
            <div className="flex items-center justify-between">
              <div className="flex items-center gap-3">
                <div className="w-12 h-12 bg-dark-bg-subtle rounded-lg flex items-center justify-center border border-dark-border-bright">
                  <Clock className="w-6 h-6 text-dark-success" />
                </div>
                <div>
                  <p className="text-sm text-dark-text-secondary">Runtime</p>
                  <p className="text-3xl font-bold text-dark-text tabular-nums">{formattedStats.totalRuntime}</p>
                </div>
              </div>
            </div>
          </div>
        </div>
      )}

      {/* Secondary stats */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        <StatCard
          title="Albums"
          value={formattedStats?.totalAlbums ?? '0'}
          icon={<Disc className="w-5 h-5" />}
        />
        <StatCard
          title="Artists"
          value={formattedStats?.totalArtists ?? '0'}
          icon={<Mic className="w-5 h-5" />}
        />
        <StatCard
          title="Metadata Diffs"
          value={formattedStats?.totalDiffs ?? '0'}
          icon={<AlertTriangle className="w-5 h-5" />}
          to="/library/diffs"
          accent={stats !== null && stats.total_diffs > 0}
        />
      </div>

      {/* Metadata accuracy */}
      <div className="card p-6">
        <div className="flex items-start justify-between mb-4">
          <h2 className="text-lg font-semibold text-dark-text">Metadata Accuracy</h2>
          {stats && stats.total_diffs > 0 && (
            <Link to="/library/diffs" className="link text-sm">View diffs →</Link>
          )}
        </div>
        <div className="flex items-center">
          <div className="flex-1">
            <div className="flex items-baseline gap-3">
              <span className="text-5xl font-bold text-dark-text tabular-nums">
                {formattedStats?.metadataAccuracy ?? '0.0'}%
              </span>
              <span className="text-sm text-dark-text-secondary">
                {formattedStats?.matchedFiles ?? '0'} / {formattedStats?.totalFiles ?? '0'} matched
              </span>
            </div>
            <div className="mt-6 w-full bg-dark-bg-subtle rounded-full h-2 overflow-hidden">
              <div
                className={`h-2 rounded-full transition-all duration-500 ${
                  (stats?.metadata_accuracy ?? 0) >= 80
                    ? 'bg-dark-success'
                    : (stats?.metadata_accuracy ?? 0) >= 50
                    ? 'bg-dark-accent'
                    : 'bg-dark-error'
                }`}
                style={{ width: `${stats?.metadata_accuracy ?? 0}%` }}
              />
            </div>
          </div>
        </div>
      </div>

      {/* Recently followed artists */}
      <RecentlyFollowedArtists />

      {/* Wanted Albums and Upcoming Albums */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <WantedAlbumsSummary />
        <UpcomingAlbums />
      </div>

      {/* Recently Released Albums */}
      <RecentlyReleasedAlbums />

      {/* Actions */}
      <div className="card p-6">
        <h2 className="text-lg font-semibold text-dark-text mb-4">Actions</h2>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <button
            onClick={startScan}
            disabled={!stats?.library_path || scanning}
            className="flex items-center gap-3 p-4 rounded-lg border border-dark-border bg-dark-bg-subtle hover:bg-dark-bg-elevated hover:border-dark-border-bright transition-all disabled:opacity-50 disabled:cursor-not-allowed text-left"
            title={!stats?.library_path ? 'Configure library path in settings first' : ''}
          >
            <div className="w-10 h-10 bg-dark-accent/10 rounded-lg flex items-center justify-center border border-dark-accent/30 flex-shrink-0">
              <RefreshCw className={`w-5 h-5 text-dark-accent ${scanning ? 'animate-spin' : ''}`} />
            </div>
            <div className="flex-1 min-w-0">
              <p className="font-medium text-dark-text">Scan Library</p>
              <p className="text-sm text-dark-text-secondary">Detect new, modified, and deleted files</p>
            </div>
          </button>

          <button
            onClick={startForceScan}
            disabled={!stats?.library_path || forceScanning}
            className="flex items-center gap-3 p-4 rounded-lg border border-dark-border bg-dark-bg-subtle hover:bg-dark-bg-elevated hover:border-dark-border-bright transition-all disabled:opacity-50 disabled:cursor-not-allowed text-left"
            title={!stats?.library_path ? 'Configure library path in settings first' : ''}
          >
            <div className="w-10 h-10 bg-dark-info/10 rounded-lg flex items-center justify-center border border-dark-info/30 flex-shrink-0">
              <RotateCcw className={`w-5 h-5 text-dark-info ${forceScanning ? 'animate-spin' : ''}`} />
            </div>
            <div className="flex-1 min-w-0">
              <p className="font-medium text-dark-text">Force Re-scan</p>
              <p className="text-sm text-dark-text-secondary">Re-read metadata from all files</p>
            </div>
          </button>
        </div>
      </div>
    </div>
  );
}

function StatCard({ title, value, icon, to, accent }: { title: string; value: string; icon: React.ReactNode; to?: string; accent?: boolean }) {
  const content = (
    <div className="flex items-center justify-between">
      <div className="flex-1">
        <p className="text-sm text-dark-text-secondary mb-1">{title}</p>
        <p className={`text-2xl font-bold tabular-nums ${accent ? 'text-dark-accent' : 'text-dark-text'}`}>{value}</p>
      </div>
      <div className={`w-10 h-10 rounded-lg flex items-center justify-center border ${accent ? 'border-dark-accent/30 bg-dark-accent/10 text-dark-accent' : 'border-dark-border-bright text-dark-text-secondary'}`}>
        {icon}
      </div>
    </div>
  );

  if (to) {
    return (
      <Link to={to} className={`card-hover p-5 ${accent ? 'glow-hover' : ''}`}>
        {content}
      </Link>
    );
  }

  return (
    <div className="card p-5">
      {content}
    </div>
  );
}
