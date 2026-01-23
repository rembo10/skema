import { useState, useEffect, memo } from 'react';
import { Link } from 'react-router-dom';
import { Heart, Disc, Download, CheckCircle, AlertCircle } from 'lucide-react';
import { api } from '../lib/api';
import type { AlbumOverviewStats } from '../types/api';

interface WantedStats {
  total: number;
  wanted: number;
  searching: number;
  downloading: number;
  inLibrary: number;
  failed: number;
}

function WantedAlbumsSummaryComponent() {
  const [stats, setStats] = useState<WantedStats | null>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    loadStats();
  }, []);

  const loadStats = async () => {
    try {
      setLoading(true);
      const response = await api.getAlbumOverview({
        wanted: true,
        limit: 0, // Just get stats
      });

      // Process stats from response
      const statsByState = new Map(response.stats.by_state);

      const wantedStats: WantedStats = {
        total: response.pagination.total,
        wanted: (statsByState.get('Wanted') || 0) + (statsByState.get('Monitored') || 0),
        searching: statsByState.get('Searching') || 0,
        downloading: (statsByState.get('Downloading') || 0) + (statsByState.get('Upgrading') || 0),
        inLibrary: statsByState.get('InLibrary') || 0,
        failed: (statsByState.get('Failed') || 0) + (statsByState.get('IdentificationFailed') || 0),
      };

      setStats(wantedStats);
    } catch (error) {
      console.error('Failed to load wanted albums stats:', error);
    } finally {
      setLoading(false);
    }
  };

  if (loading) {
    return (
      <div className="card p-6">
        <div className="flex items-center gap-3 mb-5">
          <Heart className="w-5 h-5 text-dark-accent" />
          <h2 className="text-lg font-semibold text-dark-text">Wanted Albums</h2>
        </div>
        <div className="space-y-3">
          {[...Array(4)].map((_, i) => (
            <div key={i} className="animate-pulse flex justify-between">
              <div className="h-4 bg-dark-bg-subtle rounded w-1/3" />
              <div className="h-4 bg-dark-bg-subtle rounded w-12" />
            </div>
          ))}
        </div>
      </div>
    );
  }

  if (!stats || stats.total === 0) {
    return (
      <div className="card p-6">
        <div className="flex items-center gap-3 mb-5">
          <Heart className="w-5 h-5 text-dark-accent" />
          <h2 className="text-lg font-semibold text-dark-text">Wanted Albums</h2>
        </div>
        <div className="text-center py-8">
          <Disc className="mx-auto h-12 w-12 text-dark-text-tertiary mb-3" />
          <p className="text-sm text-dark-text-secondary">
            No wanted albums yet
          </p>
          <p className="text-xs text-dark-text-tertiary mt-1">
            Search and add albums to start monitoring
          </p>
        </div>
      </div>
    );
  }

  return (
    <div className="card p-6">
      <div className="flex items-start justify-between mb-5">
        <div>
          <div className="flex items-center gap-3">
            <Heart className="w-5 h-5 text-dark-accent" />
            <h2 className="text-lg font-semibold text-dark-text">Wanted Albums</h2>
          </div>
          <p className="mt-1 text-sm text-dark-text-secondary">
            Monitoring {stats.total} album{stats.total !== 1 ? 's' : ''}
          </p>
        </div>
        <Link to="/albums?state=Wanted,Failed,IdentificationFailed" className="link text-sm">View all →</Link>
      </div>

      <div className="space-y-3">
        {/* Wanted */}
        {stats.wanted > 0 && (
          <Link
            to="/albums?state=Wanted,Failed,IdentificationFailed"
            className="flex items-center justify-between p-3 rounded-lg hover:bg-dark-bg-hover transition-colors"
          >
            <div className="flex items-center gap-3">
              <div className="w-8 h-8 bg-dark-accent/10 rounded-lg flex items-center justify-center border border-dark-accent/30">
                <Heart className="w-4 h-4 text-dark-accent" />
              </div>
              <span className="text-sm text-dark-text">Wanted</span>
            </div>
            <span className="text-lg font-bold text-dark-accent tabular-nums">
              {stats.wanted}
            </span>
          </Link>
        )}

        {/* Searching */}
        {stats.searching > 0 && (
          <div className="flex items-center justify-between p-3 rounded-lg bg-dark-bg-subtle">
            <div className="flex items-center gap-3">
              <div className="w-8 h-8 bg-dark-info/10 rounded-lg flex items-center justify-center border border-dark-info/30">
                <Disc className="w-4 h-4 text-dark-info animate-spin" />
              </div>
              <span className="text-sm text-dark-text">Searching</span>
            </div>
            <span className="text-lg font-bold text-dark-info tabular-nums">
              {stats.searching}
            </span>
          </div>
        )}

        {/* Downloading */}
        {stats.downloading > 0 && (
          <Link
            to="/downloads"
            className="flex items-center justify-between p-3 rounded-lg hover:bg-dark-bg-hover transition-colors"
          >
            <div className="flex items-center gap-3">
              <div className="w-8 h-8 bg-dark-info/10 rounded-lg flex items-center justify-center border border-dark-info/30">
                <Download className="w-4 h-4 text-dark-info" />
              </div>
              <span className="text-sm text-dark-text">Downloading</span>
            </div>
            <span className="text-lg font-bold text-dark-info tabular-nums">
              {stats.downloading}
            </span>
          </Link>
        )}

        {/* In Library */}
        {stats.inLibrary > 0 && (
          <div className="flex items-center justify-between p-3 rounded-lg bg-dark-success/5">
            <div className="flex items-center gap-3">
              <div className="w-8 h-8 bg-dark-success/10 rounded-lg flex items-center justify-center border border-dark-success/30">
                <CheckCircle className="w-4 h-4 text-dark-success" />
              </div>
              <span className="text-sm text-dark-text">In Library</span>
            </div>
            <span className="text-lg font-bold text-dark-success tabular-nums">
              {stats.inLibrary}
            </span>
          </div>
        )}

        {/* Failed */}
        {stats.failed > 0 && (
          <div className="flex items-center justify-between p-3 rounded-lg bg-dark-error/5">
            <div className="flex items-center gap-3">
              <div className="w-8 h-8 bg-dark-error/10 rounded-lg flex items-center justify-center border border-dark-error/30">
                <AlertCircle className="w-4 h-4 text-dark-error" />
              </div>
              <span className="text-sm text-dark-text">Failed</span>
            </div>
            <span className="text-lg font-bold text-dark-error tabular-nums">
              {stats.failed}
            </span>
          </div>
        )}
      </div>
    </div>
  );
}

export const WantedAlbumsSummary = memo(WantedAlbumsSummaryComponent);
