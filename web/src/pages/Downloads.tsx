import { useEffect, useState, useRef } from 'react';
import { api } from '../lib/api';
import type { Download, DownloadStatus } from '../types/api';
import { Download as DownloadIcon, Trash2, Clock, CheckCircle2, XCircle, Archive, Loader2, AlertCircle, RefreshCw, ChevronLeft, ChevronRight } from 'lucide-react';
import toast from 'react-hot-toast';
import { useAppStore } from '../store';
import { DownloadCardSkeleton } from '../components/LoadingSkeleton';
import { LoadingState } from '../components/LoadingState';

const ITEMS_PER_PAGE = 50;

export default function Downloads() {
  // Local state - no longer using store for paginated data
  const [downloads, setDownloads] = useState<Download[]>([]);
  const [loading, setLoading] = useState(true);
  const [selectedTab, setSelectedTab] = useState<'active' | 'history'>('active');
  const [offset, setOffset] = useState(0);
  const [totalCount, setTotalCount] = useState(0);
  const connectionStatus = useAppStore((state) => state.connectionStatus);
  const prevConnectionStatus = useRef(connectionStatus);

  // Load downloads on mount and when offset changes
  useEffect(() => {
    loadDownloads();
  }, [offset]);

  // Reload downloads when SSE reconnects after disconnection
  useEffect(() => {
    if (prevConnectionStatus.current !== 'connected' && connectionStatus === 'connected') {
      console.log('[Downloads] SSE reconnected, reloading downloads');
      loadDownloads();
    }
    prevConnectionStatus.current = connectionStatus;
  }, [connectionStatus]);

  const loadDownloads = async () => {
    try {
      setLoading(true);
      const response = await api.getAllDownloads(offset, ITEMS_PER_PAGE);
      setDownloads(response.downloads);
      setTotalCount(response.pagination.total);
    } catch (error) {
      toast.error('Failed to load downloads');
      console.error('Error loading downloads:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async (download: Download) => {
    if (!confirm(`Delete ${download.title} from history?`)) return;

    try {
      await api.deleteDownload(download.id);
      toast.success('Download deleted');
      await loadDownloads();
    } catch (error) {
      toast.error('Failed to delete download');
      console.error('Error deleting download:', error);
    }
  };

  const handleReidentify = async (download: Download) => {
    try {
      await api.reidentifyDownload(download.id);
      toast.success(`Re-identifying ${download.title}...`);
      await loadDownloads();
    } catch (error) {
      toast.error('Failed to re-identify download');
      console.error('Error re-identifying download:', error);
    }
  };

  const formatBytes = (bytes: number | null) => {
    if (!bytes) return 'Unknown size';
    const gb = bytes / (1024 * 1024 * 1024);
    if (gb >= 1) return `${gb.toFixed(2)} GB`;
    const mb = bytes / (1024 * 1024);
    return `${mb.toFixed(2)} MB`;
  };

  const formatDate = (dateString: string | null) => {
    if (!dateString) return 'Unknown';
    try {
      return new Date(dateString).toLocaleString('en-US', {
        month: 'short',
        day: 'numeric',
        hour: '2-digit',
        minute: '2-digit',
      });
    } catch {
      return dateString;
    }
  };

  const getStatusIcon = (status: DownloadStatus) => {
    switch (status) {
      case 'queued':
        return <Clock className="w-5 h-5 text-dark-text-secondary" />;
      case 'downloading':
        return <DownloadIcon className="w-5 h-5 text-dark-accent animate-pulse" />;
      case 'completed':
        return <CheckCircle2 className="w-5 h-5 text-dark-success" />;
      case 'failed':
        return <XCircle className="w-5 h-5 text-dark-error" />;
      case 'imported':
        return <Archive className="w-5 h-5 text-dark-success" />;
      case 'cancelled':
        return <XCircle className="w-5 h-5 text-dark-text-secondary" />;
      case 'identification_failure':
        return <AlertCircle className="w-5 h-5 text-yellow-500" />;
      default:
        return <Clock className="w-5 h-5 text-dark-text-secondary" />;
    }
  };

  const getStatusText = (status: DownloadStatus) => {
    if (status === 'identification_failure') {
      return 'Identification Failed';
    }
    return status.charAt(0).toUpperCase() + status.slice(1);
  };

  const getStatusColor = (status: DownloadStatus) => {
    switch (status) {
      case 'queued':
        return 'text-dark-text-secondary';
      case 'downloading':
        return 'text-dark-accent';
      case 'completed':
        return 'text-dark-success';
      case 'failed':
        return 'text-dark-error';
      case 'imported':
        return 'text-dark-success';
      case 'cancelled':
        return 'text-dark-text-secondary';
      case 'identification_failure':
        return 'text-yellow-500';
      default:
        return 'text-dark-text-secondary';
    }
  };

  // Group downloads by status
  const activeDownloads = downloads.filter(d => d.status === 'queued' || d.status === 'downloading');
  const historyDownloads = downloads.filter(d => d.status !== 'queued' && d.status !== 'downloading');

  const currentDownloads = selectedTab === 'active' ? activeDownloads : historyDownloads;

  const DownloadItem = ({ download }: { download: Download }) => (
    <div className="bg-dark-bg-elevated rounded-lg p-4 border border-dark-border hover:border-dark-border-hover transition-all">
      <div className="flex items-start gap-4">
        {/* Status icon */}
        <div className="flex-shrink-0 mt-1">
          {getStatusIcon(download.status)}
        </div>

        {/* Main content */}
        <div className="flex-1 min-w-0">
          {/* Title and status */}
          <div className="flex items-start justify-between gap-4 mb-2">
            <div className="flex-1 min-w-0">
              <h3 className="font-medium text-dark-text truncate mb-1">
                {download.title}
              </h3>
              <div className="flex items-center gap-3 text-sm text-dark-text-secondary">
                <span className={`font-medium ${getStatusColor(download.status)}`}>
                  {getStatusText(download.status)}
                </span>
                {download.indexer_name && (
                  <>
                    <span>•</span>
                    <span>{download.indexer_name}</span>
                  </>
                )}
                {download.size_bytes && (
                  <>
                    <span>•</span>
                    <span>{formatBytes(download.size_bytes)}</span>
                  </>
                )}
              </div>
            </div>

            {/* Action buttons */}
            <div className="flex items-center gap-2">
              {download.status === 'identification_failure' && (
                <button
                  onClick={() => handleReidentify(download)}
                  className="p-2 rounded-lg hover:bg-dark-bg-hover text-yellow-500 hover:text-yellow-400 transition-colors"
                  title="Re-identify with MusicBrainz"
                >
                  <RefreshCw className="w-4 h-4" />
                </button>
              )}
              <button
                onClick={() => handleDelete(download)}
                className="p-2 rounded-lg hover:bg-dark-bg-hover text-dark-text-secondary hover:text-dark-error transition-colors"
                title="Delete download"
              >
                <Trash2 className="w-4 h-4" />
              </button>
            </div>
          </div>

          {/* Progress bar (for active downloads) */}
          {download.status === 'downloading' && (
            <div className="mb-2">
              <div className="flex items-center justify-between text-xs text-dark-text-secondary mb-1">
                <span>Progress</span>
                <span>{download.progress.toFixed(1)}%</span>
              </div>
              <div className="h-2 bg-dark-bg-hover rounded-full overflow-hidden">
                <div
                  className="h-full bg-dark-accent transition-all duration-300"
                  style={{ width: `${download.progress}%` }}
                />
              </div>
            </div>
          )}

          {/* Error message */}
          {download.error_message && (
            <div className="mt-2 p-2 bg-dark-error/10 border border-dark-error/20 rounded text-sm text-dark-error">
              {download.error_message}
            </div>
          )}

          {/* Library path (for imported downloads) */}
          {download.library_path && download.status === 'imported' && (
            <div className="mt-2 text-sm text-dark-text-secondary">
              <span className="opacity-75">Imported to:</span>{' '}
              <span className="font-mono">{download.library_path}</span>
            </div>
          )}

          {/* Metadata */}
          <div className="flex items-center gap-4 text-xs text-dark-text-secondary mt-2">
            {download.queued_at && (
              <span>Queued {formatDate(download.queued_at)}</span>
            )}
            {download.completed_at && (
              <span>Completed {formatDate(download.completed_at)}</span>
            )}
            {download.imported_at && (
              <span>Imported {formatDate(download.imported_at)}</span>
            )}
          </div>
        </div>
      </div>
    </div>
  );

  return (
    <div className="space-y-6 animate-fade-in">
      {/* Tabs */}
      <div className="flex gap-2 border-b border-dark-border mb-6">
        <button
          onClick={() => setSelectedTab('active')}
          className={`px-4 py-2 font-medium transition-all ${
            selectedTab === 'active'
              ? 'text-dark-accent border-b-2 border-dark-accent'
              : 'text-dark-text-secondary hover:text-dark-text'
          }`}
        >
          Active {activeDownloads.length > 0 && `(${activeDownloads.length})`}
        </button>
        <button
          onClick={() => setSelectedTab('history')}
          className={`px-4 py-2 font-medium transition-all ${
            selectedTab === 'history'
              ? 'text-dark-accent border-b-2 border-dark-accent'
              : 'text-dark-text-secondary hover:text-dark-text'
          }`}
        >
          History {historyDownloads.length > 0 && `(${historyDownloads.length})`}
        </button>
      </div>

      {/* Header */}
      <div>
        <h1 className="text-3xl font-bold text-dark-text">Downloads</h1>
        <p className="text-dark-text-secondary mt-2">
          Monitor and manage your downloads
        </p>
      </div>

      {/* Downloads list */}
      <div className="space-y-3">
        <LoadingState
          loading={loading}
          empty={currentDownloads.length === 0}
          skeleton={[...Array(5)].map((_, i) => (
            <DownloadCardSkeleton key={i} />
          ))}
          emptyState={
            <div className="text-center py-12">
              <DownloadIcon className="w-12 h-12 text-dark-text-secondary mx-auto mb-4 opacity-50" />
              <p className="text-dark-text-secondary">
                {selectedTab === 'active' ? 'No active downloads' : 'No download history'}
              </p>
            </div>
          }
        >
          {currentDownloads.map(download => (
            <DownloadItem key={download.id} download={download} />
          ))}
        </LoadingState>
      </div>

      {/* Pagination Controls */}
      {totalCount > ITEMS_PER_PAGE && (
        <div className="flex items-center justify-between border-t border-dark-border pt-4 mt-4">
          <div className="text-sm text-dark-text-secondary">
            Showing {offset + 1}-{Math.min(offset + ITEMS_PER_PAGE, totalCount)} of {totalCount}
          </div>
          <div className="flex gap-2">
            <button
              onClick={() => setOffset((o) => Math.max(0, o - ITEMS_PER_PAGE))}
              disabled={offset === 0}
              className="p-2 rounded-lg border border-dark-border hover:bg-dark-bg-subtle transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
              title="Previous page"
            >
              <ChevronLeft size={16} />
            </button>
            <button
              onClick={() => setOffset((o) => Math.min(totalCount - ITEMS_PER_PAGE, o + ITEMS_PER_PAGE))}
              disabled={offset + ITEMS_PER_PAGE >= totalCount}
              className="p-2 rounded-lg border border-dark-border hover:bg-dark-bg-subtle transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
              title="Next page"
            >
              <ChevronRight size={16} />
            </button>
          </div>
        </div>
      )}
    </div>
  );
}
