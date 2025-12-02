import { useState, useMemo } from 'react';
import { useAppStore } from '../store';
import { Loader2, CheckCircle2, XCircle, ChevronUp, ChevronDown, Download } from 'lucide-react';
import type { Download as DownloadType } from '../types/api';

function formatBytes(bytes: number | null): string {
  if (bytes === null || bytes === 0) return '0 B';
  const k = 1024;
  const sizes = ['B', 'KB', 'MB', 'GB'];
  const i = Math.floor(Math.log(bytes) / Math.log(k));
  return `${parseFloat((bytes / Math.pow(k, i)).toFixed(1))} ${sizes[i]}`;
}

function DownloadItem({ download }: { download: DownloadType }) {
  const getStatusColor = () => {
    switch (download.status) {
      case 'downloading':
        return 'text-dark-accent';
      case 'completed':
      case 'imported':
        return 'text-dark-success';
      case 'failed':
      case 'identification_failure':
      case 'cancelled':
        return 'text-dark-error';
      case 'queued':
      default:
        return 'text-dark-text-secondary';
    }
  };

  const getStatusIcon = () => {
    switch (download.status) {
      case 'downloading':
        return <Loader2 className="animate-spin" size={14} />;
      case 'completed':
      case 'imported':
        return <CheckCircle2 size={14} />;
      case 'failed':
      case 'identification_failure':
      case 'cancelled':
        return <XCircle size={14} />;
      case 'queued':
      default:
        return <Download size={14} />;
    }
  };

  const progress = download.progress || 0;

  return (
    <div className="py-2 px-3 hover:bg-dark-bg-hover rounded-lg transition-colors">
      <div className="flex items-center gap-3">
        <div className={`flex-shrink-0 ${getStatusColor()}`}>
          {getStatusIcon()}
        </div>
        <div className="flex-1 min-w-0">
          <p className="text-sm text-dark-text truncate" title={download.title}>
            {download.title}
          </p>
          <div className="flex items-center gap-2 mt-1">
            {download.status === 'downloading' && (
              <>
                <div className="flex-1 max-w-[200px] bg-dark-bg-subtle rounded-full h-1.5">
                  <div
                    className="bg-dark-accent h-1.5 rounded-full transition-all duration-300"
                    style={{ width: `${progress}%` }}
                  />
                </div>
                <span className="text-xs text-dark-text-secondary tabular-nums">
                  {Math.round(progress)}%
                </span>
              </>
            )}
            {download.size_bytes && (
              <span className="text-xs text-dark-text-tertiary">
                {formatBytes(download.size_bytes)}
              </span>
            )}
            {download.quality && (
              <span className="text-xs text-dark-text-tertiary">
                • {download.quality}
              </span>
            )}
            {download.status === 'failed' && download.error_message && (
              <span className="text-xs text-dark-error truncate" title={download.error_message}>
                {download.error_message}
              </span>
            )}
          </div>
        </div>
      </div>
    </div>
  );
}

export function StatusLine() {
  const [isExpanded, setIsExpanded] = useState(false);
  const status = useAppStore((state) => state.currentStatus);
  const connectionStatus = useAppStore((state) => state.connectionStatus);
  const downloads = useAppStore((state) => state.downloads);

  // Get active downloads (downloading or queued)
  const activeDownloads = useMemo(() => 
    downloads.filter(d => d.status === 'downloading' || d.status === 'queued'),
    [downloads]
  );

  // Get recent downloads (completed, failed, imported in last hour)
  const recentDownloads = useMemo(() => {
    const oneHourAgo = new Date(Date.now() - 60 * 60 * 1000);
    return downloads
      .filter(d => {
        if (d.status === 'downloading' || d.status === 'queued') return false;
        const completedAt = d.completed_at ? new Date(d.completed_at) : null;
        const importedAt = d.imported_at ? new Date(d.imported_at) : null;
        const relevantDate = importedAt || completedAt;
        return relevantDate && relevantDate > oneHourAgo;
      })
      .slice(0, 5);
  }, [downloads]);

  const hasDownloads = activeDownloads.length > 0 || recentDownloads.length > 0;

  const getConnectionIcon = () => {
    switch (connectionStatus) {
      case 'connected':
        return <div className="w-2 h-2 rounded-full bg-dark-success" />;
      case 'connecting':
        return <Loader2 className="animate-spin text-dark-accent" size={16} />;
      case 'disconnected':
        return <div className="w-2 h-2 rounded-full bg-dark-text-tertiary" />;
      case 'error':
        return <div className="w-2 h-2 rounded-full bg-dark-error" />;
    }
  };

  const getConnectionText = () => {
    switch (connectionStatus) {
      case 'connected':
        return 'Connected';
      case 'connecting':
        return 'Connecting...';
      case 'disconnected':
        return 'Disconnected';
      case 'error':
        return 'Connection error';
    }
  };

  const getStatusIcon = () => {
    if (!status) return null;

    switch (status.type) {
      case 'in_progress':
        return <Loader2 className="animate-spin text-dark-accent" size={16} />;
      case 'success':
        return <CheckCircle2 className="text-dark-success" size={16} />;
      case 'error':
        return <XCircle className="text-dark-error" size={16} />;
    }
  };

  const getProgressBar = () => {
    if (!status || status.type !== 'in_progress' || !status.progress) {
      return null;
    }

    const { current, total } = status.progress;
    const percent = total > 0 ? Math.round((current / total) * 100) : 0;

    return (
      <div className="flex items-center space-x-2">
        <div className="w-32 bg-dark-bg-subtle rounded-full h-1.5">
          <div
            className="bg-dark-accent h-1.5 rounded-full transition-all duration-300"
            style={{ width: `${percent}%` }}
          />
        </div>
        <span className="text-xs text-dark-text-secondary tabular-nums min-w-[4rem] text-right">
          {current}/{total}
        </span>
      </div>
    );
  };

  const getBgColor = () => {
    if (status) {
      switch (status.type) {
        case 'in_progress':
          return 'bg-dark-bg-elevated border-dark-accent/30';
        case 'success':
          return 'bg-dark-success-muted border-dark-success/30';
        case 'error':
          return 'bg-dark-error-muted border-dark-error/30';
      }
    }

    // Connection status colors when no operation is running
    switch (connectionStatus) {
      case 'connected':
        return 'bg-dark-bg-elevated border-dark-border';
      case 'connecting':
        return 'bg-dark-bg-elevated border-dark-accent/30';
      case 'error':
        return 'bg-dark-error-muted border-dark-error/30';
      default:
        return 'bg-dark-bg-elevated border-dark-border';
    }
  };

  return (
    <div className={`border-t ${getBgColor()} transition-all duration-300`}>
      {/* Expanded downloads panel */}
      {isExpanded && hasDownloads && (
        <div className="border-b border-dark-border bg-dark-bg-elevated max-h-80 overflow-y-auto">
          <div className="max-w-7xl mx-auto px-4 py-3">
            {activeDownloads.length > 0 && (
              <div className="mb-3">
                <h4 className="text-xs font-medium text-dark-text-secondary uppercase tracking-wider mb-2 flex items-center gap-2">
                  <Download size={12} />
                  Active Downloads ({activeDownloads.length})
                </h4>
                <div className="space-y-1">
                  {activeDownloads.map(download => (
                    <DownloadItem key={download.id} download={download} />
                  ))}
                </div>
              </div>
            )}
            
            {recentDownloads.length > 0 && (
              <div>
                <h4 className="text-xs font-medium text-dark-text-secondary uppercase tracking-wider mb-2 flex items-center gap-2">
                  <CheckCircle2 size={12} />
                  Recent ({recentDownloads.length})
                </h4>
                <div className="space-y-1">
                  {recentDownloads.map(download => (
                    <DownloadItem key={download.id} download={download} />
                  ))}
                </div>
              </div>
            )}
          </div>
        </div>
      )}

      {/* Main status bar */}
      <div className="px-4 py-2.5">
        <div className="max-w-7xl mx-auto flex items-center justify-between gap-4">
          {/* Left side - operation status or connection status */}
          <div className="flex items-center gap-3 flex-1 min-w-0">
            <div className="flex-shrink-0">
              {status ? getStatusIcon() : getConnectionIcon()}
            </div>
            <p className="text-sm font-medium text-dark-text truncate">
              {status ? status.message : getConnectionText()}
            </p>
          </div>

          {/* Center - progress bar if active */}
          <div className="flex-shrink-0">
            {getProgressBar()}
          </div>

          {/* Right side - expand button if there are downloads */}
          {hasDownloads && (
            <button
              onClick={() => setIsExpanded(!isExpanded)}
              className="flex items-center gap-2 px-3 py-1 rounded-lg text-dark-text-secondary hover:text-dark-text hover:bg-dark-bg-hover transition-colors"
              aria-label={isExpanded ? 'Collapse downloads' : 'Expand downloads'}
              aria-expanded={isExpanded}
            >
              <Download size={14} />
              <span className="text-xs font-medium">
                {activeDownloads.length > 0 ? activeDownloads.length : recentDownloads.length}
              </span>
              {isExpanded ? <ChevronDown size={14} /> : <ChevronUp size={14} />}
            </button>
          )}
        </div>
      </div>
    </div>
  );
}
