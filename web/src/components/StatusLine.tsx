import { useAppStore } from '../store';
import { Loader2, CheckCircle2, XCircle } from 'lucide-react';

export function StatusLine() {
  const status = useAppStore((state) => state.currentStatus);
  const connectionStatus = useAppStore((state) => state.connectionStatus);

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
    <div className={`border-t ${getBgColor()} px-4 py-2.5 transition-all duration-300`}>
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

        {/* Right side - progress bar if active */}
        <div className="flex-shrink-0">
          {getProgressBar()}
        </div>
      </div>
    </div>
  );
}
