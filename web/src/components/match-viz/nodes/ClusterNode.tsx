import { memo } from 'react';
import { Handle, Position, NodeProps } from 'reactflow';
import { Cluster } from '../../../types/api';
import { Disc, Lock, Zap, Search, CheckCircle2, AlertCircle } from 'lucide-react';

interface ClusterNodeData {
  cluster: Cluster;
  color: string;
  onClusterUpdate: () => void;
}

export const ClusterNode = memo(({ data }: NodeProps<ClusterNodeData>) => {
  const { cluster, color } = data;

  const getMatchIcon = () => {
    if (!cluster.mb_release_id) {
      return <AlertCircle className="h-3 w-3 text-red-400" />;
    }
    if (cluster.match_locked) {
      return <Lock className="h-3 w-3 text-purple-400" />;
    }
    if (cluster.match_source === 'auto_fingerprint') {
      return <Zap className="h-3 w-3 text-blue-400" />;
    }
    if (cluster.match_source === 'auto_metadata') {
      return <Search className="h-3 w-3 text-green-400" />;
    }
    return <CheckCircle2 className="h-3 w-3 text-green-400" />;
  };

  const confidencePercent = cluster.mb_confidence ? Math.round(cluster.mb_confidence * 100) : 0;

  return (
    <div
      className="relative bg-dark-bg-elevated rounded-lg shadow-lg border-2 transition-all duration-200 hover:shadow-xl hover:scale-105"
      style={{
        borderColor: color,
        width: '200px',
        height: '120px',
      }}
    >
      <Handle type="target" position={Position.Top} className="w-2 h-2" />
      <Handle type="source" position={Position.Bottom} className="w-2 h-2" />

      <div className="p-3 h-full flex flex-col">
        {/* Header with icon */}
        <div className="flex items-start justify-between gap-2 mb-2">
          <div className="flex items-center gap-1.5">
            {getMatchIcon()}
            {cluster.mb_confidence !== null && cluster.mb_release_id && (
              <span className="text-xs font-mono text-dark-text-tertiary">
                {confidencePercent}%
              </span>
            )}
          </div>
          <Disc className="h-4 w-4 text-dark-text-tertiary opacity-50" />
        </div>

        {/* Album info */}
        <div className="flex-1 min-h-0">
          <div className="font-medium text-sm text-dark-text truncate">
            {cluster.album || cluster.mb_release_title || (
              <span className="italic text-dark-text-tertiary">Unknown Album</span>
            )}
          </div>
          <div className="text-xs text-dark-text-secondary truncate mt-0.5">
            {cluster.album_artist || cluster.mb_release_artist || (
              <span className="italic text-dark-text-tertiary">Unknown Artist</span>
            )}
          </div>
        </div>

        {/* Footer */}
        <div className="flex items-center justify-between text-xs text-dark-text-tertiary mt-2 pt-2 border-t border-dark-border">
          <span>{cluster.track_count} tracks</span>
          {cluster.year && <span>{cluster.year}</span>}
        </div>
      </div>

      {/* Glow effect for high confidence matches */}
      {cluster.mb_confidence && cluster.mb_confidence >= 0.9 && (
        <div
          className="absolute inset-0 rounded-lg pointer-events-none"
          style={{
            boxShadow: `0 0 20px ${color}40`,
          }}
        />
      )}
    </div>
  );
});

ClusterNode.displayName = 'ClusterNode';
