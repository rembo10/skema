import { useDroppable } from '@dnd-kit/core';
import { Cluster } from '../types/api';
import { Disc, ExternalLink } from 'lucide-react';

interface ClusterCardProps {
  cluster: Cluster;
  onClick: () => void;
  getMatchSourceBadge: (source: string | null, locked: boolean) => React.ReactNode;
  getConfidenceBadge: (confidence: number | null) => React.ReactNode;
}

export function ClusterCard({ cluster, onClick, getMatchSourceBadge, getConfidenceBadge }: ClusterCardProps) {
  const { isOver, setNodeRef } = useDroppable({
    id: `cluster-${cluster.id}`,
    data: {
      type: 'cluster',
      clusterId: cluster.id,
    },
  });

  return (
    <div
      ref={setNodeRef}
      onClick={onClick}
      className={`card p-4 cursor-pointer transition-all duration-200 hover:shadow-lg hover:border-dark-accent/50 ${
        isOver ? 'ring-2 ring-dark-accent bg-dark-accent/5 scale-105' : ''
      }`}
    >
      {/* Header */}
      <div className="flex items-start gap-3 mb-3">
        <div className="flex-shrink-0 p-2 bg-dark-bg-subtle rounded-lg">
          <Disc className="h-5 w-5 text-dark-accent" />
        </div>
        <div className="flex-1 min-w-0">
          <h3 className="font-medium text-dark-text truncate">
            {cluster.album || <span className="text-dark-text-tertiary italic">Unknown Album</span>}
          </h3>
          <p className="text-sm text-dark-text-secondary truncate">
            {cluster.album_artist || <span className="text-dark-text-tertiary italic">Unknown Artist</span>}
          </p>
        </div>
      </div>

      {/* Match info */}
      {cluster.mb_release_id ? (
        <div className="space-y-2 mb-3">
          <div className="text-xs text-dark-text-secondary truncate">
            <ExternalLink className="h-3 w-3 inline mr-1" />
            {cluster.mb_release_title || 'MusicBrainz Match'}
          </div>
          {cluster.mb_release_country && (
            <div className="text-xs text-dark-text-tertiary">
              {cluster.mb_release_country}
              {cluster.mb_release_date && ` • ${cluster.mb_release_date.substring(0, 4)}`}
            </div>
          )}
        </div>
      ) : (
        <div className="mb-3 text-xs text-dark-text-tertiary italic">
          Not matched
        </div>
      )}

      {/* Badges */}
      <div className="flex flex-wrap gap-2 items-center">
        {getMatchSourceBadge(cluster.match_source, cluster.match_locked)}
        {getConfidenceBadge(cluster.mb_confidence)}
        <div className="text-xs text-dark-text-tertiary">
          {cluster.track_count} {cluster.track_count === 1 ? 'track' : 'tracks'}
        </div>
      </div>
    </div>
  );
}
