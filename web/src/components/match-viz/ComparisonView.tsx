import { Cluster } from '../../types/api';
import { GitCompare } from 'lucide-react';

interface ComparisonViewProps {
  clusters: Cluster[];
  onClusterUpdate: () => void;
}

export function ComparisonView({ clusters }: ComparisonViewProps) {
  // Filter to low-confidence or unmatched clusters
  const problematicClusters = clusters.filter(
    (c) => !c.mb_release_id || (c.mb_confidence !== null && c.mb_confidence < 0.6)
  );

  return (
    <div className="flex-1 flex items-center justify-center bg-dark-bg-elevated border border-dark-border rounded-lg">
      <div className="text-center p-12">
        <GitCompare className="h-16 w-16 text-dark-text-tertiary mx-auto mb-4 opacity-50" />
        <h3 className="text-lg font-medium text-dark-text mb-2">Comparison View</h3>
        <p className="text-sm text-dark-text-secondary max-w-md">
          Side-by-side comparison of cluster metadata vs MusicBrainz releases for low-confidence matches.
        </p>
        {problematicClusters.length > 0 && (
          <p className="text-sm text-dark-accent mt-3">
            {problematicClusters.length} {problematicClusters.length === 1 ? 'cluster needs' : 'clusters need'} review
          </p>
        )}
        <p className="text-xs text-dark-text-tertiary mt-4">Coming soon...</p>
      </div>
    </div>
  );
}
