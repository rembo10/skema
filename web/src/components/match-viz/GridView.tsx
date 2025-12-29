import { DndContext, DragOverlay, closestCenter, PointerSensor, useSensor, useSensors, DragStartEvent, DragEndEvent } from '@dnd-kit/core';
import { useState } from 'react';
import { Cluster, ClusterTrack } from '../../types/api';
import { api } from '../../lib/api';
import toast from 'react-hot-toast';
import { ClusterCard } from '../ClusterCard';
import { Music } from 'lucide-react';

interface GridViewProps {
  clusters: Cluster[];
  onClusterUpdate: () => void;
  getMatchSourceBadge: (source: string | null, locked: boolean) => React.ReactNode;
  getConfidenceBadge: (confidence: number | null) => React.ReactNode;
}

export function GridView({ clusters, onClusterUpdate, getMatchSourceBadge, getConfidenceBadge }: GridViewProps) {
  const [activeTrack, setActiveTrack] = useState<ClusterTrack | null>(null);

  const sensors = useSensors(
    useSensor(PointerSensor, {
      activationConstraint: {
        distance: 8,
      },
    })
  );

  const handleDragStart = (event: DragStartEvent) => {
    const { active } = event;
    if (active.data.current?.type === 'track') {
      setActiveTrack(active.data.current.track);
    }
  };

  const handleDragEnd = async (event: DragEndEvent) => {
    const { active, over } = event;
    setActiveTrack(null);

    if (!over) return;

    const trackId = active.data.current?.track?.id;
    const targetClusterId = over.data.current?.clusterId;

    if (!trackId || !targetClusterId) return;

    // Don't do anything if dropped on same cluster
    const sourceClusterId = active.data.current?.clusterId;
    if (sourceClusterId === targetClusterId) return;

    try {
      // Move track to new cluster
      await api.updateTrack(trackId, { cluster_id: targetClusterId });
      toast.success('Track moved successfully');
      onClusterUpdate();
    } catch (error) {
      console.error('Failed to move track:', error);
      toast.error('Failed to move track');
    }
  };

  return (
    <DndContext
      sensors={sensors}
      collisionDetection={closestCenter}
      onDragStart={handleDragStart}
      onDragEnd={handleDragEnd}
    >
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-4">
        {clusters.map((cluster) => (
          <ClusterCard
            key={cluster.id}
            cluster={cluster}
            onClick={() => {
              // Could open a detail modal here
              console.log('Cluster clicked:', cluster.id);
            }}
            getMatchSourceBadge={getMatchSourceBadge}
            getConfidenceBadge={getConfidenceBadge}
          />
        ))}
      </div>

      <DragOverlay>
        {activeTrack ? (
          <div className="bg-dark-bg-elevated border-2 border-dark-accent rounded-lg p-3 shadow-2xl opacity-90">
            <div className="flex items-center gap-2">
              <Music className="h-4 w-4 text-dark-accent" />
              <div className="text-sm text-dark-text font-medium truncate max-w-xs">
                {activeTrack.title || 'Unknown Track'}
              </div>
            </div>
          </div>
        ) : null}
      </DragOverlay>
    </DndContext>
  );
}
