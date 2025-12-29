import { useDraggable } from '@dnd-kit/core';
import { ClusterTrack } from '../types/api';
import { Music, GripVertical } from 'lucide-react';

interface TrackItemProps {
  track: ClusterTrack;
  clusterId: number;
}

export function TrackItem({ track, clusterId }: TrackItemProps) {
  const { attributes, listeners, setNodeRef, isDragging } = useDraggable({
    id: `track-${track.id}`,
    data: {
      type: 'track',
      track,
      clusterId,
    },
  });

  const formatDuration = (seconds: number | null) => {
    if (!seconds) return '--:--';
    const mins = Math.floor(seconds / 60);
    const secs = Math.floor(seconds % 60);
    return `${mins}:${secs.toString().padStart(2, '0')}`;
  };

  return (
    <div
      ref={setNodeRef}
      className={`flex items-center gap-3 p-3 bg-dark-bg-elevated rounded-lg border border-dark-border hover:bg-dark-bg-hover transition-colors ${
        isDragging ? 'opacity-50' : ''
      }`}
    >
      <button
        {...listeners}
        {...attributes}
        className="cursor-grab active:cursor-grabbing text-dark-text-tertiary hover:text-dark-text transition-colors p-1"
      >
        <GripVertical className="h-4 w-4" />
      </button>

      <Music className="h-4 w-4 text-dark-accent flex-shrink-0" />

      <div className="flex-1 min-w-0">
        <div className="text-sm font-medium text-dark-text truncate">
          {track.title || <span className="text-dark-text-tertiary italic">Unknown Title</span>}
        </div>
        {track.artist && (
          <div className="text-xs text-dark-text-secondary truncate">
            {track.artist}
          </div>
        )}
      </div>

      <div className="flex items-center gap-3 text-xs text-dark-text-tertiary flex-shrink-0">
        {track.track_number !== null && (
          <div className="font-mono">
            {track.disc_number ? `${track.disc_number}-` : ''}{track.track_number}
          </div>
        )}
        <div className="font-mono">
          {formatDuration(track.duration)}
        </div>
      </div>
    </div>
  );
}
