import { useState, useEffect } from 'react';
import { Cluster, ClusterTrack, CandidateRelease, MBTrackInfo } from '../../types/api';
import { api } from '../../lib/api';
import { useAppStore } from '../../store';
import toast from 'react-hot-toast';
import {
  X,
  Search as SearchIcon,
  Loader2,
  RefreshCw,
  CheckCircle2,
  Edit2,
  Save,
  XCircle,
  AlertTriangle,
  GripVertical,
} from 'lucide-react';
import {
  DndContext,
  closestCenter,
  KeyboardSensor,
  PointerSensor,
  useSensor,
  useSensors,
  DragEndEvent,
} from '@dnd-kit/core';
import {
  arrayMove,
  SortableContext,
  sortableKeyboardCoordinates,
  useSortable,
  verticalListSortingStrategy,
} from '@dnd-kit/sortable';
import { CSS } from '@dnd-kit/utilities';

interface RematchModalProps {
  cluster: Cluster;
  onClose: () => void;
  onUpdate: () => void;
}

// Sortable track item component
function SortableTrackItem({ track }: { track: ClusterTrack }) {
  const {
    attributes,
    listeners,
    setNodeRef,
    transform,
    transition,
    isDragging,
  } = useSortable({ id: track.id });

  const style = {
    transform: CSS.Transform.toString(transform),
    transition,
    opacity: isDragging ? 0.5 : 1,
  };

  return (
    <div
      ref={setNodeRef}
      style={style}
      {...attributes}
      {...listeners}
      className="card p-3 bg-dark-bg h-[76px] flex items-center gap-3 cursor-grab active:cursor-grabbing"
    >
      <GripVertical className="h-4 w-4 text-dark-text-tertiary flex-shrink-0" />
      <div className="flex-1 min-w-0">
        <div className="flex items-center gap-2">
          {track.disc_number && track.track_number && (
            <span className="text-xs font-mono text-dark-text-tertiary">
              {track.disc_number}-{track.track_number}
            </span>
          )}
          {!track.disc_number && track.track_number && (
            <span className="text-xs font-mono text-dark-text-tertiary">
              {track.track_number}
            </span>
          )}
          <span className="text-sm text-dark-text truncate">
            {track.title || 'Unknown'}
          </span>
        </div>
        {track.artist && (
          <div className="text-xs text-dark-text-secondary truncate mt-0.5">
            {track.artist}
          </div>
        )}
        {track.duration && (
          <div className="text-xs text-dark-text-tertiary mt-0.5">
            {Math.floor(track.duration / 60)}:{String(Math.floor(track.duration % 60)).padStart(2, '0')}
          </div>
        )}
      </div>
    </div>
  );
}

// Sortable empty slot component
function SortableEmptySlot({ index }: { index: number }) {
  const {
    attributes,
    listeners,
    setNodeRef,
    transform,
    transition,
    isDragging,
  } = useSortable({ id: `empty-${index}` });

  const style = {
    transform: CSS.Transform.toString(transform),
    transition,
    opacity: isDragging ? 0.5 : 1,
  };

  return (
    <div
      ref={setNodeRef}
      style={style}
      {...attributes}
      {...listeners}
      className="card p-3 bg-dark-bg-elevated/30 border border-dashed border-dark-border h-[76px] flex items-center justify-center cursor-grab active:cursor-grabbing"
    >
      <span className="text-xs text-dark-text-tertiary italic">Empty slot</span>
    </div>
  );
}

// MusicBrainz track item component (non-draggable)
function MBTrackItem({ track }: { track: MBTrackInfo }) {
  return (
    <div className="card p-3 bg-dark-bg-elevated border border-dark-border h-[76px] flex items-center">
      <div className="flex-1 min-w-0">
        <div className="flex items-center gap-2">
          <span className="text-xs font-mono text-dark-text-tertiary">
            {track.disc_number}-{track.position}
          </span>
          <span className="text-sm text-dark-text truncate">
            {track.title}
          </span>
        </div>
        {track.artist && (
          <div className="text-xs text-dark-text-secondary truncate mt-0.5">
            {track.artist}
          </div>
        )}
        {track.length && (
          <div className="text-xs text-dark-text-tertiary mt-0.5">
            {Math.floor(track.length / 60000)}:{String(Math.floor((track.length % 60000) / 1000)).padStart(2, '0')}
          </div>
        )}
      </div>
    </div>
  );
}

export function RematchModal({ cluster, onClose, onUpdate }: RematchModalProps) {
  // Get cached data from store
  const cachedTracks = useAppStore((state) => state.clusterTracks[cluster.id]);
  const cachedCandidates = useAppStore((state) => state.clusterCandidates[cluster.id]);
  const setClusterTracks = useAppStore((state) => state.setClusterTracks);
  const setClusterCandidates = useAppStore((state) => state.setClusterCandidates);

  const [loading, setLoading] = useState(false);
  const [refreshing, setRefreshing] = useState(false);
  const [searchQuery, setSearchQuery] = useState('');
  const [searching, setSearching] = useState(false);
  const [searchResults, setSearchResults] = useState<CandidateRelease[]>([]);
  const [editingTrackId, setEditingTrackId] = useState<number | null>(null);
  const [newRecordingId, setNewRecordingId] = useState('');
  const [mbTracks, setMBTracks] = useState<MBTrackInfo[] | null>(null);
  const [orderedTracks, setOrderedTracks] = useState<(ClusterTrack | null)[]>([]);

  // Use cached data or fetch if not available
  const tracks = cachedTracks || [];
  const candidates = cachedCandidates || [];

  // Initialize ordered tracks aligned with MB tracks based on recording_id matches
  useEffect(() => {
    if (tracks.length > 0 && mbTracks && mbTracks.length > 0) {
      // Create a complete array (not sparse) where each track is positioned according to its MB match
      // Use null for empty slots
      const aligned: (ClusterTrack | null)[] = new Array(mbTracks.length).fill(null);
      const unmatchedTracks: ClusterTrack[] = [];

      // First pass: place tracks in their matched positions
      tracks.forEach(track => {
        const matchIndex = mbTracks.findIndex(mb => mb.recording_id === track.mb_recording_id);
        if (matchIndex >= 0) {
          aligned[matchIndex] = track;
        } else {
          unmatchedTracks.push(track);
        }
      });

      // Second pass: fill empty slots with unmatched tracks
      let unmatchedIndex = 0;
      for (let i = 0; i < mbTracks.length; i++) {
        if (!aligned[i] && unmatchedIndex < unmatchedTracks.length) {
          aligned[i] = unmatchedTracks[unmatchedIndex++];
        }
      }

      setOrderedTracks(aligned as (ClusterTrack | null)[]);
    } else if (tracks.length > 0) {
      setOrderedTracks([...tracks]);
    }
  }, [tracks, mbTracks]);

  useEffect(() => {
    // Only load if we don't have cached data
    if (!cachedTracks || !cachedCandidates) {
      loadData();
    }
  }, [cluster.id, cachedTracks, cachedCandidates]);

  // Auto-save track mappings when order changes
  useEffect(() => {
    // Skip initial load or when no MB tracks
    if (!mbTracks || mbTracks.length === 0 || orderedTracks.length === 0 || !tracks.length) {
      return;
    }

    // Check if order has changed from initial alignment
    const hasChanges = orderedTracks.some((track, index) => {
      if (!track || !mbTracks[index]) return false;
      // Check if this track's recording ID differs from the MB track at this position
      return track.mb_recording_id !== mbTracks[index].recording_id;
    });

    if (!hasChanges) {
      return;
    }

    // Save updated mappings
    const saveUpdates = async () => {
      const updatedTracks = [...tracks];

      for (let i = 0; i < orderedTracks.length; i++) {
        const localTrack = orderedTracks[i];
        const mbTrack = mbTracks[i];

        if (localTrack && mbTrack) {
          const expectedRecordingId = mbTrack.recording_id;

          // Only update if the mapping changed
          if (localTrack.mb_recording_id !== expectedRecordingId) {
            try {
              await api.updateTrackRecording(
                cluster.id,
                localTrack.id,
                expectedRecordingId,
                mbTrack.title
              );

              // Update local state
              localTrack.mb_recording_id = expectedRecordingId;
              localTrack.mb_recording_title = mbTrack.title;

              // Update the track in the cache
              const trackIndex = updatedTracks.findIndex(t => t.id === localTrack.id);
              if (trackIndex >= 0) {
                updatedTracks[trackIndex] = { ...localTrack };
              }
            } catch (error) {
              console.error(`Failed to update track ${localTrack.id}:`, error);
              toast.error(`Failed to update ${localTrack.title || 'track'}`);
            }
          }
        }
      }

      // Update cache without triggering full refresh
      setClusterTracks(cluster.id, updatedTracks);
    };

    saveUpdates();
  }, [orderedTracks, mbTracks, cluster.id, tracks, setClusterTracks]);

  const loadData = async () => {
    try {
      setLoading(true);
      const [clusterData, candidateData] = await Promise.all([
        api.getClusterWithTracks(cluster.id),
        api.getCandidates(cluster.id),
      ]);
      // Update store cache
      setClusterTracks(cluster.id, clusterData.tracks);
      setClusterCandidates(cluster.id, candidateData);
      // Set MB tracks
      setMBTracks(clusterData.mb_tracks);
    } catch (error) {
      console.error('Failed to load data:', error);
      toast.error('Failed to load cluster data');
    } finally {
      setLoading(false);
    }
  };

  const handleRefreshCandidates = async () => {
    try {
      setRefreshing(true);
      // Trigger targeted re-identification for this cluster
      await api.reidentifyCluster(cluster.id);
      toast.success('Re-identification complete');
      // Store will be updated via SSE event, just reload local state
      await loadData();
    } catch (error) {
      console.error('Failed to refresh candidates:', error);
      toast.error('Failed to refresh candidates');
    } finally {
      setRefreshing(false);
    }
  };

  const handleSearch = async () => {
    if (!searchQuery.trim()) return;

    try {
      setSearching(true);
      // Search for releases directly (not release groups)
      const candidates = await api.searchReleases(searchQuery, 10);
      setSearchResults(candidates);
    } catch (error) {
      console.error('Failed to search:', error);
      toast.error('Failed to search MusicBrainz');
    } finally {
      setSearching(false);
    }
  };

  const handleAssignRelease = async (releaseId: string, releaseGroupId?: string) => {
    try {
      await api.assignRelease(cluster.id, releaseId, releaseGroupId);
      toast.success('Release assigned successfully');
      onUpdate();
      onClose();
    } catch (error) {
      console.error('Failed to assign release:', error);
      toast.error('Failed to assign release');
    }
  };

  const handleUpdateTrackRecording = async (trackId: number) => {
    if (!newRecordingId.trim()) return;

    try {
      await api.updateTrackRecording(cluster.id, trackId, newRecordingId.trim());
      toast.success('Track recording updated');
      setEditingTrackId(null);
      setNewRecordingId('');
      await loadData();
      onUpdate();
    } catch (error) {
      console.error('Failed to update track recording:', error);
      toast.error('Failed to update track recording');
    }
  };

  // Drag and drop setup
  const sensors = useSensors(
    useSensor(PointerSensor),
    useSensor(KeyboardSensor, {
      coordinateGetter: sortableKeyboardCoordinates,
    })
  );

  const handleDragEnd = (event: DragEndEvent) => {
    const { active, over } = event;

    if (over && active.id !== over.id) {
      setOrderedTracks((items) => {
        // Handle both regular items and empty slots
        const oldIndex = items.findIndex((item, idx) =>
          item?.id === active.id || `empty-${idx}` === active.id
        );
        const newIndex = items.findIndex((item, idx) =>
          item?.id === over.id || `empty-${idx}` === over.id
        );

        if (oldIndex === -1 || newIndex === -1) return items;

        return arrayMove(items, oldIndex, newIndex);
      });
    }
  };

  if (loading) {
    return (
      <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50">
        <div className="bg-dark-bg-elevated rounded-lg p-8">
          <Loader2 className="h-8 w-8 text-dark-accent animate-spin" />
        </div>
      </div>
    );
  }

  const displayCandidates = searchResults.length > 0 ? searchResults : candidates;

  return (
    <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4">
      <div className="bg-dark-bg-elevated rounded-lg max-w-6xl w-full max-h-[90vh] overflow-hidden flex flex-col">
        {/* Header */}
        <div className="flex items-start justify-between p-6 border-b border-dark-border">
          <div>
            <h2 className="text-2xl font-bold text-dark-text">Re-match Cluster</h2>
            <p className="text-dark-text-secondary mt-1">
              {cluster.album || 'Unknown Album'} · {cluster.album_artist || 'Unknown Artist'}
            </p>
          </div>
          <button
            onClick={onClose}
            className="text-dark-text-secondary hover:text-dark-text transition-colors"
          >
            <X className="h-6 w-6" />
          </button>
        </div>

        {/* Content */}
        <div className="flex-1 overflow-auto p-6 space-y-6">
          {/* Search Section */}
          <div>
            <h3 className="text-sm font-semibold text-dark-text uppercase tracking-wide mb-3">
              Search MusicBrainz
            </h3>
            <div className="flex gap-3">
              <div className="relative flex-1">
                <SearchIcon className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-dark-text-tertiary" />
                <input
                  type="text"
                  placeholder="Search for releases..."
                  value={searchQuery}
                  onChange={(e) => setSearchQuery(e.target.value)}
                  onKeyDown={(e) => e.key === 'Enter' && handleSearch()}
                  className="w-full pl-9 pr-4 py-2 bg-dark-bg border border-dark-border rounded-lg text-sm text-dark-text placeholder-dark-text-tertiary focus:outline-none focus:border-dark-accent"
                />
              </div>
              <button
                onClick={handleSearch}
                disabled={searching || !searchQuery.trim()}
                className="btn-secondary flex items-center gap-2"
              >
                {searching ? (
                  <Loader2 className="h-4 w-4 animate-spin" />
                ) : (
                  <SearchIcon className="h-4 w-4" />
                )}
                Search
              </button>
              <button
                onClick={handleRefreshCandidates}
                disabled={refreshing}
                className="btn-secondary flex items-center gap-2"
                title="Refresh candidates based on cluster metadata"
              >
                {refreshing ? (
                  <Loader2 className="h-4 w-4 animate-spin" />
                ) : (
                  <RefreshCw className="h-4 w-4" />
                )}
                Refresh
              </button>
            </div>
          </div>

          {/* Local vs MusicBrainz Comparison */}
          <div>
            <h3 className="text-sm font-semibold text-dark-text uppercase tracking-wide mb-3">
              Metadata Comparison
            </h3>
            <div className="grid grid-cols-2 gap-4">
              {/* Local Metadata */}
              <div className="card p-4 bg-dark-bg">
                <div className="text-xs font-semibold text-dark-text-tertiary uppercase tracking-wide mb-2">
                  Your Files
                </div>
                <div className="space-y-1 text-sm">
                  <div>
                    <span className="text-dark-text-tertiary">Album:</span>{' '}
                    <span className="text-dark-text">{cluster.album || '—'}</span>
                  </div>
                  <div>
                    <span className="text-dark-text-tertiary">Artist:</span>{' '}
                    <span className="text-dark-text">{cluster.album_artist || '—'}</span>
                  </div>
                  {cluster.year && (
                    <div>
                      <span className="text-dark-text-tertiary">Year:</span>{' '}
                      <span className="text-dark-text">{cluster.year}</span>
                    </div>
                  )}
                  {cluster.date && (
                    <div>
                      <span className="text-dark-text-tertiary">Date:</span>{' '}
                      <span className="text-dark-text">{cluster.date}</span>
                    </div>
                  )}
                  {cluster.country && (
                    <div>
                      <span className="text-dark-text-tertiary">Country:</span>{' '}
                      <span className="text-dark-text">{cluster.country}</span>
                    </div>
                  )}
                  {cluster.label && (
                    <div>
                      <span className="text-dark-text-tertiary">Label:</span>{' '}
                      <span className="text-dark-text">{cluster.label}</span>
                    </div>
                  )}
                  {cluster.catalog_number && (
                    <div>
                      <span className="text-dark-text-tertiary">Catalog #:</span>{' '}
                      <span className="text-dark-text font-mono text-xs">{cluster.catalog_number}</span>
                    </div>
                  )}
                  {cluster.barcode && (
                    <div>
                      <span className="text-dark-text-tertiary">Barcode:</span>{' '}
                      <span className="text-dark-text font-mono text-xs">{cluster.barcode}</span>
                    </div>
                  )}
                  <div>
                    <span className="text-dark-text-tertiary">Tracks:</span>{' '}
                    <span className="text-dark-text">{cluster.track_count}</span>
                  </div>
                </div>
              </div>

              {/* MusicBrainz Metadata */}
              <div className={`card p-4 ${cluster.mb_release_id ? 'bg-dark-bg border-2 border-green-400/20' : 'bg-dark-bg'}`}>
                <div className="text-xs font-semibold text-dark-text-tertiary uppercase tracking-wide mb-2">
                  MusicBrainz Match {cluster.mb_release_id && '✓'}
                </div>
                {cluster.mb_release_id ? (
                  <div className="space-y-1 text-sm">
                    <div>
                      <span className="text-dark-text-tertiary">Album:</span>{' '}
                      <span className="text-dark-text">{cluster.mb_release_title || '—'}</span>
                    </div>
                    <div>
                      <span className="text-dark-text-tertiary">Artist:</span>{' '}
                      <span className="text-dark-text">{cluster.mb_release_artist || '—'}</span>
                    </div>
                    {cluster.mb_release_date && (
                      <div>
                        <span className="text-dark-text-tertiary">Date:</span>{' '}
                        <span className="text-dark-text">{cluster.mb_release_date}</span>
                      </div>
                    )}
                    {cluster.mb_release_country && (
                      <div>
                        <span className="text-dark-text-tertiary">Country:</span>{' '}
                        <span className="text-dark-text">{cluster.mb_release_country}</span>
                      </div>
                    )}
                    {cluster.mb_release_label && (
                      <div>
                        <span className="text-dark-text-tertiary">Label:</span>{' '}
                        <span className="text-dark-text">{cluster.mb_release_label}</span>
                      </div>
                    )}
                    {cluster.mb_release_catalog_number && (
                      <div>
                        <span className="text-dark-text-tertiary">Catalog #:</span>{' '}
                        <span className="text-dark-text font-mono text-xs">{cluster.mb_release_catalog_number}</span>
                      </div>
                    )}
                    {cluster.mb_release_barcode && (
                      <div>
                        <span className="text-dark-text-tertiary">Barcode:</span>{' '}
                        <span className="text-dark-text font-mono text-xs">{cluster.mb_release_barcode}</span>
                      </div>
                    )}
                    {cluster.mb_release_format && (
                      <div>
                        <span className="text-dark-text-tertiary">Format:</span>{' '}
                        <span className="text-dark-text">{cluster.mb_release_format}</span>
                      </div>
                    )}
                    {cluster.mb_release_track_count !== undefined && cluster.mb_release_track_count !== null && (
                      <div>
                        <span className="text-dark-text-tertiary">Tracks:</span>{' '}
                        <span className="text-dark-text">{cluster.mb_release_track_count}</span>
                      </div>
                    )}
                    {cluster.mb_confidence && (
                      <div>
                        <span className="text-dark-text-tertiary">Confidence:</span>{' '}
                        <span className="text-dark-accent font-mono text-xs">
                          {Math.round(cluster.mb_confidence * 100)}%
                        </span>
                      </div>
                    )}
                    {cluster.match_source && (
                      <div>
                        <span className="text-dark-text-tertiary">Source:</span>{' '}
                        <span className="text-dark-text text-xs">
                          {cluster.match_source === 'auto_fingerprint' ? 'Auto (Fingerprint)' :
                           cluster.match_source === 'auto' ? 'Auto (Metadata)' :
                           cluster.match_source === 'manual' ? 'Manual' : cluster.match_source}
                        </span>
                      </div>
                    )}
                    {cluster.match_locked && (
                      <div>
                        <span className="text-dark-text-tertiary">Status:</span>{' '}
                        <span className="text-purple-400 text-xs">🔒 Locked</span>
                      </div>
                    )}
                    <div className="pt-2 space-y-1">
                      <a
                        href={`https://musicbrainz.org/release/${cluster.mb_release_id}`}
                        target="_blank"
                        rel="noopener noreferrer"
                        className="text-xs text-dark-accent hover:underline block"
                      >
                        View Release on MusicBrainz →
                      </a>
                      {cluster.mb_release_group_id && (
                        <a
                          href={`https://musicbrainz.org/release-group/${cluster.mb_release_group_id}`}
                          target="_blank"
                          rel="noopener noreferrer"
                          className="text-xs text-dark-accent hover:underline block"
                        >
                          View Release Group on MusicBrainz →
                        </a>
                      )}
                    </div>
                  </div>
                ) : (
                  <div className="text-sm text-dark-text-secondary italic">
                    No match assigned yet
                  </div>
                )}
              </div>
            </div>
          </div>

          {/* Current Match */}
          {cluster.mb_release_id && (
            <div>
              <h3 className="text-sm font-semibold text-dark-text uppercase tracking-wide mb-3">
                Current Match
              </h3>
              <div className="card p-4 bg-dark-bg border-2 border-green-400/20">
                <div className="flex items-start justify-between">
                  <div>
                    <div className="flex items-center gap-2">
                      <CheckCircle2 className="h-5 w-5 text-green-400" />
                      <div className="font-medium text-dark-text">{cluster.mb_release_title}</div>
                    </div>
                    <div className="text-sm text-dark-text-secondary mt-1">{cluster.mb_release_artist}</div>
                    <div className="flex items-center gap-3 mt-2 text-xs text-dark-text-tertiary">
                      {cluster.mb_release_date && <span>{cluster.mb_release_date}</span>}
                      {cluster.mb_release_country && <span>{cluster.mb_release_country}</span>}
                      {cluster.mb_confidence && (
                        <span className="font-mono text-dark-accent">
                          {Math.round(cluster.mb_confidence * 100)}% confidence
                        </span>
                      )}
                    </div>
                  </div>
                  <a
                    href={`https://musicbrainz.org/release/${cluster.mb_release_id}`}
                    target="_blank"
                    rel="noopener noreferrer"
                    className="text-xs text-dark-accent hover:underline"
                  >
                    View on MB →
                  </a>
                </div>
              </div>
            </div>
          )}

          {/* Candidates */}
          <div>
            <h3 className="text-sm font-semibold text-dark-text uppercase tracking-wide mb-3">
              {searchResults.length > 0 ? 'Search Results' : 'Available Candidates'} ({displayCandidates.length})
            </h3>
            {displayCandidates.length === 0 ? (
              <div className="card p-8 bg-dark-bg text-center">
                <AlertTriangle className="h-12 w-12 mx-auto mb-3 text-dark-text-tertiary" />
                <p className="text-dark-text-secondary mb-2">
                  {searchResults.length === 0 && candidates.length === 0
                    ? 'No candidates found during automatic identification.'
                    : 'No results found. Try a different search query.'}
                </p>
                {searchResults.length === 0 && candidates.length === 0 && (
                  <p className="text-sm text-dark-text-tertiary">
                    Use the search bar above to manually find and select the correct release.
                  </p>
                )}
              </div>
            ) : (
              <div className="space-y-2">
                {displayCandidates.map((candidate) => {
                  const isCurrent = candidate.release_id === cluster.mb_release_id;
                  return (
                    <div
                      key={candidate.release_id}
                      className={`card p-4 bg-dark-bg hover:bg-dark-bg-elevated transition-colors ${
                        isCurrent ? 'border-2 border-green-400/20' : ''
                      }`}
                    >
                      <div className="flex items-start justify-between">
                        <div className="flex-1 min-w-0">
                          <div className="flex items-center gap-2">
                            {isCurrent && <CheckCircle2 className="h-4 w-4 text-green-400 flex-shrink-0" />}
                            <div className="font-medium text-dark-text">{candidate.title}</div>
                          </div>
                          <div className="text-sm text-dark-text-secondary mt-1">{candidate.artist}</div>
                          <div className="flex items-center gap-3 mt-2 text-xs text-dark-text-tertiary flex-wrap">
                            {candidate.date && <span>{candidate.date}</span>}
                            {candidate.country && <span>{candidate.country}</span>}
                            {candidate.track_count > 0 && <span>{candidate.track_count} tracks</span>}
                            {candidate.barcode && <span className="font-mono">🏷️ {candidate.barcode}</span>}
                            {candidate.label && <span>📀 {candidate.label}</span>}
                            {candidate.catalog_number && <span className="font-mono">#{candidate.catalog_number}</span>}
                            {candidate.confidence > 0 && (
                              <span className="font-mono text-dark-accent">
                                {Math.round(candidate.confidence * 100)}%
                              </span>
                            )}
                            <a
                              href={`https://musicbrainz.org/release/${candidate.release_id}`}
                              target="_blank"
                              rel="noopener noreferrer"
                              className="text-dark-accent hover:underline"
                              onClick={(e) => e.stopPropagation()}
                            >
                              View on MusicBrainz →
                            </a>
                          </div>
                        </div>
                        {!isCurrent && (
                          <button
                            onClick={() => handleAssignRelease(candidate.release_id)}
                            className="btn-primary text-xs ml-4"
                          >
                            Select
                          </button>
                        )}
                      </div>
                    </div>
                  );
                })}
              </div>
            )}
          </div>

          {/* Track Matching - Drag and Drop */}
          {mbTracks && mbTracks.length > 0 && (
            <div>
              <h3 className="text-sm font-semibold text-dark-text uppercase tracking-wide mb-3">
                Track Matching ({orderedTracks.length} local / {mbTracks.length} MusicBrainz)
              </h3>
              <p className="text-sm text-dark-text-secondary mb-4">
                Drag your local tracks (left) to reorder them to match the MusicBrainz tracks (right). This helps correct track matching when the automatic algorithm gets it wrong.
              </p>
              <div className="grid grid-cols-2 gap-4">
                {/* Local tracks - with gaps for alignment and drag-drop */}
                <div>
                  <div className="text-xs font-semibold text-dark-text-tertiary uppercase tracking-wide mb-2">
                    Your Files (Drag to Reorder)
                  </div>
                  <DndContext
                    sensors={sensors}
                    collisionDetection={closestCenter}
                    onDragEnd={handleDragEnd}
                  >
                    <SortableContext
                      items={orderedTracks.map((t, i) => t?.id || `empty-${i}`)}
                      strategy={verticalListSortingStrategy}
                    >
                      <div className="space-y-2">
                        {mbTracks.map((mbTrack, mbIndex) => {
                          // Get the track at this position in the ordered list
                          const localTrack = orderedTracks[mbIndex];

                          // Smart comparison: check multiple factors
                          let matchQuality: 'good' | 'partial' | 'poor' | 'none' = 'none';

                          if (localTrack) {
                            let score = 0;
                            let checks = 0;

                            // Check 1: Recording ID matches (strongest signal)
                            if (localTrack.mb_recording_id === mbTrack.recording_id) {
                              score += 3;
                            }
                            checks += 3;

                            // Check 2: Title similarity (case-insensitive contains)
                            if (localTrack.title && mbTrack.title) {
                              const localTitle = localTrack.title.toLowerCase();
                              const mbTitle = mbTrack.title.toLowerCase();
                              if (localTitle === mbTitle) {
                                score += 2;
                              } else if (localTitle.includes(mbTitle) || mbTitle.includes(localTitle)) {
                                score += 1;
                              }
                              checks += 2;
                            }

                            // Check 3: Duration similarity (within 5 seconds)
                            if (localTrack.duration && mbTrack.length) {
                              const localDuration = localTrack.duration;
                              const mbDuration = mbTrack.length / 1000; // MB is in milliseconds
                              const diff = Math.abs(localDuration - mbDuration);
                              if (diff <= 2) {
                                score += 2;
                              } else if (diff <= 5) {
                                score += 1;
                              }
                              checks += 2;
                            }

                            // Determine quality based on score
                            const percentage = checks > 0 ? (score / checks) : 0;
                            if (percentage >= 0.8) {
                              matchQuality = 'good';
                            } else if (percentage >= 0.5) {
                              matchQuality = 'partial';
                            } else if (percentage > 0) {
                              matchQuality = 'poor';
                            }
                          }

                          const ringClass =
                            matchQuality === 'good' ? 'ring-2 ring-green-500/70 rounded' :
                            matchQuality === 'partial' ? 'ring-2 ring-yellow-500/70 rounded' :
                            matchQuality === 'poor' ? 'ring-2 ring-red-500/70 rounded' :
                            '';

                          return (
                            <div key={`slot-${mbTrack.disc_number}-${mbTrack.position}`}>
                              {localTrack ? (
                                <div className={ringClass}>
                                  <SortableTrackItem track={localTrack} />
                                </div>
                              ) : (
                                <SortableEmptySlot index={mbIndex} />
                              )}
                            </div>
                          );
                        })}
                      </div>
                    </SortableContext>
                  </DndContext>
                </div>

                {/* MusicBrainz tracks - static reference */}
                <div>
                  <div className="text-xs font-semibold text-dark-text-tertiary uppercase tracking-wide mb-2">
                    MusicBrainz Release
                  </div>
                  <div className="space-y-2">
                    {mbTracks.map((track) => (
                      <MBTrackItem key={`mb-${track.disc_number}-${track.position}`} track={track} />
                    ))}
                  </div>
                </div>
              </div>
            </div>
          )}
        </div>
      </div>
    </div>
  );
}
