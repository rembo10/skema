import { useState, useEffect } from 'react';
import { Cluster, ClusterTrack, CandidateRelease } from '../../types/api';
import { api } from '../../lib/api';
import toast from 'react-hot-toast';
import {
  X,
  Disc,
  Lock,
  Unlock,
  Zap,
  Search as SearchIcon,
  CheckCircle2,
  AlertCircle,
  Loader2,
  RefreshCw,
  Edit2,
  Save,
  XCircle,
  Trash2,
} from 'lucide-react';

interface ClusterModalProps {
  clusterId: number;
  onClose: () => void;
  onUpdate: () => void;
}

export function ClusterModal({ clusterId, onClose, onUpdate }: ClusterModalProps) {
  const [cluster, setCluster] = useState<Cluster | null>(null);
  const [tracks, setTracks] = useState<ClusterTrack[]>([]);
  const [candidates, setCandidates] = useState<CandidateRelease[]>([]);
  const [loading, setLoading] = useState(true);
  const [identifying, setIdentifying] = useState(false);
  const [editingTrackId, setEditingTrackId] = useState<number | null>(null);
  const [newRecordingId, setNewRecordingId] = useState('');

  useEffect(() => {
    loadClusterData();
  }, [clusterId]);

  const loadClusterData = async () => {
    try {
      setLoading(true);
      const data = await api.getClusterWithTracks(clusterId);
      setCluster(data.cluster);
      setTracks(data.tracks);

      // Load candidate releases from API
      const candidateData = await api.getCandidates(clusterId);
      setCandidates(candidateData);
    } catch (error) {
      console.error('Failed to load cluster:', error);
      toast.error('Failed to load cluster details');
    } finally {
      setLoading(false);
    }
  };

  const handleReidentify = async () => {
    if (!cluster) return;

    try {
      setIdentifying(true);
      // Remove current match to unlock cluster
      if (cluster.mb_release_id) {
        await api.removeRelease(cluster.id);
      }
      // Trigger library identification via event bus
      await api.submitEvent('IdentificationRequested');
      toast.success('Re-identification triggered - check back shortly');
      await loadClusterData();
      onUpdate();
    } catch (error) {
      console.error('Failed to re-identify cluster:', error);
      toast.error('Failed to re-identify cluster');
    } finally {
      setIdentifying(false);
    }
  };

  const handleMatchToRelease = async (releaseId: string, releaseGroupId?: string) => {
    if (!cluster) return;

    try {
      await api.assignRelease(cluster.id, releaseId, releaseGroupId);
      toast.success('Release assigned to cluster');
      await loadClusterData();
      onUpdate();
    } catch (error) {
      console.error('Failed to assign release:', error);
      toast.error('Failed to assign release');
    }
  };

  const handleRemoveRelease = async () => {
    if (!cluster) return;

    try {
      await api.removeRelease(cluster.id);
      toast.success('Release removed from cluster');
      await loadClusterData();
      onUpdate();
    } catch (error) {
      console.error('Failed to remove release:', error);
      toast.error('Failed to remove release');
    }
  };

  const handleUpdateTrackRecording = async (trackId: number) => {
    if (!cluster || !newRecordingId.trim()) return;

    try {
      await api.updateTrackRecording(cluster.id, trackId, newRecordingId.trim());
      toast.success('Track recording updated');
      setEditingTrackId(null);
      setNewRecordingId('');
      await loadClusterData();
      onUpdate();
    } catch (error) {
      console.error('Failed to update track recording:', error);
      toast.error('Failed to update track recording');
    }
  };

  if (loading || !cluster) {
    return (
      <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50">
        <div className="bg-dark-bg-elevated rounded-lg p-8">
          <Loader2 className="h-8 w-8 text-dark-accent animate-spin" />
        </div>
      </div>
    );
  }

  const getMatchIcon = () => {
    if (!cluster.mb_release_id) {
      return <AlertCircle className="h-5 w-5 text-red-400" />;
    }
    if (cluster.match_locked) {
      return <Lock className="h-5 w-5 text-purple-400" />;
    }
    if (cluster.match_source === 'auto_fingerprint') {
      return <Zap className="h-5 w-5 text-blue-400" />;
    }
    return <CheckCircle2 className="h-5 w-5 text-green-400" />;
  };

  return (
    <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4">
      <div className="bg-dark-bg-elevated rounded-lg max-w-4xl w-full max-h-[90vh] overflow-hidden flex flex-col">
        {/* Header */}
        <div className="flex items-start justify-between p-6 border-b border-dark-border">
          <div className="flex items-start gap-4">
            <div className="p-3 bg-dark-bg rounded-lg">
              <Disc className="h-8 w-8 text-dark-accent" />
            </div>
            <div>
              <div className="flex items-center gap-3">
                {getMatchIcon()}
                <h2 className="text-2xl font-bold text-dark-text">
                  {cluster.album || cluster.mb_release_title || 'Unknown Album'}
                </h2>
              </div>
              <p className="text-dark-text-secondary mt-1">
                {cluster.album_artist || cluster.mb_release_artist || 'Unknown Artist'}
              </p>
              <div className="flex items-center gap-4 mt-2 text-sm text-dark-text-tertiary">
                <span>{tracks.length} tracks</span>
                {cluster.year && <span>{cluster.year}</span>}
                {cluster.mb_confidence && (
                  <span className="font-mono">
                    {Math.round(cluster.mb_confidence * 100)}% confidence
                  </span>
                )}
              </div>
            </div>
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
          {/* Actions */}
          <div className="flex gap-3">
            <button
              onClick={handleReidentify}
              disabled={identifying}
              className="btn-secondary flex items-center gap-2"
            >
              {identifying ? (
                <Loader2 className="h-4 w-4 animate-spin" />
              ) : (
                <RefreshCw className="h-4 w-4" />
              )}
              Re-identify
            </button>
            {cluster.mb_release_id && (
              <button
                onClick={handleRemoveRelease}
                className="btn-secondary flex items-center gap-2 text-red-400 hover:text-red-300"
              >
                <Trash2 className="h-4 w-4" />
                Remove Match
              </button>
            )}
          </div>

          {/* Current Match */}
          {cluster.mb_release_id && (
            <div>
              <h3 className="text-sm font-semibold text-dark-text uppercase tracking-wide mb-3">
                Current Match
              </h3>
              <div className="card p-4 bg-dark-bg">
                <div className="flex items-start justify-between">
                  <div>
                    <div className="font-medium text-dark-text">{cluster.mb_release_title}</div>
                    <div className="text-sm text-dark-text-secondary">{cluster.mb_release_artist}</div>
                    {cluster.mb_release_date && (
                      <div className="text-xs text-dark-text-tertiary mt-1">
                        {cluster.mb_release_date}
                        {cluster.mb_release_country && ` · ${cluster.mb_release_country}`}
                      </div>
                    )}
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

          {/* Tracks in Cluster */}
          <div>
            <h3 className="text-sm font-semibold text-dark-text uppercase tracking-wide mb-3">
              Tracks ({tracks.length})
            </h3>
            <div className="space-y-2">
              {tracks.map((track) => (
                <div
                  key={track.id}
                  className="card p-3 bg-dark-bg"
                >
                  <div className="flex items-start justify-between gap-3">
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
                        <div className="text-xs text-dark-text-secondary truncate">
                          {track.artist}
                        </div>
                      )}
                      {editingTrackId === track.id ? (
                        <div className="mt-2 flex items-center gap-2">
                          <input
                            type="text"
                            placeholder="MusicBrainz Recording ID"
                            value={newRecordingId}
                            onChange={(e) => setNewRecordingId(e.target.value)}
                            className="flex-1 px-2 py-1 text-xs bg-dark-bg-elevated border border-dark-border rounded text-dark-text placeholder-dark-text-tertiary focus:outline-none focus:border-dark-accent"
                            autoFocus
                          />
                          <button
                            onClick={() => handleUpdateTrackRecording(track.id)}
                            className="p-1 text-green-400 hover:text-green-300 transition-colors"
                            title="Save"
                          >
                            <Save className="h-4 w-4" />
                          </button>
                          <button
                            onClick={() => {
                              setEditingTrackId(null);
                              setNewRecordingId('');
                            }}
                            className="p-1 text-red-400 hover:text-red-300 transition-colors"
                            title="Cancel"
                          >
                            <XCircle className="h-4 w-4" />
                          </button>
                        </div>
                      ) : (
                        <div className="flex items-center gap-2 mt-1">
                          {track.mb_recording_title ? (
                            <div className="text-xs text-dark-text-tertiary truncate">
                              MB: {track.mb_recording_title}
                              {track.mb_recording_id && (
                                <span className="ml-2 font-mono text-dark-text-tertiary/50">
                                  ({track.mb_recording_id})
                                </span>
                              )}
                            </div>
                          ) : (
                            <div className="text-xs italic text-red-400">
                              No recording match
                            </div>
                          )}
                        </div>
                      )}
                    </div>
                    {editingTrackId !== track.id && (
                      <button
                        onClick={() => {
                          setEditingTrackId(track.id);
                          setNewRecordingId(track.mb_recording_id || '');
                        }}
                        className="flex-shrink-0 p-1 text-dark-text-secondary hover:text-dark-accent transition-colors"
                        title="Edit recording mapping"
                      >
                        <Edit2 className="h-4 w-4" />
                      </button>
                    )}
                  </div>
                </div>
              ))}
            </div>
          </div>

          {/* Alternate Releases */}
          {candidates.length > 0 && (
            <div>
              <h3 className="text-sm font-semibold text-dark-text uppercase tracking-wide mb-3">
                Alternate Releases ({candidates.length})
              </h3>
              <div className="space-y-2">
                {candidates.map((candidate) => (
                  <div
                    key={candidate.release_id}
                    className="card p-4 bg-dark-bg hover:bg-dark-bg-elevated transition-colors"
                  >
                    <div className="flex items-start justify-between">
                      <div className="flex-1 min-w-0">
                        <div className="font-medium text-dark-text">{candidate.title}</div>
                        <div className="text-sm text-dark-text-secondary">{candidate.artist}</div>
                        <div className="flex items-center gap-3 mt-1 text-xs text-dark-text-tertiary">
                          {candidate.date && <span>{candidate.date}</span>}
                          {candidate.country && <span>{candidate.country}</span>}
                          <span>{candidate.track_count} tracks</span>
                          <span className="font-mono text-dark-accent">
                            {Math.round(candidate.confidence * 100)}%
                          </span>
                        </div>
                      </div>
                      <button
                        onClick={() => handleMatchToRelease(candidate.release_id)}
                        className="btn-primary text-xs"
                      >
                        Match
                      </button>
                    </div>
                  </div>
                ))}
              </div>
            </div>
          )}

          {!cluster.mb_release_id && candidates.length === 0 && (
            <div className="text-center py-8 text-dark-text-tertiary">
              <AlertCircle className="h-12 w-12 mx-auto mb-3 opacity-50" />
              <p className="text-sm">No matches found for this cluster</p>
              <p className="text-xs mt-1">Try re-identifying to find matches</p>
            </div>
          )}
        </div>
      </div>
    </div>
  );
}
