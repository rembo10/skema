import { useState, useEffect } from 'react';
import { Cluster, ClusterWithTracks, CandidateRelease, ClusterTrack } from '../../types/api';
import { api } from '../../lib/api';
import toast from 'react-hot-toast';
import {
  Music,
  ExternalLink,
  CheckCircle2,
  AlertCircle,
  RefreshCw,
  Lock,
  Unlock,
  Edit2,
  Check,
  X
} from 'lucide-react';

interface MatchEditorProps {
  clusters: Cluster[];
  onClusterUpdate: () => void;
}

export function MatchEditor({ clusters, onClusterUpdate }: MatchEditorProps) {
  const [selectedCluster, setSelectedCluster] = useState<ClusterWithTracks | null>(null);
  const [candidates, setCandidates] = useState<CandidateRelease[]>([]);
  const [loading, setLoading] = useState(false);
  const [loadingCandidates, setLoadingCandidates] = useState(false);
  const [editingTrackId, setEditingTrackId] = useState<number | null>(null);
  const [editRecordingId, setEditRecordingId] = useState('');

  const loadClusterDetails = async (clusterId: number) => {
    try {
      setLoading(true);
      const data = await api.getClusterWithTracks(clusterId);

      if (!data || !data.cluster || !data.tracks) {
        console.error('Invalid cluster data structure:', data);
        toast.error('Invalid cluster data received');
        setSelectedCluster(null);
        return;
      }

      setSelectedCluster(data);

      // Load candidates if the cluster has a release
      if (data.cluster.mb_release_id) {
        loadCandidates(clusterId);
      } else {
        setCandidates([]);
      }
    } catch (error) {
      console.error('Failed to load cluster details:', error);
      toast.error('Failed to load cluster details');
      setSelectedCluster(null);
    } finally {
      setLoading(false);
    }
  };

  const loadCandidates = async (clusterId: number) => {
    try {
      setLoadingCandidates(true);
      const candidatesList = await api.getCandidates(clusterId);
      setCandidates(candidatesList);
    } catch (error) {
      console.error('Failed to load candidates:', error);
      // Don't show error toast for candidates as it's not critical
    } finally {
      setLoadingCandidates(false);
    }
  };

  const handleAssignRelease = async (releaseId: string, confidence: number) => {
    if (!selectedCluster) return;

    try {
      setLoading(true);
      await api.assignRelease(selectedCluster.cluster.id, releaseId, undefined, confidence);
      toast.success('Release assigned successfully');
      onClusterUpdate();
      // Reload cluster details
      await loadClusterDetails(selectedCluster.cluster.id);
    } catch (error) {
      console.error('Failed to assign release:', error);
      toast.error('Failed to assign release');
    } finally {
      setLoading(false);
    }
  };

  const handleRemoveRelease = async () => {
    if (!selectedCluster) return;

    try {
      setLoading(true);
      await api.removeRelease(selectedCluster.cluster.id);
      toast.success('Release removed');
      onClusterUpdate();
      // Reload cluster details
      await loadClusterDetails(selectedCluster.cluster.id);
    } catch (error) {
      console.error('Failed to remove release:', error);
      toast.error('Failed to remove release');
    } finally {
      setLoading(false);
    }
  };

  const handleUpdateTrackRecording = async (trackId: number, recordingId: string) => {
    if (!selectedCluster) return;

    try {
      await api.updateTrackRecording(selectedCluster.cluster.id, trackId, recordingId);
      toast.success('Track recording updated');
      // Reload cluster details
      await loadClusterDetails(selectedCluster.cluster.id);
      setEditingTrackId(null);
      setEditRecordingId('');
    } catch (error) {
      console.error('Failed to update track recording:', error);
      toast.error('Failed to update track recording');
    }
  };

  const startEditingTrack = (track: ClusterTrack) => {
    setEditingTrackId(track.id);
    setEditRecordingId(track.mb_recording_id || '');
  };

  const cancelEditingTrack = () => {
    setEditingTrackId(null);
    setEditRecordingId('');
  };

  return (
    <div className="flex-1 flex gap-6 overflow-hidden">
      {/* Left panel - Cluster list */}
      <div className="w-96 flex flex-col bg-dark-bg-elevated border border-dark-border rounded-lg overflow-hidden">
        <div className="p-4 border-b border-dark-border">
          <h2 className="text-lg font-semibold text-dark-text">Clusters</h2>
          <p className="text-xs text-dark-text-tertiary mt-1">{clusters.length} total</p>
        </div>

        <div className="flex-1 overflow-y-auto">
          {clusters.map((cluster) => {
            const isSelected = selectedCluster?.cluster?.id === cluster.id;
            const isMatched = cluster.mb_release_id !== null;

            return (
              <button
                key={cluster.id}
                onClick={() => loadClusterDetails(cluster.id)}
                className={`w-full p-4 border-b border-dark-border text-left transition-colors ${
                  isSelected
                    ? 'bg-dark-accent/20 border-l-4 border-l-dark-accent'
                    : 'hover:bg-dark-bg-hover border-l-4 border-l-transparent'
                }`}
              >
                <div className="flex items-start justify-between gap-2">
                  <div className="flex-1 min-w-0">
                    <div className="font-medium text-dark-text text-sm truncate">
                      {cluster.album || <span className="italic text-dark-text-tertiary">Unknown Album</span>}
                    </div>
                    <div className="text-xs text-dark-text-secondary truncate mt-0.5">
                      {cluster.album_artist || <span className="italic text-dark-text-tertiary">Unknown Artist</span>}
                    </div>

                    {/* Match status */}
                    <div className="flex items-center gap-2 mt-2">
                      {isMatched ? (
                        <>
                          <CheckCircle2 className="h-3 w-3 text-dark-success" />
                          <span className="text-xs text-dark-success">Matched</span>
                          {cluster.match_locked && <Lock className="h-3 w-3 text-purple-400" />}
                        </>
                      ) : (
                        <>
                          <AlertCircle className="h-3 w-3 text-dark-error" />
                          <span className="text-xs text-dark-error">Not matched</span>
                        </>
                      )}
                    </div>
                  </div>
                </div>
              </button>
            );
          })}
        </div>
      </div>

      {/* Right panel - Match editor */}
      <div className="flex-1 flex flex-col bg-dark-bg-elevated border border-dark-border rounded-lg overflow-hidden">
        {loading && !selectedCluster ? (
          <div className="flex-1 flex items-center justify-center">
            <RefreshCw className="h-8 w-8 text-dark-accent animate-spin" />
          </div>
        ) : selectedCluster ? (
          <>
            {/* Cluster header */}
            <div className="p-6 border-b border-dark-border">
              <h2 className="text-2xl font-bold text-dark-text">
                {selectedCluster.cluster.album || 'Unknown Album'}
              </h2>
              <p className="text-dark-text-secondary mt-1">
                {selectedCluster.cluster.album_artist || 'Unknown Artist'}
              </p>
            </div>

            {/* Scrollable content */}
            <div className="flex-1 overflow-y-auto p-6 space-y-6">
              {/* Current release match */}
              <div>
                <div className="flex items-center justify-between mb-3">
                  <h3 className="text-sm font-semibold text-dark-text uppercase tracking-wide">
                    Current MusicBrainz Match
                  </h3>
                  {selectedCluster.cluster.mb_release_id && (
                    <button
                      onClick={handleRemoveRelease}
                      disabled={loading}
                      className="text-xs px-3 py-1 bg-dark-error/10 text-dark-error rounded hover:bg-dark-error/20 transition-colors disabled:opacity-50"
                    >
                      Remove Match
                    </button>
                  )}
                </div>

                {selectedCluster.cluster.mb_release_id ? (
                  <div className="bg-dark-bg-subtle rounded-lg p-4">
                    <a
                      href={`https://musicbrainz.org/release/${selectedCluster.cluster.mb_release_id}`}
                      target="_blank"
                      rel="noopener noreferrer"
                      className="text-dark-accent hover:text-dark-accent/80 font-medium flex items-center gap-2"
                    >
                      {selectedCluster.cluster.mb_release_title || selectedCluster.cluster.album || 'View on MusicBrainz'}
                      <ExternalLink className="h-4 w-4" />
                    </a>
                    {selectedCluster.cluster.mb_confidence !== null && (
                      <div className="flex items-center gap-2 mt-3 pt-3 border-t border-dark-border">
                        <span className="text-sm text-dark-text-secondary">Confidence:</span>
                        <div className="flex-1 bg-dark-bg h-2 rounded-full overflow-hidden">
                          <div
                            className={`h-full ${
                              selectedCluster.cluster.mb_confidence >= 0.8
                                ? 'bg-dark-success'
                                : selectedCluster.cluster.mb_confidence >= 0.6
                                ? 'bg-blue-500'
                                : 'bg-yellow-500'
                            }`}
                            style={{ width: `${selectedCluster.cluster.mb_confidence * 100}%` }}
                          />
                        </div>
                        <span className="text-sm font-medium text-dark-text">
                          {Math.round(selectedCluster.cluster.mb_confidence * 100)}%
                        </span>
                      </div>
                    )}
                    {selectedCluster.cluster.match_locked && (
                      <div className="flex items-center gap-2 mt-2 text-xs text-purple-400">
                        <Lock className="h-3 w-3" />
                        <span>This match is locked (manually assigned)</span>
                      </div>
                    )}
                  </div>
                ) : (
                  <div className="bg-dark-bg-subtle rounded-lg p-4 text-center">
                    <AlertCircle className="h-8 w-8 text-dark-error mx-auto mb-2" />
                    <p className="text-sm text-dark-text-secondary">
                      This cluster has not been matched to a MusicBrainz release
                    </p>
                  </div>
                )}
              </div>

              {/* Alternative candidates */}
              {candidates.length > 0 && (
                <div>
                  <h3 className="text-sm font-semibold text-dark-text uppercase tracking-wide mb-3">
                    Alternative Release Candidates
                  </h3>
                  <div className="space-y-2">
                    {candidates.map((candidate) => {
                      const isCurrentMatch = candidate.release_id === selectedCluster.cluster.mb_release_id;

                      return (
                        <div
                          key={candidate.release_id}
                          className={`p-4 rounded-lg border transition-colors ${
                            isCurrentMatch
                              ? 'bg-dark-accent/10 border-dark-accent'
                              : 'bg-dark-bg-subtle border-dark-border hover:border-dark-accent/50'
                          }`}
                        >
                          <div className="flex items-start justify-between gap-4">
                            <div className="flex-1 min-w-0">
                              <div className="font-medium text-dark-text text-sm">
                                {candidate.title}
                              </div>
                              <div className="text-xs text-dark-text-secondary mt-1">
                                {candidate.artist}
                              </div>
                              <div className="flex items-center gap-3 mt-2 text-xs text-dark-text-tertiary">
                                {candidate.country && <span>{candidate.country}</span>}
                                {candidate.date && <span>{candidate.date.substring(0, 4)}</span>}
                                <span>{candidate.track_count} tracks</span>
                              </div>
                            </div>
                            <div className="flex items-center gap-2">
                              {!isCurrentMatch && (
                                <button
                                  onClick={() => handleAssignRelease(candidate.release_id, candidate.confidence)}
                                  disabled={loading}
                                  className="text-xs px-3 py-1 bg-dark-accent text-white rounded hover:bg-dark-accent/80 transition-colors disabled:opacity-50"
                                >
                                  Use This
                                </button>
                              )}
                              {isCurrentMatch && (
                                <div className="flex items-center gap-1 text-xs text-dark-success">
                                  <CheckCircle2 className="h-4 w-4" />
                                  <span>Current</span>
                                </div>
                              )}
                            </div>
                          </div>
                        </div>
                      );
                    })}
                  </div>
                </div>
              )}

              {/* Track listing with recording mappings */}
              <div>
                <h3 className="text-sm font-semibold text-dark-text uppercase tracking-wide mb-3">
                  Track → Recording Mappings
                </h3>
                {selectedCluster.tracks.length === 0 ? (
                  <div className="bg-dark-bg-subtle rounded-lg p-8 text-center">
                    <Music className="h-8 w-8 text-dark-text-tertiary mx-auto mb-2 opacity-50" />
                    <p className="text-sm text-dark-text-secondary">No tracks in this cluster</p>
                  </div>
                ) : (
                  <div className="space-y-2">
                    {selectedCluster.tracks.map((track) => {
                      const isEditing = editingTrackId === track.id;

                      return (
                        <div
                          key={track.id}
                          className="p-3 bg-dark-bg-subtle rounded-lg"
                        >
                          <div className="flex items-start gap-3">
                            <Music className="h-4 w-4 text-dark-accent flex-shrink-0 mt-0.5" />
                            <div className="flex-1 min-w-0">
                              <div className="text-sm font-medium text-dark-text truncate">
                                {track.title || <span className="text-dark-text-tertiary italic">Unknown Title</span>}
                              </div>
                              {track.artist && (
                                <div className="text-xs text-dark-text-secondary truncate">
                                  {track.artist}
                                </div>
                              )}

                              {/* Recording mapping */}
                              {!isEditing && (
                                <div className="flex items-center gap-2 mt-2">
                                  {track.mb_recording_id ? (
                                    <>
                                      <CheckCircle2 className="h-3 w-3 text-dark-success" />
                                      <span className="text-xs text-dark-success">
                                        {track.mb_recording_title || track.mb_recording_id}
                                      </span>
                                      <button
                                        onClick={() => startEditingTrack(track)}
                                        className="ml-2 p-1 hover:bg-dark-bg rounded"
                                      >
                                        <Edit2 className="h-3 w-3 text-dark-text-tertiary" />
                                      </button>
                                    </>
                                  ) : (
                                    <>
                                      <AlertCircle className="h-3 w-3 text-dark-error" />
                                      <span className="text-xs text-dark-error">No recording mapped</span>
                                      <button
                                        onClick={() => startEditingTrack(track)}
                                        className="ml-2 text-xs px-2 py-1 bg-dark-accent/10 text-dark-accent rounded hover:bg-dark-accent/20"
                                      >
                                        Add Recording
                                      </button>
                                    </>
                                  )}
                                </div>
                              )}

                              {/* Edit mode */}
                              {isEditing && (
                                <div className="flex items-center gap-2 mt-2">
                                  <input
                                    type="text"
                                    value={editRecordingId}
                                    onChange={(e) => setEditRecordingId(e.target.value)}
                                    placeholder="Recording MBID"
                                    className="flex-1 px-2 py-1 text-xs bg-dark-bg border border-dark-border rounded text-dark-text"
                                  />
                                  <button
                                    onClick={() => handleUpdateTrackRecording(track.id, editRecordingId)}
                                    disabled={!editRecordingId.trim()}
                                    className="p-1 text-dark-success hover:bg-dark-success/10 rounded disabled:opacity-50"
                                  >
                                    <Check className="h-4 w-4" />
                                  </button>
                                  <button
                                    onClick={cancelEditingTrack}
                                    className="p-1 text-dark-error hover:bg-dark-error/10 rounded"
                                  >
                                    <X className="h-4 w-4" />
                                  </button>
                                </div>
                              )}
                            </div>
                            <div className="flex items-center gap-3 text-xs text-dark-text-tertiary flex-shrink-0">
                              {track.track_number !== null && (
                                <div className="font-mono">
                                  {track.disc_number ? `${track.disc_number}-` : ''}{track.track_number}
                                </div>
                              )}
                            </div>
                          </div>
                        </div>
                      );
                    })}
                  </div>
                )}
              </div>
            </div>
          </>
        ) : (
          <div className="flex-1 flex items-center justify-center">
            <div className="text-center">
              <Music className="h-12 w-12 text-dark-text-tertiary mx-auto mb-4 opacity-50" />
              <p className="text-dark-text-secondary">Select a cluster to edit matches</p>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}
