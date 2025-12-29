import { useState } from 'react';
import { Cluster, ClusterWithTracks } from '../../types/api';
import { api } from '../../lib/api';
import toast from 'react-hot-toast';
import { Disc, Music, ExternalLink, CheckCircle2, AlertCircle, ChevronRight, Lock, Zap, Search as SearchIcon } from 'lucide-react';

interface InspectorViewProps {
  clusters: Cluster[];
  onClusterUpdate: () => void;
}

export function InspectorView({ clusters, onClusterUpdate }: InspectorViewProps) {
  const [selectedCluster, setSelectedCluster] = useState<ClusterWithTracks | null>(null);
  const [loading, setLoading] = useState(false);

  const loadClusterDetails = async (clusterId: number) => {
    try {
      setLoading(true);
      const data = await api.getClusterWithTracks(clusterId);
      // Validate the response structure
      if (!data || !data.cluster || !data.tracks) {
        console.error('Invalid cluster data structure:', data);
        toast.error('Invalid cluster data received');
        setSelectedCluster(null);
        return;
      }
      setSelectedCluster(data);
    } catch (error) {
      console.error('Failed to load cluster details:', error);
      toast.error('Failed to load cluster details');
      setSelectedCluster(null);
    } finally {
      setLoading(false);
    }
  };

  const getMatchSourceInfo = (cluster: Cluster) => {
    if (cluster.match_locked) {
      return {
        icon: Lock,
        label: 'Manual Match',
        color: 'text-purple-400',
        bg: 'bg-purple-500/10',
      };
    }
    if (cluster.match_source === 'auto_fingerprint') {
      return {
        icon: Zap,
        label: 'Fingerprint Match',
        color: 'text-blue-400',
        bg: 'bg-blue-500/10',
      };
    }
    if (cluster.match_source === 'auto_metadata') {
      return {
        icon: SearchIcon,
        label: 'Metadata Match',
        color: 'text-green-400',
        bg: 'bg-green-500/10',
      };
    }
    return null;
  };

  const getClusteringReasons = (cluster: Cluster) => {
    const reasons = [];
    if (cluster.album) reasons.push({ label: 'Album tag', value: cluster.album });
    if (cluster.album_artist) reasons.push({ label: 'Artist tag', value: cluster.album_artist });
    if (cluster.date) reasons.push({ label: 'Release date', value: cluster.date });
    return reasons;
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
            const matchInfo = getMatchSourceInfo(cluster);
            const isSelected = selectedCluster?.cluster?.id === cluster.id;

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
                      {cluster.mb_release_id ? (
                        <>
                          <CheckCircle2 className="h-3 w-3 text-dark-success" />
                          <span className="text-xs text-dark-success">Matched</span>
                          {cluster.mb_confidence !== null && (
                            <span className="text-xs text-dark-text-tertiary">
                              • {Math.round(cluster.mb_confidence * 100)}%
                            </span>
                          )}
                        </>
                      ) : (
                        <>
                          <AlertCircle className="h-3 w-3 text-dark-error" />
                          <span className="text-xs text-dark-error">Not matched</span>
                        </>
                      )}
                    </div>

                    {/* Match source badge */}
                    {matchInfo && (
                      <div className={`flex items-center gap-1.5 mt-2 text-xs ${matchInfo.color}`}>
                        <matchInfo.icon className="h-3 w-3" />
                        <span>{matchInfo.label}</span>
                      </div>
                    )}
                  </div>

                  <ChevronRight className={`h-4 w-4 text-dark-text-tertiary flex-shrink-0 ${isSelected ? 'text-dark-accent' : ''}`} />
                </div>
              </button>
            );
          })}
        </div>
      </div>

      {/* Right panel - Detail view */}
      <div className="flex-1 flex flex-col bg-dark-bg-elevated border border-dark-border rounded-lg overflow-hidden">
        {loading ? (
          <div className="flex-1 flex items-center justify-center">
            <div className="text-dark-text-secondary">Loading...</div>
          </div>
        ) : selectedCluster ? (
          <>
            {/* Cluster info header */}
            <div className="p-6 border-b border-dark-border">
              <div className="flex items-start gap-4">
                <div className="p-3 bg-dark-bg-subtle rounded-lg">
                  <Disc className="h-8 w-8 text-dark-accent" />
                </div>
                <div className="flex-1">
                  <h2 className="text-2xl font-bold text-dark-text">
                    {selectedCluster.cluster.album || 'Unknown Album'}
                  </h2>
                  <p className="text-dark-text-secondary mt-1">
                    {selectedCluster.cluster.album_artist || 'Unknown Artist'}
                  </p>
                  {selectedCluster.cluster.date && (
                    <p className="text-sm text-dark-text-tertiary mt-1">
                      {selectedCluster.cluster.date}
                    </p>
                  )}
                </div>
              </div>
            </div>

            {/* Scrollable content area */}
            <div className="flex-1 overflow-y-auto p-6 space-y-6">
              {/* Clustering explanation */}
              <div>
                <h3 className="text-sm font-semibold text-dark-text uppercase tracking-wide mb-3">
                  Why These Tracks Were Grouped Together
                </h3>
                <div className="bg-dark-bg-subtle rounded-lg p-4 space-y-2">
                  {getClusteringReasons(selectedCluster.cluster).map((reason, i) => (
                    <div key={i} className="flex items-start gap-2">
                      <CheckCircle2 className="h-4 w-4 text-dark-success flex-shrink-0 mt-0.5" />
                      <div className="flex-1 min-w-0">
                        <span className="text-sm text-dark-text-secondary">{reason.label}:</span>
                        <span className="text-sm text-dark-text ml-2 font-medium">{reason.value}</span>
                      </div>
                    </div>
                  ))}
                  {selectedCluster.tracks.length > 0 && (
                    <div className="flex items-start gap-2">
                      <CheckCircle2 className="h-4 w-4 text-dark-success flex-shrink-0 mt-0.5" />
                      <div>
                        <span className="text-sm text-dark-text-secondary">Track count:</span>
                        <span className="text-sm text-dark-text ml-2 font-medium">
                          {selectedCluster.tracks.length} {selectedCluster.tracks.length === 1 ? 'track' : 'tracks'}
                        </span>
                      </div>
                    </div>
                  )}
                </div>
              </div>

              {/* MusicBrainz match explanation */}
              {selectedCluster.cluster.mb_release_id ? (
                <div>
                  <h3 className="text-sm font-semibold text-dark-text uppercase tracking-wide mb-3">
                    MusicBrainz Match
                  </h3>
                  <div className="bg-dark-bg-subtle rounded-lg p-4 space-y-3">
                    <div>
                      <a
                        href={`https://musicbrainz.org/release/${selectedCluster.cluster.mb_release_id}`}
                        target="_blank"
                        rel="noopener noreferrer"
                        className="text-dark-accent hover:text-dark-accent/80 font-medium flex items-center gap-2"
                      >
                        {selectedCluster.cluster.mb_release_title || 'View on MusicBrainz'}
                        <ExternalLink className="h-4 w-4" />
                      </a>
                      {selectedCluster.cluster.mb_release_country && (
                        <p className="text-sm text-dark-text-tertiary mt-1">
                          {selectedCluster.cluster.mb_release_country}
                          {selectedCluster.cluster.mb_release_date &&
                            ` • ${selectedCluster.cluster.mb_release_date.substring(0, 4)}`}
                        </p>
                      )}
                    </div>

                    {/* Confidence score */}
                    {selectedCluster.cluster.mb_confidence !== null && (
                      <div className="flex items-center gap-2 pt-2 border-t border-dark-border">
                        <span className="text-sm text-dark-text-secondary">Confidence:</span>
                        <div className="flex-1 bg-dark-bg h-2 rounded-full overflow-hidden">
                          <div
                            className={`h-full ${
                              selectedCluster.cluster.mb_confidence >= 0.8
                                ? 'bg-dark-success'
                                : selectedCluster.cluster.mb_confidence >= 0.6
                                ? 'bg-blue-500'
                                : selectedCluster.cluster.mb_confidence >= 0.35
                                ? 'bg-yellow-500'
                                : 'bg-dark-error'
                            }`}
                            style={{ width: `${selectedCluster.cluster.mb_confidence * 100}%` }}
                          />
                        </div>
                        <span className="text-sm font-medium text-dark-text">
                          {Math.round(selectedCluster.cluster.mb_confidence * 100)}%
                        </span>
                      </div>
                    )}

                    {/* Match source */}
                    {(() => {
                      const matchInfo = getMatchSourceInfo(selectedCluster.cluster);
                      if (!matchInfo) return null;

                      return (
                        <div className="pt-2 border-t border-dark-border">
                          <div className="flex items-center gap-2">
                            <span className="text-sm text-dark-text-secondary">Matched via:</span>
                            <div className={`flex items-center gap-1.5 ${matchInfo.color}`}>
                              <matchInfo.icon className="h-4 w-4" />
                              <span className="text-sm font-medium">{matchInfo.label}</span>
                            </div>
                          </div>
                        </div>
                      );
                    })()}
                  </div>
                </div>
              ) : (
                <div>
                  <h3 className="text-sm font-semibold text-dark-text uppercase tracking-wide mb-3">
                    MusicBrainz Match
                  </h3>
                  <div className="bg-dark-bg-subtle rounded-lg p-4 text-center">
                    <AlertCircle className="h-8 w-8 text-dark-error mx-auto mb-2" />
                    <p className="text-sm text-dark-text-secondary">
                      This cluster has not been matched to a MusicBrainz release
                    </p>
                  </div>
                </div>
              )}

              {/* Track listing */}
              <div>
                <h3 className="text-sm font-semibold text-dark-text uppercase tracking-wide mb-3">
                  Tracks
                </h3>
                <div className="space-y-2">
                  {selectedCluster.tracks.length === 0 ? (
                    <div className="bg-dark-bg-subtle rounded-lg p-8 text-center">
                      <Music className="h-8 w-8 text-dark-text-tertiary mx-auto mb-2 opacity-50" />
                      <p className="text-sm text-dark-text-secondary">No tracks in this cluster</p>
                    </div>
                  ) : (
                    selectedCluster.tracks.map((track) => (
                      <div
                        key={track.id}
                        className="flex items-center gap-3 p-3 bg-dark-bg-subtle rounded-lg"
                      >
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
                            {track.duration
                              ? `${Math.floor(track.duration / 60)}:${Math.floor(track.duration % 60)
                                  .toString()
                                  .padStart(2, '0')}`
                              : '--:--'}
                          </div>
                        </div>
                      </div>
                    ))
                  )}
                </div>
              </div>
            </div>
          </>
        ) : (
          <div className="flex-1 flex items-center justify-center">
            <div className="text-center">
              <Disc className="h-12 w-12 text-dark-text-tertiary mx-auto mb-4 opacity-50" />
              <p className="text-dark-text-secondary">Select a cluster to view details</p>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}
