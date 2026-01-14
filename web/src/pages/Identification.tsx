import { useState, useEffect } from 'react';
import { useLocation } from 'react-router-dom';
import { Cluster, CandidateRelease } from '../types/api';
import { api } from '../lib/api';
import toast from 'react-hot-toast';
import { IdentificationNav } from '../components/IdentificationNav';
import Tracks from './Tracks';
import { Disc, ExternalLink, X, RefreshCw, Loader2, ChevronLeft, ChevronRight } from 'lucide-react';

const ITEMS_PER_PAGE = 50;

function ClustersView() {
  const [clusters, setClusters] = useState<Cluster[]>([]);
  const [loading, setLoading] = useState(true);
  const [selectedCluster, setSelectedCluster] = useState<Cluster | null>(null);
  const [candidates, setCandidates] = useState<CandidateRelease[]>([]);
  const [loadingCandidates, setLoadingCandidates] = useState(false);
  const [assigningRelease, setAssigningRelease] = useState(false);
  const [offset, setOffset] = useState(0);
  const [totalCount, setTotalCount] = useState(0);

  useEffect(() => {
    loadClusters();
  }, [offset]);

  useEffect(() => {
    if (selectedCluster) {
      loadCandidates(selectedCluster.id);
    } else {
      setCandidates([]);
    }
  }, [selectedCluster]);

  const loadClusters = async () => {
    try {
      setLoading(true);
      const response = await api.getClusters(offset, ITEMS_PER_PAGE);
      setClusters(response.clusters);
      setTotalCount(response.pagination.total);
    } catch (error) {
      console.error('Failed to load clusters:', error);
      toast.error('Failed to load clusters');
    } finally {
      setLoading(false);
    }
  };

  const loadCandidates = async (clusterId: number) => {
    try {
      setLoadingCandidates(true);
      const data = await api.getCandidateReleases(clusterId);
      setCandidates(data);
    } catch (error) {
      console.error('Failed to load candidates:', error);
      toast.error('Failed to load candidate releases');
    } finally {
      setLoadingCandidates(false);
    }
  };

  const assignCandidate = async (releaseId: string) => {
    if (!selectedCluster) return;

    try {
      setAssigningRelease(true);
      const updatedCluster = await api.assignRelease(selectedCluster.id, releaseId);

      // Update the cluster in the list
      setClusters(clusters.map(c => c.id === updatedCluster.id ? updatedCluster : c));
      setSelectedCluster(updatedCluster);

      toast.success('Release assigned successfully');
    } catch (error) {
      console.error('Failed to assign release:', error);
      toast.error('Failed to assign release');
    } finally {
      setAssigningRelease(false);
    }
  };

  const formatConfidence = (confidence: number | null) => {
    if (confidence === null) return 'N/A';
    return `${Math.round(confidence * 100)}%`;
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center h-64">
        <Loader2 className="w-12 h-12 text-dark-accent animate-spin" />
      </div>
    );
  }

  return (
    <div className="space-y-6 animate-fade-in">
      <div>
        <h1 className="text-3xl font-bold text-dark-text">Album Identification</h1>
        <p className="text-dark-text-secondary mt-2">
          Manage MusicBrainz matches for your album clusters
        </p>
      </div>

      {clusters.length === 0 ? (
        <div className="card p-12 text-center">
          <Disc className="mx-auto h-12 w-12 text-dark-text-tertiary" />
          <h3 className="mt-4 text-sm font-medium text-dark-text">No clusters found</h3>
          <p className="mt-2 text-sm text-dark-text-secondary">
            Run a library scan to create album clusters.
          </p>
        </div>
      ) : (
        <div className="card overflow-hidden">
          <div className="overflow-x-auto">
            <table className="min-w-full divide-y divide-dark-border">
              <thead className="bg-dark-bg-subtle">
                <tr>
                  <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">
                    Album
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">
                    Artist
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">
                    Tracks
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">
                    MusicBrainz Match
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">
                    Confidence
                  </th>
                  <th className="px-6 py-3 text-right text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">
                    Actions
                  </th>
                </tr>
              </thead>
              <tbody className="divide-y divide-dark-border">
              {clusters.map((cluster) => (
                <tr
                  key={cluster.id}
                  className="hover:bg-dark-bg-hover cursor-pointer transition-colors duration-150"
                  onClick={() => setSelectedCluster(cluster)}
                >
                  <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-dark-text">
                    {cluster.album || <span className="text-dark-text-tertiary italic">Unknown Album</span>}
                  </td>
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-dark-text-secondary">
                    {cluster.album_artist || <span className="text-dark-text-tertiary italic">Unknown Artist</span>}
                  </td>
                  <td className="px-6 py-4 whitespace-nowrap text-sm text-dark-text-secondary">
                    {cluster.track_count}
                  </td>
                  <td className="px-6 py-4 text-sm text-dark-text-secondary">
                    {cluster.mb_release_id ? (
                      <div className="space-y-1">
                        <a
                          href={`https://musicbrainz.org/release/${cluster.mb_release_id}`}
                          target="_blank"
                          rel="noopener noreferrer"
                          className="link flex items-center gap-1"
                          onClick={(e) => e.stopPropagation()}
                        >
                          {cluster.mb_release_title || 'View on MusicBrainz'}
                          <ExternalLink className="h-3 w-3" />
                        </a>
                        {cluster.mb_release_country && (
                          <div className="text-xs text-dark-text-tertiary">
                            {cluster.mb_release_country}
                            {cluster.mb_release_date && ` • ${cluster.mb_release_date.substring(0, 4)}`}
                          </div>
                        )}
                        {(cluster.album && cluster.mb_release_title && cluster.album !== cluster.mb_release_title) && (
                          <span className="text-xs text-yellow-500" title="Title differs from your files">⚠ Different title</span>
                        )}
                      </div>
                    ) : (
                      <span className="text-dark-text-tertiary italic">Not matched</span>
                    )}
                  </td>
                  <td className="px-6 py-4 whitespace-nowrap">
                    {cluster.mb_confidence !== null ? (
                      <span className={`px-2 inline-flex text-xs leading-5 font-semibold rounded-full ${
                        cluster.mb_confidence >= 0.8
                          ? 'bg-dark-success-muted text-dark-success border border-dark-success/30'
                          : cluster.mb_confidence >= 0.6
                          ? 'bg-blue-500/10 text-blue-400 border border-blue-500/30'
                          : cluster.mb_confidence >= 0.35
                          ? 'bg-yellow-500/10 text-yellow-400 border border-yellow-500/30'
                          : 'bg-dark-error/10 text-dark-error border border-dark-error/30'
                      }`}>
                        {formatConfidence(cluster.mb_confidence)}
                      </span>
                    ) : (
                      <span className="text-dark-text-tertiary italic">N/A</span>
                    )}
                  </td>
                  <td className="px-6 py-4 whitespace-nowrap text-right text-sm font-medium">
                    <button
                      onClick={(e) => {
                        e.stopPropagation();
                        setSelectedCluster(cluster);
                      }}
                      className="link"
                    >
                      Manage
                    </button>
                  </td>
                </tr>
              ))}
              </tbody>
            </table>
          </div>

          {/* Pagination */}
          {totalCount > ITEMS_PER_PAGE && (
            <div className="px-6 py-4 border-t border-dark-border flex items-center justify-between">
              <div className="text-sm text-dark-text-secondary">
                Showing {offset + 1}-{Math.min(offset + ITEMS_PER_PAGE, totalCount)} of {totalCount}
              </div>
              <div className="flex gap-2">
                <button
                  onClick={() => setOffset((o) => Math.max(0, o - ITEMS_PER_PAGE))}
                  disabled={offset === 0}
                  className="btn-secondary px-3 py-2 disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  <ChevronLeft size={16} />
                </button>
                <button
                  onClick={() => setOffset((o) => Math.min(totalCount - ITEMS_PER_PAGE, o + ITEMS_PER_PAGE))}
                  disabled={offset + ITEMS_PER_PAGE >= totalCount}
                  className="btn-secondary px-3 py-2 disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  <ChevronRight size={16} />
                </button>
              </div>
            </div>
          )}
        </div>
      )}

      {/* Cluster detail panel */}
      {selectedCluster && (
        <div className="fixed inset-0 bg-black/50 backdrop-blur-sm flex items-center justify-center p-4 z-50">
          <div className="bg-dark-bg-elevated rounded-lg shadow-xl max-w-4xl w-full max-h-[90vh] overflow-hidden border border-dark-border">
            <div className="px-6 py-4 border-b border-dark-border flex justify-between items-center">
              <h2 className="text-lg font-medium text-dark-text flex items-center gap-2">
                <Disc className="h-5 w-5 text-dark-accent" />
                {selectedCluster.album || 'Unknown Album'}
              </h2>
              <button
                onClick={() => setSelectedCluster(null)}
                className="text-dark-text-tertiary hover:text-dark-text transition-colors p-1 rounded-lg hover:bg-dark-bg-hover"
              >
                <X className="h-6 w-6" />
              </button>
            </div>
            <div className="px-6 py-4 max-h-[calc(90vh-200px)] overflow-y-auto">
              <div className="space-y-6">
                {/* Library Cluster Data - used for identification */}
                <div>
                  <div className="mb-3">
                    <h3 className="text-lg font-medium text-dark-text">Library Cluster Data</h3>
                    <p className="text-xs text-dark-text-secondary mt-1">
                      Metadata from your files used to identify this album on MusicBrainz
                    </p>
                  </div>
                  <dl className="grid grid-cols-1 gap-3 sm:grid-cols-2">
                    <div>
                      <dt className="text-sm font-medium text-dark-text-secondary">Album</dt>
                      <dd className="mt-1 text-sm text-dark-text">{selectedCluster.album || <span className="text-dark-text-tertiary italic">Unknown</span>}</dd>
                    </div>
                    <div>
                      <dt className="text-sm font-medium text-dark-text-secondary">Album Artist</dt>
                      <dd className="mt-1 text-sm text-dark-text">{selectedCluster.album_artist || <span className="text-dark-text-tertiary italic">Unknown</span>}</dd>
                    </div>
                    <div>
                      <dt className="text-sm font-medium text-dark-text-secondary">Track Count</dt>
                      <dd className="mt-1 text-sm text-dark-text">{selectedCluster.track_count}</dd>
                    </div>
                    {selectedCluster.label && (
                      <div>
                        <dt className="text-sm font-medium text-dark-text-secondary">Label</dt>
                        <dd className="mt-1 text-sm text-dark-text">{selectedCluster.label}</dd>
                      </div>
                    )}
                    {selectedCluster.catalog_number && (
                      <div>
                        <dt className="text-sm font-medium text-dark-text-secondary">Catalog Number</dt>
                        <dd className="mt-1 text-sm text-dark-text">{selectedCluster.catalog_number}</dd>
                      </div>
                    )}
                    {selectedCluster.barcode && (
                      <div>
                        <dt className="text-sm font-medium text-dark-text-secondary">Barcode</dt>
                        <dd className="mt-1 text-sm text-dark-text">{selectedCluster.barcode}</dd>
                      </div>
                    )}
                    {selectedCluster.country && (
                      <div>
                        <dt className="text-sm font-medium text-dark-text-secondary">Country</dt>
                        <dd className="mt-1 text-sm text-dark-text">{selectedCluster.country}</dd>
                      </div>
                    )}
                    {selectedCluster.date && (
                      <div>
                        <dt className="text-sm font-medium text-dark-text-secondary">Date</dt>
                        <dd className="mt-1 text-sm text-dark-text">{selectedCluster.date}</dd>
                      </div>
                    )}
                    {selectedCluster.year && (
                      <div>
                        <dt className="text-sm font-medium text-dark-text-secondary">Year</dt>
                        <dd className="mt-1 text-sm text-dark-text">{selectedCluster.year}</dd>
                      </div>
                    )}
                  </dl>
                </div>

                {selectedCluster.mb_release_id ? (
                  <>
                    {/* Match Confidence */}
                    <div className="border-t border-dark-border pt-4">
                      <div className="flex items-center justify-between mb-3">
                        <h3 className="text-lg font-medium text-dark-text">MusicBrainz Match</h3>
                        <span className={`px-3 py-1 text-sm font-semibold rounded-full ${
                          (selectedCluster.mb_confidence || 0) >= 0.8
                            ? 'bg-dark-success-muted text-dark-success border border-dark-success/30'
                            : (selectedCluster.mb_confidence || 0) >= 0.6
                            ? 'bg-blue-500/10 text-blue-400 border border-blue-500/30'
                            : (selectedCluster.mb_confidence || 0) >= 0.35
                            ? 'bg-yellow-500/10 text-yellow-400 border border-yellow-500/30'
                            : 'bg-dark-error/10 text-dark-error border border-dark-error/30'
                        }`}>
                          {formatConfidence(selectedCluster.mb_confidence)} confidence
                        </span>
                      </div>

                      {/* MusicBrainz Release Details */}
                      <dl className="grid grid-cols-1 gap-3 sm:grid-cols-2">
                        <div>
                          <dt className="text-sm font-medium text-dark-text-secondary">Release Title</dt>
                          <dd className="mt-1 text-sm text-dark-text">
                            {selectedCluster.mb_release_title || <span className="text-dark-text-tertiary italic">N/A</span>}
                            {selectedCluster.album && selectedCluster.mb_release_title && selectedCluster.album !== selectedCluster.mb_release_title && (
                              <span className="ml-2 text-yellow-500" title="Different from your files">⚠</span>
                            )}
                          </dd>
                        </div>
                        <div>
                          <dt className="text-sm font-medium text-dark-text-secondary">Release Artist</dt>
                          <dd className="mt-1 text-sm text-dark-text">
                            {selectedCluster.mb_release_artist || <span className="text-dark-text-tertiary italic">N/A</span>}
                            {selectedCluster.album_artist && selectedCluster.mb_release_artist && selectedCluster.album_artist !== selectedCluster.mb_release_artist && (
                              <span className="ml-2 text-yellow-500" title="Different from your files">⚠</span>
                            )}
                          </dd>
                        </div>
                        <div>
                          <dt className="text-sm font-medium text-dark-text-secondary">Country</dt>
                          <dd className="mt-1 text-sm text-dark-text">{selectedCluster.mb_release_country || <span className="text-dark-text-tertiary italic">N/A</span>}</dd>
                        </div>
                        <div>
                          <dt className="text-sm font-medium text-dark-text-secondary">Date</dt>
                          <dd className="mt-1 text-sm text-dark-text">{selectedCluster.mb_release_date || <span className="text-dark-text-tertiary italic">N/A</span>}</dd>
                        </div>
                        <div>
                          <dt className="text-sm font-medium text-dark-text-secondary">Label</dt>
                          <dd className="mt-1 text-sm text-dark-text">{selectedCluster.mb_release_label || <span className="text-dark-text-tertiary italic">N/A</span>}</dd>
                        </div>
                        <div>
                          <dt className="text-sm font-medium text-dark-text-secondary">Catalog Number</dt>
                          <dd className="mt-1 text-sm text-dark-text">{selectedCluster.mb_release_catalog_number || <span className="text-dark-text-tertiary italic">N/A</span>}</dd>
                        </div>
                        <div>
                          <dt className="text-sm font-medium text-dark-text-secondary">Barcode</dt>
                          <dd className="mt-1 text-sm text-dark-text">{selectedCluster.mb_release_barcode || <span className="text-dark-text-tertiary italic">N/A</span>}</dd>
                        </div>
                        <div>
                          <dt className="text-sm font-medium text-dark-text-secondary">Release ID</dt>
                          <dd className="mt-1 text-sm">
                            <a
                              href={`https://musicbrainz.org/release/${selectedCluster.mb_release_id}`}
                              target="_blank"
                              rel="noopener noreferrer"
                              className="link font-mono text-xs inline-flex items-center gap-1"
                            >
                              {selectedCluster.mb_release_id}
                              <ExternalLink className="h-3 w-3" />
                            </a>
                          </dd>
                        </div>
                      </dl>
                    </div>

                    {/* Info about matching */}
                    <div className="bg-blue-500/10 border border-blue-500/30 rounded-lg p-4">
                      <h4 className="text-sm font-medium text-blue-400 mb-2">How Matching Works</h4>
                      <p className="text-xs text-dark-text-secondary mb-2">
                        MusicBrainz identification uses a weighted scoring algorithm to find the best match:
                      </p>
                      <ul className="text-xs text-dark-text-secondary space-y-1 mb-3">
                        <li><strong>Primary factors:</strong> Album title (10x weight) and artist (8x weight)</li>
                        <li><strong>Filter rules:</strong> Barcode (exact match), Country, Label (fuzzy match), Track count (release must have ≥ your tracks)</li>
                        <li><strong>Secondary factors:</strong> Track count match (3x), Catalog number (1x), Date (0.5x)</li>
                        <li><strong>Track matching:</strong> Individual tracks matched using title (5x), duration (3x), and position (1x)</li>
                      </ul>
                      <div className="text-xs text-dark-text-secondary border-t border-blue-500/30 pt-2">
                        <strong>Confidence guide:</strong>
                        <ul className="mt-1 space-y-1">
                          <li>• <strong>80-100%:</strong> Excellent match - very likely correct</li>
                          <li>• <strong>60-79%:</strong> Good match - probably correct (minor differences)</li>
                          <li>• <strong>35-59%:</strong> Fair match - review carefully (may need manual selection)</li>
                          <li>• <strong>&lt;35%:</strong> Poor match - automatic matching rejected</li>
                        </ul>
                      </div>
                    </div>
                  </>
                ) : (
                  <div className="border-t border-dark-border pt-4">
                    <div className="bg-dark-bg-subtle border border-dark-border rounded-lg p-4 text-center">
                      <p className="text-sm text-dark-text-secondary">No MusicBrainz match found yet.</p>
                      <p className="text-xs text-dark-text-tertiary mt-1">Try running identification again or manually assign a release.</p>
                    </div>
                  </div>
                )}

                {/* Alternative Releases / Candidates */}
                <div className="border-t border-dark-border pt-4">
                  <h3 className="text-lg font-medium text-dark-text mb-3">Alternative Releases</h3>

                  {loadingCandidates ? (
                    <div className="flex justify-center py-8">
                      <Loader2 className="w-8 h-8 text-dark-accent animate-spin" />
                    </div>
                  ) : candidates.length > 0 ? (
                    <div className="space-y-3">
                      <p className="text-sm text-dark-text-secondary mb-3">
                        Select a different release if the automatic match is incorrect:
                      </p>
                      {candidates.map((candidate) => (
                        <div
                          key={candidate.release_id}
                          className={`border rounded-lg p-4 hover:bg-dark-bg-hover transition-colors ${
                            selectedCluster.mb_release_id === candidate.release_id
                              ? 'border-dark-accent bg-dark-accent/10'
                              : 'border-dark-border'
                          }`}
                        >
                          <div className="flex justify-between items-start">
                            <div className="flex-1">
                              <div className="flex items-center gap-2 mb-1">
                                <h4 className="font-medium text-dark-text">{candidate.title}</h4>
                                {selectedCluster.mb_release_id === candidate.release_id && (
                                  <span className="px-2 py-0.5 text-xs font-semibold rounded-full bg-dark-accent text-dark-bg">
                                    Current
                                  </span>
                                )}
                              </div>
                              <p className="text-sm text-dark-text-secondary mb-2">{candidate.artist}</p>
                              <div className="flex flex-wrap gap-3 text-xs text-dark-text-tertiary">
                                {candidate.country && (
                                  <span>{candidate.country}</span>
                                )}
                                {candidate.date && (
                                  <span>{candidate.date.substring(0, 4)}</span>
                                )}
                                <span>{candidate.track_count} tracks</span>
                              </div>
                            </div>
                            <div className="flex flex-col items-end gap-2">
                              <a
                                href={`https://musicbrainz.org/release/${candidate.release_id}`}
                                target="_blank"
                                rel="noopener noreferrer"
                                className="link text-xs inline-flex items-center gap-1"
                                onClick={(e) => e.stopPropagation()}
                              >
                                View on MB
                                <ExternalLink className="h-3 w-3" />
                              </a>
                              {selectedCluster.mb_release_id !== candidate.release_id && (
                                <button
                                  onClick={() => assignCandidate(candidate.release_id)}
                                  disabled={assigningRelease}
                                  className="px-3 py-1.5 text-xs font-medium text-dark-bg bg-dark-accent hover:bg-dark-accent/80 rounded-lg disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
                                >
                                  {assigningRelease ? 'Assigning...' : 'Use This'}
                                </button>
                              )}
                            </div>
                          </div>
                        </div>
                      ))}
                    </div>
                  ) : (
                    <div className="bg-dark-bg-subtle border border-dark-border rounded-lg p-4 text-center">
                      <p className="text-sm text-dark-text-secondary">No alternative releases cached.</p>
                      <p className="text-xs text-dark-text-tertiary mt-1">
                        Alternatives are only available after a successful identification.
                      </p>
                    </div>
                  )}
                </div>
              </div>
            </div>
            <div className="px-6 py-4 bg-dark-bg-subtle border-t border-dark-border flex justify-end space-x-3">
              <button
                onClick={() => setSelectedCluster(null)}
                className="px-4 py-2 border border-dark-border rounded-lg text-sm font-medium text-dark-text bg-dark-bg-elevated hover:bg-dark-bg-hover transition-colors"
              >
                Close
              </button>
              <button
                onClick={() => {
                  // TODO: Implement re-identify
                  toast.success('Re-identify feature coming soon');
                }}
                className="px-4 py-2 rounded-lg text-sm font-medium text-dark-bg bg-dark-accent hover:bg-dark-accent/80 transition-colors flex items-center gap-2"
              >
                <RefreshCw className="h-4 w-4" />
                Re-identify
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}

export default function Identification() {
  const location = useLocation();
  const isTracksView = location.pathname === '/identification/tracks';

  return (
    <div className="space-y-6 animate-fade-in">
      <IdentificationNav />
      {isTracksView ? <Tracks /> : <ClustersView />}
    </div>
  );
}
