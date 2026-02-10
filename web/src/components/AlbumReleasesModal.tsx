import { useState, useEffect, useRef } from 'react';
import { api } from '../lib/api';
import { formatDate } from '../lib/formatters';
import toast from 'react-hot-toast';
import { CatalogAlbumOverview, AlbumRelease } from '../types/api';
import { X, Disc, Loader2, Download } from 'lucide-react';

interface AlbumReleasesModalProps {
  album: CatalogAlbumOverview;
  onClose: () => void;
}

export function AlbumReleasesModal({ album, onClose }: AlbumReleasesModalProps) {
  const [searchingReleases, setSearchingReleases] = useState(true);
  const [releases, setReleases] = useState<AlbumRelease[]>([]);
  const [searchingSources, setSearchingSources] = useState<Set<string>>(new Set());
  const [queueingRelease, setQueueingRelease] = useState<string | null>(null);
  const eventSourceRef = useRef<EventSource | null>(null);

  useEffect(() => {
    // Track count via ref to avoid stale closure
    let releaseCount = 0;

    // Start streaming search
    const eventSource = api.streamAlbumReleases(album.id, {
      onStarted: (source) => {
        setSearchingSources(prev => new Set([...prev, source]));
      },
      onRelease: (_source, release) => {
        releaseCount++;
        setReleases(prev => [...prev, release]);
      },
      onCompleted: (source, _count) => {
        setSearchingSources(prev => {
          const next = new Set(prev);
          next.delete(source);
          return next;
        });
      },
      onError: (source, error) => {
        console.error(`Search error from ${source}:`, error);
        setSearchingSources(prev => {
          const next = new Set(prev);
          next.delete(source);
          return next;
        });
      },
      onDone: (totalTime) => {
        setSearchingReleases(false);
        setSearchingSources(new Set());
        if (releaseCount === 0) {
          toast('No releases found', { icon: 'i' });
        } else {
          toast.success(`Found ${releaseCount} release(s) in ${totalTime.toFixed(1)}s`);
        }
      },
    });

    eventSourceRef.current = eventSource;

    // Handle connection errors
    eventSource.onerror = () => {
      setSearchingReleases(false);
      setSearchingSources(new Set());
      toast.error('Connection lost while searching');
    };

    // Cleanup on unmount
    return () => {
      if (eventSourceRef.current) {
        eventSourceRef.current.close();
        eventSourceRef.current = null;
      }
    };
  }, [album.id]);

  const handleQueueDownload = async (release: AlbumRelease) => {
    if (queueingRelease !== null) return;
    setQueueingRelease(release.download_url);
    try {
      const response = await api.queueDownload({
        catalog_album_id: album.id,
        indexer_name: release.source,
        url: release.download_url,
        title: release.title,
        size_bytes: release.size,
        quality: release.quality,
        format: release.download_type.toUpperCase(),
        seeders: release.seeders,
        slskd_username: release.slskd_username,
        slskd_files: release.slskd_files,
      });

      if (response.success) {
        toast.success('Download queued successfully');
        onClose();
      } else {
        toast.error(response.message || 'Failed to queue download');
      }
    } catch (error) {
      console.error('Failed to queue download:', error);
      toast.error('Failed to queue download');
    } finally {
      setQueueingRelease(null);
    }
  };

  return (
    <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4">
      <div className="bg-dark-bg-elevated rounded-lg shadow-xl max-w-4xl w-full max-h-[80vh] overflow-hidden flex flex-col">
        {/* Modal Header */}
        <div className="px-6 py-4 border-b border-dark-border flex items-center justify-between">
          <div>
            <h2 className="text-xl font-bold text-dark-text">Available Releases</h2>
            <p className="text-sm text-dark-text-secondary mt-1">
              {album.title} - {album.artist_name}
            </p>
          </div>
          <button
            onClick={onClose}
            className="text-dark-text-secondary hover:text-dark-text transition-colors"
          >
            <X className="h-6 w-6" />
          </button>
        </div>

        {/* Modal Body */}
        <div className="flex-1 overflow-y-auto p-6">
          {/* Search status bar */}
          {searchingReleases && (
            <div className="mb-4 flex items-center gap-2 text-sm text-dark-text-secondary">
              <Loader2 className="h-4 w-4 animate-spin text-dark-accent" />
              <span>Searching</span>
              {searchingSources.size > 0 && (
                <span className="flex gap-1">
                  {[...searchingSources].map(source => (
                    <span key={source} className="px-2 py-0.5 bg-dark-bg rounded text-xs">
                      {source}
                    </span>
                  ))}
                </span>
              )}
              {releases.length > 0 && (
                <span className="ml-2">- {releases.length} found</span>
              )}
            </div>
          )}

          {/* No results (only show after search is done) */}
          {!searchingReleases && releases.length === 0 ? (
            <div className="text-center py-12">
              <Disc className="mx-auto h-12 w-12 text-dark-text-tertiary" />
              <h3 className="mt-4 text-sm font-medium text-dark-text">No releases found</h3>
              <p className="mt-2 text-sm text-dark-text-secondary">
                No releases available for this album at the moment
              </p>
            </div>
          ) : (
            <div className="space-y-3">
              {releases.map((release, index) => (
                <div
                  key={index}
                  className="card p-4 hover:bg-dark-bg-hover transition-colors"
                >
                  <div className="flex items-start justify-between gap-4">
                    <div className="flex-1 min-w-0">
                      <h3 className="text-sm font-medium text-dark-text mb-2">{release.title}</h3>
                      <div className="flex flex-wrap items-center gap-x-4 gap-y-1 text-xs text-dark-text-secondary">
                        <span className="flex items-center gap-1">
                          <span className="text-dark-text-tertiary">Source:</span>
                          <span className="font-medium">{release.source}</span>
                        </span>
                        <span className="flex items-center gap-1">
                          <span className="text-dark-text-tertiary">Quality:</span>
                          <span className="font-medium text-dark-accent">{release.quality}</span>
                        </span>
                        {release.size && (
                          <span className="flex items-center gap-1">
                            <span className="text-dark-text-tertiary">Size:</span>
                            <span className="font-medium">
                              {release.size >= 1024 * 1024 * 1024
                                ? `${(release.size / 1024 / 1024 / 1024).toFixed(2)} GB`
                                : `${(release.size / 1024 / 1024).toFixed(0)} MB`}
                            </span>
                          </span>
                        )}
                        {release.download_type === 'torrent' && release.seeders !== null && release.seeders !== undefined && (
                          <span className="flex items-center gap-1">
                            <span className="text-dark-text-tertiary">Seeders:</span>
                            <span className={`font-medium ${release.seeders > 0 ? 'text-green-400' : 'text-red-400'}`}>
                              {release.seeders}
                            </span>
                          </span>
                        )}
                        {release.download_type === 'torrent' && release.peers !== null && release.peers !== undefined && (
                          <span className="flex items-center gap-1">
                            <span className="text-dark-text-tertiary">Peers:</span>
                            <span className="font-medium">{release.peers}</span>
                          </span>
                        )}
                        {release.publish_date && (
                          <span className="flex items-center gap-1">
                            <span className="text-dark-text-tertiary">Uploaded:</span>
                            <span className="font-medium">{formatDate(release.publish_date)}</span>
                          </span>
                        )}
                        <span className="flex items-center gap-1">
                          <span className="text-dark-text-tertiary">Type:</span>
                          <span className="uppercase text-xs font-mono font-medium">{release.download_type}</span>
                        </span>
                      </div>
                    </div>
                    <button
                      className={`btn-primary px-4 py-2 flex-shrink-0 ${
                        queueingRelease === release.download_url ? 'opacity-50 cursor-not-allowed' : ''
                      }`}
                      disabled={queueingRelease === release.download_url}
                      onClick={() => handleQueueDownload(release)}
                    >
                      {queueingRelease === release.download_url ? (
                        <>
                          <Loader2 className="h-4 w-4 mr-2 animate-spin" />
                          Queuing...
                        </>
                      ) : (
                        <>
                          <Download className="h-4 w-4 mr-2" />
                          Download
                        </>
                      )}
                    </button>
                  </div>
                </div>
              ))}
            </div>
          )}
        </div>

        {/* Modal Footer */}
        <div className="px-6 py-4 border-t border-dark-border flex justify-end">
          <button
            onClick={onClose}
            className="btn-secondary"
          >
            Close
          </button>
        </div>
      </div>
    </div>
  );
}
