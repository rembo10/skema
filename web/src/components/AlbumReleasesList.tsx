import { useState, useEffect, useRef, useCallback } from 'react';
import { api } from '../lib/api';
import { formatDate } from '../lib/formatters';
import toast from 'react-hot-toast';
import { AlbumRelease } from '../types/api';
import { Disc, Loader2, Download, RefreshCw, Search as SearchIcon } from 'lucide-react';

interface AlbumReleasesListProps {
  albumId: number;
  albumTitle: string;
  artistName: string;
  // When the cache is empty on open, automatically kick off a live search.
  // Used by the modal (opened explicitly to find releases); the always-visible
  // detail-page section leaves it off so visiting an album never auto-hits
  // indexers — the user searches on demand via the Refresh button.
  autoSearchWhenEmpty?: boolean;
  // Called after a release has been queued successfully
  onQueued?: () => void;
}

/**
 * Shows available download releases for an album. On open it renders the cached
 * releases (populated by prior searches and the RSS monitor) instantly, and a
 * "Refresh" button runs a live streaming search across all configured
 * indexers/slskd — which also refreshes the cache. Shared by the Albums table
 * modal and the album detail page.
 */
export function AlbumReleasesList({ albumId, albumTitle, artistName, autoSearchWhenEmpty = false, onQueued }: AlbumReleasesListProps) {
  const [loadingCache, setLoadingCache] = useState(true);
  const [searching, setSearching] = useState(false);
  const [releases, setReleases] = useState<AlbumRelease[]>([]);
  const [searchingSources, setSearchingSources] = useState<Set<string>>(new Set());
  const [queueingRelease, setQueueingRelease] = useState<string | null>(null);
  const eventSourceRef = useRef<EventSource | null>(null);

  // Merge a streamed release into the list, deduplicated by download URL.
  const mergeRelease = useCallback((release: AlbumRelease) => {
    setReleases(prev => {
      const idx = prev.findIndex(r => r.download_url === release.download_url);
      if (idx >= 0) {
        const next = [...prev];
        next[idx] = release;
        return next;
      }
      return [...prev, release];
    });
  }, []);

  // Start a live streaming search across indexers (also refreshes the cache).
  const startSearch = useCallback(() => {
    if (eventSourceRef.current) return; // already searching
    let found = 0;
    setSearching(true);

    const eventSource = api.streamAlbumReleases(albumId, {
      onStarted: (source) => {
        setSearchingSources(prev => new Set([...prev, source]));
      },
      onRelease: (_source, release) => {
        found++;
        mergeRelease(release);
      },
      onCompleted: (source) => {
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
        setSearching(false);
        setSearchingSources(new Set());
        eventSourceRef.current = null;
        if (found === 0) {
          toast('No new releases found', { icon: 'ℹ️' });
        } else {
          toast.success(`Found ${found} release(s) in ${totalTime.toFixed(1)}s`);
        }
      },
    });

    eventSourceRef.current = eventSource;

    eventSource.onerror = () => {
      setSearching(false);
      setSearchingSources(new Set());
      eventSourceRef.current = null;
      toast.error('Connection lost while searching');
    };
  }, [albumId, mergeRelease]);

  // On open: load cached releases instantly; if the cache is empty, kick off a
  // live search automatically so the first visit still returns something.
  useEffect(() => {
    let cancelled = false;
    setLoadingCache(true);
    setReleases([]);

    api.getAlbumReleases(albumId)
      .then(res => {
        if (cancelled) return;
        setReleases(res.releases);
        setLoadingCache(false);
        if (res.releases.length === 0 && autoSearchWhenEmpty) {
          startSearch();
        }
      })
      .catch(err => {
        if (cancelled) return;
        console.error('Failed to load cached releases:', err);
        setLoadingCache(false);
        if (autoSearchWhenEmpty) {
          startSearch();
        }
      });

    return () => {
      cancelled = true;
      if (eventSourceRef.current) {
        eventSourceRef.current.close();
        eventSourceRef.current = null;
      }
    };
  }, [albumId, startSearch, autoSearchWhenEmpty]);

  const handleQueueDownload = async (release: AlbumRelease) => {
    if (queueingRelease !== null) return;
    setQueueingRelease(release.download_url);
    try {
      const response = await api.queueDownload({
        catalog_album_id: albumId,
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
        onQueued?.();
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
    <div>
      {/* Header: refresh control + search status */}
      <div className="mb-4 flex items-center justify-between gap-2">
        <div className="flex items-center gap-2 text-sm text-dark-text-secondary min-h-[2rem]">
          {loadingCache ? (
            <>
              <Loader2 className="h-4 w-4 animate-spin text-dark-accent" />
              <span>Loading releases</span>
            </>
          ) : searching ? (
            <>
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
              {releases.length > 0 && <span className="ml-2">- {releases.length} shown</span>}
            </>
          ) : releases.length > 0 ? (
            <span>{releases.length} release(s) cached</span>
          ) : null}
        </div>

        <button
          onClick={startSearch}
          disabled={searching || loadingCache}
          className="flex items-center gap-2 px-3 py-1.5 bg-dark-bg-hover hover:bg-dark-accent-muted text-dark-text-secondary hover:text-dark-accent rounded transition-colors text-sm disabled:opacity-50 disabled:cursor-not-allowed"
          title="Search indexers for new releases"
        >
          <RefreshCw className={`h-4 w-4 ${searching ? 'animate-spin' : ''}`} />
          {searching ? 'Searching…' : 'Refresh'}
        </button>
      </div>

      {/* No results (only show once the cache has loaded and no search is running) */}
      {!loadingCache && !searching && releases.length === 0 ? (
        <div className="text-center py-12">
          <Disc className="mx-auto h-12 w-12 text-dark-text-tertiary" />
          <h3 className="mt-4 text-sm font-medium text-dark-text">No releases cached</h3>
          <p className="mt-2 text-sm text-dark-text-secondary">
            No releases cached for {albumTitle} by {artistName} yet.
          </p>
          <button
            onClick={startSearch}
            className="mt-4 inline-flex items-center gap-2 px-4 py-2 bg-dark-bg-hover hover:bg-dark-accent-muted text-dark-text-secondary hover:text-dark-accent rounded transition-colors text-sm"
          >
            <SearchIcon className="h-4 w-4" />
            Search indexers
          </button>
        </div>
      ) : (
        <div className="space-y-3">
          {releases.map((release, index) => (
            <div
              key={release.download_url || index}
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
  );
}
