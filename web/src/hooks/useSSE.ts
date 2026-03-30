import { useEffect, useRef } from 'react';
import toast from 'react-hot-toast';
import { getJWT, setAuthRequired, getApiBase } from '../lib/api';
import { useAppStore } from '../store';
import { emitSSEEvent } from './useSSEEvent';

/**
 * Hook to connect to SSE endpoint and keep state updated.
 * Should be used at the App level to maintain a single connection.
 */
export function useSSE(enabled: boolean = true) {
  const eventSourceRef = useRef<EventSource | null>(null);
  const reconnectTimeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null);
  const shouldReconnectRef = useRef(true);
  const statusTimeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null);
  const {
    setCurrentStatus,
    setConnectionStatus,
    setAuthEnabled,
  } = useAppStore();

  useEffect(() => {
    // Don't connect if disabled (e.g., on login page)
    if (!enabled) {
      setConnectionStatus('disconnected');
      return;
    }

    // Re-enable reconnection when component becomes enabled (e.g., after login)
    shouldReconnectRef.current = true;

    let mounted = true;

    // Listen for unauthorized events and stop reconnection attempts
    const handleUnauthorized = () => {
      shouldReconnectRef.current = false;

      // Close existing connection
      if (eventSourceRef.current) {
        eventSourceRef.current.close();
        eventSourceRef.current = null;
      }

      // Clear any pending reconnection
      if (reconnectTimeoutRef.current) {
        clearTimeout(reconnectTimeoutRef.current);
        reconnectTimeoutRef.current = null;
      }
    };

    window.addEventListener('unauthorized', handleUnauthorized);

    function connect() {
      // Don't connect if we've been told to stop (e.g., due to 401)
      if (!shouldReconnectRef.current) {
        return;
      }

      // Get JWT token for SSE authentication (may be null if auth is disabled)
      const jwt = getJWT();
      const apiBase = getApiBase();

      // Connect to SSE (with or without token)
      // Use window.location.origin to ensure absolute URL (fixes relative path issues)
      const url = jwt
        ? `${window.location.origin}${apiBase}/events?token=${encodeURIComponent(jwt)}`
        : `${window.location.origin}${apiBase}/events`;
      setConnectionStatus('connecting');
      const eventSource = new EventSource(url);
      eventSourceRef.current = eventSource;

      eventSource.onopen = () => {
        setConnectionStatus('connected');
      };

      eventSource.onerror = (error) => {
        console.error('SSE connection error:', error);
        setConnectionStatus('error');
        eventSource.close();
        eventSourceRef.current = null;

        // Only reconnect if we haven't been told to stop
        if (mounted && shouldReconnectRef.current) {
          reconnectTimeoutRef.current = setTimeout(() => {
            emitSSEEvent('SSEReconnected', {});
            connect();
          }, 5000);
        }
      };

      // Helper to auto-clear status after a delay
      const setStatusWithTimeout = (status: { type: 'in_progress' | 'success' | 'error'; message: string; progress?: { current: number; total: number } }, clearAfterMs: number = 5000) => {
        setCurrentStatus(status);

        if (statusTimeoutRef.current) {
          clearTimeout(statusTimeoutRef.current);
        }

        statusTimeoutRef.current = setTimeout(() => {
          setCurrentStatus(null);
        }, clearAfterMs);
      };

      // Library scan events
      eventSource.addEventListener('LibraryScanRequested', (_e: MessageEvent) => {
        setCurrentStatus({
          type: 'in_progress',
          message: 'Starting library scan...',
        });
      });

      eventSource.addEventListener('MetadataReadStarted', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        setCurrentStatus({
          type: 'in_progress',
          message: `Reading metadata from ${data.files_to_read} files...`,
          progress: { current: 0, total: data.files_to_read },
        });
      });

      eventSource.addEventListener('MetadataReadProgress', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        setCurrentStatus({
          type: 'in_progress',
          message: `Reading: ${data.current_file}`,
          progress: { current: data.files_processed, total: data.files_to_read },
        });
      });

      eventSource.addEventListener('MetadataReadComplete', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        setStatusWithTimeout({
          type: 'success',
          message: `Read metadata from ${data.files_processed} files`,
        }, 3000);
      });

      eventSource.addEventListener('ClustersGenerated', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        setCurrentStatus({
          type: 'in_progress',
          message: `Found ${data.total_groups} albums (${data.needs_identification} need identification)`,
        });
      });

      // Identification events
      eventSource.addEventListener('IdentificationStarted', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        setCurrentStatus({
          type: 'in_progress',
          message: `Identifying ${data.group_count} albums with MusicBrainz...`,
          progress: { current: 0, total: data.group_count },
        });
      });

      eventSource.addEventListener('IdentificationProgress', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        setCurrentStatus({
          type: 'in_progress',
          message: `Identifying: ${data.current_album} by ${data.current_artist}`,
          progress: { current: data.albums_processed, total: data.total_albums },
        });
      });

      eventSource.addEventListener('IdentificationComplete', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        setStatusWithTimeout({
          type: 'success',
          message: `Identified ${data.matches_found}/${data.groups_processed} albums`,
        }, 5000);
      });

      eventSource.addEventListener('ClusterIdentified', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        emitSSEEvent('ClusterIdentified', data);
      });

      // Persistence events
      eventSource.addEventListener('ResultsPersisted', () => {
        emitSSEEvent('ResultsPersisted', {});
      });

      // Diff events
      eventSource.addEventListener('TrackDiffsGenerated', () => {
        emitSSEEvent('TrackDiffsGenerated', {});
      });

      eventSource.addEventListener('MetadataDiffApplied', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        emitSSEEvent('MetadataDiffApplied', data);
      });

      eventSource.addEventListener('GroupedDiffsApplied', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        emitSSEEvent('GroupedDiffsApplied', data);
        setStatusWithTimeout({
          type: 'success',
          message: `Applied ${data.diffs_applied} metadata changes to ${data.files_updated} files`,
        }, 5000);
      });

      // Metadata write events
      eventSource.addEventListener('MetadataWriteStarted', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        setCurrentStatus({
          type: 'in_progress',
          message: 'Applying metadata changes',
          progress: { current: 0, total: data.total_changes },
        });
      });

      eventSource.addEventListener('MetadataWriteProgress', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        setCurrentStatus({
          type: 'in_progress',
          message: `Applying metadata changes [${data.changes_processed}/${data.total_changes}]`,
          progress: { current: data.changes_processed, total: data.total_changes },
        });
      });

      eventSource.addEventListener('MetadataWriteCompleted', () => {
        emitSSEEvent('MetadataWriteCompleted', {});
        setStatusWithTimeout({
          type: 'success',
          message: 'Metadata successfully updated',
        }, 5000);
        toast.success('Metadata successfully updated');
      });

      eventSource.addEventListener('MetadataWriteFailed', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        setStatusWithTimeout({
          type: 'error',
          message: `Failed to apply metadata: ${data.error_message}`,
        }, 7000);
        toast.error(`Failed to apply metadata: ${data.error_message}`);
      });

      // Stats updates
      eventSource.addEventListener('StatsUpdated', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        emitSSEEvent('StatsUpdated', data);
      });

      // Acquisition events
      eventSource.addEventListener('TrackedArtistAdded', (e: MessageEvent) => {
        try {
          const data = JSON.parse(e.data);
          emitSSEEvent('TrackedArtistAdded', data);
        } catch (error) {
          console.error('Error handling TrackedArtistAdded:', error, e.data);
        }
      });

      eventSource.addEventListener('ArtistImageFetched', (e: MessageEvent) => {
        try {
          const data = JSON.parse(e.data);
          emitSSEEvent('ArtistImageFetched', data);
        } catch (error) {
          console.error('Error handling ArtistImageFetched:', error);
        }
      });

      eventSource.addEventListener('ArtistBioFetched', (e: MessageEvent) => {
        try {
          const data = JSON.parse(e.data);
          emitSSEEvent('ArtistBioFetched', data);
        } catch (error) {
          console.error('Error handling ArtistBioFetched:', error);
        }
      });

      // Album events
      eventSource.addEventListener('CatalogAlbumAdded', (e: MessageEvent) => {
        try {
          const data = JSON.parse(e.data);
          emitSSEEvent('CatalogAlbumAdded', data);
        } catch (error) {
          console.error('Error handling CatalogAlbumAdded:', error);
        }
      });

      eventSource.addEventListener('AlbumCoverFetched', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        emitSSEEvent('AlbumCoverFetched', data);
      });

      eventSource.addEventListener('CatalogAlbumUpdated', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        emitSSEEvent('CatalogAlbumUpdated', data);
      });

      // Config events
      eventSource.addEventListener('ConfigUpdated', (e: MessageEvent) => {
        const data = JSON.parse(e.data);

        // Update auth enabled state in both store and module-level cache
        if (data.server_auth_enabled !== undefined) {
          setAuthEnabled(data.server_auth_enabled);
          setAuthRequired(data.server_auth_enabled);
        }

        emitSSEEvent('ConfigUpdated', data);
      });

      // Download events
      eventSource.addEventListener('DownloadStarted', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        emitSSEEvent('DownloadStarted', data);
        // Show in status line
        setCurrentStatus({
          type: 'in_progress',
          message: `Downloading: ${data.download_title}`,
          progress: { current: 0, total: 100 },
        });
      });

      eventSource.addEventListener('DownloadProgress', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        emitSSEEvent('DownloadProgress', data);
        // Update status line with current progress
        setCurrentStatus({
          type: 'in_progress',
          message: `Downloading: ${data.download_title}`,
          progress: { current: Math.round(data.download_progress * 100), total: 100 },
        });
      });

      eventSource.addEventListener('DownloadCompleted', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        emitSSEEvent('DownloadCompleted', data);
        // Show completion in status line (will auto-clear after timeout)
        setStatusWithTimeout({
          type: 'success',
          message: `Download completed: ${data.download_title}`,
        }, 5000);
      });

      eventSource.addEventListener('DownloadFailed', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        emitSSEEvent('DownloadFailed', data);
      });

      eventSource.addEventListener('DownloadImported', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        emitSSEEvent('DownloadImported', data);
      });

      eventSource.addEventListener('ArtistDiscographyFetched', (e: MessageEvent) => {
        try {
          const data = JSON.parse(e.data);

          // Show a brief status notification
          setStatusWithTimeout({
            type: 'success',
            message: `Fetched ${data.release_group_count} albums for ${data.artist_name}`,
          }, 3000);

          emitSSEEvent('ArtistDiscographyFetched', data);
        } catch (error) {
          console.error('Error handling ArtistDiscographyFetched:', error);
        }
      });

      // Task events
      eventSource.addEventListener('TaskCompleted', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        emitSSEEvent('TaskCompleted', data);
      });

      eventSource.addEventListener('TaskFailed', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        emitSSEEvent('TaskFailed', data);
      });

      // Additional events that don't have specific UI handling
      eventSource.addEventListener('FileSystemDiffGenerated', () => {});
      eventSource.addEventListener('LibraryArtistFound', () => {});
      eventSource.addEventListener('CatalogArtistFollowed', () => {});
      eventSource.addEventListener('CatalogArtistRefreshRequested', () => {});
      eventSource.addEventListener('WantedAlbumAdded', () => {});
      eventSource.addEventListener('AlbumSearchStarted', () => {});
      eventSource.addEventListener('IndexerSearchCompleted', () => {});
      eventSource.addEventListener('AlbumSearchCompleted', () => {});
      eventSource.addEventListener('BestReleaseSelected', () => {});
      eventSource.addEventListener('DownloadQueued', () => {});
      eventSource.addEventListener('MetadataDiffGenerated', () => {});
      eventSource.addEventListener('Heartbeat', () => {});
    }

    connect();

    return () => {
      mounted = false;
      window.removeEventListener('unauthorized', handleUnauthorized);
      if (eventSourceRef.current) {
        eventSourceRef.current.close();
      }
      if (reconnectTimeoutRef.current) {
        clearTimeout(reconnectTimeoutRef.current);
      }
      if (statusTimeoutRef.current) {
        clearTimeout(statusTimeoutRef.current);
      }
    };
    // Zustand store methods are stable and don't need to be in the dependency array
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [enabled]);
}
