import { useEffect, useRef } from 'react';
import toast from 'react-hot-toast';
import { getJWT, setAuthRequired, getApiBase } from '../lib/api';
import { useAppStore } from '../store';

/**
 * Hook to connect to SSE endpoint and keep state updated.
 * Should be used at the App level to maintain a single connection.
 */
export function useSSE(enabled: boolean = true) {
  const eventSourceRef = useRef<EventSource | null>(null);
  const reconnectTimeoutRef = useRef<NodeJS.Timeout>();
  const shouldReconnectRef = useRef(true);
  const statusTimeoutRef = useRef<NodeJS.Timeout>();
  const {
    setCurrentStatus,
    setConnectionStatus,
    removeDiffById,
    setStats,
    setGroupedDiffsStale,
    updateFollowedArtistImageById,
    updateFollowedArtist,
    addFollowedArtist,
    addCatalogAlbum,
    updateAlbumCover,
    addDownload,
    updateDownload,
    setAuthEnabled,
    addCluster,
    updateCluster
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
        reconnectTimeoutRef.current = undefined;
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
            // Mark all data as stale so components reload on next mount/visit
            setGroupedDiffsStale(true);
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
      eventSource.addEventListener('LibraryScanRequested', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
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

      eventSource.addEventListener('ClusterIdentified', async (e: MessageEvent) => {
        // Fetch and update the cluster in the store
        try {
          const data = JSON.parse(e.data);
          const { api } = await import('../lib/api');
          const updatedCluster = await api.getClusters().then(clusters =>
            clusters.find(c => c.id === data.cluster_id)
          );
          if (updatedCluster) {
            const clusters = useAppStore.getState().clusters;
            if (clusters.some(c => c.id === updatedCluster.id)) {
              updateCluster(updatedCluster.id, updatedCluster);
            } else {
              addCluster(updatedCluster);
            }
          }
        } catch (error) {
          console.error('Error handling ClusterIdentified:', error);
        }
      });

      // Persistence events
      eventSource.addEventListener('ResultsPersisted', () => {
        // Mark diffs as stale so they reload
        setGroupedDiffsStale(true);
      });

      // Diff events
      eventSource.addEventListener('TrackDiffsGenerated', () => {
        // Mark diffs as stale
        setGroupedDiffsStale(true);
      });

      eventSource.addEventListener('MetadataDiffApplied', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        if (data.diff_id) {
          removeDiffById(data.diff_id);
        }
      });

      eventSource.addEventListener('GroupedDiffsApplied', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        setGroupedDiffsStale(true);
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
        setGroupedDiffsStale(true);
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
        // Merge new stats with existing stats to preserve fields like library_path
        // that may not be included in partial updates
        const currentStats = useAppStore.getState().stats;
        setStats({
          ...currentStats,
          ...data,
          // Explicitly preserve library_path if not provided in the update
          library_path: data.library_path ?? currentStats?.library_path ?? null,
        });
      });

      // Acquisition events
      eventSource.addEventListener('TrackedArtistAdded', (e: MessageEvent) => {
        try {
          const data = JSON.parse(e.data);
          // Add the new artist with complete data including ID
          // Image will be null initially and updated via ArtistImageFetched event
          const artistData = {
            id: data.artist_id,
            mbid: data.artist_mbid,
            name: data.artist_name,
            image_url: null,
            thumbnail_url: null,
            followed: true,
            added_by_source_id: 0,
            source_cluster_id: data.cluster_id,
            last_checked_at: null,
            created_at: new Date().toISOString(),
            updated_at: new Date().toISOString(),
          };
          addFollowedArtist(artistData);
        } catch (error) {
          console.error('Error handling TrackedArtistAdded:', error, e.data);
        }
      });

      eventSource.addEventListener('ArtistImageFetched', (e: MessageEvent) => {
        try {
          const data = JSON.parse(e.data);
          updateFollowedArtistImageById(data.artist_id, data.image_url, data.thumbnail_url);
        } catch (error) {
          console.error('Error handling ArtistImageFetched:', error);
        }
      });

      // Album events
      eventSource.addEventListener('CatalogAlbumAdded', (e: MessageEvent) => {
        try {
          const data = JSON.parse(e.data);
          // Add the album directly with all data from the event
          // No GET request needed - everything is pushed via SSE!
          addCatalogAlbum({
            id: data.album_id,
            release_group_mbid: data.release_group_mbid,
            title: data.album_title,
            artist_mbid: data.artist_mbid,
            artist_name: data.artist_name,
            type: data.album_type,
            first_release_date: data.first_release_date,
            cover_url: null, // Will be updated via AlbumCoverFetched event
            cover_thumbnail_url: null,
            wanted: data.wanted,
            matched_cluster_id: null,
            score: null,
            quality_profile_id: null,
            created_at: new Date().toISOString(),
            updated_at: new Date().toISOString(),
          });
        } catch (error) {
          console.error('Error handling CatalogAlbumAdded:', error);
        }
      });

      eventSource.addEventListener('AlbumCoverFetched', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        // Update the album's cover in the store
        updateAlbumCover(data.release_group_mbid, data.cover_url, data.thumbnail_url);
      });

      // Config events
      eventSource.addEventListener('ConfigUpdated', (e: MessageEvent) => {
        const data = JSON.parse(e.data);

        // Update auth enabled state in both store and module-level cache
        if (data.server_auth_enabled !== undefined) {
          setAuthEnabled(data.server_auth_enabled);
          setAuthRequired(data.server_auth_enabled);
        }

        // Dispatch custom event for Config page to react to
        window.dispatchEvent(new CustomEvent('config_updated', { detail: data }));

        // Update stats.library_path whenever config changes
        if (data.library_path !== undefined) {
          const currentStats = useAppStore.getState().stats;
          if (currentStats) {
            setStats({
              ...currentStats,
              library_path: data.library_path,
            });
          }
        }
      });

      // Download events
      eventSource.addEventListener('DownloadStarted', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        // Add the download to the store if it doesn't exist
        addDownload({
          id: data.download_id,
          catalog_album_id: 0,
          indexer_name: '',
          download_url: '',
          download_client: null,
          download_client_id: null,
          title: data.download_title,
          status: 'downloading',
          progress: 0,
          download_path: null,
          size_bytes: null,
          quality: null,
          format: null,
          seeders: null,
          error_message: null,
          queued_at: null,
          started_at: new Date().toISOString(),
          completed_at: null,
          imported_at: null,
          library_path: null,
        });
        // Show in status line
        setCurrentStatus({
          type: 'in_progress',
          message: `Downloading: ${data.download_title}`,
          progress: { current: 0, total: 100 },
        });
      });

      eventSource.addEventListener('DownloadProgress', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        // Update progress in the store
        updateDownload(data.download_id, {
          progress: data.download_progress * 100, // Convert 0-1 to 0-100
          size_bytes: data.download_size_bytes,
        });
        // Update status line with current progress
        setCurrentStatus({
          type: 'in_progress',
          message: `Downloading: ${data.download_title}`,
          progress: { current: Math.round(data.download_progress * 100), total: 100 },
        });
      });

      eventSource.addEventListener('DownloadCompleted', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        // Update status to completed
        updateDownload(data.download_id, {
          status: 'completed',
          progress: 100,
          download_path: data.download_path,
          completed_at: new Date().toISOString(),
        });
        // Show completion in status line (will auto-clear after timeout)
        setStatusWithTimeout({
          type: 'success',
          message: `Download completed: ${data.download_title}`,
        }, 5000);
      });

      eventSource.addEventListener('DownloadFailed', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        // Update status to failed
        updateDownload(data.download_id, {
          status: 'failed',
          error_message: data.download_error,
        });
      });

      eventSource.addEventListener('DownloadImported', (e: MessageEvent) => {
        const data = JSON.parse(e.data);
        // Update status to imported
        updateDownload(data.download_id, {
          status: 'imported',
          imported_at: new Date().toISOString(),
        });
      });

      eventSource.addEventListener('ArtistDiscographyFetched', async (e: MessageEvent) => {
        try {
          const data = JSON.parse(e.data);

          // Show a brief status notification
          setStatusWithTimeout({
            type: 'success',
            message: `Fetched ${data.release_group_count} albums for ${data.artist_name}`,
          }, 3000);

          // Update artist's last_checked_at if provided in the event
          if (data.artist_id && data.last_checked_at) {
            updateFollowedArtist(data.artist_id, {
              last_checked_at: data.last_checked_at,
            });
          }
        } catch (error) {
          console.error('Error handling ArtistDiscographyFetched:', error);
        }
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
