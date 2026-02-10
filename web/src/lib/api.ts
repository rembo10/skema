import type { LibraryStats, MetadataDiff, GroupedDiff, MetadataChange, Cluster, CandidateRelease, AcquisitionSource, WantedAlbum, Config, CatalogQueryRequest, CatalogQueryResponse, CatalogArtist, ArtistsResponse, CatalogAlbum, Download, DownloadsResponse, FilesystemBrowseResponse, QualityProfile, CreateQualityProfileRequest, UpdateQualityProfileRequest, TrackWithCluster, Task, AlbumOverviewRequest, AlbumOverviewResponse, BulkAlbumActionRequest, QueueDownloadRequest, QueueDownloadResponse } from '../types/api';
import { buildQueryString } from './queryBuilder';

// Base path configuration
// Can be set via environment variable at build time, or detected from <base> tag
function detectBasePath(): string {
  // Check for base element in HTML (conventional approach)
  const base = document.querySelector('base');
  if (base?.href) {
    const url = new URL(base.href);
    return url.pathname.replace(/\/$/, '');
  }

  // Check for build-time environment variable
  if (import.meta.env.BASE_URL && import.meta.env.BASE_URL !== '/') {
    return import.meta.env.BASE_URL.replace(/\/$/, '');
  }

  // Default to root
  return '';
}

// Initialize base paths once at module load
const BASE_PATH = detectBasePath();
const API_BASE = `${BASE_PATH}/api`;

const JWT_STORAGE_KEY = 'skema_jwt';
const JWT_EXPIRES_STORAGE_KEY = 'skema_jwt_expires';

// Export API_BASE for use in other modules (like SSE)
export function getApiBase(): string {
  return API_BASE;
}

// Export BASE_PATH for use in routing
export function getBasePath(): string {
  return BASE_PATH;
}

// JWT token management
export function getJWT(): string | null {
  const jwt = localStorage.getItem(JWT_STORAGE_KEY);
  const expiresAt = localStorage.getItem(JWT_EXPIRES_STORAGE_KEY);

  // Check if token has expired
  if (jwt && expiresAt) {
    const expiryDate = new Date(expiresAt);
    if (expiryDate <= new Date()) {
      // Token expired, clear it
      clearJWT();
      return null;
    }
  }

  return jwt;
}

export function setJWT(jwt: string, expiresAt: string): void {
  localStorage.setItem(JWT_STORAGE_KEY, jwt);
  localStorage.setItem(JWT_EXPIRES_STORAGE_KEY, expiresAt);
}

export function clearJWT(): void {
  localStorage.removeItem(JWT_STORAGE_KEY);
  localStorage.removeItem(JWT_EXPIRES_STORAGE_KEY);
}

// Track auth requirement status
let authRequired: boolean | null = null;

export function setAuthRequired(required: boolean): void {
  authRequired = required;
}

export function isAuthRequired(): boolean | null {
  return authRequired;
}

export function isAuthenticated(): boolean {
  // If auth is not required, consider always authenticated
  if (authRequired === false) {
    return true;
  }
  // If auth is required (or we don't know yet), check for JWT
  return getJWT() !== null;
}

// Custom error class for API errors
class ApiError extends Error {
  constructor(message: string, public status: number, public isAuthError: boolean = false) {
    super(message);
    this.name = 'ApiError';
  }
}

async function fetchApi<T>(endpoint: string, options?: RequestInit): Promise<T> {
  const jwt = getJWT();

  const response = await fetch(`${API_BASE}${endpoint}`, {
    ...options,
    headers: {
      'Content-Type': 'application/json',
      ...(jwt ? { 'Authorization': `Bearer ${jwt}` } : {}),
      ...options?.headers,
    },
  });

  if (!response.ok) {
    const error = await response.json().catch(() => ({ message: 'Unknown error' }));

    // If unauthorized, clear JWT and dispatch event to redirect to login
    if (response.status === 401) {
      clearJWT();
      // Dispatch custom event for global handling
      window.dispatchEvent(new CustomEvent('unauthorized'));
      // Throw an auth error that components can handle silently
      throw new ApiError(error.message || 'Unauthorized', 401, true);
    }

    throw new ApiError(error.message || `API error: ${response.status}`, response.status);
  }

  // Handle empty responses (204 No Content or empty body)
  const contentType = response.headers.get('content-type');
  if (response.status === 204 || !contentType?.includes('application/json')) {
    return undefined as T;
  }

  // Check if response has content before parsing JSON
  const text = await response.text();
  if (!text) {
    return undefined as T;
  }

  return JSON.parse(text) as T;
}

export const api = {
  // Check auth status (can be called without authentication when auth is disabled)
  // Uses a module-level cache to avoid redundant checks
  async checkAuthStatus(): Promise<{ authEnabled: boolean }> {
    // Return cached value if available
    if (authRequired !== null) {
      return { authEnabled: authRequired };
    }

    try {
      const response = await fetch(`${API_BASE}/auth/status`);

      if (response.ok) {
        const data = await response.json();
        const enabled = data.enabled || false;
        setAuthRequired(enabled);
        return { authEnabled: enabled };
      }

      // For errors, assume auth is required for safety
      setAuthRequired(true);
      return { authEnabled: true };
    } catch {
      // If we can't reach the server, assume auth is required
      setAuthRequired(true);
      return { authEnabled: true };
    }
  },

  // Authentication
  async login(username: string, password: string): Promise<{ jwt: string; expiresAt: string; message: string }> {
    // Login doesn't use the Authorization header (since we don't have one yet)
    const response = await fetch(`${API_BASE}/auth/credentials`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ username, password }),
    });

    if (!response.ok) {
      const error = await response.json().catch(() => ({ message: 'Invalid credentials' }));
      throw new Error(error.message || 'Authentication failed');
    }

    const data = await response.json();
    return {
      jwt: data.jwt,
      expiresAt: data.expires_at,
      message: data.message,
    };
  },

  logout(): void {
    clearJWT();
  },

  // Library stats
  async getStats(): Promise<LibraryStats> {
    return fetchApi<LibraryStats>('/stats');
  },

  // Submit events to the event bus
  async submitEvent(eventType: string, eventData?: Record<string, unknown>): Promise<{ success: boolean; message: string }> {
    return fetchApi<{ success: boolean; message: string }>('/events', {
      method: 'POST',
      body: JSON.stringify({ type: eventType, data: eventData }),
    });
  },

  // Library scan - now uses event-driven architecture
  async scanLibrary(): Promise<{ success: boolean; message: string }> {
    return this.submitEvent('LibraryScanRequested');
  },

  // Force re-scan library (treats all files as new)
  async forceScanLibrary(): Promise<{ success: boolean; message: string }> {
    return this.submitEvent('LibraryScanRequested', { force_rescan: true });
  },

  // Metadata diffs
  async getAllDiffs(): Promise<MetadataDiff[]> {
    return fetchApi<MetadataDiff[]>('/diffs');
  },

  async getGroupedDiffs(offset: number = 0, limit: number = 50): Promise<GroupedDiffsResponse> {
    const query = buildQueryString({ offset, limit });
    return fetchApi<GroupedDiffsResponse>(`/diffs/grouped?${query}`);
  },

  async applyDiff(diffId: number): Promise<void> {
    return fetchApi<void>(`/diffs/${diffId}/apply`, {
      method: 'POST',
    });
  },

  async applyGroupedDiff(fieldName: string, fileValue: string | null, mbValue: string | null): Promise<void> {
    return fetchApi<void>('/diffs/apply-grouped', {
      method: 'POST',
      body: JSON.stringify({ field_name: fieldName, file_value: fileValue, mb_value: mbValue }),
    });
  },

  async applyDiffToFile(fileId: number, fieldName: string, value: string | null): Promise<void> {
    return fetchApi<void>(`/diffs/apply-to-file`, {
      method: 'POST',
      body: JSON.stringify({ file_id: fileId, field_name: fieldName, value }),
    });
  },

  // Metadata changes (for undo functionality)
  async applyMetadataChanges(diffIds: number[]): Promise<MetadataChange[]> {
    return fetchApi<MetadataChange[]>('/metadata-changes', {
      method: 'POST',
      body: JSON.stringify({ diff_ids: diffIds }),
    });
  },

  async getMetadataChanges(offset: number = 0, limit: number = 50): Promise<MetadataChangesResponse> {
    const query = buildQueryString({ offset, limit });
    return fetchApi<MetadataChangesResponse>(`/metadata-changes?${query}`);
  },

  async revertMetadataChange(changeId: number): Promise<void> {
    return fetchApi<void>(`/metadata-changes/${changeId}`, {
      method: 'DELETE',
    });
  },

  // Clusters
  async getClusters(
    offset: number = 0,
    limit: number = 50,
    search?: string,
    filter?: string,
    sort?: string,
    order?: string
  ): Promise<ClustersResponse> {
    const query = buildQueryString({ offset, limit, search, filter, sort, order });
    return fetchApi<ClustersResponse>(`/clusters?${query}`);
  },

  async getCluster(clusterId: number): Promise<{
    cluster: Cluster;
    tracks: Array<{
      id: number;
      path: string;
      title: string | null;
      artist: string | null;
      track_number: number | null;
      disc_number: number | null;
      duration: number | null;
    }>;
  }> {
    return fetchApi(`/clusters/${clusterId}`);
  },

  async getCandidateReleases(clusterId: number): Promise<CandidateRelease[]> {
    return fetchApi(`/clusters/${clusterId}/candidates`);
  },

  async assignRelease(
    clusterId: number,
    releaseId: string,
    releaseGroupId?: string,
    confidence?: number
  ): Promise<Cluster> {
    return fetchApi<Cluster>(`/clusters/${clusterId}/release`, {
      method: 'PUT',
      body: JSON.stringify({
        release_id: releaseId,
        release_group_id: releaseGroupId,
        confidence,
      }),
    });
  },

  async removeRelease(clusterId: number): Promise<void> {
    return fetchApi<void>(`/clusters/${clusterId}/release`, {
      method: 'DELETE',
    });
  },

  async getCandidates(clusterId: number): Promise<CandidateRelease[]> {
    return fetchApi<CandidateRelease[]>(`/clusters/${clusterId}/candidates`);
  },

  async searchReleases(query: string, limit?: number): Promise<CandidateRelease[]> {
    const qs = buildQueryString({ query, limit });
    return fetchApi<CandidateRelease[]>(`/clusters/search-releases?${qs}`);
  },

  async searchRecordings(query: string, limit?: number): Promise<MBTrackInfo[]> {
    const qs = buildQueryString({ query, limit });
    return fetchApi<MBTrackInfo[]>(`/clusters/search-recordings?${qs}`);
  },

  async updateTrackRecording(
    clusterId: number,
    trackId: number,
    recordingId: string,
    recordingTitle?: string
  ): Promise<void> {
    return fetchApi<void>(`/clusters/${clusterId}/tracks/${trackId}/recording`, {
      method: 'PUT',
      body: JSON.stringify({
        recording_id: recordingId,
        recording_title: recordingTitle,
      }),
    });
  },

  async reidentifyCluster(clusterId: number): Promise<Task> {
    return fetchApi<Task>('/clusters/tasks', {
      method: 'POST',
      body: JSON.stringify({
        type: 'identify',
        cluster_id: clusterId,
      }),
    });
  },

  async getClusterWithTracks(clusterId: number): Promise<{
    cluster: Cluster;
    tracks: Array<{
      id: number;
      path: string;
      title: string | null;
      artist: string | null;
      track_number: number | null;
      disc_number: number | null;
      duration: number | null;
    }>;
  }> {
    return fetchApi(`/clusters/${clusterId}`);
  },

  async getAllTracks(offset: number = 0, limit: number = 50, filter?: string, sort?: string, order?: string, search?: string): Promise<TracksResponse> {
    const query = buildQueryString({
      offset,
      limit,
      filter: filter && filter !== 'all' ? filter : undefined,
      sort,
      order,
      search,
    });
    return fetchApi<TracksResponse>(`/library/tracks?${query}`);
  },

  async getTracksStats(): Promise<TracksStats> {
    return fetchApi<TracksStats>('/library/tracks/stats');
  },

  async updateTrack(trackId: number, update: { cluster_id: number | null }): Promise<void> {
    return fetchApi<void>(`/library/tracks/${trackId}`, {
      method: 'PATCH',
      body: JSON.stringify(update),
    });
  },

  // Acquisition
  async getAcquisitionSources(): Promise<AcquisitionSource[]> {
    return fetchApi<AcquisitionSource[]>('/acquisition/sources');
  },

  async createAcquisitionSource(source: {
    name: string;
    description?: string;
    source_type: string;
    artist_mbid?: string;
    enabled: boolean;
    priority: number;
    filters?: string;
  }): Promise<AcquisitionSource> {
    return fetchApi<AcquisitionSource>('/acquisition/sources', {
      method: 'POST',
      body: JSON.stringify({
        name: source.name,
        description: source.description,
        type: source.source_type,  // Backend expects 'type'
        artist_mbid: source.artist_mbid,
        enabled: source.enabled,
        priority: source.priority,
        filters: source.filters,
      }),
    });
  },

  async updateAcquisitionSource(sourceId: number, source: {
    name: string;
    description?: string;
    source_type: string;
    artist_mbid?: string;
    enabled: boolean;
    priority: number;
    filters?: string;
  }): Promise<AcquisitionSource> {
    return fetchApi<AcquisitionSource>(`/acquisition/sources/${sourceId}`, {
      method: 'PUT',
      body: JSON.stringify({
        name: source.name,
        description: source.description,
        type: source.source_type,  // Backend expects 'type'
        artist_mbid: source.artist_mbid,
        enabled: source.enabled,
        priority: source.priority,
        filters: source.filters,
      }),
    });
  },

  async deleteAcquisitionSource(sourceId: number): Promise<void> {
    return fetchApi<void>(`/acquisition/sources/${sourceId}`, {
      method: 'DELETE',
    });
  },

  async enableAcquisitionSource(sourceId: number): Promise<void> {
    return fetchApi<void>(`/acquisition/sources/${sourceId}/enable`, {
      method: 'PUT',
    });
  },

  async disableAcquisitionSource(sourceId: number): Promise<void> {
    return fetchApi<void>(`/acquisition/sources/${sourceId}/disable`, {
      method: 'PUT',
    });
  },

  async getWantedAlbums(): Promise<WantedAlbum[]> {
    // Wanted albums are now part of catalog albums with wanted=true filter
    // Backend now returns AlbumOverviewResponse, extract albums array
    const response = await fetchApi<AlbumOverviewResponse>('/catalog/albums?wanted=true');
    // Convert CatalogAlbumOverview to WantedAlbum format
    return response.albums.map(album => ({
      id: album.id,
      release_group_mbid: album.release_group_mbid,
      title: album.title,
      artist_mbid: album.artist_mbid,
      artist_name: album.artist_name,
      status: album.state.toLowerCase() as 'wanted' | 'monitoring' | 'acquired' | 'ignored',
      added_by_source_id: album.artist_id || 0, // We don't have added_by_source_id in overview, use 0 as fallback
      first_release_date: album.first_release_date,
      matched_cluster_id: album.matched_cluster_id,
      created_at: album.created_at,
      updated_at: album.updated_at,
    }));
  },

  // Catalog (universal search)
  async catalogQuery(request: CatalogQueryRequest): Promise<CatalogQueryResponse> {
    return fetchApi<CatalogQueryResponse>('/catalog/query', {
      method: 'POST',
      body: JSON.stringify(request),
    });
  },

  async getCatalogArtists(
    offset: number = 0,
    limit: number = 50,
    followed?: boolean,
    search?: string,
    sort?: string,
    order?: 'asc' | 'desc'
  ): Promise<ArtistsResponse> {
    const query = buildQueryString({ offset, limit, followed, search, sort, order });
    return fetchApi<ArtistsResponse>(`/catalog/artists?${query}`);
  },

  async createCatalogArtist(artist: {
    mbid: string;
    name: string;
    type?: string;
    image_url?: string;
    followed: boolean;
  }): Promise<CatalogArtist> {
    return fetchApi<CatalogArtist>('/catalog/artists', {
      method: 'POST',
      body: JSON.stringify(artist),
    });
  },

  async updateCatalogArtist(artistId: number, followed: boolean, qualityProfileId?: number | null): Promise<CatalogArtist> {
    const body: { followed: boolean; quality_profile_id?: number | null } = { followed };
    if (qualityProfileId !== undefined) {
      body.quality_profile_id = qualityProfileId;
    }
    return fetchApi<CatalogArtist>(`/catalog/artists/${artistId}`, {
      method: 'PATCH',
      body: JSON.stringify(body),
    });
  },

  async deleteCatalogArtist(artistId: number): Promise<void> {
    return fetchApi<void>(`/catalog/artists/${artistId}`, {
      method: 'DELETE',
    });
  },

  async refreshCatalogArtist(artistId: number): Promise<Task> {
    return fetchApi<Task>('/catalog/tasks', {
      method: 'POST',
      body: JSON.stringify({
        type: 'refresh',
        artist_id: artistId,
      }),
    });
  },

  async refreshAllCatalogArtists(): Promise<Task> {
    return fetchApi<Task>('/catalog/tasks', {
      method: 'POST',
      body: JSON.stringify({
        type: 'refresh_all',
      }),
    });
  },

  async getCatalogAlbums(wanted?: boolean, artistId?: number): Promise<CatalogAlbum[]> {
    const query = buildQueryString({ wanted, artistId });
    const url = query ? `/catalog/albums?${query}` : '/catalog/albums';
    // Backend now returns AlbumOverviewResponse, extract albums array
    const response = await fetchApi<AlbumOverviewResponse>(url);
    // Convert CatalogAlbumOverview to CatalogAlbum (use only the fields that exist in both)
    return response.albums.map(album => ({
      id: album.id,
      release_group_mbid: album.release_group_mbid,
      title: album.title,
      artist_mbid: album.artist_mbid,
      artist_name: album.artist_name,
      type: album.type,
      first_release_date: album.first_release_date,
      cover_url: album.cover_url,
      cover_thumbnail_url: album.cover_thumbnail_url,
      wanted: album.wanted,
      quality_profile_id: album.quality_profile_id,
      matched_cluster_id: album.matched_cluster_id,
      score: null,
      created_at: album.created_at,
      updated_at: album.updated_at,
    }));
  },

  async createCatalogAlbum(album: {
    release_group_mbid: string;
    title: string;
    artist_mbid: string;
    artist_name: string;
    type?: string;
    first_release_date?: string;
    wanted: boolean;
  }): Promise<CatalogAlbum> {
    return fetchApi<CatalogAlbum>('/catalog/albums', {
      method: 'POST',
      body: JSON.stringify(album),
    });
  },

  async updateCatalogAlbum(albumId: number, qualityProfileId?: number | null): Promise<CatalogAlbum> {
    const body: { quality_profile_id?: number | null } = {};
    if (qualityProfileId !== undefined) {
      body.quality_profile_id = qualityProfileId;
    }
    return fetchApi<CatalogAlbum>(`/catalog/albums/${albumId}`, {
      method: 'PATCH',
      body: JSON.stringify(body),
    });
  },

  async deleteCatalogAlbum(albumId: number): Promise<void> {
    return fetchApi<void>(`/catalog/albums/${albumId}`, {
      method: 'DELETE',
    });
  },

  async getAlbumOverview(request: AlbumOverviewRequest): Promise<AlbumOverviewResponse> {
    const query = buildQueryString({
      offset: request.offset,
      limit: request.limit,
      wanted: request.wanted,
      artistId: request.artist_id,
      search: request.search,
      sort: request.sort,
      order: request.order,
      state: request.state,
      quality: request.quality,
      release_date_after: request.release_date_after,
      release_date_before: request.release_date_before,
    });
    const url = query ? `/catalog/albums?${query}` : '/catalog/albums';

    return fetchApi<AlbumOverviewResponse>(url, {
      method: 'GET',
    });
  },

  async bulkAlbumAction(request: BulkAlbumActionRequest): Promise<void> {
    return fetchApi<void>('/catalog/albums/bulk-action', {
      method: 'POST',
      body: JSON.stringify(request),
    });
  },

  async getAlbumReleases(albumId: number): Promise<AlbumReleasesResponse> {
    return fetchApi<AlbumReleasesResponse>(`/catalog/albums/${albumId}/releases`, {
      method: 'GET',
    });
  },

  /**
   * Stream album releases via SSE. Returns an EventSource that emits:
   * - 'started': { source: string } - A search source started
   * - 'release': { source: string, release: AlbumRelease } - A release was found
   * - 'completed': { source: string, count: number } - A search source completed
   * - 'error': { source: string, error: string } - A search source had an error
   * - 'done': { total_time: number } - All searches completed
   */
  streamAlbumReleases(
    albumId: number,
    callbacks: {
      onStarted?: (source: string) => void;
      onRelease?: (source: string, release: AlbumRelease) => void;
      onCompleted?: (source: string, count: number) => void;
      onError?: (source: string, error: string) => void;
      onDone?: (totalTime: number) => void;
    }
  ): EventSource {
    const jwt = getJWT();
    const apiBase = getApiBase();
    const url = jwt
      ? `${window.location.origin}${apiBase}/catalog/albums/${albumId}/releases/stream?token=${encodeURIComponent(jwt)}`
      : `${window.location.origin}${apiBase}/catalog/albums/${albumId}/releases/stream`;

    const eventSource = new EventSource(url);

    eventSource.addEventListener('started', (e: MessageEvent) => {
      const data = JSON.parse(e.data);
      callbacks.onStarted?.(data.source);
    });

    eventSource.addEventListener('release', (e: MessageEvent) => {
      const data = JSON.parse(e.data);
      callbacks.onRelease?.(data.source, data.release);
    });

    eventSource.addEventListener('completed', (e: MessageEvent) => {
      const data = JSON.parse(e.data);
      callbacks.onCompleted?.(data.source, data.count);
    });

    eventSource.addEventListener('error', (e: MessageEvent) => {
      try {
        const data = JSON.parse(e.data);
        callbacks.onError?.(data.source, data.error);
      } catch {
        // Connection error, not a JSON event
        console.error('SSE connection error');
      }
    });

    eventSource.addEventListener('done', (e: MessageEvent) => {
      const data = JSON.parse(e.data);
      callbacks.onDone?.(data.total_time);
      eventSource.close();
    });

    return eventSource;
  },

  // Configuration
  async getConfig(): Promise<Config> {
    return fetchApi<Config>('/config');
  },

  async updateConfig(updates: Partial<Config>): Promise<Config> {
    return fetchApi<Config>('/config', {
      method: 'PUT',
      body: JSON.stringify(updates),
    });
  },

  // Downloads
  async getAllDownloads(offset: number = 0, limit: number = 50): Promise<DownloadsResponse> {
    const query = buildQueryString({ offset, limit });
    return fetchApi<DownloadsResponse>(`/downloads?${query}`);
  },

  async getDownload(downloadId: number): Promise<Download> {
    return fetchApi<Download>(`/downloads/${downloadId}`);
  },

  async deleteDownload(downloadId: number): Promise<void> {
    return fetchApi<void>(`/downloads/${downloadId}`, {
      method: 'DELETE',
    });
  },

  async queueDownload(request: QueueDownloadRequest): Promise<QueueDownloadResponse> {
    return fetchApi<QueueDownloadResponse>('/downloads/queue', {
      method: 'POST',
      body: JSON.stringify(request),
    });
  },

  async reidentifyDownload(downloadId: number): Promise<Task> {
    return fetchApi<Task>('/downloads/tasks', {
      method: 'POST',
      body: JSON.stringify({
        type: 'reidentify',
        download_id: downloadId,
      }),
    });
  },

  async retryDownload(downloadId: number): Promise<Task> {
    return fetchApi<Task>('/downloads/tasks', {
      method: 'POST',
      body: JSON.stringify({
        type: 'retry',
        download_id: downloadId,
      }),
    });
  },

  // Filesystem browsing
  async browseFilesystem(path?: string): Promise<FilesystemBrowseResponse> {
    const params = path ? `?path=${encodeURIComponent(path)}` : '';
    return fetchApi<FilesystemBrowseResponse>(`/filesystem/browse${params}`);
  },

  // Quality profiles
  async getQualityProfiles(): Promise<QualityProfile[]> {
    return fetchApi<QualityProfile[]>('/quality-profiles');
  },

  async getQualityProfile(profileId: number): Promise<QualityProfile> {
    return fetchApi<QualityProfile>(`/quality-profiles/${profileId}`);
  },

  async createQualityProfile(profile: CreateQualityProfileRequest): Promise<QualityProfile> {
    return fetchApi<QualityProfile>('/quality-profiles', {
      method: 'POST',
      body: JSON.stringify(profile),
    });
  },

  async updateQualityProfile(profileId: number, profile: UpdateQualityProfileRequest): Promise<QualityProfile> {
    return fetchApi<QualityProfile>(`/quality-profiles/${profileId}`, {
      method: 'PUT',
      body: JSON.stringify(profile),
    });
  },

  async deleteQualityProfile(profileId: number): Promise<void> {
    return fetchApi<void>(`/quality-profiles/${profileId}`, {
      method: 'DELETE',
    });
  },

  async getDefaultQualityProfile(): Promise<QualityProfile | null> {
    return fetchApi<QualityProfile | null>('/quality-profiles/default');
  },

  async setDefaultQualityProfile(profileId: number): Promise<void> {
    return fetchApi<void>(`/quality-profiles/default/${profileId}`, {
      method: 'PUT',
    });
  },
};
