import type { LibraryStats, MetadataDiff, GroupedDiff, MetadataChange, Cluster, CandidateRelease, AcquisitionSource, WantedAlbum, Config, CatalogQueryRequest, CatalogQueryResponse, CatalogArtist, CatalogAlbum, Download, FilesystemBrowseResponse, QualityProfile, CreateQualityProfileRequest, UpdateQualityProfileRequest, TrackWithCluster } from '../types/api';

// Auto-detect base path from where the app is loaded
// This allows the app to work at any subpath (e.g., /skema, /music, etc.)
function detectBasePath(): string {
  // Get the base element if it exists
  const base = document.querySelector('base');
  if (base?.href) {
    const url = new URL(base.href);
    return url.pathname.replace(/\/$/, '');
  }

  // Fallback: detect from current location
  // If we're at /skema/library, base path is /skema
  // If we're at /library, base path is '' (root)
  const path = window.location.pathname;

  // Common route patterns in the app
  const knownRoutes = [
    '/library',
    '/identify',
    '/diffs',
    '/metadata-changes',
    '/acquisition',
    '/albums',
    '/catalog',
    '/downloads',
    '/settings',
    '/login',
    '/',
  ];

  // Find if we're on a known route
  for (const route of knownRoutes) {
    if (path === route || path.startsWith(route + '/')) {
      return '';
    }

    // Check if we have a subpath before the route
    // e.g., /skema/library -> base is /skema
    const parts = path.split(route);
    if (parts.length > 1 && parts[0] !== '') {
      return parts[0].replace(/\/$/, '');
    }
  }

  // Default to root
  return '';
}

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

  // Metadata diffs
  async getAllDiffs(): Promise<MetadataDiff[]> {
    return fetchApi<MetadataDiff[]>('/diffs');
  },

  async getGroupedDiffs(): Promise<GroupedDiff[]> {
    return fetchApi<GroupedDiff[]>('/diffs/grouped');
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

  async getMetadataChanges(): Promise<MetadataChange[]> {
    return fetchApi<MetadataChange[]>('/metadata-changes');
  },

  async revertMetadataChange(changeId: number): Promise<void> {
    return fetchApi<void>(`/metadata-changes/${changeId}`, {
      method: 'DELETE',
    });
  },

  // Clusters
  async getClusters(): Promise<Cluster[]> {
    return fetchApi<Cluster[]>('/clusters');
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
    const params = new URLSearchParams({ query });
    if (limit) params.append('limit', limit.toString());
    return fetchApi<CandidateRelease[]>(`/clusters/search-releases?${params}`);
  },

  async searchRecordings(query: string, limit?: number): Promise<MBTrackInfo[]> {
    const params = new URLSearchParams({ query });
    if (limit) params.append('limit', limit.toString());
    return fetchApi<MBTrackInfo[]>(`/clusters/search-recordings?${params}`);
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

  async reidentifyCluster(clusterId: number): Promise<Cluster> {
    return fetchApi<Cluster>(`/clusters/${clusterId}/reidentify`, {
      method: 'POST',
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

  async getAllTracks(): Promise<TrackWithCluster[]> {
    return fetchApi<TrackWithCluster[]>('/library/tracks');
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
    return fetchApi<WantedAlbum[]>('/catalog/albums?wanted=true');
  },

  // Catalog (universal search)
  async catalogQuery(request: CatalogQueryRequest): Promise<CatalogQueryResponse> {
    return fetchApi<CatalogQueryResponse>('/catalog/query', {
      method: 'POST',
      body: JSON.stringify(request),
    });
  },

  async getCatalogArtists(followed?: boolean): Promise<CatalogArtist[]> {
    const params = followed !== undefined ? `?followed=${followed}` : '';
    return fetchApi<CatalogArtist[]>(`/catalog/artists${params}`);
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

  async refreshCatalogArtist(artistId: number): Promise<{ success: boolean; message: string }> {
    return fetchApi<{ success: boolean; message: string }>(`/catalog/artists/${artistId}/refresh`, {
      method: 'POST',
    });
  },

  async refreshAllCatalogArtists(): Promise<{ success: boolean; message: string }> {
    return fetchApi<{ success: boolean; message: string }>('/catalog/refresh', {
      method: 'POST',
    });
  },

  async getCatalogAlbums(wanted?: boolean, artistId?: number): Promise<CatalogAlbum[]> {
    const params = new URLSearchParams();
    if (wanted !== undefined) params.append('wanted', String(wanted));
    if (artistId !== undefined) params.append('artistId', String(artistId));
    const queryString = params.toString() ? `?${params.toString()}` : '';
    return fetchApi<CatalogAlbum[]>(`/catalog/albums${queryString}`);
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

  async updateCatalogAlbum(albumId: number, wanted: boolean, qualityProfileId?: number | null): Promise<CatalogAlbum> {
    const body: { wanted: boolean; quality_profile_id?: number | null } = { wanted };
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
  async getAllDownloads(): Promise<Download[]> {
    return fetchApi<Download[]>('/downloads');
  },

  async getDownload(downloadId: number): Promise<Download> {
    return fetchApi<Download>(`/downloads/${downloadId}`);
  },

  async deleteDownload(downloadId: number): Promise<void> {
    return fetchApi<void>(`/downloads/${downloadId}`, {
      method: 'DELETE',
    });
  },

  async reidentifyDownload(downloadId: number): Promise<void> {
    return fetchApi<void>(`/downloads/${downloadId}/reidentify`, {
      method: 'POST',
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
