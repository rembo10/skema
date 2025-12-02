import { create } from 'zustand';
import type { GroupedDiff, MetadataChange, LibraryStats, CatalogArtist, CatalogAlbum, Download } from '../types/api';

export interface CurrentStatus {
  type: 'in_progress' | 'success' | 'error';
  message: string;
  progress?: {
    current: number;
    total: number;
  };
}

export type ConnectionStatus = 'connected' | 'connecting' | 'disconnected' | 'error';

interface AppStore {
  // Authentication state
  authEnabled: boolean | null;  // null = not checked yet, true = enabled, false = disabled
  setAuthEnabled: (enabled: boolean) => void;

  // Current status (real-time operation status)
  currentStatus: CurrentStatus | null;
  setCurrentStatus: (status: CurrentStatus | null) => void;

  // Connection status
  connectionStatus: ConnectionStatus;
  setConnectionStatus: (status: ConnectionStatus) => void;

  // Diffs (lazy loaded)
  groupedDiffs: GroupedDiff[] | null;  // null = not loaded yet
  groupedDiffsStale: boolean;          // true = has updates since load
  setGroupedDiffs: (diffs: GroupedDiff[]) => void;
  setGroupedDiffsStale: (stale: boolean) => void;
  removeDiffById: (diffId: number) => void;
  removeDiffsByIds: (diffIds: number[]) => void;

  // Changes history (lazy loaded)
  changes: MetadataChange[] | null;    // null = not loaded yet
  changesStale: boolean;               // true = has updates since load
  setChanges: (changes: MetadataChange[]) => void;
  setChangesStale: (stale: boolean) => void;
  addChange: (change: MetadataChange) => void;
  updateChange: (changeId: number, updates: Partial<MetadataChange>) => void;

  // Stats (always loaded - essential)
  stats: LibraryStats | null;
  setStats: (stats: LibraryStats) => void;

  // Followed artists (all)
  followedArtists: CatalogArtist[];
  setFollowedArtists: (artists: CatalogArtist[]) => void;
  addFollowedArtist: (artist: CatalogArtist) => void;
  removeFollowedArtist: (artistId: number) => void;
  updateFollowedArtistImage: (mbid: string, imageUrl: string, thumbnailUrl: string | null) => void;
  updateFollowedArtistImageById: (artistId: number, imageUrl: string, thumbnailUrl: string | null) => void;
  updateFollowedArtistId: (mbid: string, artistId: number) => void;

  // Catalog albums (all - wanted and not wanted)
  catalogAlbums: CatalogAlbum[];
  setCatalogAlbums: (albums: CatalogAlbum[]) => void;
  addCatalogAlbum: (album: CatalogAlbum) => void;
  updateCatalogAlbum: (albumId: number, updates: Partial<CatalogAlbum>) => void;
  updateAlbumCover: (releaseGroupMBID: string, coverUrl: string, thumbnailUrl: string | null) => void;

  // Downloads (all)
  downloads: Download[];
  setDownloads: (downloads: Download[]) => void;
  addDownload: (download: Download) => void;
  updateDownload: (downloadId: number, updates: Partial<Download>) => void;
  removeDownload: (downloadId: number) => void;
}

export const useAppStore = create<AppStore>((set, _) => ({
  // Authentication state
  authEnabled: null,
  setAuthEnabled: (enabled) => set({ authEnabled: enabled }),

  // Current status
  currentStatus: null,
  setCurrentStatus: (status) => set({ currentStatus: status }),

  // Connection status
  connectionStatus: 'disconnected',
  setConnectionStatus: (status) => set({ connectionStatus: status }),

  // Diffs (lazy loaded)
  groupedDiffs: null,
  groupedDiffsStale: false,

  setGroupedDiffs: (diffs) => set({ groupedDiffs: diffs, groupedDiffsStale: false }),

  setGroupedDiffsStale: (stale) => set({ groupedDiffsStale: stale }),

  removeDiffById: (diffId) =>
    set((state) => {
      if (!state.groupedDiffs) {
        // Data not loaded yet - just mark as stale
        return { groupedDiffsStale: true };
      }

      const newGroupedDiffs = state.groupedDiffs
        .map((group) => ({
          ...group,
          diffs: group.diffs.filter((diff) => diff.id !== diffId),
          count: group.diffs.filter((diff) => diff.id !== diffId).length,
        }))
        .filter((group) => group.count > 0);

      return { groupedDiffs: newGroupedDiffs };
    }),

  removeDiffsByIds: (diffIds) =>
    set((state) => {
      if (!state.groupedDiffs) {
        // Data not loaded yet - just mark as stale
        return { groupedDiffsStale: true };
      }

      const diffIdSet = new Set(diffIds);
      const newGroupedDiffs = state.groupedDiffs
        .map((group) => ({
          ...group,
          diffs: group.diffs.filter((diff) => !diffIdSet.has(diff.id)),
          count: group.diffs.filter((diff) => !diffIdSet.has(diff.id)).length,
        }))
        .filter((group) => group.count > 0);

      return { groupedDiffs: newGroupedDiffs };
    }),

  // Changes history (lazy loaded)
  changes: null,
  changesStale: false,

  setChanges: (changes) => set({ changes, changesStale: false }),

  setChangesStale: (stale) => set({ changesStale: stale }),

  addChange: (change) =>
    set((state) => {
      if (!state.changes) {
        // Data not loaded yet - just mark as stale
        return { changesStale: true };
      }
      return { changes: [change, ...state.changes] };
    }),

  updateChange: (changeId, updates) =>
    set((state) => {
      if (!state.changes) {
        // Data not loaded yet - just mark as stale
        return { changesStale: true };
      }
      return {
        changes: state.changes.map((change) =>
          change.id === changeId ? { ...change, ...updates } : change
        ),
      };
    }),

  // Stats
  stats: null,

  setStats: (stats) => set({ stats }),

  // Followed artists (all of them, sorted by created_at DESC)
  followedArtists: [],

  setFollowedArtists: (artists) => set({ followedArtists: artists }),

  addFollowedArtist: (artist) =>
    set((state) => {
      // Check if artist already exists
      const exists = state.followedArtists.some((a) => a.mbid === artist.mbid);
      if (exists) return state;

      // Add to front (most recent)
      return {
        followedArtists: [artist, ...state.followedArtists],
      };
    }),

  removeFollowedArtist: (artistId) =>
    set((state) => ({
      followedArtists: state.followedArtists.filter((a) => a.id !== artistId),
    })),

  updateFollowedArtistImage: (mbid, imageUrl, thumbnailUrl) =>
    set((state) => {
      // Find the artist first
      const artist = state.followedArtists.find((a) => a.mbid === mbid);

      // If artist not found, don't update
      if (!artist) {
        return state;
      }

      // If values are already set, don't update
      if (artist.image_url === imageUrl && artist.thumbnail_url === thumbnailUrl) {
        return state;
      }

      // Only update if necessary
      return {
        followedArtists: state.followedArtists.map((a) =>
          a.mbid === mbid
            ? { ...a, image_url: imageUrl, thumbnail_url: thumbnailUrl }
            : a
        ),
      };
    }),

  updateFollowedArtistId: (mbid, artistId) =>
    set((state) => ({
      followedArtists: state.followedArtists.map((artist) =>
        artist.mbid === mbid
          ? { ...artist, id: artistId }
          : artist
      ),
    })),

  updateFollowedArtistImageById: (artistId, imageUrl, thumbnailUrl) =>
    set((state) => {
      // Find the artist first
      const artist = state.followedArtists.find((a) => a.id === artistId);

      // If artist not found, don't update
      if (!artist) {
        return state;
      }

      // If values are already set, don't update
      if (artist.image_url === imageUrl && artist.thumbnail_url === thumbnailUrl) {
        return state;
      }

      // Only update if necessary
      return {
        followedArtists: state.followedArtists.map((a) =>
          a.id === artistId
            ? { ...a, image_url: imageUrl, thumbnail_url: thumbnailUrl }
            : a
        ),
      };
    }),

  // Catalog albums (all albums - wanted and not wanted)
  catalogAlbums: [],

  setCatalogAlbums: (albums) => set({ catalogAlbums: albums }),

  addCatalogAlbum: (album) =>
    set((state) => {
      // Check if album already exists (by release_group_mbid)
      const exists = state.catalogAlbums.some((a) => a.release_group_mbid === album.release_group_mbid);
      if (exists) return state;

      // Add to the list
      return {
        catalogAlbums: [...state.catalogAlbums, album],
      };
    }),

  updateCatalogAlbum: (albumId, updates) =>
    set((state) => ({
      catalogAlbums: state.catalogAlbums.map((a) =>
        a.id === albumId ? { ...a, ...updates } : a
      ),
    })),

  updateAlbumCover: (releaseGroupMBID, coverUrl, thumbnailUrl) =>
    set((state) => {
      // Find the album first
      const album = state.catalogAlbums.find((a) => a.release_group_mbid === releaseGroupMBID);

      // If album not found, don't update
      if (!album) {
        return state;
      }

      // If values are already set, don't update
      if (album.cover_url === coverUrl && album.cover_thumbnail_url === thumbnailUrl) {
        return state;
      }

      // Only update if necessary
      return {
        catalogAlbums: state.catalogAlbums.map((a) =>
          a.release_group_mbid === releaseGroupMBID
            ? { ...a, cover_url: coverUrl, cover_thumbnail_url: thumbnailUrl }
            : a
        ),
      };
    }),

  // Downloads
  downloads: [],

  setDownloads: (downloads) => set({ downloads }),

  addDownload: (download) =>
    set((state) => {
      // Check if download already exists
      const exists = state.downloads.some((d) => d.id === download.id);
      if (exists) return state;

      // Add to front (most recent)
      return {
        downloads: [download, ...state.downloads],
      };
    }),

  updateDownload: (downloadId, updates) =>
    set((state) => ({
      downloads: state.downloads.map((d) =>
        d.id === downloadId ? { ...d, ...updates } : d
      ),
    })),

  removeDownload: (downloadId) =>
    set((state) => ({
      downloads: state.downloads.filter((d) => d.id !== downloadId),
    })),
}));
