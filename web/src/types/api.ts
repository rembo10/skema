export interface LibraryStats {
  total_files: number;
  total_albums: number;
  total_artists: number;
  matched_files: number;
  unmatched_files: number;
  metadata_accuracy: number; // 0-100 percentage
  total_diffs: number;
  library_size: number; // Total size in bytes
  total_runtime: number; // Total duration in seconds
  library_path: string | null; // Library path
}

export interface MetadataDiff {
  id: number;
  track_id: number;
  file_path: string;
  field_name: string;
  file_value: string | null;
  mb_value: string | null;
  created_at: string;
}

export interface GroupedDiff {
  field_name: string;
  file_value: string | null;
  mb_value: string | null;
  count: number;
  track_ids: number[];
  diffs: MetadataDiff[];
}

export interface Cluster {
  id: number;
  metadata_hash: string;
  album: string | null;
  album_artist: string | null;
  track_count: number;
  mb_release_id: string | null;
  mb_release_group_id: string | null;
  mb_confidence: number | null;
  created_at: string;
  updated_at: string;
  last_identified_at: string | null;
  // Cluster metadata (from first track, used for identification)
  label: string | null;
  catalog_number: string | null;
  barcode: string | null;
  country: string | null;
  date: string | null;
  year: number | null;
  // Matched MusicBrainz release details
  mb_release_title: string | null;
  mb_release_artist: string | null;
  mb_release_date: string | null;
  mb_release_country: string | null;
  mb_release_label: string | null;
  mb_release_catalog_number: string | null;
  mb_release_barcode: string | null;
}

export interface CandidateRelease {
  release_id: string;
  title: string;
  artist: string;
  date: string | null;
  country: string | null;
  track_count: number;
  confidence: number;
}

export interface Task {
  id: string;
  type: 'LibraryScan' | 'MusicBrainzIdentify' | 'MetadataApply';
  status: 'TaskPending' | 'TaskRunning' | 'TaskCompleted' | 'TaskFailed';
  progress?: {
    message: string;
    percent?: number;
  };
  result?: {
    files_scanned?: number;
    total_groups?: number;
    matched_groups?: number;
    files_updated?: number;
    changes_applied?: number;
  };
  created_at: string;
  started_at?: string;
  completed_at?: string;
  error?: string;
}

export interface MetadataChange {
  id: number;
  track_id: number;
  file_path: string;
  field_name: string;
  old_value: string | null;
  new_value: string | null;
  applied_at: string;
  reverted: boolean;
}

// Acquisition types
export interface AcquisitionSource {
  id: number;
  name: string;
  description: string | null;
  source_type: string; // 'library_artists' | 'metacritic' | 'pitchfork'
  enabled: boolean;
  filters: string | null;
  created_at: string;
  updated_at: string;
}

export interface WantedAlbum {
  id: number;
  release_group_mbid: string;
  title: string;
  artist_mbid: string;
  artist_name: string;
  status: string; // 'wanted' | 'monitoring' | 'acquired' | 'ignored'
  added_by_source_id: number;
  first_release_date: string | null;
  matched_cluster_id: number | null;
  created_at: string;
  updated_at: string;
}

// Download client types
export type DownloadClientType = 'sabnzbd' | 'nzbget' | 'transmission' | 'qbittorrent';

export interface DownloadClient {
  type: DownloadClientType;
  url: string;
  api_key: string | null;
  username: string | null;
  password: string | null;
  enabled: boolean;
  download_dir: string | null;
  category: string | null;
}

export interface Indexer {
  name: string;
  url: string;
  api_key: string | null;
  username: string | null;
  password: string | null;
  enabled: boolean;
  priority: number;
  categories: number[];
  normalize_query: boolean;
}

export type MusicBrainzServer = 'official' | 'headphones_vip';

export interface PushoverProvider {
  type: 'pushover';
  user_key: string;
  device: string | null;
  priority: number;
}

export type NotificationProvider = PushoverProvider;

// Config is now nested to match backend structure
export interface LibraryConfig {
  path: string | null;
  watch: boolean;
  auto_scan: boolean;
  auto_scan_interval_mins: number;
  auto_scan_on_startup: boolean;
  normalize_featuring: boolean;
  normalize_featuring_to: string;
  path_format: string;
  file_format: string;
}

export interface SystemConfig {
  watch_config_file: boolean;
  database_path: string | null;
  data_dir: string | null;
  cache_dir: string | null;
}

export interface ServerConfig {
  host: string;
  port: number;
  username: string | null;
  password: string | null;
  jwt_secret: string | null;
  jwt_expiration_hours: number;
  auth_enabled?: boolean; // computed field from backend
}

export interface DownloadConfig {
  nzb_client: DownloadClient | null;
  torrent_client: DownloadClient | null;
  directory: string | null;
  check_interval: number;
  auto_import: boolean;
  min_seeders: number | null;
  max_size: number | null;
}

export interface IndexersConfig {
  list: Indexer[];
  search_timeout: number;
}

export interface MusicbrainzConfig {
  server: MusicBrainzServer;
  username: string | null;
  password: string | null;
  album_types: string[];
  exclude_secondary_types: string[];
}

export interface MediaConfig {
  lastfm_api_key: string | null;
}

export interface NotificationsConfig {
  enabled: boolean;
  providers: NotificationProvider[];
  on_album_found: boolean;
  on_album_downloaded: boolean;
  on_album_imported: boolean;
}

export interface Config {
  library: LibraryConfig;
  system: SystemConfig;
  server: ServerConfig;
  download: DownloadConfig;
  indexers: IndexersConfig;
  musicbrainz: MusicbrainzConfig;
  media: MediaConfig;
  notifications: NotificationsConfig;
}

// Catalog types
export interface CatalogArtist {
  id: number | null;
  mbid: string;
  name: string;
  type: string | null; // 'Person' | 'Group'
  image_url: string | null;
  thumbnail_url: string | null;
  followed: boolean;
  quality_profile_id: number | null; // Quality profile for this artist (overrides global default)
  added_by_source_id: number | null; // Which acquisition source auto-followed this artist (null if manual)
  source_cluster_id: number | null; // Which library cluster this artist came from (null if from search)
  last_checked_at: string | null; // Last time acquisition checked for new releases
  score: number | null; // MusicBrainz search score (0-100), present in search results
  created_at: string | null;
  updated_at: string | null;
}

export interface CatalogAlbum {
  id: number | null;
  release_group_mbid: string;
  title: string;
  artist_mbid: string;
  artist_name: string;
  type: string | null; // 'Album' | 'EP' | 'Single'
  first_release_date: string | null;
  cover_url: string | null;
  cover_thumbnail_url: string | null;
  wanted: boolean;
  quality_profile_id: number | null; // Quality profile for this album (overrides artist/global default)
  matched_cluster_id: number | null;
  score: number | null; // MusicBrainz search score (0-100), present in search results
  created_at: string | null;
  updated_at: string | null;
}

export interface CatalogQueryRequest {
  query: string;
  limit?: number;
}

export interface CatalogQueryResponse {
  artists: CatalogArtist[];
  albums: CatalogAlbum[];
}

// Download types
export type DownloadStatus = 'queued' | 'downloading' | 'completed' | 'failed' | 'imported' | 'cancelled' | 'identification_failure';

export interface Download {
  id: number;
  catalog_album_id: number;
  indexer_name: string;
  download_url: string;
  download_client: string | null;
  download_client_id: string | null;
  status: DownloadStatus;
  download_path: string | null;
  title: string;
  size_bytes: number | null;
  quality: string | null;
  format: string | null;
  seeders: number | null;
  progress: number; // 0.0 - 100.0
  error_message: string | null;
  queued_at: string | null;
  started_at: string | null;
  completed_at: string | null;
  imported_at: string | null;
  library_path: string | null;
}

// Filesystem types
export interface FilesystemEntry {
  name: string;
  path: string;
  is_directory: boolean;
  size: number | null;
  modified_at: string | null;
  readable: boolean;
  writable: boolean;
}

export interface FilesystemBrowseResponse {
  path: string;
  parent: string | null;
  entries: FilesystemEntry[];
  error: string | null;
}

// Quality profile types
export type Quality = 'unknown' | 'mp3_192' | 'vbr2' | 'mp3_256' | 'vbr0' | 'mp3_320' | 'lossless' | 'hires_lossless';

export interface QualityPreference {
  quality: Quality;
  rank: number;
  enabled: boolean;
}

export interface QualityProfile {
  id: number;
  name: string;
  quality_preferences: QualityPreference[];
  cutoff_quality: Quality;
  upgrade_automatically: boolean;
}

export interface CreateQualityProfileRequest {
  name: string;
  quality_preferences: QualityPreference[];
  cutoff_quality: Quality;
  upgrade_automatically: boolean;
}

export interface UpdateQualityProfileRequest {
  name: string;
  quality_preferences: QualityPreference[];
  cutoff_quality: Quality;
  upgrade_automatically: boolean;
}
