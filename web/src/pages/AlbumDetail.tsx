import { useEffect, useState } from 'react';
import { useParams, useNavigate, Link } from 'react-router-dom';
import { api } from '../lib/api';
import { formatDate, formatBytes, formatTrackDurationMs, formatTrackDuration, formatTimeAgo } from '../lib/formatters';
import { formatQuality, getQualityBadgeStyle } from '../lib/quality';
import type { CatalogAlbumOverview, AlbumTracksResponse, QualityProfile, Download } from '../types/api';
import { AlbumReleasesList } from '../components/AlbumReleasesList';
import { useSSEEvent, useSSERefresh } from '../hooks/useSSEEvent';
import { handleApiError } from '../lib/errors';
import toast from 'react-hot-toast';
import {
  Disc, Music, ArrowLeft, ExternalLink, Search as SearchIcon, X,
  ListMusic, CheckCircle2, Circle, AlertCircle, Loader2, Download as DownloadIcon,
} from 'lucide-react';

// Map an audio file format (FLAC/MP3/…) to a colored badge style, mirroring the
// quality-badge palette used elsewhere.
function qualityBadgeForFormat(format: string): string {
  const f = format.toUpperCase();
  if (f === 'FLAC' || f === 'ALAC' || f === 'WAV' || f === 'APE') {
    return 'bg-green-500/20 text-green-400 border-green-500/30';
  }
  if (f === 'MP3') {
    return 'bg-lime-500/20 text-lime-400 border-lime-500/30';
  }
  if (f === 'OGG' || f === 'OPUS' || f === 'AAC' || f === 'M4A') {
    return 'bg-yellow-500/20 text-yellow-400 border-yellow-500/30';
  }
  return 'bg-gray-500/20 text-gray-400 border-gray-500/30';
}

interface LibraryTrack {
  id: number;
  path: string;
  title: string | null;
  artist: string | null;
  track_number: number | null;
  disc_number: number | null;
  duration: number | null;
  format: string | null;
  mb_recording_id: string | null;
}

const DOWNLOAD_STATUS_STYLES: Record<string, string> = {
  queued: 'text-dark-text-secondary',
  downloading: 'text-orange-400',
  completed: 'text-green-400',
  imported: 'text-green-500',
  failed: 'text-red-400',
  cancelled: 'text-dark-text-tertiary',
  identification_failure: 'text-red-400',
};

// Events that change an album's download/library state and warrant a reload.
const ALBUM_REFRESH_EVENTS = ['DownloadCompleted', 'DownloadImported', 'WantedAlbumAdded'];

export default function AlbumDetail() {
  const { id } = useParams<{ id: string }>();
  const navigate = useNavigate();
  const albumId = parseInt(id || '', 10);

  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [album, setAlbum] = useState<CatalogAlbumOverview | null>(null);
  const [qualityProfiles, setQualityProfiles] = useState<QualityProfile[]>([]);
  const [defaultProfile, setDefaultProfile] = useState<QualityProfile | null>(null);

  const [tracks, setTracks] = useState<AlbumTracksResponse | null>(null);
  const [tracksLoading, setTracksLoading] = useState(false);

  const [libraryTracks, setLibraryTracks] = useState<LibraryTrack[]>([]);
  const [downloads, setDownloads] = useState<Download[]>([]);
  const [showReleasesSearch, setShowReleasesSearch] = useState(false);

  useEffect(() => {
    loadAll();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [albumId]);

  useEffect(() => {
    loadQualityProfiles();
  }, []);

  useEffect(() => {
    if (album?.title) {
      document.title = `${album.title} :: skema`;
    }
  }, [album?.title]);

  // SSE: album state/quality/cover updates
  useSSEEvent<{ album_id: number; state: string; current_quality: string | null; quality_profile_id: number | null; quality_profile_name: string | null; cover_url: string | null; cover_thumbnail_url: string | null }>('CatalogAlbumUpdated', (data) => {
    if (data.album_id !== albumId) return;
    setAlbum(prev => prev ? {
      ...prev,
      state: data.state as CatalogAlbumOverview['state'],
      current_quality: data.current_quality,
      quality_profile_id: data.quality_profile_id,
      quality_profile_name: data.quality_profile_name,
      cover_url: data.cover_url,
      cover_thumbnail_url: data.cover_thumbnail_url,
    } : prev);
  });

  useSSEEvent<{ release_group_mbid: string; cover_url: string; thumbnail_url: string | null }>('AlbumCoverFetched', (data) => {
    setAlbum(prev => prev && prev.release_group_mbid === data.release_group_mbid
      ? { ...prev, cover_url: data.cover_url, cover_thumbnail_url: data.thumbnail_url }
      : prev);
  });

  // SSE: download/library changes — reload album + downloads
  useSSERefresh(ALBUM_REFRESH_EVENTS, () => {
    reloadAlbum();
    loadDownloads();
  });

  const loadAll = async () => {
    if (isNaN(albumId)) {
      setError('Invalid album ID');
      setLoading(false);
      return;
    }
    setLoading(true);
    setError(null);
    try {
      const found = await api.getCatalogAlbumById(albumId);
      setAlbum(found);
      // Load the rest in parallel; failures here shouldn't blank the page
      loadTracks();
      loadDownloads();
      if (found.matched_cluster_id) {
        loadLibraryTracks(found.matched_cluster_id);
      } else {
        setLibraryTracks([]);
      }
    } catch (err) {
      if (!handleApiError(err, 'Failed to load album')) {
        setError('Failed to load album');
      }
    } finally {
      setLoading(false);
    }
  };

  const reloadAlbum = async () => {
    try {
      const found = await api.getCatalogAlbumById(albumId);
      setAlbum(found);
      if (found.matched_cluster_id) {
        loadLibraryTracks(found.matched_cluster_id);
      }
    } catch {
      // Non-fatal background refresh
    }
  };

  const loadQualityProfiles = async () => {
    try {
      const [profiles, defaultProf] = await Promise.all([
        api.getQualityProfiles(),
        api.getDefaultQualityProfile(),
      ]);
      setQualityProfiles(profiles);
      setDefaultProfile(defaultProf);
    } catch (err) {
      handleApiError(err, 'Failed to load quality profiles');
    }
  };

  const loadTracks = async () => {
    setTracksLoading(true);
    try {
      const data = await api.getAlbumTracks(albumId);
      setTracks(data);
    } catch {
      // Tracklist comes from MusicBrainz and may be unavailable; degrade quietly
      setTracks(null);
    } finally {
      setTracksLoading(false);
    }
  };

  const loadDownloads = async () => {
    try {
      const resp = await api.getAllDownloads(0, 100, albumId);
      setDownloads(resp.downloads);
    } catch {
      setDownloads([]);
    }
  };

  const loadLibraryTracks = async (clusterId: number) => {
    try {
      const resp = await api.getClusterWithTracks(clusterId);
      setLibraryTracks(resp.tracks);
    } catch {
      setLibraryTracks([]);
    }
  };

  const handleQualityProfileChange = async (value: string) => {
    if (!album) return;
    try {
      let profileId: number | null;
      if (value === 'existing' || value === '') {
        profileId = null;
      } else if (value === 'default') {
        if (!defaultProfile?.id) {
          toast.error('No default quality profile configured');
          return;
        }
        profileId = defaultProfile.id;
      } else {
        profileId = parseInt(value, 10);
      }
      await api.updateCatalogAlbum(album.id, profileId);
      toast.success(profileId === null ? 'Monitoring disabled' : 'Quality profile updated');
      await reloadAlbum();
    } catch (err) {
      handleApiError(err, 'Failed to update quality profile');
    }
  };

  const handleWant = async () => {
    if (!album) return;
    const profileToUse = album.quality_profile_id || defaultProfile?.id || null;
    if (!profileToUse) {
      toast.error('No quality profile available. Please set a default quality profile first.');
      return;
    }
    try {
      await api.updateCatalogAlbum(album.id, profileToUse);
      toast.success(`Added ${album.title} to wanted list`);
      await reloadAlbum();
    } catch (err) {
      handleApiError(err, 'Failed to update album');
    }
  };

  const handleUnwant = async () => {
    if (!album) return;
    try {
      await api.updateCatalogAlbum(album.id, null);
      toast.success(`Removed ${album.title} from wanted list`);
      await reloadAlbum();
    } catch (err) {
      handleApiError(err, 'Failed to update album');
    }
  };

  const handleForceSearch = async () => {
    if (!album) return;
    try {
      await api.bulkAlbumAction({ album_ids: [album.id], action: { tag: 'TriggerSearch' } });
      toast.success('Search triggered for album');
    } catch (err) {
      handleApiError(err, 'Failed to trigger search');
    }
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center py-24">
        <Loader2 className="h-8 w-8 animate-spin text-dark-accent" />
      </div>
    );
  }

  if (error || !album) {
    return (
      <div className="space-y-6 animate-fade-in">
        <button
          onClick={() => navigate('/albums')}
          className="flex items-center gap-2 text-dark-text-secondary hover:text-dark-text transition-colors"
        >
          <ArrowLeft className="h-4 w-4" />
          Back to Albums
        </button>
        <div className="card p-12 text-center">
          <AlertCircle className="mx-auto h-12 w-12 text-dark-error" />
          <h3 className="mt-4 text-lg font-medium text-dark-text">{error || 'Album not found'}</h3>
          <p className="mt-2 text-sm text-dark-text-secondary">
            The album you're looking for doesn't exist or couldn't be loaded.
          </p>
        </div>
      </div>
    );
  }

  const coverUrl = album.cover_url || album.cover_thumbnail_url;
  const isTracked = album.quality_profile_id !== null || album.current_quality !== null;

  // Merge the MusicBrainz tracklist with library files so each track can show
  // whether the user has it, in what quality, and where. Match on the recording
  // MBID first, then fall back to disc/track position.
  const hasLibrary = libraryTracks.length > 0;
  const libByRecording = new Map<string, LibraryTrack>();
  const libByPosition = new Map<string, LibraryTrack>();
  libraryTracks.forEach((lt) => {
    if (lt.mb_recording_id) libByRecording.set(lt.mb_recording_id, lt);
    if (lt.track_number != null) libByPosition.set(`${lt.disc_number ?? 1}-${lt.track_number}`, lt);
  });

  const usedLibIds = new Set<number>();
  const mergedTracks = (tracks?.tracks || []).map((t) => {
    let lib = t.recording_mbid ? libByRecording.get(t.recording_mbid) : undefined;
    if (!lib) lib = libByPosition.get(`${t.disc_number}-${t.position}`);
    if (lib) usedLibIds.add(lib.id);
    return { mb: t, lib };
  });
  // Library files that didn't line up with any tracklist entry (e.g. bonus tracks)
  const extraLibTracks = libraryTracks.filter((lt) => !usedLibIds.has(lt.id));
  const multiDisc = mergedTracks.some(({ mb }) => mb.disc_number > 1);
  const fileName = (path: string) => path.split('/').pop() || path;

  return (
    <div className="space-y-6 animate-fade-in">
      {/* Back button */}
      <button
        onClick={() => navigate('/albums')}
        className="flex items-center gap-2 text-dark-text-secondary hover:text-dark-text transition-colors"
      >
        <ArrowLeft className="h-4 w-4" />
        Back to Albums
      </button>

      {/* Album Header */}
      <div className="card p-6">
        <div className="flex flex-col sm:flex-row gap-6">
          {/* Cover */}
          <div className="flex-shrink-0">
            {coverUrl ? (
              <img
                src={coverUrl}
                alt={album.title}
                loading="lazy"
                decoding="async"
                className="w-48 h-48 object-cover rounded-lg ring-2 ring-dark-border"
                onError={(e) => {
                  (e.target as HTMLImageElement).style.display = 'none';
                  (e.target as HTMLImageElement).nextElementSibling?.classList.remove('hidden');
                }}
              />
            ) : null}
            <div className={`w-48 h-48 bg-dark-bg-subtle rounded-lg flex items-center justify-center ring-2 ring-dark-border ${coverUrl ? 'hidden' : ''}`}>
              <Disc className="w-24 h-24 text-dark-text-tertiary" />
            </div>
          </div>

          {/* Info */}
          <div className="flex-1 min-w-0">
            <div className="flex flex-col sm:flex-row sm:items-start justify-between gap-4">
              <div className="flex-1 min-w-0">
                <h1 className="text-3xl font-bold text-dark-text break-words">{album.title}</h1>
                <div className="mt-2 text-dark-text-secondary">
                  {album.artist_id ? (
                    <Link to={`/artists/${album.artist_id}`} className="link">
                      {album.artist_name}
                    </Link>
                  ) : (
                    <span>{album.artist_name}</span>
                  )}
                </div>
                <div className="mt-3 flex flex-wrap items-center gap-2 text-sm">
                  {album.type && (
                    <span className="px-2 py-0.5 bg-dark-bg-subtle rounded text-xs text-dark-text-secondary">
                      {album.type}
                    </span>
                  )}
                  <span className="text-dark-text-secondary text-xs">{formatDate(album.first_release_date)}</span>
                  {album.current_quality && (
                    <span className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium border ${getQualityBadgeStyle(album.current_quality)}`}>
                      {formatQuality(album.current_quality)}
                    </span>
                  )}
                  <span className="px-2 py-0.5 rounded-full text-xs font-medium bg-dark-bg-subtle text-dark-text-secondary">
                    {album.state}
                  </span>
                </div>
              </div>
            </div>

            {/* Actions */}
            <div className="mt-6 flex flex-wrap items-center gap-3">
              {/* Target quality selector */}
              {qualityProfiles.length > 0 && (
                <select
                  value={album.quality_profile_id?.toString()
                    || ((album.state === 'InLibrary' || album.state === 'Monitored') ? 'existing' : 'default')}
                  onChange={(e) => handleQualityProfileChange(e.target.value)}
                  className="input max-w-xs text-sm"
                  title="Target quality profile"
                >
                  {(album.state === 'InLibrary' || album.state === 'Monitored') && !album.quality_profile_id && (
                    <option value="existing">Existing (keep current)</option>
                  )}
                  <option value="default">
                    {defaultProfile ? `Default (${defaultProfile.name})` : 'Default'}
                  </option>
                  {qualityProfiles.map((profile) => (
                    <option key={profile.id} value={profile.id.toString()}>
                      {profile.name}
                    </option>
                  ))}
                </select>
              )}

              {album.quality_profile_id ? (
                <>
                  <button
                    onClick={handleForceSearch}
                    className="flex items-center gap-2 px-4 py-2 bg-dark-bg-hover hover:bg-dark-accent-muted text-dark-text-secondary hover:text-dark-accent rounded-lg transition-colors"
                  >
                    <SearchIcon className="h-4 w-4" />
                    Search
                  </button>
                  <button
                    onClick={handleUnwant}
                    className="flex items-center gap-2 px-4 py-2 bg-dark-bg-hover hover:bg-dark-error-muted text-dark-text-secondary hover:text-dark-error rounded-lg transition-colors"
                  >
                    <X className="h-4 w-4" />
                    Unwant
                  </button>
                </>
              ) : (
                !isTracked && (
                  <button
                    onClick={handleWant}
                    className="flex items-center gap-2 px-4 py-2 bg-dark-accent hover:bg-dark-accent/80 text-dark-bg rounded-lg transition-colors"
                  >
                    <Music className="h-4 w-4" />
                    Want
                  </button>
                )
              )}
            </div>

            {/* External link */}
            <div className="mt-6">
              <a
                href={`https://musicbrainz.org/release-group/${album.release_group_mbid}`}
                target="_blank"
                rel="noopener noreferrer"
                className="link text-sm flex items-center gap-2 w-fit"
              >
                <ExternalLink className="h-4 w-4" />
                View on MusicBrainz
              </a>
            </div>
          </div>
        </div>
      </div>

      {/* Tracklist (merged with library files) */}
      <div className="card p-6">
        <div className="flex items-center justify-between mb-4">
          <h2 className="text-xl font-bold text-dark-text flex items-center gap-2">
            <ListMusic className="h-5 w-5" />
            Tracklist
          </h2>
          <div className="flex items-center gap-3">
            {tracks?.release_title && (
              <span className="text-xs text-dark-text-tertiary">
                {tracks.is_matched_release ? 'Your edition: ' : 'Edition: '}{tracks.release_title}
              </span>
            )}
            {album.matched_cluster_id && (
              <Link to={`/identification/clusters?cluster=${album.matched_cluster_id}`} className="link text-xs">
                View cluster
              </Link>
            )}
          </div>
        </div>
        {tracksLoading ? (
          <div className="flex items-center gap-2 text-sm text-dark-text-secondary py-6">
            <Loader2 className="h-4 w-4 animate-spin text-dark-accent" />
            Loading tracklist from MusicBrainz…
          </div>
        ) : mergedTracks.length === 0 && extraLibTracks.length === 0 ? (
          <p className="text-sm text-dark-text-secondary py-4">No tracklist available for this album.</p>
        ) : (
          <div className="divide-y divide-dark-border">
            {mergedTracks.map(({ mb, lib }) => (
              <div
                key={`${mb.disc_number}-${mb.position}-${mb.recording_mbid}`}
                className="flex items-center gap-4 py-2"
              >
                {/* Have indicator */}
                <span className="w-4 flex-shrink-0">
                  {hasLibrary && (lib
                    ? <CheckCircle2 className="h-4 w-4 text-green-500" />
                    : <Circle className="h-4 w-4 text-dark-text-tertiary/40" />)}
                </span>
                <span className="w-10 text-right text-sm text-dark-text-tertiary tabular-nums flex-shrink-0">
                  {multiDisc ? `${mb.disc_number}-${mb.position}` : mb.position}
                </span>
                <div className="flex-1 min-w-0">
                  <div className={`text-sm truncate ${hasLibrary && !lib ? 'text-dark-text-tertiary' : 'text-dark-text'}`}>
                    {mb.title}
                  </div>
                  {lib && (
                    <div className="text-xs text-dark-text-tertiary truncate" title={lib.path}>
                      {fileName(lib.path)}
                    </div>
                  )}
                  {!lib && mb.artist && (
                    <div className="text-xs text-dark-text-tertiary truncate">{mb.artist}</div>
                  )}
                </div>
                {lib?.format && (
                  <span className={`inline-flex items-center px-2 py-0.5 rounded-full text-xs font-medium border ${qualityBadgeForFormat(lib.format)} flex-shrink-0`}>
                    {lib.format}
                  </span>
                )}
                <span className="text-sm text-dark-text-tertiary tabular-nums flex-shrink-0 w-12 text-right">
                  {formatTrackDurationMs(mb.length_ms ?? (lib?.duration != null ? Math.round(lib.duration * 1000) : null))}
                </span>
              </div>
            ))}

            {/* Library files with no matching tracklist entry (e.g. bonus tracks) */}
            {extraLibTracks.map((lt) => (
              <div key={`extra-${lt.id}`} className="flex items-center gap-4 py-2">
                <span className="w-4 flex-shrink-0">
                  <CheckCircle2 className="h-4 w-4 text-green-500" />
                </span>
                <span className="w-10 text-right text-sm text-dark-text-tertiary tabular-nums flex-shrink-0">
                  {lt.track_number ?? '•'}
                </span>
                <div className="flex-1 min-w-0">
                  <div className="text-sm text-dark-text truncate">{lt.title || fileName(lt.path)}</div>
                  <div className="text-xs text-dark-text-tertiary truncate" title={lt.path}>
                    {fileName(lt.path)} <span className="text-dark-text-tertiary/60">· not in this edition</span>
                  </div>
                </div>
                {lt.format && (
                  <span className={`inline-flex items-center px-2 py-0.5 rounded-full text-xs font-medium border ${qualityBadgeForFormat(lt.format)} flex-shrink-0`}>
                    {lt.format}
                  </span>
                )}
                <span className="text-sm text-dark-text-tertiary tabular-nums flex-shrink-0 w-12 text-right">
                  {formatTrackDuration(lt.duration)}
                </span>
              </div>
            ))}
          </div>
        )}
      </div>

      {/* Available releases (live indexer/slskd search) */}
      <div className="card p-6">
        <div className="flex items-center justify-between mb-4">
          <h2 className="text-xl font-bold text-dark-text flex items-center gap-2">
            <DownloadIcon className="h-5 w-5" />
            Available Releases
          </h2>
          {showReleasesSearch ? (
            <button
              onClick={() => setShowReleasesSearch(false)}
              className="text-sm text-dark-text-secondary hover:text-dark-text transition-colors flex items-center gap-1.5"
            >
              <X className="h-4 w-4" />
              Hide
            </button>
          ) : (
            <button
              onClick={() => setShowReleasesSearch(true)}
              className="flex items-center gap-2 px-3 py-1.5 bg-dark-bg-hover hover:bg-dark-accent-muted text-dark-text-secondary hover:text-dark-accent rounded transition-colors text-sm"
            >
              <SearchIcon className="h-4 w-4" />
              Search indexers
            </button>
          )}
        </div>
        {showReleasesSearch ? (
          <AlbumReleasesList
            albumId={album.id}
            albumTitle={album.title}
            artistName={album.artist_name}
            onQueued={() => { setShowReleasesSearch(false); loadDownloads(); reloadAlbum(); }}
          />
        ) : (
          <p className="text-sm text-dark-text-secondary">
            Search configured indexers and Soulseek for downloadable releases of this album.
          </p>
        )}
      </div>

      {/* Download history */}
      {downloads.length > 0 && (
        <div className="card p-6">
          <h2 className="text-xl font-bold text-dark-text mb-4">Download History</h2>
          <div className="overflow-x-auto">
            <table className="min-w-full divide-y divide-dark-border">
              <thead>
                <tr className="text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">
                  <th className="py-2 pr-4">Title</th>
                  <th className="py-2 pr-4">Status</th>
                  <th className="py-2 pr-4">Quality</th>
                  <th className="py-2 pr-4">Size</th>
                  <th className="py-2 pr-4">When</th>
                </tr>
              </thead>
              <tbody className="divide-y divide-dark-border">
                {downloads.map((dl) => (
                  <tr key={dl.id} className="text-sm">
                    <td className="py-2 pr-4 max-w-md">
                      <div className="text-dark-text truncate" title={dl.title}>{dl.title}</div>
                      <div className="text-xs text-dark-text-tertiary">{dl.indexer_name}</div>
                      {dl.error_message && (
                        <div className="text-xs text-red-400 truncate" title={dl.error_message}>{dl.error_message}</div>
                      )}
                    </td>
                    <td className="py-2 pr-4">
                      <span className={`font-medium ${DOWNLOAD_STATUS_STYLES[dl.status] || 'text-dark-text-secondary'}`}>
                        {dl.status}
                      </span>
                      {dl.status === 'downloading' && (
                        <span className="text-xs text-dark-text-tertiary ml-2">{Math.round(dl.progress)}%</span>
                      )}
                    </td>
                    <td className="py-2 pr-4 text-dark-text-secondary">{dl.quality ? formatQuality(dl.quality) : '—'}</td>
                    <td className="py-2 pr-4 text-dark-text-secondary">{dl.size_bytes ? formatBytes(dl.size_bytes) : '—'}</td>
                    <td className="py-2 pr-4 text-dark-text-tertiary">
                      {dl.imported_at || dl.completed_at || dl.queued_at
                        ? formatTimeAgo((dl.imported_at || dl.completed_at || dl.queued_at)!)
                        : '—'}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      )}
    </div>
  );
}
