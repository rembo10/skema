import { useEffect, useState } from 'react';
import { useParams, useNavigate } from 'react-router-dom';
import { api } from '../lib/api';
import { formatDate, formatTimeAgo } from '../lib/formatters';
import type { CatalogArtist, CatalogAlbum, QualityProfile } from '../types/api';
import { Music, ExternalLink, Calendar, ArrowLeft, Disc, UserMinus, AlertCircle, RefreshCw, X, Award } from 'lucide-react';
import toast from 'react-hot-toast';
import { handleApiError } from '../lib/errors';
import { useSSEEvent } from '../hooks/useSSEEvent';
import { ArtistDetailSkeleton } from '../components/LoadingSkeleton';

export default function ArtistDetail() {
  const { id } = useParams<{ id: string }>();
  const navigate = useNavigate();
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [refreshing, setRefreshing] = useState(false);
  const [qualityProfiles, setQualityProfiles] = useState<QualityProfile[]>([]);
  const [defaultProfile, setDefaultProfile] = useState<QualityProfile | null>(null);
  const [localArtist, setLocalArtist] = useState<CatalogArtist | null>(null);
  const [albums, setAlbums] = useState<CatalogAlbum[]>([]);

  const artist = localArtist;

  useEffect(() => {
    loadArtistData();
  }, [id]);

  useEffect(() => {
    loadQualityProfiles();
  }, []);

  // SSE: bio update for this artist
  useSSEEvent<{ artist_id: number; bio: string }>('ArtistBioFetched', (data) => {
    if (data.artist_id === parseInt(id || '', 10)) {
      setLocalArtist(prev => prev ? { ...prev, bio: data.bio } : prev);
    }
  });

  // SSE: new album added — add if matching artist
  useSSEEvent<{ album_id: number; release_group_mbid: string; album_title: string; artist_mbid: string; artist_name: string; album_type: string | null; first_release_date: string | null; wanted: boolean }>('CatalogAlbumAdded', (data) => {
    if (!artist || data.artist_mbid !== artist.mbid) return;
    setAlbums(prev => {
      if (prev.some(a => a.release_group_mbid === data.release_group_mbid)) return prev;
      return [...prev, {
        id: data.album_id,
        release_group_mbid: data.release_group_mbid,
        title: data.album_title,
        artist_mbid: data.artist_mbid,
        artist_name: data.artist_name,
        type: data.album_type,
        first_release_date: data.first_release_date,
        cover_url: null,
        cover_thumbnail_url: null,
        wanted: data.wanted,
        matched_cluster_id: null,
        score: null,
        quality_profile_id: null,
        created_at: new Date().toISOString(),
        updated_at: new Date().toISOString(),
      }];
    });
  });

  // SSE: album cover fetched
  useSSEEvent<{ release_group_mbid: string; cover_url: string; thumbnail_url: string | null }>('AlbumCoverFetched', (data) => {
    setAlbums(prev => prev.map(a =>
      a.release_group_mbid === data.release_group_mbid
        ? { ...a, cover_url: data.cover_url, cover_thumbnail_url: data.thumbnail_url }
        : a
    ));
  });

  // SSE: album updated
  useSSEEvent<{ album_id: number; album_title: string; artist_name: string; album_type: string | null; first_release_date: string | null; quality_profile_id: number | null }>('CatalogAlbumUpdated', (data) => {
    setAlbums(prev => prev.map(a =>
      a.id === data.album_id
        ? { ...a, title: data.album_title, artist_name: data.artist_name, type: data.album_type, first_release_date: data.first_release_date, quality_profile_id: data.quality_profile_id, wanted: data.quality_profile_id != null }
        : a
    ));
  });

  // SSE: artist discography fetched
  useSSEEvent<{ artist_id: number; last_checked_at: string }>('ArtistDiscographyFetched', (data) => {
    if (data.artist_id === parseInt(id || '', 10) && data.last_checked_at) {
      setLocalArtist(prev => prev ? { ...prev, last_checked_at: data.last_checked_at } : prev);
    }
  });

  // SSE: artist image fetched
  useSSEEvent<{ artist_id: number; image_url: string; thumbnail_url: string | null }>('ArtistImageFetched', (data) => {
    if (data.artist_id === parseInt(id || '', 10)) {
      setLocalArtist(prev => prev ? { ...prev, image_url: data.image_url, thumbnail_url: data.thumbnail_url } : prev);
    }
  });

  // Update document title when artist is loaded
  useEffect(() => {
    if (artist?.name) {
      document.title = `${artist.name} :: skema`;
    }
  }, [artist?.name]);

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

  const loadArtistData = async () => {
    if (!id) {
      setError('No artist ID provided');
      setLoading(false);
      return;
    }

    const artistId = parseInt(id, 10);
    if (isNaN(artistId)) {
      setError('Invalid artist ID');
      setLoading(false);
      return;
    }

    try {
      setLoading(true);
      setError(null);

      // Load single artist by ID
      const foundArtist = await api.getCatalogArtist(artistId);

      if (!foundArtist) {
        setError('Artist not found');
        setLoading(false);
        return;
      }

      // Load albums for this artist using internal ID
      const albumsData = await api.getCatalogAlbums(undefined, foundArtist.id ?? undefined);

      setLocalArtist(foundArtist);
      setAlbums(albumsData);
    } catch (err) {
      if (!handleApiError(err, 'Failed to load artist data')) {
        setError('Failed to load artist data');
      }
    } finally {
      setLoading(false);
    }
  };

  const handleUnfollowArtist = async () => {
    if (!artist?.id) return;

    try {
      await api.updateCatalogArtist(artist.id, false);
      toast.success(`Unfollowed ${artist.name}`);
      navigate('/artists');
    } catch (error) {
      handleApiError(error, 'Failed to unfollow artist');
    }
  };

  const handleToggleWanted = async (album: CatalogAlbum) => {
    if (!album.id) return;

    try {
      const newWantedStatus = !album.wanted;
      // Toggle wanted by setting/clearing quality profile
      // If wanting: use album's profile, or artist's profile, or default profile
      // If unwanting: set to null
      let profileToUse: number | null = null;
      if (newWantedStatus) {
        profileToUse = album.quality_profile_id || artist?.quality_profile_id || defaultProfile?.id || null;
        if (!profileToUse) {
          toast.error('No quality profile available. Please set a default quality profile first.');
          return;
        }
      }

      await api.updateCatalogAlbum(album.id, profileToUse);

      // Update local state
      setAlbums(prev => prev.map(a =>
        a.id === album.id ? { ...a, wanted: newWantedStatus, quality_profile_id: profileToUse } : a
      ));

      toast.success(newWantedStatus ? `Added ${album.title} to wanted list` : `Removed ${album.title} from wanted list`);
    } catch (error) {
      handleApiError(error, 'Failed to update album status');
    }
  };

  const handleAlbumQualityProfileChange = async (album: CatalogAlbum, profileId: string) => {
    if (!album.id) return;

    try {
      const newProfileId = profileId === '' ? null : parseInt(profileId, 10);
      const updatedAlbum = await api.updateCatalogAlbum(album.id, newProfileId);

      // Update local state
      setAlbums(prev => prev.map(a =>
        a.id === album.id ? { ...a, quality_profile_id: updatedAlbum.quality_profile_id } : a
      ));

      toast.success(newProfileId === null ? 'Using artist/default quality profile' : 'Album quality profile updated');
    } catch (error) {
      handleApiError(error, 'Failed to update quality profile');
    }
  };

  const handleRefreshCatalog = async () => {
    if (!artist?.id) return;

    try {
      setRefreshing(true);
      const result = await api.refreshCatalogArtist(artist.id);
      toast.success(result.message);
      // New albums, bio, images etc. are all pushed via SSE events
    } catch (error) {
      handleApiError(error, 'Failed to refresh catalog');
    } finally {
      setRefreshing(false);
    }
  };

  const handleQualityProfileChange = async (profileId: string) => {
    if (!artist?.id) return;

    try {
      const newProfileId = profileId === '' ? null : parseInt(profileId, 10);
      const updatedArtist = await api.updateCatalogArtist(artist.id, artist.followed, newProfileId);

      // Update local state
      setLocalArtist(updatedArtist);

      toast.success(newProfileId === null ? 'Using default quality profile' : 'Quality profile updated');

      // Offer to propagate to existing albums
      if (newProfileId !== null && albums.length > 0) {
        const albumIds = albums.filter(a => a.id !== null && a.id !== undefined).map(a => a.id!);
        if (albumIds.length > 0) {
          toast(
            (t) => (
              <div className="flex flex-col gap-2">
                <span className="text-sm">Update quality profile for {albumIds.length} album{albumIds.length !== 1 ? 's' : ''} by this artist?</span>
                <div className="flex gap-2">
                  <button
                    className="px-3 py-1 text-xs font-medium bg-dark-accent text-dark-bg rounded hover:bg-dark-accent/80"
                    onClick={async () => {
                      toast.dismiss(t.id);
                      try {
                        await api.bulkAlbumAction({
                          album_ids: albumIds,
                          action: { tag: 'SetQualityProfile', contents: newProfileId },
                        });
                        // Update local album state
                        setAlbums(prev => prev.map(a =>
                          albumIds.includes(a.id!) ? { ...a, quality_profile_id: newProfileId } : a
                        ));
                        toast.success(`Updated ${albumIds.length} album${albumIds.length !== 1 ? 's' : ''}`);
                      } catch {
                        toast.error('Failed to update album profiles');
                      }
                    }}
                  >
                    Update All
                  </button>
                  <button
                    className="px-3 py-1 text-xs font-medium bg-dark-bg-hover text-dark-text rounded hover:bg-dark-bg-subtle"
                    onClick={() => toast.dismiss(t.id)}
                  >
                    Skip
                  </button>
                </div>
              </div>
            ),
            { duration: 10000 }
          );
        }
      }
    } catch (error) {
      handleApiError(error, 'Failed to update quality profile');
    }
  };

  const groupAlbumsByYear = (albums: CatalogAlbum[]) => {
    const grouped = new Map<string, CatalogAlbum[]>();

    albums.forEach(album => {
      const year = album.first_release_date
        ? new Date(album.first_release_date).getFullYear().toString()
        : 'Unknown';

      if (!grouped.has(year)) {
        grouped.set(year, []);
      }
      grouped.get(year)!.push(album);
    });

    // Sort years descending (most recent first), but keep 'Unknown' at the end
    const sortedYears = Array.from(grouped.keys()).sort((a, b) => {
      if (a === 'Unknown') return 1;
      if (b === 'Unknown') return -1;
      return parseInt(b) - parseInt(a);
    });

    return sortedYears.map(year => ({
      year,
      albums: grouped.get(year)!,
    }));
  };

  if (loading) {
    return <ArtistDetailSkeleton />;
  }

  if (error || !artist) {
    return (
      <div className="space-y-6 animate-fade-in">
        <button
          onClick={() => navigate('/artists')}
          className="flex items-center gap-2 text-dark-text-secondary hover:text-dark-text transition-colors"
        >
          <ArrowLeft className="h-4 w-4" />
          Back to Artists
        </button>
        <div className="card p-12 text-center">
          <AlertCircle className="mx-auto h-12 w-12 text-dark-error" />
          <h3 className="mt-4 text-lg font-medium text-dark-text">{error || 'Artist not found'}</h3>
          <p className="mt-2 text-sm text-dark-text-secondary">
            The artist you're looking for doesn't exist or couldn't be loaded.
          </p>
        </div>
      </div>
    );
  }

  const albumStats = {
    total: albums.length,
    inLibrary: albums.filter(a => a.matched_cluster_id !== null).length,
    wanted: albums.filter(a => a.wanted).length,
  };

  const groupedAlbums = groupAlbumsByYear(albums);

  return (
    <div className="space-y-6 animate-fade-in">
      {/* Back button */}
      <button
        onClick={() => navigate('/artists')}
        className="flex items-center gap-2 text-dark-text-secondary hover:text-dark-text transition-colors"
      >
        <ArrowLeft className="h-4 w-4" />
        Back to Artists
      </button>

      {/* Artist Header */}
      <div className="card p-6">
        <div className="flex flex-col sm:flex-row gap-6">
          {/* Artist Image */}
          <div className="flex-shrink-0">
            {artist.image_url ? (
              <img
                src={artist.image_url}
                alt={artist.name}
                loading="lazy"
                decoding="async"
                className="w-48 h-48 object-cover rounded-lg ring-2 ring-dark-border"
                onError={(e) => {
                  (e.target as HTMLImageElement).style.display = 'none';
                  (e.target as HTMLImageElement).nextElementSibling?.classList.remove('hidden');
                }}
              />
            ) : null}
            <div className={`w-48 h-48 bg-dark-bg-subtle rounded-lg flex items-center justify-center ring-2 ring-dark-border ${artist.image_url ? 'hidden' : ''}`}>
              <Music className="w-24 h-24 text-dark-text-tertiary" />
            </div>
          </div>

          {/* Artist Info */}
          <div className="flex-1 min-w-0">
            <div className="flex flex-col sm:flex-row sm:items-start justify-between gap-4">
              <div className="flex-1 min-w-0">
                <h1 className="text-3xl font-bold text-dark-text break-words">{artist.name}</h1>
                {artist.type && (
                  <p className="mt-2 text-dark-text-secondary">{artist.type}</p>
                )}
                {artist.bio && (
                  <p className="mt-3 text-sm text-dark-text-secondary leading-relaxed">
                    {artist.bio}
                    {' '}
                    <a
                      href={`https://www.last.fm/music/${encodeURIComponent(artist.name)}`}
                      target="_blank"
                      rel="noopener noreferrer"
                      className="link"
                    >
                      Read more on Last.fm
                    </a>
                  </p>
                )}
              </div>

              <div className="flex gap-2">
                {artist.followed && (
                  <>
                    <button
                      onClick={handleRefreshCatalog}
                      disabled={refreshing}
                      className="flex items-center gap-2 px-4 py-2 bg-dark-accent hover:bg-dark-accent/80 text-dark-bg rounded-lg transition-all duration-200 disabled:opacity-50 disabled:cursor-not-allowed whitespace-nowrap"
                      title="Check for new releases"
                    >
                      <RefreshCw className={`h-4 w-4 ${refreshing ? 'animate-spin' : ''}`} />
                      {refreshing ? 'Refreshing...' : 'Refresh'}
                    </button>
                    <button
                      onClick={handleUnfollowArtist}
                      className="flex items-center gap-2 px-4 py-2 bg-dark-bg-hover hover:bg-dark-error-muted text-dark-text-secondary hover:text-dark-error rounded-lg transition-all duration-200 whitespace-nowrap"
                    >
                      <UserMinus className="h-4 w-4" />
                      Unfollow
                    </button>
                  </>
                )}
              </div>
            </div>

            {/* Stats */}
            <div className="mt-6 flex flex-wrap gap-6">
              <div className="flex items-center gap-3">
                <div className="w-12 h-12 bg-dark-accent/10 rounded-lg flex items-center justify-center">
                  <Disc className="h-6 w-6 text-dark-accent" />
                </div>
                <div>
                  <p className="text-2xl font-bold text-dark-text">{albumStats.total}</p>
                  <p className="text-sm text-dark-text-secondary">Total Albums</p>
                </div>
              </div>

              <div className="flex items-center gap-3">
                <div className="w-12 h-12 bg-dark-success/10 rounded-lg flex items-center justify-center">
                  <Music className="h-6 w-6 text-dark-success" />
                </div>
                <div>
                  <p className="text-2xl font-bold text-dark-text">{albumStats.inLibrary}</p>
                  <p className="text-sm text-dark-text-secondary">In Library</p>
                </div>
              </div>

              <div className="flex items-center gap-3">
                <div className="w-12 h-12 bg-dark-warning/10 rounded-lg flex items-center justify-center">
                  <Calendar className="h-6 w-6 text-dark-warning" />
                </div>
                <div>
                  <p className="text-2xl font-bold text-dark-text">{albumStats.wanted}</p>
                  <p className="text-sm text-dark-text-secondary">Wanted</p>
                </div>
              </div>
            </div>

            {/* Quality Profile Selector */}
            {artist.followed && qualityProfiles.length > 0 && (
              <div className="mt-6">
                <label className="block text-sm font-medium text-dark-text mb-2">
                  Quality Profile
                </label>
                <div className="flex items-center gap-3">
                  <div className="flex items-center gap-2 text-dark-text-secondary">
                    <Award className="h-5 w-5" />
                  </div>
                  <select
                    value={artist.quality_profile_id?.toString() || ''}
                    onChange={(e) => handleQualityProfileChange(e.target.value)}
                    className="input flex-1 max-w-xs"
                  >
                    <option value="">
                      Default ({defaultProfile?.name || 'None'})
                    </option>
                    {qualityProfiles.map((profile) => (
                      <option key={profile.id} value={profile.id.toString()}>
                        {profile.name}
                      </option>
                    ))}
                  </select>
                </div>
                <p className="mt-2 text-xs text-dark-text-secondary">
                  Sets the preferred quality for all albums by this artist
                </p>
              </div>
            )}

            {/* External Links & Last Checked */}
            <div className="mt-6 flex flex-wrap items-center gap-4">
              <a
                href={`https://musicbrainz.org/artist/${artist.mbid}`}
                target="_blank"
                rel="noopener noreferrer"
                className="link text-sm flex items-center gap-2"
              >
                <ExternalLink className="h-4 w-4" />
                View on MusicBrainz
              </a>
              {artist.last_checked_at && (
                <span className="text-xs text-dark-text-tertiary flex items-center gap-1.5" title={artist.last_checked_at}>
                  <RefreshCw className="h-3 w-3" />
                  Last checked {formatTimeAgo(artist.last_checked_at)}
                </span>
              )}
            </div>
          </div>
        </div>
      </div>

      {/* Discography */}
      <div className="space-y-6">
        <h2 className="text-2xl font-bold text-dark-text">Discography</h2>

        {albums.length === 0 ? (
          <div className="card p-12 text-center">
            <Disc className="mx-auto h-12 w-12 text-dark-text-tertiary" />
            <h3 className="mt-4 text-sm font-medium text-dark-text">No albums found</h3>
            <p className="mt-2 text-sm text-dark-text-secondary">
              This artist doesn't have any albums in the catalog yet.
            </p>
          </div>
        ) : (
          groupedAlbums.map(({ year, albums: yearAlbums }) => (
            <div key={year} className="space-y-3">
              <h3 className="text-lg font-semibold text-dark-text-secondary">{year}</h3>
              <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-4">
                {yearAlbums.map((album) => (
                  <div
                    key={album.release_group_mbid}
                    className="card-hover overflow-hidden group"
                  >
                    {/* Album Cover */}
                    <div className="aspect-square bg-dark-bg-subtle relative overflow-hidden">
                      {album.cover_thumbnail_url || album.cover_url ? (
                        <img
                          src={album.cover_thumbnail_url || album.cover_url || ''}
                          alt={album.title}
                          loading="lazy"
                          decoding="async"
                          className="w-full h-full object-cover group-hover:scale-105 transition-transform duration-300"
                          onError={(e) => {
                            (e.target as HTMLImageElement).style.display = 'none';
                            (e.target as HTMLImageElement).nextElementSibling?.classList.remove('hidden');
                          }}
                        />
                      ) : null}
                      <div className={`absolute inset-0 flex items-center justify-center ${album.cover_thumbnail_url || album.cover_url ? 'hidden' : ''}`}>
                        <Disc className="h-16 w-16 text-dark-text-tertiary" />
                      </div>

                      {/* Status Badge */}
                      {album.matched_cluster_id && (
                        <div className="absolute top-2 right-2 px-2 py-1 rounded-full text-xs font-medium bg-dark-success text-dark-bg">
                          In Library
                        </div>
                      )}
                    </div>

                    {/* Album Info */}
                    <div className="p-4">
                      <h4 className="font-semibold text-dark-text truncate group-hover:text-dark-accent transition-colors" title={album.title}>
                        {album.title}
                      </h4>
                      <div className="mt-2 flex items-center justify-between text-sm">
                        <div className="flex items-center gap-2 text-dark-text-secondary">
                          {album.type && (
                            <span className="px-2 py-0.5 bg-dark-bg-subtle rounded text-xs">
                              {album.type}
                            </span>
                          )}
                          <span className="text-xs">
                            {formatDate(album.first_release_date)}
                          </span>
                        </div>
                      </div>

                      {/* Actions */}
                      <div className="mt-3 space-y-2">
                        <div className="flex gap-2">
                          {!album.matched_cluster_id && (
                            <button
                              onClick={() => handleToggleWanted(album)}
                              className={`flex-1 px-3 py-2 rounded-lg text-sm font-medium transition-all duration-200 flex items-center justify-center gap-1.5 ${
                                album.wanted
                                  ? 'bg-dark-warning text-dark-bg hover:bg-dark-warning/80'
                                  : 'bg-dark-bg-hover text-dark-text hover:bg-dark-accent hover:text-dark-bg'
                              }`}
                              title={album.wanted ? 'Remove from wanted list' : 'Add to wanted list'}
                            >
                              {album.wanted && <X className="h-4 w-4" />}
                              {album.wanted ? 'Unwant' : 'Want'}
                            </button>
                          )}
                          <a
                            href={`https://musicbrainz.org/release-group/${album.release_group_mbid}`}
                            target="_blank"
                            rel="noopener noreferrer"
                            className="px-3 py-2 bg-dark-bg-hover hover:bg-dark-bg-subtle rounded-lg transition-colors flex items-center justify-center"
                            title="View on MusicBrainz"
                          >
                            <ExternalLink className="h-4 w-4 text-dark-text-secondary" />
                          </a>
                        </div>

                        {/* Quality Profile Selector */}
                        {qualityProfiles.length > 0 && (
                          <select
                            value={album.quality_profile_id?.toString() || ''}
                            onChange={(e) => handleAlbumQualityProfileChange(album, e.target.value)}
                            className="input w-full text-xs py-1.5"
                            title="Quality profile for this album"
                            onClick={(e) => e.stopPropagation()}
                          >
                            <option value="">
                              Default ({artist.quality_profile_id
                                ? qualityProfiles.find(p => p.id === artist.quality_profile_id)?.name
                                : defaultProfile?.name || 'None'})
                            </option>
                            {qualityProfiles.map((profile) => (
                              <option key={profile.id} value={profile.id.toString()}>
                                {profile.name}
                              </option>
                            ))}
                          </select>
                        )}
                      </div>
                    </div>
                  </div>
                ))}
              </div>
            </div>
          ))
        )}
      </div>
    </div>
  );
}
