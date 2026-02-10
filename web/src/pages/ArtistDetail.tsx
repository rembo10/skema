import { useEffect, useState, useMemo } from 'react';
import { useParams, useNavigate, Link } from 'react-router-dom';
import { api } from '../lib/api';
import { formatDate } from '../lib/formatters';
import type { CatalogArtist, CatalogAlbum, QualityProfile } from '../types/api';
import { Music, ExternalLink, Calendar, ArrowLeft, Disc, UserMinus, AlertCircle, RefreshCw, X, Award } from 'lucide-react';
import toast from 'react-hot-toast';
import { useAppStore } from '../store';

export default function ArtistDetail() {
  const { id } = useParams<{ id: string }>();
  const navigate = useNavigate();
  const catalogAlbums = useAppStore((state) => state.catalogAlbums);
  const followedArtists = useAppStore((state) => state.followedArtists);
  const setCatalogAlbums = useAppStore((state) => state.setCatalogAlbums);
  const updateCatalogAlbum = useAppStore((state) => state.updateCatalogAlbum);
  const updateFollowedArtist = useAppStore((state) => state.updateFollowedArtist);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [refreshing, setRefreshing] = useState(false);
  const [qualityProfiles, setQualityProfiles] = useState<QualityProfile[]>([]);
  const [defaultProfile, setDefaultProfile] = useState<QualityProfile | null>(null);
  const [localArtist, setLocalArtist] = useState<CatalogArtist | null>(null);

  // Get artist from store or local state
  const artist = useMemo(() => {
    if (!id) return null;
    const artistId = parseInt(id, 10);
    if (isNaN(artistId)) return null;

    // Prefer the followed artist from the store (always up-to-date)
    const storeArtist = followedArtists.find(a => a.id === artistId);
    return storeArtist || localArtist;
  }, [id, followedArtists, localArtist]);

  // Filter albums for the current artist
  const albums = useMemo(() => {
    if (!artist) return [];
    return catalogAlbums.filter(album => album.artist_mbid === artist.mbid);
  }, [catalogAlbums, artist]);

  useEffect(() => {
    loadArtistData();
  }, [id]);

  useEffect(() => {
    loadQualityProfiles();
  }, []);

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
      console.error('Error loading quality profiles:', err);
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

      // Load all artists to find the one we need
      const artistsResponse = await api.getCatalogArtists(0, 1000);
      const foundArtist = artistsResponse.artists.find(a => a.id === artistId);

      if (!foundArtist) {
        setError('Artist not found');
        setLoading(false);
        return;
      }

      // Load albums for this artist using internal ID
      const albumsData = await api.getCatalogAlbums(undefined, foundArtist.id);

      setLocalArtist(foundArtist);

      // Merge albums into the global store (don't replace all albums, just add new ones)
      const newAlbums = albumsData.filter(album =>
        !catalogAlbums.some(a => a.release_group_mbid === album.release_group_mbid)
      );
      if (newAlbums.length > 0) {
        setCatalogAlbums([...catalogAlbums, ...newAlbums]);
      }
    } catch (err) {
      console.error('Error loading artist data:', err);
      setError('Failed to load artist data');
      toast.error('Failed to load artist data');
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
      toast.error('Failed to unfollow artist');
      console.error('Error unfollowing artist:', error);
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

      // Update global store
      updateCatalogAlbum(album.id, { wanted: newWantedStatus, quality_profile_id: profileToUse });

      toast.success(newWantedStatus ? `Added ${album.title} to wanted list` : `Removed ${album.title} from wanted list`);
    } catch (error) {
      toast.error('Failed to update album status');
      console.error('Error updating album:', error);
    }
  };

  const handleAlbumQualityProfileChange = async (album: CatalogAlbum, profileId: string) => {
    if (!album.id) return;

    try {
      const newProfileId = profileId === '' ? null : parseInt(profileId, 10);
      const updatedAlbum = await api.updateCatalogAlbum(album.id, newProfileId);

      // Update global store
      updateCatalogAlbum(album.id, { quality_profile_id: updatedAlbum.quality_profile_id });

      toast.success(newProfileId === null ? 'Using artist/default quality profile' : 'Album quality profile updated');
    } catch (error) {
      toast.error('Failed to update quality profile');
      console.error('Error updating album quality profile:', error);
    }
  };

  const handleRefreshCatalog = async () => {
    if (!artist?.id) return;

    try {
      setRefreshing(true);
      const result = await api.refreshCatalogArtist(artist.id);
      toast.success(result.message);
      // Reload artist data to get new albums
      setTimeout(() => loadArtistData(), 2000);
    } catch (error) {
      toast.error('Failed to refresh catalog');
      console.error('Error refreshing catalog:', error);
    } finally {
      setRefreshing(false);
    }
  };

  const handleQualityProfileChange = async (profileId: string) => {
    if (!artist?.id) return;

    try {
      const newProfileId = profileId === '' ? null : parseInt(profileId, 10);
      const updatedArtist = await api.updateCatalogArtist(artist.id, artist.followed, newProfileId);

      // Update both local state and the global store
      setLocalArtist(updatedArtist);
      updateFollowedArtist(artist.id, { quality_profile_id: updatedArtist.quality_profile_id });

      toast.success(newProfileId === null ? 'Using default quality profile' : 'Quality profile updated');
    } catch (error) {
      toast.error('Failed to update quality profile');
      console.error('Error updating quality profile:', error);
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
    return (
      <div className="flex items-center justify-center h-64">
        <div className="animate-spin rounded-full h-12 w-12 border-2 border-dark-border border-t-dark-accent"></div>
      </div>
    );
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

            {/* External Links */}
            <div className="mt-6 flex gap-4">
              <a
                href={`https://musicbrainz.org/artist/${artist.mbid}`}
                target="_blank"
                rel="noopener noreferrer"
                className="link text-sm flex items-center gap-2"
              >
                <ExternalLink className="h-4 w-4" />
                View on MusicBrainz
              </a>
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
