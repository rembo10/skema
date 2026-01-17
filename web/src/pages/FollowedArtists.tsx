import { useEffect, useState, useMemo } from 'react';
import { Link } from 'react-router-dom';
import { api } from '../lib/api';
import type { CatalogArtist } from '../types/api';
import { Music, ExternalLink, Calendar, Disc, UserMinus, RefreshCw, ChevronLeft, ChevronRight } from 'lucide-react';
import toast from 'react-hot-toast';
import { useAppStore } from '../store';

const ITEMS_PER_PAGE = 50;

export default function FollowedArtists() {
  // Use global store for real-time updates
  const followedArtists = useAppStore((state) => state.followedArtists);
  const catalogAlbums = useAppStore((state) => state.catalogAlbums);
  const setFollowedArtists = useAppStore((state) => state.setFollowedArtists);
  const setCatalogAlbums = useAppStore((state) => state.setCatalogAlbums);
  const removeFollowedArtist = useAppStore((state) => state.removeFollowedArtist);

  const [loading, setLoading] = useState(true);
  const [searchQuery, setSearchQuery] = useState('');
  const [refreshing, setRefreshing] = useState(false);
  const [offset, setOffset] = useState(0);

  useEffect(() => {
    loadData();
  }, []);

  const loadData = async () => {
    try {
      setLoading(true);
      // Load followed artists with embedded album data
      const artistsResponse = await api.getCatalogArtists(0, 1000, true);
      setFollowedArtists(artistsResponse.artists);

      // Collect all albums from all artists into the global store
      const allAlbums = artistsResponse.artists.flatMap(artist =>
        (artist.albums || []).map(album => ({
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
        }))
      );
      setCatalogAlbums(allAlbums);
    } catch (error) {
      toast.error('Failed to load followed artists');
      console.error('Error loading artists:', error);
    } finally {
      setLoading(false);
    }
  };

  // Client-side pagination and filtering
  const filteredArtists = useMemo(() => {
    return followedArtists.filter(artist =>
      artist.name.toLowerCase().includes(searchQuery.toLowerCase())
    );
  }, [followedArtists, searchQuery]);

  const paginatedArtists = useMemo(() => {
    return filteredArtists.slice(offset, offset + ITEMS_PER_PAGE);
  }, [filteredArtists, offset]);

  const totalCount = filteredArtists.length;

  const getAlbumsForArtist = (artistMBID: string) => {
    return catalogAlbums.filter(album => album.artist_mbid === artistMBID);
  };

  const handleUnfollowArtist = async (artist: CatalogArtist) => {
    if (!artist.id) return;

    try {
      await api.updateCatalogArtist(artist.id, false);
      // Remove from global store
      removeFollowedArtist(artist.id);
      toast.success(`Unfollowed ${artist.name}`);
    } catch (error) {
      toast.error('Failed to unfollow artist');
      console.error('Error unfollowing artist:', error);
    }
  };

  const handleRefreshAll = async () => {
    try {
      setRefreshing(true);
      const result = await api.refreshAllCatalogArtists();
      toast.success(result.message);
    } catch (error) {
      toast.error('Failed to refresh catalog');
      console.error('Error refreshing catalog:', error);
    } finally {
      setRefreshing(false);
    }
  };


  const formatDate = (dateString: string | null) => {
    if (!dateString) return 'Unknown';
    return new Date(dateString).toLocaleDateString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
    });
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center h-64">
        <div className="animate-spin rounded-full h-12 w-12 border-2 border-dark-border border-t-dark-accent"></div>
      </div>
    );
  }

  return (
    <div className="space-y-6 animate-fade-in">
      {/* Header */}
      <div className="flex justify-between items-start">
        <div>
          <h1 className="text-3xl font-bold text-dark-text">Followed Artists</h1>
          <p className="text-dark-text-secondary mt-2">
            {followedArtists.length} followed artist{followedArtists.length !== 1 ? 's' : ''} • {catalogAlbums.filter(a => a.wanted).length} wanted album{catalogAlbums.filter(a => a.wanted).length !== 1 ? 's' : ''}
          </p>
        </div>
        {followedArtists.length > 0 && (
          <button
            onClick={handleRefreshAll}
            disabled={refreshing}
            className="flex items-center gap-2 px-4 py-2 bg-dark-accent hover:bg-dark-accent/80 text-dark-bg rounded-lg transition-all duration-200 disabled:opacity-50 disabled:cursor-not-allowed"
            title="Check for new releases from all followed artists"
          >
            <RefreshCw className={`h-4 w-4 ${refreshing ? 'animate-spin' : ''}`} />
            {refreshing ? 'Refreshing...' : 'Refresh All'}
          </button>
        )}
      </div>

      {/* Search */}
      <div className="max-w-md">
        <input
          type="text"
          placeholder="Search artists..."
          value={searchQuery}
          onChange={(e) => setSearchQuery(e.target.value)}
          className="input w-full"
        />
      </div>

      {/* Artists Grid */}
      {paginatedArtists.length === 0 ? (
        <div className="card p-12 text-center">
          <Music className="mx-auto h-12 w-12 text-dark-text-tertiary" />
          <h3 className="mt-4 text-sm font-medium text-dark-text">No followed artists</h3>
          <p className="mt-2 text-sm text-dark-text-secondary">
            {searchQuery ? 'Try a different search term' : 'Use the universal search to find and follow artists'}
          </p>
        </div>
      ) : (
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-4">
          {paginatedArtists.map((artist) => {
            const artistAlbums = getAlbumsForArtist(artist.mbid);
            const wantedCount = artistAlbums.filter(a => a.wanted).length;
            const inLibraryCount = artistAlbums.filter(a => a.matched_cluster_id !== null).length;

            return (
              <div
                key={artist.id}
                className="card-hover overflow-hidden group"
              >
                <Link to={`/artists/${artist.id}`}>
                  {/* Artist Image */}
                  <div className="aspect-square bg-dark-bg-subtle relative overflow-hidden">
                    {artist.image_url ? (
                      <img
                        src={artist.image_url}
                        alt={artist.name}
                        loading="lazy"
                        decoding="async"
                        className="w-full h-full object-cover group-hover:scale-105 transition-transform duration-300"
                        onError={(e) => {
                          (e.target as HTMLImageElement).style.display = 'none';
                        }}
                      />
                    ) : (
                      <div className="w-full h-full flex items-center justify-center">
                        <Music className="h-16 w-16 text-dark-text-tertiary" />
                      </div>
                    )}
                    {/* Hover overlay */}
                    <div className="absolute inset-0 bg-gradient-to-t from-dark-bg via-transparent to-transparent opacity-0 group-hover:opacity-100 transition-opacity duration-200" />
                  </div>
                </Link>

                {/* Artist Info */}
                <div className="p-4">
                  <div className="flex items-start justify-between gap-2 mb-3">
                    <Link to={`/artists/${artist.id}`} className="flex-1 min-w-0">
                      <h3 className="font-semibold text-dark-text truncate group-hover:text-dark-accent transition-colors" title={artist.name}>
                        {artist.name}
                      </h3>
                    </Link>
                    <button
                      onClick={() => handleUnfollowArtist(artist)}
                      className="p-1.5 text-dark-text-tertiary hover:text-dark-error hover:bg-dark-error-muted rounded-lg transition-all duration-200"
                      title="Unfollow"
                    >
                      <UserMinus className="h-4 w-4" />
                    </button>
                  </div>

                  {/* Stats */}
                  <div className="space-y-1.5 text-sm text-dark-text-secondary mb-3">
                    {wantedCount > 0 && (
                      <div className="flex items-center gap-2">
                        <Disc className="h-4 w-4 text-dark-accent" />
                        <span>{wantedCount} wanted</span>
                      </div>
                    )}
                    {inLibraryCount > 0 && (
                      <div className="flex items-center gap-2">
                        <Music className="h-4 w-4 text-dark-success" />
                        <span>{inLibraryCount} in library</span>
                      </div>
                    )}
                  </div>

                  {/* Footer */}
                  <div className="flex items-center justify-between pt-3 border-t border-dark-border">
                    <a
                      href={`https://musicbrainz.org/artist/${artist.mbid}`}
                      target="_blank"
                      rel="noopener noreferrer"
                      className="link text-xs flex items-center gap-1"
                    >
                      <ExternalLink className="h-3 w-3" />
                      <span>MusicBrainz</span>
                    </a>
                    <div className="text-xs text-dark-text-tertiary">
                      {formatDate(artist.created_at)}
                    </div>
                  </div>
                </div>

                {/* Wanted Albums Preview */}
                {artistAlbums.length > 0 && (
                  <div className="px-4 pb-4">
                    <div className="bg-dark-bg-subtle rounded-lg p-3 border border-dark-border">
                      <div className="text-xs font-medium text-dark-text-secondary mb-2">
                        Albums ({artistAlbums.length})
                      </div>
                      <div className="space-y-1">
                        {artistAlbums.slice(0, 3).map((album) => (
                          <div key={album.id} className="flex items-start justify-between gap-2 text-xs">
                            <span className="truncate text-dark-text flex-1" title={album.title}>
                              {album.title}
                            </span>
                            {album.matched_cluster_id && (
                              <span className="px-1.5 py-0.5 rounded-full text-[10px] font-medium whitespace-nowrap bg-dark-success-muted text-dark-success border border-dark-success/30">
                                in library
                              </span>
                            )}
                          </div>
                        ))}
                        {artistAlbums.length > 3 && (
                          <div className="text-xs text-dark-text-tertiary pt-1">
                            +{artistAlbums.length - 3} more
                          </div>
                        )}
                      </div>
                    </div>
                  </div>
                )}
              </div>
            );
          })}
        </div>
      )}

      {/* Pagination Controls */}
      {totalCount > ITEMS_PER_PAGE && (
        <div className="flex items-center justify-between border-t border-dark-border pt-4">
          <div className="text-sm text-dark-text-secondary">
            Showing {offset + 1}-{Math.min(offset + ITEMS_PER_PAGE, totalCount)} of {totalCount}
          </div>
          <div className="flex gap-2">
            <button
              onClick={() => setOffset((o) => Math.max(0, o - ITEMS_PER_PAGE))}
              disabled={offset === 0}
              className="p-2 rounded-lg border border-dark-border hover:bg-dark-bg-subtle transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
              title="Previous page"
            >
              <ChevronLeft size={16} />
            </button>
            <button
              onClick={() => setOffset((o) => Math.min(totalCount - ITEMS_PER_PAGE, o + ITEMS_PER_PAGE))}
              disabled={offset + ITEMS_PER_PAGE >= totalCount}
              className="p-2 rounded-lg border border-dark-border hover:bg-dark-bg-subtle transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
              title="Next page"
            >
              <ChevronRight size={16} />
            </button>
          </div>
        </div>
      )}
    </div>
  );
}
