import { useEffect, useState } from 'react';
import { Link } from 'react-router-dom';
import { api } from '../lib/api';
import { useAppStore } from '../store';
import type { CatalogArtist } from '../types/api';
import { Music, ExternalLink, Disc, UserMinus, RefreshCw, LayoutGrid, List, ListPlus } from 'lucide-react';
import toast from 'react-hot-toast';

export default function FollowedArtists() {
  // Use store as single source of truth for both artists and albums
  const artists = useAppStore((state) => state.followedArtists);
  const setFollowedArtists = useAppStore((state) => state.setFollowedArtists);
  const removeFollowedArtist = useAppStore((state) => state.removeFollowedArtist);
  const catalogAlbums = useAppStore((state) => state.catalogAlbums);
  const setCatalogAlbums = useAppStore((state) => state.setCatalogAlbums);

  const [loading, setLoading] = useState(true);
  const [searchQuery, setSearchQuery] = useState('');
  const [refreshing, setRefreshing] = useState(false);
  const [viewMode, setViewMode] = useState<'grid' | 'list'>('grid');
  const [wantingAllForArtist, setWantingAllForArtist] = useState<number | null>(null);

  useEffect(() => {
    loadData();
  }, []);

  const loadData = async () => {
    try {
      setLoading(true);
      const [artistsData, albumsData] = await Promise.all([
        api.getCatalogArtists(true), // Get only followed artists
        api.getCatalogAlbums(),      // Get all albums (both wanted and in library)
      ]);
      setFollowedArtists(artistsData);
      setCatalogAlbums(albumsData);
    } catch (error) {
      toast.error('Failed to load followed artists');
      console.error('Error loading artists:', error);
    } finally {
      setLoading(false);
    }
  };

  const getAlbumsForArtist = (artistMBID: string) => {
    return catalogAlbums.filter(album => album.artist_mbid === artistMBID);
  };

  const handleUnfollowArtist = async (artist: CatalogArtist) => {
    if (!artist.id) return;

    try {
      await api.updateCatalogArtist(artist.id, false);
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

  const handleWantAllAlbums = async (artist: CatalogArtist) => {
    if (!artist.id) return;

    try {
      setWantingAllForArtist(artist.id);
      const result = await api.wantAllAlbums(artist.id);
      toast.success(result.message);
      await loadData();
    } catch (error) {
      toast.error('Failed to want all albums');
      console.error('Error wanting all albums:', error);
    } finally {
      setWantingAllForArtist(null);
    }
  };

  const filteredArtists = artists.filter(artist =>
    artist.name.toLowerCase().includes(searchQuery.toLowerCase())
  );

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
            {artists.length} followed artist{artists.length !== 1 ? 's' : ''} • {catalogAlbums.filter(a => a.wanted).length} wanted album{catalogAlbums.filter(a => a.wanted).length !== 1 ? 's' : ''}
          </p>
        </div>
        {artists.length > 0 && (
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

      {/* Search and View Toggle */}
      <div className="flex items-center gap-4">
        <div className="flex-1 max-w-md">
          <input
            type="text"
            placeholder="Search artists..."
            value={searchQuery}
            onChange={(e) => setSearchQuery(e.target.value)}
            className="input w-full"
          />
        </div>
        <div className="flex items-center gap-1 bg-dark-bg-elevated rounded-lg p-1 border border-dark-border">
          <button
            onClick={() => setViewMode('grid')}
            className={`p-2 rounded-md transition-all ${
              viewMode === 'grid'
                ? 'bg-dark-accent text-dark-bg'
                : 'text-dark-text-secondary hover:text-dark-text hover:bg-dark-bg-hover'
            }`}
            title="Grid view"
          >
            <LayoutGrid className="h-4 w-4" />
          </button>
          <button
            onClick={() => setViewMode('list')}
            className={`p-2 rounded-md transition-all ${
              viewMode === 'list'
                ? 'bg-dark-accent text-dark-bg'
                : 'text-dark-text-secondary hover:text-dark-text hover:bg-dark-bg-hover'
            }`}
            title="List view"
          >
            <List className="h-4 w-4" />
          </button>
        </div>
      </div>

      {/* Artists Display */}
      {filteredArtists.length === 0 ? (
        <div className="card p-12 text-center">
          <Music className="mx-auto h-12 w-12 text-dark-text-tertiary" />
          <h3 className="mt-4 text-sm font-medium text-dark-text">No followed artists</h3>
          <p className="mt-2 text-sm text-dark-text-secondary">
            {searchQuery ? 'Try a different search term' : 'Use the universal search to find and follow artists'}
          </p>
        </div>
      ) : viewMode === 'list' ? (
        /* List View */
        <div className="card overflow-hidden">
          <table className="w-full">
            <thead className="bg-dark-bg-subtle border-b border-dark-border">
              <tr>
                <th className="text-left px-4 py-3 text-xs font-medium text-dark-text-secondary uppercase tracking-wider">Artist</th>
                <th className="text-left px-4 py-3 text-xs font-medium text-dark-text-secondary uppercase tracking-wider">Type</th>
                <th className="text-center px-4 py-3 text-xs font-medium text-dark-text-secondary uppercase tracking-wider">Albums</th>
                <th className="text-center px-4 py-3 text-xs font-medium text-dark-text-secondary uppercase tracking-wider">Wanted</th>
                <th className="text-center px-4 py-3 text-xs font-medium text-dark-text-secondary uppercase tracking-wider">In Library</th>
                <th className="text-left px-4 py-3 text-xs font-medium text-dark-text-secondary uppercase tracking-wider">Added</th>
                <th className="text-right px-4 py-3 text-xs font-medium text-dark-text-secondary uppercase tracking-wider">Actions</th>
              </tr>
            </thead>
            <tbody className="divide-y divide-dark-border">
              {filteredArtists.map((artist) => {
                const artistAlbums = getAlbumsForArtist(artist.mbid);
                const wantedCount = artistAlbums.filter(a => a.wanted).length;
                const inLibraryCount = artistAlbums.filter(a => a.matched_cluster_id !== null).length;

                return (
                  <tr key={artist.id} className="hover:bg-dark-bg-hover transition-colors">
                    <td className="px-4 py-3">
                      <Link to={`/artists/${artist.id}`} className="flex items-center gap-3 group">
                        <div className="w-10 h-10 rounded-lg bg-dark-bg-subtle overflow-hidden flex-shrink-0">
                          {artist.image_url ? (
                            <img
                              src={artist.thumbnail_url || artist.image_url}
                              alt={artist.name}
                              className="w-full h-full object-cover"
                            />
                          ) : (
                            <div className="w-full h-full flex items-center justify-center">
                              <Music className="h-5 w-5 text-dark-text-tertiary" />
                            </div>
                          )}
                        </div>
                        <span className="font-medium text-dark-text group-hover:text-dark-accent transition-colors truncate">
                          {artist.name}
                        </span>
                      </Link>
                    </td>
                    <td className="px-4 py-3 text-sm text-dark-text-secondary">
                      {artist.type || '—'}
                    </td>
                    <td className="px-4 py-3 text-center text-sm text-dark-text-secondary">
                      {artistAlbums.length}
                    </td>
                    <td className="px-4 py-3 text-center">
                      {wantedCount > 0 ? (
                        <span className="inline-flex items-center gap-1 px-2 py-0.5 rounded-full text-xs font-medium bg-dark-accent-muted text-dark-accent border border-dark-accent/30">
                          {wantedCount}
                        </span>
                      ) : (
                        <span className="text-dark-text-tertiary">—</span>
                      )}
                    </td>
                    <td className="px-4 py-3 text-center">
                      {inLibraryCount > 0 ? (
                        <span className="inline-flex items-center gap-1 px-2 py-0.5 rounded-full text-xs font-medium bg-dark-success-muted text-dark-success border border-dark-success/30">
                          {inLibraryCount}
                        </span>
                      ) : (
                        <span className="text-dark-text-tertiary">—</span>
                      )}
                    </td>
                    <td className="px-4 py-3 text-sm text-dark-text-secondary">
                      {formatDate(artist.created_at)}
                    </td>
                    <td className="px-4 py-3 text-right">
                      <div className="flex items-center justify-end gap-2">
                        <button
                          onClick={() => handleWantAllAlbums(artist)}
                          disabled={wantingAllForArtist === artist.id}
                          className="p-1.5 text-dark-text-tertiary hover:text-dark-success hover:bg-dark-success-muted rounded-lg transition-all disabled:opacity-50"
                          title="Want all albums"
                        >
                          <ListPlus className={`h-4 w-4 ${wantingAllForArtist === artist.id ? 'animate-pulse' : ''}`} />
                        </button>
                        <a
                          href={`https://musicbrainz.org/artist/${artist.mbid}`}
                          target="_blank"
                          rel="noopener noreferrer"
                          className="p-1.5 text-dark-text-tertiary hover:text-dark-accent hover:bg-dark-bg-hover rounded-lg transition-all"
                          title="View on MusicBrainz"
                        >
                          <ExternalLink className="h-4 w-4" />
                        </a>
                        <button
                          onClick={() => handleUnfollowArtist(artist)}
                          className="p-1.5 text-dark-text-tertiary hover:text-dark-error hover:bg-dark-error-muted rounded-lg transition-all"
                          title="Unfollow"
                        >
                          <UserMinus className="h-4 w-4" />
                        </button>
                      </div>
                    </td>
                  </tr>
                );
              })}
            </tbody>
          </table>
        </div>
      ) : (
        /* Grid View */
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-4">
          {filteredArtists.map((artist) => {
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
    </div>
  );
}
