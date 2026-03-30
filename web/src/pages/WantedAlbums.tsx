import { useEffect, useState, useMemo } from 'react';
import { api } from '../lib/api';
import { formatDate } from '../lib/formatters';
import type { CatalogAlbum, QualityProfile, CatalogArtist } from '../types/api';
import { Disc, ExternalLink, Filter, X } from 'lucide-react';
import toast from 'react-hot-toast';
import { handleApiError } from '../lib/errors';
import { useSSEEvent } from '../hooks/useSSEEvent';
import { TableRowSkeleton } from '../components/LoadingSkeleton';
import { LoadingState } from '../components/LoadingState';

export default function WantedAlbums() {
  const [allAlbums, setAllAlbums] = useState<CatalogAlbum[]>([]);
  const [loading, setLoading] = useState(true);
  const [searchQuery, setSearchQuery] = useState('');
  const [sortBy, setSortBy] = useState<'date' | 'title' | 'artist'>('date');
  const [qualityProfiles, setQualityProfiles] = useState<QualityProfile[]>([]);
  const [defaultProfile, setDefaultProfile] = useState<QualityProfile | null>(null);
  const [artists, setArtists] = useState<CatalogArtist[]>([]);

  // Filter wanted albums from the local list
  const albums = useMemo(() => {
    return allAlbums.filter(album => album.wanted);
  }, [allAlbums]);

  useEffect(() => {
    loadData();
  }, []);

  // SSE: new album added
  useSSEEvent<{ album_id: number; release_group_mbid: string; album_title: string; artist_mbid: string; artist_name: string; album_type: string | null; first_release_date: string | null; wanted: boolean }>('CatalogAlbumAdded', (data) => {
    setAllAlbums(prev => {
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
    setAllAlbums(prev => prev.map(a =>
      a.release_group_mbid === data.release_group_mbid
        ? { ...a, cover_url: data.cover_url, cover_thumbnail_url: data.thumbnail_url }
        : a
    ));
  });

  // SSE: album updated
  useSSEEvent<{ album_id: number; album_title: string; artist_name: string; album_type: string | null; first_release_date: string | null; quality_profile_id: number | null }>('CatalogAlbumUpdated', (data) => {
    setAllAlbums(prev => prev.map(a =>
      a.id === data.album_id
        ? { ...a, title: data.album_title, artist_name: data.artist_name, type: data.album_type, first_release_date: data.first_release_date, quality_profile_id: data.quality_profile_id, wanted: data.quality_profile_id != null }
        : a
    ));
  });

  const loadData = async () => {
    try {
      setLoading(true);
      const [profiles, defaultProf, artistsResponse, albumsData] = await Promise.all([
        api.getQualityProfiles(),
        api.getDefaultQualityProfile(),
        api.getCatalogArtists(0, 1000),
        api.getCatalogAlbums(),
      ]);
      setQualityProfiles(profiles);
      setDefaultProfile(defaultProf);
      setArtists(artistsResponse.artists);
      setAllAlbums(albumsData);
    } catch (error) {
      handleApiError(error, 'Failed to load data');
    } finally {
      setLoading(false);
    }
  };

  const handleUnwantAlbum = async (album: CatalogAlbum) => {
    if (!album.id) return;

    try {
      // Set quality_profile_id = null to unwant (remove from monitoring)
      await api.updateCatalogAlbum(album.id, null);

      // Update local state
      setAllAlbums(prev => prev.map(a =>
        a.id === album.id ? { ...a, wanted: false } : a
      ));

      toast.success(`Removed ${album.title} from wanted list`);
    } catch (error) {
      handleApiError(error, 'Failed to remove album from wanted list');
    }
  };

  const handleAlbumQualityProfileChange = async (album: CatalogAlbum, profileId: string) => {
    if (!album.id) return;

    try {
      const newProfileId = profileId === '' ? null : parseInt(profileId, 10);
      const updatedAlbum = await api.updateCatalogAlbum(album.id, newProfileId);

      // Update local state
      setAllAlbums(prev => prev.map(a =>
        a.id === album.id ? { ...a, quality_profile_id: updatedAlbum.quality_profile_id } : a
      ));

      toast.success(newProfileId === null ? 'Using artist/default quality profile' : 'Album quality profile updated');
    } catch (error) {
      handleApiError(error, 'Failed to update quality profile');
    }
  };

  const getArtistForAlbum = (album: CatalogAlbum): CatalogArtist | undefined => {
    return artists.find(a => a.mbid === album.artist_mbid);
  };

  // Filter and sort albums
  const filteredAlbums = albums
    .filter(album => {
      const matchesSearch =
        album.title.toLowerCase().includes(searchQuery.toLowerCase()) ||
        album.artist_name.toLowerCase().includes(searchQuery.toLowerCase());
      return matchesSearch;
    })
    .sort((a, b) => {
      switch (sortBy) {
        case 'title':
          return a.title.localeCompare(b.title);
        case 'artist':
          return a.artist_name.localeCompare(b.artist_name);
        case 'date':
        default:
          // Sort by first_release_date, with nulls last
          if (!a.first_release_date && !b.first_release_date) return 0;
          if (!a.first_release_date) return 1;
          if (!b.first_release_date) return -1;
          return b.first_release_date.localeCompare(a.first_release_date);
      }
    });

  return (
    <div className="space-y-6 animate-fade-in">
      {/* Header */}
      <div>
        <h1 className="text-3xl font-bold text-dark-text">Wanted Albums</h1>
        <p className="text-dark-text-secondary mt-2">
          {filteredAlbums.length} of {albums.length} album{albums.length !== 1 ? 's' : ''}
        </p>
      </div>

      {/* Filters */}
      <div className="card p-4 space-y-4">
        <div className="flex items-center gap-2 text-sm font-medium text-dark-text">
          <Filter className="h-4 w-4" />
          <span>Filters</span>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          {/* Search */}
          <input
            type="text"
            placeholder="Search albums or artists..."
            value={searchQuery}
            onChange={(e) => setSearchQuery(e.target.value)}
            className="input"
          />

          {/* Sort */}
          <select
            value={sortBy}
            onChange={(e) => setSortBy(e.target.value as 'date' | 'title' | 'artist')}
            className="input"
          >
            <option value="date">Sort by Release Date</option>
            <option value="title">Sort by Title</option>
            <option value="artist">Sort by Artist</option>
          </select>
        </div>
      </div>

      {/* Albums List */}
      <LoadingState
        loading={loading}
        empty={filteredAlbums.length === 0}
        skeleton={
          <div className="card overflow-hidden">
            <div className="overflow-x-auto">
              <table className="min-w-full divide-y divide-dark-border">
                <thead className="bg-dark-bg-subtle">
                  <tr>
                    <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">Album</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">Artist</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">Release Date</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">Quality</th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">Actions</th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-dark-border">
                  {[...Array(10)].map((_, i) => (
                    <TableRowSkeleton key={i} columns={5} />
                  ))}
                </tbody>
              </table>
            </div>
          </div>
        }
        emptyState={
          <div className="card p-12 text-center">
            <Disc className="mx-auto h-12 w-12 text-dark-text-tertiary" />
            <h3 className="mt-4 text-sm font-medium text-dark-text">No albums found</h3>
            <p className="mt-2 text-sm text-dark-text-secondary">
              {searchQuery
                ? 'Try adjusting your search'
                : 'Add albums to your wanted list from the universal search'}
            </p>
          </div>
        }
      >
        <div className="card overflow-hidden">
          <div className="overflow-x-auto">
            <table className="min-w-full divide-y divide-dark-border">
              <thead className="bg-dark-bg-subtle">
                <tr>
                  <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">
                    Album
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">
                    Artist
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">
                    Release Date
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">
                    Quality
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-dark-text-tertiary uppercase tracking-wider">
                    Actions
                  </th>
                </tr>
              </thead>
              <tbody className="divide-y divide-dark-border">
                {filteredAlbums.map((album) => (
                  <tr key={album.id} className="hover:bg-dark-bg-hover transition-colors duration-150">
                    <td className="px-6 py-4 whitespace-nowrap">
                      <div className="flex items-center">
                        <Disc className="h-5 w-5 text-dark-accent mr-3 flex-shrink-0" />
                        <div className="text-sm font-medium text-dark-text">
                          {album.title}
                        </div>
                      </div>
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap">
                      <div className="text-sm text-dark-text-secondary">{album.artist_name}</div>
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap">
                      <div className="text-sm text-dark-text-secondary">
                        {formatDate(album.first_release_date)}
                      </div>
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap">
                      {qualityProfiles.length > 0 && (
                        <select
                          value={album.quality_profile_id?.toString() || ''}
                          onChange={(e) => handleAlbumQualityProfileChange(album, e.target.value)}
                          className="input text-xs py-1.5 w-40"
                          title="Quality profile for this album"
                          onClick={(e) => e.stopPropagation()}
                        >
                          <option value="">
                            Default ({(() => {
                              const artist = getArtistForAlbum(album);
                              if (artist?.quality_profile_id) {
                                return qualityProfiles.find(p => p.id === artist.quality_profile_id)?.name;
                              }
                              return defaultProfile?.name || 'None';
                            })()})
                          </option>
                          {qualityProfiles.map((profile) => (
                            <option key={profile.id} value={profile.id.toString()}>
                              {profile.name}
                            </option>
                          ))}
                        </select>
                      )}
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap text-sm">
                      <div className="flex items-center gap-2">
                        <button
                          onClick={() => handleUnwantAlbum(album)}
                          className="px-3 py-1.5 bg-dark-bg-hover hover:bg-dark-error-muted text-dark-text-secondary hover:text-dark-error rounded transition-colors flex items-center gap-1.5"
                          title="Remove from wanted list"
                        >
                          <X className="h-4 w-4" />
                          <span>Unwant</span>
                        </button>
                        <a
                          href={`https://musicbrainz.org/release-group/${album.release_group_mbid}`}
                          target="_blank"
                          rel="noopener noreferrer"
                          className="link inline-flex items-center gap-1"
                        >
                          <ExternalLink className="h-3 w-3" />
                          <span>MusicBrainz</span>
                        </a>
                      </div>
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      </LoadingState>
    </div>
  );
}
