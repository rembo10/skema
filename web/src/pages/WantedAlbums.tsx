import { useEffect, useState, useMemo } from 'react';
import { api } from '../lib/api';
import type { CatalogAlbum, QualityProfile, CatalogArtist } from '../types/api';
import { Disc, ExternalLink, Filter, X, Award } from 'lucide-react';
import toast from 'react-hot-toast';
import { useAppStore } from '../store';
import { TableRowSkeleton } from '../components/LoadingSkeleton';
import { LoadingState } from '../components/LoadingState';

export default function WantedAlbums() {
  const catalogAlbums = useAppStore((state) => state.catalogAlbums);
  const setCatalogAlbums = useAppStore((state) => state.setCatalogAlbums);
  const updateCatalogAlbum = useAppStore((state) => state.updateCatalogAlbum);
  const [loading, setLoading] = useState(true);
  const [searchQuery, setSearchQuery] = useState('');
  const [sortBy, setSortBy] = useState<'date' | 'title' | 'artist'>('date');
  const [qualityProfiles, setQualityProfiles] = useState<QualityProfile[]>([]);
  const [defaultProfile, setDefaultProfile] = useState<QualityProfile | null>(null);
  const [artists, setArtists] = useState<CatalogArtist[]>([]);

  // Filter wanted albums from the catalog
  const albums = useMemo(() => {
    return catalogAlbums.filter(album => album.wanted);
  }, [catalogAlbums]);

  useEffect(() => {
    loadData();
  }, []);

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
      setCatalogAlbums(albumsData);
    } catch (error) {
      toast.error('Failed to load data');
      console.error('Error loading data:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleUnwantAlbum = async (album: CatalogAlbum) => {
    if (!album.id) return;

    try {
      // Set quality_profile_id = null to unwant (remove from monitoring)
      await api.updateCatalogAlbum(album.id, null);

      // Update global store
      updateCatalogAlbum(album.id, { wanted: false });

      toast.success(`Removed ${album.title} from wanted list`);
    } catch (error) {
      toast.error('Failed to remove album from wanted list');
      console.error('Error unwanting album:', error);
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

  const getArtistForAlbum = (album: CatalogAlbum): CatalogArtist | undefined => {
    return artists.find(a => a.mbid === album.artist_mbid);
  };

  const formatDate = (dateString: string | null) => {
    if (!dateString) return 'Unknown';
    try {
      // Handle YYYY-MM-DD format
      if (dateString.includes('-')) {
        return new Date(dateString).toLocaleDateString('en-US', {
          year: 'numeric',
          month: 'short',
          day: 'numeric',
        });
      }
      return dateString;
    } catch {
      return dateString;
    }
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
            onChange={(e) => setSortBy(e.target.value as any)}
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
