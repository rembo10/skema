import { useState, useEffect, useCallback, useRef } from 'react';
import { api } from '../lib/api';
import { CatalogArtist, CatalogAlbum, QualityProfile } from '../types/api';
import { toast } from 'react-hot-toast';
import { X } from 'lucide-react';

export default function UniversalSearch() {
  const [query, setQuery] = useState('');
  const [artists, setArtists] = useState<CatalogArtist[]>([]);
  const [albums, setAlbums] = useState<CatalogAlbum[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [isOpen, setIsOpen] = useState(false);
  const [defaultProfile, setDefaultProfile] = useState<QualityProfile | null>(null);
  const dropdownRef = useRef<HTMLDivElement>(null);
  const inputRef = useRef<HTMLInputElement>(null);

  // Load default quality profile
  useEffect(() => {
    const loadDefaultProfile = async () => {
      try {
        const profile = await api.getDefaultQualityProfile();
        setDefaultProfile(profile);
      } catch (error) {
        console.error('Failed to load default quality profile:', error);
      }
    };
    loadDefaultProfile();
  }, []);

  // Close dropdown when clicking outside
  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (dropdownRef.current && !dropdownRef.current.contains(event.target as Node)) {
        setIsOpen(false);
      }
    };

    document.addEventListener('mousedown', handleClickOutside);
    return () => document.removeEventListener('mousedown', handleClickOutside);
  }, []);

  // Debounced search function
  useEffect(() => {
    if (!query.trim()) {
      setArtists([]);
      setAlbums([]);
      setIsOpen(false);
      return;
    }

    setIsOpen(true);

    const timeoutId = setTimeout(async () => {
      setLoading(true);
      setError(null);
      try {
        const response = await api.catalogQuery({ query, limit: 5 });
        setArtists(response.artists);
        setAlbums(response.albums);
      } catch (err) {
        const message = err instanceof Error ? err.message : 'Search failed';
        setError(message);
        toast.error(message);
      } finally {
        setLoading(false);
      }
    }, 300); // 300ms debounce

    return () => clearTimeout(timeoutId);
  }, [query]);

  const handleFollowArtist = useCallback(async (artist: CatalogArtist) => {
    try {
      const newFollowedState = !artist.followed;

      if (artist.id) {
        // Update existing artist
        await api.updateCatalogArtist(artist.id, newFollowedState);
      } else {
        // Create new artist in catalog
        await api.createCatalogArtist({
          mbid: artist.mbid,
          name: artist.name,
          type: artist.type || undefined,
          image_url: artist.image_url || undefined,
          followed: newFollowedState,
        });
      }

      // Update local state
      setArtists((prev) =>
        prev.map((a) =>
          a.mbid === artist.mbid ? { ...a, followed: newFollowedState, id: artist.id || a.id } : a
        )
      );

      toast.success(newFollowedState ? `Following ${artist.name}` : `Unfollowed ${artist.name}`);
    } catch (err) {
      const message = err instanceof Error ? err.message : 'Failed to update artist';
      toast.error(message);
    }
  }, []);

  const handleWantAlbum = useCallback(async (album: CatalogAlbum) => {
    try {
      const newWantedState = !album.wanted;

      if (album.id) {
        // Update existing album
        // If wanting: use album's profile or default profile
        // If unwanting: set to null
        let profileToUse: number | null = null;
        if (newWantedState) {
          profileToUse = album.quality_profile_id || defaultProfile?.id || null;
          if (!profileToUse) {
            toast.error('No quality profile available. Please set a default quality profile first.');
            return;
          }
        }
        await api.updateCatalogAlbum(album.id, profileToUse);

        // Update local state
        setAlbums((prev) =>
          prev.map((a) =>
            a.release_group_mbid === album.release_group_mbid
              ? { ...a, wanted: newWantedState, quality_profile_id: profileToUse, id: album.id || a.id }
              : a
          )
        );
      } else {
        // Create new album in catalog
        const profileToUse = album.quality_profile_id || defaultProfile?.id || null;
        if (newWantedState && !profileToUse) {
          toast.error('No quality profile available. Please set a default quality profile first.');
          return;
        }

        await api.createCatalogAlbum({
          release_group_mbid: album.release_group_mbid,
          title: album.title,
          artist_mbid: album.artist_mbid,
          artist_name: album.artist_name,
          type: album.type || undefined,
          first_release_date: album.first_release_date || undefined,
          wanted: newWantedState,
          quality_profile_id: newWantedState ? profileToUse : undefined,
        });

        // Update local state
        setAlbums((prev) =>
          prev.map((a) =>
            a.release_group_mbid === album.release_group_mbid
              ? { ...a, wanted: newWantedState, id: album.id || a.id }
              : a
          )
        );
      }

      toast.success(newWantedState ? `Added ${album.title} to wanted` : `Removed ${album.title} from wanted`);
    } catch (err) {
      const message = err instanceof Error ? err.message : 'Failed to update album';
      toast.error(message);
    }
  }, [defaultProfile]);

  return (
    <div ref={dropdownRef} className="relative w-full max-w-2xl">
      {/* Search Input */}
      <div className="relative">
        <div className="pointer-events-none absolute inset-y-0 left-0 flex items-center pl-3">
          <svg
            className="h-4 w-4 text-dark-text-tertiary"
            fill="none"
            stroke="currentColor"
            viewBox="0 0 24 24"
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth={2}
              d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"
            />
          </svg>
        </div>
        <input
          ref={inputRef}
          type="text"
          value={query}
          onChange={(e) => setQuery(e.target.value)}
          onFocus={() => query && setIsOpen(true)}
          placeholder="Search artists and albums..."
          className="input w-full pl-10"
        />
      </div>

      {/* Dropdown Results */}
      {isOpen && (
        <div className="absolute left-0 right-0 z-50 mt-2 card animate-fade-in shadow-2xl">
          <div className="max-h-[70vh] sm:max-h-[500px] overflow-y-auto">
            {loading && (
              <div className="flex items-center justify-center py-8 text-dark-text-secondary">
                <div className="h-5 w-5 animate-spin rounded-full border-2 border-dark-border border-t-dark-accent" />
                <span className="ml-2 text-sm">Searching...</span>
              </div>
            )}

            {error && (
              <div className="m-4 rounded bg-dark-error-muted border border-dark-error/30 p-3 text-sm text-dark-error">
                {error}
              </div>
            )}

            {!loading && !error && query && artists.length === 0 && albums.length === 0 && (
              <div className="py-8 text-center text-sm text-dark-text-secondary">
                No results found for "{query}"
              </div>
            )}

            {/* Artists */}
            {artists.length > 0 && (
              <div className="border-b border-dark-border p-3 sm:p-4">
                <h3 className="mb-3 text-xs font-semibold uppercase text-dark-text-tertiary tracking-wider">
                  Artists
                </h3>
                <div className="space-y-2">
                  {artists.map((artist) => (
                    <div
                      key={artist.mbid}
                      className="flex items-start sm:items-center gap-2 sm:gap-0 flex-col sm:flex-row sm:justify-between rounded-lg p-2 sm:p-3 hover:bg-dark-bg-hover transition-all duration-200"
                    >
                      <div className="flex-1 min-w-0 w-full sm:w-auto">
                        <div className="font-medium text-dark-text truncate text-sm sm:text-base">{artist.name}</div>
                        <div className="text-xs text-dark-text-secondary">
                          {artist.type && <span className="capitalize">{artist.type}</span>}
                          {artist.score && (
                            <span className="ml-2">
                              • Score: {artist.score}/100
                            </span>
                          )}
                        </div>
                      </div>
                      <button
                        onClick={() => handleFollowArtist(artist)}
                        className={`w-full sm:w-auto sm:ml-3 flex-shrink-0 rounded-lg px-3 py-1.5 text-xs font-medium transition-all duration-200 ${
                          artist.followed
                            ? 'bg-dark-bg-subtle text-dark-text-secondary hover:bg-dark-bg-hover'
                            : 'btn-primary'
                        }`}
                      >
                        {artist.followed ? 'Unfollow' : 'Follow'}
                      </button>
                    </div>
                  ))}
                </div>
              </div>
            )}

            {/* Albums */}
            {albums.length > 0 && (
              <div className="p-3 sm:p-4">
                <h3 className="mb-3 text-xs font-semibold uppercase text-dark-text-tertiary tracking-wider">
                  Albums
                </h3>
                <div className="space-y-2">
                  {albums.map((album) => (
                    <div
                      key={album.release_group_mbid}
                      className="flex items-start sm:items-center gap-2 sm:gap-0 flex-col sm:flex-row sm:justify-between rounded-lg p-2 sm:p-3 hover:bg-dark-bg-hover transition-all duration-200"
                    >
                      <div className="flex-1 min-w-0 w-full sm:w-auto">
                        <div className="font-medium text-dark-text truncate text-sm sm:text-base">{album.title}</div>
                        <div className="text-xs text-dark-text-secondary">
                          {album.artist_name}
                          {album.type && (
                            <span className="ml-2 capitalize">• {album.type}</span>
                          )}
                          {album.first_release_date && (
                            <span className="ml-2">
                              • {album.first_release_date.slice(0, 4)}
                            </span>
                          )}
                          {album.matched_cluster_id && (
                            <span className="ml-2 text-dark-success">
                              • In Library
                            </span>
                          )}
                        </div>
                      </div>
                      <button
                        onClick={() => handleWantAlbum(album)}
                        className={`w-full sm:w-auto sm:ml-3 flex-shrink-0 rounded-lg px-3 py-1.5 text-xs font-medium transition-all duration-200 flex items-center justify-center gap-1.5 ${
                          album.wanted
                            ? 'bg-dark-bg-subtle text-dark-text-secondary hover:bg-dark-bg-hover'
                            : 'bg-dark-success text-dark-bg hover:bg-dark-success/90'
                        }`}
                      >
                        {album.wanted && <X className="h-3.5 w-3.5" />}
                        {album.wanted ? 'Remove' : 'Add to Wanted'}
                      </button>
                    </div>
                  ))}
                </div>
              </div>
            )}
          </div>
        </div>
      )}
    </div>
  );
}
