import { useState, useEffect, useCallback, useRef } from 'react';
import { api } from '../lib/api';
import { CatalogArtist, CatalogAlbum, QualityProfile } from '../types/api';
import { toast } from 'react-hot-toast';
import { Check, ChevronDown } from 'lucide-react';

export default function UniversalSearch() {
  const [query, setQuery] = useState('');
  const [artists, setArtists] = useState<CatalogArtist[]>([]);
  const [albums, setAlbums] = useState<CatalogAlbum[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [isOpen, setIsOpen] = useState(false);
  const [qualityProfiles, setQualityProfiles] = useState<QualityProfile[]>([]);
  const [defaultProfileId, setDefaultProfileId] = useState<number | null>(null);
  const [profileMenuKey, setProfileMenuKey] = useState<string | null>(null);
  const dropdownRef = useRef<HTMLDivElement>(null);
  const inputRef = useRef<HTMLInputElement>(null);

  // Load quality profiles and default
  useEffect(() => {
    const loadProfiles = async () => {
      try {
        const [profiles, defaultProfile] = await Promise.all([
          api.getQualityProfiles(),
          api.getDefaultQualityProfile(),
        ]);
        setQualityProfiles(profiles);
        if (defaultProfile) {
          setDefaultProfileId(defaultProfile.id);
        }
      } catch (error) {
        console.error('Failed to load quality profiles:', error);
      }
    };
    loadProfiles();
  }, []);

  // Close dropdown when clicking outside
  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (dropdownRef.current && !dropdownRef.current.contains(event.target as Node)) {
        setIsOpen(false);
        setProfileMenuKey(null);
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

  const handleFollowArtist = useCallback(async (artist: CatalogArtist, profileId?: number | null) => {
    try {
      const newFollowedState = !artist.followed;
      const profileToSet = newFollowedState ? (profileId ?? defaultProfileId) : undefined;

      if (artist.id) {
        await api.updateCatalogArtist(artist.id, newFollowedState, profileToSet);
      } else {
        await api.createCatalogArtist({
          mbid: artist.mbid,
          name: artist.name,
          type: artist.type || undefined,
          image_url: artist.image_url || undefined,
          followed: newFollowedState,
          quality_profile_id: profileToSet,
        });
      }

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
  }, [defaultProfileId]);

  const handleWantAlbum = useCallback(async (album: CatalogAlbum, profileId?: number | null) => {
    try {
      const newWantedState = !album.wanted;
      const profileToUse = newWantedState ? (album.quality_profile_id || (profileId ?? defaultProfileId)) : null;

      if (newWantedState && !profileToUse) {
        toast.error('No quality profile available. Please set a default quality profile first.');
        return;
      }

      if (album.id) {
        await api.updateCatalogAlbum(album.id, profileToUse);
        setAlbums((prev) =>
          prev.map((a) =>
            a.release_group_mbid === album.release_group_mbid
              ? { ...a, wanted: newWantedState, quality_profile_id: profileToUse, id: album.id || a.id }
              : a
          )
        );
      } else {
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
  }, [defaultProfileId]);

  const defaultProfileName = qualityProfiles.find(p => p.id === defaultProfileId)?.name;
  const hasMultipleProfiles = qualityProfiles.length > 1;

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
                      {artist.followed ? (
                        <button
                          onClick={() => handleFollowArtist(artist)}
                          className="w-full sm:w-auto sm:ml-3 flex-shrink-0 rounded-lg px-3 py-1.5 text-xs font-medium transition-all duration-200 bg-dark-bg-subtle text-dark-text-secondary hover:bg-dark-bg-hover"
                        >
                          Unfollow
                        </button>
                      ) : (
                        <div className="relative w-full sm:w-auto sm:ml-3 flex-shrink-0">
                          <div className="flex rounded-lg overflow-hidden">
                            <button
                              onClick={() => handleFollowArtist(artist)}
                              className="flex-1 sm:flex-initial px-3 py-1.5 text-xs font-medium btn-primary rounded-none rounded-l-lg"
                              title={defaultProfileName ? `Follow with ${defaultProfileName}` : 'Follow'}
                            >
                              Follow
                            </button>
                            {hasMultipleProfiles && (
                              <button
                                onClick={(e) => {
                                  e.stopPropagation();
                                  setProfileMenuKey(profileMenuKey === `artist-${artist.mbid}` ? null : `artist-${artist.mbid}`);
                                }}
                                className="px-1.5 py-1.5 text-xs font-medium btn-primary rounded-none rounded-r-lg border-l border-white/20"
                              >
                                <ChevronDown className="h-3 w-3" />
                              </button>
                            )}
                          </div>
                          {profileMenuKey === `artist-${artist.mbid}` && (
                            <div className="absolute right-0 top-full mt-1 z-[60] min-w-[160px] rounded-lg border border-dark-border bg-dark-bg-elevated shadow-xl py-1">
                              {qualityProfiles.map((profile) => (
                                <button
                                  key={profile.id}
                                  onClick={() => {
                                    setProfileMenuKey(null);
                                    handleFollowArtist(artist, profile.id);
                                  }}
                                  className="w-full text-left px-3 py-1.5 text-xs text-dark-text hover:bg-dark-bg-hover transition-colors flex items-center justify-between gap-2"
                                >
                                  <span>{profile.name}</span>
                                  {profile.id === defaultProfileId && (
                                    <span className="text-dark-text-tertiary">(default)</span>
                                  )}
                                </button>
                              ))}
                            </div>
                          )}
                        </div>
                      )}
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
                      {album.wanted ? (
                        <button
                          disabled
                          className="w-full sm:w-auto sm:ml-3 flex-shrink-0 rounded-lg px-3 py-1.5 text-xs font-medium bg-dark-bg-subtle text-dark-text-tertiary cursor-default flex items-center justify-center gap-1.5"
                        >
                          <Check className="h-3.5 w-3.5" />
                          Wanted
                        </button>
                      ) : (
                        <div className="relative w-full sm:w-auto sm:ml-3 flex-shrink-0">
                          <div className="flex rounded-lg overflow-hidden">
                            <button
                              onClick={() => handleWantAlbum(album)}
                              className="flex-1 sm:flex-initial px-3 py-1.5 text-xs font-medium rounded-none rounded-l-lg bg-dark-success text-dark-bg hover:bg-dark-success/90 transition-all duration-200"
                              title={defaultProfileName ? `Add with ${defaultProfileName}` : 'Add to Wanted'}
                            >
                              Add to Wanted
                            </button>
                            {hasMultipleProfiles && (
                              <button
                                onClick={(e) => {
                                  e.stopPropagation();
                                  setProfileMenuKey(profileMenuKey === `album-${album.release_group_mbid}` ? null : `album-${album.release_group_mbid}`);
                                }}
                                className="px-1.5 py-1.5 text-xs font-medium rounded-none rounded-r-lg bg-dark-success text-dark-bg hover:bg-dark-success/90 transition-all duration-200 border-l border-white/20"
                              >
                                <ChevronDown className="h-3 w-3" />
                              </button>
                            )}
                          </div>
                          {profileMenuKey === `album-${album.release_group_mbid}` && (
                            <div className="absolute right-0 top-full mt-1 z-[60] min-w-[160px] rounded-lg border border-dark-border bg-dark-bg-elevated shadow-xl py-1">
                              {qualityProfiles.map((profile) => (
                                <button
                                  key={profile.id}
                                  onClick={() => {
                                    setProfileMenuKey(null);
                                    handleWantAlbum(album, profile.id);
                                  }}
                                  className="w-full text-left px-3 py-1.5 text-xs text-dark-text hover:bg-dark-bg-hover transition-colors flex items-center justify-between gap-2"
                                >
                                  <span>{profile.name}</span>
                                  {profile.id === defaultProfileId && (
                                    <span className="text-dark-text-tertiary">(default)</span>
                                  )}
                                </button>
                              ))}
                            </div>
                          )}
                        </div>
                      )}
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
