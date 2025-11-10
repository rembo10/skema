import { useState } from 'react';
import { Plus, X } from 'lucide-react';

interface ReviewScores {
  metacritic?: { min_score?: number; max_score?: number };
  pitchfork?: { min_score?: number; max_score?: number };
  operator?: 'AND' | 'OR';
}

interface ArtistFilters {
  include?: string[];
  exclude?: string[];
}

export interface Filters {
  artists?: ArtistFilters;
  reviews?: ReviewScores;
  genres?: string[];
  album_types?: string[];
  release_status?: string | string[];  // New format: string ("upcoming" | "released" | "both"), or legacy: string[]
  operator?: 'AND' | 'OR';
  min_score?: number;  // For Pitchfork
  min_critic_score?: number;  // For Metacritic
  min_user_score?: number;  // For Metacritic
}

interface FilterBuilderProps {
  filters?: Filters;
  onChange: (filters: Filters) => void;
  ruleType: 'library_artists' | 'specific_artist' | 'custom';
}

export function FilterBuilder({ filters = {}, onChange, ruleType }: FilterBuilderProps) {
  const [newGenre, setNewGenre] = useState('');
  const [newIncludeArtist, setNewIncludeArtist] = useState('');
  const [newExcludeArtist, setNewExcludeArtist] = useState('');

  // Only show filters for custom and library_artists rules
  if (ruleType === 'specific_artist') {
    return (
      <div className="text-sm text-gray-500 italic">
        Specific artist rules don't use filters. The artist is specified by MBID above.
      </div>
    );
  }

  const updateFilters = (updates: Partial<Filters>) => {
    onChange({ ...filters, ...updates });
  };

  const updateReviews = (updates: Partial<ReviewScores>) => {
    updateFilters({
      reviews: { ...filters.reviews, ...updates },
    });
  };

  const updateArtists = (updates: Partial<ArtistFilters>) => {
    updateFilters({
      artists: { ...filters.artists, ...updates },
    });
  };

  const addGenre = () => {
    if (newGenre.trim()) {
      const genres = filters.genres || [];
      updateFilters({ genres: [...genres, newGenre.trim()] });
      setNewGenre('');
    }
  };

  const removeGenre = (genre: string) => {
    updateFilters({
      genres: (filters.genres || []).filter(g => g !== genre),
    });
  };

  const addIncludeArtist = () => {
    if (newIncludeArtist.trim()) {
      const include = filters.artists?.include || [];
      updateArtists({ include: [...include, newIncludeArtist.trim()] });
      setNewIncludeArtist('');
    }
  };

  const removeIncludeArtist = (mbid: string) => {
    updateArtists({
      include: (filters.artists?.include || []).filter(a => a !== mbid),
    });
  };

  const addExcludeArtist = () => {
    if (newExcludeArtist.trim()) {
      const exclude = filters.artists?.exclude || [];
      updateArtists({ exclude: [...exclude, newExcludeArtist.trim()] });
      setNewExcludeArtist('');
    }
  };

  const removeExcludeArtist = (mbid: string) => {
    updateArtists({
      exclude: (filters.artists?.exclude || []).filter(a => a !== mbid),
    });
  };

  const toggleAlbumType = (type: string) => {
    const types = filters.album_types || [];
    if (types.includes(type)) {
      updateFilters({ album_types: types.filter(t => t !== type) });
    } else {
      updateFilters({ album_types: [...types, type] });
    }
  };

  const toggleReleaseStatus = (status: string) => {
    const statuses = filters.release_status || [];
    if (statuses.includes(status)) {
      updateFilters({ release_status: statuses.filter(s => s !== status) });
    } else {
      updateFilters({ release_status: [...statuses, status] });
    }
  };

  return (
    <div className="space-y-6 border border-gray-300 rounded-lg p-4 bg-gray-50">
      <div className="flex items-center justify-between">
        <h4 className="font-medium text-gray-900">Filters</h4>
        <div className="text-sm text-gray-500">
          Combine with:
          <select
            value={filters.operator || 'AND'}
            onChange={(e) => updateFilters({ operator: e.target.value as 'AND' | 'OR' })}
            className="ml-2 px-2 py-1 border border-gray-300 rounded text-sm"
          >
            <option value="AND">AND</option>
            <option value="OR">OR</option>
          </select>
        </div>
      </div>

      {/* Artist Filters (only for custom rules) */}
      {ruleType === 'custom' && (
        <div className="space-y-4">
          <h5 className="text-sm font-medium text-gray-700">Artists</h5>

          {/* Include Artists */}
          <div>
            <label className="block text-xs font-medium text-gray-600 mb-2">
              Include Only These Artists (MBIDs)
            </label>
            <div className="flex gap-2 mb-2">
              <input
                type="text"
                value={newIncludeArtist}
                onChange={(e) => setNewIncludeArtist(e.target.value)}
                onKeyPress={(e) => e.key === 'Enter' && (e.preventDefault(), addIncludeArtist())}
                placeholder="xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
                className="flex-1 px-2 py-1 text-sm border border-gray-300 rounded"
              />
              <button
                type="button"
                onClick={addIncludeArtist}
                className="px-3 py-1 bg-blue-100 text-blue-700 rounded hover:bg-blue-200 text-sm"
              >
                <Plus className="h-4 w-4" />
              </button>
            </div>
            <div className="flex flex-wrap gap-2">
              {(filters.artists?.include || []).map(mbid => (
                <span
                  key={mbid}
                  className="inline-flex items-center gap-1 px-2 py-1 bg-blue-100 text-blue-800 rounded text-xs"
                >
                  {mbid}
                  <button
                    type="button"
                    onClick={() => removeIncludeArtist(mbid)}
                    className="hover:text-blue-900"
                  >
                    <X className="h-3 w-3" />
                  </button>
                </span>
              ))}
            </div>
          </div>

          {/* Exclude Artists */}
          <div>
            <label className="block text-xs font-medium text-gray-600 mb-2">
              Exclude These Artists (MBIDs)
            </label>
            <div className="flex gap-2 mb-2">
              <input
                type="text"
                value={newExcludeArtist}
                onChange={(e) => setNewExcludeArtist(e.target.value)}
                onKeyPress={(e) => e.key === 'Enter' && (e.preventDefault(), addExcludeArtist())}
                placeholder="xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
                className="flex-1 px-2 py-1 text-sm border border-gray-300 rounded"
              />
              <button
                type="button"
                onClick={addExcludeArtist}
                className="px-3 py-1 bg-red-100 text-red-700 rounded hover:bg-red-200 text-sm"
              >
                <Plus className="h-4 w-4" />
              </button>
            </div>
            <div className="flex flex-wrap gap-2">
              {(filters.artists?.exclude || []).map(mbid => (
                <span
                  key={mbid}
                  className="inline-flex items-center gap-1 px-2 py-1 bg-red-100 text-red-800 rounded text-xs"
                >
                  {mbid}
                  <button
                    type="button"
                    onClick={() => removeExcludeArtist(mbid)}
                    className="hover:text-red-900"
                  >
                    <X className="h-3 w-3" />
                  </button>
                </span>
              ))}
            </div>
          </div>
        </div>
      )}

      {/* Review Scores */}
      <div className="space-y-4">
        <h5 className="text-sm font-medium text-gray-700">Review Scores</h5>

        <div className="grid grid-cols-2 gap-4">
          {/* Metacritic */}
          <div>
            <label className="block text-xs font-medium text-gray-600 mb-2">
              Metacritic Score (0-100)
            </label>
            <div className="flex gap-2 items-center">
              <input
                type="number"
                min="0"
                max="100"
                value={filters.reviews?.metacritic?.min_score || ''}
                onChange={(e) => updateReviews({
                  metacritic: {
                    ...filters.reviews?.metacritic,
                    min_score: e.target.value ? parseFloat(e.target.value) : undefined
                  }
                })}
                placeholder="Min"
                className="w-full px-2 py-1 text-sm border border-gray-300 rounded"
              />
            </div>
          </div>

          {/* Pitchfork */}
          <div>
            <label className="block text-xs font-medium text-gray-600 mb-2">
              Pitchfork Score (0-10)
            </label>
            <div className="flex gap-2 items-center">
              <input
                type="number"
                min="0"
                max="10"
                step="0.1"
                value={filters.reviews?.pitchfork?.min_score || ''}
                onChange={(e) => updateReviews({
                  pitchfork: {
                    ...filters.reviews?.pitchfork,
                    min_score: e.target.value ? parseFloat(e.target.value) : undefined
                  }
                })}
                placeholder="Min"
                className="w-full px-2 py-1 text-sm border border-gray-300 rounded"
              />
            </div>
          </div>
        </div>

        {(filters.reviews?.metacritic || filters.reviews?.pitchfork) && (
          <div className="text-xs text-gray-500">
            Combine scores with:
            <select
              value={filters.reviews?.operator || 'OR'}
              onChange={(e) => updateReviews({ operator: e.target.value as 'AND' | 'OR' })}
              className="ml-2 px-2 py-1 border border-gray-300 rounded text-xs"
            >
              <option value="AND">AND</option>
              <option value="OR">OR</option>
            </select>
          </div>
        )}
      </div>

      {/* Genres */}
      <div className="space-y-2">
        <h5 className="text-sm font-medium text-gray-700">Genres</h5>
        <div className="flex gap-2 mb-2">
          <input
            type="text"
            value={newGenre}
            onChange={(e) => setNewGenre(e.target.value)}
            onKeyPress={(e) => e.key === 'Enter' && (e.preventDefault(), addGenre())}
            placeholder="rock, rap, electronic..."
            className="flex-1 px-2 py-1 text-sm border border-gray-300 rounded"
          />
          <button
            type="button"
            onClick={addGenre}
            className="px-3 py-1 bg-green-100 text-green-700 rounded hover:bg-green-200 text-sm"
          >
            <Plus className="h-4 w-4" />
          </button>
        </div>
        <div className="flex flex-wrap gap-2">
          {(filters.genres || []).map(genre => (
            <span
              key={genre}
              className="inline-flex items-center gap-1 px-2 py-1 bg-green-100 text-green-800 rounded text-xs"
            >
              {genre}
              <button
                type="button"
                onClick={() => removeGenre(genre)}
                className="hover:text-green-900"
              >
                <X className="h-3 w-3" />
              </button>
            </span>
          ))}
        </div>
        <p className="text-xs text-gray-500 italic">Note: Genre filtering requires MusicBrainz genre data</p>
      </div>

      {/* Album Types */}
      <div className="space-y-2">
        <h5 className="text-sm font-medium text-gray-700">Album Types</h5>
        <div className="flex flex-wrap gap-2">
          {['Album', 'EP', 'Single', 'Compilation', 'Live'].map(type => (
            <button
              key={type}
              type="button"
              onClick={() => toggleAlbumType(type)}
              className={`px-3 py-1 rounded text-sm transition-colors ${
                (filters.album_types || []).includes(type)
                  ? 'bg-blue-600 text-white'
                  : 'bg-gray-200 text-gray-700 hover:bg-gray-300'
              }`}
            >
              {type}
            </button>
          ))}
        </div>
      </div>

      {/* Release Status */}
      <div className="space-y-2">
        <h5 className="text-sm font-medium text-gray-700">Release Status</h5>
        <div className="flex gap-2">
          {['upcoming', 'released'].map(status => (
            <button
              key={status}
              type="button"
              onClick={() => toggleReleaseStatus(status)}
              className={`px-3 py-1 rounded text-sm transition-colors ${
                (filters.release_status || []).includes(status)
                  ? 'bg-purple-600 text-white'
                  : 'bg-gray-200 text-gray-700 hover:bg-gray-300'
              }`}
            >
              {status.charAt(0).toUpperCase() + status.slice(1)}
            </button>
          ))}
        </div>
      </div>
    </div>
  );
}
