import { useState, useEffect } from 'react';
import { X } from 'lucide-react';
import type { AcquisitionSource } from '../types/api';
import { FilterBuilder, type Filters } from './FilterBuilder';

interface SourceModalProps {
  isOpen: boolean;
  onClose: () => void;
  onSave: (source: SourceFormData) => Promise<void>;
  source?: AcquisitionSource;  // If provided, we're editing; otherwise creating
}

export interface YouTubeMusicFilters {
  playlist_id: string;
  api_key: string;
  auto_follow_artists: boolean;
  auto_want_albums: boolean;
  poll_interval_minutes?: number;
}

export interface SourceFormData {
  name: string;
  description: string;
  source_type: 'library_artists' | 'metacritic' | 'pitchfork' | 'youtube_music';
  artist_mbid?: string;
  enabled: boolean;
  filters?: Filters;
  youtube_music_filters?: YouTubeMusicFilters;
}

export function SourceModal({ isOpen, onClose, onSave, source }: SourceModalProps) {
  const [formData, setFormData] = useState<SourceFormData>({
    name: '',
    description: '',
    source_type: 'library_artists',
    enabled: true,
  });
  const [saving, setSaving] = useState(false);

  // Reset form when modal opens or source changes
  useEffect(() => {
    if (isOpen) {
      if (source) {
        // Editing existing source - parse JSON filters if present
        let parsedFilters: Filters | undefined;
        let ytFilters: YouTubeMusicFilters | undefined;
        
        if (source.filters) {
          try {
            const parsed = JSON.parse(source.filters);
            // Handle both 'youtube_music' and 'you_tube_music' formats
            if (source.source_type === 'youtube_music' || source.source_type === 'you_tube_music') {
              ytFilters = parsed as YouTubeMusicFilters;
            } else {
              parsedFilters = parsed as Filters;
            }
          } catch (e) {
            console.error('Failed to parse filters:', e);
          }
        }

        // Normalize source_type (backend uses you_tube_music, frontend uses youtube_music)
        const normalizedSourceType = source.source_type === 'you_tube_music' ? 'youtube_music' : source.source_type;
        
        setFormData({
          name: source.name,
          description: source.description || '',
          source_type: normalizedSourceType as 'library_artists' | 'metacritic' | 'pitchfork' | 'youtube_music',
          enabled: source.enabled,
          filters: parsedFilters,
          youtube_music_filters: ytFilters,
        });
      } else {
        // Creating new source
        setFormData({
          name: '',
          description: '',
          source_type: 'library_artists',
          enabled: true,
        });
      }
    }
  }, [isOpen, source]);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setSaving(true);
    try {
      await onSave(formData);
      onClose();
    } finally {
      setSaving(false);
    }
  };

  if (!isOpen) return null;

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/50">
      {/* Modal */}
      <div className="relative bg-dark-bg-elevated border border-dark-border rounded-lg shadow-xl max-w-2xl w-full mx-4 max-h-[90vh] overflow-y-auto">
        {/* Header */}
        <div className="flex items-center justify-between p-6 border-b border-dark-border">
          <h2 className="text-xl font-semibold text-dark-text">
            {source ? 'Edit Source' : 'Create New Source'}
          </h2>
          <button
            onClick={onClose}
            className="text-dark-text-secondary hover:text-dark-text transition-colors"
          >
            <X className="h-5 w-5" />
          </button>
        </div>

        {/* Form */}
        <form onSubmit={handleSubmit} className="p-6 space-y-6">
          {/* Source Name */}
          <div>
            <label htmlFor="name" className="block text-sm font-medium text-dark-text mb-2">
              Source Name <span className="text-dark-error">*</span>
            </label>
            <input
              type="text"
              id="name"
              required
              value={formData.name}
              onChange={(e) => setFormData({ ...formData, name: e.target.value })}
              className="w-full px-3 py-2 bg-dark-bg border border-dark-border rounded-md text-dark-text placeholder-dark-text-tertiary focus:outline-none focus:ring-2 focus:ring-dark-accent focus:border-transparent transition-all"
              placeholder="My New Source"
            />
          </div>

          {/* Description */}
          <div>
            <label htmlFor="description" className="block text-sm font-medium text-dark-text mb-2">
              Description
            </label>
            <textarea
              id="description"
              value={formData.description}
              onChange={(e) => setFormData({ ...formData, description: e.target.value })}
              rows={3}
              className="w-full px-3 py-2 bg-dark-bg border border-dark-border rounded-md text-dark-text placeholder-dark-text-tertiary focus:outline-none focus:ring-2 focus:ring-dark-accent focus:border-transparent transition-all resize-none"
              placeholder="Describe what this source does..."
            />
          </div>

          {/* Source Type */}
          <div>
            <label htmlFor="source_type" className="block text-sm font-medium text-dark-text mb-2">
              Source Type <span className="text-dark-error">*</span>
            </label>
            <select
              id="source_type"
              required
              value={formData.source_type}
              onChange={(e) => setFormData({
                ...formData,
                source_type: e.target.value as 'library_artists' | 'metacritic' | 'pitchfork' | 'youtube_music'
              })}
              className="w-full px-3 py-2 bg-dark-bg border border-dark-border rounded-md text-dark-text focus:outline-none focus:ring-2 focus:ring-dark-accent focus:border-transparent transition-all"
            >
              <option value="library_artists">Library Artists</option>
              <option value="metacritic">Metacritic</option>
              <option value="pitchfork">Pitchfork</option>
              <option value="youtube_music">YouTube Music Playlist</option>
            </select>
            <p className="mt-1 text-sm text-dark-text-secondary">
              {formData.source_type === 'library_artists' && 'Automatically track all artists found in your library'}
              {formData.source_type === 'metacritic' && 'Track albums from Metacritic based on scores and genres'}
              {formData.source_type === 'pitchfork' && 'Track albums from Pitchfork based on scores and genres'}
              {formData.source_type === 'youtube_music' && 'Track artists from a YouTube Music playlist'}
            </p>
          </div>

          {/* Library Artists Filters */}
          {formData.source_type === 'library_artists' && (
            <div className="space-y-6 p-4 bg-dark-bg rounded-lg border border-dark-border">
              {/* Release Status */}
              <div>
                <label className="block text-sm font-medium text-dark-text mb-3">
                  Which albums to track
                </label>
                <div className="space-y-2">
                  <label className="flex items-center hover:bg-dark-bg-hover p-2 rounded transition-colors cursor-pointer">
                    <input
                      type="checkbox"
                      checked={
                        formData.filters?.release_status === 'upcoming' ||
                        formData.filters?.release_status === 'both' ||
                        !formData.filters?.release_status
                      }
                      onChange={(e) => {
                        const currentStatus = formData.filters?.release_status;
                        let newStatus: string;

                        if (e.target.checked) {
                          // Check upcoming
                          if (currentStatus === 'released') {
                            newStatus = 'both';
                          } else {
                            newStatus = 'upcoming';
                          }
                        } else {
                          // Uncheck upcoming
                          if (currentStatus === 'both') {
                            newStatus = 'released';
                          } else {
                            newStatus = 'upcoming'; // Keep at least one checked
                          }
                        }

                        setFormData({
                          ...formData,
                          filters: { ...formData.filters, release_status: newStatus }
                        });
                      }}
                      className="h-4 w-4 text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg border-dark-border rounded"
                    />
                    <span className="ml-3 text-sm text-dark-text">Upcoming albums</span>
                  </label>
                  <label className="flex items-center hover:bg-dark-bg-hover p-2 rounded transition-colors cursor-pointer">
                    <input
                      type="checkbox"
                      checked={
                        formData.filters?.release_status === 'released' ||
                        formData.filters?.release_status === 'both'
                      }
                      onChange={(e) => {
                        const currentStatus = formData.filters?.release_status;
                        let newStatus: string;

                        if (e.target.checked) {
                          // Check released
                          if (currentStatus === 'upcoming' || !currentStatus) {
                            newStatus = 'both';
                          } else {
                            newStatus = 'released';
                          }
                        } else {
                          // Uncheck released
                          if (currentStatus === 'both') {
                            newStatus = 'upcoming';
                          } else {
                            newStatus = 'released'; // Keep at least one checked
                          }
                        }

                        setFormData({
                          ...formData,
                          filters: { ...formData.filters, release_status: newStatus }
                        });
                      }}
                      className="h-4 w-4 text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg border-dark-border rounded"
                    />
                    <span className="ml-3 text-sm text-dark-text">Released albums</span>
                  </label>
                </div>
              </div>

              {/* Album Types */}
              <div>
                <label className="block text-sm font-medium text-dark-text mb-3">
                  Album Types
                </label>
                <div className="space-y-2">
                  {['Album', 'EP', 'Single', 'Compilation', 'Live'].map((type) => (
                    <label key={type} className="flex items-center hover:bg-dark-bg-hover p-2 rounded transition-colors cursor-pointer">
                      <input
                        type="checkbox"
                        checked={formData.filters?.album_types?.includes(type) || false}
                        onChange={(e) => {
                          const currentTypes = formData.filters?.album_types || [];
                          const newTypes = e.target.checked
                            ? [...currentTypes, type]
                            : currentTypes.filter(t => t !== type);
                          setFormData({
                            ...formData,
                            filters: { ...formData.filters, album_types: newTypes }
                          });
                        }}
                        className="h-4 w-4 text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg border-dark-border rounded"
                      />
                      <span className="ml-3 text-sm text-dark-text">{type}</span>
                    </label>
                  ))}
                </div>
                <p className="mt-2 text-xs text-dark-text-tertiary">Leave all unchecked to track all types</p>
              </div>
            </div>
          )}

          {/* Metacritic Filters */}
          {formData.source_type === 'metacritic' && (
            <div className="space-y-4 p-4 bg-dark-bg rounded-lg border border-dark-border">
              <div>
                <label className="block text-sm font-medium text-dark-text mb-3">
                  Genres
                </label>
                <div className="grid grid-cols-2 gap-2">
                  {['Pop', 'Rock', 'Alternative', 'Rap', 'Country', 'Electronic', 'R&B', 'Jazz', 'Folk', 'Metal'].map((genre) => (
                    <label key={genre} className="flex items-center hover:bg-dark-bg-hover p-2 rounded transition-colors cursor-pointer">
                      <input
                        type="checkbox"
                        checked={formData.filters?.genres?.includes(genre.toLowerCase()) || false}
                        onChange={(e) => {
                          const currentGenres = formData.filters?.genres || [];
                          const newGenres = e.target.checked
                            ? [...currentGenres, genre.toLowerCase()]
                            : currentGenres.filter(g => g !== genre.toLowerCase());
                          setFormData({
                            ...formData,
                            filters: { ...formData.filters, genres: newGenres }
                          });
                        }}
                        className="h-4 w-4 text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg border-dark-border rounded"
                      />
                      <span className="ml-3 text-sm text-dark-text">{genre}</span>
                    </label>
                  ))}
                </div>
              </div>

              <div>
                <label htmlFor="min_critic_score" className="block text-sm font-medium text-dark-text mb-2">
                  Minimum Critic Score: <span className="text-dark-accent font-semibold">{formData.filters?.min_critic_score || 0}</span>
                </label>
                <input
                  type="range"
                  id="min_critic_score"
                  min="0"
                  max="100"
                  value={formData.filters?.min_critic_score || 0}
                  onChange={(e) => setFormData({
                    ...formData,
                    filters: { ...formData.filters, min_critic_score: parseInt(e.target.value) }
                  })}
                  className="w-full h-2 bg-dark-bg-hover rounded-lg appearance-none cursor-pointer accent-dark-accent"
                />
                <div className="flex justify-between text-xs text-dark-text-tertiary mt-1">
                  <span>0</span>
                  <span>100</span>
                </div>
              </div>

              <div>
                <label htmlFor="min_user_score" className="block text-sm font-medium text-dark-text mb-2">
                  Minimum User Score: <span className="text-dark-accent font-semibold">{formData.filters?.min_user_score?.toFixed(1) || '0.0'}</span>
                </label>
                <input
                  type="range"
                  id="min_user_score"
                  min="0"
                  max="10"
                  step="0.1"
                  value={formData.filters?.min_user_score || 0}
                  onChange={(e) => setFormData({
                    ...formData,
                    filters: { ...formData.filters, min_user_score: parseFloat(e.target.value) }
                  })}
                  className="w-full h-2 bg-dark-bg-hover rounded-lg appearance-none cursor-pointer accent-dark-accent"
                />
                <div className="flex justify-between text-xs text-dark-text-tertiary mt-1">
                  <span>0.0</span>
                  <span>10.0</span>
                </div>
              </div>
            </div>
          )}

          {/* Pitchfork Filters */}
          {formData.source_type === 'pitchfork' && (
            <div className="space-y-4 p-4 bg-dark-bg rounded-lg border border-dark-border">
              <div>
                <label className="block text-sm font-medium text-dark-text mb-3">
                  Genres
                </label>
                <div className="grid grid-cols-2 gap-2">
                  {['Pop', 'Rock', 'Experimental', 'Electronic', 'Rap', 'Jazz', 'Metal', 'Folk/Country'].map((genre) => (
                    <label key={genre} className="flex items-center hover:bg-dark-bg-hover p-2 rounded transition-colors cursor-pointer">
                      <input
                        type="checkbox"
                        checked={formData.filters?.genres?.includes(genre.toLowerCase().replace('/', '')) || false}
                        onChange={(e) => {
                          const genreKey = genre.toLowerCase().replace('/', '');
                          const currentGenres = formData.filters?.genres || [];
                          const newGenres = e.target.checked
                            ? [...currentGenres, genreKey]
                            : currentGenres.filter(g => g !== genreKey);
                          setFormData({
                            ...formData,
                            filters: { ...formData.filters, genres: newGenres }
                          });
                        }}
                        className="h-4 w-4 text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg border-dark-border rounded"
                      />
                      <span className="ml-3 text-sm text-dark-text">{genre}</span>
                    </label>
                  ))}
                </div>
              </div>

              <div>
                <label htmlFor="min_score" className="block text-sm font-medium text-dark-text mb-2">
                  Minimum Score: <span className="text-dark-accent font-semibold">{formData.filters?.min_score?.toFixed(1) || '0.0'}</span>
                </label>
                <input
                  type="range"
                  id="min_score"
                  min="0"
                  max="10"
                  step="0.1"
                  value={formData.filters?.min_score || 0}
                  onChange={(e) => setFormData({
                    ...formData,
                    filters: { ...formData.filters, min_score: parseFloat(e.target.value) }
                  })}
                  className="w-full h-2 bg-dark-bg-hover rounded-lg appearance-none cursor-pointer accent-dark-accent"
                />
                <div className="flex justify-between text-xs text-dark-text-tertiary mt-1">
                  <span>0.0</span>
                  <span>10.0</span>
                </div>
              </div>
            </div>
          )}

          {/* YouTube Music Filters */}
          {formData.source_type === 'youtube_music' && (
            <div className="space-y-4 p-4 bg-dark-bg rounded-lg border border-dark-border">
              <div>
                <label htmlFor="playlist_id" className="block text-sm font-medium text-dark-text mb-2">
                  Playlist ID <span className="text-dark-error">*</span>
                </label>
                <input
                  type="text"
                  id="playlist_id"
                  required
                  value={formData.youtube_music_filters?.playlist_id || ''}
                  onChange={(e) => setFormData({
                    ...formData,
                    youtube_music_filters: {
                      ...formData.youtube_music_filters,
                      playlist_id: e.target.value,
                      api_key: formData.youtube_music_filters?.api_key || '',
                      auto_follow_artists: formData.youtube_music_filters?.auto_follow_artists ?? true,
                      auto_want_albums: formData.youtube_music_filters?.auto_want_albums ?? false,
                    }
                  })}
                  className="w-full px-3 py-2 bg-dark-bg border border-dark-border rounded-md text-dark-text placeholder-dark-text-tertiary focus:outline-none focus:ring-2 focus:ring-dark-accent focus:border-transparent transition-all"
                  placeholder="PLxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
                />
                <p className="mt-1 text-xs text-dark-text-secondary">
                  The playlist ID from the YouTube Music URL (e.g., PLxxxxxxx from music.youtube.com/playlist?list=PLxxxxxxx)
                </p>
              </div>

              <div>
                <label htmlFor="api_key" className="block text-sm font-medium text-dark-text mb-2">
                  YouTube API Key <span className="text-dark-error">*</span>
                </label>
                <input
                  type="password"
                  id="api_key"
                  required
                  value={formData.youtube_music_filters?.api_key || ''}
                  onChange={(e) => setFormData({
                    ...formData,
                    youtube_music_filters: {
                      ...formData.youtube_music_filters,
                      playlist_id: formData.youtube_music_filters?.playlist_id || '',
                      api_key: e.target.value,
                      auto_follow_artists: formData.youtube_music_filters?.auto_follow_artists ?? true,
                      auto_want_albums: formData.youtube_music_filters?.auto_want_albums ?? false,
                    }
                  })}
                  className="w-full px-3 py-2 bg-dark-bg border border-dark-border rounded-md text-dark-text placeholder-dark-text-tertiary focus:outline-none focus:ring-2 focus:ring-dark-accent focus:border-transparent transition-all"
                  placeholder="AIzaSy..."
                />
                <p className="mt-1 text-xs text-dark-text-secondary">
                  Get an API key from the <a href="https://console.cloud.google.com/apis/credentials" target="_blank" rel="noopener noreferrer" className="text-dark-accent hover:underline">Google Cloud Console</a>
                </p>
              </div>

              <div className="space-y-2">
                <label className="flex items-center hover:bg-dark-bg-hover p-2 rounded transition-colors cursor-pointer">
                  <input
                    type="checkbox"
                    checked={formData.youtube_music_filters?.auto_follow_artists ?? true}
                    onChange={(e) => setFormData({
                      ...formData,
                      youtube_music_filters: {
                        ...formData.youtube_music_filters,
                        playlist_id: formData.youtube_music_filters?.playlist_id || '',
                        api_key: formData.youtube_music_filters?.api_key || '',
                        auto_follow_artists: e.target.checked,
                        auto_want_albums: formData.youtube_music_filters?.auto_want_albums ?? false,
                      }
                    })}
                    className="h-4 w-4 text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg border-dark-border rounded"
                  />
                  <span className="ml-3 text-sm text-dark-text">Auto-follow discovered artists</span>
                </label>

                <label className="flex items-center hover:bg-dark-bg-hover p-2 rounded transition-colors cursor-pointer">
                  <input
                    type="checkbox"
                    checked={formData.youtube_music_filters?.auto_want_albums ?? false}
                    onChange={(e) => setFormData({
                      ...formData,
                      youtube_music_filters: {
                        ...formData.youtube_music_filters,
                        playlist_id: formData.youtube_music_filters?.playlist_id || '',
                        api_key: formData.youtube_music_filters?.api_key || '',
                        auto_follow_artists: formData.youtube_music_filters?.auto_follow_artists ?? true,
                        auto_want_albums: e.target.checked,
                      }
                    })}
                    className="h-4 w-4 text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg border-dark-border rounded"
                  />
                  <span className="ml-3 text-sm text-dark-text">Auto-want all albums from discovered artists</span>
                </label>
              </div>
            </div>
          )}

          {/* Enabled */}
          <div className="flex items-center p-4 bg-dark-bg rounded-lg border border-dark-border">
            <input
              type="checkbox"
              id="enabled"
              checked={formData.enabled}
              onChange={(e) => setFormData({ ...formData, enabled: e.target.checked })}
              className="h-4 w-4 text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg border-dark-border rounded"
            />
            <label htmlFor="enabled" className="ml-3 block text-sm text-dark-text cursor-pointer">
              Enabled
            </label>
          </div>

          {/* Actions */}
          <div className="flex justify-end gap-3 pt-4 border-t border-dark-border">
            <button
              type="button"
              onClick={onClose}
              className="btn-secondary"
            >
              Cancel
            </button>
            <button
              type="submit"
              disabled={saving}
              className="btn-primary disabled:opacity-50 disabled:cursor-not-allowed"
            >
              {saving ? 'Saving...' : (source ? 'Update Source' : 'Create Source')}
            </button>
          </div>
        </form>
      </div>
    </div>
  );
}
