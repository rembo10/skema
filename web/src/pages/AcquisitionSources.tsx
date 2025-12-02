import { useEffect, useState } from 'react';
import { api } from '../lib/api';
import type { AcquisitionSource, CatalogArtist, WantedAlbum } from '../types/api';
import { Settings, CheckCircle, XCircle, Users, Music, Plus, Pencil, Trash2, RefreshCw } from 'lucide-react';
import toast from 'react-hot-toast';
import { SourceModal, type SourceFormData } from '../components/SourceModal';

export default function AcquisitionSources() {
  const [sources, setSources] = useState<AcquisitionSource[]>([]);
  const [artists, setArtists] = useState<CatalogArtist[]>([]);
  const [albums, setAlbums] = useState<WantedAlbum[]>([]);
  const [loading, setLoading] = useState(true);
  const [modalOpen, setModalOpen] = useState(false);
  const [editingSource, setEditingSource] = useState<AcquisitionSource | undefined>(undefined);
  const [syncingSourceId, setSyncingSourceId] = useState<number | null>(null);

  useEffect(() => {
    loadData();
  }, []);

  const loadData = async () => {
    try {
      setLoading(true);
      const [sourcesData, artistsData, albumsData] = await Promise.all([
        api.getAcquisitionSources(),
        api.getCatalogArtists(true), // Get all followed artists (manual + source-based)
        api.getWantedAlbums(),
      ]);
      setSources(sourcesData);
      setArtists(artistsData);
      setAlbums(albumsData);
    } catch (error) {
      toast.error('Failed to load acquisition sources');
      console.error('Error loading sources:', error);
    } finally {
      setLoading(false);
    }
  };

  const getStatsForSource = (sourceId: number) => {
    const artistCount = artists.filter(a => a.added_by_source_id === sourceId).length;
    const albumCount = albums.filter(a => a.added_by_source_id === sourceId).length;
    return { artistCount, albumCount };
  };

  const toggleSource = async (sourceId: number, currentlyEnabled: boolean) => {
    try {
      if (currentlyEnabled) {
        await api.disableAcquisitionSource(sourceId);
        toast.success('Source disabled');
      } else {
        await api.enableAcquisitionSource(sourceId);
        toast.success('Source enabled');
      }
      // Optimistically update just the toggled source
      setSources(prevSources =>
        prevSources.map(source =>
          source.id === sourceId
            ? { ...source, enabled: !currentlyEnabled }
            : source
        )
      );
    } catch (error) {
      console.error('Failed to toggle source:', error);
      toast.error('Failed to update source');
    }
  };

  const handleCreateSource = () => {
    setEditingSource(undefined);
    setModalOpen(true);
  };

  const handleEditSource = (source: AcquisitionSource) => {
    setEditingSource(source);
    setModalOpen(true);
  };

  const handleSaveSource = async (formData: SourceFormData) => {
    try {
      // Determine which filters to serialize based on source type
      let filtersJson: string | undefined;
      if (formData.source_type === 'youtube_music' && formData.youtube_music_filters) {
        filtersJson = JSON.stringify(formData.youtube_music_filters);
      } else if (formData.filters) {
        filtersJson = JSON.stringify(formData.filters);
      }

      const sourceData = {
        name: formData.name,
        description: formData.description || undefined,
        source_type: formData.source_type,
        enabled: formData.enabled,
        filters: filtersJson,
      };

      if (editingSource) {
        // Update existing source
        await api.updateAcquisitionSource(editingSource.id, sourceData);
        toast.success('Source updated successfully');
      } else {
        // Create new source
        await api.createAcquisitionSource(sourceData);
        toast.success('Source created successfully');
      }

      // Reload data to reflect changes
      await loadData();
    } catch (error) {
      console.error('Failed to save source:', error);
      toast.error('Failed to save source');
      throw error; // Re-throw so modal can handle it
    }
  };

  const handleDeleteSource = async (sourceId: number) => {
    if (!confirm('Are you sure you want to delete this source? This action cannot be undone.')) {
      return;
    }

    try {
      await api.deleteAcquisitionSource(sourceId);
      toast.success('Source deleted successfully');
      // Reload data to reflect changes
      await loadData();
    } catch (error) {
      console.error('Failed to delete source:', error);
      toast.error('Failed to delete source');
    }
  };

  const handleSyncSource = async (sourceId: number) => {
    setSyncingSourceId(sourceId);
    try {
      const result = await api.syncAcquisitionSource(sourceId);
      if (result.success) {
        toast.success(`Sync completed: ${result.count} item(s) added`);
        // Reload data to reflect changes
        await loadData();
      } else {
        toast.error(`Sync failed: ${result.message}`);
      }
    } catch (error) {
      console.error('Failed to sync source:', error);
      toast.error('Failed to sync source');
    } finally {
      setSyncingSourceId(null);
    }
  };

  const formatSourceType = (type: string) => {
    switch (type) {
      case 'library_artists':
        return 'Library Artists';
      case 'metacritic':
        return 'Metacritic';
      case 'pitchfork':
        return 'Pitchfork';
      case 'youtube_music':
      case 'you_tube_music':
        return 'YouTube Music';
      default:
        return type;
    }
  };

  const getSourceTypeColor = (type: string) => {
    switch (type) {
      case 'library_artists':
        return 'bg-blue-100 text-blue-800 border-blue-200';
      case 'metacritic':
        return 'bg-purple-100 text-purple-800 border-purple-200';
      case 'pitchfork':
        return 'bg-orange-100 text-orange-800 border-orange-200';
      case 'youtube_music':
      case 'you_tube_music':
        return 'bg-red-100 text-red-800 border-red-200';
      default:
        return 'bg-gray-100 text-gray-800 border-gray-200';
    }
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
          <h1 className="text-3xl font-bold text-dark-text">Input Sources</h1>
          <p className="text-dark-text-secondary mt-2">
            {sources.length} source{sources.length !== 1 ? 's' : ''} configured
          </p>
        </div>
        <button
          onClick={handleCreateSource}
          className="btn-primary flex items-center gap-2"
        >
          <Plus className="h-5 w-5" />
          Create Source
        </button>
      </div>

      {/* Sources List */}
      {sources.length === 0 ? (
        <div className="card p-12 text-center">
          <Settings className="mx-auto h-12 w-12 text-dark-text-tertiary" />
          <h3 className="mt-4 text-sm font-medium text-dark-text">No sources configured</h3>
          <p className="mt-2 text-sm text-dark-text-secondary">
            Create your first source to start automatically following artists and albums
          </p>
        </div>
      ) : (
        <div className="space-y-4">
          {sources.map((source) => {
            const stats = getStatsForSource(source.id);

            return (
              <div
                key={source.id}
                className="card p-6 hover:border-dark-border-bright transition-all duration-200"
              >
                <div className="flex items-start justify-between">
                  {/* Source Info */}
                  <div className="flex-1">
                    <div className="flex items-center gap-3 flex-wrap">
                      <h3 className="text-lg font-semibold text-dark-text">
                        {source.name}
                      </h3>
                      {source.enabled ? (
                        <span className="inline-flex items-center gap-1 px-2 py-1 rounded-lg text-xs font-medium bg-dark-success-muted text-dark-success border border-dark-success/30">
                          <CheckCircle className="h-3 w-3" />
                          Enabled
                        </span>
                      ) : (
                        <span className="inline-flex items-center gap-1 px-2 py-1 rounded-lg text-xs font-medium bg-dark-bg-subtle text-dark-text-tertiary border border-dark-border">
                          <XCircle className="h-3 w-3" />
                          Disabled
                        </span>
                      )}
                      <span className="inline-flex items-center px-2 py-1 rounded-lg text-xs font-medium bg-dark-bg-subtle text-dark-text-secondary border border-dark-border">
                        {formatSourceType(source.source_type)}
                      </span>
                    </div>

                    {source.description && (
                      <p className="mt-2 text-sm text-dark-text-secondary">{source.description}</p>
                    )}

                    {/* Stats */}
                    <div className="mt-4 flex items-center gap-6">
                      <div className="flex items-center gap-2 text-sm">
                        <Users className="h-4 w-4 text-dark-accent" />
                        <span className="font-medium text-dark-text tabular-nums">{stats.artistCount}</span>
                        <span className="text-dark-text-secondary">artist{stats.artistCount !== 1 ? 's' : ''}</span>
                      </div>
                      <div className="flex items-center gap-2 text-sm">
                        <Music className="h-4 w-4 text-dark-info" />
                        <span className="font-medium text-dark-text tabular-nums">{stats.albumCount}</span>
                        <span className="text-dark-text-secondary">album{stats.albumCount !== 1 ? 's' : ''}</span>
                      </div>
                    </div>

                    {/* Metadata */}
                    <div className="mt-4 flex items-center gap-4 text-xs text-dark-text-tertiary">
                      {source.filters && <span>Custom filters</span>}
                      {source.created_at && (
                        <span>
                          {source.filters && '• '}
                          {new Date(source.created_at).toLocaleDateString('en-US', {
                            year: 'numeric',
                            month: 'short',
                            day: 'numeric',
                          })}
                        </span>
                      )}
                    </div>
                  </div>

                  {/* Actions */}
                  <div className="flex items-center gap-2">
                    {/* Only show sync for non-library_artists sources */}
                    {source.source_type !== 'library_artists' && (
                      <button
                        onClick={() => handleSyncSource(source.id)}
                        disabled={syncingSourceId === source.id}
                        className="p-2 text-dark-text-tertiary hover:text-dark-accent hover:bg-dark-bg-hover rounded-lg transition-all duration-200 disabled:opacity-50 disabled:cursor-not-allowed"
                        title="Sync now"
                      >
                        <RefreshCw className={`h-4 w-4 ${syncingSourceId === source.id ? 'animate-spin' : ''}`} />
                      </button>
                    )}
                    <button
                      onClick={() => handleEditSource(source)}
                      className="p-2 text-dark-text-tertiary hover:text-dark-accent hover:bg-dark-bg-hover rounded-lg transition-all duration-200"
                      title="Edit source"
                    >
                      <Pencil className="h-4 w-4" />
                    </button>
                    <button
                      onClick={() => handleDeleteSource(source.id)}
                      className="p-2 text-dark-text-tertiary hover:text-dark-error hover:bg-dark-error-muted rounded-lg transition-all duration-200"
                      title="Delete source"
                    >
                      <Trash2 className="h-4 w-4" />
                    </button>
                    <div className="w-px h-6 bg-dark-border mx-1" />
                    <button
                      onClick={() => toggleSource(source.id, source.enabled)}
                      className={`relative inline-flex h-6 w-11 flex-shrink-0 items-center rounded-full transition-all duration-200 focus:outline-none focus:ring-2 focus:ring-dark-accent focus:ring-offset-2 focus:ring-offset-dark-bg-elevated ${
                        source.enabled ? 'bg-dark-accent' : 'bg-dark-bg-subtle'
                      }`}
                      title={source.enabled ? 'Disable source' : 'Enable source'}
                    >
                      <span
                        className={`inline-block h-4 w-4 transform rounded-full bg-dark-bg transition-transform ${
                          source.enabled ? 'translate-x-6' : 'translate-x-1'
                        }`}
                      />
                    </button>
                  </div>
                </div>
              </div>
            );
          })}
        </div>
      )}

      {/* Summary Stats */}
      {sources.length > 0 && (
        <div className="card p-4 border-dark-accent/30 bg-dark-accent/5">
          <div className="flex items-start gap-3">
            <Settings className="h-5 w-5 text-dark-accent mt-0.5" />
            <div>
              <h3 className="text-sm font-medium text-dark-text">Acquisition Summary</h3>
              <p className="mt-1 text-sm text-dark-text-secondary">
                {sources.filter(s => s.enabled).length} of {sources.length} sources enabled • {' '}
                {artists.length} artists followed • {' '}
                {albums.filter(a => a.status === 'wanted').length} albums wanted
              </p>
            </div>
          </div>
        </div>
      )}

      {/* Source Modal */}
      <SourceModal
        isOpen={modalOpen}
        onClose={() => setModalOpen(false)}
        onSave={handleSaveSource}
        source={editingSource}
      />
    </div>
  );
}
