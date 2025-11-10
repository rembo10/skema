import { useEffect, useState, useRef, useCallback } from 'react';
import toast from 'react-hot-toast';
import { Save, Library, Settings, Server, Download, Search, Database, Plus, Edit2, Trash2, X, HelpCircle } from 'lucide-react';
import { api } from '../lib/api';
import { useSSE } from '../hooks/useSSE';
import { PathInput } from '../components/PathInput';
import type { Config, DownloadClient, Indexer, DownloadClientType } from '../types/api';

type TabId = 'library' | 'system' | 'server' | 'download' | 'indexers' | 'musicbrainz';

interface Tab {
  id: TabId;
  label: string;
  icon: React.ComponentType<{ className?: string }>;
}

const tabs: Tab[] = [
  { id: 'library', label: 'Library', icon: Library },
  { id: 'system', label: 'System', icon: Settings },
  { id: 'server', label: 'Server', icon: Server },
  { id: 'download', label: 'Downloads', icon: Download },
  { id: 'indexers', label: 'Indexers', icon: Search },
  { id: 'musicbrainz', label: 'MusicBrainz', icon: Database },
];

export default function Config() {
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [config, setConfig] = useState<Config | null>(null);
  const [formData, setFormData] = useState<Partial<Config>>({});
  const [activeTab, setActiveTab] = useState<TabId>('library');

  // Password is stored separately since it's not returned by the API
  const [serverPassword, setServerPassword] = useState('');
  const [passwordChanged, setPasswordChanged] = useState(false);

  // Download client editing
  const [editingClientType, setEditingClientType] = useState<'nzb' | 'torrent' | null>(null);
  const [editingClient, setEditingClient] = useState<DownloadClient | null>(null);
  const [showClientForm, setShowClientForm] = useState(false);

  // Indexer editing
  const [editingIndexer, setEditingIndexer] = useState<{ index: number; indexer: Indexer } | null>(null);
  const [showIndexerForm, setShowIndexerForm] = useState(false);

  // Autosave with debounce
  const autosaveTimerRef = useRef<NodeJS.Timeout | null>(null);

  // Autosave function
  const performSave = useCallback(async (updates: Partial<Config>, includePassword: boolean = false) => {
    // Include password if it was changed
    const updatesWithPassword = includePassword && passwordChanged
      ? { ...updates, server_password: serverPassword || null }
      : updates;

    if (Object.keys(updatesWithPassword).length === 0) return;

    setSaving(true);
    try {
      const updated = await api.updateConfig(updatesWithPassword);
      setConfig(updated);
      setFormData(updated);
      // Reset password state after successful save
      if (includePassword) {
        setServerPassword('');
        setPasswordChanged(false);
      }
      // Toast will be shown by SSE event handler when file is written
    } catch (error) {
      console.error('Failed to save config:', error);
      toast.error('Failed to save configuration');
    } finally {
      setSaving(false);
    }
  }, [passwordChanged, serverPassword]);

  // Load config on mount and subscribe to real-time updates
  useEffect(() => {
    async function loadConfig() {
      try {
        console.log('[Config] Loading config...');
        const data = await api.getConfig();
        console.log('[Config] Loaded config:', data);
            setConfig(data);
        setFormData(data);
        setServerPassword('');
        setPasswordChanged(false);
        setLoading(false);
      } catch (error: any) {
        // Don't log 401 errors - they're expected when auth is required
        if (!error.isAuthError) {
          console.error('[Config] Failed to load config:', error);
          toast.error('Failed to load configuration');
        }
        setLoading(false);
      }
    }
    loadConfig();

    // Listen for config updates from SSE
    const handleConfigUpdate = ((event: CustomEvent) => {
      const data = event.detail;
      console.log('[Config] Received config update:', data);
      setConfig(data);
      setFormData(data);
      // Don't reset password state here - it's not included in the response
      toast.success('Configuration saved');
    }) as EventListener;

    window.addEventListener('config_updated', handleConfigUpdate);
    return () => {
      window.removeEventListener('config_updated', handleConfigUpdate);
      // Clear autosave timer on unmount
      if (autosaveTimerRef.current) {
        clearTimeout(autosaveTimerRef.current);
      }
    };
  }, []);

  async function handleSave(e: React.FormEvent) {
    e.preventDefault();

    // Cancel pending autosave
    if (autosaveTimerRef.current) {
      clearTimeout(autosaveTimerRef.current);
      autosaveTimerRef.current = null;
    }

    // Only send changed fields
    const updates: Partial<Config> = {};
    if (config) {
      Object.keys(formData).forEach((key) => {
        const formKey = key as keyof Config;
        if (formData[formKey] !== config[formKey]) {
          (updates as any)[formKey] = formData[formKey];
        }
      });
    }

    await performSave(updates, true); // Include password when manually saving
  }

  function handleChange(field: keyof Config, value: any) {
    setFormData((prev) => {
      const newFormData = { ...prev, [field]: value };

      // Cancel previous autosave timer
      if (autosaveTimerRef.current) {
        clearTimeout(autosaveTimerRef.current);
      }

      // Schedule autosave after 5 seconds of no changes
      autosaveTimerRef.current = setTimeout(() => {
        if (!config) return;

        // Calculate changed fields
        const updates: Partial<Config> = {};
        Object.keys(newFormData).forEach((key) => {
          const formKey = key as keyof Config;
          if (newFormData[formKey] !== config[formKey]) {
            (updates as any)[formKey] = newFormData[formKey];
          }
        });

        performSave(updates, false); // Don't include password in autosave for security
      }, 5000);

      return newFormData;
    });
  }

  function handlePasswordChange(newPassword: string) {
    setServerPassword(newPassword);
    setPasswordChanged(true);
  }

  // Download client management
  function getClientCategory(type: DownloadClientType): 'torrent' | 'nzb' {
    return type === 'transmission' || type === 'qbittorrent' ? 'torrent' : 'nzb';
  }

  function getClientTypeName(type: DownloadClientType): string {
    switch (type) {
      case 'sabnzbd': return 'SABnzbd';
      case 'nzbget': return 'NZBGet';
      case 'transmission': return 'Transmission';
      case 'qbittorrent': return 'qBittorrent';
      default: return type;
    }
  }

  function editDownloadClient(type: 'nzb' | 'torrent') {
    const client = type === 'nzb' ? formData.download_nzb_client : formData.download_torrent_client;

    if (client) {
      setEditingClient({ ...client });
    } else {
      // Create default client based on type
      const defaultType: DownloadClientType = type === 'nzb' ? 'sabnzbd' : 'transmission';
      setEditingClient({
        type: defaultType,
        url: '',
        api_key: null,
        username: null,
        password: null,
        enabled: true,
        download_dir: null,
        category: null,
      });
    }

    setEditingClientType(type);
    setShowClientForm(true);
  }

  function saveDownloadClient() {
    if (!editingClient || !editingClientType) return;

    const fieldName = editingClientType === 'nzb' ? 'download_nzb_client' : 'download_torrent_client';
    handleChange(fieldName, editingClient);
    setShowClientForm(false);
    setEditingClient(null);
    setEditingClientType(null);
  }

  function deleteDownloadClient(type: 'nzb' | 'torrent') {
    const fieldName = type === 'nzb' ? 'download_nzb_client' : 'download_torrent_client';
    handleChange(fieldName, null);
  }

  function updateEditingClient(field: keyof DownloadClient, value: any) {
    if (!editingClient) return;
    setEditingClient({
      ...editingClient,
      [field]: value,
    });
  }

  // Indexer management
  function addIndexer() {
    const newIndexer: Indexer = {
      name: '',
      url: '',
      api_key: null,
      username: null,
      password: null,
      enabled: true,
      priority: 0,
      categories: [3000, 3010], // Default: Audio, MP3
    };
    setEditingIndexer({ index: -1, indexer: newIndexer });
    setShowIndexerForm(true);
  }

  function editIndexer(index: number) {
    const indexers = formData.indexers_list || [];
    setEditingIndexer({ index, indexer: { ...indexers[index] } });
    setShowIndexerForm(true);
  }

  function saveIndexer() {
    if (!editingIndexer) return;

    const indexers = [...(formData.indexers_list || [])];
    if (editingIndexer.index === -1) {
      // Adding new indexer
      indexers.push(editingIndexer.indexer);
    } else {
      // Updating existing indexer
      indexers[editingIndexer.index] = editingIndexer.indexer;
    }

    handleChange('indexers_list', indexers);
    setShowIndexerForm(false);
    setEditingIndexer(null);
  }

  function deleteIndexer(index: number) {
    const indexers = [...(formData.indexers_list || [])];
    indexers.splice(index, 1);
    handleChange('indexers_list', indexers);
  }

  function updateEditingIndexer(field: keyof Indexer, value: any) {
    if (!editingIndexer) return;
    setEditingIndexer({
      ...editingIndexer,
      indexer: { ...editingIndexer.indexer, [field]: value },
    });
  }

  if (loading) {
    return (
      <div className="flex items-center justify-center h-64">
        <div className="text-dark-text-secondary">Loading...</div>
      </div>
    );
  }

  return (
    <div className="space-y-6 animate-fade-in">
      {/* Header */}
      <div className="flex items-start justify-between">
        <div>
          <h1 className="text-3xl font-bold text-dark-text">Settings</h1>
          <p className="mt-2 text-dark-text-secondary">
            Configure your application
          </p>
        </div>
      </div>

      {/* Tab Navigation */}
      <div className="card p-2">
        <nav className="flex flex-wrap gap-2">
          {tabs.map((tab) => {
            const Icon = tab.icon;
            const isActive = activeTab === tab.id;
            return (
              <button
                key={tab.id}
                type="button"
                onClick={() => setActiveTab(tab.id)}
                className={`flex items-center gap-2 px-4 py-2 rounded-lg transition-all duration-200 ${
                  isActive
                    ? 'bg-dark-accent text-dark-bg font-medium'
                    : 'text-dark-text-secondary hover:bg-dark-bg-hover hover:text-dark-text'
                }`}
              >
                <Icon className="h-4 w-4" />
                <span className="text-sm">{tab.label}</span>
              </button>
            );
          })}
        </nav>
      </div>

      {/* Main content area */}
      <form onSubmit={handleSave} className="space-y-6">
            {/* Library Settings */}
            {activeTab === 'library' && (
              <div className="card">
          <div className="px-6 py-5 border-b border-dark-border">
            <h2 className="text-lg font-medium text-dark-text">Library Settings</h2>
            <p className="mt-1 text-sm text-dark-text-secondary">
              Configure your music library location and scanning behavior
            </p>
          </div>
          <div className="px-6 py-5 space-y-6">
            {/* Library Path */}
            <PathInput
              id="library_path"
              label="Library Path"
              value={formData.library_path || ''}
              onChange={(value) => handleChange('library_path', value)}
              type="directory"
              placeholder="/path/to/your/music/library"
              description="The full path to your music library directory"
            />

            {/* File System Watching */}
            <div className="flex items-start gap-3">
              <input
                id="library_watch"
                type="checkbox"
                checked={formData.library_watch ?? false}
                onChange={(e) => handleChange('library_watch', e.target.checked)}
                className="mt-0.5 h-4 w-4 rounded border-dark-border bg-dark-bg-subtle text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg-elevated"
              />
              <div>
                <label htmlFor="library_watch" className="text-sm font-medium text-dark-text">
                  Enable File System Watching
                </label>
                <p className="text-sm text-dark-text-secondary mt-0.5">
                  Automatically detect changes to your library in real-time
                </p>
              </div>
            </div>

            {/* Auto Scan */}
            <div className="flex items-start gap-3">
              <input
                  id="library_auto_scan"
                  type="checkbox"
                  checked={formData.library_auto_scan ?? false}
                  onChange={(e) => handleChange('library_auto_scan', e.target.checked)}
                  className="mt-0.5 h-4 w-4 rounded border-dark-border bg-dark-bg-subtle text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg-elevated"
                />
              <div>
                <label htmlFor="library_auto_scan" className="text-sm font-medium text-dark-text">
                  Enable Automatic Periodic Scanning
                </label>
                <p className="text-sm text-dark-text-secondary">
                  Periodically scan your library for changes
                </p>
              </div>
            </div>

            {/* Auto Scan Interval */}
            <div>
              <label htmlFor="library_auto_scan_interval_mins" className="block text-sm font-medium text-dark-text mb-2">
                Auto Scan Interval (minutes)
              </label>
              <input
                type="number"
                id="library_auto_scan_interval_mins"
                value={formData.library_auto_scan_interval_mins ?? 60}
                onChange={(e) => handleChange('library_auto_scan_interval_mins', parseInt(e.target.value))}
                min="1"
                className="input w-48"
              />
              <p className="mt-2 text-sm text-dark-text-secondary">
                How often to scan the library automatically
              </p>
            </div>

            {/* Auto Scan on Startup */}
            <div className="flex items-start gap-3">
              <input
                  id="library_auto_scan_on_startup"
                  type="checkbox"
                  checked={formData.library_auto_scan_on_startup ?? false}
                  onChange={(e) => handleChange('library_auto_scan_on_startup', e.target.checked)}
                  className="mt-0.5 h-4 w-4 rounded border-dark-border bg-dark-bg-subtle text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg-elevated"
                />
              <div>
                <label htmlFor="library_auto_scan_on_startup" className="text-sm font-medium text-dark-text">
                  Scan Library on Startup
                </label>
                <p className="text-sm text-dark-text-secondary">
                  Run a full library scan when the application starts
                </p>
              </div>
            </div>

            {/* Normalize Featuring */}
            <div className="flex items-start gap-3">
              <input
                  id="library_normalize_featuring"
                  type="checkbox"
                  checked={formData.library_normalize_featuring ?? false}
                  onChange={(e) => handleChange('library_normalize_featuring', e.target.checked)}
                  className="mt-0.5 h-4 w-4 rounded border-dark-border bg-dark-bg-subtle text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg-elevated"
                />
              <div>
                <label htmlFor="library_normalize_featuring" className="text-sm font-medium text-dark-text">
                  Normalize Featuring Artists
                </label>
                <p className="text-sm text-dark-text-secondary">
                  Standardize "featuring" join phrases in artist credits
                </p>
              </div>
            </div>

            {/* Normalize Featuring To */}
            {formData.library_normalize_featuring && (
              <div>
                <label htmlFor="library_normalize_featuring_to" className="block text-sm font-medium text-dark-text mb-2">
                  Normalize Featuring To
                </label>
                <input
                  type="text"
                  id="library_normalize_featuring_to"
                  value={formData.library_normalize_featuring_to || 'feat.'}
                  onChange={(e) => handleChange('library_normalize_featuring_to', e.target.value)}
                  className="input w-48"
                  placeholder="feat."
                />
                <p className="mt-2 text-sm text-dark-text-secondary">
                  What to normalize "featuring" join phrases to (e.g., "feat.", "ft.")
                </p>
              </div>
            )}

            {/* Path Format */}
            <div>
              <div className="flex items-center gap-2 mb-2">
                <label htmlFor="library_path_format" className="text-sm font-medium text-dark-text">
                  Library Path Format
                </label>
                <button
                  type="button"
                  title="Variables: {album_artist}, {album}, {year}, {country}, {label}, {catalog_number}, {total_discs}, {total_tracks}&#10;Functions: {lower:variable}, {upper:variable}, {capitalize:variable}, {trim:variable}&#10;Conditionals: {if:multidisc|then|else}, {if:lossless|then|else}&#10;&#10;All text variables are automatically sanitized for filesystem safety"
                  className="text-dark-text-secondary hover:text-dark-text transition-colors"
                >
                  <HelpCircle className="w-4 h-4" />
                </button>
              </div>
              <input
                type="text"
                id="library_path_format"
                value={formData.library_path_format || '{album_artist}/{album} [{year}]'}
                onChange={(e) => handleChange('library_path_format', e.target.value)}
                className="input w-full font-mono"
                placeholder="{album_artist}/{album} [{year}]"
              />
              <p className="mt-2 text-sm text-dark-text-secondary">
                Template for organizing album directories
              </p>
            </div>

            {/* File Format */}
            <div>
              <div className="flex items-center gap-2 mb-2">
                <label htmlFor="library_file_format" className="text-sm font-medium text-dark-text">
                  Library File Format
                </label>
                <button
                  type="button"
                  title="Variables: {track:02} (padded), {disc:02}, {artist}, {title}, {album}, {year}, {format} (FLAC/MP3), {bitrate}, {sample_rate}, {bit_depth}, {ext}&#10;Functions: {lower:variable}, {upper:variable}, {capitalize:variable}, {trim:variable}&#10;Conditionals: {if:multidisc|then|else}, {if:lossless|then|else}&#10;&#10;All text variables are automatically sanitized for filesystem safety"
                  className="text-dark-text-secondary hover:text-dark-text transition-colors"
                >
                  <HelpCircle className="w-4 h-4" />
                </button>
              </div>
              <input
                type="text"
                id="library_file_format"
                value={formData.library_file_format || '{track:02} {artist} - {album} {year} - {title}.{ext}'}
                onChange={(e) => handleChange('library_file_format', e.target.value)}
                className="input w-full font-mono"
                placeholder="{track:02} {artist} - {album} {year} - {title}.{ext}"
              />
              <p className="mt-2 text-sm text-dark-text-secondary">
                Template for naming track files
              </p>
            </div>
          </div>
        </div>
            )}

            {/* System Settings */}
            {activeTab === 'system' && (
        <div className="card">
          <div className="px-6 py-5 border-b border-dark-border">
            <h2 className="text-lg font-medium text-dark-text">System Settings</h2>
            <p className="mt-1 text-sm text-dark-text-secondary">
              Configure system-level behavior and database settings
            </p>
          </div>
          <div className="px-6 py-5 space-y-6">
            {/* Watch Config File */}
            <div className="flex items-start gap-3">
              <input
                  id="system_watch_config_file"
                  type="checkbox"
                  checked={formData.system_watch_config_file ?? false}
                  onChange={(e) => handleChange('system_watch_config_file', e.target.checked)}
                  className="mt-0.5 h-4 w-4 rounded border-dark-border bg-dark-bg-subtle text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg-elevated"
                />
              <div>
                <label htmlFor="system_watch_config_file" className="text-sm font-medium text-dark-text">
                  Watch Config File for Changes
                </label>
                <p className="text-sm text-dark-text-secondary">
                  Automatically reload configuration when the config file changes
                </p>
              </div>
            </div>

            {/* Database Backend */}
            <div>
              <label htmlFor="system_database_backend" className="block text-sm font-medium text-dark-text mb-2">
                Database Backend
              </label>
              <select
                id="system_database_backend"
                value={formData.system_database_backend || 'sqlite'}
                onChange={(e) => handleChange('system_database_backend', e.target.value)}
                className="input w-64"
              >
                <option value="sqlite">SQLite</option>
                <option value="postgresql">PostgreSQL</option>
              </select>
              <p className="mt-2 text-sm text-dark-text-secondary">
                Database backend to use for storing metadata
              </p>
            </div>

            {/* Database Path */}
            <PathInput
              id="system_database_path"
              label="Database Path / Connection String"
              value={formData.system_database_path || ''}
              onChange={(value) => handleChange('system_database_path', value)}
              type={formData.system_database_backend === 'postgresql' ? 'file' : 'file'}
              placeholder={formData.system_database_backend === 'postgresql' ? 'host=localhost port=5432 dbname=skema' : './skema.db'}
              description={
                formData.system_database_backend === 'postgresql'
                  ? 'PostgreSQL connection string (e.g., host=localhost port=5432 dbname=skema)'
                  : 'Path to SQLite database file'
              }
            />
          </div>
        </div>
            )}

            {/* Server Settings */}
            {activeTab === 'server' && (
        <div className="card">
          <div className="px-6 py-5 border-b border-dark-border">
            <h2 className="text-lg font-medium text-dark-text">Server Settings</h2>
            <p className="mt-1 text-sm text-dark-text-secondary">
              Configure server network and authentication settings
            </p>
          </div>
          <div className="px-6 py-5 space-y-6">
            {/* Server Host */}
            <div>
              <label htmlFor="server_host" className="block text-sm font-medium text-dark-text mb-2">
                Server Host
              </label>
              <input
                type="text"
                id="server_host"
                value={formData.server_host || ''}
                onChange={(e) => handleChange('server_host', e.target.value)}
                className="input w-64"
                placeholder="127.0.0.1"
              />
              <p className="mt-2 text-sm text-dark-text-secondary">
                Host address to bind to (use 0.0.0.0 to listen on all interfaces)
              </p>
            </div>

            {/* Server Port */}
            <div>
              <label htmlFor="server_port" className="block text-sm font-medium text-dark-text mb-2">
                Server Port
              </label>
              <input
                type="number"
                id="server_port"
                value={formData.server_port ?? 8181}
                onChange={(e) => handleChange('server_port', parseInt(e.target.value))}
                min="1"
                max="65535"
                className="input w-48"
              />
              <p className="mt-2 text-sm text-dark-text-secondary">
                HTTP API server port
              </p>
            </div>

            {/* JWT Expiration */}
            <div>
              <label htmlFor="server_jwt_expiration_hours" className="block text-sm font-medium text-dark-text mb-2">
                JWT Token Expiration (hours)
              </label>
              <input
                type="number"
                id="server_jwt_expiration_hours"
                value={formData.server_jwt_expiration_hours ?? 24}
                onChange={(e) => handleChange('server_jwt_expiration_hours', parseInt(e.target.value))}
                min="1"
                className="input w-48"
              />
              <p className="mt-2 text-sm text-dark-text-secondary">
                How long JWT tokens remain valid
              </p>
            </div>

            {/* Server Username */}
            <div>
              <label htmlFor="server_username" className="block text-sm font-medium text-dark-text mb-2">
                Username (Optional)
              </label>
              <input
                type="text"
                id="server_username"
                value={formData.server_username || ''}
                onChange={(e) => handleChange('server_username', e.target.value || null)}
                className="input w-64"
                placeholder="username"
              />
              <p className="mt-2 text-sm text-dark-text-secondary">
                Username for API authentication. Leave empty to disable authentication.
              </p>
            </div>

            {/* Server Password */}
            <div>
              <label htmlFor="server_password" className="block text-sm font-medium text-dark-text mb-2">
                Password (Optional)
              </label>
              <input
                type="password"
                id="server_password"
                value={serverPassword}
                onChange={(e) => handlePasswordChange(e.target.value)}
                className="input w-64"
                placeholder={
                  config?.server_auth_enabled
                    ? 'Password is set (enter new password to change)'
                    : 'Enter password to enable authentication'
                }
              />
              <p className="mt-2 text-sm text-dark-text-secondary">
                Password for API authentication. Will be securely hashed when saved. {passwordChanged && '(Password changes require clicking Save Changes button)'}
              </p>
            </div>

            {/* Authentication Status */}
            <div className="rounded-lg bg-dark-info-muted border border-dark-info/30 p-4">
              <div className="flex gap-3">
                <svg className="h-5 w-5 text-dark-info flex-shrink-0 mt-0.5" viewBox="0 0 20 20" fill="currentColor">
                  <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clipRule="evenodd" />
                </svg>
                <div className="flex-1">
                  <p className="text-sm text-dark-text">
                    <strong>Current Status:</strong> {config?.server_auth_enabled ? 'Authentication Enabled' : 'Authentication Disabled'}
                    {config?.server_username && ` (User: ${config.server_username})`}
                  </p>
                  <p className="mt-2 text-sm text-dark-text-secondary">
                    <strong>Note:</strong> Environment variables SKEMA_USERNAME and SKEMA_PASSWORD will override config file values if set.
                    The username shown above reflects the actual effective value (including any env var override).
                  </p>
                  {config?.server_auth_enabled && (
                    <p className="mt-1 text-xs text-dark-text-tertiary">
                      To fully disable authentication, ensure both config file values and environment variables are unset.
                    </p>
                  )}
                </div>
              </div>
            </div>
          </div>
        </div>
            )}

            {/* Download Settings */}
            {activeTab === 'download' && (
              <div className="space-y-6">
                {/* General Download Settings */}
                <div className="card">
                  <div className="px-6 py-5 border-b border-dark-border">
                    <h2 className="text-lg font-medium text-dark-text">Download Settings</h2>
                    <p className="mt-1 text-sm text-dark-text-secondary">
                      Configure download behavior and directory
                    </p>
                  </div>
                  <div className="px-6 py-5 space-y-6">
                    {/* Download Directory */}
                    <PathInput
                      id="download_directory"
                      label="Download Directory"
                      value={formData.download_directory || ''}
                      onChange={(value) => handleChange('download_directory', value)}
                      type="directory"
                      placeholder="/path/to/downloads"
                      description="Directory where completed downloads are stored before import"
                    />

                    {/* Check Interval */}
                    <div>
                      <label htmlFor="download_check_interval" className="block text-sm font-medium text-dark-text mb-2">
                        Check Interval (seconds)
                      </label>
                      <input
                        type="number"
                        id="download_check_interval"
                        value={formData.download_check_interval ?? 60}
                        onChange={(e) => handleChange('download_check_interval', parseInt(e.target.value))}
                        min="10"
                        className="input w-48"
                      />
                      <p className="mt-2 text-sm text-dark-text-secondary">
                        How often to check download clients for status updates
                      </p>
                    </div>

                    {/* Auto Import */}
                    <div className="flex items-start gap-3">
                      <input
                        id="download_auto_import"
                        type="checkbox"
                        checked={formData.download_auto_import ?? false}
                        onChange={(e) => handleChange('download_auto_import', e.target.checked)}
                        className="mt-0.5 h-4 w-4 rounded border-dark-border bg-dark-bg-subtle text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg-elevated"
                      />
                      <div>
                        <label htmlFor="download_auto_import" className="text-sm font-medium text-dark-text">
                          Auto Import Completed Downloads
                        </label>
                        <p className="text-sm text-dark-text-secondary mt-0.5">
                          Automatically import completed downloads to your library
                        </p>
                      </div>
                    </div>

                    {/* Min Seeders */}
                    <div>
                      <label htmlFor="download_min_seeders" className="block text-sm font-medium text-dark-text mb-2">
                        Minimum Seeders (Optional)
                      </label>
                      <input
                        type="number"
                        id="download_min_seeders"
                        value={formData.download_min_seeders ?? ''}
                        onChange={(e) => handleChange('download_min_seeders', e.target.value ? parseInt(e.target.value) : null)}
                        min="0"
                        className="input w-48"
                        placeholder="No minimum"
                      />
                      <p className="mt-2 text-sm text-dark-text-secondary">
                        Minimum number of seeders required for torrent downloads
                      </p>
                    </div>

                    {/* Max Size */}
                    <div>
                      <label htmlFor="download_max_size_mb" className="block text-sm font-medium text-dark-text mb-2">
                        Maximum Size (MB, Optional)
                      </label>
                      <input
                        type="number"
                        id="download_max_size_mb"
                        value={formData.download_max_size_mb ?? ''}
                        onChange={(e) => handleChange('download_max_size_mb', e.target.value ? parseInt(e.target.value) : null)}
                        min="1"
                        className="input w-48"
                        placeholder="No maximum"
                      />
                      <p className="mt-2 text-sm text-dark-text-secondary">
                        Maximum download size to accept
                      </p>
                    </div>
                  </div>
                </div>

                {/* NZB Client */}
                <div className="card">
                  <div className="px-6 py-5 border-b border-dark-border flex items-center justify-between">
                    <div>
                      <h2 className="text-lg font-medium text-dark-text">NZB Client</h2>
                      <p className="mt-1 text-sm text-dark-text-secondary">
                        Configure your NZB download client (SABnzbd or NZBGet)
                      </p>
                    </div>
                    <button
                      type="button"
                      onClick={() => editDownloadClient('nzb')}
                      className="btn-primary text-sm"
                    >
                      {formData.download_nzb_client ? (
                        <>
                          <Edit2 className="w-4 h-4 mr-1" />
                          Edit
                        </>
                      ) : (
                        <>
                          <Plus className="w-4 h-4 mr-1" />
                          Configure
                        </>
                      )}
                    </button>
                  </div>
                  <div className="px-6 py-5">
                    {formData.download_nzb_client ? (
                      <div className="border border-dark-border rounded-lg p-4">
                        <div className="flex items-start justify-between">
                          <div className="flex-1">
                            <div className="flex items-center gap-2 mb-2">
                              <h3 className="text-sm font-medium text-dark-text">{getClientTypeName(formData.download_nzb_client.type)}</h3>
                              <span className="inline-flex rounded-full px-2 py-0.5 text-xs font-semibold border bg-dark-info/10 text-dark-info border-dark-info/30">
                                NZB
                              </span>
                              <span className={`inline-flex rounded-full px-2 py-0.5 text-xs font-semibold border ${
                                formData.download_nzb_client.enabled
                                  ? 'bg-dark-success-muted text-dark-success border-dark-success/30'
                                  : 'bg-dark-bg-subtle text-dark-text-tertiary border-dark-border'
                              }`}>
                                {formData.download_nzb_client.enabled ? 'Enabled' : 'Disabled'}
                              </span>
                            </div>
                            <p className="text-sm text-dark-text-secondary">{formData.download_nzb_client.type}</p>
                            <p className="text-sm text-dark-text-secondary mt-1">{formData.download_nzb_client.url}</p>
                          </div>
                          <div className="flex items-center gap-2 ml-4">
                            <button
                              type="button"
                              onClick={() => editDownloadClient('nzb')}
                              className="p-2 text-dark-text-tertiary hover:text-dark-accent transition-colors"
                              title="Edit client"
                            >
                              <Edit2 className="w-4 h-4" />
                            </button>
                            <button
                              type="button"
                              onClick={() => {
                                if (confirm(`Delete NZB client (${formData.download_nzb_client ? getClientTypeName(formData.download_nzb_client.type) : 'unknown'})?`)) {
                                  deleteDownloadClient('nzb');
                                }
                              }}
                              className="p-2 text-dark-text-tertiary hover:text-dark-error transition-colors"
                              title="Delete client"
                            >
                              <Trash2 className="w-4 h-4" />
                            </button>
                          </div>
                        </div>
                      </div>
                    ) : (
                      <div className="text-center py-8">
                        <Download className="mx-auto h-12 w-12 text-dark-text-tertiary" />
                        <p className="mt-2 text-sm text-dark-text-secondary">No NZB client configured</p>
                        <button
                          type="button"
                          onClick={() => editDownloadClient('nzb')}
                          className="btn-secondary mt-4"
                        >
                          <Plus className="w-4 h-4 mr-2" />
                          Configure NZB Client
                        </button>
                      </div>
                    )}
                  </div>
                </div>

                {/* Torrent Client */}
                <div className="card">
                  <div className="px-6 py-5 border-b border-dark-border flex items-center justify-between">
                    <div>
                      <h2 className="text-lg font-medium text-dark-text">Torrent Client</h2>
                      <p className="mt-1 text-sm text-dark-text-secondary">
                        Configure your torrent download client (Transmission or qBittorrent)
                      </p>
                    </div>
                    <button
                      type="button"
                      onClick={() => editDownloadClient('torrent')}
                      className="btn-primary text-sm"
                    >
                      {formData.download_torrent_client ? (
                        <>
                          <Edit2 className="w-4 h-4 mr-1" />
                          Edit
                        </>
                      ) : (
                        <>
                          <Plus className="w-4 h-4 mr-1" />
                          Configure
                        </>
                      )}
                    </button>
                  </div>
                  <div className="px-6 py-5">
                    {formData.download_torrent_client ? (
                      <div className="border border-dark-border rounded-lg p-4">
                        <div className="flex items-start justify-between">
                          <div className="flex-1">
                            <div className="flex items-center gap-2 mb-2">
                              <h3 className="text-sm font-medium text-dark-text">{getClientTypeName(formData.download_torrent_client.type)}</h3>
                              <span className="inline-flex rounded-full px-2 py-0.5 text-xs font-semibold border bg-dark-accent/10 text-dark-accent border-dark-accent/30">
                                Torrent
                              </span>
                              <span className={`inline-flex rounded-full px-2 py-0.5 text-xs font-semibold border ${
                                formData.download_torrent_client.enabled
                                  ? 'bg-dark-success-muted text-dark-success border-dark-success/30'
                                  : 'bg-dark-bg-subtle text-dark-text-tertiary border-dark-border'
                              }`}>
                                {formData.download_torrent_client.enabled ? 'Enabled' : 'Disabled'}
                              </span>
                            </div>
                            <p className="text-sm text-dark-text-secondary">{formData.download_torrent_client.type}</p>
                            <p className="text-sm text-dark-text-secondary mt-1">{formData.download_torrent_client.url}</p>
                          </div>
                          <div className="flex items-center gap-2 ml-4">
                            <button
                              type="button"
                              onClick={() => editDownloadClient('torrent')}
                              className="p-2 text-dark-text-tertiary hover:text-dark-accent transition-colors"
                              title="Edit client"
                            >
                              <Edit2 className="w-4 h-4" />
                            </button>
                            <button
                              type="button"
                              onClick={() => {
                                if (confirm(`Delete torrent client (${formData.download_torrent_client ? getClientTypeName(formData.download_torrent_client.type) : 'unknown'})?`)) {
                                  deleteDownloadClient('torrent');
                                }
                              }}
                              className="p-2 text-dark-text-tertiary hover:text-dark-error transition-colors"
                              title="Delete client"
                            >
                              <Trash2 className="w-4 h-4" />
                            </button>
                          </div>
                        </div>
                      </div>
                    ) : (
                      <div className="text-center py-8">
                        <Download className="mx-auto h-12 w-12 text-dark-text-tertiary" />
                        <p className="mt-2 text-sm text-dark-text-secondary">No torrent client configured</p>
                        <button
                          type="button"
                          onClick={() => editDownloadClient('torrent')}
                          className="btn-secondary mt-4"
                        >
                          <Plus className="w-4 h-4 mr-2" />
                          Configure Torrent Client
                        </button>
                      </div>
                    )}
                  </div>
                </div>
              </div>
            )}

            {/* Indexer Settings */}
            {activeTab === 'indexers' && (
              <div className="space-y-6">
                {/* General Indexer Settings */}
                <div className="card">
                  <div className="px-6 py-5 border-b border-dark-border">
                    <h2 className="text-lg font-medium text-dark-text">Indexer Settings</h2>
                    <p className="mt-1 text-sm text-dark-text-secondary">
                      Configure global indexer behavior
                    </p>
                  </div>
                  <div className="px-6 py-5 space-y-6">
                    {/* Search Timeout */}
                    <div>
                      <label htmlFor="indexers_search_timeout" className="block text-sm font-medium text-dark-text mb-2">
                        Search Timeout (seconds)
                      </label>
                      <input
                        type="number"
                        id="indexers_search_timeout"
                        value={formData.indexers_search_timeout ?? 30}
                        onChange={(e) => handleChange('indexers_search_timeout', parseInt(e.target.value))}
                        min="5"
                        className="input w-48"
                      />
                      <p className="mt-2 text-sm text-dark-text-secondary">
                        Maximum time to wait for indexer search responses
                      </p>
                    </div>
                  </div>
                </div>

                {/* Indexer List */}
                <div className="card">
                  <div className="px-6 py-5 border-b border-dark-border flex items-center justify-between">
                    <div>
                      <h2 className="text-lg font-medium text-dark-text">Configured Indexers</h2>
                      <p className="mt-1 text-sm text-dark-text-secondary">
                        Newznab and Torznab indexers for searching releases
                      </p>
                    </div>
                    <button
                      type="button"
                      onClick={addIndexer}
                      className="btn-primary text-sm"
                    >
                      <Plus className="w-4 h-4 mr-1" />
                      Add Indexer
                    </button>
                  </div>
                  <div className="px-6 py-5">
                    {formData.indexers_list && formData.indexers_list.length > 0 ? (
                      <div className="space-y-3">
                        {formData.indexers_list.map((indexer, idx) => (
                          <div key={idx} className="border border-dark-border rounded-lg p-4 hover:border-dark-border-bright transition-colors">
                            <div className="flex items-start justify-between">
                              <div className="flex-1">
                                <div className="flex items-center gap-3 mb-2">
                                  <h3 className="text-sm font-medium text-dark-text">{indexer.name || 'Unnamed Indexer'}</h3>
                                  <span className={`inline-flex rounded-full px-2 py-0.5 text-xs font-semibold border ${
                                    indexer.enabled
                                      ? 'bg-dark-success-muted text-dark-success border-dark-success/30'
                                      : 'bg-dark-bg-subtle text-dark-text-tertiary border-dark-border'
                                  }`}>
                                    {indexer.enabled ? 'Enabled' : 'Disabled'}
                                  </span>
                                </div>
                                <p className="text-sm text-dark-text-secondary mb-2">{indexer.url}</p>
                                <div className="flex gap-4 text-xs text-dark-text-secondary">
                                  {indexer.categories && indexer.categories.length > 0 && (
                                    <span>Categories: {indexer.categories.join(', ')}</span>
                                  )}
                                  <span>Priority: {indexer.priority}</span>
                                </div>
                              </div>
                              <div className="flex items-center gap-2 ml-4">
                                <button
                                  type="button"
                                  onClick={() => editIndexer(idx)}
                                  className="p-2 text-dark-text-tertiary hover:text-dark-accent transition-colors"
                                  title="Edit indexer"
                                >
                                  <Edit2 className="w-4 h-4" />
                                </button>
                                <button
                                  type="button"
                                  onClick={() => {
                                    if (confirm(`Delete indexer "${indexer.name}"?`)) {
                                      deleteIndexer(idx);
                                    }
                                  }}
                                  className="p-2 text-dark-text-tertiary hover:text-dark-error transition-colors"
                                  title="Delete indexer"
                                >
                                  <Trash2 className="w-4 h-4" />
                                </button>
                              </div>
                            </div>
                          </div>
                        ))}
                      </div>
                    ) : (
                      <div className="text-center py-8">
                        <Search className="mx-auto h-12 w-12 text-dark-text-tertiary" />
                        <p className="mt-2 text-sm text-dark-text-secondary">No indexers configured</p>
                        <button
                          type="button"
                          onClick={addIndexer}
                          className="btn-secondary mt-4"
                        >
                          <Plus className="w-4 h-4 mr-2" />
                          Add Your First Indexer
                        </button>
                      </div>
                    )}
                  </div>
                </div>
              </div>
            )}

            {/* MusicBrainz Settings */}
            {activeTab === 'musicbrainz' && (
              <div className="card">
                <div className="px-6 py-5 border-b border-dark-border">
                  <h2 className="text-lg font-medium text-dark-text">MusicBrainz Settings</h2>
                  <p className="mt-1 text-sm text-dark-text-secondary">
                    Configure MusicBrainz server and authentication
                  </p>
                </div>
                <div className="px-6 py-5 space-y-6">
                  {/* MusicBrainz Server */}
                  <div>
                    <label htmlFor="musicbrainz_server" className="block text-sm font-medium text-dark-text mb-2">
                      MusicBrainz Server
                    </label>
                    <select
                      id="musicbrainz_server"
                      value={formData.musicbrainz_server || 'official'}
                      onChange={(e) => handleChange('musicbrainz_server', e.target.value)}
                      className="input w-64"
                    >
                      <option value="official">Official MusicBrainz</option>
                      <option value="headphones_vip">Headphones VIP Mirror</option>
                    </select>
                    <p className="mt-2 text-sm text-dark-text-secondary">
                      Which MusicBrainz server to use for metadata lookups
                    </p>
                  </div>

                  {/* Username */}
                  <div>
                    <label htmlFor="musicbrainz_username" className="block text-sm font-medium text-dark-text mb-2">
                      Username (Optional)
                    </label>
                    <input
                      type="text"
                      id="musicbrainz_username"
                      value={formData.musicbrainz_username || ''}
                      onChange={(e) => handleChange('musicbrainz_username', e.target.value)}
                      className="input w-64"
                      placeholder="username"
                    />
                    <p className="mt-2 text-sm text-dark-text-secondary">
                      MusicBrainz username for authenticated requests
                    </p>
                  </div>

                  {/* Password */}
                  <div>
                    <label htmlFor="musicbrainz_password" className="block text-sm font-medium text-dark-text mb-2">
                      Password (Optional)
                    </label>
                    <input
                      type="password"
                      id="musicbrainz_password"
                      value={formData.musicbrainz_password || ''}
                      onChange={(e) => handleChange('musicbrainz_password', e.target.value)}
                      className="input w-64"
                      placeholder="••••••••"
                    />
                    <p className="mt-2 text-sm text-dark-text-secondary">
                      MusicBrainz password for authenticated requests
                    </p>
                  </div>
                </div>
              </div>
            )}

        {/* Form actions */}
        <div className="flex justify-end pt-4">
          <button
            type="submit"
            disabled={saving}
            className="btn-primary disabled:opacity-50 disabled:cursor-not-allowed flex items-center gap-2"
          >
            <Save className="w-4 h-4" />
            {saving ? 'Saving...' : 'Save Changes'}
          </button>
        </div>
      </form>

      {/* Download Client Edit Modal */}
      {showClientForm && editingClient && editingClientType && (
        <div className="fixed inset-0 bg-black/50 flex items-center justify-center p-4 z-50">
          <div className="bg-dark-bg-elevated rounded-lg shadow-xl max-w-2xl w-full max-h-[90vh] overflow-y-auto border border-dark-border">
            <div className="px-6 py-4 border-b border-dark-border flex items-center justify-between">
              <h3 className="text-lg font-medium text-dark-text">
                {editingClientType === 'nzb' ? 'Configure NZB Client' : 'Configure Torrent Client'}
              </h3>
              <button
                type="button"
                onClick={() => {
                  setShowClientForm(false);
                  setEditingClient(null);
                  setEditingClientType(null);
                }}
                className="text-dark-text-tertiary hover:text-dark-text-secondary"
              >
                <X className="w-5 h-5" />
              </button>
            </div>
            <div className="px-6 py-4 space-y-4">
              {/* Type */}
              <div>
                <label className="block text-sm font-medium text-dark-text mb-2">Client Type</label>
                <select
                  value={editingClient.type}
                  onChange={(e) => updateEditingClient('type', e.target.value as DownloadClientType)}
                  className="input w-full"
                >
                  {editingClientType === 'nzb' ? (
                    <>
                      <option value="sabnzbd">SABnzbd</option>
                      <option value="nzbget">NZBGet</option>
                    </>
                  ) : (
                    <>
                      <option value="transmission">Transmission</option>
                      <option value="qbittorrent">qBittorrent</option>
                    </>
                  )}
                </select>
              </div>

              {/* URL */}
              <div>
                <label className="block text-sm font-medium text-dark-text mb-2">URL</label>
                <input
                  type="text"
                  value={editingClient.url}
                  onChange={(e) => updateEditingClient('url', e.target.value)}
                  className="input w-full"
                  placeholder="http://localhost:8080"
                />
              </div>

              {/* API Key (for SABnzbd, NZBGet) */}
              {(editingClient.type === 'sabnzbd' || editingClient.type === 'nzbget') && (
                <div>
                  <label className="block text-sm font-medium text-dark-text mb-2">API Key</label>
                  <input
                    type="text"
                    value={editingClient.api_key || ''}
                    onChange={(e) => updateEditingClient('api_key', e.target.value || null)}
                    className="input w-full"
                    placeholder="API Key"
                  />
                </div>
              )}

              {/* Username (for Transmission, qBittorrent) */}
              {(editingClient.type === 'transmission' || editingClient.type === 'qbittorrent') && (
                <>
                  <div>
                    <label className="block text-sm font-medium text-dark-text mb-2">Username</label>
                    <input
                      type="text"
                      value={editingClient.username || ''}
                      onChange={(e) => updateEditingClient('username', e.target.value || null)}
                      className="input w-full"
                      placeholder="Username"
                    />
                  </div>
                  <div>
                    <label className="block text-sm font-medium text-dark-text mb-2">Password</label>
                    <input
                      type="password"
                      value={editingClient.password || ''}
                      onChange={(e) => updateEditingClient('password', e.target.value || null)}
                      className="input w-full"
                      placeholder="Password"
                    />
                  </div>
                </>
              )}

              {/* Download Directory (for SABnzbd) */}
              {editingClient.type === 'sabnzbd' && (
                <PathInput
                  label="Download Directory"
                  value={editingClient.download_dir || ''}
                  onChange={(value) => updateEditingClient('download_dir', value || null)}
                  type="directory"
                  placeholder="/path/to/downloads"
                  description="Base directory where SABnzbd stores completed downloads"
                />
              )}

              {/* Category (for SABnzbd) */}
              {editingClient.type === 'sabnzbd' && (
                <div>
                  <label className="block text-sm font-medium text-dark-text mb-2">Category</label>
                  <input
                    type="text"
                    value={editingClient.category || ''}
                    onChange={(e) => updateEditingClient('category', e.target.value || null)}
                    className="input w-full"
                    placeholder="music"
                  />
                  <p className="mt-1 text-sm text-dark-text-secondary">
                    SABnzbd category to use (e.g., "music" or "headphones")
                  </p>
                </div>
              )}

              {/* Enabled */}
              <div className="flex items-start">
                <div className="flex items-center h-5">
                  <input
                    type="checkbox"
                    checked={editingClient.enabled}
                    onChange={(e) => updateEditingClient('enabled', e.target.checked)}
                    className="mt-0.5 h-4 w-4 rounded border-dark-border bg-dark-bg-subtle text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg-elevated"
                  />
                </div>
                <div className="ml-3">
                  <label className="text-sm font-medium text-dark-text">Enabled</label>
                  <p className="text-sm text-dark-text-secondary">
                    Enable this download client
                  </p>
                </div>
              </div>
            </div>
            <div className="px-6 py-4 bg-dark-bg-subtle border-t border-dark-border flex justify-end gap-3">
              <button
                type="button"
                onClick={() => {
                  setShowClientForm(false);
                  setEditingClient(null);
                  setEditingClientType(null);
                }}
                className="btn-secondary"
              >
                Cancel
              </button>
              <button
                type="button"
                onClick={saveDownloadClient}
                className="btn-primary"
              >
                Save
              </button>
            </div>
          </div>
        </div>
      )}

      {/* Indexer Edit Modal */}
      {showIndexerForm && editingIndexer && (
        <div className="fixed inset-0 bg-black/50 flex items-center justify-center p-4 z-50">
          <div className="bg-dark-bg-elevated rounded-lg shadow-xl max-w-2xl w-full max-h-[90vh] overflow-y-auto border border-dark-border">
            <div className="px-6 py-4 border-b border-dark-border flex items-center justify-between">
              <h3 className="text-lg font-medium text-dark-text">
                {editingIndexer.index === -1 ? 'Add Indexer' : 'Edit Indexer'}
              </h3>
              <button
                type="button"
                onClick={() => {
                  setShowIndexerForm(false);
                  setEditingIndexer(null);
                }}
                className="text-dark-text-tertiary hover:text-dark-text-secondary"
              >
                <X className="w-5 h-5" />
              </button>
            </div>
            <div className="px-6 py-4 space-y-4">
              {/* Name */}
              <div>
                <label className="block text-sm font-medium text-dark-text mb-2">Name</label>
                <input
                  type="text"
                  value={editingIndexer.indexer.name}
                  onChange={(e) => updateEditingIndexer('name', e.target.value)}
                  className="input w-full"
                  placeholder="My Indexer"
                />
              </div>

              {/* URL */}
              <div>
                <label className="block text-sm font-medium text-dark-text mb-2">URL</label>
                <input
                  type="text"
                  value={editingIndexer.indexer.url}
                  onChange={(e) => updateEditingIndexer('url', e.target.value)}
                  className="input w-full"
                  placeholder="https://indexer.example.com"
                />
              </div>

              {/* API Key */}
              <div>
                <label className="block text-sm font-medium text-dark-text mb-2">API Key (Optional)</label>
                <input
                  type="text"
                  value={editingIndexer.indexer.api_key || ''}
                  onChange={(e) => updateEditingIndexer('api_key', e.target.value || null)}
                  className="input w-full"
                  placeholder="API Key"
                />
              </div>

              {/* Username */}
              <div>
                <label className="block text-sm font-medium text-dark-text mb-2">Username (Optional)</label>
                <input
                  type="text"
                  value={editingIndexer.indexer.username || ''}
                  onChange={(e) => updateEditingIndexer('username', e.target.value || null)}
                  className="input w-full"
                  placeholder="Username"
                />
              </div>

              {/* Password */}
              <div>
                <label className="block text-sm font-medium text-dark-text mb-2">Password (Optional)</label>
                <input
                  type="password"
                  value={editingIndexer.indexer.password || ''}
                  onChange={(e) => updateEditingIndexer('password', e.target.value || null)}
                  className="input w-full"
                  placeholder="Password"
                />
              </div>

              {/* Categories */}
              <div>
                <label className="block text-sm font-medium text-dark-text mb-2">Categories</label>
                <input
                  type="text"
                  value={editingIndexer.indexer.categories.join(', ')}
                  onChange={(e) => {
                    const cats = e.target.value.split(',').map(c => parseInt(c.trim())).filter(c => !isNaN(c));
                    updateEditingIndexer('categories', cats);
                  }}
                  className="input w-full"
                  placeholder="3000, 3010"
                />
                <p className="mt-1 text-sm text-dark-text-secondary">Comma-separated Newznab category IDs (e.g., 3000 for Audio, 3010 for MP3)</p>
              </div>

              {/* Priority */}
              <div>
                <label className="block text-sm font-medium text-dark-text mb-2">Priority</label>
                <input
                  type="number"
                  value={editingIndexer.indexer.priority}
                  onChange={(e) => updateEditingIndexer('priority', parseInt(e.target.value))}
                  className="input w-full"
                  placeholder="0"
                />
                <p className="mt-1 text-sm text-dark-text-secondary">Higher priority indexers are searched first</p>
              </div>

              {/* Enabled */}
              <div className="flex items-start">
                <div className="flex items-center h-5">
                  <input
                    type="checkbox"
                    checked={editingIndexer.indexer.enabled}
                    onChange={(e) => updateEditingIndexer('enabled', e.target.checked)}
                    className="mt-0.5 h-4 w-4 rounded border-dark-border bg-dark-bg-subtle text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg-elevated"
                  />
                </div>
                <div className="ml-3">
                  <label className="text-sm font-medium text-dark-text">Enabled</label>
                  <p className="text-sm text-dark-text-secondary">Enable this indexer</p>
                </div>
              </div>
            </div>
            <div className="px-6 py-4 bg-dark-bg-subtle border-t border-dark-border flex justify-end gap-3">
              <button
                type="button"
                onClick={() => {
                  setShowIndexerForm(false);
                  setEditingIndexer(null);
                }}
                className="btn-secondary"
              >
                Cancel
              </button>
              <button
                type="button"
                onClick={saveIndexer}
                className="btn-primary"
              >
                Save
              </button>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}
