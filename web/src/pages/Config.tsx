import { useEffect, useState, useRef, useCallback } from 'react';
import toast from 'react-hot-toast';
import { Save, Library, Settings, Server, Download, Search, Database, Plus, Edit2, Trash2, X, Bell } from 'lucide-react';
import { api } from '../lib/api';
import { PathInput } from '../components/PathInput';
import {
  LibraryConfigSection,
  SystemConfigSection,
  ServerConfigSection,
  MusicbrainzConfigSection,
  MediaConfigSection,
} from '../components/ConfigFields.generated';
import type { Config, DownloadClient, Indexer, DownloadClientType, NotificationProvider } from '../types/api';

type TabId = 'library' | 'system' | 'server' | 'download' | 'indexers' | 'musicbrainz' | 'media' | 'notifications';

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
  { id: 'notifications', label: 'Notifications', icon: Bell },
];

export default function Config() {
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [config, setConfig] = useState<Config | null>(null);
  const [formData, setFormData] = useState<Config | null>(null);
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

  // Notification provider editing
  const [editingProvider, setEditingProvider] = useState<{ index: number; provider: NotificationProvider } | null>(null);
  const [showProviderForm, setShowProviderForm] = useState(false);

  // Show advanced options toggle
  const [showAdvanced, setShowAdvanced] = useState(false);

  // Autosave with debounce
  const autosaveTimerRef = useRef<NodeJS.Timeout | null>(null);

  // Autosave function
  const performSave = useCallback(async (updates: Partial<Config>, includePassword: boolean = false) => {
    // Include password if it was changed
    const updatesWithPassword = includePassword && passwordChanged && serverPassword
      ? { ...updates, server: { ...updates.server, password: serverPassword } }
      : updates;

    if (Object.keys(updatesWithPassword).length === 0) return;

    setSaving(true);
    try {
      const updated = await api.updateConfig(updatesWithPassword);
      setConfig(updated);
      if (includePassword) {
        setServerPassword('');
        setPasswordChanged(false);
      }
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
        if (!error.isAuthError) {
          console.error('[Config] Failed to load config:', error);
          toast.error('Failed to load configuration');
        }
        setLoading(false);
      }
    }
    loadConfig();

    const handleConfigUpdate = ((event: CustomEvent) => {
      const data = event.detail;
      console.log('[Config] Received config update:', data);
      setConfig(data);
      toast.success('Configuration saved');
    }) as EventListener;

    window.addEventListener('config_updated', handleConfigUpdate);
    return () => {
      window.removeEventListener('config_updated', handleConfigUpdate);
      if (autosaveTimerRef.current) {
        clearTimeout(autosaveTimerRef.current);
      }
    };
  }, []);

  async function handleSave(e: React.FormEvent) {
    e.preventDefault();
    if (autosaveTimerRef.current) {
      clearTimeout(autosaveTimerRef.current);
      autosaveTimerRef.current = null;
    }

    if (!formData || !config) return;

    // Calculate diff between formData and config
    const updates: Partial<Config> = {};
    for (const section of Object.keys(formData) as (keyof Config)[]) {
      if (JSON.stringify(formData[section]) !== JSON.stringify(config[section])) {
        (updates as any)[section] = formData[section];
      }
    }

    await performSave(updates, true);
  }

  // Handle nested field changes (section.field)
  function handleChange(section: keyof Config, field: string, value: any) {
    setFormData((prev) => {
      if (!prev) return prev;
      const newFormData = {
        ...prev,
        [section]: {
          ...(prev[section] as any),
          [field]: value,
        },
      };

      // Cancel previous autosave timer
      if (autosaveTimerRef.current) {
        clearTimeout(autosaveTimerRef.current);
      }

      // Schedule autosave after 5 seconds
      autosaveTimerRef.current = setTimeout(() => {
        if (!config) return;
        const updates: Partial<Config> = {};
        for (const sec of Object.keys(newFormData) as (keyof Config)[]) {
          if (JSON.stringify(newFormData[sec]) !== JSON.stringify(config[sec])) {
            (updates as any)[sec] = newFormData[sec];
          }
        }
        performSave(updates, false);
      }, 5000);

      return newFormData;
    });
  }

  function handlePasswordChange(newPassword: string) {
    setServerPassword(newPassword);
    setPasswordChanged(true);
  }

  // Download client management
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
    const client = type === 'nzb' ? formData?.download.nzb_client : formData?.download.torrent_client;

    if (client) {
      setEditingClient({ ...client });
    } else {
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

    const field = editingClientType === 'nzb' ? 'nzb_client' : 'torrent_client';
    handleChange('download', field, editingClient);
    setShowClientForm(false);
    setEditingClient(null);
    setEditingClientType(null);
  }

  function deleteDownloadClient(type: 'nzb' | 'torrent') {
    const field = type === 'nzb' ? 'nzb_client' : 'torrent_client';
    handleChange('download', field, null);
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
      categories: [3000, 3010],
    };
    setEditingIndexer({ index: -1, indexer: newIndexer });
    setShowIndexerForm(true);
  }

  function editIndexer(index: number) {
    const indexers = formData?.indexers.list || [];
    setEditingIndexer({ index, indexer: { ...indexers[index] } });
    setShowIndexerForm(true);
  }

  function saveIndexer() {
    if (!editingIndexer) return;

    const indexers = [...(formData?.indexers.list || [])];
    if (editingIndexer.index === -1) {
      indexers.push(editingIndexer.indexer);
    } else {
      indexers[editingIndexer.index] = editingIndexer.indexer;
    }

    handleChange('indexers', 'list', indexers);
    setShowIndexerForm(false);
    setEditingIndexer(null);
  }

  function deleteIndexer(index: number) {
    const indexers = [...(formData?.indexers.list || [])];
    indexers.splice(index, 1);
    handleChange('indexers', 'list', indexers);
  }

  function updateEditingIndexer(field: keyof Indexer, value: any) {
    if (!editingIndexer) return;
    setEditingIndexer({
      ...editingIndexer,
      indexer: { ...editingIndexer.indexer, [field]: value },
    });
  }

  // Notification provider management
  function addNotificationProvider() {
    const newProvider: NotificationProvider = {
      type: 'pushover',
      user_key: '',
      device: null,
      priority: 0,
    };
    setEditingProvider({ index: -1, provider: newProvider });
    setShowProviderForm(true);
  }

  function editNotificationProvider(index: number) {
    const providers = formData?.notifications.providers || [];
    setEditingProvider({ index, provider: { ...providers[index] } });
    setShowProviderForm(true);
  }

  function saveNotificationProvider() {
    if (!editingProvider) return;

    const providers = [...(formData?.notifications.providers || [])];
    if (editingProvider.index === -1) {
      providers.push(editingProvider.provider);
    } else {
      providers[editingProvider.index] = editingProvider.provider;
    }

    handleChange('notifications', 'providers', providers);
    setShowProviderForm(false);
    setEditingProvider(null);
  }

  function deleteNotificationProvider(index: number) {
    const providers = [...(formData?.notifications.providers || [])];
    providers.splice(index, 1);
    handleChange('notifications', 'providers', providers);
  }

  function updateEditingProvider(field: keyof NotificationProvider, value: any) {
    if (!editingProvider) return;
    setEditingProvider({
      ...editingProvider,
      provider: { ...editingProvider.provider, [field]: value },
    });
  }

  if (loading || !formData) {
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
        <label className="flex items-center gap-3 cursor-pointer">
          <span className="text-sm text-dark-text">Show Advanced</span>
          <div className="relative">
            <input
              type="checkbox"
              checked={showAdvanced}
              onChange={(e) => setShowAdvanced(e.target.checked)}
              className="sr-only peer"
            />
            <div className="w-10 h-6 bg-dark-bg-subtle rounded-full peer peer-checked:bg-dark-accent transition-colors"></div>
            <div className="absolute left-1 top-1 w-4 h-4 bg-dark-text-tertiary rounded-full transition-all peer-checked:translate-x-4 peer-checked:bg-white"></div>
          </div>
        </label>
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
        {/* Library Settings - using generated component */}
        {activeTab === 'library' && (
          <div className="card">
            <div className="px-6 py-5 border-b border-dark-border">
              <h2 className="text-lg font-medium text-dark-text">Library Settings</h2>
              <p className="mt-1 text-sm text-dark-text-secondary">
                Configure your music library location and scanning behavior
              </p>
            </div>
            <div className="px-6 py-5">
              <LibraryConfigSection config={formData} onChange={handleChange} showAdvanced={showAdvanced} />
            </div>
          </div>
        )}

        {/* System Settings - using generated component */}
        {activeTab === 'system' && (
          <div className="card">
            <div className="px-6 py-5 border-b border-dark-border">
              <h2 className="text-lg font-medium text-dark-text">System Settings</h2>
              <p className="mt-1 text-sm text-dark-text-secondary">
                Configure system-level behavior and database settings
              </p>
            </div>
            <div className="px-6 py-5">
              <SystemConfigSection config={formData} onChange={handleChange} showAdvanced={showAdvanced} />
            </div>
          </div>
        )}

        {/* Server Settings - using generated component */}
        {activeTab === 'server' && (
          <div className="card">
            <div className="px-6 py-5 border-b border-dark-border">
              <h2 className="text-lg font-medium text-dark-text">Server Settings</h2>
              <p className="mt-1 text-sm text-dark-text-secondary">
                Configure server network and authentication settings
              </p>
            </div>
            <div className="px-6 py-5 space-y-6">
              <ServerConfigSection config={formData} onChange={handleChange} showAdvanced={showAdvanced} />

              {/* Custom password field with change tracking */}
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
                    config?.server.auth_enabled
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
                      <strong>Current Status:</strong> {config?.server.auth_enabled ? 'Authentication Enabled' : 'Authentication Disabled'}
                      {config?.server.username && ` (User: ${config.server.username})`}
                    </p>
                    <p className="mt-2 text-sm text-dark-text-secondary">
                      <strong>Note:</strong> Environment variables SKEMA_USERNAME and SKEMA_PASSWORD will override config file values if set.
                    </p>
                  </div>
                </div>
              </div>
            </div>
          </div>
        )}

        {/* Download Settings - manual because of complex client forms */}
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
                <PathInput
                  id="download_directory"
                  label="Download Directory"
                  value={formData.download.directory || ''}
                  onChange={(value) => handleChange('download', 'directory', value)}
                  type="directory"
                  placeholder="/path/to/downloads"
                  description="Directory where completed downloads are stored before import"
                />

                <div>
                  <label htmlFor="download_check_interval" className="block text-sm font-medium text-dark-text mb-2">
                    Check Interval (seconds)
                  </label>
                  <input
                    type="number"
                    id="download_check_interval"
                    value={formData.download.check_interval ?? 60}
                    onChange={(e) => handleChange('download', 'check_interval', parseInt(e.target.value))}
                    min="10"
                    className="input w-48"
                  />
                  <p className="mt-2 text-sm text-dark-text-secondary">
                    How often to check download clients for status updates
                  </p>
                </div>

                <div className="flex items-start gap-3">
                  <input
                    id="download_auto_import"
                    type="checkbox"
                    checked={formData.download.auto_import ?? false}
                    onChange={(e) => handleChange('download', 'auto_import', e.target.checked)}
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

                <div>
                  <label htmlFor="download_min_seeders" className="block text-sm font-medium text-dark-text mb-2">
                    Minimum Seeders (Optional)
                  </label>
                  <input
                    type="number"
                    id="download_min_seeders"
                    value={formData.download.min_seeders ?? ''}
                    onChange={(e) => handleChange('download', 'min_seeders', e.target.value ? parseInt(e.target.value) : null)}
                    min="0"
                    className="input w-48"
                    placeholder="No minimum"
                  />
                  <p className="mt-2 text-sm text-dark-text-secondary">
                    Minimum number of seeders required for torrent downloads
                  </p>
                </div>

                <div>
                  <label htmlFor="download_max_size" className="block text-sm font-medium text-dark-text mb-2">
                    Maximum Size (MB, Optional)
                  </label>
                  <input
                    type="number"
                    id="download_max_size"
                    value={formData.download.max_size ?? ''}
                    onChange={(e) => handleChange('download', 'max_size', e.target.value ? parseInt(e.target.value) : null)}
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
                  {formData.download.nzb_client ? (
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
                {formData.download.nzb_client ? (
                  <div className="border border-dark-border rounded-lg p-4">
                    <div className="flex items-start justify-between">
                      <div className="flex-1">
                        <div className="flex items-center gap-2 mb-2">
                          <h3 className="text-sm font-medium text-dark-text">{getClientTypeName(formData.download.nzb_client.type)}</h3>
                          <span className="inline-flex rounded-full px-2 py-0.5 text-xs font-semibold border bg-dark-info/10 text-dark-info border-dark-info/30">
                            NZB
                          </span>
                          <span className={`inline-flex rounded-full px-2 py-0.5 text-xs font-semibold border ${
                            formData.download.nzb_client.enabled
                              ? 'bg-dark-success-muted text-dark-success border-dark-success/30'
                              : 'bg-dark-bg-subtle text-dark-text-tertiary border-dark-border'
                          }`}>
                            {formData.download.nzb_client.enabled ? 'Enabled' : 'Disabled'}
                          </span>
                        </div>
                        <p className="text-sm text-dark-text-secondary mt-1">{formData.download.nzb_client.url}</p>
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
                            if (confirm(`Delete NZB client?`)) {
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
                  {formData.download.torrent_client ? (
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
                {formData.download.torrent_client ? (
                  <div className="border border-dark-border rounded-lg p-4">
                    <div className="flex items-start justify-between">
                      <div className="flex-1">
                        <div className="flex items-center gap-2 mb-2">
                          <h3 className="text-sm font-medium text-dark-text">{getClientTypeName(formData.download.torrent_client.type)}</h3>
                          <span className="inline-flex rounded-full px-2 py-0.5 text-xs font-semibold border bg-dark-accent/10 text-dark-accent border-dark-accent/30">
                            Torrent
                          </span>
                          <span className={`inline-flex rounded-full px-2 py-0.5 text-xs font-semibold border ${
                            formData.download.torrent_client.enabled
                              ? 'bg-dark-success-muted text-dark-success border-dark-success/30'
                              : 'bg-dark-bg-subtle text-dark-text-tertiary border-dark-border'
                          }`}>
                            {formData.download.torrent_client.enabled ? 'Enabled' : 'Disabled'}
                          </span>
                        </div>
                        <p className="text-sm text-dark-text-secondary mt-1">{formData.download.torrent_client.url}</p>
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
                            if (confirm(`Delete torrent client?`)) {
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

        {/* Indexer Settings - manual because of complex list management */}
        {activeTab === 'indexers' && (
          <div className="space-y-6">
            <div className="card">
              <div className="px-6 py-5 border-b border-dark-border">
                <h2 className="text-lg font-medium text-dark-text">Indexer Settings</h2>
                <p className="mt-1 text-sm text-dark-text-secondary">
                  Configure global indexer behavior
                </p>
              </div>
              <div className="px-6 py-5 space-y-6">
                <div>
                  <label htmlFor="indexers_search_timeout" className="block text-sm font-medium text-dark-text mb-2">
                    Search Timeout (seconds)
                  </label>
                  <input
                    type="number"
                    id="indexers_search_timeout"
                    value={formData.indexers.search_timeout ?? 30}
                    onChange={(e) => handleChange('indexers', 'search_timeout', parseInt(e.target.value))}
                    min="5"
                    className="input w-48"
                  />
                  <p className="mt-2 text-sm text-dark-text-secondary">
                    Maximum time to wait for indexer search responses
                  </p>
                </div>
              </div>
            </div>

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
                {formData.indexers.list && formData.indexers.list.length > 0 ? (
                  <div className="space-y-3">
                    {formData.indexers.list.map((indexer, idx) => (
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

        {/* MusicBrainz Settings - using generated component */}
        {activeTab === 'musicbrainz' && (
          <div className="card">
            <div className="px-6 py-5 border-b border-dark-border">
              <h2 className="text-lg font-medium text-dark-text">MusicBrainz Settings</h2>
              <p className="mt-1 text-sm text-dark-text-secondary">
                Configure MusicBrainz server and authentication
              </p>
            </div>
            <div className="px-6 py-5">
              <MusicbrainzConfigSection config={formData} onChange={handleChange} showAdvanced={showAdvanced} />
            </div>
          </div>
        )}

        {/* Notification Settings - manual because of complex provider forms */}
        {activeTab === 'notifications' && (
          <div className="space-y-6">
            <div className="card">
              <div className="px-6 py-5 border-b border-dark-border">
                <h2 className="text-lg font-medium text-dark-text">Notification Settings</h2>
                <p className="mt-1 text-sm text-dark-text-secondary">
                  Get notified about important events
                </p>
              </div>
              <div className="px-6 py-5 space-y-6">
                <div className="flex items-start gap-3">
                  <input
                    id="notification_enabled"
                    type="checkbox"
                    checked={formData.notifications.enabled ?? false}
                    onChange={(e) => handleChange('notifications', 'enabled', e.target.checked)}
                    className="mt-0.5 h-4 w-4 rounded border-dark-border bg-dark-bg-subtle text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg-elevated"
                  />
                  <div>
                    <label htmlFor="notification_enabled" className="text-sm font-medium text-dark-text">
                      Enable Notifications
                    </label>
                    <p className="text-sm text-dark-text-secondary mt-0.5">
                      Send notifications through configured providers
                    </p>
                  </div>
                </div>

                <div className="space-y-4">
                  <h3 className="text-sm font-medium text-dark-text">Notify When:</h3>

                  <div className="flex items-start gap-3">
                    <input
                      id="notification_on_album_found"
                      type="checkbox"
                      checked={formData.notifications.on_album_found ?? true}
                      onChange={(e) => handleChange('notifications', 'on_album_found', e.target.checked)}
                      className="mt-0.5 h-4 w-4 rounded border-dark-border bg-dark-bg-subtle text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg-elevated"
                    />
                    <div>
                      <label htmlFor="notification_on_album_found" className="text-sm font-medium text-dark-text">
                        New Albums Found
                      </label>
                      <p className="text-sm text-dark-text-secondary mt-0.5">
                        When new wanted albums are discovered
                      </p>
                    </div>
                  </div>

                  <div className="flex items-start gap-3">
                    <input
                      id="notification_on_album_imported"
                      type="checkbox"
                      checked={formData.notifications.on_album_imported ?? true}
                      onChange={(e) => handleChange('notifications', 'on_album_imported', e.target.checked)}
                      className="mt-0.5 h-4 w-4 rounded border-dark-border bg-dark-bg-subtle text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg-elevated"
                    />
                    <div>
                      <label htmlFor="notification_on_album_imported" className="text-sm font-medium text-dark-text">
                        Albums Imported
                      </label>
                      <p className="text-sm text-dark-text-secondary mt-0.5">
                        When albums are imported to your library
                      </p>
                    </div>
                  </div>
                </div>
              </div>
            </div>

            <div className="card">
              <div className="px-6 py-5 border-b border-dark-border flex items-center justify-between">
                <div>
                  <h2 className="text-lg font-medium text-dark-text">Notification Providers</h2>
                  <p className="mt-1 text-sm text-dark-text-secondary">
                    Configure services to send notifications to
                  </p>
                </div>
                <button
                  type="button"
                  onClick={addNotificationProvider}
                  className="btn-primary text-sm"
                >
                  <Plus className="w-4 h-4 mr-1" />
                  Add Provider
                </button>
              </div>
              <div className="px-6 py-5">
                {formData.notifications.providers && formData.notifications.providers.length > 0 ? (
                  <div className="space-y-3">
                    {formData.notifications.providers.map((provider, idx) => (
                      <div key={idx} className="border border-dark-border rounded-lg p-4 hover:border-dark-border-bright transition-colors">
                        <div className="flex items-start justify-between">
                          <div className="flex-1">
                            <div className="flex items-center gap-3 mb-2">
                              <h3 className="text-sm font-medium text-dark-text">Pushover</h3>
                              <span className="inline-flex rounded-full px-2 py-0.5 text-xs font-semibold border bg-dark-accent/10 text-dark-accent border-dark-accent/30">
                                {provider.type}
                              </span>
                            </div>
                            <div className="text-xs text-dark-text-secondary space-y-1">
                              <p>User Key: {provider.user_key.substring(0, 8)}...</p>
                              <p>Priority: {provider.priority}</p>
                              {provider.device && <p>Device: {provider.device}</p>}
                            </div>
                          </div>
                          <div className="flex items-center gap-2 ml-4">
                            <button
                              type="button"
                              onClick={() => editNotificationProvider(idx)}
                              className="p-2 text-dark-text-tertiary hover:text-dark-accent transition-colors"
                              title="Edit provider"
                            >
                              <Edit2 className="w-4 h-4" />
                            </button>
                            <button
                              type="button"
                              onClick={() => {
                                if (confirm('Delete this notification provider?')) {
                                  deleteNotificationProvider(idx);
                                }
                              }}
                              className="p-2 text-dark-text-tertiary hover:text-dark-error transition-colors"
                              title="Delete provider"
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
                    <Bell className="mx-auto h-12 w-12 text-dark-text-tertiary" />
                    <p className="mt-2 text-sm text-dark-text-secondary">No notification providers configured</p>
                    <button
                      type="button"
                      onClick={addNotificationProvider}
                      className="btn-secondary mt-4"
                    >
                      <Plus className="w-4 h-4 mr-2" />
                      Add Your First Provider
                    </button>
                  </div>
                )}
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

              {editingClient.type === 'sabnzbd' && (
                <>
                  <PathInput
                    label="Download Directory"
                    value={editingClient.download_dir || ''}
                    onChange={(value) => updateEditingClient('download_dir', value || null)}
                    type="directory"
                    placeholder="/path/to/downloads"
                    description="Base directory where SABnzbd stores completed downloads"
                  />
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
                      SABnzbd category to use
                    </p>
                  </div>
                </>
              )}

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
                <p className="mt-1 text-sm text-dark-text-secondary">Comma-separated Newznab category IDs</p>
              </div>

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

      {/* Notification Provider Edit Modal */}
      {showProviderForm && editingProvider && (
        <div className="fixed inset-0 bg-black/50 flex items-center justify-center p-4 z-50">
          <div className="bg-dark-bg-elevated rounded-lg shadow-xl max-w-2xl w-full max-h-[90vh] overflow-y-auto border border-dark-border">
            <div className="px-6 py-4 border-b border-dark-border flex items-center justify-between">
              <h3 className="text-lg font-medium text-dark-text">
                {editingProvider.index === -1 ? 'Add Notification Provider' : 'Edit Notification Provider'}
              </h3>
              <button
                type="button"
                onClick={() => {
                  setShowProviderForm(false);
                  setEditingProvider(null);
                }}
                className="text-dark-text-tertiary hover:text-dark-text-secondary"
              >
                <X className="w-5 h-5" />
              </button>
            </div>
            <div className="px-6 py-4 space-y-4">
              <div>
                <label className="block text-sm font-medium text-dark-text mb-2">Provider Type</label>
                <select
                  value="pushover"
                  disabled
                  className="input w-full opacity-50 cursor-not-allowed"
                >
                  <option value="pushover">Pushover</option>
                </select>
                <p className="mt-1 text-sm text-dark-text-secondary">Currently only Pushover is supported</p>
              </div>

              <div>
                <label className="block text-sm font-medium text-dark-text mb-2">User Key *</label>
                <input
                  type="text"
                  value={editingProvider.provider.user_key}
                  onChange={(e) => updateEditingProvider('user_key', e.target.value)}
                  className="input w-full"
                  placeholder="Enter your Pushover user key"
                />
                <p className="mt-1 text-sm text-dark-text-secondary">Your Pushover user or group key</p>
              </div>

              <div>
                <label className="block text-sm font-medium text-dark-text mb-2">Device Name (Optional)</label>
                <input
                  type="text"
                  value={editingProvider.provider.device || ''}
                  onChange={(e) => updateEditingProvider('device', e.target.value || null)}
                  className="input w-full"
                  placeholder="Leave empty for all devices"
                />
              </div>

              <div>
                <label className="block text-sm font-medium text-dark-text mb-2">Priority</label>
                <select
                  value={editingProvider.provider.priority}
                  onChange={(e) => updateEditingProvider('priority', parseInt(e.target.value))}
                  className="input w-full"
                >
                  <option value="-2">Lowest (-2)</option>
                  <option value="-1">Low (-1)</option>
                  <option value="0">Normal (0)</option>
                  <option value="1">High (1)</option>
                  <option value="2">Emergency (2)</option>
                </select>
              </div>
            </div>
            <div className="px-6 py-4 bg-dark-bg-subtle border-t border-dark-border flex justify-end gap-3">
              <button
                type="button"
                onClick={() => {
                  setShowProviderForm(false);
                  setEditingProvider(null);
                }}
                className="btn-secondary"
              >
                Cancel
              </button>
              <button
                type="button"
                onClick={saveNotificationProvider}
                className="btn-primary"
                disabled={!editingProvider.provider.user_key}
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
