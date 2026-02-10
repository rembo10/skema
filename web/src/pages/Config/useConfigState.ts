import { useEffect, useState, useRef, useCallback } from 'react';
import toast from 'react-hot-toast';
import { api } from '../../lib/api';
import type { Config, DownloadClient, Indexer, NotificationProvider, SlskdConfig, DownloadClientType } from '../../types/api';

export type TabId = 'library' | 'system' | 'server' | 'download' | 'indexers' | 'musicbrainz' | 'notifications';

export interface ConfigState {
  loading: boolean;
  saving: boolean;
  config: Config | null;
  formData: Config | null;
  activeTab: TabId;
  showAdvanced: boolean;
  // Password state
  serverPassword: string;
  passwordChanged: boolean;
  // Download client editing
  editingClientType: 'nzb' | 'torrent' | null;
  editingClient: DownloadClient | null;
  showClientForm: boolean;
  // slskd client editing
  editingSlskd: SlskdConfig | null;
  showSlskdForm: boolean;
  // Indexer editing
  editingIndexer: { index: number; indexer: Indexer } | null;
  showIndexerForm: boolean;
  // Notification provider editing
  editingProvider: { index: number; provider: NotificationProvider } | null;
  showProviderForm: boolean;
}

export interface ConfigActions {
  setActiveTab: (tab: TabId) => void;
  setShowAdvanced: (show: boolean) => void;
  handleChange: (section: keyof Config, field: string, value: unknown) => void;
  handleSave: (e: React.FormEvent) => Promise<void>;
  // Password
  handlePasswordChange: (password: string) => void;
  // Download client
  editDownloadClient: (type: 'nzb' | 'torrent') => void;
  saveDownloadClient: () => void;
  deleteDownloadClient: (type: 'nzb' | 'torrent') => void;
  updateEditingClient: (field: keyof DownloadClient, value: unknown) => void;
  closeClientForm: () => void;
  // slskd client
  editSlskdClient: () => void;
  saveSlskdClient: () => void;
  deleteSlskdClient: () => void;
  updateEditingSlskd: (field: keyof SlskdConfig, value: unknown) => void;
  closeSlskdForm: () => void;
  // Indexer
  addIndexer: () => void;
  editIndexer: (index: number) => void;
  saveIndexer: () => void;
  deleteIndexer: (index: number) => void;
  updateEditingIndexer: (field: keyof Indexer, value: unknown) => void;
  closeIndexerForm: () => void;
  // Notification provider
  addNotificationProvider: () => void;
  editNotificationProvider: (index: number) => void;
  saveNotificationProvider: () => void;
  deleteNotificationProvider: (index: number) => void;
  updateEditingProvider: (field: keyof NotificationProvider, value: unknown) => void;
  closeProviderForm: () => void;
}

export function useConfigState(): [ConfigState, ConfigActions] {
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [config, setConfig] = useState<Config | null>(null);
  const [formData, setFormData] = useState<Config | null>(null);
  const [activeTab, setActiveTab] = useState<TabId>('library');
  const [showAdvanced, setShowAdvanced] = useState(false);

  // Password is stored separately since it's not returned by the API
  const [serverPassword, setServerPassword] = useState('');
  const [passwordChanged, setPasswordChanged] = useState(false);

  // Download client editing
  const [editingClientType, setEditingClientType] = useState<'nzb' | 'torrent' | null>(null);
  const [editingClient, setEditingClient] = useState<DownloadClient | null>(null);
  const [showClientForm, setShowClientForm] = useState(false);

  // slskd client editing
  const [editingSlskd, setEditingSlskd] = useState<SlskdConfig | null>(null);
  const [showSlskdForm, setShowSlskdForm] = useState(false);

  // Indexer editing
  const [editingIndexer, setEditingIndexer] = useState<{ index: number; indexer: Indexer } | null>(null);
  const [showIndexerForm, setShowIndexerForm] = useState(false);

  // Notification provider editing
  const [editingProvider, setEditingProvider] = useState<{ index: number; provider: NotificationProvider } | null>(null);
  const [showProviderForm, setShowProviderForm] = useState(false);

  // Autosave with debounce
  const autosaveTimerRef = useRef<ReturnType<typeof setTimeout> | null>(null);

  // Autosave function
  const performSave = useCallback(async (updates: Partial<Config>, includePassword: boolean = false) => {
    // Include password if it was changed
    const updatesWithPassword = includePassword && passwordChanged && serverPassword
      ? { ...updates, server: { ...updates.server, password: serverPassword } } as Partial<Config>
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
      } catch (error: unknown) {
        const err = error as { isAuthError?: boolean };
        if (!err.isAuthError) {
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
        (updates as Record<string, unknown>)[section] = formData[section];
      }
    }

    await performSave(updates, true);
  }

  // Handle nested field changes (section.field)
  function handleChange(section: keyof Config, field: string, value: unknown) {
    setFormData((prev) => {
      if (!prev) return prev;
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const sectionValue = prev[section] as any;
      const newFormData = {
        ...prev,
        [section]: {
          ...sectionValue,
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
            (updates as Record<string, unknown>)[sec] = newFormData[sec];
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

  function updateEditingClient(field: keyof DownloadClient, value: unknown) {
    if (!editingClient) return;
    setEditingClient({
      ...editingClient,
      [field]: value,
    });
  }

  function closeClientForm() {
    setShowClientForm(false);
    setEditingClient(null);
    setEditingClientType(null);
  }

  // slskd client management
  function editSlskdClient() {
    const client = formData?.download.slskd_client;

    if (client) {
      setEditingSlskd({ ...client });
    } else {
      setEditingSlskd({
        url: '',
        api_key: '',
        enabled: true,
        download_directory: '/downloads/slskd',
      });
    }

    setShowSlskdForm(true);
  }

  function saveSlskdClient() {
    if (!editingSlskd) return;

    handleChange('download', 'slskd_client', editingSlskd);
    setShowSlskdForm(false);
    setEditingSlskd(null);
  }

  function deleteSlskdClient() {
    handleChange('download', 'slskd_client', null);
  }

  function updateEditingSlskd(field: keyof SlskdConfig, value: unknown) {
    if (!editingSlskd) return;
    setEditingSlskd({
      ...editingSlskd,
      [field]: value,
    });
  }

  function closeSlskdForm() {
    setShowSlskdForm(false);
    setEditingSlskd(null);
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
      normalize_query: false,
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

  function updateEditingIndexer(field: keyof Indexer, value: unknown) {
    if (!editingIndexer) return;
    setEditingIndexer({
      ...editingIndexer,
      indexer: { ...editingIndexer.indexer, [field]: value },
    });
  }

  function closeIndexerForm() {
    setShowIndexerForm(false);
    setEditingIndexer(null);
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

  function updateEditingProvider(field: keyof NotificationProvider, value: unknown) {
    if (!editingProvider) return;
    setEditingProvider({
      ...editingProvider,
      provider: { ...editingProvider.provider, [field]: value },
    });
  }

  function closeProviderForm() {
    setShowProviderForm(false);
    setEditingProvider(null);
  }

  const state: ConfigState = {
    loading,
    saving,
    config,
    formData,
    activeTab,
    showAdvanced,
    serverPassword,
    passwordChanged,
    editingClientType,
    editingClient,
    showClientForm,
    editingSlskd,
    showSlskdForm,
    editingIndexer,
    showIndexerForm,
    editingProvider,
    showProviderForm,
  };

  const actions: ConfigActions = {
    setActiveTab,
    setShowAdvanced,
    handleChange,
    handleSave,
    handlePasswordChange,
    editDownloadClient,
    saveDownloadClient,
    deleteDownloadClient,
    updateEditingClient,
    closeClientForm,
    editSlskdClient,
    saveSlskdClient,
    deleteSlskdClient,
    updateEditingSlskd,
    closeSlskdForm,
    addIndexer,
    editIndexer,
    saveIndexer,
    deleteIndexer,
    updateEditingIndexer,
    closeIndexerForm,
    addNotificationProvider,
    editNotificationProvider,
    saveNotificationProvider,
    deleteNotificationProvider,
    updateEditingProvider,
    closeProviderForm,
  };

  return [state, actions];
}

export function getClientTypeName(type: DownloadClientType): string {
  switch (type) {
    case 'sabnzbd': return 'SABnzbd';
    case 'nzbget': return 'NZBGet';
    case 'transmission': return 'Transmission';
    case 'qbittorrent': return 'qBittorrent';
    default: return type;
  }
}
