import { Save } from 'lucide-react';

import { useConfigState } from './useConfigState';
import { ConfigTabs } from './ConfigTabs';
import { LibraryTab } from './tabs/LibraryTab';
import { SystemTab } from './tabs/SystemTab';
import { ServerTab } from './tabs/ServerTab';
import { DownloadTab } from './tabs/DownloadTab';
import { IndexersTab } from './tabs/IndexersTab';
import { MusicbrainzTab } from './tabs/MusicbrainzTab';
import { NotificationsTab } from './tabs/NotificationsTab';
import { DownloadClientModal } from './modals/DownloadClientModal';
import { SlskdClientModal } from './modals/SlskdClientModal';
import { IndexerModal } from './modals/IndexerModal';
import { NotificationProviderModal } from './modals/NotificationProviderModal';

export default function Config() {
  const [state, actions] = useConfigState();

  if (state.loading || !state.formData) {
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
              checked={state.showAdvanced}
              onChange={(e) => actions.setShowAdvanced(e.target.checked)}
              className="sr-only peer"
            />
            <div className="w-10 h-6 bg-dark-bg-subtle rounded-full peer peer-checked:bg-dark-accent transition-colors"></div>
            <div className="absolute left-1 top-1 w-4 h-4 bg-dark-text-tertiary rounded-full transition-all peer-checked:translate-x-4 peer-checked:bg-white"></div>
          </div>
        </label>
      </div>

      {/* Tab Navigation */}
      <ConfigTabs activeTab={state.activeTab} onTabChange={actions.setActiveTab} />

      {/* Main content area */}
      <form onSubmit={actions.handleSave} className="space-y-6">
        {state.activeTab === 'library' && (
          <LibraryTab
            formData={state.formData}
            showAdvanced={state.showAdvanced}
            onChange={actions.handleChange}
          />
        )}

        {state.activeTab === 'system' && (
          <SystemTab
            formData={state.formData}
            showAdvanced={state.showAdvanced}
            onChange={actions.handleChange}
          />
        )}

        {state.activeTab === 'server' && state.config && (
          <ServerTab
            formData={state.formData}
            config={state.config}
            showAdvanced={state.showAdvanced}
            serverPassword={state.serverPassword}
            passwordChanged={state.passwordChanged}
            onChange={actions.handleChange}
            onPasswordChange={actions.handlePasswordChange}
          />
        )}

        {state.activeTab === 'download' && (
          <DownloadTab
            formData={state.formData}
            onChange={actions.handleChange}
            onEditNzbClient={() => actions.editDownloadClient('nzb')}
            onEditTorrentClient={() => actions.editDownloadClient('torrent')}
            onEditSlskdClient={actions.editSlskdClient}
            onDeleteNzbClient={() => actions.deleteDownloadClient('nzb')}
            onDeleteTorrentClient={() => actions.deleteDownloadClient('torrent')}
            onDeleteSlskdClient={actions.deleteSlskdClient}
          />
        )}

        {state.activeTab === 'indexers' && (
          <IndexersTab
            formData={state.formData}
            onChange={actions.handleChange}
            onAddIndexer={actions.addIndexer}
            onEditIndexer={actions.editIndexer}
            onDeleteIndexer={actions.deleteIndexer}
          />
        )}

        {state.activeTab === 'musicbrainz' && (
          <MusicbrainzTab
            formData={state.formData}
            showAdvanced={state.showAdvanced}
            onChange={actions.handleChange}
          />
        )}

        {state.activeTab === 'notifications' && (
          <NotificationsTab
            formData={state.formData}
            onChange={actions.handleChange}
            onAddProvider={actions.addNotificationProvider}
            onEditProvider={actions.editNotificationProvider}
            onDeleteProvider={actions.deleteNotificationProvider}
          />
        )}

        {/* Form actions */}
        <div className="flex justify-end pt-4">
          <button
            type="submit"
            disabled={state.saving}
            className="btn-primary disabled:opacity-50 disabled:cursor-not-allowed flex items-center gap-2"
          >
            <Save className="w-4 h-4" />
            {state.saving ? 'Saving...' : 'Save Changes'}
          </button>
        </div>
      </form>

      {/* Modals */}
      {state.showClientForm && state.editingClient && state.editingClientType && (
        <DownloadClientModal
          isOpen={state.showClientForm}
          clientType={state.editingClientType}
          client={state.editingClient}
          onClose={actions.closeClientForm}
          onSave={actions.saveDownloadClient}
          onUpdate={actions.updateEditingClient}
        />
      )}

      {state.showSlskdForm && state.editingSlskd && (
        <SlskdClientModal
          isOpen={state.showSlskdForm}
          client={state.editingSlskd}
          onClose={actions.closeSlskdForm}
          onSave={actions.saveSlskdClient}
          onUpdate={actions.updateEditingSlskd}
        />
      )}

      {state.showIndexerForm && state.editingIndexer && (
        <IndexerModal
          isOpen={state.showIndexerForm}
          isNew={state.editingIndexer.index === -1}
          indexer={state.editingIndexer.indexer}
          onClose={actions.closeIndexerForm}
          onSave={actions.saveIndexer}
          onUpdate={actions.updateEditingIndexer}
        />
      )}

      {state.showProviderForm && state.editingProvider && (
        <NotificationProviderModal
          isOpen={state.showProviderForm}
          isNew={state.editingProvider.index === -1}
          provider={state.editingProvider.provider}
          onClose={actions.closeProviderForm}
          onSave={actions.saveNotificationProvider}
          onUpdate={actions.updateEditingProvider}
        />
      )}
    </div>
  );
}
