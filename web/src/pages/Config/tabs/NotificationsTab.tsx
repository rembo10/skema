import { Plus, Edit2, Trash2, Bell } from 'lucide-react';
import type { Config, NotificationProvider } from '../../../types/api';

interface NotificationsTabProps {
  formData: Config;
  onChange: (section: keyof Config, field: string, value: unknown) => void;
  onAddProvider: () => void;
  onEditProvider: (index: number) => void;
  onDeleteProvider: (index: number) => void;
}

export function NotificationsTab({
  formData,
  onChange,
  onAddProvider,
  onEditProvider,
  onDeleteProvider,
}: NotificationsTabProps) {
  return (
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
              onChange={(e) => onChange('notifications', 'enabled', e.target.checked)}
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
                onChange={(e) => onChange('notifications', 'on_album_found', e.target.checked)}
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
                onChange={(e) => onChange('notifications', 'on_album_imported', e.target.checked)}
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
            onClick={onAddProvider}
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
                <ProviderCard
                  key={idx}
                  provider={provider}
                  onEdit={() => onEditProvider(idx)}
                  onDelete={() => {
                    if (confirm('Delete this notification provider?')) {
                      onDeleteProvider(idx);
                    }
                  }}
                />
              ))}
            </div>
          ) : (
            <div className="text-center py-8">
              <Bell className="mx-auto h-12 w-12 text-dark-text-tertiary" />
              <p className="mt-2 text-sm text-dark-text-secondary">No notification providers configured</p>
              <button
                type="button"
                onClick={onAddProvider}
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
  );
}

function ProviderCard({
  provider,
  onEdit,
  onDelete,
}: {
  provider: NotificationProvider;
  onEdit: () => void;
  onDelete: () => void;
}) {
  return (
    <div className="border border-dark-border rounded-lg p-4 hover:border-dark-border-bright transition-colors">
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
            onClick={onEdit}
            className="p-2 text-dark-text-tertiary hover:text-dark-accent transition-colors"
            title="Edit provider"
          >
            <Edit2 className="w-4 h-4" />
          </button>
          <button
            type="button"
            onClick={onDelete}
            className="p-2 text-dark-text-tertiary hover:text-dark-error transition-colors"
            title="Delete provider"
          >
            <Trash2 className="w-4 h-4" />
          </button>
        </div>
      </div>
    </div>
  );
}
