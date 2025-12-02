import { X } from 'lucide-react';
import type { NotificationProvider } from '../../types/api';

interface NotificationProviderModalProps {
  isOpen: boolean;
  isNew: boolean;
  provider: NotificationProvider;
  onClose: () => void;
  onSave: () => void;
  onUpdate: (field: keyof NotificationProvider, value: unknown) => void;
}

export function NotificationProviderModal({
  isOpen,
  isNew,
  provider,
  onClose,
  onSave,
  onUpdate,
}: NotificationProviderModalProps) {
  if (!isOpen) return null;

  return (
    <div className="fixed inset-0 bg-black/50 flex items-center justify-center p-4 z-50">
      <div 
        className="bg-dark-bg-elevated rounded-lg shadow-xl max-w-2xl w-full max-h-[90vh] overflow-y-auto border border-dark-border"
        role="dialog"
        aria-modal="true"
        aria-labelledby="notification-provider-modal-title"
      >
        <div className="px-6 py-4 border-b border-dark-border flex items-center justify-between">
          <h3 id="notification-provider-modal-title" className="text-lg font-medium text-dark-text">
            {isNew ? 'Add Notification Provider' : 'Edit Notification Provider'}
          </h3>
          <button
            type="button"
            onClick={onClose}
            className="text-dark-text-tertiary hover:text-dark-text-secondary"
            aria-label="Close modal"
          >
            <X className="w-5 h-5" />
          </button>
        </div>
        <div className="px-6 py-4 space-y-4">
          <div>
            <label htmlFor="provider-type" className="block text-sm font-medium text-dark-text mb-2">
              Provider Type
            </label>
            <select
              id="provider-type"
              value="pushover"
              disabled
              className="input w-full opacity-50 cursor-not-allowed"
            >
              <option value="pushover">Pushover</option>
            </select>
            <p className="mt-1 text-sm text-dark-text-secondary">Currently only Pushover is supported</p>
          </div>

          <div>
            <label htmlFor="provider-user-key" className="block text-sm font-medium text-dark-text mb-2">
              User Key <span className="text-dark-error">*</span>
            </label>
            <input
              id="provider-user-key"
              type="text"
              value={provider.user_key}
              onChange={(e) => onUpdate('user_key', e.target.value)}
              className="input w-full"
              placeholder="Enter your Pushover user key"
              required
            />
            <p className="mt-1 text-sm text-dark-text-secondary">Your Pushover user or group key</p>
          </div>

          <div>
            <label htmlFor="provider-device" className="block text-sm font-medium text-dark-text mb-2">
              Device Name (Optional)
            </label>
            <input
              id="provider-device"
              type="text"
              value={provider.device || ''}
              onChange={(e) => onUpdate('device', e.target.value || null)}
              className="input w-full"
              placeholder="Leave empty for all devices"
            />
          </div>

          <div>
            <label htmlFor="provider-priority" className="block text-sm font-medium text-dark-text mb-2">
              Priority
            </label>
            <select
              id="provider-priority"
              value={provider.priority}
              onChange={(e) => onUpdate('priority', parseInt(e.target.value))}
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
            onClick={onClose}
            className="btn-secondary"
          >
            Cancel
          </button>
          <button
            type="button"
            onClick={onSave}
            className="btn-primary"
            disabled={!provider.user_key}
          >
            Save
          </button>
        </div>
      </div>
    </div>
  );
}
