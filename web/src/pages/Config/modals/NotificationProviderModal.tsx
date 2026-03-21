import { Modal } from '../../../components/Modal';
import type { NotificationProvider } from '../../../types/api';

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
    <Modal
      title={isNew ? 'Add Notification Provider' : 'Edit Notification Provider'}
      onClose={onClose}
      footer={
        <>
          <button type="button" onClick={onClose} className="btn-secondary">Cancel</button>
          <button type="button" onClick={onSave} className="btn-primary" disabled={!provider.user_key}>Save</button>
        </>
      }
    >
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
          value={provider.user_key}
          onChange={(e) => onUpdate('user_key', e.target.value)}
          className="input w-full"
          placeholder="Enter your Pushover user key"
        />
        <p className="mt-1 text-sm text-dark-text-secondary">Your Pushover user or group key</p>
      </div>

      <div>
        <label className="block text-sm font-medium text-dark-text mb-2">Device Name (Optional)</label>
        <input
          type="text"
          value={provider.device || ''}
          onChange={(e) => onUpdate('device', e.target.value || null)}
          className="input w-full"
          placeholder="Leave empty for all devices"
        />
      </div>

      <div>
        <label className="block text-sm font-medium text-dark-text mb-2">Priority</label>
        <select
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
    </Modal>
  );
}
