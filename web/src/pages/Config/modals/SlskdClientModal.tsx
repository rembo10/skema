import { Modal } from '../../../components/Modal';
import { PathInput } from '../../../components/PathInput';
import { UrlInput } from '../../../components/UrlInput';
import type { SlskdConfig } from '../../../types/api';

interface SlskdClientModalProps {
  isOpen: boolean;
  client: SlskdConfig;
  onClose: () => void;
  onSave: () => void;
  onUpdate: (field: keyof SlskdConfig, value: unknown) => void;
}

export function SlskdClientModal({
  isOpen,
  client,
  onClose,
  onSave,
  onUpdate,
}: SlskdClientModalProps) {
  if (!isOpen) return null;

  return (
    <Modal
      title="Configure slskd Client"
      onClose={onClose}
      footer={
        <>
          <button type="button" onClick={onClose} className="btn-secondary">Cancel</button>
          <button type="button" onClick={onSave} className="btn-primary">Save</button>
        </>
      }
    >
      <UrlInput
        label="URL"
        value={client.url}
        onChange={(value) => onUpdate('url', value)}
        placeholder="localhost:5030"
        defaultProtocol="http://"
      />

      <div>
        <label className="block text-sm font-medium text-dark-text mb-2">API Key</label>
        <input
          type="text"
          value={client.api_key || ''}
          onChange={(e) => onUpdate('api_key', e.target.value)}
          className="input w-full"
          placeholder="API Key"
        />
        <p className="mt-1 text-sm text-dark-text-secondary">
          API key from slskd settings
        </p>
      </div>

      <PathInput
        label="Download Directory"
        value={client.download_directory || ''}
        onChange={(value) => onUpdate('download_directory', value)}
        type="directory"
        placeholder="/downloads/slskd"
        description="Directory where slskd stores completed downloads"
      />

      <div className="flex items-start">
        <div className="flex items-center h-5">
          <input
            type="checkbox"
            checked={client.enabled}
            onChange={(e) => onUpdate('enabled', e.target.checked)}
            className="mt-0.5 h-4 w-4 rounded border-dark-border bg-dark-bg-subtle text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg-elevated"
          />
        </div>
        <div className="ml-3">
          <label className="text-sm font-medium text-dark-text">Enabled</label>
          <p className="text-sm text-dark-text-secondary">
            Enable slskd for music downloads
          </p>
        </div>
      </div>
    </Modal>
  );
}
