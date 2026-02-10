import { X } from 'lucide-react';
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
    <div className="fixed inset-0 bg-black/50 flex items-center justify-center p-4 z-50">
      <div className="bg-dark-bg-elevated rounded-lg shadow-xl max-w-2xl w-full max-h-[90vh] overflow-y-auto border border-dark-border">
        <div className="px-6 py-4 border-b border-dark-border flex items-center justify-between">
          <h3 className="text-lg font-medium text-dark-text">
            Configure slskd Client
          </h3>
          <button
            type="button"
            onClick={onClose}
            className="text-dark-text-tertiary hover:text-dark-text-secondary"
          >
            <X className="w-5 h-5" />
          </button>
        </div>
        <div className="px-6 py-4 space-y-4">
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
          >
            Save
          </button>
        </div>
      </div>
    </div>
  );
}
