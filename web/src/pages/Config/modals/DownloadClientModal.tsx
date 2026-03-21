import { Modal } from '../../../components/Modal';
import { PathInput } from '../../../components/PathInput';
import { UrlInput } from '../../../components/UrlInput';
import type { DownloadClient, DownloadClientType } from '../../../types/api';

interface DownloadClientModalProps {
  isOpen: boolean;
  clientType: 'nzb' | 'torrent';
  client: DownloadClient;
  onClose: () => void;
  onSave: () => void;
  onUpdate: (field: keyof DownloadClient, value: unknown) => void;
}

export function DownloadClientModal({
  isOpen,
  clientType,
  client,
  onClose,
  onSave,
  onUpdate,
}: DownloadClientModalProps) {
  if (!isOpen) return null;

  return (
    <Modal
      title={clientType === 'nzb' ? 'Configure NZB Client' : 'Configure Torrent Client'}
      onClose={onClose}
      footer={
        <>
          <button type="button" onClick={onClose} className="btn-secondary">Cancel</button>
          <button type="button" onClick={onSave} className="btn-primary">Save</button>
        </>
      }
    >
      <div>
        <label className="block text-sm font-medium text-dark-text mb-2">Client Type</label>
        <select
          value={client.type}
          onChange={(e) => onUpdate('type', e.target.value as DownloadClientType)}
          className="input w-full"
        >
          {clientType === 'nzb' ? (
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

      <UrlInput
        label="URL"
        value={client.url}
        onChange={(value) => onUpdate('url', value)}
        placeholder="localhost:8080"
        defaultProtocol="http://"
      />

      {(client.type === 'sabnzbd' || client.type === 'nzbget') && (
        <div>
          <label className="block text-sm font-medium text-dark-text mb-2">API Key</label>
          <input
            type="text"
            value={client.api_key || ''}
            onChange={(e) => onUpdate('api_key', e.target.value || null)}
            className="input w-full"
            placeholder="API Key"
          />
        </div>
      )}

      {(client.type === 'transmission' || client.type === 'qbittorrent') && (
        <>
          <div>
            <label className="block text-sm font-medium text-dark-text mb-2">Username</label>
            <input
              type="text"
              value={client.username || ''}
              onChange={(e) => onUpdate('username', e.target.value || null)}
              className="input w-full"
              placeholder="Username"
            />
          </div>
          <div>
            <label className="block text-sm font-medium text-dark-text mb-2">Password</label>
            <input
              type="password"
              value={client.password || ''}
              onChange={(e) => onUpdate('password', e.target.value || null)}
              className="input w-full"
              placeholder="Password"
            />
          </div>
        </>
      )}

      {client.type === 'sabnzbd' && (
        <>
          <PathInput
            label="Download Directory"
            value={client.download_dir || ''}
            onChange={(value) => onUpdate('download_dir', value || null)}
            type="directory"
            placeholder="/path/to/downloads"
            description="Base directory where SABnzbd stores completed downloads"
          />
          <div>
            <label className="block text-sm font-medium text-dark-text mb-2">Category</label>
            <input
              type="text"
              value={client.category || ''}
              onChange={(e) => onUpdate('category', e.target.value || null)}
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
            checked={client.enabled}
            onChange={(e) => onUpdate('enabled', e.target.checked)}
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
    </Modal>
  );
}
