import { Plus, Edit2, Trash2, Download } from 'lucide-react';
import type { Config, DownloadClient, SlskdConfig } from '../../../types/api';
import { getClientTypeName } from '../useConfigState';

interface DownloadTabProps {
  formData: Config;
  onChange: (section: keyof Config, field: string, value: unknown) => void;
  onEditNzbClient: () => void;
  onEditTorrentClient: () => void;
  onEditSlskdClient: () => void;
  onDeleteNzbClient: () => void;
  onDeleteTorrentClient: () => void;
  onDeleteSlskdClient: () => void;
}

function ClientCard({
  client,
  clientType,
  label,
  badgeColor,
  onEdit,
  onDelete,
}: {
  client: DownloadClient | SlskdConfig | null;
  clientType: 'nzb' | 'torrent' | 'slskd';
  label: string;
  badgeColor: string;
  onEdit: () => void;
  onDelete: () => void;
}) {
  if (!client) {
    return (
      <div className="text-center py-8">
        <Download className="mx-auto h-12 w-12 text-dark-text-tertiary" />
        <p className="mt-2 text-sm text-dark-text-secondary">No {label} configured</p>
        <button
          type="button"
          onClick={onEdit}
          className="btn-secondary mt-4"
        >
          <Plus className="w-4 h-4 mr-2" />
          Configure {label}
        </button>
      </div>
    );
  }

  const isDownloadClient = 'type' in client;
  const clientName = isDownloadClient ? getClientTypeName(client.type) : 'slskd';

  return (
    <div className="border border-dark-border rounded-lg p-4">
      <div className="flex items-start justify-between">
        <div className="flex-1">
          <div className="flex items-center gap-2 mb-2">
            <h3 className="text-sm font-medium text-dark-text">{clientName}</h3>
            <span className={`inline-flex rounded-full px-2 py-0.5 text-xs font-semibold border ${badgeColor}`}>
              {clientType === 'nzb' ? 'NZB' : clientType === 'torrent' ? 'Torrent' : 'Soulseek'}
            </span>
            <span className={`inline-flex rounded-full px-2 py-0.5 text-xs font-semibold border ${
              client.enabled
                ? 'bg-dark-success-muted text-dark-success border-dark-success/30'
                : 'bg-dark-bg-subtle text-dark-text-tertiary border-dark-border'
            }`}>
              {client.enabled ? 'Enabled' : 'Disabled'}
            </span>
          </div>
          <p className="text-sm text-dark-text-secondary mt-1">{client.url}</p>
        </div>
        <div className="flex items-center gap-2 ml-4">
          <button
            type="button"
            onClick={onEdit}
            className="p-2 text-dark-text-tertiary hover:text-dark-accent transition-colors"
            title="Edit client"
          >
            <Edit2 className="w-4 h-4" />
          </button>
          <button
            type="button"
            onClick={() => {
              if (confirm(`Delete ${label}?`)) {
                onDelete();
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
  );
}

export function DownloadTab({
  formData,
  onChange,
  onEditNzbClient,
  onEditTorrentClient,
  onEditSlskdClient,
  onDeleteNzbClient,
  onDeleteTorrentClient,
  onDeleteSlskdClient,
}: DownloadTabProps) {
  return (
    <div className="space-y-6">
      {/* General Download Settings */}
      <div className="card">
        <div className="px-6 py-5 border-b border-dark-border">
          <h2 className="text-lg font-medium text-dark-text">Download Settings</h2>
          <p className="mt-1 text-sm text-dark-text-secondary">
            Configure download behavior
          </p>
        </div>
        <div className="px-6 py-5 space-y-6">
          <div>
            <label htmlFor="download_check_interval" className="block text-sm font-medium text-dark-text mb-2">
              Check Interval (seconds)
            </label>
            <input
              type="number"
              id="download_check_interval"
              value={formData.download.check_interval ?? 60}
              onChange={(e) => onChange('download', 'check_interval', parseInt(e.target.value))}
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
              onChange={(e) => onChange('download', 'auto_import', e.target.checked)}
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
              onChange={(e) => onChange('download', 'min_seeders', e.target.value ? parseInt(e.target.value) : null)}
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
              onChange={(e) => onChange('download', 'max_size', e.target.value ? parseInt(e.target.value) : null)}
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
            onClick={onEditNzbClient}
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
          <ClientCard
            client={formData.download.nzb_client}
            clientType="nzb"
            label="NZB client"
            badgeColor="bg-dark-info/10 text-dark-info border-dark-info/30"
            onEdit={onEditNzbClient}
            onDelete={onDeleteNzbClient}
          />
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
            onClick={onEditTorrentClient}
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
          <ClientCard
            client={formData.download.torrent_client}
            clientType="torrent"
            label="torrent client"
            badgeColor="bg-dark-accent/10 text-dark-accent border-dark-accent/30"
            onEdit={onEditTorrentClient}
            onDelete={onDeleteTorrentClient}
          />
        </div>
      </div>

      {/* slskd (Soulseek) Client */}
      <div className="card">
        <div className="px-6 py-5 border-b border-dark-border flex justify-between items-center">
          <div>
            <h2 className="text-lg font-medium text-dark-text">slskd Client</h2>
            <p className="mt-1 text-sm text-dark-text-secondary">
              Configure slskd (Soulseek) for P2P music downloads
            </p>
          </div>
          <button
            type="button"
            onClick={onEditSlskdClient}
            className="btn-secondary text-sm"
          >
            {formData.download.slskd_client ? (
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
          <ClientCard
            client={formData.download.slskd_client}
            clientType="slskd"
            label="slskd client"
            badgeColor="bg-purple-500/10 text-purple-400 border-purple-500/30"
            onEdit={onEditSlskdClient}
            onDelete={onDeleteSlskdClient}
          />
        </div>
      </div>
    </div>
  );
}
