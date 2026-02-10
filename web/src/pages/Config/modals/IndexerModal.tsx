import { X } from 'lucide-react';
import { UrlInput } from '../../../components/UrlInput';
import type { Indexer } from '../../../types/api';

interface IndexerModalProps {
  isOpen: boolean;
  isNew: boolean;
  indexer: Indexer;
  onClose: () => void;
  onSave: () => void;
  onUpdate: (field: keyof Indexer, value: unknown) => void;
}

export function IndexerModal({
  isOpen,
  isNew,
  indexer,
  onClose,
  onSave,
  onUpdate,
}: IndexerModalProps) {
  if (!isOpen) return null;

  return (
    <div className="fixed inset-0 bg-black/50 flex items-center justify-center p-4 z-50">
      <div className="bg-dark-bg-elevated rounded-lg shadow-xl max-w-2xl w-full max-h-[90vh] overflow-y-auto border border-dark-border">
        <div className="px-6 py-4 border-b border-dark-border flex items-center justify-between">
          <h3 className="text-lg font-medium text-dark-text">
            {isNew ? 'Add Indexer' : 'Edit Indexer'}
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
          <div>
            <label className="block text-sm font-medium text-dark-text mb-2">Name</label>
            <input
              type="text"
              value={indexer.name}
              onChange={(e) => onUpdate('name', e.target.value)}
              className="input w-full"
              placeholder="My Indexer"
            />
          </div>

          <UrlInput
            label="URL"
            value={indexer.url}
            onChange={(value) => onUpdate('url', value)}
            placeholder="indexer.example.com"
          />

          <div>
            <label className="block text-sm font-medium text-dark-text mb-2">API Key (Optional)</label>
            <input
              type="text"
              value={indexer.api_key || ''}
              onChange={(e) => onUpdate('api_key', e.target.value || null)}
              className="input w-full"
              placeholder="API Key"
            />
          </div>

          <div>
            <label className="block text-sm font-medium text-dark-text mb-2">Username (Optional)</label>
            <input
              type="text"
              value={indexer.username || ''}
              onChange={(e) => onUpdate('username', e.target.value || null)}
              className="input w-full"
              placeholder="Username"
            />
          </div>

          <div>
            <label className="block text-sm font-medium text-dark-text mb-2">Password (Optional)</label>
            <input
              type="password"
              value={indexer.password || ''}
              onChange={(e) => onUpdate('password', e.target.value || null)}
              className="input w-full"
              placeholder="Password"
            />
          </div>

          <div>
            <label className="block text-sm font-medium text-dark-text mb-2">Categories</label>
            <input
              type="text"
              value={indexer.categories.join(', ')}
              onChange={(e) => {
                const cats = e.target.value.split(',').map(c => parseInt(c.trim())).filter(c => !isNaN(c));
                onUpdate('categories', cats);
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
              value={indexer.priority}
              onChange={(e) => onUpdate('priority', parseInt(e.target.value))}
              className="input w-full"
              placeholder="0"
            />
            <p className="mt-1 text-sm text-dark-text-secondary">Higher priority indexers are searched first</p>
          </div>

          <div className="flex items-start">
            <div className="flex items-center h-5">
              <input
                type="checkbox"
                checked={indexer.enabled}
                onChange={(e) => onUpdate('enabled', e.target.checked)}
                className="mt-0.5 h-4 w-4 rounded border-dark-border bg-dark-bg-subtle text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg-elevated"
              />
            </div>
            <div className="ml-3">
              <label className="text-sm font-medium text-dark-text">Enabled</label>
              <p className="text-sm text-dark-text-secondary">Enable this indexer</p>
            </div>
          </div>

          <div className="flex items-start">
            <div className="flex items-center h-5">
              <input
                type="checkbox"
                checked={indexer.normalize_query || false}
                onChange={(e) => onUpdate('normalize_query', e.target.checked)}
                className="mt-0.5 h-4 w-4 rounded border-dark-border bg-dark-bg-subtle text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg-elevated"
              />
            </div>
            <div className="ml-3">
              <label className="text-sm font-medium text-dark-text">Normalize Queries</label>
              <p className="text-sm text-dark-text-secondary">Remove special characters from searches (e.g., "AC/DC" to "ACDC"). Enable for older indexers that don't handle special characters well. Modern indexers like Bullet should keep this disabled.</p>
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
