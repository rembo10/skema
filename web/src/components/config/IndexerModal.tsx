import { X } from 'lucide-react';
import { UrlInput } from '../UrlInput';
import type { Indexer } from '../../types/api';

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
      <div 
        className="bg-dark-bg-elevated rounded-lg shadow-xl max-w-2xl w-full max-h-[90vh] overflow-y-auto border border-dark-border"
        role="dialog"
        aria-modal="true"
        aria-labelledby="indexer-modal-title"
      >
        <div className="px-6 py-4 border-b border-dark-border flex items-center justify-between">
          <h3 id="indexer-modal-title" className="text-lg font-medium text-dark-text">
            {isNew ? 'Add Indexer' : 'Edit Indexer'}
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
            <label htmlFor="indexer-name" className="block text-sm font-medium text-dark-text mb-2">
              Name
            </label>
            <input
              id="indexer-name"
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
            <label htmlFor="indexer-api-key" className="block text-sm font-medium text-dark-text mb-2">
              API Key (Optional)
            </label>
            <input
              id="indexer-api-key"
              type="text"
              value={indexer.api_key || ''}
              onChange={(e) => onUpdate('api_key', e.target.value || null)}
              className="input w-full"
              placeholder="API Key"
            />
          </div>

          <div>
            <label htmlFor="indexer-username" className="block text-sm font-medium text-dark-text mb-2">
              Username (Optional)
            </label>
            <input
              id="indexer-username"
              type="text"
              value={indexer.username || ''}
              onChange={(e) => onUpdate('username', e.target.value || null)}
              className="input w-full"
              placeholder="Username"
            />
          </div>

          <div>
            <label htmlFor="indexer-password" className="block text-sm font-medium text-dark-text mb-2">
              Password (Optional)
            </label>
            <input
              id="indexer-password"
              type="password"
              value={indexer.password || ''}
              onChange={(e) => onUpdate('password', e.target.value || null)}
              className="input w-full"
              placeholder="Password"
            />
          </div>

          <div>
            <label htmlFor="indexer-categories" className="block text-sm font-medium text-dark-text mb-2">
              Categories
            </label>
            <input
              id="indexer-categories"
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
            <label htmlFor="indexer-priority" className="block text-sm font-medium text-dark-text mb-2">
              Priority
            </label>
            <input
              id="indexer-priority"
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
                id="indexer-enabled"
                type="checkbox"
                checked={indexer.enabled}
                onChange={(e) => onUpdate('enabled', e.target.checked)}
                className="mt-0.5 h-4 w-4 rounded border-dark-border bg-dark-bg-subtle text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg-elevated"
              />
            </div>
            <div className="ml-3">
              <label htmlFor="indexer-enabled" className="text-sm font-medium text-dark-text">
                Enabled
              </label>
              <p className="text-sm text-dark-text-secondary">Enable this indexer</p>
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
