import { Plus, Edit2, Trash2, Search } from 'lucide-react';
import type { Config, Indexer } from '../../../types/api';

interface IndexersTabProps {
  formData: Config;
  onChange: (section: keyof Config, field: string, value: unknown) => void;
  onAddIndexer: () => void;
  onEditIndexer: (index: number) => void;
  onDeleteIndexer: (index: number) => void;
}

export function IndexersTab({
  formData,
  onChange,
  onAddIndexer,
  onEditIndexer,
  onDeleteIndexer,
}: IndexersTabProps) {
  return (
    <div className="space-y-6">
      <div className="card">
        <div className="px-6 py-5 border-b border-dark-border">
          <h2 className="text-lg font-medium text-dark-text">Indexer Settings</h2>
          <p className="mt-1 text-sm text-dark-text-secondary">
            Configure global indexer behavior
          </p>
        </div>
        <div className="px-6 py-5 space-y-6">
          <div>
            <label htmlFor="indexers_search_timeout" className="block text-sm font-medium text-dark-text mb-2">
              Search Timeout (seconds)
            </label>
            <input
              type="number"
              id="indexers_search_timeout"
              value={formData.indexers.search_timeout ?? 30}
              onChange={(e) => onChange('indexers', 'search_timeout', parseInt(e.target.value))}
              min="5"
              className="input w-48"
            />
            <p className="mt-2 text-sm text-dark-text-secondary">
              Maximum time to wait for indexer search responses
            </p>
          </div>
        </div>
      </div>

      <div className="card">
        <div className="px-6 py-5 border-b border-dark-border flex items-center justify-between">
          <div>
            <h2 className="text-lg font-medium text-dark-text">Configured Indexers</h2>
            <p className="mt-1 text-sm text-dark-text-secondary">
              Newznab and Torznab indexers for searching releases
            </p>
          </div>
          <button
            type="button"
            onClick={onAddIndexer}
            className="btn-primary text-sm"
          >
            <Plus className="w-4 h-4 mr-1" />
            Add Indexer
          </button>
        </div>
        <div className="px-6 py-5">
          {formData.indexers.list && formData.indexers.list.length > 0 ? (
            <div className="space-y-3">
              {formData.indexers.list.map((indexer, idx) => (
                <IndexerCard
                  key={idx}
                  indexer={indexer}
                  onEdit={() => onEditIndexer(idx)}
                  onDelete={() => {
                    if (confirm(`Delete indexer "${indexer.name}"?`)) {
                      onDeleteIndexer(idx);
                    }
                  }}
                />
              ))}
            </div>
          ) : (
            <div className="text-center py-8">
              <Search className="mx-auto h-12 w-12 text-dark-text-tertiary" />
              <p className="mt-2 text-sm text-dark-text-secondary">No indexers configured</p>
              <button
                type="button"
                onClick={onAddIndexer}
                className="btn-secondary mt-4"
              >
                <Plus className="w-4 h-4 mr-2" />
                Add Your First Indexer
              </button>
            </div>
          )}
        </div>
      </div>
    </div>
  );
}

function IndexerCard({
  indexer,
  onEdit,
  onDelete,
}: {
  indexer: Indexer;
  onEdit: () => void;
  onDelete: () => void;
}) {
  return (
    <div className="border border-dark-border rounded-lg p-4 hover:border-dark-border-bright transition-colors">
      <div className="flex items-start justify-between">
        <div className="flex-1">
          <div className="flex items-center gap-3 mb-2">
            <h3 className="text-sm font-medium text-dark-text">{indexer.name || 'Unnamed Indexer'}</h3>
            <span className={`inline-flex rounded-full px-2 py-0.5 text-xs font-semibold border ${
              indexer.enabled
                ? 'bg-dark-success-muted text-dark-success border-dark-success/30'
                : 'bg-dark-bg-subtle text-dark-text-tertiary border-dark-border'
            }`}>
              {indexer.enabled ? 'Enabled' : 'Disabled'}
            </span>
          </div>
          <p className="text-sm text-dark-text-secondary mb-2">{indexer.url}</p>
          <div className="flex gap-4 text-xs text-dark-text-secondary">
            {indexer.categories && indexer.categories.length > 0 && (
              <span>Categories: {indexer.categories.join(', ')}</span>
            )}
            <span>Priority: {indexer.priority}</span>
          </div>
        </div>
        <div className="flex items-center gap-2 ml-4">
          <button
            type="button"
            onClick={onEdit}
            className="p-2 text-dark-text-tertiary hover:text-dark-accent transition-colors"
            title="Edit indexer"
          >
            <Edit2 className="w-4 h-4" />
          </button>
          <button
            type="button"
            onClick={onDelete}
            className="p-2 text-dark-text-tertiary hover:text-dark-error transition-colors"
            title="Delete indexer"
          >
            <Trash2 className="w-4 h-4" />
          </button>
        </div>
      </div>
    </div>
  );
}
