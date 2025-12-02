import { useState, useEffect } from 'react';
import { X, ChevronDown, ChevronRight, Search, Clock, CheckCircle, XCircle, AlertCircle, Timer } from 'lucide-react';
import { api } from '../lib/api';
import type { SearchHistory, SearchHistoryResult, CatalogAlbum } from '../types/api';

interface SearchHistoryModalProps {
  album: CatalogAlbum;
  onClose: () => void;
}

function formatBytes(bytes: number | null): string {
  if (bytes === null) return '-';
  if (bytes < 1024) return `${bytes} B`;
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)} KB`;
  if (bytes < 1024 * 1024 * 1024) return `${(bytes / (1024 * 1024)).toFixed(1)} MB`;
  return `${(bytes / (1024 * 1024 * 1024)).toFixed(2)} GB`;
}

function formatDate(dateString: string): string {
  const date = new Date(dateString);
  return date.toLocaleString('en-US', {
    month: 'short',
    day: 'numeric',
    year: 'numeric',
    hour: '2-digit',
    minute: '2-digit',
  });
}

function OutcomeIcon({ outcome }: { outcome: string }) {
  switch (outcome) {
    case 'downloaded':
      return <CheckCircle className="h-4 w-4 text-green-500" />;
    case 'no_results':
      return <Search className="h-4 w-4 text-yellow-500" />;
    case 'failed':
      return <XCircle className="h-4 w-4 text-red-500" />;
    case 'no_client':
      return <AlertCircle className="h-4 w-4 text-orange-500" />;
    case 'timeout':
      return <Timer className="h-4 w-4 text-gray-500" />;
    default:
      return <Clock className="h-4 w-4 text-gray-500" />;
  }
}

function OutcomeLabel({ outcome }: { outcome: string }) {
  const labels: Record<string, string> = {
    downloaded: 'Downloaded',
    no_results: 'No Results',
    failed: 'Failed',
    no_client: 'No Client',
    timeout: 'Timeout',
  };
  return <span>{labels[outcome] || outcome}</span>;
}

export default function SearchHistoryModal({ album, onClose }: SearchHistoryModalProps) {
  const [history, setHistory] = useState<SearchHistory[]>([]);
  const [loading, setLoading] = useState(true);
  const [expandedId, setExpandedId] = useState<number | null>(null);
  const [results, setResults] = useState<Record<number, SearchHistoryResult[]>>({});
  const [loadingResults, setLoadingResults] = useState<number | null>(null);

  useEffect(() => {
    loadHistory();
  }, [album.id]);

  const loadHistory = async () => {
    if (!album.id) return;
    try {
      setLoading(true);
      const data = await api.getSearchHistory(album.id);
      setHistory(data);
    } catch (error) {
      console.error('Failed to load search history:', error);
    } finally {
      setLoading(false);
    }
  };

  const toggleExpand = async (historyId: number) => {
    if (expandedId === historyId) {
      setExpandedId(null);
      return;
    }

    setExpandedId(historyId);

    // Load results if not already loaded
    if (!results[historyId] && album.id) {
      try {
        setLoadingResults(historyId);
        const data = await api.getSearchHistoryResults(album.id, historyId);
        setResults(prev => ({ ...prev, [historyId]: data }));
      } catch (error) {
        console.error('Failed to load search results:', error);
      } finally {
        setLoadingResults(null);
      }
    }
  };

  return (
    <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50 p-4">
      <div className="bg-dark-bg-elevated rounded-lg shadow-xl w-full max-w-4xl max-h-[80vh] flex flex-col">
        {/* Header */}
        <div className="flex items-center justify-between p-4 border-b border-dark-border">
          <div>
            <h2 className="text-lg font-semibold text-dark-text">Search History</h2>
            <p className="text-sm text-dark-text-secondary">
              {album.title} by {album.artist_name}
            </p>
          </div>
          <button
            onClick={onClose}
            className="p-2 hover:bg-dark-bg-hover rounded-lg transition-colors"
          >
            <X className="h-5 w-5 text-dark-text-secondary" />
          </button>
        </div>

        {/* Content */}
        <div className="flex-1 overflow-y-auto p-4">
          {loading ? (
            <div className="flex items-center justify-center h-32">
              <div className="animate-spin rounded-full h-8 w-8 border-2 border-dark-border border-t-dark-accent"></div>
            </div>
          ) : history.length === 0 ? (
            <div className="text-center py-12">
              <Search className="mx-auto h-12 w-12 text-dark-text-tertiary" />
              <p className="mt-4 text-dark-text-secondary">No search history for this album</p>
            </div>
          ) : (
            <div className="space-y-2">
              {history.map((entry) => (
                <div key={entry.id} className="border border-dark-border rounded-lg overflow-hidden">
                  {/* Search entry header */}
                  <button
                    onClick={() => toggleExpand(entry.id)}
                    className="w-full px-4 py-3 flex items-center gap-4 hover:bg-dark-bg-hover transition-colors text-left"
                  >
                    {expandedId === entry.id ? (
                      <ChevronDown className="h-4 w-4 text-dark-text-tertiary flex-shrink-0" />
                    ) : (
                      <ChevronRight className="h-4 w-4 text-dark-text-tertiary flex-shrink-0" />
                    )}
                    
                    <div className="flex-1 min-w-0">
                      <div className="flex items-center gap-2">
                        <OutcomeIcon outcome={entry.outcome} />
                        <span className="text-sm font-medium text-dark-text">
                          <OutcomeLabel outcome={entry.outcome} />
                        </span>
                        <span className="text-xs text-dark-text-tertiary">
                          {formatDate(entry.searched_at)}
                        </span>
                      </div>
                      
                      <div className="flex items-center gap-4 mt-1 text-xs text-dark-text-secondary">
                        <span>{entry.total_results} results</span>
                        {entry.duration_ms && (
                          <span>{(entry.duration_ms / 1000).toFixed(2)}s</span>
                        )}
                        {entry.selected_title && (
                          <span className="truncate">
                            Selected: {entry.selected_title}
                          </span>
                        )}
                      </div>
                    </div>

                    {entry.selected_score && (
                      <div className="text-right flex-shrink-0">
                        <div className="text-sm font-medium text-dark-accent">
                          Score: {entry.selected_score}
                        </div>
                        {entry.selected_indexer && (
                          <div className="text-xs text-dark-text-tertiary">
                            {entry.selected_indexer}
                          </div>
                        )}
                      </div>
                    )}
                  </button>

                  {/* Expanded results */}
                  {expandedId === entry.id && (
                    <div className="border-t border-dark-border bg-dark-bg-subtle">
                      {loadingResults === entry.id ? (
                        <div className="flex items-center justify-center py-8">
                          <div className="animate-spin rounded-full h-6 w-6 border-2 border-dark-border border-t-dark-accent"></div>
                        </div>
                      ) : results[entry.id]?.length === 0 ? (
                        <div className="py-8 text-center text-dark-text-secondary text-sm">
                          No results stored for this search
                        </div>
                      ) : (
                        <div className="overflow-x-auto">
                          <table className="min-w-full text-sm">
                            <thead>
                              <tr className="text-xs text-dark-text-tertiary uppercase">
                                <th className="px-4 py-2 text-left">#</th>
                                <th className="px-4 py-2 text-left">Title</th>
                                <th className="px-4 py-2 text-left">Indexer</th>
                                <th className="px-4 py-2 text-right">Size</th>
                                <th className="px-4 py-2 text-right">Seeders</th>
                                <th className="px-4 py-2 text-right">Score</th>
                                <th className="px-4 py-2 text-left">Quality</th>
                              </tr>
                            </thead>
                            <tbody className="divide-y divide-dark-border">
                              {results[entry.id]?.map((result) => (
                                <tr 
                                  key={result.id} 
                                  className={`hover:bg-dark-bg-hover ${
                                    entry.selected_title === result.title ? 'bg-dark-accent/10' : ''
                                  }`}
                                >
                                  <td className="px-4 py-2 text-dark-text-tertiary">
                                    {result.rank}
                                  </td>
                                  <td className="px-4 py-2">
                                    <div className="max-w-md truncate text-dark-text" title={result.title}>
                                      {result.title}
                                    </div>
                                    <div className="text-xs text-dark-text-tertiary">
                                      {result.download_type.toUpperCase()}
                                    </div>
                                  </td>
                                  <td className="px-4 py-2 text-dark-text-secondary">
                                    {result.indexer_name}
                                  </td>
                                  <td className="px-4 py-2 text-right text-dark-text-secondary">
                                    {formatBytes(result.size_bytes)}
                                  </td>
                                  <td className="px-4 py-2 text-right text-dark-text-secondary">
                                    {result.seeders ?? '-'}
                                  </td>
                                  <td className="px-4 py-2 text-right font-medium text-dark-accent">
                                    {result.score}
                                  </td>
                                  <td className="px-4 py-2 text-dark-text-secondary">
                                    {result.quality || '-'}
                                  </td>
                                </tr>
                              ))}
                            </tbody>
                          </table>
                        </div>
                      )}
                    </div>
                  )}
                </div>
              ))}
            </div>
          )}
        </div>

        {/* Footer */}
        <div className="flex justify-end p-4 border-t border-dark-border">
          <button
            onClick={onClose}
            className="px-4 py-2 bg-dark-bg-hover hover:bg-dark-border text-dark-text rounded-lg transition-colors"
          >
            Close
          </button>
        </div>
      </div>
    </div>
  );
}
