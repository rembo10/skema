import { useState, useEffect } from 'react';
import { X, Folder, File, Home, ArrowUp, Loader2, AlertCircle } from 'lucide-react';
import { api } from '../lib/api';
import type { FilesystemEntry } from '../types/api';

interface FileBrowserModalProps {
  isOpen: boolean;
  onClose: () => void;
  onSelect: (path: string) => void;
  initialPath?: string;
  type?: 'directory' | 'file';
  title?: string;
}

export function FileBrowserModal({
  isOpen,
  onClose,
  onSelect,
  initialPath = '/',
  type = 'directory',
  title = 'Select Directory',
}: FileBrowserModalProps) {
  const [currentPath, setCurrentPath] = useState(initialPath);
  const [entries, setEntries] = useState<FilesystemEntry[]>([]);
  const [parentPath, setParentPath] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (isOpen) {
      loadDirectory(initialPath);
    }
  }, [isOpen, initialPath]);

  const loadDirectory = async (path: string) => {
    setLoading(true);
    setError(null);

    try {
      const response = await api.browseFilesystem(path);

      if (response.error) {
        setError(response.error);
        setEntries([]);
      } else {
        setCurrentPath(response.path);
        setParentPath(response.parent);
        setEntries(response.entries);
      }
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to load directory');
      setEntries([]);
    } finally {
      setLoading(false);
    }
  };

  const handleEntryClick = (entry: FilesystemEntry) => {
    if (entry.is_directory) {
      loadDirectory(entry.path);
    } else if (type === 'file') {
      // If selecting a file, allow clicking on it
      onSelect(entry.path);
      onClose();
    }
  };

  const handleGoUp = () => {
    if (parentPath) {
      loadDirectory(parentPath);
    }
  };

  const handleGoHome = () => {
    loadDirectory('~');
  };

  const handleSelectCurrent = () => {
    onSelect(currentPath);
    onClose();
  };

  if (!isOpen) return null;

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/50">
      <div className="bg-dark-bg-elevated border border-dark-border rounded-lg shadow-xl w-full max-w-2xl max-h-[80vh] flex flex-col">
        {/* Header */}
        <div className="flex items-center justify-between p-4 border-b border-dark-border">
          <h2 className="text-lg font-semibold text-dark-text">{title}</h2>
          <button
            onClick={onClose}
            className="text-dark-text-secondary hover:text-dark-text transition-colors"
          >
            <X className="w-5 h-5" />
          </button>
        </div>

        {/* Current path and navigation */}
        <div className="p-4 border-b border-dark-border bg-dark-bg">
          <div className="flex items-center gap-2 mb-3">
            <button
              onClick={handleGoHome}
              className="btn-secondary px-3 py-1.5 text-sm flex items-center gap-1.5"
              title="Go to home directory"
            >
              <Home className="w-4 h-4" />
              Home
            </button>
            <button
              onClick={handleGoUp}
              disabled={!parentPath}
              className="btn-secondary px-3 py-1.5 text-sm flex items-center gap-1.5 disabled:opacity-50 disabled:cursor-not-allowed"
              title="Go up one level"
            >
              <ArrowUp className="w-4 h-4" />
              Up
            </button>
          </div>
          <div className="font-mono text-sm text-dark-text bg-dark-bg-hover px-3 py-2 rounded border border-dark-border">
            {currentPath}
          </div>
        </div>

        {/* Directory listing */}
        <div className="flex-1 overflow-y-auto p-4">
          {loading ? (
            <div className="flex items-center justify-center py-12">
              <Loader2 className="w-8 h-8 animate-spin text-dark-accent" />
            </div>
          ) : error ? (
            <div className="flex items-center gap-3 p-4 bg-red-500/10 border border-red-500/30 rounded text-red-400">
              <AlertCircle className="w-5 h-5 flex-shrink-0" />
              <div>
                <p className="font-medium">Error loading directory</p>
                <p className="text-sm mt-1">{error}</p>
              </div>
            </div>
          ) : entries.length === 0 ? (
            <div className="text-center py-12 text-dark-text-secondary">
              <p>This directory is empty</p>
            </div>
          ) : (
            <div className="space-y-1">
              {entries.map((entry) => {
                const isSelectable = type === 'file' || entry.is_directory;
                return (
                  <button
                    key={entry.path}
                    onClick={() => handleEntryClick(entry)}
                    disabled={!entry.readable || (type === 'directory' && !entry.is_directory)}
                    className={`w-full flex items-center gap-3 px-3 py-2 rounded transition-colors text-left ${
                      isSelectable && entry.readable
                        ? 'hover:bg-dark-bg-hover text-dark-text'
                        : 'text-dark-text-tertiary cursor-not-allowed opacity-50'
                    }`}
                  >
                    {entry.is_directory ? (
                      <Folder className="w-5 h-5 text-blue-400 flex-shrink-0" />
                    ) : (
                      <File className="w-5 h-5 text-dark-text-tertiary flex-shrink-0" />
                    )}
                    <div className="flex-1 min-w-0">
                      <div className="font-mono text-sm truncate">{entry.name}</div>
                      {!entry.is_directory && (
                        <div className="text-xs text-dark-text-tertiary">
                          {formatFileSize(entry.size)}
                        </div>
                      )}
                    </div>
                    {!entry.readable && (
                      <span className="text-xs text-red-400">No access</span>
                    )}
                  </button>
                );
              })}
            </div>
          )}
        </div>

        {/* Footer */}
        <div className="p-4 border-t border-dark-border flex items-center justify-between">
          <div className="text-sm text-dark-text-secondary">
            {type === 'directory' ? 'Click a folder to navigate, or select current directory' : 'Click a file or folder to select'}
          </div>
          <div className="flex gap-2">
            <button onClick={onClose} className="btn-secondary">
              Cancel
            </button>
            {type === 'directory' && (
              <button onClick={handleSelectCurrent} className="btn-primary">
                Select Current Directory
              </button>
            )}
          </div>
        </div>
      </div>
    </div>
  );
}

function formatFileSize(bytes: number | null | undefined): string {
  if (bytes === null || bytes === undefined || isNaN(bytes)) {
    return '0 B';
  }

  const units = ['B', 'KB', 'MB', 'GB', 'TB'];
  let size = bytes;
  let unitIndex = 0;

  while (size >= 1024 && unitIndex < units.length - 1) {
    size /= 1024;
    unitIndex++;
  }

  return `${size.toFixed(unitIndex === 0 ? 0 : 1)} ${units[unitIndex]}`;
}
