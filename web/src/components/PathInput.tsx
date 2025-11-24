import { useState, useRef, useEffect } from 'react';
import { FolderOpen, X } from 'lucide-react';
import { FileBrowserModal } from './FileBrowserModal';
import { api } from '../lib/api';
import type { FilesystemEntry } from '../types/api';

interface PathInputProps {
  value: string;
  onChange: (value: string) => void;
  placeholder?: string;
  className?: string;
  id?: string;
  type?: 'directory' | 'file';
  label?: string;
  description?: string;
}

// localStorage key for recent paths
const RECENT_PATHS_KEY = 'skema_recent_paths';
const MAX_RECENT_PATHS = 10;

function getRecentPaths(): string[] {
  try {
    const stored = localStorage.getItem(RECENT_PATHS_KEY);
    return stored ? JSON.parse(stored) : [];
  } catch {
    return [];
  }
}

function addRecentPath(path: string): void {
  if (!path || path.trim().length === 0) return;

  const recent = getRecentPaths();
  const filtered = recent.filter(p => p !== path);
  const updated = [path, ...filtered].slice(0, MAX_RECENT_PATHS);

  try {
    localStorage.setItem(RECENT_PATHS_KEY, JSON.stringify(updated));
  } catch {
    // Ignore storage errors
  }
}

// Get parent directory from a path
function getParentPath(path: string): string {
  if (!path || path === '/') return '/';
  const normalized = path.replace(/\/+$/, ''); // Remove trailing slashes
  const lastSlash = normalized.lastIndexOf('/');
  if (lastSlash === 0) return '/';
  if (lastSlash === -1) return '/';
  return normalized.substring(0, lastSlash);
}

export function PathInput({
  value,
  onChange,
  placeholder,
  className = '',
  id,
  type = 'directory',
  label,
  description,
}: PathInputProps) {
  const [showSuggestions, setShowSuggestions] = useState(false);
  const [showBrowser, setShowBrowser] = useState(false);
  const [recentPaths, setRecentPaths] = useState<string[]>([]);
  const [suggestions, setSuggestions] = useState<FilesystemEntry[]>([]);
  const [selectedIndex, setSelectedIndex] = useState(0);
  const [loadingSuggestions, setLoadingSuggestions] = useState(false);
  const inputRef = useRef<HTMLInputElement>(null);
  const inputWrapperRef = useRef<HTMLDivElement>(null);

  // Load recent paths on mount
  useEffect(() => {
    setRecentPaths(getRecentPaths());
  }, []);

  // Load filesystem suggestions when value changes
  useEffect(() => {
    const loadSuggestions = async () => {
      if (!value || value.trim().length === 0) {
        setSuggestions([]);
        return;
      }

      // Determine which directory to browse
      let pathToBrowse = value;

      // If path ends with /, browse that directory
      // Otherwise, browse the parent directory
      if (!value.endsWith('/')) {
        pathToBrowse = getParentPath(value);
      }

      try {
        setLoadingSuggestions(true);
        const response = await api.browseFilesystem(pathToBrowse);

        if (!response.error && response.entries) {
          // Filter to only directories if type is directory
          const filtered = type === 'directory'
            ? response.entries.filter(e => e.is_directory)
            : response.entries;

          // If user is typing a partial name, filter by that
          if (!value.endsWith('/')) {
            const lastPart = value.substring(value.lastIndexOf('/') + 1).toLowerCase();
            if (lastPart) {
              setSuggestions(filtered.filter(e =>
                e.name.toLowerCase().startsWith(lastPart)
              ));
            } else {
              setSuggestions(filtered);
            }
          } else {
            setSuggestions(filtered);
          }
        } else {
          setSuggestions([]);
        }
      } catch {
        setSuggestions([]);
      } finally {
        setLoadingSuggestions(false);
      }
    };

    // Only load suggestions if we're showing them
    if (showSuggestions) {
      loadSuggestions();
    }
  }, [value, showSuggestions, type]);

  // Close suggestions when clicking outside
  useEffect(() => {
    function handleClickOutside(event: MouseEvent) {
      if (inputWrapperRef.current && !inputWrapperRef.current.contains(event.target as Node)) {
        setShowSuggestions(false);
      }
    }

    document.addEventListener('mousedown', handleClickOutside);
    return () => document.removeEventListener('mousedown', handleClickOutside);
  }, []);

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    onChange(e.target.value);
    setShowSuggestions(true);
    setSelectedIndex(0);
  };

  const handleSuggestionClick = (entry: FilesystemEntry) => {
    onChange(entry.path);
    addRecentPath(entry.path);
    setRecentPaths(getRecentPaths());
    setShowSuggestions(false);
    inputRef.current?.focus();
  };

  const handleKeyDown = (e: React.KeyboardEvent<HTMLInputElement>) => {
    if (!showSuggestions || suggestions.length === 0) return;

    switch (e.key) {
      case 'ArrowDown':
        e.preventDefault();
        setSelectedIndex(prev => (prev + 1) % suggestions.length);
        break;
      case 'ArrowUp':
        e.preventDefault();
        setSelectedIndex(prev => (prev - 1 + suggestions.length) % suggestions.length);
        break;
      case 'Enter':
        e.preventDefault();
        if (suggestions[selectedIndex]) {
          handleSuggestionClick(suggestions[selectedIndex]);
        }
        break;
      case 'Escape':
        e.preventDefault();
        setShowSuggestions(false);
        break;
      case 'Tab':
      case 'ArrowRight':
        // Accept the preview
        if (suggestions[selectedIndex]) {
          e.preventDefault();
          onChange(suggestions[selectedIndex].path);
          setSelectedIndex(0);
        }
        break;
    }
  };

  // Get preview text based on selected suggestion
  const getPreviewText = (): string => {
    if (!showSuggestions || suggestions.length === 0 || !suggestions[selectedIndex]) {
      return '';
    }

    const selectedPath = suggestions[selectedIndex].path;

    // If the current value is empty or the selected path doesn't start with it, show nothing
    if (!value || !selectedPath.startsWith(value)) {
      return '';
    }

    // Return only the part that extends beyond the current value
    return selectedPath.substring(value.length);
  };

  const previewText = getPreviewText();

  const handleClear = () => {
    onChange('');
    inputRef.current?.focus();
  };

  const handleBrowse = () => {
    setShowBrowser(true);
    setShowSuggestions(false);
  };

  const handleBrowserSelect = (path: string) => {
    onChange(path);
    addRecentPath(path);
    setRecentPaths(getRecentPaths());
    setShowBrowser(false);
    setShowSuggestions(false); // Don't show suggestions after browsing
    inputRef.current?.focus();
  };

  return (
    <div className="max-w-md">
      {label && (
        <label htmlFor={id} className="block text-sm font-medium text-dark-text mb-2">
          {label}
        </label>
      )}
      <div className="relative flex gap-2" ref={inputWrapperRef}>
        <div className="relative flex-1">
          {/* Background "input" showing the preview text */}
          <div className="input w-full pr-8 absolute inset-0 pointer-events-none overflow-hidden">
            <span className="text-dark-text whitespace-pre">
              <span style={{ color: 'transparent' }}>{value}</span>
              <span style={{ opacity: 0.5 }}>{previewText}</span>
            </span>
          </div>
          {/* Actual input with transparent background */}
          <input
            ref={inputRef}
            type="text"
            id={id}
            value={value}
            onChange={handleInputChange}
            onKeyDown={handleKeyDown}
            className={`input w-full pr-8 ${className} relative bg-transparent`}
            placeholder={placeholder || `Enter ${type} path...`}
            autoComplete="off"
            style={{ position: 'relative', zIndex: 1 }}
          />
          {value && (
            <button
              type="button"
              onClick={handleClear}
              className="absolute right-2 top-1/2 -translate-y-1/2 text-dark-text-tertiary hover:text-dark-text transition-colors z-10"
              title="Clear"
            >
              <X className="w-4 h-4" />
            </button>
          )}
        </div>
        <button
          type="button"
          onClick={handleBrowse}
          className="btn-secondary px-3 flex items-center gap-2"
          title={`Browse for ${type}`}
        >
          <FolderOpen className="w-4 h-4" />
          <span className="hidden sm:inline">Browse</span>
        </button>

        {/* Autocomplete suggestions - positioned absolutely within this wrapper */}
        {showSuggestions && suggestions.length > 0 && (
          <div className="absolute top-full left-0 right-16 mt-1 z-10 bg-dark-bg-elevated border border-dark-border rounded-lg shadow-lg max-h-60 overflow-y-auto">
            <div className="py-1">
              {suggestions.map((entry, index) => (
                <button
                  key={entry.path}
                  type="button"
                  onClick={() => handleSuggestionClick(entry)}
                  className={`w-full text-left px-4 py-2 text-sm transition-colors ${
                    index === selectedIndex
                      ? 'bg-dark-accent/10 text-dark-text'
                      : 'text-dark-text-secondary hover:bg-dark-bg-hover hover:text-dark-text'
                  }`}
                >
                  <span className="font-mono">{entry.name}</span>
                </button>
              ))}
            </div>
          </div>
        )}
      </div>
      {description && (
        <p className="mt-2 text-sm text-dark-text-secondary">{description}</p>
      )}

      {/* File browser modal */}
      <FileBrowserModal
        isOpen={showBrowser}
        onClose={() => setShowBrowser(false)}
        onSelect={handleBrowserSelect}
        initialPath={value || '/'}
        type={type}
        title={type === 'directory' ? 'Select Directory' : 'Select File or Directory'}
      />
    </div>
  );
}
