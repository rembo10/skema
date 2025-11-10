import { useEffect, useState, useRef } from 'react';
import toast from 'react-hot-toast';
import { api } from '../lib/api';
import { useAppStore } from '../store';
import type { GroupedDiff, MetadataDiff, MetadataChange } from '../types/api';
import { LibraryNav } from '../components/LibraryNav';

type Tab = 'diffs' | 'history';

export default function MetadataDiffs() {
  const [activeTab, setActiveTab] = useState<Tab>('diffs');
  const [changes, setChanges] = useState<MetadataChange[]>([]);
  const [loadingChanges, setLoadingChanges] = useState(false);
  const [loadingDiffs, setLoadingDiffs] = useState(false);
  const [applying, setApplying] = useState(false);
  const [applyingGroup, setApplyingGroup] = useState<string | null>(null);
  const [selectedDiffIds, setSelectedDiffIds] = useState<Set<number>>(new Set());

  // Read from store
  const groupedDiffs = useAppStore((state) => state.groupedDiffs);
  const groupedDiffsStale = useAppStore((state) => state.groupedDiffsStale);
  const setGroupedDiffs = useAppStore((state) => state.setGroupedDiffs);

  // Load diffs on mount if not loaded or stale
  useEffect(() => {
    if (groupedDiffs === null || groupedDiffsStale) {
      loadDiffs();
    }
  }, []);

  async function loadDiffs() {
    setLoadingDiffs(true);
    try {
      console.log('[MetadataDiffs] Loading diffs...');
      const diffs = await api.getGroupedDiffs();
      setGroupedDiffs(diffs);
      console.log('[MetadataDiffs] Loaded', diffs.length, 'diff groups');
    } catch (error: any) {
      // Don't log or toast 401 errors - they're expected when auth is required
      if (!error.isAuthError) {
        console.error('[MetadataDiffs] Failed to load diffs:', error);
        toast.error('Failed to load metadata diffs');
      }
      // 401 will trigger redirect via global unauthorized event handler
    } finally {
      setLoadingDiffs(false);
    }
  }

  // Load changes history when switching to history tab
  useEffect(() => {
    if (activeTab === 'history') {
      loadChangesHistory();
    }
  }, [activeTab]);

  async function loadChangesHistory() {
    setLoadingChanges(true);
    try {
      const history = await api.getMetadataChanges();
      setChanges(history);
    } catch (error: any) {
      // Don't log or toast 401 errors - they're expected when auth is required
      if (!error.isAuthError) {
        console.error('Failed to load changes history:', error);
        toast.error('Failed to load changes history');
      }
      // 401 will trigger redirect via global unauthorized event handler
    } finally {
      setLoadingChanges(false);
    }
  }

  async function applySelected() {
    if (selectedDiffIds.size === 0) {
      toast.error('No diffs selected');
      return;
    }

    setApplying(true);
    try {
      await api.applyMetadataChanges(Array.from(selectedDiffIds));
      setSelectedDiffIds(new Set());
      toast.success('Applying metadata changes');
      // Progress will be shown via SSE events in the status line
      // Completion/failure toasts will be shown by the SSE handlers
    } catch (error) {
      console.error('Failed to apply metadata changes:', error);
      toast.error('Failed to apply metadata changes');
    } finally {
      setApplying(false);
    }
  }

  async function applyGroup(group: GroupedDiff) {
    const key = `${group.field_name}:${group.file_value}:${group.mb_value}`;
    setApplyingGroup(key);
    try {
      const diffIds = group.diffs.map(d => d.id);
      await api.applyMetadataChanges(diffIds);
      toast.success('Applying metadata changes');
      // Progress will be shown via SSE events in the status line
      // Completion/failure toasts will be shown by the SSE handlers
    } catch (error) {
      console.error('Failed to apply metadata changes:', error);
      toast.error('Failed to apply metadata changes');
    } finally {
      setApplyingGroup(null);
    }
  }

  async function revertChange(changeId: number) {
    try {
      await api.revertMetadataChange(changeId);
      await loadChangesHistory();
      toast.success('Change reverted successfully');
    } catch (error) {
      console.error('Failed to revert change:', error);
      toast.error('Failed to revert change');
    }
  }

  function toggleDiffSelection(diffId: number) {
    const newSelection = new Set(selectedDiffIds);
    if (newSelection.has(diffId)) {
      newSelection.delete(diffId);
    } else {
      newSelection.add(diffId);
    }
    setSelectedDiffIds(newSelection);
  }

  function toggleGroupSelection(group: GroupedDiff) {
    const groupDiffIds = group.diffs.map(d => d.id);
    const allSelected = groupDiffIds.every(id => selectedDiffIds.has(id));

    const newSelection = new Set(selectedDiffIds);
    if (allSelected) {
      // Deselect all in group
      groupDiffIds.forEach(id => newSelection.delete(id));
    } else {
      // Select all in group
      groupDiffIds.forEach(id => newSelection.add(id));
    }
    setSelectedDiffIds(newSelection);
  }

  function toggleSelectAll() {
    const allDiffIds = getAllDiffIds();
    if (selectedDiffIds.size === allDiffIds.length) {
      // Deselect all
      setSelectedDiffIds(new Set());
    } else {
      // Select all
      setSelectedDiffIds(new Set(allDiffIds));
    }
  }

  function getAllDiffIds(): number[] {
    if (!groupedDiffs) return [];
    return groupedDiffs.flatMap(group => group.diffs.map(diff => diff.id));
  }

  // Show loading state while diffs are being fetched
  const showDiffsLoading = loadingDiffs || (groupedDiffs === null && activeTab === 'diffs');

  return (
    <div className="space-y-6 animate-fade-in">
      <LibraryNav />

      {/* Header with tabs */}
      <div>
        <h1 className="text-3xl font-bold text-dark-text">Metadata Management</h1>
        <p className="mt-2 text-dark-text-secondary">
          Review and apply metadata corrections from MusicBrainz
        </p>

        {/* Tabs */}
        <div className="mt-6 border-b border-dark-border">
          <nav className="-mb-px flex space-x-8">
            <button
              onClick={() => setActiveTab('diffs')}
              className={`${
                activeTab === 'diffs'
                  ? 'border-dark-accent text-dark-accent'
                  : 'border-transparent text-dark-text-secondary hover:text-dark-text hover:border-dark-border-bright'
              } whitespace-nowrap pb-4 px-1 border-b-2 font-medium text-sm transition-colors duration-200`}
            >
              Pending Diffs
              {groupedDiffs && groupedDiffs.length > 0 && (
                <span className="ml-2 inline-flex items-center px-2.5 py-0.5 rounded-lg text-xs font-medium bg-dark-accent/20 text-dark-accent border border-dark-accent/30">
                  {groupedDiffs.reduce((acc, g) => acc + g.count, 0)}
                </span>
              )}
            </button>
            <button
              onClick={() => setActiveTab('history')}
              className={`${
                activeTab === 'history'
                  ? 'border-dark-accent text-dark-accent'
                  : 'border-transparent text-dark-text-secondary hover:text-dark-text hover:border-dark-border-bright'
              } whitespace-nowrap pb-4 px-1 border-b-2 font-medium text-sm transition-colors duration-200`}
            >
              Change History
            </button>
          </nav>
        </div>
      </div>

      {/* Content */}
      {activeTab === 'diffs' ? (
        showDiffsLoading ? (
          <div className="flex items-center justify-center h-64">
            <div className="text-dark-text-secondary">Loading diffs...</div>
          </div>
        ) : (
          <DiffsView
            groupedDiffs={groupedDiffs || []}
            selectedDiffIds={selectedDiffIds}
            applying={applying}
            applyingGroup={applyingGroup}
            onToggleSelection={toggleDiffSelection}
            onToggleGroupSelection={toggleGroupSelection}
            onToggleSelectAll={toggleSelectAll}
            onApplySelected={applySelected}
            onApplyGroup={applyGroup}
          />
        )
      ) : (
        <HistoryView changes={changes} loading={loadingChanges} onRevert={revertChange} />
      )}
    </div>
  );
}

function DiffsView({
  groupedDiffs,
  selectedDiffIds,
  applying,
  applyingGroup,
  onToggleSelection,
  onToggleGroupSelection,
  onToggleSelectAll,
  onApplySelected,
  onApplyGroup,
}: {
  groupedDiffs: GroupedDiff[];
  selectedDiffIds: Set<number>;
  applying: boolean;
  applyingGroup: string | null;
  onToggleSelection: (diffId: number) => void;
  onToggleGroupSelection: (group: GroupedDiff) => void;
  onToggleSelectAll: () => void;
  onApplySelected: () => void;
  onApplyGroup: (group: GroupedDiff) => void;
}) {
  if (groupedDiffs.length === 0) {
    return (
      <div className="card p-12 text-center">
        <p className="text-dark-text">No metadata differences found.</p>
        <p className="mt-2 text-sm text-dark-text-secondary">
          Run a library scan to identify differences between your files and MusicBrainz.
        </p>
      </div>
    );
  }

  const allDiffIds = groupedDiffs.flatMap(group => group.diffs.map(diff => diff.id));
  const allSelected = allDiffIds.length > 0 && selectedDiffIds.size === allDiffIds.length;
  const someSelected = selectedDiffIds.size > 0 && selectedDiffIds.size < allDiffIds.length;

  return (
    <div>
      {/* Sticky bulk actions panel */}
      <div className="sticky top-0 z-10 mb-4 card p-4 flex items-center justify-between">
        <div className="flex items-center space-x-4">
          <label className="flex items-center cursor-pointer">
            <input
              type="checkbox"
              checked={allSelected}
              ref={input => {
                if (input) {
                  input.indeterminate = someSelected;
                }
              }}
              onChange={onToggleSelectAll}
              className="h-4 w-4 text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg-elevated border-dark-border rounded"
            />
            <span className="ml-2 text-sm text-dark-text">
              {allSelected ? 'Deselect All' : 'Select All'}
            </span>
          </label>
          <span className="text-sm text-dark-text-secondary">
            {selectedDiffIds.size} selected
          </span>
        </div>
        <button
          onClick={onApplySelected}
          disabled={selectedDiffIds.size === 0 || applying}
          className="inline-flex items-center px-4 py-2 rounded-lg text-sm font-medium bg-dark-success text-dark-bg hover:bg-dark-success/90 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-dark-success focus:ring-offset-dark-bg-elevated disabled:opacity-50 disabled:cursor-not-allowed transition-all duration-200"
        >
          {applying ? 'Applying...' : `Apply Selected (${selectedDiffIds.size})`}
        </button>
      </div>

      {/* Grouped diffs */}
      <div className="space-y-4">
        {groupedDiffs.map((group, idx) => (
          <DiffGroup
            key={idx}
            group={group}
            selectedDiffIds={selectedDiffIds}
            applyingGroup={applyingGroup}
            onToggleSelection={onToggleSelection}
            onToggleGroupSelection={onToggleGroupSelection}
            onApplyGroup={onApplyGroup}
          />
        ))}
      </div>
    </div>
  );
}

function DiffGroup({
  group,
  selectedDiffIds,
  applyingGroup,
  onToggleSelection,
  onToggleGroupSelection,
  onApplyGroup,
}: {
  group: GroupedDiff;
  selectedDiffIds: Set<number>;
  applyingGroup: string | null;
  onToggleSelection: (diffId: number) => void;
  onToggleGroupSelection: (group: GroupedDiff) => void;
  onApplyGroup: (group: GroupedDiff) => void;
}) {
  const [expanded, setExpanded] = useState(false);

  const fieldDisplayName = group.field_name
    .split('_')
    .map(word => word.charAt(0).toUpperCase() + word.slice(1))
    .join(' ');

  const groupKey = `${group.field_name}:${group.file_value}:${group.mb_value}`;
  const isApplyingGroup = applyingGroup === groupKey;

  const selectedInGroup = group.diffs.filter(diff => selectedDiffIds.has(diff.id)).length;
  const allInGroupSelected = selectedInGroup === group.diffs.length;
  const someInGroupSelected = selectedInGroup > 0 && selectedInGroup < group.diffs.length;

  return (
    <div className="card overflow-hidden">
      {/* Header */}
      <div className="p-4 sm:p-6 border-b border-dark-border">
        <div className="flex items-start justify-between gap-4">
          <div className="flex-1 min-w-0">
            <div className="flex items-center gap-3 flex-wrap">
              <label className="flex items-center cursor-pointer">
                <input
                  type="checkbox"
                  checked={allInGroupSelected}
                  ref={input => {
                    if (input) {
                      input.indeterminate = someInGroupSelected;
                    }
                  }}
                  onChange={() => onToggleGroupSelection(group)}
                  className="h-4 w-4 text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg-elevated border-dark-border rounded"
                />
              </label>
              <h3 className="text-lg font-medium text-dark-text">{fieldDisplayName}</h3>
              <span className="inline-flex items-center px-2.5 py-0.5 rounded-lg text-xs font-medium bg-dark-accent/20 text-dark-accent border border-dark-accent/30">
                {group.count} {group.count === 1 ? 'file' : 'files'}
              </span>
              {selectedInGroup > 0 && (
                <span className="inline-flex items-center px-2.5 py-0.5 rounded-lg text-xs font-medium bg-dark-success-muted text-dark-success border border-dark-success/30">
                  {selectedInGroup} selected
                </span>
              )}
            </div>
            <div className="mt-4 grid grid-cols-1 md:grid-cols-2 gap-4">
              <div>
                <p className="text-xs font-medium text-dark-text-tertiary uppercase tracking-wider mb-2">Current Value</p>
                <p className="text-sm text-dark-text font-mono bg-dark-bg-subtle px-3 py-2 rounded-lg border border-dark-border">
                  {group.file_value || <span className="text-dark-text-tertiary italic">empty</span>}
                </p>
              </div>
              <div>
                <p className="text-xs font-medium text-dark-text-tertiary uppercase tracking-wider mb-2">MusicBrainz Value</p>
                <p className="text-sm text-dark-success font-mono bg-dark-success-muted px-3 py-2 rounded-lg border border-dark-success/30">
                  {group.mb_value || <span className="text-dark-text-tertiary italic">empty</span>}
                </p>
              </div>
            </div>
          </div>

          <div className="flex flex-col items-end gap-2">
            <button
              onClick={() => onApplyGroup(group)}
              disabled={isApplyingGroup}
              className="inline-flex items-center px-4 py-2 text-sm font-medium rounded-lg bg-dark-success text-dark-bg hover:bg-dark-success/90 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-dark-success focus:ring-offset-dark-bg-elevated disabled:opacity-50 disabled:cursor-not-allowed transition-all duration-200 whitespace-nowrap"
            >
              {isApplyingGroup ? 'Applying...' : `Apply All`}
            </button>
            <button
              onClick={() => setExpanded(!expanded)}
              className="btn-secondary text-sm whitespace-nowrap"
            >
              {expanded ? 'Hide Files' : 'Show Files'}
            </button>
          </div>
        </div>
      </div>

      {/* Expanded file list */}
      {expanded && (
        <div className="bg-dark-bg-subtle px-4 py-3 sm:px-6">
          <div className="space-y-2">
            {group.diffs.map((diff) => (
              <FileRow
                key={diff.id}
                diff={diff}
                isSelected={selectedDiffIds.has(diff.id)}
                onToggle={() => onToggleSelection(diff.id)}
              />
            ))}
          </div>
        </div>
      )}
    </div>
  );
}

function FileRow({
  diff,
  isSelected,
  onToggle,
}: {
  diff: MetadataDiff;
  isSelected: boolean;
  onToggle: () => void;
}) {
  const pathRef = useRef<HTMLDivElement>(null);

  // Scroll to the right (filename) on mount
  useEffect(() => {
    if (pathRef.current) {
      pathRef.current.scrollLeft = pathRef.current.scrollWidth;
    }
  }, []);

  return (
    <div className="flex items-center bg-dark-bg-elevated px-3 py-2 rounded-lg border border-dark-border hover:border-dark-border-bright transition-colors duration-150">
      <label className="flex items-center flex-1 min-w-0 cursor-pointer">
        <input
          type="checkbox"
          checked={isSelected}
          onChange={onToggle}
          className="h-4 w-4 text-dark-accent focus:ring-dark-accent focus:ring-offset-dark-bg-elevated border-dark-border rounded flex-shrink-0"
        />
        <div
          ref={pathRef}
          className="ml-3 text-sm text-dark-text font-mono overflow-x-auto whitespace-nowrap"
          style={{ direction: 'rtl', textAlign: 'left' }}
        >
          <span style={{ direction: 'ltr', unicodeBidi: 'bidi-override' }}>
            {diff.file_path}
          </span>
        </div>
      </label>
    </div>
  );
}

function HistoryView({
  changes,
  loading,
  onRevert,
}: {
  changes: MetadataChange[];
  loading: boolean;
  onRevert: (changeId: number) => void;
}) {
  if (loading) {
    return (
      <div className="flex items-center justify-center h-64">
        <div className="text-dark-text-secondary">Loading...</div>
      </div>
    );
  }

  if (changes.length === 0) {
    return (
      <div className="card p-12 text-center">
        <p className="text-dark-text">No change history found.</p>
        <p className="mt-2 text-sm text-dark-text-secondary">
          Apply some metadata changes to see them appear here.
        </p>
      </div>
    );
  }

  return (
    <div className="card overflow-hidden">
      <ul className="divide-y divide-dark-border">
        {changes.map((change) => {
          const fieldDisplayName = change.field_name
            .split('_')
            .map(word => word.charAt(0).toUpperCase() + word.slice(1))
            .join(' ');

          return (
            <li key={change.id} className={`p-4 sm:p-6 ${change.reverted ? 'opacity-50' : ''}`}>
              <div className="flex items-start justify-between gap-4">
                <div className="flex-1 min-w-0">
                  <div className="flex items-center gap-2 flex-wrap">
                    <h3 className="text-sm font-medium text-dark-text">{fieldDisplayName}</h3>
                    {change.reverted && (
                      <span className="inline-flex items-center px-2.5 py-0.5 rounded-lg text-xs font-medium bg-dark-bg-subtle text-dark-text-tertiary border border-dark-border">
                        Reverted
                      </span>
                    )}
                  </div>
                  <p className="mt-1 text-sm text-dark-text-secondary font-mono truncate">{change.file_path}</p>
                  <div className="mt-3 grid grid-cols-1 md:grid-cols-2 gap-4">
                    <div>
                      <p className="text-xs font-medium text-dark-text-tertiary uppercase tracking-wider mb-2">Old Value</p>
                      <p className="text-sm text-dark-text font-mono bg-dark-bg-subtle px-3 py-2 rounded-lg border border-dark-border">
                        {change.old_value || <span className="text-dark-text-tertiary italic">empty</span>}
                      </p>
                    </div>
                    <div>
                      <p className="text-xs font-medium text-dark-text-tertiary uppercase tracking-wider mb-2">New Value</p>
                      <p className="text-sm text-dark-success font-mono bg-dark-success-muted px-3 py-2 rounded-lg border border-dark-success/30">
                        {change.new_value || <span className="text-dark-text-tertiary italic">empty</span>}
                      </p>
                    </div>
                  </div>
                  <p className="mt-3 text-xs text-dark-text-tertiary">
                    Applied {new Date(change.applied_at).toLocaleString()}
                  </p>
                </div>

                <div>
                  {!change.reverted && (
                    <button
                      onClick={() => onRevert(change.id)}
                      className="btn-secondary text-sm whitespace-nowrap"
                    >
                      Undo
                    </button>
                  )}
                </div>
              </div>
            </li>
          );
        })}
      </ul>
    </div>
  );
}
