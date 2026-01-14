import { ChevronLeft, ChevronRight } from 'lucide-react';

interface PaginationControlsProps {
  offset: number;
  limit: number;
  total: number;
  onPrevPage: () => void;
  onNextPage: () => void;
  itemName?: string;
  filteredCount?: number;
}

export function PaginationControls({
  offset,
  limit,
  total,
  onPrevPage,
  onNextPage,
  itemName = 'items',
  filteredCount,
}: PaginationControlsProps) {
  // Don't show pagination if total is 0 or not yet loaded
  if (total === 0 || total <= limit) {
    return null;
  }

  const start = offset + 1;
  const end = Math.min(offset + limit, total);
  const hasPrev = offset > 0;
  const hasNext = offset + limit < total;

  return (
    <div className="flex items-center justify-between border-t border-dark-border pt-4">
      <div className="text-sm text-dark-text-secondary">
        Showing {start}-{end} of {total} {itemName}
        {filteredCount !== undefined && filteredCount < total && ` (${filteredCount} filtered)`}
      </div>
      <div className="flex gap-2">
        <button
          onClick={onPrevPage}
          disabled={!hasPrev}
          className="p-2 rounded-lg border border-dark-border hover:bg-dark-bg-subtle transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
          title="Previous page"
        >
          <ChevronLeft size={16} />
        </button>
        <button
          onClick={onNextPage}
          disabled={!hasNext}
          className="p-2 rounded-lg border border-dark-border hover:bg-dark-bg-subtle transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
          title="Next page"
        >
          <ChevronRight size={16} />
        </button>
      </div>
    </div>
  );
}
