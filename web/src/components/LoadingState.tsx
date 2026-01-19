import { ReactNode } from 'react';

interface LoadingStateProps {
  loading: boolean;
  empty: boolean;
  skeleton: ReactNode;
  emptyState: ReactNode;
  children: ReactNode;
}

/**
 * Handles the common loading/empty/data rendering pattern
 *
 * @param loading - Whether data is currently loading
 * @param empty - Whether the data array is empty
 * @param skeleton - Skeleton component(s) to show while loading
 * @param emptyState - Empty state message to show when no data
 * @param children - Actual content to render when data is available
 */
export function LoadingState({ loading, empty, skeleton, emptyState, children }: LoadingStateProps) {
  if (loading) {
    return <>{skeleton}</>;
  }

  if (empty) {
    return <>{emptyState}</>;
  }

  return <>{children}</>;
}
