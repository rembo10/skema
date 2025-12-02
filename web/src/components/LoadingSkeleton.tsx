export function Skeleton({ className = '' }: { className?: string }) {
  return (
    <div
      className={`animate-pulse bg-dark-bg-subtle rounded ${className}`}
      aria-label="Loading..."
    />
  );
}

export function CardSkeleton() {
  return (
    <div className="card p-6">
      <div className="flex items-center justify-between">
        <div className="flex items-center gap-3 flex-1">
          <Skeleton className="w-12 h-12 rounded-lg" />
          <div className="flex-1">
            <Skeleton className="h-4 w-20 mb-2" />
            <Skeleton className="h-8 w-32" />
          </div>
        </div>
      </div>
    </div>
  );
}

export function StatCardSkeleton() {
  return (
    <div className="card p-5">
      <div className="flex items-center justify-between">
        <div className="flex-1">
          <Skeleton className="h-4 w-16 mb-1" />
          <Skeleton className="h-7 w-24" />
        </div>
        <Skeleton className="w-10 h-10 rounded-lg" />
      </div>
    </div>
  );
}

export function PageLoadingSkeleton() {
  return (
    <div className="flex items-center justify-center h-64">
      <div className="flex flex-col items-center gap-4">
        <div className="w-8 h-8 border-2 border-dark-accent border-t-transparent rounded-full animate-spin" />
        <span className="text-dark-text-secondary text-sm">Loading...</span>
      </div>
    </div>
  );
}

export function DashboardSkeleton() {
  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex items-start justify-between">
        <div className="flex-1">
          <Skeleton className="h-9 w-40 mb-2" />
          <Skeleton className="h-5 w-64" />
        </div>
        <Skeleton className="h-10 w-32 rounded-lg" />
      </div>

      {/* Primary stats grid */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        <CardSkeleton />
        <CardSkeleton />
        <CardSkeleton />
      </div>

      {/* Secondary stats */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        <StatCardSkeleton />
        <StatCardSkeleton />
        <StatCardSkeleton />
      </div>

      {/* Metadata accuracy */}
      <div className="card p-6">
        <div className="flex items-start justify-between mb-4">
          <Skeleton className="h-6 w-48" />
        </div>
        <div className="flex items-center">
          <div className="flex-1">
            <div className="flex items-baseline gap-3 mb-6">
              <Skeleton className="h-14 w-32" />
              <Skeleton className="h-5 w-40" />
            </div>
            <Skeleton className="h-2 w-full rounded-full" />
          </div>
        </div>
      </div>

      {/* Recently followed artists */}
      <div className="card p-6">
        <Skeleton className="h-6 w-48 mb-4" />
        <div className="grid grid-cols-2 sm:grid-cols-3 md:grid-cols-4 lg:grid-cols-6 gap-4">
          {[...Array(6)].map((_, i) => (
            <div key={i} className="flex flex-col items-center">
              <Skeleton className="w-full aspect-square rounded-lg mb-2" />
              <Skeleton className="h-4 w-3/4" />
            </div>
          ))}
        </div>
      </div>
    </div>
  );
}
