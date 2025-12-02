import { QueryClient } from '@tanstack/react-query';

export const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      staleTime: 1000 * 60, // 1 minute
      gcTime: 1000 * 60 * 5, // 5 minutes (formerly cacheTime)
      retry: 1,
      refetchOnWindowFocus: false,
    },
    mutations: {
      retry: 0,
    },
  },
});

// Query keys factory for type-safe query keys
export const queryKeys = {
  // Stats
  stats: ['stats'] as const,
  
  // Config
  config: ['config'] as const,
  
  // Artists
  artists: {
    all: ['artists'] as const,
    list: (filters?: { followed?: boolean }) => ['artists', 'list', filters] as const,
    detail: (id: number) => ['artists', 'detail', id] as const,
  },
  
  // Albums
  albums: {
    all: ['albums'] as const,
    list: (filters?: { wanted?: boolean; artistId?: number }) => ['albums', 'list', filters] as const,
    detail: (id: number) => ['albums', 'detail', id] as const,
  },
  
  // Diffs
  diffs: {
    all: ['diffs'] as const,
    grouped: ['diffs', 'grouped'] as const,
  },
  
  // Clusters
  clusters: {
    all: ['clusters'] as const,
    detail: (id: number) => ['clusters', 'detail', id] as const,
    candidates: (id: number) => ['clusters', 'candidates', id] as const,
  },
  
  // Downloads
  downloads: {
    all: ['downloads'] as const,
    detail: (id: number) => ['downloads', 'detail', id] as const,
  },
  
  // Quality profiles
  qualityProfiles: {
    all: ['qualityProfiles'] as const,
    detail: (id: number) => ['qualityProfiles', 'detail', id] as const,
    default: ['qualityProfiles', 'default'] as const,
  },
  
  // Acquisition sources
  acquisitionSources: {
    all: ['acquisitionSources'] as const,
  },
  
  // Metadata changes
  metadataChanges: {
    all: ['metadataChanges'] as const,
  },
} as const;
