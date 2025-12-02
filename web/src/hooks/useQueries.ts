import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { api } from '../lib/api';
import { queryKeys } from '../lib/queryClient';
import toast from 'react-hot-toast';
import type { Config } from '../types/api';

// Stats
export function useStats() {
  return useQuery({
    queryKey: queryKeys.stats,
    queryFn: () => api.getStats(),
  });
}

// Config
export function useConfig() {
  return useQuery({
    queryKey: queryKeys.config,
    queryFn: () => api.getConfig(),
  });
}

export function useUpdateConfig() {
  const queryClient = useQueryClient();
  
  return useMutation({
    mutationFn: (updates: Partial<Config>) => api.updateConfig(updates),
    onSuccess: (updatedConfig: Config) => {
      queryClient.setQueryData(queryKeys.config, updatedConfig);
      toast.success('Configuration saved');
    },
    onError: () => {
      toast.error('Failed to save configuration');
    },
  });
}

// Artists
export function useArtists(followed?: boolean) {
  return useQuery({
    queryKey: queryKeys.artists.list({ followed }),
    queryFn: () => api.getCatalogArtists(followed),
  });
}

export function useFollowArtist() {
  const queryClient = useQueryClient();
  
  return useMutation({
    mutationFn: (artist: Parameters<typeof api.createCatalogArtist>[0]) => 
      api.createCatalogArtist(artist),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: queryKeys.artists.all });
      toast.success('Artist followed');
    },
    onError: () => {
      toast.error('Failed to follow artist');
    },
  });
}

export function useUnfollowArtist() {
  const queryClient = useQueryClient();
  
  return useMutation({
    mutationFn: (artistId: number) => api.updateCatalogArtist(artistId, false),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: queryKeys.artists.all });
      toast.success('Artist unfollowed');
    },
    onError: () => {
      toast.error('Failed to unfollow artist');
    },
  });
}

// Albums
export function useAlbums(filters?: { wanted?: boolean; artistId?: number }) {
  return useQuery({
    queryKey: queryKeys.albums.list(filters),
    queryFn: () => api.getCatalogAlbums(filters?.wanted, filters?.artistId),
  });
}

export function useUpdateAlbum() {
  const queryClient = useQueryClient();
  
  return useMutation({
    mutationFn: ({ albumId, wanted, qualityProfileId }: { 
      albumId: number; 
      wanted: boolean; 
      qualityProfileId?: number | null;
    }) => api.updateCatalogAlbum(albumId, wanted, qualityProfileId),
    onSuccess: (_updatedAlbum, variables) => {
      queryClient.invalidateQueries({ queryKey: queryKeys.albums.all });
      toast.success(variables.wanted ? 'Added to wanted list' : 'Removed from wanted list');
    },
    onError: () => {
      toast.error('Failed to update album');
    },
  });
}

// Quality Profiles
export function useQualityProfiles() {
  return useQuery({
    queryKey: queryKeys.qualityProfiles.all,
    queryFn: () => api.getQualityProfiles(),
  });
}

export function useDefaultQualityProfile() {
  return useQuery({
    queryKey: queryKeys.qualityProfiles.default,
    queryFn: () => api.getDefaultQualityProfile(),
  });
}

// Grouped Diffs
export function useGroupedDiffs() {
  return useQuery({
    queryKey: queryKeys.diffs.grouped,
    queryFn: () => api.getGroupedDiffs(),
  });
}

// Downloads
export function useDownloads() {
  return useQuery({
    queryKey: queryKeys.downloads.all,
    queryFn: () => api.getAllDownloads(),
  });
}

// Clusters
export function useClusters() {
  return useQuery({
    queryKey: queryKeys.clusters.all,
    queryFn: () => api.getClusters(),
  });
}

export function useCluster(clusterId: number) {
  return useQuery({
    queryKey: queryKeys.clusters.detail(clusterId),
    queryFn: () => api.getCluster(clusterId),
    enabled: clusterId > 0,
  });
}

export function useClusterCandidates(clusterId: number) {
  return useQuery({
    queryKey: queryKeys.clusters.candidates(clusterId),
    queryFn: () => api.getCandidateReleases(clusterId),
    enabled: clusterId > 0,
  });
}

// Acquisition Sources
export function useAcquisitionSources() {
  return useQuery({
    queryKey: queryKeys.acquisitionSources.all,
    queryFn: () => api.getAcquisitionSources(),
  });
}

// Metadata Changes
export function useMetadataChanges() {
  return useQuery({
    queryKey: queryKeys.metadataChanges.all,
    queryFn: () => api.getMetadataChanges(),
  });
}
