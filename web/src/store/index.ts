import { create } from 'zustand';
import type { CandidateRelease, ClusterTrack } from '../types/api';

export interface CurrentStatus {
  type: 'in_progress' | 'success' | 'error';
  message: string;
  progress?: {
    current: number;
    total: number;
  };
}

export type ConnectionStatus = 'connected' | 'connecting' | 'disconnected' | 'error';

interface AppStore {
  // Authentication state
  authEnabled: boolean | null;  // null = not checked yet, true = enabled, false = disabled
  setAuthEnabled: (enabled: boolean) => void;

  // Current status (real-time operation status)
  currentStatus: CurrentStatus | null;
  setCurrentStatus: (status: CurrentStatus | null) => void;

  // Connection status
  connectionStatus: ConnectionStatus;
  setConnectionStatus: (status: ConnectionStatus) => void;

  // Cluster candidates cache (per cluster)
  clusterCandidates: Record<number, CandidateRelease[]>;
  setClusterCandidates: (clusterId: number, candidates: CandidateRelease[]) => void;

  // Cluster tracks cache (per cluster)
  clusterTracks: Record<number, ClusterTrack[]>;
  setClusterTracks: (clusterId: number, tracks: ClusterTrack[]) => void;
}

export const useAppStore = create<AppStore>((set) => ({
  // Authentication state
  authEnabled: null,
  setAuthEnabled: (enabled) => set({ authEnabled: enabled }),

  // Current status
  currentStatus: null,
  setCurrentStatus: (status) => set({ currentStatus: status }),

  // Connection status
  connectionStatus: 'disconnected',
  setConnectionStatus: (status) => set({ connectionStatus: status }),

  // Cluster candidates cache
  clusterCandidates: {},

  setClusterCandidates: (clusterId, candidates) =>
    set((state) => ({
      clusterCandidates: {
        ...state.clusterCandidates,
        [clusterId]: candidates,
      },
    })),

  // Cluster tracks cache
  clusterTracks: {},

  setClusterTracks: (clusterId, tracks) =>
    set((state) => ({
      clusterTracks: {
        ...state.clusterTracks,
        [clusterId]: tracks,
      },
    })),
}));
