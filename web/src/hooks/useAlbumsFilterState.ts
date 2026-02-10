import { useReducer, useCallback } from 'react';
import { AlbumState } from '../types/api';

export type QuickFilter = 'all' | 'tracked' | 'unacquired' | 'library' | 'upcoming';

export interface AlbumsFilterState {
  sortBy: 'title' | 'artist' | 'date' | 'quality' | 'state';
  sortOrder: 'asc' | 'desc';
  qualityFilter: string[];
  statusFilter: AlbumState[];
  upcomingFilter: boolean;
  quickFilter: QuickFilter;
  searchFilter: string;
}

type AlbumsFilterAction =
  | { type: 'SET_SORT_BY'; payload: AlbumsFilterState['sortBy'] }
  | { type: 'SET_SORT_ORDER'; payload: AlbumsFilterState['sortOrder'] }
  | { type: 'TOGGLE_SORT'; payload: AlbumsFilterState['sortBy'] }
  | { type: 'SET_QUALITY_FILTER'; payload: string[] }
  | { type: 'TOGGLE_QUALITY'; payload: string }
  | { type: 'CLEAR_QUALITY_FILTER' }
  | { type: 'SET_STATUS_FILTER'; payload: AlbumState[] }
  | { type: 'TOGGLE_STATUS'; payload: AlbumState }
  | { type: 'TOGGLE_UPGRADING' }
  | { type: 'CLEAR_STATUS_FILTER' }
  | { type: 'SET_UPCOMING_FILTER'; payload: boolean }
  | { type: 'SET_QUICK_FILTER'; payload: QuickFilter }
  | { type: 'SET_SEARCH_FILTER'; payload: string }
  | { type: 'APPLY_URL_PARAMS'; payload: { stateParam: string | null; releaseDateAfter: string | null } };

const initialState: AlbumsFilterState = {
  sortBy: 'date',
  sortOrder: 'desc',
  qualityFilter: [],
  statusFilter: ['Wanted', 'Searching', 'Downloading', 'Failed', 'IdentificationFailed', 'InLibrary', 'Monitored', 'Upgrading'],
  upcomingFilter: false,
  quickFilter: 'tracked',
  searchFilter: '',
};

function albumsFilterReducer(state: AlbumsFilterState, action: AlbumsFilterAction): AlbumsFilterState {
  switch (action.type) {
    case 'SET_SORT_BY':
      return { ...state, sortBy: action.payload };

    case 'SET_SORT_ORDER':
      return { ...state, sortOrder: action.payload };

    case 'TOGGLE_SORT':
      if (state.sortBy === action.payload) {
        return { ...state, sortOrder: state.sortOrder === 'asc' ? 'desc' : 'asc' };
      }
      return { ...state, sortBy: action.payload, sortOrder: 'desc' };

    case 'SET_QUALITY_FILTER':
      return { ...state, qualityFilter: action.payload };

    case 'TOGGLE_QUALITY': {
      const quality = action.payload;
      const isSelected = state.qualityFilter.includes(quality);
      return {
        ...state,
        qualityFilter: isSelected
          ? state.qualityFilter.filter(q => q !== quality)
          : [...state.qualityFilter, quality],
      };
    }

    case 'CLEAR_QUALITY_FILTER':
      return { ...state, qualityFilter: [] };

    case 'SET_STATUS_FILTER':
      return { ...state, statusFilter: action.payload };

    case 'TOGGLE_STATUS': {
      const status = action.payload;
      const isSelected = state.statusFilter.includes(status);
      return {
        ...state,
        statusFilter: isSelected
          ? state.statusFilter.filter(s => s !== status)
          : [...state.statusFilter, status],
      };
    }

    case 'TOGGLE_UPGRADING': {
      const hasMonitored = state.statusFilter.includes('Monitored');
      const hasUpgrading = state.statusFilter.includes('Upgrading');
      const isSelected = hasMonitored || hasUpgrading;
      if (isSelected) {
        return {
          ...state,
          statusFilter: state.statusFilter.filter(s => s !== 'Monitored' && s !== 'Upgrading'),
        };
      }
      return {
        ...state,
        statusFilter: [...state.statusFilter, 'Monitored', 'Upgrading'],
      };
    }

    case 'CLEAR_STATUS_FILTER':
      return { ...state, statusFilter: [] };

    case 'SET_UPCOMING_FILTER':
      return { ...state, upcomingFilter: action.payload };

    case 'SET_QUICK_FILTER': {
      const filter = action.payload;
      switch (filter) {
        case 'all':
          return {
            ...state,
            quickFilter: filter,
            statusFilter: [],
            upcomingFilter: false,
          };
        case 'tracked':
          return {
            ...state,
            quickFilter: filter,
            statusFilter: ['Wanted', 'Searching', 'Downloading', 'Failed', 'IdentificationFailed', 'InLibrary', 'Monitored', 'Upgrading'],
            upcomingFilter: false,
          };
        case 'unacquired':
          return {
            ...state,
            quickFilter: filter,
            statusFilter: ['Wanted', 'Searching', 'Downloading', 'Failed', 'IdentificationFailed'],
            upcomingFilter: false,
          };
        case 'library':
          return {
            ...state,
            quickFilter: filter,
            statusFilter: ['InLibrary', 'Monitored', 'Upgrading'],
            upcomingFilter: false,
          };
        case 'upcoming':
          return {
            ...state,
            quickFilter: filter,
            statusFilter: [],
            upcomingFilter: true,
          };
        default:
          return state;
      }
    }

    case 'SET_SEARCH_FILTER':
      return { ...state, searchFilter: action.payload };

    case 'APPLY_URL_PARAMS': {
      const { stateParam, releaseDateAfter } = action.payload;

      if (stateParam) {
        const states = stateParam.split(',') as AlbumState[];
        let quickFilter: QuickFilter = 'tracked';

        if (states.includes('Wanted') || states.includes('Failed') || states.includes('IdentificationFailed')) {
          quickFilter = 'unacquired';
        } else if (states.includes('InLibrary') || states.includes('Monitored') || states.includes('Upgrading')) {
          quickFilter = 'library';
        }

        return {
          ...state,
          statusFilter: states,
          upcomingFilter: false,
          quickFilter,
        };
      } else if (releaseDateAfter) {
        return {
          ...state,
          statusFilter: [],
          upcomingFilter: true,
          quickFilter: 'upcoming',
        };
      } else {
        // No URL params - apply default 'tracked' filter
        return {
          ...state,
          statusFilter: ['Wanted', 'Searching', 'Downloading', 'Failed', 'IdentificationFailed', 'InLibrary', 'Monitored', 'Upgrading'],
          upcomingFilter: false,
          quickFilter: 'tracked',
        };
      }
    }

    default:
      return state;
  }
}

export interface UseAlbumsFilterStateResult {
  state: AlbumsFilterState;
  toggleSort: (column: AlbumsFilterState['sortBy']) => void;
  toggleQuality: (quality: string) => void;
  clearQualityFilter: () => void;
  toggleStatus: (status: AlbumState) => void;
  toggleUpgrading: () => void;
  clearStatusFilter: () => void;
  setQuickFilter: (filter: QuickFilter) => void;
  setSearchFilter: (search: string) => void;
  applyUrlParams: (stateParam: string | null, releaseDateAfter: string | null) => void;
}

export function useAlbumsFilterState(): UseAlbumsFilterStateResult {
  const [state, dispatch] = useReducer(albumsFilterReducer, initialState);

  const toggleSort = useCallback((column: AlbumsFilterState['sortBy']) => {
    dispatch({ type: 'TOGGLE_SORT', payload: column });
  }, []);

  const toggleQuality = useCallback((quality: string) => {
    dispatch({ type: 'TOGGLE_QUALITY', payload: quality });
  }, []);

  const clearQualityFilter = useCallback(() => {
    dispatch({ type: 'CLEAR_QUALITY_FILTER' });
  }, []);

  const toggleStatus = useCallback((status: AlbumState) => {
    dispatch({ type: 'TOGGLE_STATUS', payload: status });
  }, []);

  const toggleUpgrading = useCallback(() => {
    dispatch({ type: 'TOGGLE_UPGRADING' });
  }, []);

  const clearStatusFilter = useCallback(() => {
    dispatch({ type: 'CLEAR_STATUS_FILTER' });
  }, []);

  const setQuickFilter = useCallback((filter: QuickFilter) => {
    dispatch({ type: 'SET_QUICK_FILTER', payload: filter });
  }, []);

  const setSearchFilter = useCallback((search: string) => {
    dispatch({ type: 'SET_SEARCH_FILTER', payload: search });
  }, []);

  const applyUrlParams = useCallback((stateParam: string | null, releaseDateAfter: string | null) => {
    dispatch({ type: 'APPLY_URL_PARAMS', payload: { stateParam, releaseDateAfter } });
  }, []);

  return {
    state,
    toggleSort,
    toggleQuality,
    clearQualityFilter,
    toggleStatus,
    toggleUpgrading,
    clearStatusFilter,
    setQuickFilter,
    setSearchFilter,
    applyUrlParams,
  };
}
