import { useState, useCallback } from 'react';

export interface PaginationState {
  offset: number;
  limit: number;
  total: number;
}

export interface UsePaginationResult {
  offset: number;
  totalCount: number;
  currentPage: number;
  totalPages: number;
  hasNextPage: boolean;
  hasPrevPage: boolean;
  setTotalCount: (total: number) => void;
  nextPage: () => void;
  prevPage: () => void;
  goToPage: (page: number) => void;
  resetOffset: () => void;
}

export function usePagination(itemsPerPage: number = 50): UsePaginationResult {
  const [offset, setOffset] = useState(0);
  const [totalCount, setTotalCount] = useState(0);

  const currentPage = Math.floor(offset / itemsPerPage) + 1;
  const totalPages = Math.ceil(totalCount / itemsPerPage);
  const hasNextPage = offset + itemsPerPage < totalCount;
  const hasPrevPage = offset > 0;

  const nextPage = useCallback(() => {
    if (hasNextPage) {
      setOffset(prev => prev + itemsPerPage);
    }
  }, [hasNextPage, itemsPerPage]);

  const prevPage = useCallback(() => {
    if (hasPrevPage) {
      setOffset(prev => Math.max(0, prev - itemsPerPage));
    }
  }, [hasPrevPage, itemsPerPage]);

  const goToPage = useCallback((page: number) => {
    const newOffset = (page - 1) * itemsPerPage;
    if (newOffset >= 0 && newOffset < totalCount) {
      setOffset(newOffset);
    }
  }, [itemsPerPage, totalCount]);

  const resetOffset = useCallback(() => {
    setOffset(0);
  }, []);

  return {
    offset,
    totalCount,
    currentPage,
    totalPages,
    hasNextPage,
    hasPrevPage,
    setTotalCount,
    nextPage,
    prevPage,
    goToPage,
    resetOffset,
  };
}
