import { describe, it, expect } from 'vitest';
import { render, screen } from '@testing-library/react';
import { Skeleton, PageLoadingSkeleton, CardSkeleton, StatCardSkeleton, DashboardSkeleton } from './LoadingSkeleton';

describe('LoadingSkeleton components', () => {
  describe('Skeleton', () => {
    it('renders with default class', () => {
      render(<Skeleton />);
      const skeleton = screen.getByLabelText('Loading...');
      expect(skeleton).toBeInTheDocument();
      expect(skeleton).toHaveClass('animate-pulse', 'bg-dark-bg-subtle', 'rounded');
    });

    it('applies custom className', () => {
      render(<Skeleton className="w-32 h-8" />);
      const skeleton = screen.getByLabelText('Loading...');
      expect(skeleton).toHaveClass('w-32', 'h-8');
    });
  });

  describe('PageLoadingSkeleton', () => {
    it('renders loading spinner and text', () => {
      render(<PageLoadingSkeleton />);
      expect(screen.getByText('Loading...')).toBeInTheDocument();
    });
  });

  describe('CardSkeleton', () => {
    it('renders card skeleton structure', () => {
      const { container } = render(<CardSkeleton />);
      expect(container.querySelector('.card')).toBeInTheDocument();
    });
  });

  describe('StatCardSkeleton', () => {
    it('renders stat card skeleton structure', () => {
      const { container } = render(<StatCardSkeleton />);
      expect(container.querySelector('.card')).toBeInTheDocument();
    });
  });

  describe('DashboardSkeleton', () => {
    it('renders dashboard skeleton with multiple sections', () => {
      const { container } = render(<DashboardSkeleton />);
      // Should have multiple card skeletons
      const cards = container.querySelectorAll('.card');
      expect(cards.length).toBeGreaterThan(0);
    });
  });
});
