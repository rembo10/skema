/**
 * Shared formatting utilities for displaying data in the UI
 */

/**
 * Format bytes to human-readable string
 * @param bytes - Number of bytes (can be null/undefined)
 * @param options - Formatting options
 * @returns Formatted string like "1.5 GB" or "Unknown size"
 */
export const formatBytes = (
  bytes: number | null | undefined,
  options: { unknownText?: string } = {}
): string => {
  const { unknownText = 'Unknown size' } = options;

  if (bytes === null || bytes === undefined) return unknownText;
  if (bytes === 0) return '0 B';

  const k = 1024;
  const sizes = ['B', 'KB', 'MB', 'GB', 'TB'];
  const i = Math.floor(Math.log(bytes) / Math.log(k));
  return Math.round((bytes / Math.pow(k, i)) * 100) / 100 + ' ' + sizes[i];
};

/**
 * Format a date string to localized date (year, month, day)
 * @param dateString - ISO date string or similar (can be null/undefined)
 * @param options - Formatting options
 * @returns Formatted date like "Jan 15, 2024" or "Unknown"
 */
export const formatDate = (
  dateString: string | null | undefined,
  options: { unknownText?: string } = {}
): string => {
  const { unknownText = 'Unknown' } = options;

  if (!dateString) return unknownText;

  try {
    // Handle YYYY-MM-DD format and other date formats
    if (dateString.includes('-') || dateString.includes('/') || dateString.includes('T')) {
      return new Date(dateString).toLocaleDateString('en-US', {
        year: 'numeric',
        month: 'short',
        day: 'numeric',
      });
    }
    // If it's just a year or other simple format, return as-is
    return dateString;
  } catch {
    return dateString;
  }
};

/**
 * Format a date string to localized datetime (month, day, hour, minute)
 * Useful for timestamps where the year is less important than the time
 * @param dateString - ISO date string or similar (can be null/undefined)
 * @param options - Formatting options
 * @returns Formatted datetime like "Jan 15, 10:30 AM" or "Unknown"
 */
export const formatDateTime = (
  dateString: string | null | undefined,
  options: { unknownText?: string } = {}
): string => {
  const { unknownText = 'Unknown' } = options;

  if (!dateString) return unknownText;

  try {
    return new Date(dateString).toLocaleString('en-US', {
      month: 'short',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
    });
  } catch {
    return dateString;
  }
};

/**
 * Format duration in seconds to human-readable string
 * @param seconds - Duration in seconds
 * @returns Formatted string like "2d 5h 30m" or "45s"
 */
export const formatDuration = (seconds: number): string => {
  if (seconds === 0) return '0s';

  const days = Math.floor(seconds / 86400);
  const hours = Math.floor((seconds % 86400) / 3600);
  const minutes = Math.floor((seconds % 3600) / 60);
  const secs = Math.floor(seconds % 60);

  const parts: string[] = [];
  if (days > 0) parts.push(`${days}d`);
  if (hours > 0) parts.push(`${hours}h`);
  if (minutes > 0) parts.push(`${minutes}m`);
  if (secs > 0 && days === 0 && hours === 0) parts.push(`${secs}s`); // Only show seconds if less than an hour

  return parts.join(' ') || '0s';
};

/**
 * Format a time relative to now, e.g. "just now", "5m ago", "2h ago", "3d ago"
 */
export const formatTimeAgo = (dateString: string): string => {
  const date = new Date(dateString);
  const seconds = Math.floor((new Date().getTime() - date.getTime()) / 1000);

  if (seconds < 60) return 'just now';
  const minutes = Math.floor(seconds / 60);
  if (minutes < 60) return `${minutes}m ago`;
  const hours = Math.floor(minutes / 60);
  if (hours < 24) return `${hours}h ago`;
  const days = Math.floor(hours / 24);
  return `${days}d ago`;
};

/**
 * Format a release date relative to now: "Today", "Yesterday", "5d ago", "2w ago", "1mo ago", or "Jan 2024"
 */
export const formatRelativeDate = (dateString: string | null): string | null => {
  if (!dateString) return null;
  const date = new Date(dateString);
  const now = new Date();
  const diffTime = now.getTime() - date.getTime();
  const diffDays = Math.floor(diffTime / (1000 * 60 * 60 * 24));

  if (diffDays === 0) return 'Today';
  if (diffDays === 1) return 'Yesterday';
  if (diffDays < 7) return `${diffDays}d ago`;
  if (diffDays < 30) return `${Math.floor(diffDays / 7)}w ago`;
  if (diffDays < 365) return `${Math.floor(diffDays / 30)}mo ago`;
  return date.toLocaleDateString('en-US', { year: 'numeric', month: 'short' });
};

/**
 * Format days until a future date: "Today", "Tomorrow", "5 days", "2 weeks", "1 months"
 */
export const getDaysUntil = (dateString: string): string => {
  const releaseDate = new Date(dateString);
  const now = new Date();
  const diffTime = releaseDate.getTime() - now.getTime();
  const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24));

  if (diffDays === 0) return 'Today';
  if (diffDays === 1) return 'Tomorrow';
  if (diffDays < 7) return `${diffDays} days`;
  if (diffDays < 30) return `${Math.floor(diffDays / 7)} weeks`;
  return `${Math.floor(diffDays / 30)} months`;
};

/**
 * Format a track duration from seconds to "M:SS" format
 */
export const formatTrackDuration = (seconds: number | null): string => {
  if (!seconds) return '--:--';
  const mins = Math.floor(seconds / 60);
  const secs = Math.floor(seconds % 60);
  return `${mins}:${secs.toString().padStart(2, '0')}`;
};

/**
 * Format a track duration from milliseconds to "M:SS" format
 */
export const formatTrackDurationMs = (ms: number | null | undefined): string => {
  if (!ms) return '\u2014';
  const seconds = Math.floor(ms / 1000);
  const minutes = Math.floor(seconds / 60);
  const remainingSeconds = seconds % 60;
  return `${minutes}:${remainingSeconds.toString().padStart(2, '0')}`;
};
