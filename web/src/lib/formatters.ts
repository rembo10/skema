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
