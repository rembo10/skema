import {
  CheckCircle2,
  AlertCircle,
  Lock,
  Zap,
  Clock,
  Download as DownloadIcon,
  XCircle,
  Archive,
} from 'lucide-react';
import type { DownloadStatus, MatchSource } from '../../types/api';

// Type for data that can have a match status (clusters and tracks)
export interface MatchStatusData {
  mb_release_id: string | null;
  match_locked: boolean;
  match_source: MatchSource | null;
}

export type MatchStatus = 'unmatched' | 'locked' | 'fingerprint' | 'matched';

export interface MatchStatusInfo {
  status: MatchStatus;
  icon: React.ReactNode;
  text: string;
  color: string;
}

/**
 * Determine the match status from cluster or track data
 */
export function getMatchStatus(data: MatchStatusData): MatchStatusInfo {
  if (!data.mb_release_id) {
    return {
      status: 'unmatched',
      icon: <AlertCircle className="h-4 w-4 text-red-400" />,
      text: 'Unmatched',
      color: 'text-red-400',
    };
  }
  if (data.match_locked) {
    return {
      status: 'locked',
      icon: <Lock className="h-4 w-4 text-purple-400" />,
      text: 'Locked',
      color: 'text-purple-400',
    };
  }
  if (data.match_source === 'auto_fingerprint') {
    return {
      status: 'fingerprint',
      icon: <Zap className="h-4 w-4 text-blue-400" />,
      text: 'Fingerprint',
      color: 'text-blue-400',
    };
  }
  return {
    status: 'matched',
    icon: <CheckCircle2 className="h-4 w-4 text-green-400" />,
    text: 'Matched',
    color: 'text-green-400',
  };
}

interface MatchStatusBadgeProps {
  data: MatchStatusData;
  showText?: boolean;
}

/**
 * Badge component for displaying cluster/track match status
 */
export function MatchStatusBadge({ data, showText = false }: MatchStatusBadgeProps) {
  const status = getMatchStatus(data);

  return (
    <div className="flex items-center gap-2">
      {status.icon}
      {showText && <span className={`text-sm ${status.color}`}>{status.text}</span>}
    </div>
  );
}

// Download status helpers

export interface DownloadStatusInfo {
  icon: React.ReactNode;
  text: string;
  color: string;
}

function getDownloadStatusInfo(status: DownloadStatus): DownloadStatusInfo {
  switch (status) {
    case 'queued':
      return {
        icon: <Clock className="w-5 h-5 text-dark-text-secondary" />,
        text: 'Queued',
        color: 'text-dark-text-secondary',
      };
    case 'downloading':
      return {
        icon: <DownloadIcon className="w-5 h-5 text-dark-accent animate-pulse" />,
        text: 'Downloading',
        color: 'text-dark-accent',
      };
    case 'completed':
      return {
        icon: <CheckCircle2 className="w-5 h-5 text-dark-success" />,
        text: 'Completed',
        color: 'text-dark-success',
      };
    case 'failed':
      return {
        icon: <XCircle className="w-5 h-5 text-dark-error" />,
        text: 'Failed',
        color: 'text-dark-error',
      };
    case 'imported':
      return {
        icon: <Archive className="w-5 h-5 text-dark-success" />,
        text: 'Imported',
        color: 'text-dark-success',
      };
    case 'cancelled':
      return {
        icon: <XCircle className="w-5 h-5 text-dark-text-secondary" />,
        text: 'Cancelled',
        color: 'text-dark-text-secondary',
      };
    case 'identification_failure':
      return {
        icon: <AlertCircle className="w-5 h-5 text-yellow-500" />,
        text: 'Identification Failed',
        color: 'text-yellow-500',
      };
  }
}

interface DownloadStatusBadgeProps {
  status: DownloadStatus;
  showText?: boolean;
}

/**
 * Badge component for displaying download status
 */
export function DownloadStatusBadge({ status, showText = true }: DownloadStatusBadgeProps) {
  const info = getDownloadStatusInfo(status);

  return (
    <div className="flex items-center gap-2">
      {info.icon}
      {showText && <span className={`font-medium ${info.color}`}>{info.text}</span>}
    </div>
  );
}

/**
 * Get just the download status icon (for use in lists where icon is separate from text)
 */
export function getDownloadStatusIcon(status: DownloadStatus): React.ReactNode {
  return getDownloadStatusInfo(status).icon;
}

/**
 * Get just the download status text
 */
export function getDownloadStatusText(status: DownloadStatus): string {
  return getDownloadStatusInfo(status).text;
}

/**
 * Get just the download status color class
 */
export function getDownloadStatusColor(status: DownloadStatus): string {
  return getDownloadStatusInfo(status).color;
}
