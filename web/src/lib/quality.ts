// Shared helpers for rendering audio quality labels/badges across pages.

const QUALITY_LABELS: Record<string, string> = {
  unknown: 'Unknown',
  mp3_192: 'MP3 192',
  vbr2: 'VBR V2',
  mp3_256: 'MP3 256',
  vbr0: 'VBR V0',
  mp3_320: 'MP3 320',
  lossless: 'FLAC',
  hires_lossless: 'Hi-Res FLAC',
};

const QUALITY_BADGE_STYLES: Record<string, string> = {
  unknown: 'bg-gray-500/20 text-gray-400 border-gray-500/30',
  mp3_192: 'bg-orange-500/20 text-orange-400 border-orange-500/30',
  vbr2: 'bg-orange-500/20 text-orange-400 border-orange-500/30',
  mp3_256: 'bg-yellow-500/20 text-yellow-400 border-yellow-500/30',
  vbr0: 'bg-yellow-500/20 text-yellow-400 border-yellow-500/30',
  mp3_320: 'bg-lime-500/20 text-lime-400 border-lime-500/30',
  lossless: 'bg-green-500/20 text-green-400 border-green-500/30',
  hires_lossless: 'bg-blue-500/20 text-blue-400 border-blue-500/30',
};

// The set of quality identifiers shown as filter chips / selectors.
export const QUALITY_VALUES = [
  'unknown',
  'mp3_192',
  'vbr2',
  'mp3_256',
  'vbr0',
  'mp3_320',
  'lossless',
  'hires_lossless',
] as const;

export function formatQuality(quality: string | null): string {
  if (!quality) return 'Unknown';
  return QUALITY_LABELS[quality] || quality;
}

export function getQualityBadgeStyle(quality: string | null): string {
  if (!quality) return QUALITY_BADGE_STYLES.unknown;
  return QUALITY_BADGE_STYLES[quality] || QUALITY_BADGE_STYLES.unknown;
}
