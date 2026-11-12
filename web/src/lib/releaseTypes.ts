// Shared helpers for describing release groups by their MusicBrainz primary type.

import { pluralize } from './formatters';

// MusicBrainz's primary types in its own display order
// (release_group_primary_type.child_order).
const PRIMARY_TYPE_ORDER = ['Album', 'Single', 'EP', 'Broadcast', 'Other'];

const typeRank = (type: string): number => {
  const rank = PRIMARY_TYPE_ORDER.indexOf(type);
  return rank === -1 ? PRIMARY_TYPE_ORDER.length : rank;
};

/**
 * Count release groups per primary type, e.g. "12 Singles · 9 EPs".
 * Lists only the types present, in MusicBrainz order. Types MusicBrainz
 * doesn't define follow Other alphabetically; release groups without a type
 * come last as "Unspecified" (MusicBrainz's "Unspecified type").
 * Returns '' when there are none.
 */
export function formatReleaseTypeCounts(types: (string | null)[]): string {
  const counts = new Map<string, number>();
  let unspecified = 0;
  for (const type of types) {
    if (type) counts.set(type, (counts.get(type) ?? 0) + 1);
    else unspecified++;
  }

  const parts = [...counts]
    .sort(([a], [b]) => typeRank(a) - typeRank(b) || a.localeCompare(b))
    // "Other" names a category, not a thing: "3 Other", like "2 Unspecified"
    .map(([type, count]) => (type === 'Other' ? `${count} Other` : pluralize(count, type)));
  if (unspecified > 0) parts.push(`${unspecified} Unspecified`);

  // Non-breaking spaces keep each count with its type and each separator with
  // the part before it, so a narrow screen only wraps after a "·".
  return parts.map(part => part.replace(/ /g, '\u00a0')).join('\u00a0· ');
}
