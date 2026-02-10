/**
 * Builds a query string from a params object.
 * - Filters out undefined values
 * - Handles string arrays by joining with commas
 * - Converts booleans and numbers to strings
 * @param params - Record of parameter names to values
 * @returns Query string without leading '?'
 */
export function buildQueryString(
  params: Record<string, string | number | boolean | string[] | undefined>
): string {
  const searchParams = new URLSearchParams();

  for (const [key, value] of Object.entries(params)) {
    if (value === undefined) {
      continue;
    }

    if (Array.isArray(value)) {
      if (value.length > 0) {
        searchParams.append(key, value.join(','));
      }
    } else if (typeof value === 'boolean' || typeof value === 'number') {
      searchParams.append(key, String(value));
    } else {
      searchParams.append(key, value);
    }
  }

  return searchParams.toString();
}
