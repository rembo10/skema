#!/usr/bin/env bash
# Bump the project version in one shot.
#
# Usage: scripts/bump-version.sh 0.1.3
#
# Updates server/skema.cabal (x.y.z.0) and web/package.json + package-lock.json
# (x.y.z, via npm). Does not commit — review and commit yourself.
set -euo pipefail

v="${1:-}"
if [[ ! "$v" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "Usage: $0 <version>   e.g. $0 0.1.3" >&2
  exit 1
fi

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

sed -i "s/^version: .*/version: ${v}.0/" "$root/server/skema.cabal"
( cd "$root/web" && npm version "$v" --no-git-tag-version >/dev/null )

echo "Bumped to $v"
echo "  server/skema.cabal    -> ${v}.0"
echo "  web/package.json      -> ${v}"
echo "  web/package-lock.json -> ${v}"
