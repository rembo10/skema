#!/usr/bin/env bash
#
# Run the test suite under HPC coverage and enforce a minimum coverage floor.
#
# Coverage is reported as HPC *expression* coverage of the library (the Skema.*
# modules) as exercised by the test suite. The floor is a ratchet: it locks in
# the current level and fails on regressions. Raise MIN_COVERAGE as coverage
# improves; do not lower it to make a red build pass.
#
# Usage:
#   scripts/coverage.sh                 # enforce the default floor
#   MIN_COVERAGE=40 scripts/coverage.sh # enforce a custom floor
#
# Note: the coverage build uses a separate build plan from the normal (cached)
# `cabal test` build, so it never clobbers it. Do not run another cabal process
# against the same dist-newstyle concurrently — overlapping builds corrupt .hi
# files and produce spurious interface-file errors.
set -euo pipefail

MIN_COVERAGE="${MIN_COVERAGE:-34}"

cd "$(dirname "$0")/.."

# -O0 keeps the instrumented build fast; HPC tick counts are inserted before
# optimisation, so the coverage number is independent of the optimisation level.
cabal test --enable-coverage --ghc-options=-O0 --test-show-details=direct

html=$(find dist-newstyle -path '*/hpc/vanilla/html/hpc_index.html' | head -1)
if [ -z "${html}" ]; then
  echo "coverage: could not locate hpc_index.html under dist-newstyle" >&2
  exit 1
fi

# The library-only "Program Coverage Total" row carries three fractions in
# order: top-level definitions, alternatives, and expressions. Take the last.
total_row=$(grep -iA1 "Program Coverage Total" "${html}" | tail -1)
expr_frac=$(echo "${total_row}" | grep -oE '[0-9]+/[0-9]+' | tail -1)
if [ -z "${expr_frac}" ]; then
  echo "coverage: could not parse the coverage total from ${html}" >&2
  exit 1
fi

covered=${expr_frac%/*}
total=${expr_frac#*/}
pct=$(( covered * 100 / total ))

echo "----------------------------------------"
echo "HPC expression coverage: ${pct}% (${covered}/${total})"
echo "Minimum required:        ${MIN_COVERAGE}%"
echo "Full HTML report:        ${html}"
echo "----------------------------------------"

if [ "${pct}" -lt "${MIN_COVERAGE}" ]; then
  echo "coverage: FAILED — ${pct}% is below the ${MIN_COVERAGE}% floor" >&2
  exit 1
fi
echo "coverage: OK"
