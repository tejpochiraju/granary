#!/usr/bin/env bash
# Generate test coverage report using bisect_ppx.
#
# Usage:
#   ./scripts/coverage.sh             # summary to stdout
#   ./scripts/coverage.sh html        # summary + HTML in _coverage/
#   ./scripts/coverage.sh ci          # fail if below COVERAGE_THRESHOLD (default 85%)
#
# Requires: sqlocaml-dev Podman image (built from Containerfile, includes bisect_ppx).
# Run from the repo root.

set -euo pipefail

REPO="$(cd "$(dirname "$0")/.." && pwd)"
THRESHOLD="${COVERAGE_THRESHOLD:-85}"
MODE="${1:-summary}"

PODMAN="podman run --rm -v ${REPO}:/workspace:Z -w /workspace sqlocaml-dev"

echo "==> Building and running instrumented test suite..."
$PODMAN bash -c "
  dune clean 2>/dev/null || true
  dune runtest --instrument-with bisect_ppx 2>/dev/null
" 2>&1

COVFILES=$($PODMAN bash -c "find _build -name '*.coverage' | tr '\n' ' '" 2>/dev/null)

if [ -z "${COVFILES// }" ]; then
  echo "ERROR: no coverage files generated. Is bisect_ppx installed in sqlocaml-dev?" >&2
  exit 1
fi

case "$MODE" in
  html)
    mkdir -p "$REPO/_coverage"
    chmod 777 "$REPO/_coverage"
    $PODMAN bash -c "
      bisect-ppx-report summary --per-file $COVFILES
      bisect-ppx-report html -o _coverage $COVFILES
    " 2>&1
    sudo chown -R "$(id -un):$(id -gn)" "$REPO/_coverage" 2>/dev/null || true
    echo ""
    echo "==> HTML report written to: _coverage/index.html"
    ;;
  ci)
    SUMMARY=$($PODMAN bash -c "bisect-ppx-report summary --per-file $COVFILES" 2>&1)
    echo "$SUMMARY"
    # Extract overall percentage from the last line
    PCT=$(echo "$SUMMARY" | tail -1 | grep -oP '\d+\.\d+(?= %)' | head -1)
    echo ""
    echo "==> Overall coverage: ${PCT}%  (threshold: ${THRESHOLD}%)"
    if (( $(echo "$PCT < $THRESHOLD" | bc -l) )); then
      echo "FAIL: coverage ${PCT}% is below threshold ${THRESHOLD}%" >&2
      exit 1
    fi
    echo "PASS"
    ;;
  summary|*)
    $PODMAN bash -c "bisect-ppx-report summary --per-file $COVFILES" 2>&1
    ;;
esac
