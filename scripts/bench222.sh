#!/usr/bin/env bash
# #222 — run the cross-engine benchmark and emit a per-host CSV + metadata.
#
# File I/O is bind-mounted to a REAL host directory ($BENCH_DATA_DIR) so it hits
# the actual disk (HDD on 'here', NVMe on 'otp-prod-1'), not the podman overlay.
#
# Usage:  scripts/bench222.sh [host-label]
# Env:    GRANARY_BENCH_ROWS / _OPS / _SCANS / _COMMITS / _REPEATS / _PAGE_CACHE
set -euo pipefail

REPO="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO"

HOST_LABEL="${1:-$(hostname)}"
IMAGE="localhost/granary-bench"
RESULTS_DIR="$REPO/bench/results"
DATA_DIR="${BENCH_DATA_DIR:-$REPO/bench/data}"   # on the host's real fs
mkdir -p "$RESULTS_DIR" "$DATA_DIR"
chmod 777 "$REPO" "$DATA_DIR" || true

# Build the bench image if absent.
if ! podman image exists "$IMAGE"; then
  echo "building $IMAGE ..." >&2
  podman build -t granary-bench -f containers/bench.Containerfile .
fi

# Tunables (defaults chosen so the HDD host exercises fsync/seek meaningfully).
ROWS="${GRANARY_BENCH_ROWS:-100000}"
OPS="${GRANARY_BENCH_OPS:-20000}"
SCANS="${GRANARY_BENCH_SCANS:-50}"
COMMITS="${GRANARY_BENCH_COMMITS:-500}"
REPEATS="${GRANARY_BENCH_REPEATS:-5}"
PAGE_CACHE="${GRANARY_BENCH_PAGE_CACHE:-1024}"

GIT_SHA="$(git rev-parse --short HEAD)"
SQLITE_VER="$(podman run --rm "$IMAGE" sqlite3 --version | awk '{print $1}')"

# Metadata sidecar.
{
  echo "host=$HOST_LABEL"
  echo "date=$(date -u +%FT%TZ)"
  echo "nproc=$(nproc)"
  echo "disk=$(lsblk -d -o NAME,ROTA,MODEL 2>/dev/null | awk 'NR>1 && $1!~"loop"{print $0}' | tr '\n' ';')"
  echo "sqlite_version=$SQLITE_VER"
  echo "granary_sha=$GIT_SHA"
  echo "rows=$ROWS ops=$OPS scans=$SCANS commits=$COMMITS repeats=$REPEATS page_cache=$PAGE_CACHE"
} > "$RESULTS_DIR/$HOST_LABEL.meta"

echo "running bench on $HOST_LABEL (sqlite=$SQLITE_VER sha=$GIT_SHA) ..." >&2

# Build first (separate from run so build noise stays off the CSV).
podman run --rm -v "$REPO":/workspace:Z -w /workspace "$IMAGE" \
  dune build test/bench_compare.exe

# Run: bind-mount the host data dir at /benchdata; point the bench's temp dir there.
podman run --rm \
  -e GRANARY_BENCH_HOST="$HOST_LABEL" \
  -e GRANARY_BENCH_ROWS="$ROWS" -e GRANARY_BENCH_OPS="$OPS" \
  -e GRANARY_BENCH_SCANS="$SCANS" -e GRANARY_BENCH_COMMITS="$COMMITS" \
  -e GRANARY_BENCH_REPEATS="$REPEATS" -e GRANARY_BENCH_PAGE_CACHE="$PAGE_CACHE" \
  -e TMPDIR=/benchdata \
  -v "$REPO":/workspace:Z -w /workspace \
  -v "$DATA_DIR":/benchdata:Z \
  "$IMAGE" \
  dune exec test/bench_compare.exe 2>/dev/null \
  > "$RESULTS_DIR/$HOST_LABEL.csv"

echo "wrote $RESULTS_DIR/$HOST_LABEL.csv" >&2
cat "$RESULTS_DIR/$HOST_LABEL.csv"
