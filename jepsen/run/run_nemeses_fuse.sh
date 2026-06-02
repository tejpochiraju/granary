#!/usr/bin/env bash
# Run the nemeses that need special host privileges, from the HOST (not inside
# a container): lazyfs needs --device /dev/fuse + --cap-add SYS_ADMIN; clock-skew
# needs libfaketime via LD_PRELOAD.  Uses the sqlocaml-jepsen image (OCaml harness
# + lazyfs + libfaketime).  Run gen_177.sh / check_177.sh separately for the rest.
#
# Usage:  bash jepsen/run/run_nemeses_fuse.sh        # from the repo root
set -eu
REPO="$(cd "$(dirname "$0")/../.." && pwd)"
IMG="${IMG_JEPSEN:-sqlocaml-jepsen}"
H=/workspace/jepsen/run/histories
KEY=0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef
FT=/usr/lib/x86_64-linux-gnu/faketime/libfaketime.so.1
chmod 777 "$REPO/jepsen/run/histories" 2>/dev/null || true

echo "=== building harness in $IMG ==="
podman run --rm -v "$REPO":/workspace:z -w /workspace --entrypoint bash "$IMG" \
  -c 'opam exec -- dune build jepsen/ocaml/'

echo "=== clock-skew (list-append, FAKETIME=+5d) ==="
podman run --rm -v "$REPO":/workspace:z -w /workspace --entrypoint bash "$IMG" -c "
  EXE=_build/default/jepsen/ocaml/harness.exe
  LD_PRELOAD=$FT FAKETIME='+5d' \"\$EXE\" --workload list-append --backend mem \
    --nemesis clock-skew --workers 4 --ops 50 --history $H/list_append_clockskew.edn"

echo "=== lazyfs set/wal (un-fsynced write loss) ==="
podman run --rm --device /dev/fuse --cap-add SYS_ADMIN \
  -v "$REPO":/workspace:z -w /workspace --entrypoint bash "$IMG" -c "
  _build/default/jepsen/ocaml/harness.exe --workload set --backend wal \
    --path /tmp/lazyfs_mount/test.db --nemesis lazyfs --workers 2 --ops 40 \
    --history $H/set_wal_lazyfs.edn"

echo "=== lazyfs set/enc-wal (encrypted, un-fsynced write loss) ==="
podman run --rm --device /dev/fuse --cap-add SYS_ADMIN \
  -v "$REPO":/workspace:z -w /workspace --entrypoint bash "$IMG" -c "
  _build/default/jepsen/ocaml/harness.exe --workload set --backend enc-wal --key $KEY \
    --path /tmp/lazyfs_mount_enc/test.db --nemesis lazyfs --workers 2 --ops 40 \
    --history $H/set_encwal_lazyfs.edn"

echo "=== done — check with: podman run --rm -v \$PWD:/workspace:z $IMG-elle2 bash jepsen/run/check_177.sh ==="
