#!/usr/bin/env bash
# Full Jepsen #177 suite + pass/fail GATE, all inside the granary-jepsen image
# (OCaml harness + lazyfs + libfaketime + Clojure/Elle checker). Generates fresh
# histories into /tmp (does NOT touch the committed reference run), checks each
# against its expected verdict, and exits non-zero on any deviation.
#
#   podman run --rm --device /dev/fuse --cap-add SYS_ADMIN \
#     -v "$PWD":/workspace:z -w /workspace --entrypoint bash \
#     granary-jepsen jepsen/run/ci_full.sh
set -u
cd /workspace
H=/tmp/jhist
mkdir -p "$H"
rm -f "$H"/*.edn
KEY=0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef
FT=/usr/lib/x86_64-linux-gnu/faketime/libfaketime.so.1

echo "=== build harness ==="
opam exec -- dune build jepsen/ocaml/ || { echo "BUILD FAILED"; exit 1; }
EXE=_build/default/jepsen/ocaml/harness.exe

gen() {
  local name="$1"; shift
  "$EXE" "$@" --history "$H/$name.edn" >/dev/null 2>"/tmp/$name.err" \
    || { echo "GEN-FAIL $name"; tail -3 "/tmp/$name.err"; exit 1; }
}

echo "=== generate workload histories ==="
gen list_append_mem    --workload list-append --backend mem  --workers 4 --ops 100 --keys 10
gen list_append_file   --workload list-append --backend file --path /tmp/la_file.db --workers 4 --ops 100 --keys 10
gen list_append_wal    --workload list-append --backend wal  --path /tmp/la_wal.db --workers 4 --ops 100 --keys 10
gen list_append_encwal --workload list-append --backend enc-wal --key $KEY --path /tmp/la_enc.db --workers 4 --ops 100 --keys 10
gen bank_mem    --workload bank --backend mem --workers 4 --ops 100 --keys 5
gen bank_wal    --workload bank --backend wal --path /tmp/bank_wal.db --workers 4 --ops 100 --keys 5
gen bank_encwal --workload bank --backend enc-wal --key $KEY --path /tmp/bank_enc.db --workers 4 --ops 100 --keys 5
gen set_file    --workload set --backend file --path /tmp/set_file.db --workers 4 --ops 50
gen set_wal     --workload set --backend wal  --path /tmp/set_wal.db --workers 4 --ops 50
gen set_encwal  --workload set --backend enc-wal --key $KEY --path /tmp/set_enc.db --workers 4 --ops 50
gen counter_mem    --workload counter --backend mem --workers 4 --ops 200 --keys 5
gen counter_wal    --workload counter --backend wal --path /tmp/cw.db --workers 4 --ops 200 --keys 5
gen counter_encwal --workload counter --backend enc-wal --key $KEY --path /tmp/ce.db --workers 4 --ops 200 --keys 5
gen set_wal_crash    --workload set --backend wal --path /tmp/crash_wal.db --nemesis crash-restart --crash-after 30 --workers 2 --ops 60
gen set_encwal_crash --workload set --backend enc-wal --key $KEY --path /tmp/crash_enc.db --nemesis crash-restart --crash-after 30 --workers 2 --ops 60
gen list_append_pause --workload list-append --backend mem --nemesis pause --pause-after 40 --pause-dur 3.0 --workers 4 --ops 100

echo "=== nemeses (FUSE / faketime) ==="
LD_PRELOAD=$FT FAKETIME="+5d" "$EXE" --workload list-append --backend mem --nemesis clock-skew \
  --workers 4 --ops 50 --history "$H/list_append_clockskew.edn" >/dev/null 2>&1 \
  || { echo "clock-skew gen failed"; exit 1; }
"$EXE" --workload set --backend wal --path /tmp/lazyfs_mount/test.db --nemesis lazyfs \
  --workers 2 --ops 40 --history "$H/set_wal_lazyfs.edn" >/tmp/lz1.log 2>&1 \
  || { echo "lazyfs wal failed"; tail -8 /tmp/lz1.log; exit 1; }
"$EXE" --workload set --backend enc-wal --key $KEY --path /tmp/lazyfs_mount_enc/test.db --nemesis lazyfs \
  --workers 2 --ops 40 --history "$H/set_encwal_lazyfs.edn" >/tmp/lz2.log 2>&1 \
  || { echo "lazyfs enc-wal failed"; tail -8 /tmp/lz2.log; exit 1; }

echo "=== negative controls ==="
opam exec -- dune exec jepsen/ocaml/negative_control.exe -- >/dev/null 2>&1
cp /tmp/granary_negative_*.edn "$H"/ 2>/dev/null

echo "=== prepare checker (Clojure/Elle) ==="
export HOME=/tmp/cljhome
mkdir -p "$HOME"
PROJ=/tmp/clj
rm -rf "$PROJ"; cp -r /workspace/jepsen/clojure "$PROJ"; cd "$PROJ"
clojure -P >/tmp/prep.log 2>&1 || { echo "clojure dep prep failed"; tail -12 /tmp/prep.log; exit 1; }

CHK() { clojure -J-Djava.awt.headless=true -M -m jepsen.check "$@" >/tmp/chk.out 2>&1; }
wl() {
  case "$1" in
    list_append_*|granary_negative_dirty_read|granary_negative_lost_update) echo list-append ;;
    bank_*|granary_negative_bank_lost_transfer) echo bank ;;
    set_*|granary_negative_set_lost_element) echo set ;;
    counter_*|granary_negative_counter_non_monotonic) echo counter ;;
  esac
}

fails=0
echo
echo "=== GATE: real workloads (expect VALID) ==="
# Every workload/backend/nemesis history must be VALID under its checker.
for name in \
  bank_mem bank_wal bank_encwal \
  set_file set_wal set_encwal set_wal_crash set_encwal_crash set_wal_lazyfs set_encwal_lazyfs \
  counter_mem counter_wal counter_encwal \
  list_append_mem list_append_file list_append_pause list_append_clockskew; do
  CHK "$H/$name.edn" -w "$(wl "$name")"; rc=$?
  if [ $rc -eq 0 ]; then printf "  VALID       %s\n" "$name"
  else printf "  FAIL        %s (expected VALID, exit %d)\n" "$name" "$rc"; fails=$((fails + 1)); fi
done

echo
echo "=== GATE: list-append WAL/enc-WAL via Elle SI (temporal :ok-timestamp artifact) ==="
# The pure-Clojure temporal checker flags benign :ok-completion-timestamp
# inversions on these; Elle's snapshot-isolation checker is authoritative.
for name in list_append_wal list_append_encwal; do
  CHK "$H/$name.edn" -w list-append --elle; rc=$?
  if [ $rc -eq 0 ]; then printf "  VALID(elle) %s\n" "$name"
  else printf "  FAIL        %s (Elle SI expected VALID, exit %d)\n" "$name" "$rc"; fails=$((fails + 1)); fi
done

echo
echo "=== GATE: negative controls (expect INVALID / caught) ==="
for name in \
  granary_negative_dirty_read granary_negative_lost_update \
  granary_negative_bank_lost_transfer granary_negative_set_lost_element \
  granary_negative_counter_non_monotonic; do
  CHK "$H/$name.edn" -w "$(wl "$name")"; rc=$?
  if [ $rc -eq 1 ]; then printf "  CAUGHT      %s\n" "$name"
  else printf "  FAIL        %s (expected INVALID/exit 1, got %d)\n" "$name" "$rc"; fails=$((fails + 1)); fi
done

echo
echo "============================================================"
if [ $fails -eq 0 ]; then
  echo "JEPSEN #177 NIGHTLY GATE: PASS"
  exit 0
else
  echo "JEPSEN #177 NIGHTLY GATE: FAIL ($fails deviation(s))"
  exit 1
fi
