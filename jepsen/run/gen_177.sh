#!/usr/bin/env bash
# Generate all Jepsen #177 histories into jepsen/run/histories/.
# Runs INSIDE the granary-dev container.
set -u
cd /workspace
H=jepsen/run/histories
mkdir -p "$H"
rm -f "$H"/*.edn
KEY=0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef

opam exec -- dune build jepsen/ocaml/ || { echo "BUILD FAILED"; exit 1; }
EXE() { opam exec -- dune exec jepsen/ocaml/harness.exe -- "$@"; }

run() {
  local name="$1"; shift
  echo "### gen $name"
  EXE "$@" >/dev/null 2>"/tmp/${name}.err"
  local rc=$?
  if [ $rc -ne 0 ]; then echo "  GEN-FAIL $name (exit $rc):"; tail -3 "/tmp/${name}.err" | sed 's/^/    /'; fi
}

# ---- Core real workloads (should be VALID) ----
run list_append_mem  --workload list-append --backend mem  --workers 4 --ops 100 --keys 10 --history $H/list_append_mem.edn
run list_append_file --workload list-append --backend file --path /tmp/la_file.db --workers 4 --ops 100 --keys 10 --history $H/list_append_file.edn
run list_append_wal  --workload list-append --backend wal  --path /tmp/la_wal.db  --workers 4 --ops 100 --keys 10 --history $H/list_append_wal.edn
run bank_mem    --workload bank --backend mem  --workers 4 --ops 100 --keys 5 --history $H/bank_mem.edn
run bank_wal    --workload bank --backend wal  --path /tmp/bank_wal.db --workers 4 --ops 100 --keys 5 --history $H/bank_wal.edn
run set_file    --workload set --backend file --path /tmp/set_file.db --workers 4 --ops 50 --history $H/set_file.edn
run set_wal     --workload set --backend wal  --path /tmp/set_wal.db  --workers 4 --ops 50 --history $H/set_wal.edn
run counter_mem --workload counter --backend mem --workers 4 --ops 200 --keys 5 --history $H/counter_mem.edn
run counter_wal --workload counter --backend wal --path /tmp/counter_wal.db --workers 4 --ops 200 --keys 5 --history $H/counter_wal.edn

# ---- Encrypted-at-rest WAL (new-feature focus: #84/#214/#215) ----
run list_append_encwal --workload list-append --backend enc-wal --key $KEY --path /tmp/la_enc.db --workers 4 --ops 100 --keys 10 --history $H/list_append_encwal.edn
run bank_encwal    --workload bank    --backend enc-wal --key $KEY --path /tmp/bank_enc.db    --workers 4 --ops 100 --keys 5 --history $H/bank_encwal.edn
run set_encwal     --workload set     --backend enc-wal --key $KEY --path /tmp/set_enc.db     --workers 4 --ops 50 --history $H/set_encwal.edn
run counter_encwal --workload counter --backend enc-wal --key $KEY --path /tmp/counter_enc.db --workers 4 --ops 200 --keys 5 --history $H/counter_encwal.edn

# ---- Nemeses ----
run set_wal_crash    --workload set --backend wal --path /tmp/crash_wal.db --nemesis crash-restart --crash-after 30 --workers 2 --ops 60 --history $H/set_wal_crash.edn
run set_encwal_crash --workload set --backend enc-wal --key $KEY --path /tmp/crash_enc.db --nemesis crash-restart --crash-after 30 --workers 2 --ops 60 --history $H/set_encwal_crash.edn
run list_append_pause --workload list-append --backend mem --nemesis pause --pause-after 40 --pause-dur 3.0 --workers 4 --ops 100 --history $H/list_append_pause.edn

# ---- Negative controls (should be INVALID) ----
echo "### gen negative controls"
opam exec -- dune exec jepsen/ocaml/negative_control.exe -- >/dev/null 2>&1
cp /tmp/granary_negative_*.edn $H/ 2>/dev/null

echo
echo "=== generated histories ==="
for f in "$H"/*.edn; do printf "  %-40s %6s lines\n" "$(basename "$f")" "$(wc -l < "$f")"; done
