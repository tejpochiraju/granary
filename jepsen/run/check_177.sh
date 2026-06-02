#!/usr/bin/env bash
# Check all #177 histories with the Clojure/Elle checkers.
# Runs INSIDE the sqlocaml-jepsen-elle2 container.
# The mounted project dir + opam $HOME are not writable by the runtime user,
# so we copy the checker into /tmp and use a writable HOME for the dep cache.
set -u
export HOME=/tmp/clj-home
mkdir -p "$HOME"
PROJ=/tmp/clj
rm -rf "$PROJ"; cp -r /workspace/jepsen/clojure "$PROJ"
cd "$PROJ"
H=/workspace/jepsen/run/histories

echo "### warming clojure deps (one-time download) ..."
clojure -P >/tmp/prep.out 2>&1 || { echo "DEP PREP FAILED:"; tail -15 /tmp/prep.out; exit 1; }
echo "### deps ready"
echo

CHK() { clojure -J-Djava.awt.headless=true -M -m jepsen.check "$@" >/tmp/chk.out 2>&1; }

wl() {
  case "$1" in
    list_append_*|sqlocaml_negative_dirty_read|sqlocaml_negative_lost_update) echo list-append ;;
    bank_*|sqlocaml_negative_bank_lost_transfer) echo bank ;;
    set_*|sqlocaml_negative_set_lost_element) echo set ;;
    counter_*|sqlocaml_negative_counter_non_monotonic) echo counter ;;
    *) echo list-append ;;
  esac
}

echo "============================================================"
echo "  #177 JEPSEN CHECK - real histories (expect VALID, exit 0)"
echo "============================================================"
for f in $(ls "$H" | grep -v '^sqlocaml_negative_' | sort); do
  name="${f%.edn}"; w="$(wl "$name")"
  CHK "$H/$f" -w "$w"; rc=$?
  res=$(grep -m1 '^RESULT:' /tmp/chk.out | sed 's/RESULT: //')
  [ -z "$res" ] && res="(no RESULT - error: $(tail -1 /tmp/chk.out))"
  printf "  %-40s [%-11s] exit=%d  %s\n" "$name" "$w" "$rc" "$res"
done

echo
echo "--- list-append Elle SI checker (informational; --elle) ---"
for f in list_append_mem list_append_wal list_append_encwal; do
  [ -f "$H/$f.edn" ] || continue
  CHK "$H/$f.edn" -w list-append --elle; rc=$?
  res=$(grep -m1 '^RESULT:' /tmp/chk.out | sed 's/RESULT: //')
  anom=$(grep -m1 -iE ':anomaly|duplicate-elements|:anomalies' /tmp/chk.out)
  [ -z "$res" ] && res="(no RESULT - error: $(tail -1 /tmp/chk.out))"
  printf "  %-22s exit=%d  %s  %s\n" "$f" "$rc" "$res" "$anom"
done

echo
echo "============================================================"
echo "  #177 JEPSEN CHECK - negative controls (expect INVALID, exit 1)"
echo "============================================================"
for f in $(ls "$H" | grep '^sqlocaml_negative_' | sort); do
  name="${f%.edn}"; w="$(wl "$name")"
  CHK "$H/$f" -w "$w"; rc=$?
  res=$(grep -m1 '^RESULT:' /tmp/chk.out | sed 's/RESULT: //')
  if [ "$rc" -eq 1 ] && echo "$res" | grep -q INVALID; then verdict="CAUGHT ok"
  elif [ -z "$res" ]; then verdict="ERROR"; res="$(tail -1 /tmp/chk.out)"
  else verdict="UNEXPECTED"; fi
  printf "  %-42s [%-11s] exit=%d  %-10s %s\n" "$name" "$w" "$rc" "$verdict" "$res"
done
