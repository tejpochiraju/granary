#!/usr/bin/env bash
set -u
export HOME=/tmp/clj-home
mkdir -p "$HOME"
PROJ=/tmp/clj
[ -d "$PROJ" ] || cp -r /workspace/jepsen/clojure "$PROJ"
cd "$PROJ"
H=/workspace/jepsen/run/histories
clojure -P >/dev/null 2>&1

dump() {
  local f="$1" w="$2"
  echo "############################################################"
  echo "### $f  (-w $w)"
  echo "############################################################"
  clojure -J-Djava.awt.headless=true -M -m jepsen.check "$H/$f" -w "$w" 2>&1 \
    | grep -vE '^(Downloading|WARNING)' | sed -n '/Full analysis/,/^$/p' | head -60
  echo
}

dump counter_wal.edn counter
dump counter_encwal.edn counter
dump list_append_wal.edn list-append
dump list_append_encwal.edn list-append
