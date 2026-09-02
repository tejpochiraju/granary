#!/bin/sh
# Workflow mirror drift check (.forgejo/workflows vs .github/workflows)
#
# The two workflow trees are mirrors of each other, but they legitimately differ
# in places (runner labels, container setup, and bench-nightly.yml's schedule,
# which is deliberately Forgejo-only — see CLAUDE.md).  So this does NOT diff
# whole files.  It diffs the individual steps listed in SHARED_STEPS below,
# which are the ones that must be edited in lockstep and where a one-sided edit
# would fail silently: the guard would simply stop guarding on one forge.
#
# Adding a step here costs one line.  Adding a step that must stay in lockstep
# and NOT listing it here is the failure mode this script exists to prevent.
#
# Refs #574.

set -eu

cd "$(dirname "$0")/.."

# <workflow file>|<exact step name>
# #728: the merlint pin (its commit sha AND the pre-pin cleanup around it) must
# be identical on both forges, or the two lint jobs silently run different
# merlint versions — or one of them keeps failing on a poisoned opam cache after
# the other is fixed.
SHARED_STEPS='ci.yml|Policy — no real-SQLite outside gated comparison files (#370)
ci.yml|Workflow mirror drift check (.forgejo vs .github)
ci.yml|Install opam packages + merlint'

# Print the named step, re-indented relative to its own `- name:` line, so that
# a difference in surrounding job nesting is not reported as drift.
extract_step() {
  awk -v want="$1" '
    function indent_of(s,   n) { match(s, /^[ \t]*/); return RLENGTH }
    {
      trimmed = $0
      sub(/^[ \t]+/, "", trimmed)
      if (!found) {
        if (trimmed == "- name: " want) {
          found = 1
          ind = indent_of($0)
          print trimmed
        }
        next
      }
      if ($0 ~ /^[ \t]*$/) { print ""; next }
      if (indent_of($0) <= ind) { exit }
      print substr($0, ind + 1)
    }
    END { if (!found) exit 3 }
  ' "$2"
}

fail=0

echo "$SHARED_STEPS" | while IFS='|' read -r workflow step; do
  [ -n "$workflow" ] || continue
  forgejo=".forgejo/workflows/$workflow"
  github=".github/workflows/$workflow"

  for f in "$forgejo" "$github"; do
    if [ ! -f "$f" ]; then
      echo "FAIL: $f does not exist (listed in SHARED_STEPS)."
      exit 1
    fi
  done

  a=$(extract_step "$step" "$forgejo") || {
    echo "FAIL: $forgejo has no step named:"
    echo "        $step"
    exit 1
  }
  b=$(extract_step "$step" "$github") || {
    echo "FAIL: $github has no step named:"
    echo "        $step"
    exit 1
  }

  if [ "$a" != "$b" ]; then
    echo "FAIL: mirror drift in '$step' ($workflow):"
    ta=$(mktemp)
    tb=$(mktemp)
    printf '%s\n' "$a" >"$ta"
    printf '%s\n' "$b" >"$tb"
    diff -u "$ta" "$tb" --label "$forgejo" --label "$github" || true
    rm -f "$ta" "$tb"
    echo "      These two copies must be edited in lockstep."
    exit 1
  fi

  echo "OK: $workflow — '$step' matches on both forges"
done || fail=1

[ "$fail" -eq 0 ] || exit 1
