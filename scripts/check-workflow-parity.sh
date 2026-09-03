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

# THIS SCRIPT SELF-TESTS ON EVERY RUN (see [self_test]).  It is an enforcement
# gate, and an enforcement gate breaks by PERMITTING things: a comparison that
# stops comparing prints the same "OK" lines it always did.  Nothing else covers
# it — the shellcheck gate added by #591 is a syntactic linter and cannot tell a
# working comparison from a broken one.  Pass `--self-test` to run only the
# self-test, verbosely.

set -eu

# <workflow file>|<exact step name>
# #728: the merlint pin (its commit sha AND the pre-pin cleanup around it) must
# be identical on both forges, or the two lint jobs silently run different
# merlint versions — or one of them keeps failing on a poisoned opam cache after
# the other is fixed.
#
# #591: the shellcheck gate and the apt line that installs its linter.  Both
# must be listed: a gate present on one forge only is a gate half the pushes
# never meet, and a linter installed on one forge only turns the other's gate
# into an immediate hard failure.  The lint job's dep step was RENAMED to
# '... + shellcheck (#591)' to make it addressable here at all — [extract_step]
# takes the FIRST step of a given name in a file, and the build-and-test job
# above it had the identical name, so the lint job's copy could not be named.
SHARED_STEPS='ci.yml|Policy — no real-SQLite outside gated comparison files (#370)
ci.yml|Workflow mirror drift check (.forgejo vs .github)
ci.yml|Install opam packages + merlint
ci.yml|Install system deps + shellcheck (#591)
ci.yml|Shell lint — shellcheck over scripts/ (#591)'

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

# The comparison, parameterised over the two workflow directories so that
# [self_test] can drive it against a planted tree.  Returns non-zero on drift.
compare_steps() {
  cs_forgejo_dir=$1
  cs_github_dir=$2
  cs_steps=$3
  cs_quiet=${4:-0}
  fail=0

  echo "$cs_steps" | while IFS='|' read -r workflow step; do
  [ -n "$workflow" ] || continue
  forgejo="$cs_forgejo_dir/$workflow"
  github="$cs_github_dir/$workflow"

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

  [ "$cs_quiet" -ne 0 ] || echo "OK: $workflow — '$step' matches on both forges"
  done || fail=1

  return "$fail"
}

# --- self-test -------------------------------------------------------------
#
# Plants a pair of workflow files that agree, asserts a PASS; then plants a pair
# that disagree in the named step, and asserts a FAIL.  Both halves matter: the
# first proves a green verdict is not vacuous, the second proves the gate can
# still go red, which is the only thing that makes the green one worth reading.
# The two "missing file" and "missing step" refusals are covered too, because
# each of them is a way for a comparison to have nothing to compare.
self_test() {
  st_verbose=$1
  # `set -e` is suspended for a function invoked in a condition, so an unchecked
  # mktemp failure would carry on with root='' and write into /.  Check it.
  st_root=$(mktemp -d) || return 1
  case ${st_root:-} in
    /*/*) ;;
    *) return 1 ;;
  esac
  st_fail=0
  mkdir -p "$st_root/forgejo" "$st_root/github"

  st_steps='ci.yml|Planted step'

  st_body() {
    cat >"$1" <<EOF
jobs:
  lint:
    steps:
      - name: Planted step
        run: sh scripts/$2
      - name: Something else
        run: true
EOF
  }

  st_expect() {
    # $1 = expected outcome (ok|fail), $2 = label
    if compare_steps "$st_root/forgejo" "$st_root/github" "$st_steps" 1 \
      >"$st_root/out" 2>&1; then
      st_got=ok
    else
      st_got=fail
    fi
    if [ "$st_got" = "$1" ]; then
      [ "$st_verbose" -eq 0 ] || echo "OK (self-test): $2"
    else
      echo "FAIL (self-test): $2 — expected $1, got $st_got"
      cat "$st_root/out"
      st_fail=1
    fi
  }

  st_body "$st_root/forgejo/ci.yml" a.sh
  st_body "$st_root/github/ci.yml" a.sh
  st_expect ok "identical steps compare equal"

  st_body "$st_root/github/ci.yml" b.sh
  st_expect fail "a one-sided edit to the named step is detected"

  st_body "$st_root/github/ci.yml" a.sh
  sed 's/- name: Planted step/- name: Renamed step/' "$st_root/github/ci.yml" \
    >"$st_root/github/ci.yml.tmp"
  mv "$st_root/github/ci.yml.tmp" "$st_root/github/ci.yml"
  st_expect fail "a step listed in SHARED_STEPS but absent on one forge is detected"

  st_body "$st_root/github/ci.yml" a.sh
  rm -f "$st_root/forgejo/ci.yml"
  st_expect fail "a workflow file missing on one forge is detected"

  rm -rf "$st_root"
  return "$st_fail"
}

# --- main ------------------------------------------------------------------

cd "$(dirname "$0")/.." || exit 1

if [ "${1:-}" = "--self-test" ]; then
  self_test 1 || exit 1
  echo "Self-test OK: the comparison passes on agreement and fails on drift"
  exit 0
fi

if ! self_test 0; then
  echo "FAIL: this gate's own self-test is broken, so its verdict below cannot"
  echo "      be trusted.  Fix scripts/check-workflow-parity.sh."
  exit 1
fi

compare_steps .forgejo/workflows .github/workflows "$SHARED_STEPS" || exit 1
