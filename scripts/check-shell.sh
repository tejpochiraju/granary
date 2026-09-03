#!/bin/sh
# Shell lint gate — shellcheck over scripts/*.sh (#591)
#
# Why this exists.  `scripts/` holds enforcement gates — check-sqlite-policy.sh
# (#370) and check-workflow-parity.sh (#574) — and an enforcement gate breaks by
# PERMITTING things: when it fails, CI stays green and nobody hears about it.
# PR #582 shipped a demonstrated fail-open of exactly that shape (an empty
# allowlist variable made `printf '%s\n' $VAR` emit one blank line, `grep -vFf`
# with a blank pattern matched every line, and the script printed "Policy OK"
# with a planted violation sitting in lib/).  It was caught only because a
# reviewer went looking for fail-open modes by hand: `grep -rn shellcheck
# .forgejo/ .github/` returned nothing, so there was no lint gate on shell at
# all and every shell-script policy was reviewable-only.
#
# What this does and does NOT buy.  shellcheck is a syntactic linter; it would
# not have caught #582's bug, which is a semantic one (an empty pattern file
# means "match everything" to grep).  It catches the neighbouring class —
# unquoted expansions, unassigned references, misused test operators, `set -e`
# traps — which is the class the two gates are written in.  Defence in depth,
# not a proof.
#
# Scope: every `*.sh` under scripts/, at shellcheck's DEFAULT severity.  Info-
# and style-level findings are included deliberately: SC2086 (word splitting) is
# info-level, and word splitting is the mechanism #582's fail-open rode in on.
# Where a script splits deliberately it says so with an inline
# `# shellcheck disable=SC…` and a reason, which is a review artefact rather
# than a silent exemption.
#
# THIS SCRIPT SELF-TESTS ON EVERY RUN (see [self_test]), for the same reason
# check-sqlite-policy.sh does: a gate that has never been observed to FAIL is a
# gate nobody knows works.  Pass `--self-test` to run only the self-test,
# verbosely, and `--list` to print the files it would check.
#
# Usage (from the repo or worktree root, on the host — it self-wraps podman the
# way scripts/check-fmt.sh does, because shellcheck lives in the dev image):
#   sh scripts/check-shell.sh
#
# Override the image with GRANARY_DEV_IMAGE, or the binary with SHELLCHECK.
#
# Refs #591, #574, #370, PR #582.

set -eu

IMAGE="${GRANARY_DEV_IMAGE:-localhost/granary-dev:latest}"
SHELLCHECK="${SHELLCHECK:-shellcheck}"

# --- target collection -----------------------------------------------------
#
# Derived from the tree, never a hand-maintained list: a list would go stale the
# first time somebody adds a script, and the failure mode of a stale list here
# is a file nobody lints — silently.  The empty-set case is checked below for
# the same reason PR #582's empty allowlist was a fail-open.
collect_targets() {
  ct_dir=$1
  find "$ct_dir" -maxdepth 1 -type f -name '*.sh' | sort
}

cd "$(dirname "$0")/.." || exit 1

# Answered before the linter is looked for: listing the targets is useful
# precisely when shellcheck is missing and you want to know what the gate would
# have covered.
if [ "${1:-}" = "--list" ]; then
  collect_targets scripts
  exit 0
fi

# shellcheck lives in the dev image and in the CI lint job's apt packages.  If
# it is not on PATH we are on a bare host: re-exec this same script inside the
# container, where it IS on PATH, so there is no re-exec loop.  In CI the `if`
# is false and nothing is wrapped — which is what keeps the gate independent of
# podman being available on the runner.
if ! command -v "$SHELLCHECK" >/dev/null 2>&1; then
  if command -v podman >/dev/null 2>&1; then
    # The argument list is deliberately split back apart inside the container.
    # shellcheck disable=SC2086
    exec podman run --rm --user 0 -v "$PWD":/work -w /work "$IMAGE" \
      sh -c "sh scripts/check-shell.sh $*"
  fi
  echo "FAIL: '$SHELLCHECK' is not on PATH and podman is not available to"
  echo "      re-exec this inside $IMAGE."
  echo
  echo "      This is deliberately a FAILURE and not a skip.  A lint gate that"
  echo "      silently passes when its linter is missing is worse than no gate:"
  echo "      it reports green for a tree nobody checked.  Install shellcheck"
  echo "      (apt-get install shellcheck), rebuild the dev image, or point"
  echo "      SHELLCHECK at a binary."
  exit 1
fi

# --- self-test -------------------------------------------------------------
#
# Plants a script with a genuine violation and asserts the linter FAILS on it,
# and a trivially clean one and asserts it PASSES.  Both halves matter: the
# first proves the gate can still fail (the thing #591 is about), the second
# proves a green run means something rather than the linter erroring out on
# every input.
self_test() {
  st_verbose=$1
  # `set -e` is suspended for a function invoked in a condition, so an
  # unchecked mktemp failure would carry on with root='' and write into /.
  # Check it, the way check-sqlite-policy.sh's self-test does.
  st_root=$(mktemp -d) || return 1
  case ${st_root:-} in
    /*/*) ;;
    *) return 1 ;;
  esac
  st_fail=0

  # A violation shellcheck reports at default severity in POSIX-sh mode:
  # SC2086, an unquoted expansion that word-splits.  Chosen because it is the
  # mechanism behind PR #582's fail-open, and because it is stable across
  # shellcheck versions.
  cat >"$st_root/dirty.sh" <<'EOF'
#!/bin/sh
target=$1
rm -f $target
EOF
  cat >"$st_root/clean.sh" <<'EOF'
#!/bin/sh
set -eu
echo "clean"
EOF

  if "$SHELLCHECK" "$st_root/dirty.sh" >"$st_root/dirty.out" 2>&1; then
    echo "FAIL (self-test): shellcheck accepted a script with an unquoted"
    echo "      expansion, so this gate would not catch one in scripts/."
    [ "$st_verbose" -eq 0 ] || cat "$st_root/dirty.out"
    st_fail=1
  elif [ "$st_verbose" -ne 0 ]; then
    echo "OK (self-test): a planted violation is rejected"
  fi

  if "$SHELLCHECK" "$st_root/clean.sh" >"$st_root/clean.out" 2>&1; then
    [ "$st_verbose" -eq 0 ] || echo "OK (self-test): a clean script is accepted"
  else
    echo "FAIL (self-test): shellcheck rejected a trivially clean script, so a"
    echo "      failure of this gate would not mean what it says."
    cat "$st_root/clean.out"
    st_fail=1
  fi

  rm -rf "$st_root"
  return "$st_fail"
}

# --- main ------------------------------------------------------------------

if [ "${1:-}" = "--self-test" ]; then
  self_test 1 || exit 1
  echo "Self-test OK: the linter rejects a planted violation and accepts a clean script"
  exit 0
fi

if ! self_test 0; then
  echo "FAIL: this gate's own self-test is broken, so its verdict below cannot"
  echo "      be trusted.  Fix scripts/check-shell.sh or the shellcheck install."
  exit 1
fi

# Via a file rather than `for f in $(...)`: a bare unquoted expansion is the
# construct this gate exists to complain about, and a loop body that ran in a
# pipeline's subshell could not report its verdict back through $status.
targets=$(mktemp) || exit 1
collect_targets scripts >"$targets"

if [ ! -s "$targets" ]; then
  rm -f "$targets"
  echo "FAIL: no *.sh found under scripts/."
  echo
  echo "      An empty target set is treated as a failure, not a pass: it is"
  echo "      indistinguishable from 'the scan is broken', and reporting green"
  echo "      for zero files checked is precisely the fail-open shape #591 was"
  echo "      filed about.  Run this from the repository root."
  exit 1
fi

count=0
status=0
while IFS= read -r f; do
  [ -n "$f" ] || continue
  count=$((count + 1))
  if ! "$SHELLCHECK" "$f"; then status=1; fi
done <"$targets"
rm -f "$targets"

echo
if [ "$status" -ne 0 ]; then
  echo "Shell lint FAILED ($count file(s) checked)."
  echo "Fix the finding, or — where the construct is deliberate — annotate it"
  echo "with an inline '# shellcheck disable=SCxxxx' AND a comment saying why."
  exit 1
fi
echo "Shell lint OK: $count file(s) under scripts/ pass shellcheck ($("$SHELLCHECK" --version | awk '/^version:/ {print $2}'))."
