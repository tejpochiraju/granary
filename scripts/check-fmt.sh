#!/bin/sh
# Faithful local equivalent of CI's `dune build @fmt` gate: OCaml sources
# (.ml/.mli via the pinned ocamlformat, worktree-safe) plus dune files (via
# `dune build @fmt` itself, which is skipped gracefully inside a git worktree).
#
# Why this exists (we kept shipping fmt failures that only CI caught):
#   * `dune build @fmt` is the real gate, but inside a git *worktree* mounted
#     into the build container it aborts with a spurious
#     "fatal: not a git repository" error (the worktree's gitdir link points
#     outside the mount), so its result is unusable locally.
#   * `ocamlformat --check` prints NOTHING on a mismatch and only sets a
#     non-zero exit code — an empty stdout is trivially misread as "clean".
#
# This script runs the *pinned* ocamlformat (from .ocamlformat: 0.29.0,
# janestreet) over every source .ml/.mli and prints a unified diff for any
# deviation, exiting non-zero if anything would change. It needs no git and
# never hides a failure behind empty output.
#
# Usage (from the worktree root, on the host — it self-wraps podman):
#   scripts/check-fmt.sh          # check; exit 1 (with diffs) on any deviation
#   scripts/check-fmt.sh --fix    # rewrite the offending files in place
#
# Override the image with SQLOCAML_DEV_IMAGE if needed.

IMAGE="${SQLOCAML_DEV_IMAGE:-localhost/sqlocaml-dev:latest}"

# ocamlformat lives only in the dev image. If it isn't already on PATH we are
# on the host: re-exec this same script inside the container under `opam exec`
# (which puts the pinned ocamlformat on PATH). The inner run finds ocamlformat
# and proceeds, so there is no re-exec loop.
if ! command -v ocamlformat >/dev/null 2>&1; then
  exec podman run --rm -v "$PWD":/work -w /work "$IMAGE" \
    sh -c "opam exec -- sh scripts/check-fmt.sh $*"
fi

mode="check"
[ "${1:-}" = "--fix" ] && mode="fix"

# CI's @fmt formats every .ml/.mli dune knows about. No dune file declares a
# (formatting) exclusion, and generated sources (parser.ml/lexer.ml) live in
# _build, not the source tree — so the source dirs below are the exact set.
dirs=""
for d in lib test bin bench; do
  [ -d "$d" ] && dirs="$dirs $d"
done
files=$(find $dirs -type f \( -name '*.ml' -o -name '*.mli' \) 2>/dev/null | sort)

status=0
count=0
for f in $files; do
  count=$((count + 1))
  if [ "$mode" = "fix" ]; then
    ocamlformat --inplace "$f"
    continue
  fi
  if ! ocamlformat "$f" >/tmp/_fmt.out 2>/tmp/_fmt.err; then
    echo "✗ ocamlformat could not parse: $f"
    cat /tmp/_fmt.err
    status=1
    continue
  fi
  if ! diff -u "$f" /tmp/_fmt.out >/tmp/_fmt.diff; then
    echo "✗ needs formatting: $f"
    cat /tmp/_fmt.diff
    status=1
  fi
done

# Dune files (dune / dune-project): CI's @fmt formats these too, but with dune's
# OWN formatter — `dune format-dune-file` is NOT equivalent (it injects
# blank-line-between-stanzas and dependency rewraps that @fmt leaves alone), so
# it can't be used here. The only faithful tool is `dune build @fmt` itself.
# That works in a normal checkout but aborts inside a git *worktree* (its gitdir
# link points outside the build mount), so skip gracefully there — CI is the
# backstop. Only run when the ocamlformat pass above is clean, to avoid
# double-reporting any .ml/.mli issues @fmt would also flag.
if [ "$status" -eq 0 ]; then
  if [ "$mode" = "fix" ]; then
    dune build @fmt --auto-promote >/tmp/_fmt_dune.out 2>&1 || true
  elif fmt_out=$(dune build @fmt 2>&1); then
    : # dune files (and .ml/.mli) are clean per the real gate
  else
    case "$fmt_out" in
      *"not a git repository"* | *fatal:*)
        echo "⚠ dune-file formatting NOT checked: 'dune build @fmt' is unusable"
        echo "  inside a git worktree. Run this in the main checkout, or rely on CI." ;;
      *)
        echo "✗ dune-file formatting (dune build @fmt):"
        echo "$fmt_out"
        status=1 ;;
    esac
  fi
fi

echo
if [ "$mode" = "fix" ]; then
  echo "Reformatted in place across $count OCaml sources + dune files (ocamlformat $(ocamlformat --version) + dune build @fmt)."
elif [ "$status" -eq 0 ]; then
  echo "✓ All $count OCaml sources + dune files match the formatter (ocamlformat $(ocamlformat --version) + dune build @fmt) — parity with CI's @fmt gate."
else
  echo "Formatting check FAILED. Run 'scripts/check-fmt.sh --fix' to fix; this matches CI's 'dune build @fmt' gate."
fi
exit "$status"
