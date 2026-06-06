#!/bin/sh
# Faithful local equivalent of CI's `dune build @fmt` gate for OCaml sources.
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

echo
if [ "$mode" = "fix" ]; then
  echo "Reformatted in place across $count source files (ocamlformat $(ocamlformat --version))."
elif [ "$status" -eq 0 ]; then
  echo "✓ All $count OCaml sources match ocamlformat $(ocamlformat --version) — parity with CI's dune build @fmt gate."
else
  echo "Formatting check FAILED. Run 'scripts/check-fmt.sh --fix' to fix; this matches CI's 'dune build @fmt' gate."
fi
exit "$status"
