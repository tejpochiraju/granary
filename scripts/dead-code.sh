#!/bin/sh
# Advisory dead-code sweep (#160): reports exported values, constructors and
# record fields that nothing in lib/, test/ or bin/ references.
#
# This is the missing piece of #160's acceptance criteria — the tool, the dev
# image, the baseline and docs/DEAD_CODE.md all already existed, but there was
# no single command to run it, so the documented two-step invocation had to be
# retyped from the docs each time (and step 1 is easy to skip, which silently
# triples the finding count).
#
# It is ADVISORY and always exits 0 on a successful analysis. It is deliberately
# NOT a CI gate: ~19 of the findings are `pp` functions that merlint REQUIRES,
# so the two linters contradict each other and a gate would be unsatisfiable.
# See "Why it is not a gate" in docs/DEAD_CODE.md.
#
# Usage (from the repo or worktree root, on the host — it self-wraps podman):
#   sh scripts/dead-code.sh              # full sweep: @check, then analyse
#   sh scripts/dead-code.sh --no-build   # analyse an existing _build
#
# Override the image with GRANARY_DEV_IMAGE if needed.

IMAGE="${GRANARY_DEV_IMAGE:-localhost/granary-dev:latest}"
BASELINE=56

# dead_code_analyzer lives only in the dev image. If it is not already on PATH
# we are on the host: re-exec this same script inside the container under
# `opam exec`. The inner run finds the binary and proceeds, so there is no loop.
#
# --user 0 is required for the same reason check-fmt.sh needs it: under
# rootless podman the image's default `opam` user maps to an unrelated subuid,
# so a _build it creates cannot be removed later without `podman unshare`.
if ! command -v dead_code_analyzer >/dev/null 2>&1; then
  # The argument list is deliberately split back apart inside the container.
  # shellcheck disable=SC2086
  exec podman run --rm --user 0 -v "$PWD":/work -w /work "$IMAGE" \
    sh -c "opam exec -- sh scripts/dead-code.sh $*"
fi

do_build=1
for arg in "$@"; do
  case "$arg" in
    --no-build) do_build=0 ;;
    -h|--help)  sed -n '2,22p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) printf 'dead-code.sh: unknown argument: %s\n' "$arg" >&2; exit 2 ;;
  esac
done

if [ "$do_build" -eq 1 ]; then
  printf '==> dune build @check (required — see below)\n'
  # @check is the whole trick: a plain `dune build` emits only ONE test .cmt,
  # so every value called only from the test suite looks unreferenced and the
  # count balloons from 56 to 185. @check builds .cmt files for every stanza
  # without linking or running them.
  #
  # It is EXPECTED to fail on test/bench_compare.ml, which needs the `sqlite3`
  # opam library that the dev image omits by design. That stanza is (optional),
  # so an ordinary build skips it silently; @check asks every stanza for a .cmt
  # whether or not its libraries resolve. dune still emits the other test .cmt
  # files before it stops, so the analysis below is unaffected. Hence `|| true`
  # rather than a failure — but the output is shown, so a DIFFERENT failure is
  # still visible to the reader.
  dune build @check 2>&1 | sed 's/^/    /'
  printf '    (a bench_compare failure above is expected: no sqlite3 in the image)\n'
fi

if [ ! -d _build/default/lib ]; then
  printf 'dead-code.sh: _build/default/lib is missing — run without --no-build.\n' >&2
  exit 2
fi

printf '\n==> dead_code_analyzer (advisory)\n'
out=$(dead_code_analyzer \
        --references _build/default/test \
        --references _build/default/bin \
        -E all -M all \
        _build/default/lib 2>&1) || {
  printf '%s\n' "$out" >&2
  printf 'dead-code.sh: analyzer failed.\n' >&2
  exit 2
}
printf '%s\n' "$out"

# The analyzer prints one finding per line in its sections; count the lines that
# carry a source location, which is stable across its section headings.
count=$(printf '%s\n' "$out" | grep -cE '^ *[^ ].*\.mli?:[0-9]+' 2>/dev/null || true)
[ -n "$count" ] || count=0

printf '\n----------------------------------------------------------------\n'
printf 'findings: %s   (docs/DEAD_CODE.md baseline: %s)\n' "$count" "$BASELINE"
if [ "$count" -gt "$BASELINE" ] 2>/dev/null; then
  printf 'ABOVE the recorded baseline. Triage new entries into (a) genuine dead\n'
  printf 'code, (b) intentional public API, (c) false positive — then update\n'
  printf 'the baseline in docs/DEAD_CODE.md and this script.\n'
elif [ "$count" -lt "$BASELINE" ] 2>/dev/null; then
  printf 'BELOW the recorded baseline — lower it in docs/DEAD_CODE.md and this\n'
  printf 'script so the number keeps meaning something.\n'
fi
printf 'Advisory only: ~19 findings are pp functions that merlint REQUIRES, so\n'
printf 'this is deliberately not a CI gate. See docs/DEAD_CODE.md.\n'
exit 0
