# granary — Agent Guide

Pure-OCaml SQL engine targeting MirageOS. All implementation is in `lib/`; tests in `test/`; benchmarks in `bench/`.

## Working in this repo

**All work must go through a PR. Never commit directly to `main`.**

Push protection is enforced both locally (pre-push hook) and on the remote (Forgejo branch protection). Direct pushes to `main` will be rejected.

### Starting work on a task

Always create a git worktree so your work is isolated from `main`:

```sh
git worktree add .worktrees/<short-name> -b <branch-name>
```

Branch naming: `feat/NNN-description`, `fix/NNN-description`, `cleanup/NNN-description` where NNN is the Forgejo issue number. Example: `feat/381-btree-cursor`.

Worktrees live in `.worktrees/` which is gitignored. After the branch is created, `cd` into the worktree and do all work there.

**Worktree container permission**: the container user can't write into a worktree unless you chmod it first:

```sh
chmod 777 .worktrees/<short-name>
```

### Building and testing

Never call `dune` directly on the host. All OCaml build commands run inside the dev container:

```sh
# build
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build

# test
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test

# specific test
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune test test/test_foo.exe
```

From inside a worktree, substitute the worktree path for `$(pwd)`.

### Formatting

`dune build @fmt` has git interaction issues in worktrees. Use ocamlformat directly and redirect to host:

```sh
# check
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev ocamlformat --check lib/foo.ml

# fix (write to host via redirect)
tmp=$(mktemp) && podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev \
  ocamlformat lib/foo.ml > "$tmp" && mv "$tmp" lib/foo.ml && chmod 644 lib/foo.ml
```

The pre-commit hook runs format checks automatically on staged `.ml`/`.mli` files.

### Before pushing: dune-file formatting + merlint

The CI **lint** job runs `dune build @fmt` (which checks dune-file formatting *and* ocamlformat) and `merlint`. Formatting only your `.ml`/`.mli` with ocamlformat is **not** enough — CI will still fail on unformatted `dune` files or merlint findings. Run both locally before pushing:

```sh
# dune-file formatting — @fmt can't run in a worktree (the worktree .git is a
# file → "fatal: not a git repository"), so use the standalone formatter:
tmp=$(mktemp) && podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev \
  dune format-dune-file path/to/dune > "$tmp" && mv "$tmp" path/to/dune && chmod 644 path/to/dune
# verify a dune file is already formatted (expect no diff):
diff <(podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune format-dune-file path/to/dune) path/to/dune

# merlint — run from the workspace root; expect 0 issues for your files
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint
```

merlint enforces (among others): max nesting depth 4, every library module has a `.mli`, an abstract `type t` has a `pp`, and every public `val` in an `.mli` has a `(** … *)` doc comment (not `(* … *)`). It ignores the pre-existing `sqlite3 not found` build warning and still reports.

### Coverage

`dune runtest --instrument-with bisect_ppx` works (verified on 5.4). Two
prerequisites, both of which the `coverage.yml` workflow also applies:

- `bisect_ppx` must be pinned to the unmerged upstream PR that supports the
  OCaml 5.2+ AST — the released 2.8.3 caps `ppxlib < 0.36`, which is
  incompatible with the `lwt_ppx 6.1` / `ppxlib 0.38` this project needs (#166).
- The `bench_*` timing gates must be neutralized; instrumentation slows
  everything enough that they fail near-deterministically.

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace \
  -e GRANARY_BENCH_MIN_SPEEDUP=0 -e GRANARY_BENCH_PARALLEL_MAX=1000000 \
  -e GRANARY_BENCH_MAX_WRITER_S=1000000 -e GRANARY_BENCH_MAX_RATIO=1000000 \
  granary-dev bash -c '
    opam pin add -y -k git bisect_ppx \
      "https://github.com/patricoferris/bisect_ppx.git#7061d643ff492b0045796357ee6917ded21fb1f0"
    dune runtest --instrument-with bisect_ppx
    bisect-ppx-report html -o _coverage_report/ $(find _build -name "*.coverage")
  '
```

Handwritten coverage (excluding the generated parser/lexer) is ~80%; CI gates
tags at 75%. Note the CI image ships **mawk** and has neither `python3` nor
`bc` — keep report post-processing to portable awk.

### Opening a PR

When your work is ready:

```sh
git push origin <branch-name>
~/.local/bin/forgejo pr create tej/granary \
  --title="feat(#NNN): short description" \
  --head=<branch-name> \
  --base=main \
  --body="$(cat <<'EOF'
## Summary
- what changed and why

## Test plan
- [ ] dune test passes
- [ ] relevant new tests added

Closes #NNN
EOF
)"
```

### Filing issues

"File an issue" means Forgejo (`tej/granary`), not GitHub or any third-party URL.

```sh
~/.local/bin/forgejo issue create tej/granary --title="..." --body="..."
```

### Testing standards

- Target 100% line coverage on every module.
- Add QCheck property tests for every non-trivial function (see existing tests for patterns).
- Tests that require real SQLite are gated by the `GRANARY_TEST_SQLITE` env var (see `test/compare_sqlite/`).

## Repository structure

```
lib/          — engine: parser, planner, executor, storage, WAL, columnar store
test/         — unit + integration + QCheck + Jepsen harness
bench/        — microbenchmarks (criterion-style)
bin/          — CLI entry point
.worktrees/   — gitignored; agent worktrees live here
.claude/      — Claude Code project settings (gitignored)
```

## Forgejo CLI reference

Binary: `~/.local/bin/forgejo`. Repo slug: `tej/granary`.

Common commands:

```sh
forgejo issue list tej/granary
forgejo issue view tej/granary <N>
forgejo issue close tej/granary <N>
forgejo pr list tej/granary
forgejo pr view tej/granary <N>
forgejo pr review tej/granary <N> --approve
forgejo pr merge tej/granary <N> --method=squash
```
