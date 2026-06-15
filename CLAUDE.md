# sqlocaml — Agent Guide

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
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune build

# test
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test

# specific test
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_foo.exe
```

From inside a worktree, substitute the worktree path for `$(pwd)`.

### Formatting

`dune build @fmt` has git interaction issues in worktrees. Use ocamlformat directly and redirect to host:

```sh
# check
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev ocamlformat --check lib/foo.ml

# fix (write to host via redirect)
tmp=$(mktemp) && podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev \
  ocamlformat lib/foo.ml > "$tmp" && mv "$tmp" lib/foo.ml && chmod 644 lib/foo.ml
```

The pre-commit hook runs format checks automatically on staged `.ml`/`.mli` files.

### Coverage

Use the manual binary loop, not `dune runtest --instrument-with=bisect_ppx` (requires RPC daemon):

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev \
  bisect-ppx-report html --output _coverage_report/ _build/default/test/*.coverage
```

### Opening a PR

When your work is ready:

```sh
git push origin <branch-name>
~/.local/bin/forgejo pr create tej/sqlite_ocaml_port \
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

"File an issue" means Forgejo (`tej/sqlite_ocaml_port`), not GitHub or any third-party URL.

```sh
~/.local/bin/forgejo issue create tej/sqlite_ocaml_port --title="..." --body="..."
```

### Testing standards

- Target 100% line coverage on every module.
- Add QCheck property tests for every non-trivial function (see existing tests for patterns).
- Tests that require real SQLite are gated by the `SQLOCAML_TEST_SQLITE` env var (see `test/compare_sqlite/`).

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

Binary: `~/.local/bin/forgejo`. Repo slug: `tej/sqlite_ocaml_port`.

Common commands:

```sh
forgejo issue list tej/sqlite_ocaml_port
forgejo issue view tej/sqlite_ocaml_port <N>
forgejo issue close tej/sqlite_ocaml_port <N>
forgejo pr list tej/sqlite_ocaml_port
forgejo pr view tej/sqlite_ocaml_port <N>
forgejo pr review tej/sqlite_ocaml_port <N> --approve
forgejo pr merge tej/sqlite_ocaml_port <N> --method=squash
```
