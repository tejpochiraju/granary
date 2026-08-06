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

**Never use `git stash`.** `refs/stash` is one repo-wide stack, not per-worktree, and several agents work in sibling worktrees at once — a `pop` in one worktree can apply another's changes and silently empty its tree (2026-08-01, #527 and #514).

To measure before/after numbers, use one of the three alternatives below — they are a menu, not a sequence. `SP` is your session scratchpad directory and `529` stands for your worktree; set both to your own:

```sh
SP=<your session scratchpad>; WT=529   # prefix every scratch file with $WT

# 1. WIP commit — preferred: no shared namespace to collide in
git commit -m wip
git checkout HEAD~1 -- lib/        # old code
git checkout HEAD   -- lib/        # back to WIP

# 2. or: patch aside
git add -N .                       # so new files are seen
git diff HEAD > "$SP/$WT-wip.patch"

# 3. or: copy aside
cp lib/sql/exec.ml "$SP/$WT-exec.ml.good"
```

**Name every scratch file after your worktree** — `$SP/529-exec.ml.good`, never `$SP/exec.ml.good`. The session scratchpad is shared by *all* agents in the session, not one per worktree. On 2026-08-01 two agents each saved `exec.ml.good` there; the second overwrote the first, and the first restored the other's `exec.ml` into its own tree. A generic filename in a shared directory is a stash by another name — `refs/stash` was one instance of the pattern, not the pattern.

Plain `git diff` captures neither staged nor untracked changes — use `git diff HEAD` after `git add -N`. `git checkout HEAD~1 -- lib/` also will not delete files that exist only in the WIP commit; remove those by hand. Commit early on your own branch regardless: an uncommitted tree is the only thing at risk.

After any recovery, verify with `md5sum` against a known-good source rather than assuming — both incidents were caught that way, and in both the timestamps looked innocent.

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

### Timing and scaling gates (`GRANARY_BENCH_*`)

Several tests assert a wall-clock ratio or ceiling rather than a value. They
guard regressions that are invisible to a correctness test — an O(n) drain, an
O(n²) bulk insert, a lost reader/writer overlap:

| test | guards | gate | neutralizer |
|---|---|---|---|
| `test_fts_scaling` | #233 FTS `cursor_open` drain | 3x index < 2.0x slower | `GRANARY_BENCH_MAX_RATIO` |
| `test_insert_scaling` | #228/#229 O(n²) insert, full-scan lookup | ratio < 2.5, speedup ≥ 4.0 | `GRANARY_BENCH_MAX_RATIO`, `GRANARY_BENCH_MIN_SPEEDUP` |
| `bench_wal_fsync_overlap` | #149/#159 fsync overlap | overlap ≤ 1.15, speedup ≥ 1.2 | `GRANARY_BENCH_MIN_SPEEDUP` |
| `bench_wal_reader_scaling` | #149 parallel-read regression | parallel ≤ 2.0x serial | `GRANARY_BENCH_PARALLEL_MAX` |
| `bench_slow_read_yield` | reader starving the writer | writer ≤ 3.0 s | `GRANARY_BENCH_MAX_WRITER_S` |

One gate is **not** wall-clock and therefore **not** neutralized anywhere:

| test | guards | gate | knob |
|---|---|---|---|
| `test_not_null_600` | #600 `PRAGMA not_null_check` retaining every violating row | marginal peak live heap < 4 words/row when the table doubles | `GRANARY_MEM_MAX_WORDS_PER_ROW` |

It measures *allocation* (peak live major-heap words, sampled through a `Gc`
alarm), so a loaded runner does not move it — ±0.02% across runs, which no
wall-clock gate manages. That is why it runs armed in `ci.yml`, `coverage.yml`
and `cross-arch.yml` alongside the ones those jobs disarm. What load cannot
change, a different allocator or word size can, and `cross-arch.yml`'s arm64
arm has never run it, so `GRANARY_MEM_MAX_WORDS_PER_ROW` exists as the escape
hatch — it raises the ceiling without disabling the correctness assertions the
same test makes. Reach for it only after ruling out the thing it guards: the
measured slopes are ≈20-23 words/row retaining (19.61-23.52 across three runs;
the 40 000-row point is the noisy one) and 1.6-1.9 counting, so a failure
anywhere between those two bands is a regression, not a platform difference.

**Where they run armed.** `ci.yml`, `coverage.yml` and `cross-arch.yml` — all
six files, Forgejo and GitHub — neutralize every one of them, because those
jobs share a loaded runner with every other PR. The single *scheduled* job that
runs them with their ceilings live is `.forgejo/workflows/bench-nightly.yml`:
`dune runtest --force -j 1`, no `GRANARY_BENCH_*` neutralizer, on the
self-hosted runner, reporting via an auto-filed issue rather than blocking PRs
(#549). It shares the `nightly` concurrency group with `jepsen-nightly.yml` so
the two heavy nightlies queue instead of perturbing each other's timings.

The `.github/` mirror of that file is **manual-only** (`workflow_dispatch`, no
`schedule`) and that is deliberate: GitHub's `ubuntu-latest` is a shared 2-core
VM, so a scheduled armed run there would auto-file a recurring public issue
nobody acts on and train everyone to ignore the marker — #549's disease, not
its cure. Treat a red run there as weak evidence.

Do not "fix" a red nightly by adding a neutralizer to that workflow — that is
the honour system #549 removed. Anywhere else, these gates are armed only when
*you* run the suite locally and read the result.

**Tuning knobs, not gates.** `GRANARY_BENCH_TRIALS` (best-of-N draws;
`test_fts_scaling` default 9, `bench_wal_fsync_overlap` default 3) and
`GRANARY_BENCH_REPS` (`test_fts_scaling` iterations per timed loop, default
500) only reduce variance — they cannot make a failing test pass. On a loaded
box, raise these rather than disarming the ceiling.

If a gate fails on your box, check `uptime` first: sibling agents running
suites in parallel are the usual cause, and the gates print their per-trial
numbers so you can tell noise from a regression.

### Formatting

**Use `scripts/check-fmt.sh` — it is the canonical local equivalent of CI's `dune build @fmt` gate.** It self-wraps podman (no manual `podman run`), runs the pinned ocamlformat over every `.ml`/`.mli` in `lib/ test/ bin/ bench/`, prints a unified diff for each deviation, and exits non-zero. Run it from the repo or worktree root:

```sh
sh scripts/check-fmt.sh          # check; exits 1 and prints diffs on any deviation
sh scripts/check-fmt.sh --fix    # rewrite the offending files in place
```

Read its final summary line, not just the exit code:

- `✓ … parity with CI's @fmt gate` — OCaml sources **and** dune files verified.
- `◐ … Dune files UNVERIFIED` — OCaml sources are clean but the dune-file check was skipped (see below). Exits 0; **not** full parity with CI.

Why not the raw tools:

- `dune build @fmt` is the real gate and works in the **main checkout**, but inside a **worktree** it aborts with `fatal: not a git repository` (the worktree's `.git` is a file pointing outside the container mount). It only consults git when it has a change to *promote*, so in a worktree it passes silently while everything is clean and fails only once a file deviates — success there proves nothing.
- `ocamlformat --check` reports a mismatch through the **exit code only**, printing nothing at all. Testing its captured output for emptiness reports every file as falsely clean. This trap sank five PRs (#213, #253, #277, #287, #308) before the script existed.
- **`dune format-dune-file` is NOT equivalent to `@fmt` for dune files** — verified 2026-07-30: on `dune-project` it wants 47 lines changed (blank lines between stanzas, dependency constraints rewrapped) that `@fmt` accepts as-is. Do not use it to "fix" dune files; it also makes the `diff`-expects-nothing check a false failure.
- A hand-rolled `podman run … ocamlformat --inplace` **fails silently**: under rootless podman the image's default `opam` user maps to an unrelated subuid, so every checkout file looks root-owned and `--inplace` exits 2 without writing. The script passes `--user 0` (host user → container root), which writes correctly and preserves `tej:tej` ownership. If you must invoke a container by hand and need it to *write* into the checkout, pass `--user 0` too.

The pre-commit hook runs format checks automatically on staged `.ml`/`.mli` files.

### Before pushing: dune-file formatting + merlint

The CI **lint** job runs `dune build @fmt` (dune-file formatting *and* ocamlformat) plus `merlint`. Formatting only your `.ml`/`.mli` is **not** enough — CI still fails on unformatted `dune` files or merlint findings.

```sh
# formatting (both .ml/.mli and dune files) — see above
sh scripts/check-fmt.sh

# merlint — run from the workspace root; expect 0 issues for your files
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint
```

**If you touched a `dune` or `dune-project` file while working in a worktree**, `check-fmt.sh` will report `◐ … Dune files UNVERIFIED`. To actually verify them before pushing, run the real gate in the main checkout (it needs no worktree state — the dune files are what matter):

```sh
cd /home/tej/projects/sqlite_ocaml_port   # the main checkout, not a worktree
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev dune build @fmt
```

merlint enforces (among others): max nesting depth 4, every library module has a `.mli`, an abstract `type t` has a `pp`, and every public `val` in an `.mli` has a `(** … *)` doc comment (not `(* … *)`). It ignores the pre-existing `sqlite3 not found` build warning and still reports.

### Dead-code analysis

`dead_code_analyzer` is in the dev image and is **advisory, not a CI gate**.
Run it before a release or when pruning, and always after `dune build @check` —
a plain `dune build` emits one test `.cmt`, which makes the whole public API
look dead (185 findings instead of 56). Note that ~19 findings are `pp`
functions that merlint *requires*; the two linters collide there. Full
instructions, the current baseline, and the OCaml 5.4 fork pin are in
`docs/DEAD_CODE.md`.

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
~/.local/bin/forgejo pr create IoTReadyNext/granary \
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

"File an issue" means Forgejo (`IoTReadyNext/granary`), not GitHub or any third-party URL.

```sh
~/.local/bin/forgejo issue create IoTReadyNext/granary --title="..." --body="..."
```

### Testing standards

- Target 100% line coverage on every module.
- Add QCheck property tests for every non-trivial function (see existing tests for patterns).
- Tests that require real SQLite live in `test/test_sqlite_compare.ml`; they skip gracefully via `sqlite3_available ()` (`:18`) when `sqlite3` is not in `PATH`, and its header comment has the exact `podman run` invocation that mounts the host `sqlite3` and its libs into the container. **There is no `test/compare_sqlite/` directory** — that path was in this file for a while and sent agents looking for a harness they concluded did not exist.
- Nothing in the tree reads `GRANARY_TEST_SQLITE`; it is a phantom invented by the same stale sentence. The SQLite comparison tests gate on `sqlite3` in `PATH`; the slow TPC-C smoke run gates on `GRANARY_TPCC_SMOKE` (`test/test_tpcc_smoke.ml:238`).
- Granary is **inspired by** SQLite, not a port of it. When pinning a behaviour that differs from SQLite, record the divergence deliberately rather than assuming parity is the goal — e.g. since #530 every PRIMARY KEY column implies NOT NULL, whichever way the key is spelled, where SQLite leaves PK columns nullable on *rowid* tables (its legacy behaviour; it does enforce PK NOT NULL on `WITHOUT ROWID` tables, which this engine also supports). `ALTER TABLE ... ADD COLUMN ... PRIMARY KEY` is refused outright for the same reason: that path built no backing index, so it could only ever have produced a "primary key" that was not one.
- **NaN is a value here, and it sorts below every number (#536, decided 2026-08-02).**
  SQLite has no NaN at all: `sqlite3_bind_double(NaN)` binds NULL, and a NaN
  expression result is NULL, so `o >= ?` bound to NaN is *unknown* and returns
  no rows. Granary instead keeps NaN as a real value in a total order, and
  diverges from SQLite in **both** directions — `o >= NaN` returns every row,
  `o <= NaN` returns none. Two mechanisms have to agree for that to be sound:
  - `Exec.cmp_result` promotes cross-type numeric operands with `Float.compare`,
    whose total order puts NaN below `neg_infinity`.
  - `Index_key.encode_value` writes NaN as the single `0x00` NULL/NaN byte,
    which also sorts below every other key.

  **They agree that NaN sorts below every number, and that — not blanket
  agreement — is what keeps this from being a rows-lost bug.** (They do *not*
  agree about NaN vs NULL; see below. That costs no rows on the read path,
  because a seek over the shared `0x00` prefix still runs the residual
  predicate.) A seek and its residual predicate can never disagree about where
  NaN sits relative to a number, so no future change may move one of the two
  without the other, or an index seek will start skipping rows the predicate
  would have kept. Option A (fold NaN to NULL at the value-ingress points,
  matching SQLite) remains the only variant worth the disruption; option B
  (NULL comparison, unchanged storage) was rejected precisely because it breaks
  that agreement.

  `nan_and_infinite_bounds_are_sound` in `test/test_range_bound_517.ml` pins it
  (300 rows for a NaN lower bound, 0 for an upper one).

  **The agreement is only about NaN vs *numbers*.** The rest of the engine was
  surveyed when #536 was decided, and NaN vs *NULL* is where the two levels part
  company — `Exec.compare_values` puts `NULL < NaN < every number`, while the
  index encoding makes NaN and NULL the same `0x00` byte, i.e. *equal*. The
  value-level answers are all coherent with the decided order and need no
  change:
  - **ORDER BY** (`compare_with_nulls`) places NULLs by an explicit flag and
    sends NaN to `compare_values`, so NaN sorts after NULLs and before every
    other real.
  - **DISTINCT** and hash joins dedup on `row_key`'s `%h` rendering, so all NaNs
    collapse to one and none collapses into NULL.
  - **GROUP BY** / window PARTITION BY group on `compare_values = 0`, so two
    NaNs group together and NaN never groups with NULL — consistent with
    DISTINCT.
  - **MIN/MAX** skip NULLs and use `compare_values`, so `MIN` over a REAL column
    containing NaN returns NaN and `MAX` only does when NaN is the sole
    non-NULL value.

  **Index-keyed uniqueness is the one that does NOT agree, and it is a real
  bug — #578, not part of this decision.** The UNIQUE NULL exemption
  (`any_null_val`) tests the *value*, so a NaN is correctly not exempted; but
  the conflict probe then compares *encoded bytes*, where NaN's key is
  byte-identical to NULL's. So `INSERT NULL` then `INSERT NaN` raises a spurious
  `UNIQUE constraint failed`, while the reverse order succeeds. Do not "fix"
  that by making the exemption byte-based — that would silently exempt NaN from
  uniqueness altogether. **Option C should not be treated as fully settled while
  that stands**: an order-dependent `UNIQUE` on any nullable REAL column is a
  live defect, and if it is fixed by giving NaN its own tag byte, the "NaN is a
  value in a total order" position gets stronger rather than weaker.

  One further comparator defect surfaced in the same survey and is *not*
  NaN-specific: `Exec.compare_values` returns `0` for any int-vs-real pair (its
  `| _, _ -> 0` catch-all), while `Exec.cmp_result` promotes through
  `Float.compare`. Strict column typing hides this for stored columns, but
  `ORDER BY` over a mixed *computed* column returns rows unsorted. Tracked as
  #579. There are four independent value comparators in the tree
  (`compare_values`, `cmp_result`, `row_key`'s string rendering, and
  `Reactive_view.value_compare`) and they do not all agree.

- **`OR IGNORE` skips a NOT NULL violation; `OR REPLACE` raises on one (#599, decided 2026-08-02).**
  A conflict-resolution modifier means the same thing for NOT NULL as it does
  for UNIQUE. `OR IGNORE` skips the offending row — consistent with the UNIQUE
  path, with the modifier's own meaning, and (incidentally, not decisively)
  with SQLite. Every other resolution raises: bare, `OR ABORT`, `OR FAIL`,
  `OR ROLLBACK` and `OR REPLACE`.

  **`OR REPLACE` is the divergence.** SQLite substitutes the column's DEFAULT
  for the NULL and aborts only when the column has none — oracle-checked:
  `INSERT OR REPLACE INTO d VALUES (2, NULL)` on `v INTEGER NOT NULL DEFAULT 42`
  stores `2|42`. Granary raises even when a DEFAULT exists. `REPLACE` here
  means "delete the row this one conflicts with", and a NULL conflicts with
  nothing; storing a value the caller never supplied is a larger surprise than
  the error. Anyone changing this is changing a decision, not fixing an
  oversight.

  **The skip is decided in exactly one place, and it is the runtime one.**
  `Exec.not_null_skip_or_fail` is called from the INSERT sites only —
  `execute_insert_write` and the two columnstore `Op_insert` /
  `Op_insert_select` arms. `Exec.enforce_not_null` is unchanged and still
  raises unconditionally. Since #620 it has exactly ONE call site left —
  `write_row_rekeyed` — but three write paths funnel through it: plain
  `UPDATE`, `UPSERT ... DO UPDATE`, and `ON UPDATE CASCADE`. None of the three
  has an `OR IGNORE` form to consult, so softening that function would relax
  all three at once, silently and with no syntax asking for it.
  `Sema.bind_insert_row`'s static literal-NULL check **suspends itself** under
  `CA_ignore` rather than deciding anything — it exists to give the better,
  earlier error for the other resolutions, and it must not pre-empt the
  runtime skip.

  That last point is the one that shipped wrong once. The static check fires
  per STATEMENT, so while it was unconditional
  `INSERT OR IGNORE INTO t VALUES (1,10),(2,NULL),(3,30)` lost **all three**
  rows, where the parameter spelling of the same statement skipped one — worse
  than the defect #599 was filed about. Note also that the boundary was never
  "a literal NULL is a bind error" but "a literal NULL *in a VALUES list*":
  `INSERT OR IGNORE ... SELECT k, NULL FROM s` was always skipped silently,
  because `Sema` does not inspect a projection. `or_ignore_skips_every_spelling_of_null`
  in `test/test_not_null_599.ml` pins all four spellings together for that
  reason.

- A column's `not_null` no longer records *why* it is set — declared or implied by a primary key — because #530 folded both into the one stored bit. Anything that removes a key therefore cannot restore the column's original nullability: `ALTER TABLE ... DROP COLUMN` on a composite-PK member clears `primary_key` on the survivors but deliberately leaves `not_null`, since the engine is still enforcing it. Two bits (or an origin tag) is the fix if this ever needs to be exact — not cleverness at the ALTER sites.

### One `Db.t`, one explicit transaction (#555)

A `Db.t` carries a single explicit-transaction slot and every statement resolves
its transaction from it. **Autocommit sharing across fibers is fine and stays
fine** — `test_concurrent_rmw_223.ml` and `test_multifiber_stress.ml` both share
one handle across fibers and are correct. Explicit transactions are the problem:
two fibers cannot hold two.

Since #555 a `BEGIN` that arrives while another explicit transaction is active
fails *and* **poisons the handle**. While poisoned, every statement — read,
write, DDL, `SAVEPOINT`, prepared `run`/`iter`, and `COMMIT` — is rejected with a
`Runtime` error. `ROLLBACK` is the sole exit: it aborts whatever transaction was
in flight and clears the poison, after which the handle is fully usable.
`Db.transaction_poisoned` exposes the flag.

**The poison narrows the hazard; it does not close it.** It covers the window
between the failed `BEGIN` and the first `ROLLBACK` — and `ROLLBACK` is the
prescribed recovery, so the window closes by design. Past it the original
contamination is reachable with the fibers exchanged: the recovering fiber
`BEGIN`s afresh, and the fiber whose transaction was aborted — never told —
writes into the new one and commits it. That is **#584**, pinned as a canary by
`residual_584` in `test_txn.ml`. The enabling change for a real fix is **#585**
(a scoped `Db.with_transaction`, giving the engine an extent to attach an owner
token to); **#555** option 1 (a session object) is what an OLTP throughput
number needs. Do not read the poison as permission to share a handle.

It also dooms the *winner's* transaction — the engine cannot tell the two fibers
apart, so it cannot let `COMMIT` through without letting the wrong fiber's
`COMMIT` through.

Under ATTACH, poisoning is **per-handle**: each attached schema is its own
`Db.t` with its own slot, `BEGIN` routes to the active schema, and a poisoned
`aux` does not stop statements routed to `main`. Three consequences are wired in
deliberately — `Db.transaction_poisoned` ORs over the attached sub-handles (or
it would answer `false` on a genuinely poisoned connection), and both routing
statements that could carry a caller *away* from a poisoned schema are refused:
`PRAGMA active_database = …` (or the recovering `ROLLBACK` would route to the
wrong schema and answer "no active transaction" while the poisoned one still
held its writer lock) and `DETACH DATABASE` (which would otherwise drop the
sub-handle and its transaction — a clean outcome, but a *second* exit from the
poisoned state, making "ROLLBACK is the sole exit" false).

**#555's "arriving at a poisoned schema stays legal" valve no longer exists.**
It was there so a schema poisoned from elsewhere could still be reached to be
rolled back. Since #598, arriving requires a switch, a poisoned schema by
construction holds a transaction, so the #598 gate refuses the arrival too. The
state is unreachable today — a poisoned schema is always the one the caller is
already on, because that is where their `BEGIN` went — so nothing is stranded.
But anything that makes a poisoned schema reachable from elsewhere (a session
object, #555 option 1) must re-open that path explicitly rather than assume the
valve is still there.

**The ATTACH story is contained, not closed (#598).** #555's poison only fires
on a *collision*, and under ATTACH one `Db.t` legitimately holds two
explicit-transaction slots — so nothing collided when a `PRAGMA
active_database` switch moved the routing out from under an open transaction.
The caller's next write then landed in the *other* database and was durably
autocommitted there, with `transaction_poisoned = false` throughout; the caller
found out at `COMMIT`, after the write was on disk.

Since #598 the two statements that can move a caller's routing are **refused
while any schema on the connection has an explicit transaction open**:
`PRAGMA active_database = …` (unless it names the schema already active, which
is a no-op and stays legal) and `DETACH DATABASE`. The error is a distinct
message from `poisoned_msg` — nothing is poisoned and the caller's transaction
is intact; the statement just cannot be honoured yet. The gate sits *below*
#555's two poison gates in `execute_control_op`, so on a poisoned connection
the caller is still told to `ROLLBACK` rather than told a transaction is open,
and "ROLLBACK is the sole exit" stays true. Pinned by
`test/test_attach_active_txn_598.ml`.

That is the issue's *interim containment*, and it is deliberately blunt: it
buys loudness by removing the ability to switch schemas mid-transaction. The
real fix is the #585 family — bind a statement to the transaction its caller
opened instead of resolving it through shared mutable routing state at
execution time.

**Two deliberate compatibility breaks, both wider than the bug:**

- `PRAGMA active_database` mid-transaction previously worked for a pure **read**
  of another schema, and that is now refused too. The gate cannot tell a read
  from a write ahead of time, and the read case is one statement away from the
  write case that loses data.
- `DETACH DATABASE` is refused while **any** schema has a transaction open, even
  when the detach target itself has none. Narrowing it to the target's own slot
  would be defensible; it is not what is implemented.

### `Db.with_transaction` — the scoped extent (#585)

`Db.with_transaction db (fun db -> …)` BEGINs, runs the body, COMMITs on
success, ROLLBACKs and **re-raises** on exception. An exception is never
converted into `Error`; `Error` is reserved for a failed BEGIN or COMMIT.

Its point is not convenience. It is the first place a transaction has a dynamic
*extent*, so an owner token can live in an Lwt key for its duration — the thing
#555 and #584 both name as the missing prerequisite. The token is minted at
BEGIN, stored on the handle (`txn_scope`) and published into the calling fiber's
Lwt storage (`txn_scope_key`); a fiber owns the transaction iff the two agree,
which is what `Db.in_transaction_scope` reports.

**Nesting is refused, and that is a decision, not an omission.** A nested
`with_transaction` on the same handle — directly, or from a fiber spawned inside
the body, which inherits the token — returns `Error` without opening anything,
without rolling anything back and **without poisoning**. A savepoint would make
the inner scope's "commit" a `RELEASE`, so returning from it would not mean
durable and the outer scope could still discard it. A no-op join would make the
inner scope's rollback-on-exception abort the *outer* transaction while
returning to code that believes only its own work was undone. Refusal is the
only answer that does not lie; `SAVEPOINT`/`RELEASE`/`ROLLBACK TO` inside the
body is the supported partial-undo point.

The clean refusal is also the token's only live decision today, and it is worth
seeing why it is sound: the token *proves* the caller is the fiber that opened
the outer transaction, so there is nothing to contain. The #555 poison exists
precisely for the case where the engine cannot tell the fibers apart, and that
case is untouched — a second *fiber* on a shared handle holds no token, so its
BEGIN collides and poisons exactly as a bare `BEGIN` does.

**The poison contract is preserved, deliberately.** `with_transaction` issues no
`ROLLBACK` when its BEGIN fails, and none when its COMMIT fails — otherwise it
would be a second exit from the poisoned state and "ROLLBACK is the sole exit"
would become false. It rolls back only its *own* transaction, on the
body-raised-an-exception path.

**#584 is narrowed at the scope boundary, not closed.** If another fiber's
`ROLLBACK` aborts this scope's transaction and then opens its own in the freed
slot, the token no longer matches the slot; the combinator detects that at scope
exit and issues **neither** COMMIT nor ROLLBACK, returning `Error` — either
would act on the other fiber's transaction, which is #584 itself. It does *not*
cover statements *inside* the body: those still resolve the transaction from the
handle's mutable slot, so a displaced scope's writes land in the other fiber's
transaction before the boundary check reports the loss. Binding statements to
their owner is #555 option 1's work. **This is not permission to share a handle
across fibers** — `create_worker_handle` still is.

The body owns the statements, not the transaction: a `BEGIN` inside it poisons,
and a `COMMIT`/`ROLLBACK` inside it empties the slot so the combinator's own
COMMIT reports `no active transaction` even though the work committed. Neither
is guarded against, because the slot is shared mutable state with nothing to
guard it with yet. Pinned by `test/test_with_transaction_585.ml`.

### Running explicit transactions from more than one fiber

`Db.create_worker_handle` is the mechanism, and it is sound: it is `of_store`
over the *same* `Store.t`, so each handle gets its own `explicit_txn` while
sharing the store's single-writer `Rwlock`. A second fiber's `BEGIN` **blocks**
until the first commits rather than contaminating it, and neither handle is ever
poisoned. (This also corrects #555's premise that "a second `Db.t` over the same
path would be a second lock with no mutual exclusion" — true of a second
`open_file`, false here.)

**#589 is fixed — writes to every table shape are now safe.** Read the history
anyway, because the fix's invariant is what keeps it that way.

Each handle still gets a *fresh catalog* — that is what makes DDL invisible
across handles — but it no longer gets a fresh **rowid allocator**. The
allocator's live state was lifted out of the cached `table_meta` and into
`Schema_cache.rowid_counters`, a table `Cat.open_ ?rowid_counters` accepts so a
second catalog over the same store shares it. Every read of a cached
`table_meta` is patched from that table on the way out and every write publishes
to it on the way in, which keeps `table_meta` the only type the rest of the
engine sees. Two rules make the sharing correct and must survive any future
edit:

- **Keyed by tree id, not by table name** — a tree id identifies the data tree
  the counter counts for and survives `ALTER TABLE … RENAME`; keying by name
  would let a `DROP`+`CREATE` inherit the dead table's counter. **But a tree id
  is not unique for all time.** It is never reused after a *committed* `DROP`
  (measured: drop tid 16, next `CREATE` gets 18) and **is** reused after a
  *rolled-back* `CREATE` — `next_user_tid_tx` writes the bumped counter inside
  the transaction, so `S.rollback` reverts it and the next `CREATE` gets the
  same id (measured: doomed 17, rolled back, next `CREATE` also 17). **The real
  invariant is therefore not "tree ids are unique" but "the entry is cleared or
  overwritten before the reused id is allocated from"** — and *two redundant
  mechanisms* do that, each sufficient alone (verified by mutation): (1)
  `put_table`'s undo runs `del_meta` → `unpublish`; (2) the replacement `CREATE`
  publishes `empty_next_rowid` under the same tree id, overwriting the stale
  entry. Removing either alone still passes the suite; removing both loses the
  row. `tid_reuse_after_rolled_back_create` and `tid_reuse_worker` guard **the
  pair** — so a green suite is not evidence that the mechanism you are editing
  is dead. Keeping DDL on the `set_meta`/`del_meta` chokepoints keeps both.
- Negative tree ids are skipped (`tree_id >= 0` guard) because they are
  ephemeral/sentinel metas that name no data tree and would all collide on one
  entry. There are four: `-1` for a CTE *and* for a decoded columnar table whose
  stored tid is 0, `-2` for `sqlite_master`, `-3` for `sqlite_sequence`.
- **Open-time seeding uses `seed_table`, not `put_table_durable`** — it will not
  overwrite a counter that is already live. A worker re-reads the catalog off
  disk, and disk is never fresher than the running allocator; clobbering would
  reintroduce the same collision with the roles exchanged, making the **parent**
  go stale.

What it used to do, and what the tests now assert the opposite of — two counters
over one data tree, neither invalidating the other, so an `INSERT` with an
engine-assigned rowid reused a rowid the other handle had already committed and
silently overwrote it:

| shape | before #589 | now |
|---|---|---|
| `CREATE TABLE t (b TEXT)` — plain rowid, the commonest shape | 1 row where there should be 2 | 2 rows |
| `a INTEGER PRIMARY KEY`, engine-assigned | 1 row | 2 rows, ids 1 and 2 |
| `a INTEGER PRIMARY KEY`, **caller-supplied** values | correct | correct |
| `a INTEGER PRIMARY KEY AUTOINCREMENT` | 1 row; `sqlite_sequence` reads `1` on both handles | 2 rows; `sqlite_sequence` reads `2` on both |
| `k TEXT PRIMARY KEY`, caller-supplied keys | 1 row **plus index corruption** | 2 rows, seeks correct |
| `WITHOUT ROWID` | correct | correct |

`TEXT PRIMARY KEY` was the worst case because the consequence was **wrong query
answers**, not row loss: the index kept a phantom entry for the overwritten key
pointing at the reused rowid. It was also symmetric and unbounded (a worker
created *after* the parent's rows snapshotted correctly, then the parent went
stale), persisted to disk, and was unaffected by explicit transactions — never a
race, so the writer lock was irrelevant. The whole matrix plus three
handles/five inserts, a rollback, and a close/reopen is pinned by
`test/test_worker_handle_589.ml`; `worker_handle_text_pk_corruption` and
`worker_handle_stale_rowid_counter` in `test_txn.ml` keep the issue's own two
sequences.

**What is still unsafe about a worker handle** — none of it corrupts anything:

- **DDL on one handle is invisible to the other's schema cache.** This one is
  inherent to the per-handle catalog and was *not* fixed: create a table on the
  parent and the worker cannot see it until reopened. Pinned by
  `ddl_still_invisible_across_handles`.
- Reactive views, ATTACHed schemas and the active schema are per-handle; a
  worker starts with none of the parent's.
- Write transactions *serialize* on the shared lock rather than overlapping, and
  a read-only transaction does not overlap a writer either. Genuine write
  concurrency still needs #555 option 1.
- Anything else that reaches `Db.of_store` over an **already-open** store owes
  it `~rowid_counters` by hand; `create_worker_handle` is the only caller that
  does so today.

`Tpcc_driver`'s one-deep worker pool predates this and serializes whole
transactions on a single handle; that is why its terminal-count sweep flatlines
by construction.

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

Binary: `~/.local/bin/forgejo`. Repo slug: `IoTReadyNext/granary`.

Common commands:

```sh
forgejo issue list IoTReadyNext/granary
forgejo issue view IoTReadyNext/granary <N>
forgejo issue close IoTReadyNext/granary <N>
forgejo pr list IoTReadyNext/granary
forgejo pr view IoTReadyNext/granary <N>
forgejo pr review IoTReadyNext/granary <N> --approve
forgejo pr merge IoTReadyNext/granary <N> --method=squash
```
