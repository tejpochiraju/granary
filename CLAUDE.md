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
| `test_unix_file` (`read_page_yields_to_timer_test`) | #158 `read_page` not yielding to the scheduler | timer fires ≤ 0.25 s | `GRANARY_BENCH_MAX_TIMER_S` |

**`test_unix_file`'s row was missing from this table until #730, and that cost
real PR time.** It is an ordinary wall-clock gate — a 50 ms timer posted
alongside a 10 000-iteration `read_page` loop, asserted to fire within 0.25 s —
but it carried no `GRANARY_BENCH_*` knob, so it was armed in *every* job
including the six that neutralize everything else. On a loaded runner it failed
PRs that changed nothing, and because it lives in `test_unix_file` rather than a
`bench_*` file, nobody looking at the neutralizer list noticed it was missing.
**A new wall-clock assertion belongs in this table and in all six workflow
files, whatever file it is written in.**

Two further things about that test are worth knowing before editing it:

- **It can fail from being too FAST, not only too slow.** If the reader finishes
  all 10 000 reads inside the timer's 50 ms deadline, the timer necessarily
  fires after the reader, and the ordering assertion inverts — while proving
  nothing, because a *non*-cooperative reader that quick would look identical.
  Since #730 that case prints `INCONCLUSIVE` and does not assert: a measurement
  that cannot discriminate must not be a gate.
- **Its cross-process `lockf` test used to be a two-second race, not a
  synchronisation.** The child locked the file, signalled, then `Unix.sleep 2`
  and exited; if the parent's `UF.open_` probe took longer than the child's
  remaining sleep, the lock was already gone, `open_` succeeded, and the test
  failed. A green run proved the machine was fast that minute. It is now a
  two-pipe handshake — the child holds the lock until the parent says it is done
  probing — so no timing assumption survives.

Three gates are **not** wall-clock and therefore **not** neutralized anywhere:

| test | guards | gate | knob |
|---|---|---|---|
| `test_not_null_600` | #600 `PRAGMA not_null_check` retaining every violating row | marginal peak live heap < 4 words/row when the table doubles | `GRANARY_MEM_MAX_WORDS_PER_ROW` |
| `test_not_null_repair_630` | #630 `PRAGMA not_null_repair`'s **scan** draining the tree via `cursor_open` | same gate, but with the violation count held FIXED at 5 while the table doubles, so only the scan can move it | `GRANARY_MEM_MAX_WORDS_PER_ROW` |
| `test_correlated_exists_493` | #493 a correlated `EXISTS` leaking one RO snapshot per outer row | peak live RO snapshots does not grow when the outer rows go 100 → 400 | `GRANARY_MAX_LIVE_READERS` |

The first two are complements, not duplicates: #600's doubles the violations along
with the table and so cannot tell a retaining scan from a retaining victim
buffer; #630's holds the violations fixed and therefore measures the scan
alone. The repair's victim buffer is *supposed* to be O(violations) — #541's
finding is that the rows must be collected and sorted before they are fetched,
because fetching in index-key order costs up to a page read per row once the
table outgrows the pager cache. Do not "bound" that by streaming it.

They measure *allocation* (peak live major-heap words, sampled through a `Gc`
alarm), so a loaded runner does not move them — ±0.02% across runs, which no
wall-clock gate manages. That is why they run armed in `ci.yml`, `coverage.yml`
and `cross-arch.yml` alongside the ones those jobs disarm. What load cannot
change, a different allocator or word size can, and `cross-arch.yml`'s arm64
arm has never run it, so `GRANARY_MEM_MAX_WORDS_PER_ROW` exists as the escape
hatch — it raises the ceiling without disabling the correctness assertions the
same test makes. Reach for it only after ruling out the thing it guards: the
measured slopes are ≈20-23 words/row retaining (19.61-23.52 across three runs;
the 40 000-row point is the noisy one) and 1.6-1.9 counting, so a failure
anywhere between those two bands is a regression, not a platform difference.

`test_correlated_exists_493` measures a *count* — `Store.active_reader_count`,
an integer folded from a refcount table — sampled between outer rows, so load
cannot move it either. It has **two** assertions and they are not equally
trustworthy:

- the **ratio** (`peak(400) ≤ peak(100) + 2`) needs no prediction about what the
  healthy number is, and is the real gate;
- the **ceiling** (`peak(400) ≤ GRANARY_MAX_LIVE_READERS`, default 32) does, and
  **that default is a prediction, not a measurement** — this test shipped in a
  batch whose build and benchmark pass was deferred, and no armed run has ever
  been observed. A leaked run reports ≈400, so the ceiling has ~12x headroom
  over the expected healthy value; if it nonetheless fails while the ratio
  passes, the default was simply wrong and raising it is correct. If the
  **ratio** fails, that is the leak and no knob should be touched.

It must run **on disk**: the `Mem` backend answers 0 for the reader counters
unconditionally, so an in-memory version passes vacuously.

**Being non-wall-clock is necessary but not sufficient to run armed
everywhere — being *measured* is the other half.** A second allocation gate
exists and is deliberately **not** armed by default:

| test | guards | gate | knob |
|---|---|---|---|
| `test_scan_borrow_481` | #481 the scan path copying the key, the value twice, and a 4 KB page per leaf | scan allocates < 2.0 bytes per byte of payload it never reads | `GRANARY_MEM_MAX_PAYLOAD_SLOPE` |

The unit is *copies of the payload*: 1.0 is the one copy the caller asked for,
3.0 is the three #481 removed, and word-size rounding moves it by ~0.02, so the
2.0 boundary is an integer boundary rather than a tuned number. That derivation
is why the ceiling is defensible; the fact that **nobody has run it yet** is why
it is unarmed. Unset (the default in `ci.yml`, `coverage.yml`, `cross-arch.yml`
and on your box), the test measures and prints the slope and still asserts every
correctness property around it — it just does not block. It is armed in exactly
one place, `bench-nightly.yml`, which sets `GRANARY_MEM_MAX_PAYLOAD_SLOPE=2.0`
and reports through an auto-filed issue rather than failing a PR.

This is the shape to copy for any future allocation gate: **derive the ceiling,
arm it nightly, and promote it to armed-by-default only once a real measurement
backs it.** Arming a guessed ceiling on every PR is how a gate gets silently
neutralized later, which is the honour system #549 removed. When #481's
benchmark pass produces the number, flip the default in the test and move its
row into the table above.

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
  -e GRANARY_BENCH_MAX_TIMER_S=1000000 \
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
  - `Index_key.encode_value` gives NaN its own single-byte tag `0x01` — below
    INTEGER's `0x02` and REAL's `0x03`, above NULL's `0x00` — so it also sorts
    below every other number, and *distinctly from* NULL.

  **They agree that NaN sorts below every number, and that — not blanket
  agreement — is what keeps this from being a rows-lost bug.** They now also
  agree about NaN vs NULL: `0x01` was #578's fix, and before it NaN and NULL
  shared `0x00` and were byte-identical. A seek and its residual predicate can never disagree about where
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

  **Index-keyed uniqueness used to be the one that did NOT agree — #578, since
  FIXED (`d296baf`).** The UNIQUE NULL exemption (`any_null_val`) tests the
  *value*, so a NaN is correctly not exempted; the conflict probe then compares
  *encoded bytes*, and while NaN's key was byte-identical to NULL's,
  `INSERT NULL` then `INSERT NaN` raised a spurious `UNIQUE constraint failed`
  while the reverse order succeeded. The fix was the one this paragraph
  predicted would be the good one: NaN got its own tag byte (`0x01`,
  `lib/encoding/index_key.ml:117-126`). Note what was NOT done, and must not
  be: making the exemption byte-based would have silently exempted NaN from
  uniqueness altogether. **Option C is correspondingly stronger, not weaker** —
  the value level and the key level now agree about NaN's position exactly,
  rather than only about "both below every number".

  **#579 (fixed): `Exec.compare_values` is now a TOTAL order.** It used to end
  in `| _, _ -> 0  (* cross-type: shouldn't happen *)`, and it does happen:
  strict column typing keeps a *stored* column single-typed, but a *computed*
  one is unconstrained per row, so `CASE WHEN i = 0 THEN f ELSE i END` mixes
  INTEGERs and REALs freely. Every such pair compared **equal**, making the
  relation **non-transitive** (`1 = 2.5`, `2.5 = 3`, but `1 < 3`) — and
  `List.sort` on a non-transitive comparator has no defined result, so
  `ORDER BY` over such a column returned rows in scan order, unsorted, with no
  error. The same comparator is behind GROUP BY (which sorts and then groups
  adjacent runs, so *which* rows landed in *which* group was input-order-
  dependent), window PARTITION BY, and MIN/MAX (which became first-wins).

  Two rules: within the numeric class compare **exactly** via `cmp_int_real`;
  across classes order NULL < number < TEXT < BLOB via `value_class_rank`. That
  order is SQLite's documented storage-class order *and* the order of
  `Index_key.encode_value`'s tag bytes, so the value level and the index level
  cannot disagree about which **class** sorts first. They still disagree about
  INTEGER vs REAL *within* the numeric class — the encoding gives them separate
  tags (`0x02`, `0x03`) and so puts every integer before every real — but that
  is pre-existing.

  **The exactness is the part that is easy to get wrong, and the first
  revision of the fix did.** Promoting through `Int64.to_float` rounds, so
  above 2^53 two distinct int64s promote to the same float and the comparator
  is *still* non-transitive — `9007199254740993 ≡ 9007199254740992.0` and
  `9007199254740992.0 ≡ 9007199254740992` while `9007199254740993 >
  9007199254740992`. That is #579's own defect one magnitude up, and the
  issue's repro reproduces verbatim with large values. `cmp_int_real` compares
  without converting (sign and range first, then `Int64.compare` against the
  truncated float, then the fraction as tiebreak), which is also what SQLite
  does (`sqlite3IntFloatCompare`). **`compare_values` is a total order for
  every input, with no magnitude caveat** — but that claim is about that
  function, not about the engine.

  **`cmp_result`, the WHERE-predicate comparator, is NOT the same function and
  differs in two filed ways.** Do not restate the rule as "the two comparators
  now agree":
  - it promotes int-vs-real through `Int64.to_float`, so above 2^53 a
    *predicate* still answers equal for a pair the *ordering* separates
    (**#733**);
  - it ends in `| _ -> Row.V_int 0L`, so every cross-class predicate is false —
    it applies no class order at all. After #579, `ORDER BY` says `5 < 'abc'`
    while `WHERE` says that is false. sqlite3 answers `1`, so `cmp_result` is
    the wrong half (**#734**). This WHERE/sort disagreement is *new* in the
    cross-class direction and was traded for removing the cross-numeric one.

  **Why #579 did not disturb the index path, which is the thing to check before
  touching this again.** `range_bound_key`'s `pred`/`succ` widening exists to
  compensate for an *inexact* residual predicate, and its doc comment is
  written against exactly that. It is unaffected because the residual runs
  through `cmp_result`, which handles int-vs-real in its own arm and never
  reaches `compare_values` for that pair. If #733 makes `cmp_result` exact, the
  widening and `test_range_bound_517`'s above-2^53 cases are owed the same
  change: an exact predicate is *narrower* than the seek, which is safe only
  for as long as a residual actually runs over the seek's output.

  **Unremarked improvement, recorded so nobody finds it by bisect.**
  `compare_values` is not only an ordering function: nine call sites read
  `compare_values a b = 0` as "equal"/"unchanged", and the old catch-all made
  every cross-class pair satisfy that. So `SELECT 1 IN ('abc')` answered `1`
  and `CASE 1 WHEN 'abc' …` matched. Two of the nine are FK correctness:
  `check_fk_parent_update_restrict`'s `unchanged` fast path and its deferred
  twin skipped the child probe entirely when a parent key moved between storage
  classes, and `fk_child_has_ref*`'s match tests counted a child row of a
  different class as a live reference. All now behave correctly.

  **DISTINCT is deliberately not routed through it** and still dedups on
  `row_key`'s string rendering, where `1` and `1.0` are different keys. So
  DISTINCT and GROUP BY still disagree about whether an int and a numerically
  equal real are one key. That is the residual: there are still four
  independent value comparators in the tree (`compare_values`, `cmp_result`,
  `row_key`'s string rendering, and `Reactive_view.value_compare` — the last of
  which orders all ints before all reals) and they do not all agree. Folding
  them into one is what #579's own "Note" asks for and is not what that fix
  did. Pinned by `test/test_compare_values_579.ml`. Two of its cases carry the
  weight: `the_falsifying_triple_is_ordered_exactly` pins the three >2^53
  comparisons directly, and the QCheck `compare_values is transitive over
  random triples` property fuzzes for the same class of defect. **That
  generator is weighted, not uniform, and the weights are load-bearing** — a
  uniform draw over its branches needs ~4x10^5 cases to reach a falsifying
  triple, and 20 000 uniform cases pass against the promoting comparator.
  Verified by mutation: with promotion restored, four cases in that file fail,
  including both properties. An assertion on a single fixed input order catches
  none of it.

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
  `execute_insert`, `execute_insert_write` and the two columnstore `Op_insert`
  / `Op_insert_select` arms. (`execute_insert`'s call is #639's: it is guarded
  by `CA_ignore` and runs *before* conflict resolution, so the skip no longer
  depends on which index the row collides with;
  `execute_insert_write`'s is guarded by `not skip` and so never
  double-evaluates it.) `Exec.enforce_not_null` is unchanged and still
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

- **An aggregated SELECT sorts by grouped columns, or it refuses (#663, decided
  2026-09-02).** This is the same rule the projection and HAVING already
  applied, arriving late at the third of the three clauses — and it is a
  **deliberate divergence from sqlite3**, which accepts a bare non-grouped
  column in `ORDER BY` (oracle-checked: it returns rows, picking an arbitrary
  row's value from each group). Granary already declines that permissiveness in
  the other two clauses, and ORDER BY was the one where the consequence of not
  having the rule was a *wrong answer* rather than an error.

  The mechanism is worth knowing because it is the same one twice. An
  aggregated SELECT sorts AFTER projection, and
  `Planner.plan_post_agg_sort_input_space`'s `remap_e` rewrites a key's column
  index from pre-aggregation space into the aggregated output row. It only
  rewrites an index it *finds in* `group_by`, and — until #663 — only at the
  ROOT of the key expression. So:

  - a **non-grouped** column bound to its input index, was not remapped, and the
    key read whatever OUTPUT column sat at that index.
    `SELECT nm, COUNT(*) FROM g3 GROUP BY nm ORDER BY pad` sorted by the count,
    silently; on a table wide enough that the input index exceeded the output
    arity it indexed past the end of the row and raised `Invalid_argument` from
    `Exec` mid-query instead of a `Db.error`.
  - a **grouped** column *inside an expression* was not remapped either, because
    the rewrite did not recurse. That one only ever looked right when the
    grouped column sat at input index 0, where the two spaces coincide — which
    is what every fixture in the tree happened to do.

  Both halves are needed and they lean on each other: `Sema.bind_select_order`
  now refuses a key naming any non-grouped column (via `expr_col_refs`, so a
  column buried in a `CASE` or a concatenation is caught too), which is
  precisely what makes it safe for `remap_e` to recurse — **every** `P_col`
  remaining under an aggregated ORDER BY key is a grouped column, so descending
  cannot mis-fire on one that should have been left alone.

  **Being grouped is necessary but NOT sufficient to be remappable, and that
  gap is a third thing the fix owes.** `remap_e`'s inner search needs the
  column to appear in `agg_proj` as an `AP_group_col` — to be *projected as a
  bare group column*. Group by a column and do not project it and the search
  fails, so the old `| None -> e` arm kept the pre-aggregation index and both
  of the symptoms above came straight back:
  `SELECT COUNT(*) FROM g GROUP BY nm ORDER BY nm` raised `Invalid_argument`
  out of the executor, and `SELECT UPPER(nm), COUNT(*) … ORDER BY nm` sorted by
  the count. Making `remap_e` recursive *widened* the reach of that arm rather
  than narrowing it. An unprojected grouped column now gets a **hidden output
  slot** — appended to `Op_aggregate`'s own projection where its ordinal means
  what it was bound to mean, sorted on by position, and trimmed by an
  `Op_project` around the sort — which is exactly the mechanism
  `plan_agg_order_hidden` already uses for #495's aggregate keys. One slot per
  grouped column, not per mention. `GROUP BY` a column you do not project and
  then `ORDER BY` it is idiomatic, not a corner.

  **`is_aggregated` is not the same question as "has a GROUP BY", and the check
  is gated on both.** It is also true for an aggregate in the projection with
  no GROUP BY at all, where `group_cols` is `[]` and therefore every column
  reference fails the membership test. Gating on `is_aggregated` alone refused
  `SELECT COUNT(*) FROM g ORDER BY nm` — which sqlite3 answers, which `main`
  answered, and whose error message named a grouping there is none of. Such a
  statement returns exactly one row, so its ORDER BY is a no-op whatever it
  names; the refusal is skipped and the planner drops the sort rather than
  indexing a one-column output row with a pre-aggregation ordinal.

  The two sites that produce the refusal message —
  `bind_select_order_agg`'s `grouped` and `bind_select`'s `grouped_check` —
  share one `non_grouped_order_msg` helper rather than two copy-pasted format
  strings.

  Three things stay legal and are pinned as such: an ordinal and an output alias
  (both address the output row directly and are not column references, #489), an
  expression over a *grouped* column, and every non-aggregated SELECT
  (`grouped_check` is `None` there — a bare column is bound and evaluated
  against the input row exactly as before). An ORDER BY that *mentions an
  aggregate* keeps taking #495's separate `bind_select_order_agg` path, which
  had this discipline from the start. Pinned by `test/test_agg_order_by_663.ml`,
  whose first two cases assert the projection and HAVING already had the rule —
  that is what makes this consistency rather than a new opinion.

- **One rule resolves a correlated subquery's outer references (#635/#626/#615, 2026-08-06).**
  An input's **scope identifier** is its FROM item's alias where it has one and
  its table name otherwise — an alias *replaces* the name. A qualified outer
  reference names a scope identifier; an unqualified one names a column exactly
  one input carries; **anything else is an error, never a silent empty or NULL
  result.** That rule holds at any nesting depth and in every clause a
  correlated subquery can sit in — WHERE, an INNER or OUTER join's ON,
  a projection, and HAVING.

  It is implemented at **three** levels and they must not be allowed to drift
  apart again, because each pair that disagreed produced a different silent
  wrong answer:
  - `Sema.from_ident` for the binder's two qualified lookups (`bind_expr_join`,
    `select_qual_lookup`);
  - `Exec.inner_scope_of` for a subquery's *own* FROM (this one was always
    right);
  - `Exec.scan_ident` / `get_outer_scan_metas` for the outer inputs, which
    needs `alias` on `Plan.Op_seq_scan` / `Op_col_seq_scan` /
    `Op_index_lookup` / `Op_rowid_lookup` and `right_alias` on
    `Op_nested_loop_join`, carried from `Sema.BS_select.table_alias` and
    `Sema.bound_join.right_alias`.

  Consequences worth knowing before editing this area:
  - **`SELECT t.x FROM t s` is now an error**, matching sqlite3. It used to
    answer rows, and that is what made an alias-hidden name inside a subquery
    resolve *inward*: the subquery was never recognised as correlated, was
    folded to a constant, and rows were lost with no error (#635's comment).
  - The duplicate guard in `get_outer_scan_metas` is by **identifier**, not by
    table name. `FROM l AS x JOIN l AS y` therefore resolves; the unaliased
    `FROM l JOIN l` still cannot and is still refused. #592's
    `self_join_is_refused_not_emptied` was rewritten to the unaliased spelling
    for exactly this reason — a green suite on the aliased one would now mean
    the opposite of what it used to. `test/test_refusal_error_627.ml` was
    written in parallel and had to be rewritten for the same reason, in two of
    its four categories: it provoked #627's refusals with the *aliased*
    self-join and with a plain correlated ON in an outer join, and #635 and
    #615 respectively turned both into answers. They are now the unaliased
    self-join and `l.a` under `FROM l AS x` in an outer join's ON. **Anything
    that resolves those spellings too must replace them again, not delete the
    row** — #627 covers seven public surfaces, and a category that quietly
    stops firing takes all seven with it.
  - `substitute_outer_in_expr` descends into nested `E_subquery` / `E_exists` /
    `E_in_select` carrying the **union** of every enclosing subquery's scope.
    Carrying only the innermost scope is the obvious implementation and is
    wrong: it rewrites an *intermediate* subquery's own column from the outer
    row.
  - **That descent is necessary but was not sufficient, and the missing half is
    "what counts as correlated".** `Sema` treats `E_subquery` / `E_exists` /
    `E_in_select` as opaque leaves and never descends into them, so a statement
    whose correlation sits TWO levels down *binds cleanly*. Every caller read
    "it bound" as "it is uncorrelated" and evaluated it eagerly, before any
    outer row existed to substitute from; the reference was then met for the
    first time by the intermediate query's own `stream_filter`, whose inputs are
    the intermediate FROM, and refused there. So the two-level shape
    `FROM l AS x WHERE EXISTS (SELECT 1 FROM r WHERE EXISTS (… v < x.a))` was
    refused with the descent in place — the descent was correct but never ran.
    `Exec.stmt_has_free_column_ref` answers the second question and is consulted
    at the one chokepoint all four callers share, `plan_subquery_cached`, so
    `refuse_unresolved_correlation` and the three `eval_*_subquery` functions
    cannot disagree about which statements are correlated. It is implemented by
    *running* `substitute_outer_in_stmt` with a binding that resolves nothing and
    records that it was asked, so the detector and the substituter agree by
    construction about which references are free — the same discipline the two
    binders are held to, and the reason not to hand-write a second walker
    (`substitute_outer_in_plan_expr` is already the odd member of a four-walker
    set, #670). It can only move a statement from "evaluate eagerly" to "treat
    as correlated", and `inner_scope_of` answers "owned" for everything it
    cannot resolve, so an unresolvable FROM never manufactures a free reference.
    Pinned by `two_nesting_levels` in `test/test_alias_outer_ref_635.ml`.
  - **#566's refusal of a correlated ON subquery in an OUTER join is
    reopened (#615).** Its stated blocker — no correlation source over a join
    node — was removed by #592, so the `Left` arm now substitutes per (left,
    right) *pair* inside the join. It has to be per pair, not per surviving
    row: for an outer join the ON predicate **is** the match test, and a filter
    above the join rejects the null-extended row it must emit (#552). The pure
    pairing loop is kept as the arm taken when no subquery survives.
  - `Op_nested_loop_join` cannot carry a subquery in its probe through the
    planner, but it is a public constructor, so `stream_nested_loop_join`
    refuses one explicitly rather than encoding it as NULL.
- **A GENERATED column's NOT NULL is enforced on its COMPUTED value, at both
  levels (#629).** This was the third instance of the same shape as #567 and
  #599 — a check running where it cannot see the truth — and the fix narrows
  *when* each check runs, never *whether*.

  The caller is forbidden from supplying a generated column, so it is always
  omitted, and `Sema.bind_insert_row` fills an omitted column with a literal-NULL
  placeholder. Judging that placeholder made a `NOT NULL GENERATED` column reject
  **every** INSERT: no spelling could succeed, so the table was uninsertable.
  `bind_insert_row` therefore exempts generated columns of **both** storage
  classes — at bind time neither has a value. It keeps its literal-NULL error for
  every other column.

  Two neighbouring binders were out of step with that and had to move too, or
  the headline symptom survived the fix. `bind_insert`'s **implicit column list**
  now omits generated columns, so bare `INSERT INTO g VALUES (…)` works — before,
  it was `Arity_mismatch` with one value per base column and "cannot INSERT into
  generated column" when padded out. That was not a new divergence decision: the
  `INSERT … SELECT` binder's `columns = []` arm already applied exactly that
  filter, and `Db.dump` already emits an explicit list that omits them; the
  VALUES binder was the odd one out. And `bind_upsert_assignments` now refuses
  `DO UPDATE SET <generated> = …` the way `bind_update_assignments` always did —
  it was the third assignment spelling and the only unguarded one, so the
  assignment bound fine and `compute_stored_generated_cols` overwrote the column
  immediately after, making the statement silently do nothing.

  The runtime half had the mirror-image gap. `Exec.not_null_exempt_col` exempted
  VIRTUAL generated columns outright (#567's reasoning: their stored cell is
  `V_null` by design), which meant a NOT NULL VIRTUAL column was *unenforceable*.
  `Exec.not_null_violation` now recomputes the virtuals into a copy of the row
  before judging it, and consults the exemption only when it cannot — a row that
  does not cover every column, where evaluating the generated expression would
  raise. **The exemption is the degraded mode; do not re-broaden it.**

  **STORED columns rest on an ordering claim, and the claim covers the
  ROW-STORE sites only** — do not restate it as "every enforcement site", which
  is how it shipped and is false. `compute_stored_generated_cols` runs before
  the check at `execute_insert`, `execute_upsert_update`, `update_col_in_tx` and
  `apply_update_row`. It is **not** called on either columnar arm: both build the
  row with `Array.make n_cols Row.V_null` and pass it straight to
  `not_null_skip_or_fail`. Nor does `stream_col_seq_scan` recompute VIRTUAL ones
  on the way out. So a generated column on a columnstore table read NULL forever
  in **both** classes, silently. `Sema.bind_create` now **refuses** a GENERATED
  column on a `USING COLUMNSTORE` table (#660) — which is what makes the ordering
  question not arise at those two sites, rather than a claim that it was already
  answered there. Wiring the computation into the write arms alone was rejected:
  it fixes STORED and leaves VIRTUAL reading NULL, deepening the asymmetry. If
  #660 lifts the refusal, both halves are owed at once.

  **#661 (fixed): `ALTER TABLE ... ADD COLUMN ... NOT NULL GENERATED ... VIRTUAL`
  is no longer refused, and it is #629 that makes lifting the refusal safe.**
  `Sema.bind_add_column`'s NOT NULL gate exists because existing rows decode
  SHORT — they were written without the new column, so it reads back as a stored
  NULL, and a DEFAULT is the only thing that can supply a value for them. That
  reason holds for a plain column and for a **STORED** generated one, which has
  no read-side recompute for rows written before the ALTER; both keep the
  refusal. It does not hold for a **VIRTUAL** one, which has no stored cell at
  all, so the exemption relaxes *when* the constraint is checked and not
  *whether* — `not_null_violation` recomputes the virtuals before judging, so
  the added column is genuinely enforced at write time.

  **The ALTER deliberately does NOT validate the expression over EXISTING
  rows** — the judgement call #661 leaves open. `bind_add_column` is a binder
  with no store, so the check would have to move into the exec ALTER path and
  would turn an O(1) metadata-only DDL into a full table scan; and it matches
  how pre-existing constraint violations are treated generally, reported by
  `PRAGMA not_null_check` rather than rejected at DDL time. **The residual is
  real and has no repair surface of its own**: a row already on disk whose
  generated expression is NULL reads its NULL silently and raises only on the
  next write that rewrites it (`write_row_rekeyed` → `enforce_not_null`), and
  `not_null_scan_cols` deliberately does not cover generated columns per the
  paragraph above, so `PRAGMA not_null_check` will not report it either. Moving
  the base column is the only repair. Both halves are pinned by
  `test/test_alter_add_generated_661.ml`.

  Consequences worth knowing: a generated expression that genuinely evaluates to
  NULL is still rejected — on INSERT (skipped under `OR IGNORE`, per #599, since
  the row now reaches the runtime site that decides that), and on an UPDATE of
  the base column it reads (always raises; `write_row_rekeyed` has no `OR IGNORE`
  form). `not_null_scan_cols` (`PRAGMA not_null_check`/`repair`) deliberately did
  **not** follow — it reports on cells already on disk whose only repair is to
  rewrite them, which is meaningless for a column never read from disk. Pinned by
  `test/test_not_null_629.ml`.
- **An aggregate's ARGUMENT is an expression, and both rules that follow from
  that are now settled (#665 and #664, 2026-09-03).** #488 made
  `SUM(price * (1 - disc))` legal; these two are the consequences it did not
  finish.

  **#665: the SUM/AVG numeric check reaches the expression spelling.** #568 had
  already unified the *bare* `SUM(text_col)` and the *wrapped*
  `SUM(text_col) + 0` on one bind-time check, precisely so that a pair of
  parentheses could not turn it off. #488's expression argument was a third
  spelling with no column ordinal, so `agg_numeric_check` — which keys off a
  stored column's declared type — never ran on it:
  `SUM(CASE WHEN … THEN 'a' ELSE 'b' END)` failed at RUNTIME mid-scan, and
  **succeeded outright on an empty table**, which is the exact defect #568 was
  filed about. The verdict now lives in one function,
  `Sema.agg_numeric_ty_check`, reached by both spellings.

  **`Sema.agg_arg_static_ty` is deliberately NOT `Sema.infer_type`, and the
  reason is a compatibility break avoided rather than a style preference.**
  `infer_type` indexes a `Row.column list` where the binder has only the
  resolver's `agg_arg_col_ty` ordinal lookup; more importantly it must not
  descend into `BE_case`, because its other caller is `bind_update_assignments`
  and Granary's typing there is strict (`ty_equal Integer Real = false`), so a
  CASE arm would newly reject
  `UPDATE t SET real_col = CASE WHEN c THEN 1 ELSE 2 END`. The CASE descent is
  the whole point for #665 — the issue's headline shape is a CASE — so it lives
  in the aggregate-only function. Every arm that cannot be sure answers `None`,
  and `None` is `Ok`, so the check can only move a failure EARLIER; it can never
  refuse a query that would have answered. The residual it does not close: an
  argument whose type is statically indeterminate (a scalar function, a bound
  parameter, a CASE whose arms disagree) still reaches the runtime accumulator,
  and on an empty table still succeeds. Pinned by
  `test/test_agg_numeric_665.ml`, whose
  `indeterminate_argument_still_fails_at_runtime` is the boundary marker.

  **#664: a subquery in the argument is evaluated, not refused — and it does
  NOT follow #558's rule.** This is the part to read before touching either
  path. #558 pre-evaluates subqueries in `having` and `proj`; those are
  evaluated per aggregate **OUTPUT** row, whose only input-derived slots are the
  grouped columns, hence #558's rule that a correlated subquery there may
  reference a GROUP BY column and **nothing else**. An **ARGUMENT** is evaluated
  per **INPUT** row, before any grouping, so its outer reference may name any
  column the child carries; it is resolved the way `stream_expr_project`
  resolves a correlated projection, against `get_outer_scan_metas child`, per
  row. Two rules for two different rows. They have separate refusal messages
  (`agg_subquery_refusal` vs `agg_arg_subquery_refusal`) for exactly that
  reason: reporting the GROUP BY one for an argument sends the reader to a rule
  that does not apply to it. **Unifying the two would be a silent wrong answer**,
  not a simplification — `correlated_under_a_group_by_on_another_column` in
  `test/test_agg_arg_subquery_664.ml` is a query #558's mechanism could not
  answer at all.

  Mechanically the resolved argument VALUE is parked in a hidden trailing slot
  appended to its input row and the spec rewritten to `P_col slot` — the same
  hidden-slot mechanism #495/#663 use. That is what keeps the subquery evaluated
  exactly once per row: #491's DISTINCT filter and `aggregate_one` both read the
  argument through `agg_arg_getter`, so a `P_subquery` left in place would have
  had to be resolved separately by each. Widening is safe because everything
  that indexes an input row there indexes a PREFIX of it, and the aggregate
  output row is `group_key @ agg_vals`, so no hidden slot escapes into a result.

  **The #247 fast path gives such a query up**, gated above its child dispatch
  so both `run_aggregate_fast_path` and #674's `run_index_cover_walk` are
  covered. Its loop is pure and `eval_expr` answers `V_null` for an unresolved
  `P_subquery`, so keeping the query would silently fold NULLs — the same reason
  #558 made it give up a subquery-bearing projection. Every scalar assertion in
  #664's test runs with the fast path forced ON and forced OFF and must agree,
  so a fast path that quietly kept such a query shows up as a disagreement
  rather than as a plausible number.

  `Sema.expr_has_subquery_ast` is gone with the refusal that was its only
  caller. Three tests that asserted the refusal now assert the answer
  (`test_agg_expr_495_488`, `test_agg_subquery_558`, `test_sema`); they were
  converted rather than deleted so the moved boundary is visible in the diff.

- **An explicit `ON CONFLICT` target beats the statement's conflict-resolution
  modifier, for the index it names (#639, decided 2026-08-06).** The modifier
  still governs every *other* index. Before this, `CA_ignore` matched above the
  upsert arm in both places that resolve a conflict — `check_insert_unique` for
  a secondary UNIQUE index and `execute_insert_write`'s `put_x` arm for the
  rowid-alias PK — so `INSERT OR IGNORE ... ON CONFLICT(k) DO UPDATE` silently
  skipped for *any* conflict: a caller who wrote both got "insert, or do
  nothing".

  **The target is resolved in its own pass, before the modifier sees anything,
  and that is what makes the answer well-defined rather than a micro-
  optimisation.** `check_insert_unique` used to fold over every unique index in
  one pass and let whichever conflicted first decide. With two unique indexes
  and a row conflicting on both, the outcome then depended on the order
  `Cat.indexes_for_table` returned them in — newest-first, i.e. on `CREATE
  UNIQUE INDEX` order: target first gave an upsert, the other first gave a skip
  under `OR IGNORE`. Same schema, same statement, two answers. When the target
  hits, the accumulator is reset to `(false, [], Some rid)`: `skip` is
  meaningless because the insert it was decided against is discarded, and
  `dels` **must** be empty because `execute_insert`'s upsert branch never calls
  `delete_replace_conflicts` — a non-empty `dels` there is a queued delete that
  never happens.

  **What this pass does NOT do is hand the check downstream — `write_row_rekeyed`
  itself performs no uniqueness probe.** Its index loop is still an unconditional
  `S.del` of the old key and `S.put` of the new one; see the `#667 (fixed)`
  paragraph immediately below for where that check now lives instead.
  Discarding the target pass's other-index verdicts here is sound regardless —
  they were computed against the row being INSERTED, which is discarded, and a
  `SET v = 42` does not touch the column they were about.

  **#667 (fixed): a DO UPDATE that writes a duplicate into another unique index
  now raises, via a check at the `execute_upsert_update` call site rather than
  inside `write_row_rekeyed`.** `write_row_rekeyed` is shared by three write
  paths — plain `UPDATE`, `UPSERT ... DO UPDATE`, and `ON UPDATE CASCADE` — so
  pushing the probe inside it would have changed the other two as well; that is
  a separate decision (see the `enforce_not_null` bullet above, which makes the
  same point). `execute_upsert_update` calls the new shared helper
  `check_indexes_unique_on_update` — one `Lwt_list.iter_s` over
  `Cat.indexes_for_table` calling `check_index_unique_on_update` per index —
  before calling `write_row_rekeyed`, passing it the already-computed
  `new_row_for_idx` again via `write_row_rekeyed`'s new `?new_row_for_idx` so
  the VIRTUAL-column evaluation isn't paid twice. `check_index_unique_on_update`
  already excludes the row being updated from its own conflict probe (by
  rowid) and exempts an unchanged key and a NULL-containing key (#290), so
  this includes the conflict-target index itself — a DO UPDATE that moves the
  very column named in `ON CONFLICT(...)` to a value a third row already
  holds is caught too, not just a DO UPDATE touching an unrelated index.
  `validate_update_unique`, the plain-UPDATE pre-pass, now calls the same
  `check_indexes_unique_on_update` helper instead of hand-rolling an identical
  loop — the two call sites cannot drift apart on a future change (a new
  exemption, an early exit, batching) the way the first revision of this fix
  would have let them.

  Two things about `check_index_unique_on_update` moved as part of this fix
  are correctness-bearing, not just relocation:

  - It (and `check_indexes_unique_on_update`) moved earlier in `exec.ml`, to
    just before `write_row_rekeyed`, so `execute_upsert_update` — defined
    before their original position — could call them.
  - `unique_violation_on_update`'s `unchanged` fast path used to compare only
    the indexed COLUMN values (`old_vs` vs `new_vs`), never whether `old_row`
    had matched the index's `idx_where_sql` at all. A row flipping into a
    PARTIAL unique index's domain without touching the indexed column — e.g.
    `active` going 0 → 1 under `UNIQUE INDEX ... WHERE active = 1` while `a`
    stays the same — read as "nothing moved" and skipped the probe, so
    `write_row_rekeyed` inserted a second live entry for a key another row
    already held under that index. `unchanged` now also requires `old_row` to
    have matched the WHERE clause. Pre-existing in `validate_update_unique`'s
    path too (same shared primitive) — fixed there for free by fixing the one
    function both now call.
  - `unique_violation_on_update` also used to recompute the row's index
    values independently, via `with_computed_virtuals_cols None [||]` —
    hardcoding a fresh clock and no bound params instead of reusing the ones
    its only caller, `check_index_unique_on_update`, had already evaluated
    `new_vs` with a few lines above. For a UNIQUE index over a
    clock-dependent VIRTUAL column that let the probe's seek key diverge from
    the value just validated. It now takes the already-computed `key_vals`
    directly and does no evaluation of its own, which removes the second
    computation entirely (a `#667` review finding on top of the `unchanged`
    one) rather than just aligning its inputs.

  Pinned by `test/test_upsert_unique_667.ml`, covering both conflict shapes
  (secondary UNIQUE index and rowid-alias PRIMARY KEY, since both funnel
  through `execute_upsert_update`), a plain-UPDATE regression check, and the
  partial-index WHERE-transition case for both the UPSERT and plain-UPDATE
  paths.

  **#693 (fixed): `ON UPDATE CASCADE` / `SET NULL` / `SET DEFAULT` — the third
  `write_row_rekeyed` caller named above — had no uniqueness probe at all, not
  even the imprecise one #667 fixed for upserts.** `update_col_in_tx` now
  calls the same shared `check_indexes_unique_on_update` before
  `write_row_rekeyed`, with `~clock:None ~params:[||]` (its existing constants
  — a cascade runs outside any statement's clock/params scope) and hands the
  already-computed `new_row_for_idx` through via `write_row_rekeyed`'s
  `?new_row_for_idx`, exactly as `execute_upsert_update` and
  `validate_update_unique` do. All three `write_row_rekeyed` callers now run
  the identical pre-write probe. `SET NULL` can never trip it — #290 exempts
  any NULL-containing key — but `SET DEFAULT` can, when the column's default
  collides with a value another live row already holds; pinned alongside the
  issue's own CASCADE repro in `test/test_cascade_unique_693.ml`.

  One consequence of the target pass is a change for the *raising* modifiers:
  bare / `OR ABORT` / `OR FAIL` / `OR ROLLBACK` used to report
  `UNIQUE constraint failed` for a second index the discarded insert row
  collided with, and now run the DO UPDATE. That was a false positive — the
  conflicting row is never written — and it is pinned by
  `raising_modifiers_no_longer_report_the_discarded_rows_conflict`.

  The **rowid-alias PK** is the other thing an ON CONFLICT clause can name, and
  it has no index, so it is invisible to that fold. When the clause names it,
  `execute_insert` probes the row key with one `S.get` *before*
  `check_insert_unique` — otherwise a secondary conflict would set `skip`
  (losing the DO UPDATE) or run `delete_replace_conflicts` (displacing rows for
  an insert that then never happens, because `put_x` discovers the alias
  conflict afterwards). The probe is only paid when an upsert clause names the
  alias column, so #350's plain-INSERT path is untouched. The arm in
  `execute_insert_write` is kept as a backstop, not the primary path.

  **Two things ride on that pre-probe, both decided rather than incidental**,
  because routing a conflicting row to the upsert branch skips
  `execute_insert_write` entirely and that function did more than one job:

  - **The insert row's NOT NULL check moved up, for upserts only.** It is now in
    `execute_insert`, entered when the statement is `OR IGNORE` *or* carries an
    upsert clause. Without that, `INSERT INTO t VALUES (1, ?) ON CONFLICT(k) DO
    UPDATE …` bound to NULL raised on `main` and silently became a successful DO
    UPDATE. The rule stands: an `ON CONFLICT` clause never intercepts a NOT NULL
    violation. The *secondary-index* shape is the one that changed to agree — it
    never raised here — and a statement with **no** upsert clause is untouched,
    including the precedence between a UNIQUE error and a NOT NULL one.
  - **`last_insert_rowid()` is no longer set by a DO UPDATE.** No row was
    inserted, so it should not move. Before #639 the alias-PK shape set it (it
    returned through the INSERT branch) and the secondary-index shape never did —
    the same "answer depends on which constraint you hit" split #639 is about.
    `test_upsert_on_pk_last_rowid` in `test/test_rowid_alias.ml` asserted the old
    answer **vacuously** (it seeded rowid 5 and upserted rowid 5, so the seed's
    own value satisfied it); it now seeds 7, upserts 5, and pins the new one. The
    comment it carried claimed SQLite parity for the opposite answer and was
    never oracle-checked; the reasoning runs the other way (SQLite sets the value
    at `OP_Insert` under `OPFLAG_LASTROWID`, and a DO UPDATE is generated as an
    UPDATE). If the oracle disagrees, set it in **both** shapes — do not restore
    the split.

  **An unremarked improvement, recorded so nobody finds it by bisect:** the
  pre-probe also removes a bogus #417 delta. The old alias-PK upsert path emitted
  *both* an `Updated` and a phantom `Inserted { rowid; row }` — with `row` being
  the *attempted insert* row, not the stored one — so any reactive view over the
  table saw a row that was never written. The upsert branch emits only `Updated`.

  **A conflict target that names no PRIMARY KEY or UNIQUE constraint is silently
  ignored, where SQLite rejects the statement** ("ON CONFLICT clause does not
  match any PRIMARY KEY or UNIQUE constraint"). Granary treats it as a plain
  INSERT and drops the `DO UPDATE`. Pre-existing, pinned by
  `a_non_unique_index_is_not_a_conflict_target`, and tracked as **#668** — it is
  #639's failure mode reached through a schema mistake instead of a modifier.
  `Exec.index_is_conflict_target` does require `idx_unique`, so a non-unique
  index can never be *promoted* into a target; what is missing is the rejection.

  **`OR REPLACE` defers to the target too, and that is a decision, not a side
  effect of the arm order (#639, decided 2026-08-06).** `INSERT OR REPLACE ...
  ON CONFLICT(k) DO UPDATE` now updates the conflicting row in place instead of
  deleting it and inserting the new one. The alternative — `CA_replace` keeping
  precedence over the named target — reproduces #639 exactly, for `REPLACE`
  instead of `IGNORE`: the caller writes an explicit `DO UPDATE` and the engine
  silently does something else with it. One rule for all six modifiers is the
  only reading under which writing both clauses means anything. This is
  **believed** to match SQLite but was **not oracle-checked**; if the oracle
  disagrees, the divergence is deliberate under "inspired by, not a port" and
  whoever changes it is re-deciding, not fixing an oversight.

  **NOT NULL is not a uniqueness conflict, so an `ON CONFLICT` clause never
  intercepts it** — the modifier does, per #599 above. The two directions
  differ and both are pinned in `test/test_or_ignore_upsert_639.ml`: a NULL in
  the row being *inserted* skips under `OR IGNORE`, while a NULL *assigned by
  the DO UPDATE* raises, because that write funnels through
  `write_row_rekeyed` → `enforce_not_null`. `Sema.bind_upsert_assignments`'s
  static literal-NULL check is therefore **not** suspended under `CA_ignore`
  (unlike `bind_insert_row`'s): the runtime answer below it is "raise", so the
  two levels agree rather than disagreeing. #639 noted that binder check was
  load-bearing while the runtime path was unreachable; the path is reachable
  now, and the check stays as the earlier, better-located error.

  `INSERT ... SELECT ... ON CONFLICT DO UPDATE` has no grammar at all
  (`S_insert_select` carries no `upsert_update`) and must stay a **parse
  error** rather than a silently-dropped clause — that would be #639 again in
  a new place.

- **A skipped `INSERT` leaves nothing behind in the STORE and the CATALOG,
  including its BEFORE INSERT trigger's nested DML, in an explicit transaction
  as well as in autocommit (#631, fixed 2026-08-06).** Read the scope literally:
  the two things it does *not* revert are the #240 dirty-table set and the #417
  row-level change feed. A spurious dirty mark is over-invalidation of an
  external cache and is safe; a stale `record_change` delta is a phantom row for
  a reactive view whose base table the trigger wrote to. **Neither is a
  regression** — autocommit's `S.rollback` never cleared them either — but the
  invariant above is about `Store` and `Schema_cache` state only. Tracked as
  #666. The undo used to be `if owned then S.rollback`,
  keyed on *who owns the transaction* rather than on *what the statement
  decided*, so the same statement left a trace or not depending on whether the
  caller had opened a `BEGIN`. It is now a statement-level savepoint
  (`Store.savepoint_begin` plus #280's schema-undo marker, which also restores
  #303's rowid counters), rolled back when the statement wrote nothing — so it
  covers the long-standing UNIQUE skip and #599's NOT NULL skip alike.

  It is taken **only** when the transaction is borrowed, a BEFORE INSERT
  trigger exists on the table, and the resolution is `CA_ignore`; outside that
  intersection no savepoint is pushed, which is what keeps it off the TPC-C
  write path (a B-tree savepoint clones the pager dirty set, and
  `Schema_cache.savepoint_begin` also encodes every columnar store). It is
  opened and resolved within one statement and never touches `explicit_txn` or
  the #555 poison flag, so it is **not** a second exit from a poisoned handle
  and "ROLLBACK is the sole exit" stays true. On an *exception* the savepoint
  is released, not rolled back: a raising statement's partial effects already
  survive in a borrowed transaction, and statement atomicity on error is a
  different problem.

- A column's `not_null` no longer records *why* it is set — declared or implied by a primary key — because #530 folded both into the one stored bit. Anything that removes a key therefore cannot restore the column's original nullability: `ALTER TABLE ... DROP COLUMN` on a composite-PK member clears `primary_key` on the survivors but deliberately leaves `not_null`, since the engine is still enforcing it. Two bits (or an origin tag) is the fix if this ever needs to be exact — not cleverness at the ALTER sites.

### A failing autocheckpoint is surfaced, never raised (#638)

Both auto paths used to run the checkpoint under
`Lwt.catch … (fun _ -> Lwt.return_unit)`, so every failure was discarded whole.
The background one (`maybe_autockpt_after_commit`) was the worse of the two —
nobody awaits that fiber — and a checkpoint that failed on every attempt was
completely invisible: the WAL grew without bound with no counter, no event and
no log, and the first symptom was a full disk or a very slow recovery. On a long
TPC-C run that reads as a performance cliff.

Since #638 a failure is **recorded and emitted, and still not raised to the
caller of the commit that triggered it**. That asymmetry is the decision, not an
oversight: the commit has already succeeded and its WAL frames are still valid
frames, so failing it would convert a deferrable maintenance problem into
spurious transaction failures. Three surfaces, all fed from the one
`Store.note_checkpoint_failure` chokepoint:

- `Store_event.Checkpoint_failed { target_frames; consecutive; message }` — the
  live signal, and the thing that finally balances the `Checkpoint_begin` an
  aborting checkpoint used to leave dangling.
- `Store.checkpoint_health` — sticky, so an operator can read it long after the
  failing commit returned. `consecutive_failures` (and `last_error`) are cleared
  by any checkpoint that completes and by `Store.clear_checkpoint_error`;
  `total_failures` is never cleared by success, because "this store has been
  unable to truncate its WAL at least once" is a different question from "is it
  failing right now".
- `PRAGMA checkpoint_status` — one row of
  `(total_failures, consecutive_failures, last_error)`; `last_error` is NULL
  when nothing has failed since the last completing checkpoint.

The **explicit** `Store.checkpoint` path already surfaced its failure by
raising, and still does — it merely feeds the same counters, so
`checkpoint_health` describes the store rather than only its automatic path. Do
not "fix" the remaining silence by making the auto path raise; the escalation
this issue asks for is visibility. `test/test_checkpoint_failure_638.ml` pins
both halves (observable, and the triggering commit still succeeds with its data
readable) by failing the main-file `write_page` — in WAL mode the main file is
written *only* by a checkpoint, so commits keep succeeding while every
checkpoint fails.

### The writer lock is measured, and every acquisition goes through one door (#718)

`Store.lock_stats` reports the writer lock's wait and hold time per acquisition
site (`Txn`, `Checkpoint`, `Autocheckpoint`, `Commit_sink`), plus a "who held it
when the wait began" matrix. It exists because service-time profiling cannot see
the critical section: `commit_wal` releases the lock *before* it fsyncs, and a
`BEGIN` that finds the lock held is waiting rather than working. #716's
"75.3% of NewOrder service time is transaction control" was sound arithmetic
over service time; "75.3% of the critical section" did not follow from it.

**Every `Rwlock.acquire_write t.lock` and `Rwlock.release_write t.lock` in
`store.ml` goes through `acquire_writer` / `release_writer`.** A site that
acquires directly is not merely unmeasured — it holds the lock while the
accumulator believes nobody does, so it corrupts the `blocked_by` attribution of
everyone who waits behind it. `report.unattributed_waits` and
`unbalanced_releases` are the detectors for exactly that, and they are bug
signals rather than measurements: a bypassed acquisition leaves the totals
looking entirely plausible. A new acquisition site owes itself a new `site`
constructor rather than borrowing one.

Two properties are worth knowing before editing this:

- **The contention COUNTS need no clock.** They come from
  `Rwlock.writer_active` sampled immediately before the acquire, which is exact
  (`acquire_write` blocks iff a writer holds the lock at that moment), so they
  stay valid on a pure-Mirage build where every duration is `0.`.
  `report.clock_installed` is what keeps a reader from mistaking that zero for
  "nothing waited" — `Store.set_clock` installs the clock, and its default
  returns `0.`.
- **A wait is attributed to the holder observed when the wait BEGAN.**
  `Rwlock.acquire_write` wakes every waiter on release and lets the scheduler
  pick, so there is no queue position to read; a wait spanning several holders
  lands wholly on the first. Documented approximation, deliberate.

What it measured, first time out (`bench/results/2026-09-02-tpcc-stmt-profile-granary.csv`):
at **one** terminal the writer lock is **99.8% occupied**, and the background
autocheckpoint holds it for **25.1%** of the interval while being the blocker
for **100%** of all writer-lock wait. That settles #716 item 2 — `BEGIN` is
waiting, and it waits for `maybe_autockpt_after_commit` — and it means no part
of #716's headline converts to throughput at `TERMINALS > 1` until something
leaves the critical section. Tracked as #719.

**This instrument cannot see scheduler drain, so it narrows that hypothesis
rather than excluding it.** An uncontended `Rwlock.acquire_write` returns an
already-resolved promise, so an uncontended acquisition contains no yield by
construction; the sub-millisecond residual in the `wait_ms` column is the
instrument's own two-clock-read floor (~0.3 µs per acquisition, and the
zero-contention `autocheckpoint` row is the built-in control for it), not drain.
Do not quote that residual as a drain measurement.
`COMMIT`'s fsync is outside all of it by construction and is a separate
question.

Any test of this must be **file-backed and WAL-mode** for the site attribution:
the Mem backend has no WAL, so `Store.checkpoint` returns before it acquires
anything and no autocheckpoint is ever dispatched — an in-memory version passes
while measuring nothing. `test/test_lock_stats_718.ml` is.

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
`ROLLBACK` aborts this scope's transaction — whether it then refills the slot or
leaves it empty — the token no longer matches; the combinator detects that at
scope exit and issues **neither** COMMIT nor ROLLBACK, returning `Error`, since
either would act on state that is no longer the scope's. It does *not* cover
statements *inside* the body: those still resolve the transaction from the
handle's mutable slot, so a displaced scope's writes land in the other fiber's
transaction before the boundary check reports the loss. Binding statements to
their owner is #555 option 1's work. **This is not permission to share a handle
across fibers** — `create_worker_handle` still is.

**The token must be invalidated by every path that ends a transaction, not just
by `with_transaction`'s own exit.** This shipped wrong once and the failure was
the guard's own headline case: `txn_scope` was written only by the combinator,
so after `B: BEGIN` (collide) → `B: ROLLBACK` (A's transaction aborted) →
`B: BEGIN` (B's transaction now in the slot), A's stale `Some 1` still equalled
the handle's `Some 1`, and A's scope exit COMMITted **B's** transaction and
returned `Ok`. `stolen_txn_msg` only fired when the displacing fiber also used
`with_transaction` — i.e. never in the spelling #584 is written in. The clears
now sit next to every `explicit_txn` assignment: `begin_txn` and `savepoint_txn`'s
auto-begin clear it when they *fill* the slot; `force_rollback_txn`, `commit_txn`,
`rollback_txn` and `release_savepoint`'s auto-commit clear it when they *empty*
it. Keep them adjacent — a token that outlives its transaction turns the guard
into a false match, which is worse than no guard at all.

**The token lives on the handle the BEGIN routed to** (`active_handle`), not on
the top-level handle, because that sub-handle is the one whose slot those clears
maintain. #598 keeps the routing from moving under an open scope, so the handle
is stable for the extent.

One inherited wrinkle: the rollback-on-exception path issues `ROLLBACK`, which
clears the handle's *poison* flag unconditionally (#555 made it unconditional so
a handle can never be stranded). Poison is connection state, not transaction
state, so an unwinding scope can clear a poison another fiber was told to
recover from; that fiber's own `ROLLBACK` then answers "no active transaction".
Nothing is lost, and making the clear conditional would strand the handle.

The body owns the statements, not the transaction: a `BEGIN` inside it poisons,
and a `COMMIT`/`ROLLBACK` inside it empties the slot *and clears the token*, so
the scope exit takes the displacement branch and returns `Error` without issuing
a second COMMIT. The body's work lands as the body asked; the `Error` is the
caller's only signal that the scope did not end the way it looks like it did.
`SAVEPOINT`/`RELEASE`/`ROLLBACK TO` are fine and leave the transaction in place.
Pinned by `test/test_with_transaction_585.ml`.

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
allocator's live state was lifted out of the cached `table_meta` and into a
tree-id-keyed table that **`Store.t` owns** (`Store.rowid_counters`, #633), so
every catalog opened over one store shares one allocator by construction. Every
read of a cached `table_meta` is patched from that table on the way out and
every write publishes to it on the way in, which keeps `table_meta` the only
type the rest of the engine sees.

**#633 moved the ownership; do not move it back.** It was originally threaded by
hand as `Cat.open_ ?rowid_counters` / `Db.of_store ?rowid_counters`, with
`create_worker_handle` as the only caller that remembered — and the penalty for
the next caller forgetting was #589 verbatim (silent row loss plus durable index
corruption). A tree id is only an identity within one store, so the store is the
only correct home. This also makes ATTACH right by type rather than by accident:
an attached schema is a different `Store.t` and therefore, necessarily, a
different set of counters.

Two rules make the sharing correct and must survive any future edit:

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
  go stale. **The rule is absolute: an entry that exists is never overwritten,
  whatever its value.** PR #650 briefly carved out an exception for
  `empty_next_rowid` ("a sentinel has never allocated, so seeding over it can
  only move the counter up") and it was wrong twice over — the sentinel is *also*
  written deliberately by the sqlite_sequence reset paths
  (`reset_next_rowid_in_txn`, `reset_all_next_rowid_in_txn`) *inside* an open
  transaction, so a concurrent `open_` would clobber the reset with the committed
  pre-reset high-water; and the obvious guard (skip tables dirty in
  `rowid_bumped`) does not work, because `rowid_bumped` is **per-cache** — the
  resetting transaction's flag lives on its own cache and the seeding cache is a
  brand-new one whose set is empty. There is no cheap store-wide discriminator,
  so there is no exception.

  **A test that wants a genuine restart must close a file-backed store**, not
  re-open a catalog over a live one: since #633 the latter is a worker handle and
  correctly shares the counter. `test_mirror_recovers_next_rowid` and
  `test_mirror_recovers_negative_next_rowid` in `test_catalog.ml` were converted
  for exactly this reason.

  **Accepted residual:** with no exception, mirror recovery is invisible to a
  *second* catalog over a *live* store whose counter is still the sentinel — the
  recovered `max(rowid)+1` loses to the sentinel the original `CREATE`
  published. Reaching it needs rows in the data tree that the allocator never
  issued *and* a lost `_sys_tables` row, on a still-open store: corruption on a
  live store, not a restart. The rejected alternative silently reverses a
  sqlite_sequence reset and needs no corruption at all. It is the better trade,
  but it is a trade, and nothing in the suite covers it.
- **Every allocator allocates *and publishes* with the writer lock held (#632).**
  That — not the weaker "the allocator holds the lock" — is the property that
  makes the shared table safe, because it is what makes the interval between
  reading the counter and publishing the new one an interval in which nothing
  else can allocate. `next_rowid_in_txn` and `bump_next_rowid_in_txn` have it by
  construction (they are handed a txn). `Catalog.next_rowid`, the autocommit
  allocator, used to publish *before* `S.rw_begin`: another handle could take
  the lock, `ROLLBACK`, and *lower* the counter through #293's recompute,
  discarding an allocation already handed out. It now takes the lock first and
  publishes immediately after the allocation, with no `Lwt` yield in between.

  **`S.commit` does not count as "under the lock", and this is the trap to
  know.** It releases the writer lock *before* its promise resolves —
  `commit_wal` calls `unlock_once ()` (`store.ml:2071`) and only then awaits the
  fsync; the non-WAL arm releases from a `Lwt.finalize` handler
  (`store.ml:2184`). So publishing in a `let%lwt () = S.commit tx in …`
  continuation runs *after* other fibers can take the lock, leaving the shared
  counter too LOW for the whole fsync — the direction that collides. (Too HIGH
  merely skips ids, and is the residual this design accepts if a commit fails.)
  PR #650 shipped that ordering for one round before review caught it.
  **The in-memory backend cannot detect the difference** — its commit releases
  and returns an already-resolved promise (`store.ml:2164`), so the bind runs
  synchronously and both orderings pass. Any test for this class of bug must be
  **WAL-mode and on disk**; `wal_two_fiber_catalog_next_rowid` and
  `wal_two_fiber_inserts` in `test/test_rowid_counter_ownership_632.ml` are.

  The unknown-table `Failure` stays *synchronous* (raised before any `Lwt.t`
  exists) — `test_rowid_unknown_table` in `test_catalog.ml` pins the contract.

  **The ROLLBACK side of the same shared table was not brought under this
  discipline until #706 — same class of bug as #632, different function.**
  `Cat.recompute_rowid_counters_after_rollback` re-derives a rolled-back
  table's counter (`recover_next_rowid`'s RO tree scan, or
  `read_committed_next_rowid` for AUTOINCREMENT) and publishes it via
  `Schema_cache.set_rowid_durable`. The RO scan correctly runs *after*
  `S.rollback` releases the writer lock (its own RO txn would otherwise
  self-deadlock against it), but the **publish** used to run right after with
  no lock at all — an unlocked write into the one counters table every
  catalog over the store shares, exactly what #632 eliminated for the
  allocate path. A second worker handle could `rw_begin` in that window, read
  the still-stale (too-high, not-yet-recomputed) counter, allocate from it,
  and commit — and the recompute's blind publish would then clobber the
  counter back down past that commit's id, so the next allocation reissued it
  and silently overwrote the row. Reliably reproducible with
  `GRANARY_TPCC_TERMINALS >= 2` once `Tpcc_driver`'s terminals got genuine
  concurrency (#703): NewOrder's ~1% rollback rate raced a sibling terminal's
  concurrent allocation on `new_order`/`order_line` on nearly every run.

  Unlike #632, re-acquiring the lock around a **blind** overwrite is not
  enough, and would break the common (no-race) case: the value already
  sitting in the shared table when the recompute is ready to publish is
  usually this *same* rollback's own stale bump — the value the recompute
  exists to correct — so "the live value differs from what I'm about to
  write" cannot tell that apart from "a concurrent commit already moved
  it". The fix is a compare-and-swap, `Schema_cache.cas_rowid_durable`:
  capture `expected`, the live counter as of the *start* of the recompute
  (before either RO scan runs); after the scan, re-acquire the writer lock
  and publish the recomputed value only if the live value still equals
  `expected`. A match means nothing else touched it — safe to lower, same as
  before #706. A mismatch means a concurrent commit already advanced it past
  what this recompute saw, and the recompute leaves it alone; the "wasted"
  ids between the recomputed value and the live one are the same accepted
  "too HIGH" residual #632's own writeup names, never a reissued one. All of
  a rollback's bumped tables are scanned first (unlocked), then published
  together under a *single* `rw_begin`/`rollback` bracket (not `commit` —
  nothing is written to any tree, only the in-memory counters table is
  republished, and `rollback` is the cheaper release: no header bump, no WAL
  frame).

  `test/test_rollback_recompute_publish_race_706.ml` pins it via
  `Cat.rollback_recompute_publish_hook`, a test-only seam (production code
  never assigns it) awaited at exactly the point between the RO scans
  finishing and the lock being re-acquired to publish — deterministic
  interleaving of a second handle's competing allocation, rather than relying
  on real scheduling timing the way the TPC-C repro does. Must run **WAL-mode
  and on disk**, for the same reason as #632's own tests: the in-memory
  backend's RO scan does no real (yielding) I/O, so nothing would interleave
  with it even without the hook.

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

**What is still unsafe about a worker handle** — the DDL/scoping items below do
not corrupt anything, but the first one, found while implementing #703, does:

- **#706 (open, found via #703): `ROLLBACK`'s rowid-counter recompute publishes
  outside the writer lock and can race a concurrent worker handle's allocation
  on the same tree, reproducing #589's exact symptom (a reused engine rowid,
  second `S.put` silently overwriting the first row).** `force_rollback_txn`
  calls `S.rollback` — which releases the writer lock as its last synchronous
  step — and only THEN calls `Cat.recompute_rowid_counters_after_rollback`,
  deliberately outside the lock (its RO scan would deadlock inside `rw_begin`
  otherwise). That recompute's publish
  (`Schema_cache.set_rowid_durable` → `Catalog.publish`) is an unconditional
  `Hashtbl.replace` on `Store.rowid_counters` — no compare-and-swap, no
  re-acquisition of the lock. If a sibling worker handle begins, allocates from
  the same tree, and commits in the window between the rollback's unlock and
  this recompute's publish, the publish clobbers that legitimate allocation
  back down to a stale, lower value, and the next `INSERT` reissues an
  already-used rowid. This is the *same* mechanism the "#632" bullet above
  documents as fixed for `Catalog.next_rowid` — "every allocator allocates
  *and* publishes with the writer lock held" — except the rollback-recompute
  path was never brought under that discipline, because nothing before #703
  drove genuinely concurrent worker handles through a multi-statement,
  sometimes-rolling-back transaction on the same table under load. TPC-C's
  NewOrder profile's spec-mandated ~1% invalid-item `ROLLBACK` (after already
  bumping `orders`/`new_order`/`order_line`'s counters earlier in the same
  transaction) is exactly the shape that exposes it, and it reproduces
  reliably — not a flake — at `GRANARY_TPCC_TERMINALS` 2, 4, 8 and 16 (never at
  1, where there is no second handle to race). See #706 for the full
  repro/analysis. **Until #706 is fixed, do not treat a multi-terminal
  `Tpcc_driver`/`bench_tpcc` run — or any other workload that rolls back a
  bumped rowid table concurrently with a sibling worker handle's writes — as
  producing a consistent database; always check its own consistency oracle
  before trusting output from such a run.**
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
  it `~cohort` by hand; `create_worker_handle` is the only caller that does so
  today. It no longer owes `~rowid_counters` — see below.

`Db.of_store` over an already-open store no longer has a rowid-allocator
argument to forget, as of #633: the allocator hangs off `Store.t`, so naming the
same store *is* sharing it. `test/test_rowid_counter_ownership_632.ml` exercises
that path directly (bare `of_store`, no `create_worker_handle`) alongside the
#632 rollback cases and the four invariants above. **`~cohort` is the one
argument still owed**, and for a different reason: it is about handle lifetime,
not allocator identity.

**A `VACUUM` on any handle kills every other handle over the same store (#634).**
VACUUM closes the `Store.t`, rebuilds the file and swaps a freshly opened store
into the handle that ran it. Siblings cannot follow: they keep the closed store
*and* the pre-VACUUM `rowid_counters` table. Since #634 that is **loud** — the
issue's option 2, not option 3. Handles over one store share a
`Db.store_cohort`; VACUUM bumps it and re-stamps only the vacuuming handle, so
every sibling is stale and every statement on it is refused with an error naming
VACUUM. `Db.stale_after_vacuum` exposes it.

**`ROLLBACK` is *not* an exit, and that is the deliberate difference from #555's
poison.** A poisoned handle is recoverable because its store is still there; a
stale handle's store is closed, so there is nothing to roll back into. The
`Op_rollback` arm of `execute_control_op` is exempt from the poison gate and
**below** the staleness gate for exactly that reason. The only supported
operation on a stale handle is `Db.close`, which deliberately skips `S.close`
(VACUUM already closed that store; a second teardown touches closed fds).

Two consequences worth knowing before editing this:

- **VACUUM does not move tree ids.** `copy_all_trees` writes each tid to the
  same tid in the rebuilt file, so #589's "counters are keyed by tree id" rule is
  untouched: the vacuuming handle gets a *fresh* counter table (`Cat.open_
  new_store`, no `?rowid_counters`), re-seeded from the copied data — rescanned
  for a plain rowid table, read from the copied `_sys_tables` row for
  AUTOINCREMENT. The table is **replaced wholesale, not remapped**. Anything
  that makes VACUUM renumber trees must remap or clear that table with it.
- **A worker handle cannot itself run VACUUM**, because `create_worker_handle`
  passes no `~file_path` — the statement is refused as "not file-backed" long
  before the cohort is consulted. So the symmetric "worker vacuums, parent goes
  stale" case is unreachable today; forwarding `file_path` to workers would make
  it reachable, and the cohort already handles it.

**Since #703, `Tpcc_driver` no longer runs a one-deep pool.** `test/bench_tpcc.ml`
mints one worker handle per terminal (after the load phase, per the DDL
limitation above) instead of serializing every terminal through a single
`Db.t`. The terminal-count sweep is no longer flat-by-construction — see
`docs/benchmarks/BENCHMARKS-TPCC.md`'s `#703` section for the measured shape —
but see the #706 bullet above: a multi-terminal run currently trips its own
consistency oracle, because #703 is the first thing in the tree to drive real
concurrent worker-handle writers through a workload that rolls back a bumped
rowid table under load.

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
