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

Six gates are **not** wall-clock and therefore **not** neutralized anywhere:

| test | guards | gate | knob |
|---|---|---|---|
| `test_not_null_600` | #600 `PRAGMA not_null_check` retaining every violating row | marginal peak live heap < 4 words/row when the table doubles | `GRANARY_MEM_MAX_WORDS_PER_ROW` |
| `test_not_null_repair_630` | #630 `PRAGMA not_null_repair`'s **scan** draining the tree via `cursor_open` | same gate, but with the violation count held FIXED at 5 while the table doubles, so only the scan can move it | `GRANARY_MEM_MAX_WORDS_PER_ROW` |
| `test_correlated_exists_493` | #493 a correlated `EXISTS` leaking one RO snapshot per outer row | peak live RO snapshots does not grow when the outer rows go 100 → 400 | `GRANARY_MAX_LIVE_READERS` |
| `test_agg_retention_423` | #423 the net-zero SUM-group retention in `Aggregate` growing per UPDATE rather than per distinct group, or costing more than one map node plus one record | marginal live heap < 16 words per retained group when the group count doubles, and quadrupling the churn over ONE key adds < 16 words total | `GRANARY_MEM_MAX_WORDS_PER_GROUP` |
| `test_view_callback_746` | #746 `Db.register_view_callback` going back to an O(n^2) list append | doubling the registrations must not more than double the words allocated (< 2.5; linear is 2.0, the old `@` append measured 4.0) | `GRANARY_MEM_MAX_CALLBACK_SLOPE` |
| `test_autoinc_mirror_316` | #316 the #314 per-row AUTOINCREMENT catalog-mirror write growing beyond its measured cost | an autocommit AUTOINCREMENT insert allocates < 3.0x a plain rowid one (measured 1.20 at 3 columns, 1.27 at 30), and < 1.5x inside an explicit transaction (measured 1.001) | `GRANARY_MIRROR_MAX_RATIO` |
| `test_point_lookup_alloc_416` | #416 the warm point-lookup read path re-growing its per-lookup allocation — specifically the `Op_project` `Lwt_stream.map` layer and the per-snapshot meta-tree root resolution | a warm `Op_rowid_lookup` re-execution allocates < 1050 words (measured 950.9, deterministic to the decimal across runs) | `GRANARY_MEM_MAX_WORDS_PER_LOOKUP` |

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

**`test_agg_retention_423` gates on the SETTLED heap, not the sampled peak, and
that is the opposite of the other three for a reason.** They bound a
*transient* — a scan that must not retain what it walks — which only a peak can
see. #423 bounds what *survives*, so the settled heap after a `full_major` with
the operator still reachable is the figure that carries it, and the `Gc` alarm's
asynchronous samples routinely all land below that (the alarm fires at
major-slice boundaries, not on demand). The test prints `max sampled settled` as
its peak so a peak below the settled heap is not mistaken for a smaller
footprint. Its two measured numbers are **9.00 words per retained group** for the
bare operator with an immediate key and **16.00 words** for
`Reactive_view.Agg_engine`'s boxed-`INTEGER` key — exact, not approximate,
because they are block layouts (one `Map` node at 6 words plus the `{ mult; aggv }`
record at 3, plus 7 for the key) rather than allocator behaviour. The 16-word
ceiling therefore has ~1.8x headroom over the bare operator and is far below the
20+ any *element*-retaining regression would cost. It is armed on the first run
that produced the number, which is what `test_scan_borrow_481` below asks for.

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

**`bench_wal_fsync_overlap` sizes its own reader workload, and that calibration
is where its flakes come from — not the ceiling (#623, 2026-09-03).** The gate's
arithmetic ceiling is `1 + T_readers / T_writer`, so `read_ops` is chosen at
startup as `target_seconds / per_op` from paired probe runs rather than
hard-coded. That divisor is the whole risk surface: a per-op estimate that is
too LOW oversizes the workload, which is #538's root cause, and the resize
safety net in `measure` deliberately does not cover the `(1.15, 1.667]` oversize
band, so a draw in that range is not self-correcting. #590 removed the
estimator's *bias*; #623 is the residual *spread* it left on the record — the
chosen `read_ops` spanned ~3.3x run-to-run on a quiet box, wider than #590's
stated ~2x. The fix is seven paired probe reps folded with a symmetric trimmed
mean, where it was three folded with a median. **The trimmed mean is the median
generalised, not a replacement for it** — on three draws the two are the same
number, which is what makes this a variance change rather than a re-decision of
#538's fold — and on more draws it uses more of them, so its sampling variance
is lower without moving the centre, and in particular without moving it *down*
(the marginal's noise is a difference of two one-sided delays and is therefore
symmetric; where a loaded box does skew it, the trimmed mean sits *above* the
median, which is the safe side). Pinned by three deterministic cases in the
file's own `statistics` suite, which need no quiet box because estimator
variance is a property of the arithmetic — unlike the false-failure rate the
issue's alternative remedy would have measured. Cost: calibration is ~2.5 s
longer, on a run that takes ~10 s and up to ~30 s when it retries.

**It does NOT close #623, and the negative measurement is the reason.** On a
*loaded* box (12-core, shared, load 2.3-4.6), 10 interleaved old/new pairs of
the calibration gave max/min 2.46x before and 2.69x after — no narrowing, and
well inside what 10 draws per arm resolve. The estimator's variance genuinely
falls; it is simply not the dominant term once load varies between calibration
and measurement, which is the residual `reader_ratio`'s own comment already
names. #623's number was taken on a *quiet* box and only a quiet box can
retire it. The untried next lever is `probe_ops`, not more reps: the marginal
is a difference of two probes, so raising the probe SIZE improves its
signal-to-noise linearly where more reps only do so as the square root. Note
also that the issue's "an oversized `read_ops` fails loudly" framing is only
half the story — an *undersized* one drives the secondary ceiling
`1 + T_r/T_w` down to the 1.2 floor and reports INCONCLUSIVE, and a run whose
every config lands there fails as "measured nothing".

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

### Before pushing: dune-file formatting + merlint + shellcheck

The CI **lint** job runs `dune build @fmt` (dune-file formatting *and* ocamlformat), `merlint`, and — since #591 — `shellcheck` over `scripts/*.sh`. Formatting only your `.ml`/`.mli` is **not** enough — CI still fails on unformatted `dune` files, merlint findings, or a shell finding.

```sh
# formatting (both .ml/.mli and dune files) — see above
sh scripts/check-fmt.sh

# merlint — run from the workspace root; expect 0 issues for your files
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint

# shell lint — only if you touched a script; self-wraps podman like check-fmt.sh
sh scripts/check-shell.sh
```

**Every script in `scripts/` is an enforcement gate or a dev tool, and the three `check-*.sh` ones now self-test on every run** (#591). That is the shape to copy: a gate that has never been observed to FAIL is a gate nobody knows works, so each plants a violation in a throwaway tree, asserts its own machinery rejects it, and refuses to report a verdict if that self-check is broken. `check-shell.sh` also **fails closed when `shellcheck` is missing** rather than skipping — a lint gate that silently passes with no linter installed reports green for a tree nobody checked, which is #549's honour system in a new place.

What the shellcheck gate does *not* buy, so nobody over-reads a green run: it is a **syntactic** linter. The fail-open it was filed for (PR #582: an empty allowlist variable made `grep -vFf` match every line, so the script printed `Policy OK` over a planted violation) is a *semantic* bug and shellcheck cannot see it. The self-tests are what cover meaning; shellcheck covers the neighbouring mechanical class — unquoted expansions, unassigned references, misused test operators. Where a script splits a word deliberately it carries an inline `# shellcheck disable=SC…` **and a comment saying why**, which is a review artefact rather than a silent exemption.

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

**Some exports exist only so a guard can hold onto them, and pruning one breaks
nothing — which is what makes it dangerous (#619).**
`Granary_encoding.Sql_ident.sql_keywords` / `.is_sql_keyword` /
`.ident_needs_quoting` have no caller in `lib/` outside their own module: the
engine reaches for `quote_ident` alone. Their consumer is the `lexer.mll` drift
guard in `test/test_ident_quoting_572.ml`, which re-derives the keyword table
from the lexer and fails when the two have drifted — the only thing binding the
list to the lexer now that they live in different libraries. Delete them and
nothing goes red; the guard simply stops guarding, and the next reserved word
added to the lexer corrupts stored SQL again (#577's failure mode through the
back door). `docs/DEAD_CODE.md` carries the never-prune list, and each such
value carries the reason in its own `.mli`.

Until #619 the guard held that handle one indirection further out, through
`Ast.sql_keywords` / `Ast.is_sql_keyword` / `Ast.ident_needs_quoting`, which had
**no** consumers in `lib/` at all. Those three aliases are gone; `Ast.quote_ident`
stays, because `Ast.expr_to_sql` calls it and `Exec.quote_ident` re-exports it.

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
~/bin/forgejo pr create IoTReadyNext/granary \
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
~/bin/forgejo issue create IoTReadyNext/granary --title="..." --body="..."
```

### Testing standards

- Target 100% line coverage on every module.
- Add QCheck property tests for every non-trivial function (see existing tests for patterns).
- Tests that require real SQLite live in `test/test_sqlite_compare.ml`; they skip gracefully via `sqlite3_available ()` (`:18`) when `sqlite3` is not in `PATH`, and its header comment has the exact `podman run` invocation that mounts the host `sqlite3` and its libs into the container. **There is no `test/compare_sqlite/` directory** — that path was in this file for a while and sent agents looking for a harness they concluded did not exist.
- Nothing in the tree reads `GRANARY_TEST_SQLITE`; it is a phantom invented by the same stale sentence. The SQLite comparison tests gate on `sqlite3` in `PATH`; the slow TPC-C smoke run gates on `GRANARY_TPCC_SMOKE` (`test/test_tpcc_smoke.ml:238`).
- Granary is **inspired by** SQLite, not a port of it. When pinning a behaviour that differs from SQLite, record the divergence deliberately rather than assuming parity is the goal — e.g. since #530 every PRIMARY KEY column implies NOT NULL, whichever way the key is spelled, where SQLite leaves PK columns nullable on *rowid* tables (its legacy behaviour; it does enforce PK NOT NULL on `WITHOUT ROWID` tables, which this engine also supports). `ALTER TABLE ... ADD COLUMN ... PRIMARY KEY` is refused outright for the same reason: that path built no backing index, so it could only ever have produced a "primary key" that was not one.

### Decisions and divergences (index)

The bullets below are one-line summaries. Full rationale, mechanism, and
pinning-test detail for every one of them lives in `docs/DECISIONS.md` — read
that file before touching any of this behavior, since the reasoning (and the
things that were tried and rejected) is usually the point.

- **NaN sorts below every number, in both the comparator and the index
  encoding (#536).** Granary keeps NaN as a real, totally-ordered value and
  diverges from SQLite (which treats it as NULL) in both directions of `>=`/`<=`.
- **A cross-numeric JOIN KEY equality matches, in both join executors
  (#743).** #738 fixed this for `col = literal`; the hash join's canonical key
  and the nested-loop probe's typed index seek closed the same gap for `ON
  l.a = r.b`. The FK child-reference probe had the analogous bug; fixed
  separately in #755 (below).
- **A cross-numeric FK child reference is found by RESTRICT and every
  cascade action, indexed or not (#755).** `Exec.fk_child_has_ref_multi{,_in_tx}`
  and `Exec.scan_child_rows_multi_tx` (the latter backs the immediate RESTRICT
  check AND SET NULL/SET DEFAULT/CASCADE) now seek the child index through
  `Exec.index_lookup_values`, keyed by the child column's declared type — the
  same rule #743 gave the nested-loop join probe. A VIRTUAL generated
  FK-child column is excluded from the indexed fast path
  (`Exec.child_index_key_types`) because its expression's actual runtime
  storage class can differ from its declared column type, unlike an ordinary
  or STORED column (`Row.encode_col_value` enforces that agreement only for
  those). PR #765's review chased the deferred-recheck path's identity
  problem twice on the recheck side — `Exec.make_fk_recheck` re-resolving by
  ordinal (`Exec.fk_ordinal`, stable across `RENAME COLUMN`) then raising
  loudly instead of silently reporting "not violated" when `DROP COLUMN`
  left a constraint unresolvable — before a THIRD mutation shape (`DROP
  COLUMN` then `ADD COLUMN` reusing the identical name) showed that fixing
  the recheck side can never get ahead of the next mutation shape, because
  the recheck only ever sees the schema AFTER the mutation. Round 3 fixes
  it at the source instead: `ALTER TABLE ... RENAME COLUMN` / `DROP COLUMN`
  now **refuses outright** when the column still has a deferred FK
  obligation pending in the current transaction (`Exec.fk_obligation_conflict`,
  consulting `Cat.peek_pending_fk_checks`), matching this project's own
  conservative-refusal precedent for the structurally identical problem
  (`ALTER TABLE ... RENAME` refusing when a view/trigger depends on the
  table, #673/#645). The recheck-side ordinal/loud-failure logic stays as
  defence in depth. `precheck_update_fk`/`precheck_delete_fk` (the immediate
  RESTRICT path) also now fail with the same loud, FK-specific message the
  deferred path already gave, instead of a bare internal `Failure "column
  not found"`. Round 4 found the same disagreement one layer further down —
  four CASCADE-dispatch functions (`cascade_delete_fk`, `apply_update_cascade_fk`,
  `apply_delete_cascade_fk`, `cascade_update_fk`) resolved FK columns with
  the raw `find_col_idx_by_name{,_opt}` and disagreed with each other and
  with RESTRICT (a silent no-op, or a bare crash, for the identical
  corrupted-column condition) — plus a reopened `List.nth_opt` negative-index
  crash trap in round 3's own new `Exec.fk_obligation_conflict`. All now
  route through `Exec.resolve_fk_col_idxs` / guard the sentinel explicitly.
  Round 5 found `cascade_update_fk` was the one FK call site missing the
  `any_null_val` guard every other one has (reachable via a second-level
  `ON UPDATE CASCADE` fan-out over a nullable composite FK column — fixed
  with the same guard, no new logic), and separately narrowed
  `child_index_key_types`'s blanket VIRTUAL-column disqualification: a
  VIRTUAL column whose expression is a top-level `CAST` to its own
  declared type is now trusted like an ordinary column (`Exec.eval_cast`
  is a total, exhaustive match on the target type, so it GUARANTEES that
  storage class regardless of the inner expression), closing a
  performance cliff for a composite FK with one generated helper column
  without weakening round 1's original fix for the general case.
  `Exec.child_index_key_types` is now exposed in `exec.mli` so a test can
  confirm the fast-path/full-scan decision directly. Full write-up —
  including the residual this does NOT close (`RENAME TABLE`/`DROP TABLE`
  can still desync a pending check by table name, tracked as #768) — in
  `docs/DECISIONS.md`.
- **`OR IGNORE` skips a NOT NULL violation; every other resolution, including
  `OR REPLACE`, raises (#599).** Diverges from SQLite's OR-REPLACE-substitutes-
  DEFAULT behavior deliberately.
- **An aggregated SELECT's ORDER BY may only reference grouped columns or
  output aliases/ordinals, or it refuses (#663).** SQLite accepts a bare
  non-grouped column and picks an arbitrary row's value; granary treats that as
  a wrong-answer risk rather than a permissiveness worth keeping.
- **`COLLATE` is a comparison attribute and never rewrites the emitted value
  (#722).** Every comparison site keys both operands through one shared
  `collate_key`; `GROUP BY x COLLATE ...` stays a parse error (no expression
  slot in the grammar).
- **One rule resolves a correlated subquery's outer references at any depth or
  clause (#635/#626/#615).** A qualified reference names a scope identifier
  (alias, or table name if unaliased); anything unresolvable is a loud error,
  never a silent NULL or empty result.
- **A comma in FROM is an INNER JOIN on the literal `1`; a derived table
  desugars to a CTE (#486).** The comma form is planned via an extracted WHERE
  equality, not a real cartesian product; a reactive view over a derived table
  is refused (no base tables to invalidate on).
- **A reactive view must project explicitly — `SELECT *` is refused statically
  (#747).** A behavior break from the old runtime, arity-guessing guard. Plain
  `CREATE VIEW ... SELECT *` is unaffected.
- **A compound-root reactive view (`UNION`/`INTERSECT`/`EXCEPT`) is refused
  (#750).** It used to materialize once and silently never update again.
- **A view is resolved at every FROM position, at any depth, and each
  subquery gets its own expansion (#496/#497).** Fixes both "view named in a
  JOIN" and "view named inside a subquery" failing silently; cycles are
  refused.
- **A GENERATED column's NOT NULL is enforced on its computed value, at bind
  time and at runtime (#629).** STORED columns are computed before the check
  at every row-store write site; VIRTUAL columns are recomputed before
  enforcement rather than exempted.
- **A GENERATED column on a `USING COLUMNSTORE` table is refused at DDL
  (#660).** The columnar write/read paths never compute one; refusing is
  cheaper and safer than half-wiring the computation in.
- **`PRAGMA not_null_repair` is a write; `PRAGMA not_null_check` stays a read
  (#588).** The repair reports rows deleted via `Db.execute` and refuses under
  a read-only transaction, naming `not_null_check` as the read-only escape hatch.
- **`Db.dump`'s NOT NULL refusal now points at the repair PRAGMAs, not just at
  the violation it stopped on (#583).**
- **An aggregate's argument is a full expression, and two rules follow from
  that (#665/#664).** The SUM/AVG numeric check now reaches a CASE-expression
  argument; a subquery inside an argument is evaluated per *input* row (a
  different, wider rule than #558's per-output-row correlation).
- **An explicit `ON CONFLICT` target beats the statement's conflict-resolution
  modifier for the index it names (#639).** Resolved in its own pass before the
  modifier runs; `OR REPLACE` defers to the target too.
- **`excluded` is a scope, not a shape — nested `excluded.col` now resolves
  correctly (#741).** Before, only a bare `SET col = excluded.col` at the RHS
  root worked; anything nested (`excluded.v + 1`) silently read the target row.
- **A discarded INSERT row still burns the rowid `execute_insert` allocated for
  it (#742, accepted).** Deferring the allocation was rejected — it would break
  the NOT NULL check and the unique-conflict probe, which both need the
  allocated rowid already written into the row.
- **`TRUE`/`FALSE` are resolved as a name-resolution fallback, not as keywords
  (#744).** They alias the integers 1/0 and are consulted only after an
  identifier lookup fails, so a column literally named `true` still wins.
- **A skipped INSERT (NOT NULL/UNIQUE `OR IGNORE`) leaves nothing behind in the
  store or catalog, including a BEFORE trigger's nested DML (#631).** A
  statement-level savepoint, taken only when a BEFORE INSERT trigger exists and
  the resolution is `CA_ignore`.
- **`ALTER TABLE ... RENAME` refuses when a view or trigger depends on the
  table, even through an unrelated alias collision (#673/#645).** Views and
  triggers are stored as raw SQL text with no AST to rewrite scoped, so the
  refusal is conservative by necessity; one of the three refused shapes is a
  genuine (documented) over-refusal.
- **A rolled-back statement's row-level change-feed deltas are reverted with
  the store; its dirty-table name marks are not (#666).** Deliberate asymmetry:
  a stale delta is a phantom row, a stale name is just an extra cache miss.
  `#737` covers scheduling a reactive-view resync when a statement fails instead.
- **The dirty-table name set also covers DDL that changes a table's observable
  contents — `DROP TABLE` and every `ALTER TABLE` form (#405).** DDL that
  doesn't change existing rows (`CREATE TABLE`/`INDEX`, `VACUUM`, ...) is
  deliberately not marked.
- **A failing autocheckpoint is recorded and surfaced, never raised to the
  triggering commit's caller (#638).** `Store_event.Checkpoint_failed`,
  `Store.checkpoint_health`, and `PRAGMA checkpoint_status`.
- **A parse error reports its line/column/byte-offset and the offending token,
  but no expected-token set (#487).** The grammar resolves ~290 shift/reduce
  conflicts arbitrarily, so a synthesized expectation would be confidently
  wrong.
- **The net-zero SUM-group retention ceiling is accepted, not fixed, and
  documented (#423).** ~9-16 words per retained group, bounded by distinct
  group keys rather than update count; needs a signed weight and a
  within-group-varying measure to arise at all. Long form in
  `docs/IVM_MEMORY.md`.
- **A reactive-view callback can be detached, and registering one is O(1)
  (#746).** `Db.register_view_callback` returns a handle;
  `Db.unregister_view_callback` removes it. Callback order is now contractual
  (registration order) and mid-flush removal is a per-batch snapshot.
- **The writer lock is measured per acquisition site, and every acquire/release
  must go through the measured door (#718).** `Store.lock_stats`; a bypassing
  site would corrupt the wait attribution, not just go unmeasured.
- **A checkpoint's page migration runs outside the writer lock; only the final
  install phase holds it (#719).** Needs a separate `ckpt_mutex` for
  checkpoint-vs-checkpoint exclusion and a per-pass RO-snapshot gate. Measured
  to exactly one unlocked pass — more passes don't converge, they cost linear
  extra WAL growth.
- **A follower store refuses to run its own checkpoint, and RO snapshots below
  a checkpoint's migrated-through horizon are refused (#739).** The gap #719's
  unlocked migration opened for a follower reading stale-relative-to-migration
  main-file pages.
- **`PRAGMA wal_checkpoint` is refused inside an explicit transaction (#740).**
  The writer lock isn't re-entrant; matches sqlite3's own refusal of the same
  statement shape.
- **A pre-#636 stale WAL replay is detected and reported, never refused
  outright (#637).** `PRAGMA wal_replay_check`, keyed on header-page `txn_id`
  monotonicity; three-valued status because "no evidence" is not "verified
  clean."
- **`Db` exposes a read-only `Schema.t` projection — a live view, not a
  snapshot — instead of the mutable `Catalog.t` (#433).** `Db.plan` was added
  for planning-only access, the one legitimate use the projection can't serve.
- **An FTS `rank` projection's real cost was a linear posting-list probe, not
  the per-match `doc_length` fetch (#689).** Fixed with a hashtable (25x on a
  4 000-match benchmark); the doc-length fetch itself now picks point-fetch vs.
  cursor-scan by a measured selectivity threshold.
- **The AUTOINCREMENT mirror write's cost is measured and accepted, not
  optimised (#316).** The durable cost is two dirtied WAL pages, identical at
  3 and 30 columns — not the blob size the issue suspected. 19-27% over a plain
  rowid insert, confined to autocommit; closed as measured-and-acceptable.
- **A tree's root is a function of the committed state, not of the snapshot,
  so it can be memoized across RO snapshots at one committed generation
  (#416).** Fused `Op_project` into the rowid point lookup too; together a
  warm point lookup drops from 1419.9 to 950.9 words, −33%.
- **One `Db.t` holds one explicit transaction; a colliding `BEGIN` poisons the
  handle and `ROLLBACK` is the sole exit (#555).** `#584` and `#598` name
  residual gaps (a post-recovery contamination window; an ATTACH schema switch
  mid-transaction) that are contained, not closed.
- **`Db.with_transaction` gives a transaction a dynamic extent via an owner
  token; nesting is refused cleanly rather than silently joined (#585).**
- **`Db.create_worker_handle` lets multiple fibers hold explicit transactions
  by sharing one `Store.t`'s rowid counters safely (#589/#633/#632).** DDL
  visibility across handles and `VACUUM` (#634) remain per-handle caveats;
  **`#706` (open)** is a rollback-recompute/concurrent-allocation race with the
  same symptom as #589, reproducible under multi-terminal TPC-C — do not trust
  a multi-terminal run's output without checking its own consistency oracle
  until it's fixed.
- **`-0.0` and `+0.0` encode to the same index key, closing the #743 residual
  (#754).** `Index_key.encode_value` used to order `-0.0` strictly below
  `+0.0`, disagreeing with `compare_values`'s `-0.0 = 0.0`; a genuine on-disk
  format change with no migration path (`header.ml` bumped to format v4, a
  v3-or-older file is refused at open rather than risking a silent seek miss),
  deliberately not the same mechanism as #578's NaN tag byte despite the
  surface similarity. Also closes a real UNIQUE-index constraint hole (a REAL
  column could hold both `0.0` and `-0.0`), and excludes REAL columns from
  the #674 covering-index MIN/MAX fast path (the index key can no longer
  carry the true sign of a decoded zero, so that path now falls back to
  fetching the row instead of answering `+0.0` for a stored `-0.0` minimum).

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

Binary: `~/bin/forgejo` — **not** `~/.local/bin/forgejo`, which does not exist
and which this file claimed until 2026-09-03. Repo slug: `IoTReadyNext/granary`.

**`forgejo issue list` silently caps at 50 results and honours no limit flag**
(`--limit` is not parsed, and `forgejo issue list --help` answers `API error
(404): GetUserByName`). It prints 50 rows and no indication that more exist, so
any count taken from it is a floor, not a total. On 2026-09-03 the real backlog
was 59 open issues and the nine oldest — #405, #378, #362, #316, #266, #160,
#156, #131, #86 — were invisible to every `issue list` invocation, which is how
a whole sweep came to be planned against a number that was wrong.

**To count or enumerate issues, page the API directly** (`type=issues` excludes
PRs, which share the number space):

```sh
set -a; . ~/.config/forgejo-cli/config; set +a
curl -sS -H "Authorization: token $FORGEJO_TOKEN" \
  "$FORGEJO_URL/api/v1/repos/IoTReadyNext/granary/issues?state=open&type=issues&limit=50&page=1"
```

Increment `page` until a request returns an empty array. `issue view <N>` is
unaffected and answers correctly for an issue the list omits — which is the
symptom to watch for: an issue that `view` says is open but `list` never shows.

Common commands:

```sh
forgejo issue list IoTReadyNext/granary   # capped at 50 — see above
forgejo issue view IoTReadyNext/granary <N>
forgejo issue close IoTReadyNext/granary <N>
forgejo pr list IoTReadyNext/granary
forgejo pr view IoTReadyNext/granary <N>
forgejo pr review IoTReadyNext/granary <N> --approve
forgejo pr merge IoTReadyNext/granary <N> --method=squash
```
