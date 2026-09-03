# TPC-C-derived OLTP benchmark (#500)

`test/bench_tpcc.ml` loads a deterministic TPC-C-shaped population into granary
and into reference C SQLite, runs the same zero-think-time saturation driver
against each, checks the clause-3.3 consistency conditions before and after,
and writes CSV to stdout with a human summary on stderr.

The driver itself is `Granary_tpc.Tpcc_driver`; it is unit-tested against fake
workers in `test/test_tpcc_driver.ml`, which runs in the default `dune test`
suite in a few seconds and opens no database. The engine-backed profile run is
`test/test_tpcc_smoke.ml`, gated behind `GRANARY_TPCC_SMOKE`.

## These are not TPC-C results

The workload is **derived from** the TPC-C specification. It is not TPC-C, and
nothing here may be reported as a TPC benchmark result. In particular the
headline figure is **NewOrder transactions per second** and is **never tpmC**.

- **No audit**, no pricing disclosure, no full disclosure report.
- **No keying or think times.** The spec's terminal model has both; this driver
  has neither, by design — it measures maximum sustained throughput and keeps a
  run to seconds. tpmC is defined against the spec's timing model, so a number
  produced without it is not tpmC even if the mix matches.
- **A terminal is an Lwt task, not a process or a session.** granary is
  single-domain.
- **The data generator is ours**, following the spec's cardinalities, NURand
  distributions and surname construction, but not byte-identical to any
  reference loader.
- **Money is `REAL`, not `DECIMAL`** — neither engine has DECIMAL. Consistency
  condition 1 compares REAL accumulators and therefore uses a documented
  half-cent tolerance (see `tpcc_check.mli`).
- **StockLevel is rewritten**, but only in its FROM clause now: the spec's
  comma-join is spelled `INNER JOIN`, since granary's FROM takes one table plus
  explicit joins (#486). It used to be rewritten twice over —
  `COUNT(DISTINCT s_i_id)` was a granary parse error (#491), so the engine
  returned the deduplicated ids and the count was taken client-side. #491 is
  fixed and that half is withdrawn: the distinct count now runs in the engine.
  Every other profile runs the spec's statements untouched. The verdicts are
  recorded in `Tpcc_txn.all` and *measured* by the smoke test, not asserted.
- **Engine order is fixed.** granary loads and runs first, SQLite second, over
  the same directory, so the second engine meets a page cache the first one
  warmed.

Treat the numbers as a measurement tool for our own regressions and for a rough
sense of distance from SQLite — not as a competitive claim.

## The one-worker pool, and why the terminal sweep used to flatline

**This section describes the driver as it was before #703, and still
describes reference SQLite today.** For granary's current behaviour see the
`#703` section further down.

`Tpcc_driver.run` takes a **list of workers** and guarantees that no worker runs
two transactions at once. That is not a stylistic choice:

> A granary `Db.t` holds exactly **one** explicit-transaction slot.
> `Db.begin_txn` answers `"transaction already active"` to a second `BEGIN`, and
> two terminals interleaving `BEGIN`/`COMMIT` on one handle would not merely
> error — the second terminal's `COMMIT` would commit the first's half-finished
> work. Filed as **#555**.

A TPC-C transaction is inherently multi-statement and explicit, so one granary
connection used to carry exactly one transaction at a time, and the benchmark
ran a **one-deep pool**. Reference SQLite is still run one-deep, for a
different reason: the `sqlite3` bindings are blocking calls inside a
single-domain Lwt program, so a second handle could not overlap anything either,
there is no SQLite equivalent of `Db.create_worker_handle` to reach for, and
giving one engine a deeper pool than the other would turn the comparison into a
comparison of pool depths rather than of engines.

The consequence used to be deliberate and visible in the output rather than
hidden by it: **raising `GRANARY_TPCC_TERMINALS` could not raise throughput.**
It raised queueing delay, and the driver reports that delay as its own column.
Each profile's latency is split into

- `service_ms` — time inside the worker, the engine's own cost (which, since
  #703, includes any time granary's own writer-lock wait takes — see below),
  and
- `wait_ms` — time the terminal spent queued for a free worker in
  `Tpcc_driver`'s own pool,

and for SQLite (still one-deep) a flat throughput curve against a rising
`wait_ms` remains the *expected* reading of a terminal sweep. Since #703 that
reading no longer applies to granary — read on.

## Determinism

Each terminal draws from its own `Tpc_rand` stream, seeded from
`GRANARY_TPC_SEED` plus the terminal index, so what a terminal issues does not
depend on how the scheduler interleaved it with its peers.

A saturation run is **not** reproducible in outcome, and that is inherent: the
run is bounded by wall-clock time, so the transaction *count* varies between
runs at a fixed seed. The seed pins which transactions are issued, in order, per
terminal — not how many.

## The consistency oracle is run, and proved capable of failing

`Granary_tpc.Tpcc_check` implements the four clause-3.3 consistency conditions.
The harness runs all four **before** the measurement interval and again
**after** it, on every engine, and a violation exits non-zero: throughput
obtained by corrupting the database is not throughput.

That is not enough on its own — an oracle that cannot fail passes every run. So
`GRANARY_TPCC_ORACLE_SELFTEST=1` makes the harness, after the post-run check,
deliberately corrupt an accumulator (`w_ytd` on warehouse 1), re-run the
conditions, and **exit non-zero if they still pass**. The corruption is then
undone. This is the in-harness form of the "the conditions can fail" cases in
`test/test_tpcc_load.ml`, and it exists because the oracle has already once been
found passing vacuously on empty groups (see `tpcc_check.mli`'s header).

A drained new-order queue is a legitimate steady state — Delivery deletes
`new_order` rows — and conditions 2 and 3 consciously skip it. Expect it after a
driver run; it is not the oracle going quiet.

## Environment knobs

| variable | default | meaning |
|---|---|---|
| `GRANARY_TPCC_WAREHOUSES` | 1 | scale factor; the population loaded |
| `GRANARY_TPCC_TERMINALS` | 4 | concurrent Lwt terminals |
| `GRANARY_TPCC_SECONDS` | 10 | measured interval, wall clock |
| `GRANARY_TPCC_WARMUP_SECONDS` | 1 | untimed interval before measurement |
| `GRANARY_TPCC_RETRIES` | 3 | retries before a transaction counts as failed |
| `GRANARY_TPCC_ENGINES` | `granary,sqlite` | which engines to run |
| `GRANARY_TPCC_ORACLE_SELFTEST` | unset | prove the oracle can fail |
| `GRANARY_TPCC_STMT_PROFILE` | unset | per-statement attribution (#714); read only at `TERMINALS=1` |
| `GRANARY_TPCC_STMT_PROFILE_CSV` | unset | directory for the profile CSV; engine name is appended |
| `GRANARY_TPC_SEED` | 42 | base seed |
| `GRANARY_TPC_HOST` | hostname | CSV `host` column |
| `GRANARY_TPCC_BATCH` | 500 | load batch size |

## Running it

`bench_tpcc` is `(optional)`: it needs the `sqlite3` opam library and is skipped
by builds where that is absent. The dev image has both the bindings and the CLI.

```sh
podman run --rm --user 0 -v "$(pwd):/workspace:z" -w /workspace \
  -e GRANARY_TPCC_SECONDS=10 -e GRANARY_TPCC_TERMINALS=4 \
  -e GRANARY_TPCC_ORACLE_SELFTEST=1 \
  granary-dev dune exec test/bench_tpcc.exe
```

The W=1 load costs roughly 45 s per engine and dominates a short run; that cost
is untimed and reported separately on stderr.

## The crash on the SQLite side, and what it was: #571

`bench_tpcc` used to take **SIGSEGV (exit 139)** during the reference-SQLite
measurement interval, with no exception and no message, after the pre-run
consistency check and before any output — which reads as a hung benchmark
rather than a crash. It is fixed; the mechanism is worth recording, because
the fix is one invisible line per call site and the failure is probabilistic.

**Root cause.** Two of the OCaml `sqlite3` bindings' C stubs — the statement
finalizer and the database closer (`sqlite3_stubs.c:900` and `:552` in
sqlite3-ocaml 5.4.1) — do not register their argument as a local root, and
they read the wrapper struct *after* `caml_release_runtime_system()`.
Finalizing a statement is the last thing the caller does with it, so for the
duration of that window the custom block is unreachable from every OCaml root.
The pending GC work the blocking section runs is then free to collect it and
call its own finaliser, which finalizes the statement and `caml_stat_free`s the
wrapper — and the stub resumes by reading the freed struct and handing what it
finds to C SQLite.

**Evidence.** Under `gdb` the fault is inside `sqlite3_finalize`, called from
`caml_sqlite3_stmt_finalize`, with `pStmt` = `0x605397255167` and `si_addr` the
same: an *odd* word, so not a pointer any allocator returned — it is an OCaml
immediate sitting in memory the wrapper used to occupy. `info threads` shows a
single thread, ruling out a concurrent-use explanation. Three variants over
six runs each at 4 terminals x 40 s: the shipped code crashed **5/6**; adding a
keep-alive after the call crashed **0/6**; and the *unmodified* code against a
locally rebuilt binding that adds the missing `CAMLparam1` crashed **0/6**.

**Two things the original bisection got wrong**, both from reading a
probabilistic crash as a deterministic one: it is not specific to two or more
terminals (one terminal at 60 s crashes), and it is not a transaction-count
threshold. Terminals and duration only buy more attempts at the same window.

**The fix**, in `test/bench_tpcc.ml`, `test/bench_tpch.ml` and
`test/bench_compare.ml`: route every such call through a small wrapper that
mentions the value again afterwards, via a `keep_alive` the optimizer may not
see through, so the caller's frame holds it live across the window.
`Tpc_keepalive_lint` (`test/tpc/`, exercised by
`test/test_sqlite_keepalive_571.ml`) keeps that from being tidied away.

Long reference runs are no longer bounded by this. Running each engine in its
own process is still preferable, but for the page-cache reason noted above, not
for stability.

## Recorded results

CSV under `bench/results/`. Every number below is from the invocation printed
beside it, on a shared dev host under concurrent load from other work, so treat
run-to-run spread of a few tens of percent as noise — the engine gap is not.

### NewOrder/sec, W=1, granary vs reference SQLite — after the #671 batch

`bench/results/2026-08-07-tpcc-w1-post-batch.csv`, **three runs**, 4 terminals,
5 s measured, 1 s warm-up, on `main` at `9f1e61e` (loadavg 0.45 at start):

| engine | NewOrder/sec (3 runs) | mean | new_order service |
|---|---|---|---|
| granary | 26.28 / 26.28 / 24.60 | **25.7** | ~15.7 ms |
| sqlite | 827.1 / 784.6 / 564.1 | 725.3 | 0.79 ms |

Roughly **28-32x**, against 42x before the batch.

**Read the two spreads before the ratio.** granary's three runs span ±3%;
reference SQLite's span **±19%**. The noisy side of this comparison is the
*reference*, because SQLite commits ~4 100 NewOrders in the interval and granary
commits ~134 — SQLite's number moves with whatever else the box is doing, while
granary's is pinned by its own service time. Quote the ratio as a band, not a
figure.

### The previous figure, and how much of the gain is real

`bench/results/2026-08-02-tpcc-w1-vs-sqlite.csv`, same config, before the batch:

| engine | NewOrder/sec | new_order mean | service | wait |
|---|---|---|---|---|
| granary | 19.32 | 84.9 ms | 19.8 ms | 65.1 ms |
| sqlite | 808.59 | 0.79 ms | 0.79 ms | 0.00 ms |

19.32 → 25.7 is **+33%**, and granary's own ±3% spread puts that well outside
its noise. But **the baseline is a single run on a host that was under
concurrent load**, where the post-batch figure is three runs on a quiet one. The
*direction* is solid; the *magnitude* is soft, and a like-for-like re-measure of
the old tree would be needed to make it precise. The honest claim is "new_order
service fell from 19.8 ms to ~15.7 ms", which is the same thing measured where
the batch actually worked.

Reference SQLite's `wait` is zero throughout: the bindings are blocking, so a
transaction never yields and a terminal never queues.

### Per-profile service time, and where the cost now is

Mean granary `service_ms` across the three runs, against reference SQLite:

| profile | granary | sqlite | committed / 5 s |
|---|---|---|---|
| new_order | ~15.7 ms | 0.79 ms | 134 |
| payment | ~4.6 ms | 0.15 ms | 112 |
| order_status | ~2.4 ms | 0.54 ms | 10 |
| **delivery** | **~181 ms** | 1.66 ms | 12 |
| stock_level | ~29 ms | 0.80 ms | 14 |

**Delivery is now the dominant per-transaction cost by two orders of magnitude**
and the batch did not move it — #512 measured 166 ms for it after the
composite-seek fix, and it is ~181 ms here. Tracked as #674.

**Everything below `payment` is weak.** Those three profiles commit 10-14
transactions in the measured interval, so their percentiles are drawn from a
handful of samples and their `service_ms` swings accordingly — stock_level read
19 / 42 / 27 ms across the three runs, which is sample noise, not signal. A
longer interval is needed before anything is claimed about them; the delivery
figure is called out above because ~181 ms against 1.66 ms survives that caveat
by a wide margin, not because 12 samples are enough on their own.

### Delivery batches its ten districts into one transaction (#701)

#674 fixed three algorithmic defects in Delivery's `MIN(no_o_id)` query and
took it from ~181 ms to ~82.7 ms/transaction, but #701's per-statement
profiling found that `BEGIN`+`COMMIT` together still accounted for 77.9% of
what remained (64.4% `COMMIT` — the fsync — 13.5% `BEGIN`), because
`Tpcc_txn.delivery_district`/`delivery_district_body` ran each of Delivery's
ten districts through its **own** `BEGIN`...`COMMIT` pair, so Delivery paid
that fixed per-commit cost ten times per logical transaction where every
other profile pays it once.

TPC-C clause 2.7.1 explicitly allows grouping: "the Delivery transaction must
group any subset of the 10 delivery transactions... into groups of one or
more, at the discretion of the SUT." Since #701, `Tpcc_txn.run_delivery`
wraps all ten districts in a single `BEGIN`...`COMMIT` pair instead of ten.
Measured (`bench_tpcc.exe`, W=1, 4 terminals, 20 s, on a loaded host —
loadavg ~7 on 8 cores, so treat the absolute numbers as noisy and the
direction as the signal): delivery's `service_ms` dropped from ~94 ms
(pre-#701, one-transaction-per-district, re-measured on the same loaded host
for a like-for-like comparison) to ~65-74 ms across repeated runs, with
`new_order`/`payment`/`order_status`/`stock_level` and NewOrder/sec
unaffected, as expected since the change is scoped to Delivery's transaction
boundary only. On a quieter host #701's own instrumented profiling projected
a larger drop (~92.4 ms → ~27.5 ms, a ~3.4x reduction) — the batching removes
9 of Delivery's 10 fixed per-commit costs regardless of host load, but how
much wall-clock time that translates to depends on how expensive a single
fsync is on the box doing the measuring.

**This is a deliberate isolation-granularity tradeoff, not a free win.**
Before #701, a failure partway through Delivery's ten districts rolled back
only the failing district — the districts already committed earlier in the
same call stayed committed. Since #701 the ten districts share one
transaction, so a failure or crash partway through rolls back the *entire*
batch, and a retry (`Tpcc_driver.attempt` redraws a fresh input) reprocesses
all ten districts from scratch. This matches ordinary SQL transaction
semantics — the atomic unit is now the whole logical Delivery transaction,
which is exactly what TPC-C 2.7.1 permits the SUT to define it as — but it
does mean a mid-run crash now loses more undelivered orders' worth of partial
progress than before. See `Tpcc_txn.run`'s `.mli` doc comment and the comment
on `run_delivery` in `tpcc_txn.ml` for the same tradeoff spelled out next to
the code.

### Terminal sweep, W=1, granary

`bench/results/2026-08-02-tpcc-w1-sweep.csv`, 10 s measured, 2 s warm-up:

| terminals | NewOrder/sec | new_order mean | service | wait |
|---|---|---|---|---|
| 1 | 25.70 | 16.2 ms | 16.2 ms | 0.0 ms |
| 2 | 21.97 | 37.5 ms | 20.2 ms | 17.3 ms |
| 4 | 20.02 | 88.2 ms | 22.4 ms | 65.8 ms |
| 8 | 18.30 | 213.2 ms | 28.5 ms | 184.7 ms |
| 16 | 9.88 | 779.9 ms | 49.0 ms | 731.0 ms |

**This is the result, not a tuning failure.** Throughput does not rise with
terminals — it drifts down — while mean latency rises by 48x, and essentially
all of that rise is `wait`. `service` is roughly flat from 1 to 8 terminals,
which is what a one-deep pool must produce: the engine is doing the same work
per transaction, and every additional terminal is queued behind it.

The one-terminal figure (25.7 NewOrder/sec) is therefore the honest ceiling for
this workload today, and it is a *single-connection* ceiling — see #555.

**That sweep is from before the #671 batch and has NOT been re-measured.** Its
shape (flat throughput, all the latency rise in `wait`) is structural and will
not have changed — the pool is still one deep. Its *values* have: the batch's
post-batch **4-terminal** figure is 25.7 NewOrder/sec, which is exactly what this
table records as the pre-batch **1-terminal** ceiling. So the whole curve has
moved up by roughly the queueing penalty this table was measuring, and the
sentence above understates today's ceiling. Re-run the sweep before quoting any
row of it.

**This whole section is now superseded for granary by the `#703` section
below** — the pool referred to throughout is no longer one deep for granary.
It remains accurate for reference SQLite, which is unaffected by #703.

## #703: one worker handle per terminal, and the correctness bug it found

`Db.create_worker_handle` (#589/#632/#633, see the top-level `CLAUDE.md`) gives
a fiber its own `explicit_txn` slot over the *same* `Store.t` and writer
`Rwlock` as the handle it was minted from — two worker handles' write
transactions block on that shared lock instead of colliding/poisoning. #632 and
#633 removed the prerequisite blocker (#589: two worker handles used to hold
independent rowid counters over one tree, silently losing rows), so #703 gave
`Tpcc_driver` one worker-handle connection per terminal — minted from
`Tpcc_conn.worker_handle` after the load phase completes, since a handle's
catalog cannot see DDL run on another handle — instead of the one-deep pool
described above. `Tpcc_driver.run` itself needed **no change**: it already
accepted an arbitrary list of workers and enforced "no worker runs two
transactions at once" per worker, not per process: what changed is how many
distinct `Db.t`-backed connections `test/bench_tpcc.ml` hands it, and that they
now share one on-disk database rather than being the same handle wrapped
`terminals` times.

### A correctness bug surfaced immediately, before any throughput conclusion is safe to draw

Every existing worker-handle test drives straight-line INSERT/UPDATE traffic,
never a `ROLLBACK` racing a concurrent sibling handle's allocation on the same
table. TPC-C's NewOrder profile is the first workload in the tree to do
exactly that under load: it intentionally `ROLLBACK`s ~1% of transactions
(spec 2.4.2.3's invalid-item case) *after* already bumping
`orders`/`new_order`/`order_line`'s rowid counters earlier in the same
transaction. At `GRANARY_TPCC_TERMINALS >= 2`, `bench_tpcc.exe`'s own
before/after consistency oracle reliably — not occasionally — reports rows
missing from `new_order`/`order_line`:

```
[granary] after: condition 3 (max(no_o_id) - min(no_o_id) + 1 equals the new_order row count per district): VIOLATED — 1 offending district(s)/row(s), first = district (1,8): max=3047 min=2142 count=905 (max-min+1=906)
[granary] after: condition 4 (the sum of o_ol_cnt equals the order_line row count per district): VIOLATED — 6 offending district(s)/row(s), first = district (1,10): sum(o_ol_cnt)=30553 <> order_line count=30541
```

The root cause (filed as **#706**, with full analysis): `Db.force_rollback_txn`
calls `S.rollback` — which releases the writer lock as its last synchronous
step — and only *then* calls `Cat.recompute_rowid_counters_after_rollback` to
re-derive the rolled-back table's counter, deliberately outside the lock (its
RO scan would deadlock inside `rw_begin` otherwise). That recompute's publish
is an unconditional `Hashtbl.replace` on the shared `Store.rowid_counters`
table, with no re-acquisition of the lock. A sibling worker handle that begins,
allocates from the same tree, and commits inside the window between the
rollback's unlock and this recompute's publish has its legitimate allocation
silently clobbered back down to a stale, lower value — and the next `INSERT`
reissues an already-used rowid, exactly reproducing #589's original symptom
(a reused engine rowid, the second `S.put` silently overwriting the first row).
This is the same class of bug CLAUDE.md's `#632` bullet documents as already
fixed for the *allocate* path ("every allocator allocates *and* publishes with
the writer lock held") — the rollback-recompute path was simply never brought
under that same discipline, because nothing before #703 exercised it under
real concurrency.

**Consequence for this section: every terminals >= 2 number below comes from a
run whose database was not left consistent.** The run still completes and
prints throughput/latency figures, and the shape is real and worth recording —
but until #706 is fixed, do not read a multi-terminal `bench_tpcc` run as a
validated result, and re-run the sweep clean once it is.

**#706 is now fixed** (`5f86fb6`, PR #708 — hold the writer lock around the
rollback recompute's publish via a compare-and-swap, `Schema_cache.
cas_rowid_durable`). The table immediately below is kept as-is because it is
the record of what the bug looked like; the clean re-run is its own section
further down ("Terminal sweep, W=1, granary, worker-handle driver, post-#706").

### Terminal sweep, W=1, granary, worker-handle driver (#703)

`bench/results/2026-08-10-tpcc-w1-worker-handle-sweep.csv`, 10 s measured, 2 s
warm-up, one run per terminal count (not averaged), on a loaded container host
(loadavg ~3-5 on 8 cores — treat absolute numbers as noisy, the shape as the
signal), `GRANARY_TPCC_ENGINES=granary` only:

| terminals | NewOrder/sec | new_order mean | service | wait | oracle |
|---|---|---|---|---|---|
| 1 | 31.60 | 17.5 ms | 17.5 ms | 0.0 ms | clean |
| 2 | 32.47 | 28.7 ms | 28.7 ms | 0.0 ms | **violated** (#706) |
| 4 | 33.95 | 54.7 ms | 54.7 ms | 0.0 ms | **violated** (#706) |
| 8 | 28.24 | 140.0 ms | 140.0 ms | 0.0 ms | **violated** (#706) |
| 16 | 27.45 | 299.5 ms | 299.5 ms | 0.0 ms | **violated** (#706) |

For reference, the pre-#703 one-deep-pool shape (from the `2026-08-02` table
above, a different host and a different post-#671 baseline, so compare shapes,
not absolute values): NewOrder/sec fell monotonically from 25.70 (1 terminal)
to 9.88 (16 terminals) — a 62% collapse — with essentially all of the latency
rise landing in `wait_ms` and `service_ms` staying roughly flat.

**What moved, and why:**

- **`wait_ms` is now 0.000 at every terminal count.** `Tpcc_driver`'s pool has
  exactly as many workers as there are terminals, so no terminal ever queues
  for a worker — the queueing-for-a-connection artifact the old table measured
  is gone by construction.
- **All of the latency that used to show up as `wait_ms` now shows up as
  `service_ms` instead — it has not disappeared, only been relabeled.** A
  worker's call to `Db.begin_txn` blocks inside the worker (on the shared
  `Rwlock`) until the previous writer commits, and `Tpcc_driver.attempt_once`
  times the whole worker call as `service_ms`. So `service_ms` here is *not*
  purely "the engine's own per-transaction cost" the way it was for a one-deep
  pool — it now also carries genuine writer-lock contention. This is expected
  and is the same underlying serialization #555 always implied for concurrent
  writers; #703 only changed which column reports the wait.
- **Throughput is no longer a monotonic collapse — it is roughly flat (27-34
  NewOrder/sec) from 1 to 16 terminals**, dipping at 8 and 16 rather than
  cratering. 16-terminal throughput retains ~87% of the 1-terminal figure here,
  versus ~38% in the old one-deep-pool table. That is the real, structural
  improvement #703 set out to measure: removing the pool as an *artificial*
  bottleneck exposes the *actual* ceiling, which is the single-writer `Rwlock`
  every explicit write transaction still serializes on (per #555 — worker
  handles remove the pool-depth bottleneck, not the one-writer-at-a-time
  invariant, and #703's own issue said as much going in). It is not an N×
  throughput win, and it was never going to be one: TPC-C's write mix means
  most terminals' time is spent inside a write transaction, and only one
  writer runs at a time regardless of how many `Db.t`s exist.
- **This is a wash-to-modest-win on throughput, entangled with a real
  correctness regression that must be fixed before the win can be trusted.**
  Reporting both together, as asked: the pool-queueing collapse is gone and
  the curve is flatter and higher, which is the honest structural result: but
  every number at terminals >= 2 was produced by a run that lost rows, so the
  win is not yet one to build on without #706.

### Recommendation while #706 was open

Run `bench_tpcc.exe` with `GRANARY_TPCC_ORACLE_SELFTEST=1` and read its exit
code, not just its throughput line, whenever `GRANARY_TPCC_TERMINALS > 1`. A
regression benchmark that silently corrupts the database it is measuring is
worse than no benchmark. (Superseded by the clean re-run below — kept as the
standing advice for any *future* multi-terminal regression, not just this one.)

### Terminal sweep, W=1, granary, worker-handle driver, post-#706 (2026-08-11)

`bench/results/2026-08-11-tpcc-w1-post706-sweep.csv`, same methodology as the
table above (10 s measured, 2 s warm-up, one run per terminal count, fresh W=1
load each run, `GRANARY_TPCC_ENGINES=granary` only), run against `main` at
`1a3f627` plus #706's fix (`5f86fb6`/PR #708):

| terminals | NewOrder/sec | new_order mean | service | wait | oracle (before/after) |
|---|---|---|---|---|---|
| 1 | 30.21 | 17.6 ms | 17.6 ms | 0.0 ms | ok / ok |
| 2 | 29.79 | 30.8 ms | 30.8 ms | 0.0 ms | ok / ok |
| 4 | 33.30 | 56.3 ms | 56.3 ms | 0.0 ms | ok / ok |
| 8 | 28.19 | 144.6 ms | 144.6 ms | 0.0 ms | ok / ok |
| 16 | 27.70 | 301.2 ms | 301.2 ms | 0.0 ms | ok / ok |

All five runs exited 0 with every consistency condition (1-4) reporting `ok`,
including at 8 and 16 terminals where the pre-fix sweep tripped conditions
2/3/4 on every run. **This is now a validated result.**

The shape is unchanged from the pre-fix sweep, as expected — #706's fix
touches only the rollback-recompute publish path, not the driver, the write
path, or the writer-lock contention the numbers are actually measuring:
`wait_ms` is 0 at every terminal count (the worker-per-terminal pool has no
queueing-for-a-connection to show), `service_ms` carries the real
writer-`Rwlock` contention, and throughput is roughly flat (28-33 NewOrder/sec)
from 1 to 16 terminals rather than collapsing the way the old one-deep-pool
sweep did. The analysis in the bullet list above (`wait_ms` → `service_ms`
relabeling, the #555 single-writer ceiling, "wash-to-modest-win on
throughput") stands unchanged; only its correctness caveat is retired.

### Per-statement service-time attribution of a NewOrder (#714)

`bench/results/2026-08-11-tpcc-stmt-profile-granary.csv`, produced by the
statement profiler added in #714 (`GRANARY_TPCC_STMT_PROFILE=1`), run against
this branch at W=1, 10 s measured, 2 s warm-up, `GRANARY_TPCC_ENGINES=granary`:

```sh
podman run --rm --user 0 -v "$(pwd):/workspace:z" -w /workspace \
  -e GRANARY_TPCC_WAREHOUSES=1 -e GRANARY_TPCC_TERMINALS=1 \
  -e GRANARY_TPCC_SECONDS=10 -e GRANARY_TPCC_WARMUP_SECONDS=2 \
  -e GRANARY_TPCC_ENGINES=granary \
  -e GRANARY_TPCC_STMT_PROFILE=1 -e GRANARY_TPCC_STMT_PROFILE_CSV=bench/results \
  granary-dev dune exec test/bench_tpcc.exe
```

This command writes `bench/results/tpcc-stmt-profile-granary.csv` (the undated
name `test/bench_tpcc.ml:268` writes); the committed artifact was renamed with
the run's date afterward, so a reproducer should not read the undated filename
as evidence the committed file is stale.

The run reported **32.957 NewOrder/sec**, mean `new_order` service 16.964 ms —
i.e. the same operating point as the post-#706 sweep's 1-terminal row above.

#### What this measures, and what it does not

The profiler brackets each statement's promise from call to resolution. What it
therefore attributes is **service time** — elapsed wall clock per statement,
summing to the driver's own per-transaction `service_ms`. That is the quantity
the whole section is about, and every share below is a share of it.

**The in-lock / out-of-lock split is not measured, and the profiler cannot see
it.** Two places in the engine make that split real, and both cut across the
largest rows in the table:

- `Db.begin_txn` takes the store's single writer lock at `BEGIN`, so a `BEGIN`
  that finds the lock held is *waiting*, not working.
- `COMMIT` **releases the writer lock before it finishes**. `commit_wal` calls
  `unlock_once ()` (`lib/store/store.ml:2166`) and only then enters
  `group_commit_sync` → `Pager.wal_sync` (`:2167-2177`); the non-WAL arm
  releases with `Rwlock.release_write` inside the `Lwt.finalize` at
  `:2274-2283`. Default `sync_mode` is `` `Full ``
  (`store.ml:734`) and `Tpcc_conn.open_db` passes no `durability`, so every
  commit fsyncs *after* the lock is released. An unknown but likely majority of
  the `COMMIT` row — the largest row in the table — is therefore held **outside**
  the critical section, and `group_commit_sync` exists precisely so that
  concurrent committers coalesce that fsync. It is the part that does *not*
  serialise.

So do not read this table as an attribution of lock-hold time. It is an
attribution of the transaction's elapsed time. Establishing the split needs
instrumentation inside `Store` that does not exist.

#### `TERMINALS=1` is still the methodology, in its weaker form

Read these figures only at one terminal. What one terminal buys is that there is
no queueing behind a **sibling terminal**: the sweep above shows `wait_ms` of 0
at every terminal count while `service_ms` grows, so at higher counts the
per-statement numbers would silently absorb inter-terminal contention and the
profiler could not tell that apart from work.

It does **not** buy "no lock wait at all", and the `BEGIN` row below is the
evidence that it does not: one terminal is not one writer.
`maybe_autockpt_after_commit` dispatches the autocheckpoint through `Lwt.async`
(`lib/store/store.ml:2078`) and that fiber takes `Rwlock.acquire_write`
(`:2083`) with nobody awaiting it, so with the default 1000-frame threshold
(`store.ml:294`, `test/tpc/tpcc_conn.ml:203`) a background checkpoint **can be**
a genuine second writer.

#### Coverage first — how much of the transaction this table accounts for

Summed per-statement time against the driver's own `service_ms` for the same
interval, verbatim from the run:

```
coverage — summed statement time vs driver service time
  delivery        2093.8 ms of    2096.5 ms    99.9% attributed
  new_order       5581.1 ms of    5598.1 ms    99.7% attributed
  order_status     116.7 ms of     116.8 ms    99.9% attributed
  payment         1664.5 ms of    1667.9 ms    99.8% attributed
  stock_level      528.4 ms of     528.5 ms   100.0% attributed
```

**`new_order` is 99.7% attributed**, and every other profile is 99.8% or
better. There is no meaningful unattributed remainder: driver bookkeeping,
random-input generation and scheduling between statements together account for
0.3% of a NewOrder. The table below is therefore the real attribution of a
NewOrder's service time, not a partial view of it.

#### `new_order`, ranked by total time in the measured interval

330 NewOrder transactions — 326 `COMMIT` and 4 `ROLLBACK` statements, the
spec-mandated ~1% invalid-item abort — 5581.1 ms of attributed statement time.
(The driver's own `committed` count for `new_order` is 330: `rolled_back` is a
subset of `committed`, not a separate outcome — see `Tpcc_driver.record_success`,
which increments `a_committed` for every completed attempt and then
`a_rolled_back` when that attempt was an intentional abort. The 4 rollbacks are
4 of those 330, not 4 on top of 326.)

| sql | calls | rows | total_ms | mean_ms | % |
|---|---|---|---|---|---|
| `COMMIT` | 326 | 0 | 1699.3 | 5.213 | 30.45 |
| `ROLLBACK` | 4 | 0 | 1435.7 | 358.916 | 25.72 |
| `BEGIN` | 330 | 0 | 1065.2 | 3.228 | 19.09 |
| `UPDATE stock SET s_quantity = ?, s_ytd = …` | 3264 | 0 | 404.9 | 0.124 | 7.26 |
| `INSERT INTO order_line (…) VALUES (?,…)` | 3264 | 0 | 246.0 | 0.075 | 4.41 |
| `SELECT i_price, i_name, i_data FROM item WHERE i_id = ?` | 3268 | 3264 | 187.0 | 0.057 | 3.35 |
| `INSERT INTO orders (…) VALUES (?,…)` | 330 | 0 | 78.0 | 0.236 | 1.40 |
| `SELECT c_discount, c_last, c_credit FROM customer WHERE …` | 330 | 330 | 62.4 | 0.189 | 1.12 |
| `INSERT INTO new_order (no_o_id, no_d_id, no_w_id) VALUES (?,?,?)` | 330 | 0 | 44.5 | 0.135 | 0.80 |
| `SELECT s_quantity, s_dist_09, s_data FROM stock WHERE …` | 439 | 439 | 44.1 | 0.101 | 0.79 |
| `SELECT s_quantity, s_dist_05, s_data FROM stock WHERE …` | 394 | 394 | 43.2 | 0.110 | 0.77 |
| `SELECT s_quantity, s_dist_01, s_data FROM stock WHERE …` | 422 | 422 | 42.4 | 0.101 | 0.76 |
| `SELECT s_quantity, s_dist_08, s_data FROM stock WHERE …` | 345 | 345 | 34.2 | 0.099 | 0.61 |
| `SELECT s_quantity, s_dist_10, s_data FROM stock WHERE …` | 350 | 350 | 33.3 | 0.095 | 0.60 |
| `SELECT s_quantity, s_dist_02, s_data FROM stock WHERE …` | 307 | 307 | 28.3 | 0.092 | 0.51 |
| `SELECT s_quantity, s_dist_06, s_data FROM stock WHERE …` | 260 | 260 | 27.5 | 0.106 | 0.49 |
| `SELECT s_quantity, s_dist_04, s_data FROM stock WHERE …` | 248 | 248 | 26.0 | 0.105 | 0.47 |
| `SELECT s_quantity, s_dist_07, s_data FROM stock WHERE …` | 260 | 260 | 25.8 | 0.099 | 0.46 |
| `SELECT s_quantity, s_dist_03, s_data FROM stock WHERE …` | 239 | 239 | 21.9 | 0.092 | 0.39 |
| `UPDATE district SET d_next_o_id = d_next_o_id + 1 WHERE …` | 330 | 0 | 11.6 | 0.035 | 0.21 |
| `SELECT d_tax, d_next_o_id FROM district WHERE …` | 330 | 330 | 10.8 | 0.033 | 0.19 |
| `SELECT w_tax FROM warehouse WHERE w_id = ?` | 330 | 330 | 8.9 | 0.027 | 0.16 |

The ten `s_dist_NN` rows are the same TPC-C stock read, spelled ten times
because the district number selects the column name; together they are 326.7 ms
(5.9%) over 3264 calls at 0.100 ms/call — which would rank it **fourth**, not
tenth-through-nineteenth. That rollup was done by hand for this run and is now
done by the tool: `Tpcc_stmt_profile.families` groups keys within a profile that
differ only in an embedded number and `report` prints them under
*generated-SQL families*, so a later run read without this paragraph cannot
silently understate a generated shape by its fan-out factor. The rollup is
reported only — `ranked` and the CSV keep the raw per-key rows, because
collapsing digits is a heuristic (two statements differing only in a numeric
*literal* are distinct shapes to the planner) and an interpretation should not
overwrite the measurement.

#### What the numbers say

- **Transaction control is 75.3% of a NewOrder's service time.** `BEGIN` +
  `COMMIT` + `ROLLBACK` sum to 4200.2 ms of 5581.1 ms. Every SQL statement a
  NewOrder issues accounts for the remaining 1380.9 ms, **24.7%** — and there
  are more of them than the profile's shape suggests: 15040 non-control calls
  over 330 transactions is **45.6 statements per NewOrder**, six header
  statements plus 39.6 in the item loop, i.e. **four** statements per item
  (`item` lookup, `stock` read, `UPDATE stock`, `INSERT order_line`).
- **Excluding the four rollbacks it is 49.53%** — `BEGIN` + `COMMIT` = 2764.5 ms
  of 5581.1 ms. Stated per transaction it is a slightly different statistic:
  3.228 + 5.213 = 8.44 ms against a 16.96 ms mean NewOrder, or 49.8%, because
  `BEGIN` has 330 calls and `COMMIT` only 326. Either way, roughly half of a
  NewOrder is spent in two statements that do no SQL.
- **`BEGIN` at 3.228 ms/call is waiting, not working, and its variance across
  profiles is the proof.** After the lock is acquired, `rw_begin` does only
  in-memory bookkeeping — `set_txn_id`, an event emit, `set_alloc_min_safe`,
  `txn_owned_pool_set`, a freelist snapshot (`lib/store/store.ml:1505-1541`) —
  with no I/O and no await; and an uncontended `Rwlock.acquire_write` returns an
  already-resolved promise (`lib/store/rwlock.ml:53-62`), so it costs nothing
  when the lock is free.

  There is a second fixed-cost candidate that the `rw_begin` argument does not
  reach, and it needs its own measurement rather than an argument, because it
  applies to `BEGIN`/`COMMIT`/`ROLLBACK` and to *nothing else in the harness*:
  they are the only statements `Tpcc_conn` does not prepare. `is_control_stmt`
  routes them to the one-shot `Db.execute` path (`test/tpc/tpcc_conn.ml:48-56`,
  because `Sql.Exec.execute_with_count` refuses `Op_begin`/`Op_commit`/
  `Op_rollback` outright), so each pays a full parse+plan on every call while
  every other row in the table is a cached prepared statement. Measured
  directly — 5000 `Db.execute "BEGIN"`/`Db.execute "COMMIT"` pairs on an idle
  in-memory handle, after a 200-pair warm-up, three runs — the pair costs
  **0.0009-0.0016 ms**, i.e. under **0.001 ms per control statement**. That is
  ~0.02% of `BEGIN`'s 3.228 ms and ~0.01% of `COMMIT`'s 5.213 ms. The
  unprepared path is real but it is three orders of magnitude too small to
  matter, so it is ruled out as well, and the per-profile minima (`BEGIN`
  0.861 ms in `stock_level`, `COMMIT` 1.231 ms in `order_status`) are *not* a
  front-end floor — whatever sets them, it is not SQL parsing.

  The CSV then rules out a fixed cost of any origin directly: `BEGIN`'s mean
  varies **3.7x** across profiles for identical work — 0.861 ms in
  `stock_level`, 2.006 in `order_status`, 2.273 in `delivery`, 2.420 in
  `payment`, 3.228 in `new_order` — and is largest after the heaviest writer.
  Something is holding the lock, or the scheduler is draining, at the awaits the
  profiler brackets. The **candidate** mechanism is the background
  autocheckpoint described above (a real second writer at one terminal); this
  run does not measure which, and no cause is attributed here.
- **`ROLLBACK` costs 358.9 ms per call, ~70x a `COMMIT`.** Four calls — 1.2% of
  transactions — take 25.7% of all NewOrder time, and they are visibly the
  `new_order` p99 of 345.5 ms and max of 381.5 ms in the run's own summary
  table. The per-call cost is two orders of magnitude off the write work being
  discarded, which is a handful of rows. A **candidate** mechanism, not
  measured by this run: `Cat.recompute_rowid_counters_after_rollback`
  (`lib/catalog/catalog.ml:2425`) runs a read-only tree scan
  (`recover_next_rowid`'s `cursor_open`/`cursor_next` walk) per bumped table,
  and a NewOrder bumps three — `orders`, `new_order`, `order_line`. No cause
  is attributed here.
- **No single SQL statement is a hotspot.** The largest, the ten-item
  `UPDATE stock`, is 7.26% and runs at 0.124 ms/call; the largest read, the
  `item` lookup, is 3.35% at 0.057 ms/call. The per-call figures are uniform
  across reads and writes and across tables (0.027-0.236 ms), which is what a
  workload with no bad plan and no missing index looks like.

#### What it rules out

- **It rules out the per-statement execution path as the throughput bound.**
  The measured interval is 10.013 s and the five profiles' service time sums to
  10.008 s of it, so the terminal is never idle and wall clock is service time.
  One terminal runs all five profiles serially, so "making the entire
  statement-execution path infinitely fast" means removing every profile's
  non-control statement time, not just NewOrder's — 1380.9 ms (new_order) +
  635.1 ms (delivery) + 455.5 ms (stock_level) + 208.5 ms (payment) + 19.6 ms
  (order_status) = 2699.5 ms of the 10.008 s. That leaves 7.309 s for the same
  330 NewOrders — **45.2 NewOrder/sec, a 37% gain**, and that is the *ceiling*
  of what the entire statement-execution path is worth. Removing only
  NewOrder's own 1380.9 ms, with the rest of the mix unchanged, is a narrower
  question — "what if just NewOrder's statements were free" — and gives 8.627 s
  for the same 330 NewOrders, **38.3 NewOrder/sec, a 16% gain**. By contrast
  `BEGIN` + `COMMIT` + `ROLLBACK` across all five profiles is 7285.0 ms,
  **72.8% of the whole measured interval**. This is the strongest claim in the
  section and neither caveat above touches it: it is arithmetic over service
  time, which is exactly what the profiler measures, and it holds whatever the
  in-lock split turns out to be. The time is in `BEGIN` and `COMMIT`, not in
  what runs between them.

  What does *not* follow is that all of it is *serialisation*. A likely
  majority of the `COMMIT` row is the post-unlock fsync, which
  `group_commit_sync` coalesces across concurrent committers — so the share of
  that 72.8% which actually excludes other writers is unmeasured, and smaller.
- **It rules out the ten-item loop as the thing to batch.** Its 3264-call
  statements are individually the cheapest per call in the profile; the loop is
  large in *count*, not in *time*.
- **It rules out plan or index quality as an explanation *for NewOrder*, and
  explicitly not for the run.** Within `new_order` no statement's mean deviates
  from its neighbours in a way a bad plan would produce, and the `stock`
  reads — the ones that touch the largest table — are among the cheapest. That
  scope is deliberate: `stock_level`'s
  `SELECT COUNT(DISTINCT s_i_id) FROM order_line INNER JOIN stock …` runs at
  **13.354 ms/call**, 50-130x every other statement in the CSV, and is 454.0 ms
  — 85.9% of `stock_level` and ~17% of the 2699.5 ms of non-control statement
  time the ceiling above is computed from. That is exactly the signature a plan
  or index problem produces, and it is the one statement in the run this table
  does **not** rule out. Anything designed against the ceiling should treat it
  as a separate, already-identified target rather than as covered by the
  NewOrder finding.
- **It does not rule out anything about multi-terminal behaviour.** These
  figures are service time at one terminal; how that time interacts with a
  queue of writers is the sweep above, not this table.
- **It rules nothing in or out about *where the lock is held*.** That split is
  unmeasured (see the caveat at the top), and both of the two largest rows —
  `COMMIT`'s post-unlock fsync and `BEGIN`'s apparent waiting — sit on the wrong
  side of it for any conclusion about lock-hold time to be drawn here. (#718
  built the instrumentation that closes this gap and re-ran the profile against
  it — see the next section. `BEGIN`'s waiting is now attributed; `COMMIT`'s
  fsync still is not, and structurally cannot be.)

The other four profiles show the same shape and are in the CSV: `payment` is
87.5% `BEGIN`+`COMMIT`, `delivery` 69.7%, `order_status` 83.2%. `stock_level` is
the one exception — 85.9% of it is its single `COUNT(DISTINCT s_i_id)` join, at
13.354 ms/call — and it is a read-only profile issued at 4% of the mix.

Full data, including all five profiles and the coverage rows:
`bench/results/2026-08-11-tpcc-stmt-profile-granary.csv`.

### Where the writer lock actually goes (#718)

`bench/results/2026-09-02-tpcc-stmt-profile-granary.csv` re-runs the section
above with the writer-lock accounting #718 added — same command, same knobs,
same W=1/1-terminal/10 s operating point. The per-statement tables are
comparable row for row; the throughput headline is **not**, because the two runs
are on different hosts (see the `.meta`). The last three CSV tables have no
counterpart in the 2026-08-11 file: nothing in the tree could produce them
before #718.

The section above ends by saying it "rules nothing in or out about *where the
lock is held*". This is that measurement.

| site | acquires | contended | wait_ms | hold_ms | share of the 10.041 s interval |
|---|---|---|---|---|---|
| `txn` | 670 | 60 | 2515.531 | 7504.567 | 74.7% |
| `autocheckpoint` | 60 | 0 | 0.019 | 2519.228 | **25.1%** |
| `checkpoint` | 0 | 0 | 0.000 | 0.000 | — |
| `commit_sink` | 0 | 0 | 0.000 | 0.000 | — |

and the contention matrix has exactly one non-empty cell:

```
waiter  blocked_by_holder  waits   wait_ms
txn     autocheckpoint        60   2515.353
```

#### What this settles

- **#716 item 2 is answered, and its named candidate is the cause.** That issue
  said `BEGIN`'s 3.228 ms "cannot be transaction-setup cost", offered the
  `Lwt.async` autocheckpoint as a "candidate, not a cause", and explicitly did
  not exclude scheduler drain. **Every one of `txn`'s 60 contended acquisitions
  was behind the autocheckpoint**, carrying 2515.353 ms of the 2515.531 ms of
  all writer-lock wait in the run — 99.99%. `BEGIN` is waiting, and it is
  waiting for the background checkpoint.

  **The remaining 0.178 ms is the instrument's own measurement floor, not
  drain, and this table structurally cannot see drain.** An uncontended
  `Rwlock.acquire_write` returns `Lwt.return_unit` (`lib/store/rwlock.ml:59-61`)
  and `Lwt.bind` on an already-resolved promise runs its callback
  synchronously — so an uncontended acquisition contains no scheduler yield at
  all, by construction. What the 0.178 ms measures is the two `Unix.gettimeofday`
  reads bracketing each acquire: spread over `txn`'s 610 *uncontended*
  acquisitions that is 0.29 µs each, and the table carries its own control —
  `autocheckpoint` is 60 acquisitions with `contended = 0` and
  `wait_ms = 0.019`, i.e. 0.32 µs each, the same floor to within noise.

  Separately, the drain hypothesis #716 raised was drain at the awaits *the
  profiler* brackets, which sits outside `acquire_write` entirely. This
  measurement narrows it hard — `BEGIN`'s excess is accounted for as lock wait
  behind the autocheckpoint, leaving little room for anything else — but it does
  not bound it at 0.178 ms, and nothing here excludes it.
- **The wait is concentrated, not spread.** 60 of 670 transactions — 9.0% —
  wait at all; each of those waits a mean of 41.9 ms and up to 75.5 ms. The
  other 91% wait for nothing. That is the whole of the `new_order` p50/p99 gap
  (12.4 ms against 67.7 ms) and it is a maintenance task, not query work.
- **The writer lock is 99.8% occupied at ONE terminal.** 7504.567 + 2519.228 =
  10023.8 ms of a 10041 ms interval. The two cannot overlap — one writer at a
  time — so this is a floor on occupancy, not an estimate, and it is the answer
  to #718's "how much of #716's headline converts to throughput at
  `TERMINALS > 1`": **none of it, until something leaves the critical section**.
  There is no idle lock for a second writer to take. The single-writer ceiling
  the sweep above reports is not a scheduling artifact; it is saturation.
- **A quarter of that saturation is not the workload.** The autocheckpoint's
  25.1% is WAL maintenance holding the one lock every transaction needs, at a
  1000-frame threshold nothing in the harness overrides. Filed as **#719**, and
  **fixed**: the page migration and its fsync now run with the writer lock free,
  and the lock is taken once at the end to catch up on whatever was committed
  meanwhile and truncate the WAL. The threshold was deliberately left alone —
  raising it makes the holds fewer and longer, which is the same 25% differently
  spelled. **This table has not been re-measured since**, so the residual
  `autocheckpoint` hold is a prediction (a catch-up of a few pages, one fsync,
  and `Wal.reset`) rather than a number; re-running the profile is what turns it
  into one.
- **#717 converted, and the per-call figure is the safe way to see it.**
  `ROLLBACK` went from 358.9164 ms/call (4 calls, 25.72% of `new_order`, the
  run's p99 and max) to **0.485 ms/call** (3 calls, 0.03%) — 740x on a
  per-operation latency whose asymptotics changed from a full tree drain to a
  root-to-leaf descent over a ~300k-row `order_line`. No host difference reaches
  two orders of magnitude. `ROLLBACK` is no longer among `new_order`'s costs.

#### What it does not settle

- **It says nothing about `COMMIT`.** `commit_wal` releases the lock before the
  fsync, so the fsync is outside every number in the table above — by design,
  and it is the part `group_commit_sync` coalesces across writers, i.e. the part
  that does *not* serialise. `COMMIT` remains 40.2% of `new_order`'s service
  time and the accounting deliberately cannot attribute it.
- **It is one terminal.** At `TERMINALS > 1` the matrix would grow a
  `txn`-behind-`txn` cell, which is the quantity a multi-terminal design
  question needs and which this run has none of. Read `#706`'s caveat before
  trusting a multi-terminal run's output at all.
- **It is one run on a loaded laptop.** The shares are large enough that load
  cannot invent them, but no figure here is a repeated measurement.
