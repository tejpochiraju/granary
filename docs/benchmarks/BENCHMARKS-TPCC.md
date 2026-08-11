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
