# TPC-C per-statement attribution profiler (#714)

> **Superseded in part.** The branch's own measurement retracted three claims
> this spec makes: the `NewOrder/sec = 1 / (time a NewOrder holds the writer
> lock)` identity; that every recorded millisecond is in-lock *work* because at
> one terminal there is exactly one writer; and the "~35 statements (5 header +
> 10 items × 3)" estimate below, actually 45.6 (6 header + 4 per item). The
> first two are wrong because `commit_wal` releases the writer lock at
> `lib/store/store.ml:2166` *before* it fsyncs, and `maybe_autockpt_after_commit`
> (`:2078`/`:2083`) can take the writer lock as an unawaited second writer even
> at one terminal. See `docs/benchmarks/BENCHMARKS-TPCC.md`'s "What this
> measures, and what it does not" subsection for the current position. The body
> below is kept as the design record and is not edited.

Design spec. Scoped out of #555's "break the write ceiling" thread.

## The problem this exists to answer

granary holds one writer at a time. `Rwlock` serialises writers against each
other and nothing else — readers never block and never block writers, because
snapshot isolation makes reader/writer exclusion unnecessary. So for a
write-only workload:

```
NewOrder/sec = 1 / (time a NewOrder holds the writer lock)
```

The post-#706 sweep (`docs/benchmarks/BENCHMARKS-TPCC.md`) measures that
directly. At 1 terminal, with no contention at all, a NewOrder takes **17.6 ms**
and throughput is **30.2 NewOrder/sec**; at 16 terminals service time rises to
301 ms and throughput is **27.7/sec**. That is perfect serialisation: extra
terminals queue behind the lock and total throughput is constant. The ceiling is
set entirely by the single-terminal number.

`BEGIN` takes the writer lock at statement one (`Db.begin_txn`, `lib/db/db.ml:844`,
calls `S.rw_begin` unconditionally) and `COMMIT` releases it, so the whole
17.6 ms is inside the critical section. NewOrder issues roughly 35 statements —
5 header statements plus 10 items x 3 — which puts the mean near 0.5 ms per
statement.

**Nothing in the tree attributes those 17.6 ms.** This spec adds that
attribution and nothing else.

## What is deliberately NOT here

No engine change. The fix's design depends entirely on what the table says, so
speccing a fix now would be guessing. Three other levers were considered and
rejected for this piece of work:

- **Read-only transactions skipping the writer lock.** Real, contained, and
  literally what #555 option 1 still asks for — `Db.begin_txn` takes the writer
  lock even for a transaction that only reads, which is why OrderStatus and
  StockLevel queue behind NewOrder when the storage layer would have let them
  through. But it cannot move the NewOrder/sec headline, because NewOrder is a
  writer. Separable follow-up.
- **Concurrent writers (real MVCC).** The only thing that makes the terminal
  sweep scale rather than flatten, and by far the largest — B-tree, pager, WAL
  and rollback paths. An epic, not a PR.
- **`perf record`.** Truthful about cycles, but attributes to functions rather
  than to statements, which is harder to turn into a fix. Worth reaching for
  *after* this table fingers a statement, not instead of it.

## Methodology, and the one commitment that makes the numbers mean anything

**The profile run is `GRANARY_TPCC_TERMINALS=1`.**

This is not a convenience. At one terminal there is exactly one writer, so
`rw_begin` never blocks and every millisecond recorded is in-lock *work* rather
than lock *wait*. A multi-terminal profile would blend the two and re-measure
the contention the sweep already reports, while making each statement look
proportionally slower for a reason that has nothing to do with the statement.

The profiler measures elapsed wall-clock around each statement's Lwt promise,
not CPU time. At one terminal with no competing fiber those coincide closely
enough; where they do not, the integrity check below is what surfaces it.

## Components

### `test/tpc/tpcc_stmt_profile.ml{,i}` — the accumulator

The only module with state. Deliberately separate from `Tpcc_conn` so its
arithmetic is unit-testable: `bench_tpcc` is `(optional)` and does not build
without `sqlite3`, so anything living there is untestable — the same reason
`Tpch_check` was extracted in #505.

```
val enabled : bool
val record : profile:string -> sql:string -> rows:int -> secs:float -> unit
val report : service_ms:(string * float) list -> string
val to_csv : path:string -> unit
val reset : unit -> unit
```

- `enabled` is read **once**, at module initialisation, from
  `GRANARY_TPCC_STMT_PROFILE`. Reading it once rather than per call keeps the
  disabled path to a single bool test, and stops a mid-run environment change
  from producing a half-profiled run.
- Keyed by `(profile, sql)`. Not by `sql` alone: `BEGIN`, `COMMIT` and
  `ROLLBACK` recur across all five profiles, and lumping them would hide which
  profile's commit is expensive — the single most likely thing this table needs
  to distinguish.
- `sql` is the **raw, unrendered** shape string, which is already the stable
  identifier `Tpcc_conn.stmt_cache` keys on (#697). Rendering parameters into
  the key would explode it into one row per distinct parameter value.
- Accumulates `calls`, `rows`, `total_secs`. `mean` and `%` are derived at
  report time, never stored.
- A global table needs no locking: the harness is single-domain and Lwt is
  cooperative, and `record` performs no await, so no other fiber can interleave
  inside it.
- `reset` exists for the tests and for discarding the warm-up window; see
  below.

### `test/tpc/tpcc_txn.ml` — profile identity

`record` needs to know which profile a statement belongs to. The five profile
functions are already distinct entry points, so this is a `profile : string`
field added to the `ops` record itself — each profile is handed an `ops` stamped
with its own name, rather than passing the name through every call site. It is
**not** a new dispatch mechanism and **not** an inspection of the SQL text.

### `test/tpc/tpcc_conn.ml` — the instrumentation point

`ops` (`:163`) already wraps both `query` and `exec` and is the single place
every statement in the harness passes through. When `enabled`, wrap the returned
promise with a `Unix.gettimeofday` bracket and call `record`; `rows` is the
length of the list `prepared_query_rows_lwt` already builds, and `0` for `exec`.

The timing must bracket the **promise's resolution**, not the call that creates
it, or every statement will read as free.

Control statements (`BEGIN`/`COMMIT`/`ROLLBACK`, recognised by
`is_control_stmt`) are timed too, and this matters: `COMMIT` is where the group
commit and its fsync wait land, and it is a plausible candidate for a large
share of the transaction. Excluding them would have hidden that.

When disabled the cost is one bool test per statement against a call already
costing ~0.5 ms.

### `test/bench_tpcc.ml` — reporting

After the measured window, if enabled: print the ranked table to stderr and
write `bench/results/YYYY-MM-DD-tpcc-stmt-profile.csv`.

The warm-up window is excluded by calling `reset` at the start of the measured
window, so cold-cache first-touches and the one-off `Db.prepare` per shape do
not contaminate the steady-state means they would otherwise dominate.

## Output

CSV columns, ranked by `total_ms` descending within each profile:

```
profile, sql, calls, rows, total_ms, mean_ms, pct_of_profile
```

Plus **one summary line per profile**:

```
profile, statements_total_ms, driver_service_ms, attributed_pct
```

That second line is the integrity check and is the reason to trust the first.
The driver already records `service_ms` per profile. If summed statement time
accounts for only, say, 60% of it, then 40% of the transaction is driver or
scheduling overhead and **the fix does not live in the engine at all** — which
is exactly the kind of thing that otherwise gets chased for a day against a
table that looked authoritative. Reporting the gap makes the table honest about
its own coverage rather than merely suggestive.

## Testing

`test/test_tpcc_stmt_profile.ml`, as plain unit tests over the accumulator:

- aggregation across repeated calls to the same `(profile, sql)` key
- `(profile, sql)` keys with the same `sql` under different profiles stay
  distinct
- ranking order within a profile, and stability of that order for equal totals
- the `%` denominators — `pct_of_profile` sums to 100 within rounding for each
  profile
- the `attributed_pct` summary line against a supplied `service_ms`, including
  the over-100% case (statement time exceeding service time), which must render
  rather than be treated as impossible
- empty-table rendering — a profile with no recorded statements
- `reset` clears

Plus one driver-level test asserting the profiler is **inert when the env var is
unset**: no CSV written, `report` empty, so a normal benchmark run cannot be
perturbed by instrumentation that ships enabled by accident.

## Deliverable

1. The profiler, env-gated and off by default.
2. A profile run at W=1, `GRANARY_TPCC_TERMINALS=1`, with the CSV committed
   under `bench/results/`.
3. The ranked attribution written into `docs/benchmarks/BENCHMARKS-TPCC.md`,
   including the attributed-percentage figure.

The fix is a separate issue, designed against that table.

Refs #555, #500, #482, #697, #703, #706, #416
