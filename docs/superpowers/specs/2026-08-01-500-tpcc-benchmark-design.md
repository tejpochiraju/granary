# TPC-C-derived OLTP benchmark

Issue: #500 (the last outstanding item of epic #482)
Date: 2026-08-01

## Goal

Measure granary under a write-heavy, concurrent, contended workload, complementing
the TPC-H-derived OLAP harness shipped in #501/#505. The output is two things:

- A throughput and latency number — NewOrder transactions/sec, with p50/p95/p99.
- A correctness oracle for the transactional path, in the way the SQLite answer
  cross-check was the oracle for the analytic path. TPC-H's real payoff was 14
  engine issues, four of them silent-wrong-answer bugs; a TPC-C driver with no
  oracle can post a fine number while losing writes.

## Non-goals

- **Not an audited TPC-C result, and never reported as tpmC.** The driver has zero
  keying and think times, the terminals are Lwt tasks rather than emulated users,
  the SQL is rewritten for granary's dialect, and there is no audit or pricing
  disclosure. All documentation, CSV output, and summaries say "derived", and the
  headline figure is *NewOrder txns/sec*.
- Not a CI timing gate. CI asserts correctness and non-crash at a smoke scale only,
  matching `bench_compare` and `bench_tpch`.
- No cross-engine *concurrency* comparison. See "Concurrency model" — SQLite is
  driven serially, and its CSV rows say so.

## Constraints discovered in the engine

Established by reading `lib/db/`, `lib/store/`, and the concurrency tests before
designing. These drive the driver model and are the reason this spec departs from
the #482 design spec's sketch.

1. **`Db.t` is a connection, not a pool.** It carries a single mutable
   `explicit_txn` slot (`lib/db/db.ml:41`). Every DML path dispatches on it —
   `run_dml` at `db.ml:1767`, prepared-statement exec at `db.ml:2185`. Two fibers
   running explicit transactions on one handle is *silently wrong*, not an error:
   fiber B's `INSERT` lands inside fiber A's open transaction, and B's `COMMIT`
   commits A's in-flight work and clears the shared slot. A second `BEGIN` on the
   same handle does error, with `Runtime "transaction already active"`
   (`db.ml:583`), but that only catches the interleaving that starts with `BEGIN`.
   `test/test_multifiber_stress.ml:194` documents the shared-slot behaviour as
   intended for a single connection.

2. **`Db.create_worker_handle` is the correct primitive** (`db.mli:118`): same
   `Store.t`, therefore same write lock, but a fresh `Db.t` with its own
   `explicit_txn`. It is documented for "Jepsen-style concurrent workloads" and
   has **zero call sites** in `lib/`, `test/`, or `bin/`. This harness is its first
   user. Two consequences: each handle builds its own catalog cache
   (`catalog.ml:1716`), so DDL after handles are created is not visible to them;
   and `Db.close` on any handle closes the *shared* store (`db.ml:371`), so
   terminals must never close theirs.

3. **There are no write conflicts to retry.** Writers serialize on a single-writer
   `Rwlock` acquired in `Store.rw_begin` (`store.ml:1394`) and released only at
   commit or rollback. It blocks indefinitely — no timeout, no lock-wait deadline.
   `Db.error` is `Parse | Sema | Runtime of string | History_unavailable |
   History_pruned` (`db.mli:18-27`): there is no `Busy`, `Locked`, `Conflict`, or
   `Deadlock` variant anywhere in `db.mli` or `store.mli`. Isolation is snapshot
   plus a single writer, so write-write conflict detection does not exist by
   design. Contention manifests as *waiting*, never as a retryable error.

4. **The `Rwlock` is not reentrant.** Opening a nested store-level RW transaction
   while an explicit transaction is held self-deadlocks — a permanent hang, not an
   error (`lib/sql/exec.ml:2458`, `test/test_txn.ml:447`). Transaction bodies must
   therefore be plain DML, and must never await anything external while holding the
   lock.

5. Reads are free: `acquire_read` is a counter bump, not an exclusion
   (`store.ml:1253`). Read-only profiles do not queue behind writers.

6. Composite primary keys work (`test/test_sqlite_compare.ml:3702`), so TPC-C's
   composite keys are not a blocker.

## Architecture

Five new modules in the existing `granary_tpc` library at `test/tpc/`, plus one
executable in `test/`. The shape deliberately mirrors the TPC-H harness.

| Module | Responsibility |
|---|---|
| `Tpcc_schema` | The 9 `CREATE TABLE` statements, indexes, and a `Load` functor over `Bench_report.ENGINE` |
| `Tpcc_gen` | Deterministic seeded population of the 9 tables |
| `Tpcc_txn` | The 5 transaction profiles as parameterised statement sequences, each carrying a verdict |
| `Tpcc_check` | The consistency conditions and the run's pass/fail classification |
| `Tpcc_driver` | Worker handles, weighted mix, warm-up and measurement intervals, latency and lock-wait metrics |
| `bench_tpcc.ml` | `(optional)` executable: granary concurrent, reference SQLite serial |

`Tpcc_driver` is granary-only and Lwt-native — it drives `Db.t` directly rather
than through `Bench_report.ENGINE`, which is synchronous (`Granary_engine` wraps
every call in `Lwt_main.run`) and therefore cannot host concurrent terminals. The
serial SQLite reference path in `bench_tpcc.ml` does go through `ENGINE`.

`Tpc_rand` gains two spec primitives; `Bench_report` is reused unchanged.

**Why `Tpcc_check` is a library module and not part of the executable.** `bench_tpcc`
is `(optional)` and does not build without `sqlite3`, so nothing inside it is
testable. #505 moved the TPC-H classification into `Tpch_check` for exactly this
reason. Every rule that decides the process exit code lives in the library and is
unit-tested there.

## Data and generation

Population per the spec, scaled by W warehouses:

| Table | Rows |
|---|---|
| `warehouse` | W |
| `district` | 10 per warehouse |
| `customer` | 3,000 per district |
| `history` | 1 per customer |
| `item` | 100,000 — fixed, **not** scaled by W |
| `stock` | 100,000 per warehouse |
| `orders` | 3,000 per district |
| `new_order` | the last 900 orders per district |
| `order_line` | 5–15 per order, uniform |

At W=1 that is roughly 500k rows, comparable to TPC-H at SF 0.05, and it loads in
seconds. Loading reuses the batched-insert, one-transaction-per-table shape of
`Tpch_schema.Load`.

Two primitives are added to `Tpc_rand`, alongside the existing ones and tested the
same way:

- `nurand ~a ~x ~y` — the spec's non-uniform random distribution,
  `(((rand[0,a] | rand[x,y]) + c) mod (y - x + 1)) + x`. This is what creates
  TPC-C's access skew, and skew is the entire reason the benchmark contends.
- `last_name n` — the syllable-triple generator over `BAR OUGHT ABLE PRI PRES ESE
  ANTI CALLY ATION EING`, producing `BARBARBAR` through `EINGEINGEING`. Used by
  population and by the by-name Payment and OrderStatus variants.

**Representation.** Money is REAL and dates are TEXT `'YYYY-MM-DD HH:MM:SS'`,
consistent with the rationale already documented for TPC-H: storing granary's side
as scaled integers would give it a representation SQLite is not using and make the
comparison misleading. Lexicographic ordering keeps date range predicates correct.

## Transaction profiles

The five profiles, at the spec's weights: NewOrder 45%, Payment 43%, OrderStatus
4%, Delivery 4%, StockLevel 4%. Selection is from a seeded PRNG, so a run is
reproducible.

The spec's 1% NewOrder rollback — an invalid item id in the last order line — is
retained. It exercises the rollback path, which is a reason to include it rather
than an inconvenience.

Each profile carries a verdict, mirroring `Tpch_queries`:

```ocaml
type verdict =
  | Native              (* runs as the spec writes it *)
  | Rewritten of string (* runs after a documented transformation *)
  | Skipped of string   (* cannot be expressed; Forgejo issue reference *)
```

A `Skipped` profile is dropped from the mix, its weight redistributed
proportionally across the remainder, its status recorded in the CSV, and a Forgejo
issue filed naming the missing capability. The run still produces a number, and
the summary states plainly which profiles ran. This is the policy that made TPC-H
valuable: 12 skipped queries and 14 filed issues were the output, not a failure.

**Verdicts are an output of this work, not an input.** They are determined by
running each profile against the engine, never predicted from reading the AST.

## Deviations from the spec, and why

Every departure from TPC-C as written lives here, so the history is in one place
rather than scattered across module comments.

**StockLevel is `Rewritten` (#486).** The spec's comma-join is spelled
`INNER JOIN`, since granary's `FROM` clause takes one table plus explicit joins.
StockLevel is the only profile with a join at all, which is why it is the only one
carrying a rewrite. No other profile deviates: the remaining four are `Native`.

> **Superseded, 2026-08-06.** As written, this deviation had a second half:
> granary's parser rejected `DISTINCT` as an aggregate argument, so the spec's
> `COUNT(DISTINCT s_i_id)` was a parse error and there were no derived tables to
> wrap it in. The join, the 20-order window, the `s_quantity` threshold and the
> duplicate elimination ran in the engine as `SELECT DISTINCT s_i_id`, and only
> the final `COUNT` of the deduplicated ids was taken client-side as the row
> count. **#491 is fixed and that half is withdrawn** — the distinct count now
> runs in the engine. Only the `INNER JOIN` spelling remains, hence the citation
> change above.

**The NURand run constants are per-use, and the `c_last` run constant is derived
from `C_LOAD` rather than equal to it.** An earlier version of `Tpcc_txn.gen_input`
threaded one shared `constant_c` into all three NURand draws (`c_id` at `a:1023`,
`ol_i_id` at `a:8191`, `c_last` at `a:255`), and its `.mli` justified this by
claiming the run constant "must be the same NURand C the dataset was generated
with". That was doubly wrong. Clause 2.1.6.1 picks the three constants
*independently*, and for `c_last` it requires the **run** constant to differ from
`C_LOAD` by a delta in `[65,119]`, excluding 96 and 112 — the entire point being
that the run's hot surnames must **not** coincide with the load's, or the benchmark
measures a cache-friendlier workload than TPC-C intends. The justification was also
false on its own terms: `Tpcc_gen` assigns `last_name (c_id - 1)` to the first
1,000 customers of every district, so all 1,000 surnames exist in every district
and any run constant resolves to real rows. `Tpcc_txn.default_run_constants` now
carries three fields; the `c_last` delta is **85**, chosen as mid-range in
`[65,119]` and far from both excluded values, so a future change to
`Tpcc_gen.c_load` cannot drift it onto 96 or 112. The sign is chosen to keep the
result inside `[0,255]`, giving 88 for the current `C_LOAD` of 173.

**Money is REAL, and condition 1 compares it with a half-cent tolerance.** The
spec's money type is DECIMAL; this harness stores money as REAL, for the reason
already documented above — scaled integers would give granary a representation
SQLite is not using and make the comparison misleading. That makes a tolerance
*necessary* in condition 1 rather than lax: `w_ytd` is a single accumulator while
`SUM(d_ytd)` re-sums ten separately accumulated values, so the two take different
rounding paths over a run and an exact `=` would report a violation after a handful
of Payments even though the invariant holds exactly in decimal. `money_epsilon` is
`0.005` — below the smallest movement the workload can make (Payment's minimum
amount is 1.00) and six orders of magnitude above a double's ulp at these
magnitudes, so it can hide nothing real. It is *absolute* while float error is
proportional to magnitude; that is a deliberate choice for the range this harness
reaches (`w_ytd` starts at 300,000 and stays far below the ~1e13 where half a cent
stops dominating the ulp), and the scale-proof fix, should it ever be needed, is to
make it relative rather than to widen it. The boundary is pinned by a unit test.

**All four consistency conditions are joined client-side.** See the correctness
oracle section below; this is not a stylistic choice, and it must survive #507
being fixed.

## Concurrency model

granary is single-domain Lwt, so a terminal is an Lwt task.

- **Each terminal gets its own `Db.t` via `Db.create_worker_handle`.** Sharing one
  handle across terminals is silently incorrect per constraint 1. All DDL and the
  entire population run on the parent handle *before* any terminal is created,
  because worker handles hold independent catalog caches. Terminals never call
  `close`; the parent closes the store once at the end.
- **No retry loop.** Constraint 3 means there is nothing to retry: contention is
  blocking, not erroring. The design spec for #482 sketched "retry-on-conflict with
  retries counted separately"; implementing that here would ship a `retries` column
  that is structurally always zero, which is worse than not having it.
- **`lock_wait_ms` replaces `retries`.** Each terminal measures the interval between
  issuing `BEGIN` and that call returning — the time spent blocked in
  `rw_begin` — and the driver reports the per-transaction-type total. This is the
  real contention signal on this engine, and it is what makes the terminal count a
  meaningful knob. Errors get their own `errors` column.
- **Expect throughput flat in terminal count**, with p99 latency growing roughly
  linearly, because writers serialize on one lock. That is a correct measurement of
  granary's architecture, not a harness defect, and the stderr summary says so
  rather than leaving a reader to infer it.

**Reference SQLite runs the identical mix serially, on one connection**, as a
throughput reference point. Its CSV rows report `terminals=1`. Driving SQLite
concurrently would need OCaml threads and multiple connections in an `(optional)`
executable — fragile, and not what this harness is for. The comparison being
made is "granary under contention versus SQLite's uncontended serial rate", and the
output labels it as such.

## Correctness oracle

`Tpcc_check` implements four of the spec's clause-3.3 consistency conditions over
the post-run database. They are the cheap ones, and they catch the failures that
matter — a lost write, a mis-sequenced counter, a partially applied transaction:

1. `w_ytd` equals the sum of its districts' `d_ytd`.
2. `d_next_o_id - 1` equals both `max(o_id)` and `max(no_o_id)` for that district.
3. `max(no_o_id) - min(no_o_id) + 1` equals the `new_order` row count for that
   district.
4. The sum of `o_ol_cnt` equals the `order_line` row count for that district.

These run on **both** engines after the measurement interval. A violation is
reported loudly on stderr and sets a non-zero exit code, exactly as a TPC-H
`MISMATCH` does, because a violation means either a profile is wrong or granary
lost a write — and both are worth failing on.

Conditions 1 and 4 are also checked immediately after population, before any
terminal runs. A generator that produces an inconsistent initial state would
otherwise be indistinguishable from a driver that corrupts it.

**None of the four is a single SQL query, and that is load-bearing.** Each asks the
engine only for plain rows and bare `GROUP BY` aggregates and joins them in OCaml
over the *union* of both sides' keys. The obvious alternative — one query per
condition returning zero rows on success, with a correlated scalar subquery in the
`WHERE` clause — is a vacuous oracle twice over. First, #485: an unqualified
outer-column reference inside a correlated subquery silently evaluates to NULL
instead of erroring, turning condition 1's predicate into `w_ytd <> NULL`, never
true, zero rows, a guaranteed pass over any database however corrupt. Second, even
fully qualified, `x <> (SELECT SUM/MAX ...)` is NULL — and so not returned, and so
a pass — whenever the subquery matches **zero rows**: a warehouse with no `district`
rows passed condition 1 for any `w_ytd`, and a district with no `orders` and no
`new_order` rows passed condition 2 for any `d_next_o_id`. A `GROUP BY` over
`new_order` alone fails the same way from the other end, producing no group for a
district that has vanished from `new_order`, so condition 3 stopped examining it —
and PR 2's Delivery profile *deletes* `new_order` rows, making a drained district
reachable in the real mix. Driving each condition from the table that must have the
row and reporting a key present on one side but missing from the other closes both.
#507 (granary's aggregation planner rejects any `GROUP BY` projection item that is
not a bare grouped column, a bare aggregate call, or a window function) is a
*further*, unrelated reason conditions 3 and 4 could not be single queries; fixing
it does **not** license a revert.

Condition 4 is driven from `district` too, even though its two aggregates are
symmetric. Joined against each other alone, a district that lost *both* its
`orders` and its `order_line` rows contributes no key to either side and passes
vacuously. Condition 2's `orders` half catches that district today, so the oracle
as a whole is not blind — but a condition must not depend on a different condition
to notice its own subject disappearing.

Two empty groups are not the same thing, and the difference is decided per
condition. On a *driving* side (warehouse for 1, district for 2, 3 and 4) an empty
result is a violation: those tables are never empty in a real run, so an empty one
means the check stopped seeing real data. On the `new_order` *aggregate* side a
district with no rows is a legitimate steady state — Delivery deletes `new_order`
rows, so a fully delivered district has none — and is consciously skipped, with the
reasoning recorded at the site. An empty `orders` group is the opposite: orders are
never deleted, so it is a violation.

## Data flow

Populate W warehouses on the parent handle → run the consistency conditions on the
initial state → create T worker handles → warm-up interval, untimed → run T
terminals for D seconds → drain in-flight transactions → run the consistency
conditions → emit per-transaction-type throughput and latency percentiles.

## Configuration

Env knobs, following the existing convention:

- `GRANARY_TPCC_WAREHOUSES` — default 1
- `GRANARY_TPCC_TERMINALS` — default 4
- `GRANARY_TPCC_SECONDS` — measurement interval, default 10
- `GRANARY_TPCC_WARMUP_SECONDS` — untimed warm-up, default 2
- `GRANARY_TPC_SEED` — PRNG seed for population and mix selection
- `GRANARY_TPC_HOST` — host label for the CSV

Scale tiers: smoke is 1 warehouse / 2 terminals in CI; dev is 4 warehouses / 4
terminals; full is 16 warehouses / 16 terminals nightly on otp-prod-1.

## Output

CSV to stdout, human summary to stderr, matching `bench_compare` and `bench_tpch`.

```
host,engine,warehouses,terminals,seconds,txn_type,verdict,count,tps,p50_ms,p95_ms,p99_ms,lock_wait_ms,errors,consistency
```

`consistency` carries the run-level verdict from `Tpcc_check`, so the CSV alone
says whether the number is trustworthy. Results are committed under
`bench/results/` as they are produced, and the harness is documented in
`docs/benchmarks/BENCHMARKS-TPCC.md` alongside the existing TPC-H document.

## Error handling

- Missing `sqlite3`: the executable builds and runs granary-only; the reference
  column is reported as skipped rather than failing.
- A transaction that raises: counted in `errors` for its type, terminal continues.
  A profile whose error rate exceeds a threshold is a run failure, not a footnote —
  an engine that fails 90% of NewOrders would otherwise post an excellent latency.
- A consistency violation on either engine: loud on stderr, `consistency` column set
  accordingly, non-zero exit.
- A terminal that hangs past the measurement interval: the drain phase has a bounded
  wait, after which the run reports the hang and exits non-zero. Constraint 4 makes
  a self-deadlock a permanent hang rather than an error, so an unbounded drain would
  turn an engine bug into a stuck CI job.
- Population at a warehouse count that would exceed available disk: checked up front
  against an estimate, refused with a clear message, reusing `bench_tpch.ml`'s
  `df`-based guard.

## Testing

- `Tpcc_gen`: exact row counts per warehouse count, byte-identical output across two
  runs at one seed, different output at different seeds, per-column range sanity.
  Mirrors the existing `Tpch_gen` tests.
- QCheck properties over the two new `Tpc_rand` primitives: `nurand` stays within
  `[x, y]` for arbitrary seeds and parameters; `last_name` stays within the syllable
  alphabet and produces exactly the 1,000 distinct names the spec defines.
- `Tpcc_check`: each of the four consistency conditions must *fail* when its
  invariant is deliberately broken in a synthetic fixture, and pass on a consistent
  one. Plus the run-level classification and its exit-code mapping.
- A test asserting every one of the 5 profiles has a verdict and that none is
  silently missing from the table.
- Negative tests proving each consistency condition can FAIL, on two axes: a
  *perturbed value* in a row that stays present, and a *deleted group* — the whole
  set of rows behind one aggregate. The second axis is the one that catches the
  zero-rows hole above, and it is untestable by perturbation alone. The deletes run
  inside a transaction that is always rolled back, so the shared loaded database is
  restored exactly.
- Smoke tier as an integration test: 1 warehouse, 2 terminals, a few hundred
  transactions, consistency conditions asserted, no latency assertion.

## CI

The smoke tier joins the existing bench workflow. `test/bench_tpcc.ml` must be
allowlisted in the `#370` no-real-SQLite policy job in **both** `.forgejo/` and
`.github/` workflows: it trips both the `"sqlite3 "` string guard and the
`Sqlite3.` module guard. Omitting this is what broke `main`'s lint from #501 until
#505 caught it.

## Order of work

Two PRs.

**PR 1 — data and transactions.** `Tpc_rand` primitives, `Tpcc_schema`, `Tpcc_gen`,
`Tpcc_txn`, `Tpcc_check`, and their tests. Fully reviewable and fully tested without
any concurrency, and it surfaces engine expressibility gaps early, when there is
still time to file issues and route around them.

**PR 2 — driver and reporting.** `Tpcc_driver`, `bench_tpcc.ml`, the CI smoke tier,
the `#370` allowlist, `bench/results/`, and `docs/benchmarks/BENCHMARKS-TPCC.md`.

This mirrors how TPC-H split into #501 and #505, and keeps each diff at a
reviewable size.

## Follow-ups this work should file

Independent of the harness, the constraints above are engine findings worth
recording:

- `Db.create_worker_handle` is public, documented for concurrent workloads, and has
  no call sites and no tests. This harness becomes its first user; it deserves
  direct tests of its own.
- Concurrent explicit transactions on a single `Db.t` silently mix transaction
  boundaries rather than erroring. The single-connection semantics may well be
  correct, but the failure is silent, and a caller has no way to detect it.
