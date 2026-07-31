# TPC-derived benchmarks: TPC-H (OLAP) and TPC-C (OLTP)

Issue: #482
Date: 2026-07-31

## Goal

Measure granary against industry-recognizable workloads, not just the
hand-rolled micro-benchmarks in `test/bench_compare.ml`. Two harnesses:

- **TPC-H-derived** — an analytic workload: 8 tables, 22 queries dominated by
  multi-way joins, grouped aggregation, and large scans.
- **TPC-C-derived** — a transactional workload: 9 tables, 5 transaction
  profiles, concurrent terminals, write-heavy with contention.

Both compare granary against reference C SQLite on the same generated dataset,
reusing the cross-engine pattern already established in `bench_compare.ml`.

## Non-goals

- **These are not audited TPC results.** Queries are rewritten to fit granary's
  SQL dialect, the TPC-C driver deviates from the spec's keying/think-time
  model, and there is no audit or pricing disclosure. All documentation, CSV
  output, and summaries say "derived". The TPC-C figure is reported as
  *NewOrder txns/sec*, never as tpmC.
- Not a CI timing gate. Like `bench_compare`, these are measurement tools.
  CI asserts correctness and non-crash at a smoke scale only.
- TPC-E is out of scope; it is filed as a deferred follow-up.

## Constraints discovered in the engine

Read from `lib/sql/ast.ml` and `lib/sql/sema.ml` before designing:

- `S_select` carries `table : string` — the FROM clause names a single table.
  **There are no derived tables (subqueries in FROM).**
- `S_with_cte` binds exactly one named CTE per statement.
- `group_by_item = string * string option` — **GROUP BY accepts plain
  (optionally table-qualified) column names, not expressions.**
- `join_kind` is `Inner | Left`, over named tables with optional aliases. Self
  joins work; RIGHT/FULL do not exist (TPC-H does not need them).
- Available and useful: scalar subqueries (`E_subquery`), `EXISTS`,
  `IN (SELECT ...)`, `CASE`, `CAST`, `HAVING`, `DISTINCT`, compound set ops,
  views, window functions, and a broad scalar/date function library including
  `strftime`.
- `Sema.bind_internal` substitutes a view named in FROM position by rewriting it
  into a CTE, which makes `CREATE VIEW` a viable stand-in for a derived table.

These constraints drive the rewrite strategy below.

## Architecture

A new non-public dune library at `test/tpc/`, plus two executables alongside the
existing benches in `test/`. Library placement means merlint's rules apply:
every module gets an `.mli` with doc comments on each public `val`.

| Module | Responsibility |
|---|---|
| `Tpch_gen` | Deterministic seeded generator for the 8 TPC-H tables, implementing the spec's §4.2 value distributions and text pool |
| `Tpch_schema` | DDL and index definitions |
| `Tpch_queries` | Q1–Q22 in granary dialect, each tagged with its portability verdict |
| `Tpcc_gen` | Population of the 9 TPC-C tables |
| `Tpcc_txn` | The 5 transaction profiles as prepared-statement sequences |
| `Tpcc_driver` | N concurrent Lwt terminals, weighted mix, retry-on-conflict, metrics |
| `Bench_report` | Shared engine signature and CSV emitter into `bench/results/` |
| `bench_tpch.ml` | TPC-H executable, `(optional)` (needs `sqlite3`) |
| `bench_tpcc.ml` | TPC-C executable, `(optional)` |

### Engine abstraction

`Bench_report` hoists the `ENGINE` module signature currently local to
`bench_compare.ml` into a shared, reusable form:

```ocaml
module type ENGINE = sig
  type t
  val name : string
  val open_db : dir:string -> t
  val exec : t -> string -> unit
  val query_rows : t -> string -> string list list
  val prepare : t -> string -> stmt
  val run : stmt -> params:value list -> unit
  val close : t -> unit
end
```

Both a granary implementation and a `sqlite3` implementation satisfy it. The
`sqlite3` opam library stays optional at the dune level — when absent, the
executables still build against granary alone and the cross-check is skipped
with a logged notice rather than a failure.

`bench_compare.ml` is left alone in this work. Factoring it onto the shared
signature is a follow-up, not a prerequisite; changing it here would mix an
unrelated refactor into the diff.

## Data representation

**Money.** TPC-H specifies `DECIMAL(15,2)`. Neither granary nor SQLite has a
DECIMAL type. Both store money as **REAL**, and aggregate comparison in the
cross-check uses a relative epsilon (1e-9 relative, with an absolute floor for
values near zero). Storing granary's side as scaled integers would be more
exact but would give granary a representation SQLite is not using, making the
comparison misleading.

**Dates.** TEXT in `'YYYY-MM-DD'` form. Lexicographic ordering makes the range
predicates (`o_orderdate >= '1994-01-01'`) correct without a date type, and
`strftime` supplies year extraction for the rewrites.

**Identifiers.** INTEGER, using the rowid alias where the natural key is a
dense integer (`l_orderkey`, `o_orderkey`, `c_custkey`, ...).

## TPC-H query rewrites

Each query in `Tpch_queries` is tagged:

```ocaml
type verdict =
  | Native                (* runs as written in the spec *)
  | Rewritten of string   (* runs after a documented transformation *)
  | Skipped of string     (* cannot be expressed; Forgejo issue reference *)
```

Known rewrite levers, given the constraints above:

- A derived table in FROM becomes a `CREATE VIEW` created during setup; sema
  rewrites a view in FROM position into a CTE.
- `GROUP BY <expr>` becomes a GROUP BY over a column of a view that computes the
  expression in its projection.
- `EXTRACT(year FROM d)` becomes `CAST(strftime('%Y', d) AS INTEGER)`.

**The per-query verdict is an output of this work, not an input.** Verdicts are
determined by running each query against the engine, not predicted from reading
the AST. Any query that cannot be faithfully expressed is tagged `Skipped`, gets
a Forgejo issue naming the missing capability, and is reported as skipped in the
CSV and the summary line — the run does not abort. Setup views count as part of
the rewrite and are created before the timed section, never inside it.

A rewrite must preserve the query's answer. Any rewrite that changes the result
set is a bug, and the SQLite cross-check is what catches it.

## Data flow

**TPC-H.** Generate → load (batched inserts, one transaction per table) → create
indexes → optionally create rewrite views → per-query timed runs, best-of-N →
one CSV row per (engine, query, scale factor, run). The cross-check executes the
same SQL over the same dataset through sqlite3 and compares result rows
positionally, with the epsilon rule for REAL columns.

**TPC-C.** Populate W warehouses → warm-up interval (untimed) → run T terminals
for D seconds → emit per-transaction-type throughput and p50/p95/p99 latency.

## TPC-C driver model

granary is single-domain Lwt, so a "terminal" is an Lwt task, not a process.

- Zero keying and think times: terminals issue transactions back to back. This
  measures maximum sustained throughput and keeps runs to seconds or minutes.
- Transaction mix by the spec's weights: NewOrder 45%, Payment 43%,
  OrderStatus 4%, Delivery 4%, StockLevel 4%. Selection is from a seeded PRNG so
  a run is reproducible.
- The spec's 1% NewOrder rollback (invalid item) is retained — it exercises the
  rollback path, which is the point of including it.
- Conflicts are retried with a bounded retry count; retries are counted and
  reported separately, not silently folded into throughput.

## Scale tiers

All env-selected, with small defaults so a bare invocation is quick.

| Tier | TPC-H SF | TPC-C warehouses | Where |
|---|---|---|---|
| smoke | 0.001 | 1 | CI |
| dev | 0.01, 0.1 | 4 | developer machine |
| full | 1.0 | 16 | nightly on otp-prod-1 |

Env knobs, following the existing `GRANARY_BENCH_*` convention:

- `GRANARY_TPCH_SF` — scale factor (default 0.01)
- `GRANARY_TPCH_REPEATS` — timed repeats, best-of-N reported (default 3)
- `GRANARY_TPCH_QUERIES` — comma-separated query numbers (default all)
- `GRANARY_TPCC_WAREHOUSES` — default 1
- `GRANARY_TPCC_TERMINALS` — default 4
- `GRANARY_TPCC_SECONDS` — measurement interval (default 10)
- `GRANARY_TPC_SEED` — PRNG seed for generation and mix selection
- `GRANARY_TPC_HOST` — host label for the CSV

## Output

CSV to stdout, human summary to stderr, matching `bench_compare`'s posture.
Results are committed under `bench/results/` as they are produced.

TPC-H columns:
`host,engine,sf,query,verdict,wall_s,cpu_s,cpu_wall_ratio,rows_out,cross_check`

TPC-C columns:
`host,engine,warehouses,terminals,seconds,txn_type,count,tps,p50_ms,p95_ms,p99_ms,retries`

## Error handling

- Missing `sqlite3` library: executables build and run granary-only; cross-check
  reports `skipped`.
- A query that fails to parse, plan, or execute: recorded with its error in the
  CSV `verdict` column; the run continues to the next query.
- A cross-check mismatch: reported loudly on stderr and marked `MISMATCH` in the
  CSV, and the smoke-tier test fails on it. A mismatch means either an engine bug
  or a bad rewrite, and both are worth failing on.
- Generation at a scale factor that would exceed available disk: checked up front
  against the estimated dataset size and refused with a clear message rather than
  failing mid-load.

## Testing

- `Tpch_gen` / `Tpcc_gen` unit tests: exact row counts per scale factor
  (lineitem is the only table without a fixed multiple — assert the spec's
  documented range), byte-identical output across two runs with the same seed,
  different output for different seeds, and value-range sanity per column.
- QCheck properties over the generator's random primitives: the spec's
  `random_a_string` / `random_e_string` / non-uniform random helpers stay within
  their declared alphabets and bounds for arbitrary seeds and lengths.
- The smoke tier runs as an integration test: SF 0.001 TPC-H over all runnable
  queries with the cross-check enabled, plus a 1-warehouse TPC-C run of a few
  hundred transactions. Asserts correctness and completion, never latency.
- A test asserting every query 1–22 has a verdict and that no query is silently
  missing from the table.

## Order of work

1. `Tpch_gen` + `Tpch_schema` + loader, with generator tests.
2. Q1–Q22 rewrites, gap triage, and Forgejo issues for what cannot be expressed.
3. `Bench_report` (shared engine signature and CSV), `bench_tpch.ml`, SQLite
   cross-check.
4. `Tpcc_gen` + `Tpcc_txn` + `Tpcc_driver` + `bench_tpcc.ml`.
5. Smoke tier wired into CI.
6. File the TPC-E follow-up issue.
