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
- **StockLevel is rewritten**: `COUNT(DISTINCT s_i_id)` is a granary parse error
  (#491), so the engine returns the deduplicated ids and the count is taken
  client-side. Every other profile runs the spec's statements untouched. The
  verdicts are recorded in `Tpcc_txn.all` and *measured* by the smoke test, not
  asserted.
- **Engine order is fixed.** granary loads and runs first, SQLite second, over
  the same directory, so the second engine meets a page cache the first one
  warmed.

Treat the numbers as a measurement tool for our own regressions and for a rough
sense of distance from SQLite — not as a competitive claim.

## The one-worker pool, and why the terminal sweep flatlines

`Tpcc_driver.run` takes a **list of workers** and guarantees that no worker runs
two transactions at once. That is not a stylistic choice:

> A granary `Db.t` holds exactly **one** explicit-transaction slot.
> `Db.begin_txn` answers `"transaction already active"` to a second `BEGIN`, and
> two terminals interleaving `BEGIN`/`COMMIT` on one handle would not merely
> error — the second terminal's `COMMIT` would commit the first's half-finished
> work.

A TPC-C transaction is inherently multi-statement and explicit, so one granary
connection can carry exactly one transaction at a time, and the benchmark
therefore runs a **one-deep pool**. Reference SQLite is run one-deep too, for a
different reason: the `sqlite3` bindings are blocking calls inside a
single-domain Lwt program, so a second handle could not overlap anything either,
and giving one engine a deeper pool would turn the comparison into a comparison
of pool depths.

The consequence is deliberate and visible in the output rather than hidden by
it: **raising `GRANARY_TPCC_TERMINALS` cannot raise throughput.** It raises
queueing delay, and the driver reports that delay as its own column. Each
profile's latency is split into

- `service_ms` — time inside the worker, the engine's own cost, and
- `wait_ms` — time the terminal spent queued for a free worker,

so a flat throughput curve against a rising `wait_ms` is the *expected* reading
of a terminal sweep, not a defect in the run. Do not tune the terminal count
until the number looks better; report the flat curve.

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

## Recorded results

See `bench/results/` for CSV output, and the PR for #500 for the run that
produced the first recorded numbers along with the exact invocation.
