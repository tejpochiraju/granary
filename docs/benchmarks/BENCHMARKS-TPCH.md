# TPC-H-derived OLAP benchmark (#482)

`test/bench_tpch.ml` loads a deterministic TPC-H-shaped dataset into granary and
into reference C SQLite, runs the 22 queries on both, cross-checks the answers,
and writes CSV to stdout. `test/test_tpch_smoke.ml` shares the same data
generator, schema loader and query set at scale factor 0.001, run as an
ordinary `dune test` case — but it is **granary-only**: it asserts result
shape and row counts rather than cross-checking against reference SQLite,
deliberately, so that `granary_tpc` (and this test) keeps building on a
checkout where `sqlite3` is absent.

## These are not TPC-H results

The workload is **derived from** the TPC-H specification. It is not TPC-H, and
nothing here may be reported as a TPC benchmark result.

- **No audit.** TPC results require an independent auditor and a full disclosure
  report. There is neither.
- **No pricing disclosure**, no required durability/ACID demonstration, no
  RF1/RF2 refresh functions, no multi-stream throughput test.
- **The data generator is ours**, not `dbgen`. It follows the spec's random
  primitives, text-pool grammar (§4.2.2.1) and column domains, but its output is
  not byte-identical to `dbgen`'s.
- **Money is `REAL`, not `DECIMAL`.** Neither engine has DECIMAL. Aggregates are
  therefore compared with a relative epsilon rather than exactly. That numeric
  fallback applies only when at least one side is rendered as a float, so two
  TEXT values the engines wrote differently (`'07'` vs `'7'`) stay a
  disagreement rather than being absorbed as arithmetic noise.
- **Substitution parameters are fixed** at the spec's validation values instead
  of randomized, so runs are comparable and the cross-check has a stable target.
- **Engine order is fixed.** granary always loads and runs first, SQLite second,
  over the same directory, so the second engine meets a page cache the first one
  warmed. Best-of-`GRANARY_TPCH_REPEATS` mitigates it but the asymmetry is
  systematic, not noise.
- **Some queries are rewritten** to fit granary's SQL dialect. Every rewrite is
  recorded in the `verdict` column and its rationale lives beside the query in
  `test/tpc/tpch_queries.ml`. Queries granary cannot express at all are
  `skipped`, with the missing capability filed as a Forgejo issue.

Treat the numbers as a measurement tool for our own regressions and for a rough
sense of distance from SQLite — not as a competitive claim.

## What the SQLite cross-check does and does not prove

For every query the harness runs the **same SQL text** through granary and
through reference C SQLite and compares the answers. A green `cross_check`
column proves the two engines **agree on the SQL as written**.

It does **not** prove the SQL is faithfully TPC-H. A mis-transcribed query is
mis-transcribed identically in both engines, so both would return the same wrong
answer and the cross-check would still read `ok`. Query fidelity rests entirely
on careful transcription from the specification's functional query definitions
and on review of `tpch_queries.ml` — not on the cross-check. That limit is
stated here rather than left to be inferred from a green column.

The cross-check does catch: granary planner/executor defects, aggregate
arithmetic drift, join and filter errors, and rewrites that silently changed a
query's meaning.

### A passing cross-check on an empty result is not evidence

Learned the hard way in Task 8a. Two empty answers compare equal, so a query that
legitimately returns **0 rows on both engines** reads `ok` no matter how wrong
the SQL or the engine is — a cartesian product would score the same.

Q2 and Q20 read `ok` at SF 0.001 through two rounds of review. Re-measured at
SF 0.01, where the reference answers are 4 and 3 rows, granary returns 0 rows
for both: silently wrong (#492). Nothing had changed except that the result set
stopped being empty.

Consequently: every verdict in this document was established at **SF 0.01**, and
`Rewritten` means *verified against reference SQLite on a non-empty result*. The
one exception is Q18, flagged in the table below and in its own rationale. The
harness now emits a distinct `ok-both-empty` token in the `cross_check` column so
this class of false pass is visible in the artifact rather than depending on a
reader to notice it.

## Tie-break handling

Q2, Q3, Q10, Q18 and Q21 truncate with `LIMIT` under an `ORDER BY` that is not a
total order — Q10 ties on `revenue` alone, Q18 on `(o_totalprice, o_orderdate)`.
Two engines may legitimately order tied rows differently, so for those five
queries the comparison is on the **multiset** of returned rows. Every other
query is compared **as a sequence**, because there a wrong row order is a real
defect and must be reported.

Residual limit, stated rather than hidden: if a tie spans the `LIMIT` cut, the
two engines may return genuinely different rows and the harness still reports
`MISMATCH`. That is deliberate — at that point the disagreement is
indistinguishable from a real one without re-deriving the query's tie-break
semantics.

## How much of TPC-H granary runs today

Short version: **7 of 22 queries run on granary and are verified correct against
reference SQLite at SF 0.01 on a non-empty result.** Two more run and return the
wrong answer. One more runs and agrees, but only on an empty result, which proves
nothing. The remaining 12 cannot be expressed in granary's SQL at all.

| | count | queries |
| --- | --- | --- |
| `Rewritten`, verified at SF 0.01 on a **non-empty** result | **7** | 1, 4, 6, 7, 12, 19, 22 |
| `Rewritten`, agrees but only on an **empty** result — unverified | 1 | 18 |
| `Rewritten_pending` — runs, **wrong answer** | 2 | 2, 20 |
| `Skipped` — not expressible today | 12 | 3, 5, 8, 9, 10, 11, 13, 14, 15, 16, 17, 21 |

Reference SQLite can run all 22; the harness does not run the skipped ones on
either engine — `bench_tpch.ml` short-circuits `Skipped` queries before
dispatching to either engine, so the CSV shows `verdict=skipped, wall_s=0` for
the same 12 queries on both the granary and SQLite rows.

There is no `Native` query left. Q1, Q6 and Q13 were the last untouched spec SQL,
and none of the three ran: Q1 and Q6 now need a setup view, Q13 is skipped.
**Every query in this benchmark has been transformed to some degree**, and the
transformations are recorded per query in `test/tpc/tpch_queries.ml`.

Two performance results also bound what can be measured, independently of what
parses:

- **Q4** takes **1708 s** on granary at SF 0.01 against **0.0021 s** on SQLite,
  about 800,000× (#493). The answer is correct. It alone is ~97% of a full run.
- **Q9** is **OOM-killed** at SF 0.01 (#498) — a six-way join over ~60 k lineitem
  rows exhausts ~25 GB. SQLite answers it in ~0.05 s.

Together these put SF 0.1 out of reach, which is why Q18 cannot yet be verified
on a non-empty result.

### Per-query verdicts

Measured at SF 0.01, seed 42, one repeat, in the dev container.

| Q | verdict | reason |
| --- | --- | --- |
| 1 | `Rewritten` ✔ | row-level revenue measures projected by a setup view; aggregates take plain columns (#488). 4 rows. |
| 2 | `Rewritten_pending` ✖ | runs, **returns 0 rows where SQLite returns 4** — correlated subquery under a joined FROM (#492) |
| 3 | `Skipped` | `ORDER BY revenue DESC` ranks a computed measure — no expressible form (#490, #495, #489) |
| 4 | `Rewritten` ✔ | correlated `EXISTS` outer reference qualified (#485). 5 rows. **1708 s** (#493) |
| 5 | `Skipped` | `ORDER BY revenue DESC` — no expressible form (#490, #495, #489) |
| 6 | `Rewritten` ✔ | `l_extendedprice * l_discount` projected by a setup view (#488). 1 row. |
| 7 | `Rewritten` ✔ | spec's derived table hoisted into a setup view (#486); `EXTRACT` → `strftime`. 4 rows. |
| 8 | `Skipped` | projection divides one aggregate by another (#494) |
| 9 | `Skipped` | granary is **OOM-killed** at SF 0.01 (#498); the rewrite itself is complete |
| 10 | `Skipped` | `ORDER BY revenue DESC` — no expressible form (#490, #495, #489) |
| 11 | `Skipped` | `ORDER BY value DESC` — no expressible form (#490, #495, #489) |
| 12 | `Rewritten` ✔ | row-level `CASE` measures projected by a setup view (#488). 2 rows. |
| 13 | `Skipped` | `ORDER BY custdist DESC` — no expressible form (#490, #495, #489) |
| 14 | `Skipped` | projection divides one aggregate by another (#494) |
| 15 | `Skipped` | the spec's `(SELECT MAX(total_revenue) FROM revenue0)` names a view inside a subquery's FROM — silently 0 rows (#496); also #497, #488. #491 is fixed, so the setup carries the spec's own `CREATE VIEW revenue0 (supplier_no, total_revenue)` again |
| 16 | `Skipped` | `ORDER BY supplier_cnt` (#490, #495, #489). `COUNT(DISTINCT ps_suppkey)` was the other blocker and is fixed (#491) |
| 17 | `Skipped` | `SUM(...) / 7.0` (#494) **and** correlated subquery under a joined FROM (#492) |
| 18 | `Rewritten` ⚠ | joins made explicit (#486). Runs and agrees — but **0 rows on both engines** at SF 0.001 and 0.01, so the agreement is not evidence. Needs SF 0.1, blocked by #493. |
| 19 | `Rewritten` ✔ | row-level revenue measure projected by a setup view (#488). 1 row. |
| 20 | `Rewritten_pending` ✖ | runs, **returns 0 rows where SQLite returns 3** (#492) |
| 21 | `Skipped` | `ORDER BY numwait DESC` — no expressible form (#490, #495, #489); also #492 |
| 22 | `Rewritten` ✔ | spec's derived table hoisted into a setup view (#486); `SUBSTRING` → `substr`. 7 rows. |

Legend: ✔ verified non-empty · ⚠ agrees only on an empty result · ✖ wrong answer.

### The gaps, by how many queries they block

| Issue | Gap | Blocks |
| --- | --- | --- |
| #490 / #495 / #489 | `ORDER BY` on a computed measure: alias unsupported, aggregate expression unsupported, ordinal silently ignored | 3, 5, 10, 11, 13, 16, 21 |
| #494 | arithmetic over aggregate results in the select list | 8, 14, 17 |
| #492 | correlated subquery under a joined FROM — **silent wrong answer** | 2, 17, 20, 21 |
| #488 | aggregate argument must be a column reference | worked around by setup views in 1, 6, 12, 19 |
| #486 | no comma FROM lists, no derived tables | worked around by explicit joins and setup views throughout |
| #491 | `COUNT(DISTINCT x)`; `CREATE VIEW v (cols) AS` — **fixed**; both forms parse and Q15's setup carries the spec's column list again. Q15 and Q16 remain skipped on their other blockers | 15, 16 |
| #496 / #497 | a view is invisible inside a subquery's FROM (silently 0 rows) and unresolvable in JOIN position | 15 |
| #493 / #498 | correlated `EXISTS` at 800,000× SQLite; six-way join OOM | caps scale; leaves 18 unverifiable and 9 unrunnable |

## Views created by a query's `setup`

Q15 is defined by the spec as three statements — `CREATE VIEW`, the measured
`SELECT`, `DROP VIEW` — of which only the middle one is timed. The view lives in
the query's `setup` list, which runs outside the timed section.

Task 8b generalised that mechanism into the benchmark's main rewrite lever.
`Sema.bind_internal` rewrites a view named in FROM position into a CTE, so a
`CREATE VIEW` in `setup` can carry constructs the query itself may not use: a
row-level measure projected under a name (working around #488), or the spec's own
derived table hoisted whole (working around #486). Six queries — 1, 6, 7, 12, 19,
22 — run only because of this.

**The rule, since `setup` is untimed: a setup view may project row-level
expressions, never an aggregate the query is supposed to compute.** Pre-computing
an aggregate would move measured work out of the measurement. The only views that
aggregate are the ones the spec itself defines that way — Q15's `revenue0` and
Q13's derived table — and both of those queries are `Skipped` anyway.

The harness does **not** discard the database between queries (reloading the
dataset per query is not affordable), so it drops any view a `setup` statement
creates: once **before** setup, making setup re-runnable across repeats and
across the two engines, and once **after** the query, so no view leaks into a
later one.

## Environment knobs

| Variable | Default | Meaning |
| --- | --- | --- |
| `GRANARY_TPCH_SF` | `0.01` | Scale factor. TPC-H's raw dataset is roughly 1 GB per unit. |
| `GRANARY_TPCH_REPEATS` | `3` | Timed repetitions per query; the **best** wall time is reported. Floored at 1. |
| `GRANARY_TPCH_QUERIES` | all 22 | Comma-separated query numbers, e.g. `GRANARY_TPCH_QUERIES=1,6,14`. |
| `GRANARY_TPC_SEED` | `42` | Generator seed. Output is a pure function of seed and scale factor. |
| `GRANARY_TPC_HOST` | system hostname | Value of the CSV `host` column. |
| `GRANARY_TPCH_BATCH` | `500` | Rows per multi-row `INSERT` during the bulk load. |

Before generating anything the runner checks free space under the temp
directory, requiring about `3 × 1.1 GB × SF` — the dataset lands in two engines,
each with indexes. If `df` output cannot be parsed the guard passes: it is
advisory and must not block a run on a filesystem it does not understand.

**The runner does not clean up after itself.** Each run creates
`$TMPDIR/bench-tpch-<pid>/` and leaves it behind, so repeated runs accumulate —
about 3.3 GB per run at SF 1.0. The disk-space guard only checks free space
*before* a run starts; it does not account for, or reclaim, prior runs'
directories. Delete stale `bench-tpch-*` directories under the temp directory
by hand between runs at larger scale factors.

## Scale tiers

| Tier | SF | Roughly | Where |
| --- | --- | --- | --- |
| smoke | 0.001 | ~1 MB, ~6 k lineitem rows | CI, every push (`test_tpch_smoke`) |
| dev | 0.01 | ~10 MB | local iteration, the runner's default |
| laptop | 0.1 | ~100 MB | pre-merge sanity on a real machine |
| full | 1.0 | ~1 GB raw, ~3.3 GB on disk | dedicated host only |

The smoke tier asserts correctness and completion only. **It is never a latency
gate** — CI runners are shared and timing there is noise.

## Running it

```sh
# smoke tier, as CI runs it
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev \
  dune test test/test_tpch_smoke.exe

# full runner at the dev tier
podman run --rm -v "$(pwd):/workspace:z" -w /workspace \
  -e GRANARY_TPCH_SF=0.01 -e GRANARY_TPCH_REPEATS=3 \
  granary-dev dune exec test/bench_tpch.exe > results.csv

# one query, quickly
podman run --rm -v "$(pwd):/workspace:z" -w /workspace \
  -e GRANARY_TPCH_SF=0.001 -e GRANARY_TPCH_QUERIES=6 -e GRANARY_TPCH_REPEATS=1 \
  granary-dev dune exec test/bench_tpch.exe
```

`bench_tpch` is `(optional)` in `test/dune`, exactly like `bench_compare`: it is
the only target that links the `sqlite3` bindings, so a build without them skips
it instead of failing.

## Reading the CSV

Progress and mismatch detail go to **stderr**; only the CSV goes to stdout, so
redirecting stdout gives a clean file.

```
host,engine,sf,query,verdict,wall_s,cpu_s,cpu_wall_ratio,rows_out,cross_check
```

| Column | Meaning |
| --- | --- |
| `host` | `GRANARY_TPC_HOST`, else the system hostname. Numbers are only comparable within one host. |
| `engine` | `granary` or `sqlite`. Two rows per query, one per engine. |
| `sf` | Scale factor the row was measured at. |
| `query` | TPC-H query number, 1–22. |
| `verdict` | `native`, `rewritten`, `rewritten-not-yet-running`, `skipped`, or `error: <exception>` when the engine rejected the query. An error does not abort the run, but on a query the catalogue asserts runs it makes the run exit non-zero (#502). `rewritten-not-yet-running` means the SQL *has* been transformed — it is not spec text — but granary does not yet produce the right answer for it. |
| `wall_s` | Best wall-clock seconds across `GRANARY_TPCH_REPEATS`. `0` when the query did not run. |
| `cpu_s` | User + system CPU seconds of that same best run. |
| `cpu_wall_ratio` | `cpu_s / wall_s`. Near 1.0 means CPU-bound; well under 1.0 means the query waited on I/O. |
| `rows_out` | Rows returned; empty when the query did not run. |
| `cross_check` | `ok`, `ok-both-empty`, `MISMATCH`, `error`, or `skipped`. Identical on both of a query's two rows — it is a property of the pair. |

`ok-both-empty` means the two engines agreed on **zero rows**. It is reported
separately from `ok` because it is not evidence of correctness — see the section
above.

`skipped` means no comparison was attempted because the query's verdict is
`Skipped`. `error` means a query the catalogue asserts *runs* produced no rows
because an engine rejected it — a regression, not a deliberate omission, and it
is deliberately not the same token (#502). Both `MISMATCH` and `error` print a
line to stderr and make the process exit non-zero. Timing is reported and never
gated, but a wrong answer — or a query that stops running — is a real failure.

Because Q2 and Q20 return wrong answers today (#492), a full run **exits 1**.
That is deliberate: the harness reports what the engine actually does.

## Deviations from the design spec

Three defensible choices depart from `docs/superpowers/specs/2026-07-31-482-tpc-benchmarks-design.md`
and are recorded here rather than left to be noticed as drift:

- **The smoke tier does not cross-check against SQLite.** The design called for
  the smoke tier to run "with the cross-check enabled" and fail on mismatch.
  `test_tpch_smoke.ml` instead asserts result *shape* — arity and row count
  against the generator's known counts — with no reference engine involved.
  This is because `granary_tpc`, and this test, must build on a checkout where
  `sqlite3` is absent (see "These are not TPC-H results" above); a smoke test
  is worth having on such a checkout, so the cross-check moved entirely into
  `bench_tpch`, which is `sqlite3`-gated anyway.
- **A missing `sqlite3` skips the whole executable rather than degrading to
  granary-only.** The design described the cross-check as something that is
  "skipped" when `sqlite3` is absent while the executable still builds and
  runs granary alone. `bench_tpch` is instead `(optional)` in `test/dune`,
  exactly like the precedent set by `bench_compare`: without `sqlite3` the
  target does not build at all, rather than building a degraded granary-only
  mode. Consistency with the existing precedent won out over the spec's
  described behavior.
- **Results are reported as prose in this document, not committed CSVs.** The
  design called for `bench_tpch` output to be "committed under `bench/results/`
  as they are produced." No CSVs are committed; instead this document's
  per-query verdict table and narrative are the record of what was measured,
  regenerated by hand when the numbers change. Given Q4's ~28-minute run time
  alone, committing a CSV per run was judged more churn than value at this
  stage.
