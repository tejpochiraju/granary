# #576 tier 2: per-index leading-column histograms

> **SUPERSEDED** by [2026-08-11-576-tier2-per-position-histograms-design.md](2026-08-11-576-tier2-per-position-histograms-design.md) — its Data model/Population/Consumption sections shipped a real bug (a histogram built for the wrong index column position); see that doc's "The bug" section.

Tier 2 of the #576 umbrella issue. Tier 1 (per-index leading-column
`distinct_count`, for a non-unique **equality** prefix) shipped as PR #711.
Tier 3 (`nlj_probe_cost_ratio_seeked_build`) shipped as PR #705. This spec
covers only tier 2 — making `range_rows_estimate` honest for a range whose
bounds are known at plan time but are not both integer literals, replacing
its flat `range_seek_rows = 100` fallback with a real span read off the
index's stored value distribution.

## Scope boundary: what "honest for parameters" cannot mean here

The #576 issue's tier 2 sketch says histograms are "what would make
`range_seek_rows` honest for parameters, not just literals." That is not
achievable without a larger change than this tier makes, and the boundary is
worth stating explicitly rather than leaving it to be rediscovered:
`Db.prepare` calls `Planner.plan` exactly once (`db.ml:2720`), **before** any
parameter is bound — `run`/`iter` later execute that already-fixed plan
against different `~params` (`db.ml:2738` onward). The join strategy and
access path are frozen at prepare time, when a bound parameter's value is
genuinely unknown; no statistic, however good, can size a window whose
endpoints do not exist yet.

**This tier therefore fixes exactly the case #561 partially fixed: a range
whose bounds are literals known at plan time.** #561 handled the
both-ends-integer-literal case via `range_int_literal_span`; this tier
generalizes that using a stored histogram instead of counting an integer span
directly. A range with a `P_param` bound anywhere keeps today's flat
`range_seek_rows` fallback, unchanged. True parameter-aware estimation needs
re-planning per execution (a "generic vs. custom plan" split, in Postgres's
terms) and is out of scope for #576 — if it is ever wanted, it is a new epic
against `Planner.plan`'s one-shot-at-prepare-time architecture, not a
histogram problem.

**A second, narrower boundary: `Plan.range` itself only exists for `Integer`
and `Real` columns.** `Plan.range`'s doc comment (`plan.mli:76-100`) states
this directly — only those two types have a fixed-width (9-byte),
order-preserving key encoding that supports a byte-offset stop condition; a
TEXT or BLOB column's range predicate is never turned into a `Plan.range` at
all and falls back to an unbounded prefix scan before this function is ever
reached. So "generalize to any orderable literal type" in the previous
paragraph is `L_int` and `L_real`, not `L_text`/`L_blob` — this tier's actual
new value is (a) an honest span for a `REAL`-literal range, which #561 never
touched, and (b) a real value-distribution instead of a *distinct-integer-key*
assumption for `INTEGER` ranges too, since #561's span counts key values
assuming density, and a sparse or skewed integer column's true row count can
differ from that span by any amount.

## Data model

Extend `index_stats` (`lib/catalog/catalog.mli:132`) with an optional
histogram:

```ocaml
type histogram =
  { boundaries : string array
    (* histogram_bucket_count + 1 Index_key-encoded boundary keys, strictly
       ascending by byte order: boundaries.(0) is the smallest indexed value
       seen, boundaries.(N) the largest. Bucket i covers
       [boundaries.(i), boundaries.(i+1)). *)
  }

type index_stats =
  { distinct_count : int
  ; rows_at_analysis : int
  ; histogram : histogram option
  }
```

Boundaries are **encoded key bytes** (`Index_key.encode_value`'s output for
the leading indexed column), not typed OCaml values — the same encoding the
B-tree already sorts by. This is deliberate: it means locating a literal
against the histogram is one `Index_key.encode_value` call plus a byte-order
binary search, with no per-type comparison branch and no new disagreement
with the four-comparator inconsistency #579 already tracks. It also means the
histogram automatically inherits #536's decided total order (NaN below every
number, etc.) for free, because it reuses the same encoding that order is
defined on.

`histogram_bucket_count = 20` — a named constant, `nlj_probe_cost_ratio`-style
(one round number, documented, not re-derived per column). 20 buckets is
~5% CDF resolution: enough to separate #561's row-4 residual class of case
(a range spanning a small fraction of the table vs. one spanning most of it)
without needing a variable-resolution scheme to justify.

`histogram = None` in every case `distinct_count` is already `None` for
(`idx_stats` doc comment, `catalog.mli:147-155`: unique index, `WITHOUT
ROWID` table, expression-leading-column index, capped-cardinality walk) —
the histogram is additive to that same population and eligibility, not a
separate scan or a separate decision. It is also `None` when the leading
column's *distinct* value count is below `histogram_bucket_count`: with fewer
distinct values than buckets, an equi-depth histogram degenerates to one
boundary per value, which `distinct_count` alone already answers as well or
better (this is the same "a stat that isn't more informative than what's
already stored isn't worth persisting" reasoning tier 1 applied to unique
indexes).

## Population

Reuses `execute_create_index`'s existing walk (`exec.ml:4804`) — the same one
tier 1 populates `distinct_count`/`rows_at_analysis` from, so this adds no new
scan. The walk's `seen` table changes from `Hashtbl<string, unit>` (presence)
to `Hashtbl<string, int>` (row count per distinct encoded leading-column key)
— `Hashtbl.replace tbl ek (count + 1)` in place of today's `Hashtbl.replace
tbl ek ()`, still capped at `index_stats_cardinality_cap` (100,000 distinct
keys; above the cap, `distinct_count` *and* `histogram` both fall back to
`None`, exactly as `distinct_count` alone does today — no change to that
guard's direction or reasoning).

After the walk, if not capped and `Hashtbl.length tbl >= histogram_bucket_count`:

1. Extract `(key, count)` pairs and sort by key (byte order).
2. Walk the sorted list accumulating a running row total; each time the
   running total crosses a multiple of `rows_at_analysis / histogram_bucket_count`,
   emit the current key as a boundary.
3. Prepend the first key and append the last key so `boundaries` always has
   exactly `histogram_bucket_count + 1` entries spanning the full observed
   range, even when a single skewed value's row count overshoots several
   bucket-widths at once (a value with more rows than one bucket's worth
   still only contributes one boundary crossing — this is the standard
   equi-depth degenerate case, not a bug to guard against).

Persisted in the same DDL transaction as `distinct_count`
(`Cat.set_index_stats`, extended to take `~histogram`), so it rolls back with
the index exactly as tier 1's stats do.

No incremental maintenance — same deliberate non-goal tier 1's population
section documents, for the same reason (`DROP INDEX` + `CREATE INDEX`
recomputes it; a standalone refresh command is future work, not blocked by
this).

## Encoding

Bumps `index_info` to encoding version 5. After version 4's `idx_stats`
fields (`catalog.ml:1159-1167`), add a presence byte then, if present, a
varint `histogram_bucket_count` followed by that many length-prefixed byte
strings (the boundaries). `decode_index_ext_fields` gains a version-5 branch;
versions 1-4 decode with `histogram = None` — same backward-compatibility
story as tier 1's v3→v4 bump, no migration step. The stored bucket count is
written explicitly (not assumed to equal the current
`histogram_bucket_count` constant) so a future change to that constant does
not need a version bump of its own to stay decodable — a decoder just reads
however many boundaries were written.

## Consumption

`range_rows_estimate` (`planner.ml:904`) currently takes only `r : Plan.range`
and reads `range_int_literal_span` (`Plan.P_lit (Ast.L_int _)` on both ends).
It gains the same three arguments `estimate_rows_from_stats` already takes —
`cat`, `meta : Cat.table_meta`, `~idx_tree` — so it can look up the seeked
index's `idx_stats.histogram` the same way tier 1 already locates
`idx_stats` (`index_by_tree cat meta ~idx_tree`, no new lookup path). Its one
call site, `estimate_rows`'s `Op_index_lookup` branch (`planner.ml:1336`),
already has `cat`, `table_meta` and `idx_tree` in scope.

Recognize `L_int` and `L_real` literals on either end (still never `L_text`/
`L_blob`/`L_null` — see the scope-boundary section above for why those never
reach here as a `Plan.range` at all):

- **At least one bound present, literal, and a histogram is available**:
  encode each present bound with `Index_key.encode_value` and locate it in
  `boundaries` by binary search, giving the smallest index `i` with
  `boundaries.(i) >=` the encoded bound (a value below `boundaries.(0)` gets
  `i = 0`; at or above `boundaries.(N)` gets `i = N`). An absent bound uses
  `i = 0` (lower) or `i = N` (upper) directly — the histogram's own extremes.
  Estimate the row count as `rows_at_analysis * (i_hi - i_lo) /
  histogram_bucket_count`, floored at `range_seek_rows`. This is
  **bucket-granularity, not linear interpolation within a bucket**: encoded
  key bytes are not a numeric quantity to interpolate between in general (an
  `IK_int` and `IK_real` region are both fixed-width, but the boundary search
  is written once over raw bytes rather than decoding a value back out to
  interpolate it, which would need a type-specific branch this tier has no
  other reason to add). The uniform-*row-density-per-bucket* assumption this
  makes is the same equi-depth assumption tier 1's review called out
  explicitly for `distinct_count`; stating it here does the same for
  `histogram`.
- **Neither bound is literal (both are parameters, or the range is
  otherwise unrecognized), or no histogram is available** (index
  unanalyzed, capped, or below the bucket-count floor at population time):
  unchanged — falls through to `range_int_literal_span`'s existing
  integer-literal-span handling, and below that to the flat
  `range_seek_rows`. This is what keeps every plan this tier cannot speak to
  identical to today's, the same additivity guarantee tier 1 leans on. Note
  this is an **all-or-nothing** fallback per range, not per bound: a range
  with one literal bound and one parameter bound cannot ask the histogram
  "how many rows below this literal" without also knowing where the
  parameter falls, so it falls back whole, the same as today.

`build_side_seek_is_unambiguous` is **not** touched by this tier — it already
consults `range_rows_estimate` indirectly through `estimate_rows`
(`planner.ml:1336-1343`), so a more honest `range_rows_estimate` improves its
input without any change to its own logic.

## Testing

Following tier 1's precedent (`test_index_cardinality_576.ml`,
`test_join_cost_model_576.ml`, and the catalog round-trip tests):

- Encode/decode round-trip for `histogram` (present and absent) alongside the
  existing `idx_stats` round-trip test, plus a fixed version-4 byte string
  decoded to confirm `histogram = None` for pre-v5 data.
- A pinning test with a skewed `REAL` column (100,000 rows, non-uniform
  distribution), asserting `range_rows_estimate` for a literal range reflects
  the real distribution rather than the flat `range_seek_rows` — the case
  #561 left entirely unfixed (no span-counting logic exists for `REAL` at
  all today).
- A pinning test with a **sparse, skewed `INTEGER`** column (e.g. values
  drawn non-uniformly from a wide range, so `range_int_literal_span`'s
  distinct-key-count assumption would be far from the true row count),
  asserting the histogram-based estimate is closer to the real row count
  than #561's span-counting logic — this is the tier's improvement on the
  case #561 already partially handled, not just the new `REAL` case.
- A one-ended-range test (`WHERE x > <literal>` with no upper bound) against
  the same table, asserting the estimate differs meaningfully between a
  literal near the low end and one near the high end.
- A parameter-bound regression test: same table and range shape, but with one
  or both bounds as `?` — assert the estimate is unchanged from `main`
  (still the flat fallback), pinning the scope boundary this document opens
  with.
- A below-bucket-count-floor test: a column with fewer than
  `histogram_bucket_count` distinct values, asserting `histogram = None` is
  persisted and `range_rows_estimate` falls back to today's behavior.
