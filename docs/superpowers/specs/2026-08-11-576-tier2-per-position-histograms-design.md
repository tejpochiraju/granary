# #576 tier 2, corrected: per-column-position range histograms

Supersedes the "Data model", "Population" and "Consumption" sections of
`docs/superpowers/specs/2026-08-10-576-tier2-column-histograms-design.md`
(the original tier 2 spec). That spec's "Scope boundary" section is
unaffected and still applies. This document exists because the original
spec's Consumption section shipped a real bug — not an implementation
mistake, a design mistake caught during implementation — and the fix changes
the data model enough to need its own record.

## The bug

The original spec assumed a `Plan.range`'s bound describes the same column
an index's leading-column histogram was built over. It does not, in any
reachable query shape.

`Plan.range`'s own doc comment already said this (`plan.mli:76-100`): a
range is "the index column immediately after a seek's equality-covered
prefix." The original spec quoted that sentence and didn't draw the
conclusion. Verified directly in `planner.ml` while implementing tier 2's
Task 5 (and independently re-verified afterward):

- `access_path_for_eqs` (`planner.ml:578-607`) refuses to choose ANY index
  when there is no equality-prefix conjunct at all (`if eqs = [] then None`,
  `:593-594`).
- `prefix_for_index` (`planner.ml:331-355`) never returns a non-empty prefix
  from an empty `eqs` — an index is only even a *candidate* once at least
  one leading column is equality-pinned.
- Both call sites of `range_for_index` — the single-table access path
  (`planner.ml:606`) and the hash join's build-side probe
  (`planner.ml:2082`) — pass `~n_eq:(List.length prefix)` /
  `~n_eq:(List.length probe)`, and both `prefix`/`probe` are guaranteed
  non-empty by their own construction (a probe "always contains at least one
  `Probe_from_left`", `plan.mli:308-310`).

So a `Plan.range`, whenever one exists, always sits at index column position
`n_eq >= 1` — the column immediately after at least one equality-bound
column. **Column 0 is never the ranged column.** `range_for_index`
(`planner.ml:482-521`) is the ONLY place a `Plan.range` is ever constructed
— confirmed by grepping every `{ Plan.r_ty = ...; r_lo; r_hi }` /
`Plan.range` construction site in the tree, there is exactly one.

Tier 2's original Task 4 built a histogram for column 0 (the only column
tier 1 ever computed stats for) and consulted it for `range_rows_estimate`'s
literal bounds — bounds which describe column `n_eq`, a *different* column
of the same index whenever `n_eq >= 1`, which by the analysis above is
always. The resulting estimate compared encoded bytes from two unrelated
value domains: numerically well-formed, semantically meaningless. Every
task review (Tasks 1-4) correctly verified internal consistency against the
original spec; none of them — nor the spec itself — cross-checked that the
histogrammed column and the ranged column were the same column. They never
are.

## What survives unchanged

Tasks 1-3's work — `Cat.histogram = { boundaries : string array }`, the v5
encoding bump, and the population walk's presence→count `Hashtbl` upgrade —
is real infrastructure and is extended, not discarded. Tier 1's
`distinct_count`/`rows_at_analysis` (column 0's leading-column cardinality,
consumed by `estimate_rows_from_stats` for the equality-only case) are also
untouched: that consumer is sound — an equality-prefix seek with no range
genuinely does ask "how selective is column 0", which is exactly what
`distinct_count` answers. This bug is specific to the *range* consumer,
which the original spec bolted onto the wrong column's stat.

## Data model

`histogram : histogram option` (a single, column-0-only field) is replaced
by `range_histograms : histogram option array`, sized to
`List.length idx_columns` — one slot per index column, **including column
0, which is always `None`.** Keeping column 0's slot in the array (rather
than sizing to `n_cols - 1` and offsetting every index by one) is
deliberate: `range_rows_estimate` already has `n_eq` in hand and can index
`range_histograms.(n_eq)` directly, with no off-by-one arithmetic to get
wrong a second time.

```ocaml
type index_stats =
  { distinct_count : int
  ; rows_at_analysis : int
  ; range_histograms : histogram option array
    (* one slot per idx_columns position; index 0 is always None -- see
       this design doc's "The bug" section for why column 0 can never be
       a range's target *)
  }
```

`Cat.histogram` itself (`{ boundaries : string array }`) is unchanged — it
is still a single column's equi-depth-by-row-count boundary array, just now
instantiated once per eligible non-leading column instead of once for
column 0.

**Encoding.** Bumps `index_info` to version 6. After version 5's
`idx_stats` fields (distinct_count, rows_at_analysis, the old single
`histogram`), version 6 replaces the single optional-histogram tail with:
a varint array length (`Array.length range_histograms`, always equal to
`List.length idx_columns` for data written by this version), then for each
slot a presence byte and (if present) the same boundary-count +
length-prefixed-strings encoding version 5 used for its single histogram.
Versions 1-5 decode with `range_histograms = [||]` (an empty array —
**not** `Array.make n_cols None`, since a consumer must be able to tell
"this index predates per-position histograms" from "this index has them
but every slot happens to be None"; `range_rows_estimate` treats an
out-of-bounds or empty-array lookup as "no histogram available", so both
cases fall back identically, but the distinction is worth preserving in
what's stored for any future debugging or `PRAGMA` surface). No migration
step, same backward-compatibility story as every prior version bump in this
series.

## Population

Reuses `execute_create_index`'s existing walk, same as before. Task 3's
column-0 `seen` `Hashtbl` (`key -> row count`) is **untouched** and still
feeds `distinct_count`/`rows_at_analysis` exactly as it does today — it is
not part of `range_histograms` (its slot is always `None`, per the data
model above).

Add one more `Hashtbl<string, int>` per index column position
`1 .. n_cols - 1`, built from the same per-row `iks : Index_key.value list`
the walk already computes for every candidate row (the full column list,
not just the leading value — no new per-row I/O or decode, only extra
hashtable inserts). A position is skipped entirely (its slot stays `None`,
no `Hashtbl` allocated for it) when:

- **it names an expression column** (`col_expr_flags`'s flag at that
  position) — `range_for_index`'s `col_ordinal` lookup can never resolve an
  expression string against `meta.Cat.columns`' names, so `range_for_index`
  always answers `None` at that position anyway (see "The bug" section);
  tracking a histogram nothing can ever consult is dead weight, the same
  reasoning tier 1 applied to an expression *leading* column.

Same whole-index eligibility gates as tier 1, **unchanged and unrevisited**:
a UNIQUE index, a WITHOUT ROWID table's index, and (transitively, since it's
checked first) any table not walked at all skip the entire walk's stats
collection — column 0's `distinct_count` AND every position's
`range_histograms` slot. Whether WITHOUT ROWID could benefit from
non-leading-column histograms despite tier 1's original exemption reasoning
is explicitly out of scope for this fix.

**Cap fallback is per-position, not whole-index** — a deliberate widening
from tier 1's all-or-nothing cap behavior. Each position's `Hashtbl` is
capped independently at `index_stats_cardinality_cap` (100,000, unchanged);
a position that hits its cap mid-walk persists `None` at that slot only.
Column 0's `distinct_count` and every OTHER position's histogram are
unaffected — a table with one near-unique column and several genuinely
low-cardinality ones now keeps useful stats for the low-cardinality
columns instead of losing everything to the one outlier. This does widen
peak retained memory versus tier 1's single-`Hashtbl` walk: up to
`(n_cols - 1) * index_stats_cardinality_cap` entries retained at once in
the worst case (every non-leading, non-expression column simultaneously
near its own cap) rather than one `index_stats_cardinality_cap`-bounded
table. For the composite indexes this feature is aimed at (a handful of
columns, per `docs/superpowers/specs/2026-08-10-576-tier2-column-histograms-design.md`'s
own #532-derived motivating examples), this is a small constant-factor
increase over tier 1's already-bounded walk, not a new unbounded-growth
risk — but it is a real, measurable widening worth naming rather than
leaving implicit.

Histogram bucketing (sort by key, equi-depth-by-row-count boundary
emission, prepend/append min/max) is identical per-position logic to what
Task 3 already built for column 0 — the same `build_histogram` function,
called once per eligible non-leading position instead of once for column 0.

## Consumption

`range_histogram_estimate` (`planner.ml:1159`, from Task 4) changes its
lookup from `idx_stats.histogram` to `idx_stats.range_histograms.(n_eq)`,
where `n_eq` is the equality-prefix length already available at the call
site (`List.length keys` on the `Op_index_lookup`'s own `keys` field, or the
equivalent on the join build-side probe). An out-of-range or missing index
(pre-version-6 data, `range_histograms = [||]`) falls back exactly like a
`None` slot does today — no separate code path needed, `Array.length
range_histograms > n_eq` is the only new guard.

Everything else Task 4 built is unchanged: bucket-granularity lookup (no
linear interpolation), the all-or-nothing literal/parameter fallback per
range, the `range_seek_rows` floor, and the restriction to `Integer`/`Real`
literal bounds (`Plan.range` still only ever exists for those two types).

## Testing

Following the same precedent as the original spec:

- Encode/decode round-trip for `range_histograms` (empty array from pre-v6
  data, an array with some `Some`/some `None` slots, a fully-`None` array)
  alongside the existing `idx_stats` round-trip tests.
- A population test on a **3-column** composite index (`a`, `b`, `c`) with
  `a` equality-bound in the walk's seed data and `b`/`c` skewed
  differently, asserting `range_histograms.(0) = None`,
  `range_histograms.(1) = Some _` and `.(2) = Some _` with distinguishable
  boundary shapes (proving positions are populated independently, not one
  histogram copied across slots).
- A per-position cap test: one column near-unique (hits the cap) and a
  sibling column genuinely low-cardinality on the same index, asserting the
  capped position is `None` while the sibling position and `distinct_count`
  are still populated — the test that directly pins the "per-position, not
  whole-index" cap-fallback decision.
- The consumption-side plan-shape test this bug was found while writing
  (`test_range_histogram_576.ml`) gets rewritten against a genuinely
  reachable shape: a 2-column index, one column equality-bound in the
  query's WHERE clause, a literal range on the *second* column — the
  `#532`/`#561` pattern (`sw = 1 AND si BETWEEN 20 AND 25`) this whole tier
  was motivated by in the first place. Include the non-vacuousness
  sanity check the original Task 5 already performed (temporarily force
  `range_histogram_estimate` to return `None`, confirm the test fails,
  revert, confirm it passes again) as a permanent part of the test's own
  documentation, not just a one-time manual check.
- An expression-column-at-a-non-leading-position exemption test, mirroring
  tier 1's existing expression-*leading*-column test.
