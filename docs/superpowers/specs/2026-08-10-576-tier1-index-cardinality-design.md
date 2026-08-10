# #576 tier 1: per-index leading-column cardinality statistics

Tier 1 of the #576 umbrella issue ("the planner has no cardinality statistics,
and three constants are now standing in for them"). Tier 3 (recalibrating
`nlj_probe_cost_ratio` for a seeked build side) shipped as PR #705. This spec
covers only tier 1 — a real, measured selectivity number for a non-unique
index's leading column, replacing the `unbounded_rows` fallback that currently
makes `estimate_rows` and `build_side_seek_is_unambiguous` blind to the
difference between an equality prefix matching 1% of a table and one matching
100% of it (`planner.ml:1632`'s own doc comment names this gap explicitly).
Tier 2 (per-column histograms, needed to make `range_seek_rows` honest for
*parameterized* ranges) is explicitly out of scope for this document.

## Problem

Three places read a stand-in for a statistic the catalog does not have:

- `table_rows_estimate` (`planner.ml:964-973`) derives a table's row count from
  `next_rowid`, the only real cardinality number the catalog carries today.
  It answers "how big is the table", never "how selective is this predicate".
- `estimate_rows`'s `Op_index_lookup` branch (`planner.ml:1289-1304`) can only
  say "1" (unique point lookup) or "don't know" (`unbounded_rows`, via
  `range_rows_estimate`/`range_seek_rows` for a literal range, or the flat
  fallback otherwise) for anything that isn't a unique key. A non-unique
  equality prefix — `WHERE tenant_id = 1` on a 50-tenant table versus the same
  query on a 5,000-tenant table — gets the identical `unbounded_rows` answer
  in both cases, even though the first returns 1/50th of the table and the
  second returns 1/5000th.
- `build_side_seek_is_unambiguous` (`planner.ml:1520-1550`) therefore *declines*
  a hash join's build side seeking through a non-unique equality prefix at all,
  and falls back to a full scan — correct when the prefix is non-selective
  (#546/#575's finding: a non-selective seek is 2.6-3.5x slower than a scan on
  disk), wasteful when it is selective, and the planner has no way today to
  tell the two cases apart.

## Data model

Extend `index_info` (`lib/catalog/catalog.mli:128-137`) with an optional stats
field:

```ocaml
type index_stats =
  { distinct_count : int  (* distinct values of the leading indexed column, at analysis time *)
  ; rows_at_analysis : int  (* rows the table held when this was computed *)
  }

type index_info =
  { idx_name : string
  ; idx_table : string
  ; idx_columns : string list
  ; idx_unique : bool
  ; idx_tree_id : int
  ; idx_expr_flags : bool list
  ; idx_where_sql : string option
  ; idx_origin : idx_origin
  ; idx_stats : index_stats option  (* new; None = never analyzed *)
  }
```

`None` is the default and permanent state for: every index that exists before
this ships, every `UNIQUE` index (its cardinality is definitionally 1 per key
— no stat is useful there), every `WITHOUT ROWID` table's indexes, every
columnar table, and every expression index (the leading "column" is an
expression, not a stored value the walk can read directly — expression-index
stats are future work, not silently attempted here). Every planner change in
this design is additive on `Some`, so `None` is exactly today's behavior with
zero risk of regression for anything not analyzed.

**Encoding.** `encode_index_value`/`decode_index_value` (`catalog.ml:1123`,
`1207`) already version their extended fields — the current encoder always
writes version 3 (origin byte, expr flags, optional WHERE). This bumps to
version 4: after the version-3 fields, write a presence byte then, if present,
two varints (`distinct_count`, `rows_at_analysis`). `decode_index_ext_fields`
gains a version-4 branch; versions 1-3 decode with `idx_stats = None`, so no
migration step or format flag day is needed — old databases just read as
"unanalyzed" until their indexes are recreated.

## Population

Computed once, inside `execute_create_index`'s existing full-table walk
(`exec.ml:4797`, confirmed to already decode every row and evaluate the index
key for every candidate) — this adds no new scan. While walking (non-unique
indexes only — unique indexes skip this), accumulate a `Hashtbl` keyed on the
leading indexed column's encoded key bytes; a row that fails the partial-index
WHERE clause is excluded, matching what the index itself will contain. At the
end of the walk, `distinct_count = Hashtbl.length seen`, `rows_at_analysis` =
count of rows inserted into the index. This is written into the same
`index_info` row the walk already produces, inside the same DDL transaction
`execute_create_index` already opens — so population is atomic with index
creation, not a separate step that can be half-done.

There is no incremental maintenance: an `INSERT`/`UPDATE`/`DELETE` after
`CREATE INDEX` does not touch `idx_stats`. This is a deliberate choice, not an
oversight — tier 1 is scoped to "does a stat exist and is it directionally
useful", not "is it always exact", and incremental exact distinct-counting is
real complexity (approximate sketches, or per-write bookkeeping on every
indexed table) that this epic explicitly does not need yet. The stat goes
stale exactly the way SQLite's own `ANALYZE`-derived stats go stale between
runs. There is no dedicated refresh command in this design: `DROP INDEX`
followed by `CREATE INDEX` recomputes it. A standalone refresh/ANALYZE command
is future work if staleness turns out to matter in practice — nothing here
blocks adding one later, since it would just call the same population logic
this adds to `execute_create_index`.

## Consumption

Both changes are additive: they only fire on `Some idx_stats`, and only for a
non-unique, equality-only prefix seek with no range component (a ranged seek
is tier 2's problem, not this one).

**`estimate_rows`**, `Op_index_lookup` branch — when the seek is a non-unique
equality prefix and the matching index has `idx_stats = Some { distinct_count;
_ }` with `distinct_count > 0`, estimate:

```
min (table_rows_estimate table_meta / distinct_count) (table_rows_estimate table_meta)
```

instead of falling through to `range_rows_estimate`/`unbounded_rows`. This
mixes the *current* row count with the distinct count *at analysis time* —
deliberately: it assumes the value distribution's shape (not its absolute
scale) is stable, which is the same assumption every stats-based selectivity
estimator makes between refreshes, and is strictly better than
`unbounded_rows`'s implicit assumption that every predicate matches
everything.

**`build_side_seek_is_unambiguous`** — additionally admits a `Seek_index` with
a non-unique equality prefix and no range when the estimate above is
`<= table_seek_budget meta` (the existing `table_rows_estimate meta /
build_side_seek_break_even_ratio` budget, unchanged). Today this shape is
unconditionally declined; with a stat available and the estimate inside
budget, the build side seeks instead of scanning — the #546/#575 win the epic
named this tier for.

Nothing else in the cost model moves: `nlj_probe_cost_ratio` /
`nlj_probe_cost_ratio_seeked_build` (tier 3, done) and `range_seek_rows` /
`range_rows_estimate` (tier 2, out of scope) are untouched.

## Testing

- Round-trip encode/decode test for `index_stats` (present and absent) in
  whichever catalog test file already covers `encode_index_value` /
  `decode_index_value`, plus a fixed version-3 byte string decoded to confirm
  `idx_stats = None` for pre-existing data — the backward-compatibility
  guarantee this design leans on.
- A pinning test analogous to `test_join_cost_model_576.ml`: build a table
  with a skewed non-unique column (e.g. 100,000 rows, 50 distinct values),
  `CREATE INDEX`, and assert `estimate_rows` reflects ~2,000 rows for an
  equality prefix rather than `unbounded_rows`.
- A build-side-seek admission test: same shape, assert
  `build_side_seek_is_unambiguous` now admits the equality-prefix seek where
  it previously declined it (and still declines it when `distinct_count` would
  put the estimate over `table_seek_budget`, e.g. only 2 distinct values on
  the same table).
- A no-regression check that a `UNIQUE` index, a pre-existing (unanalyzed)
  index, and a `WITHOUT ROWID` table's indexes all keep `idx_stats = None` and
  produce identical plans to `main` before this change.
