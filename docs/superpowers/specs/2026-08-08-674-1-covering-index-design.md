# #674 (1 of 3): covering-index reads for MIN/MAX/COUNT/EXISTS over `Op_index_lookup`

Split from #677, which lists three independent perf defects downstream of a
correct index seek in Delivery's `SELECT MIN(no_o_id) FROM new_order WHERE
no_w_id = ? AND no_d_id = ?` (`rows_examined = index_entries = 900`, the whole
district, for a query that needs one column of one row). Item (2) (`Op_limit`
early-stop) shipped in `fix/677-limit-early-stop`. Item (3) (sort elision) is
being designed in parallel on `perf/674-3-sort-elision`. This is item (1),
flagged in #677 as "the deepest change" needing its own pass — design only,
no implementation.

## Where the waste actually lives

`stream_index_lookup` (`lib/sql/exec.ml:10530`) does, per matching index
entry: decode the rowid suffix, `rh_get` the full table row, decode it, hand
it up the stream. For `MIN(no_o_id)` the value being aggregated is the
*trailing key column of the index itself* — it is already sitting in the
7-or-so bytes of `ikey` the seek just walked past. The `rh_get` is pure
waste: one B-tree descent into the table tree per index entry, for a query
that never needed the table tree at all.

This is not the same bug as #677 item (2). Even after `Op_limit` stops
pulling early (already shipped) and item (3) elides the redundant `Sort`,
`SELECT MIN(no_o_id) FROM new_order WHERE no_w_id=1 AND no_d_id=5` has **no
`Op_limit` or `Op_sort` in its plan at all** — it is `Op_aggregate { child =
Op_index_lookup; aggs = [MIN(no_o_id)]; group_cols = [] }`, confirmed by
reading `plan_projection` (`lib/sql/planner.ml:1886-1908`): `is_aggregated`
wraps whichever access-path node `choose_access_path` picked (here,
`Op_index_lookup` directly — TPC-C's `no_w_id, no_d_id` equality prefix hits
`access_path_for_eqs`, `lib/sql/planner.ml:571`) in `Op_aggregate` with no
intervening node. So this item's fix is squarely in the aggregate path, not
downstream of a `Limit`/`Sort` this query doesn't have.

## There is already a fast-path mechanism to extend, not a new one to invent

`lib/sql/exec.ml:11360-11531` (`aggregate_fast_path` / `run_aggregate_fast_path`,
#247) already exists for exactly this class of problem — a no-GROUP-BY
aggregate that can fold over a cursor without materializing rows through
`Lwt_stream.to_list`. Today it recognizes exactly two child shapes:

```ocaml
match child with
| Plan.Op_seq_scan { table_meta; _ } -> run_aggregate_fast_path ... None ...
| Plan.Op_filter { pred; child = Plan.Op_seq_scan { table_meta; _ } }
  when not (plan_expr_has_subquery pred) -> run_aggregate_fast_path ... (Some pred) ...
| _ -> Lwt.return None
```

`Op_index_lookup` (and `Op_filter { child = Op_index_lookup; _ }`, for a
residual predicate the index prefix doesn't cover) fall through to `None`,
which is exactly what routes TPC-C's Delivery query to the general
`stream_aggregate` path: `to_stream` on the `Op_index_lookup` child (full
per-entry `rh_get`, as above) then `Lwt_stream.to_list`. **This item's
natural shape is a third arm of `aggregate_fast_path`, not a rewrite that
routes through items (2)/(3).**

### Why "rewrite to already-fixed (2)+(3)" is the wrong strategy here

#677's own text floated "recognize this shape and rewrite it to
already-fixed (2)+(3)" as the likely cleanest implementation — i.e. turn
`MIN(no_o_id) FROM t WHERE prefix` into `SELECT no_o_id FROM t WHERE prefix
ORDER BY no_o_id LIMIT 1` and let item (2)'s early-stop and item (3)'s sort
elision carry it. Having now read both the shipped item (2) and the plan
shape here, that framing does not hold up:

- **It solves a problem this query doesn't have.** There is no `Op_sort` and
  no `Op_limit` in `Op_aggregate { child = Op_index_lookup }` — nothing for
  (2) or (3) to fix. A rewrite would have to *manufacture* a `Sort`+`Limit`
  around the index lookup, elide the `Sort` via (3), early-stop the `Limit`
  via (2), unwrap the single-row result back into a scalar for the
  aggregate's output slot — three moving parts and two feature dependencies
  to reproduce what a direct fold does in one pass.
- **It still pays a table fetch it doesn't need.** Even a perfectly
  early-stopped, sort-elided rewrite still flows through
  `stream_index_lookup`, which does an unconditional `rh_get` per entry
  pulled. Reaching "no table tree touch at all" — the actual target the
  issue's benchmark section names (~5.2 ms → ~0.1 ms) — requires either (a) a
  new streaming mode on `stream_index_lookup` that skips `rh_get`, or (b)
  reading the value straight off `Index_key.decode`. Either way the real fix
  is inside or alongside `stream_index_lookup`, so the rewrite doesn't avoid
  that work — it just adds a detour through (2)/(3) on top of it.
- **`COUNT(*)`/`COUNT(col)` has no `LIMIT 1` reading at all.** A COUNT over
  an index lookup needs every entry counted, not the first one — the
  rewrite-to-(2)/(3) framing was written with MIN/MAX specifically in mind
  and doesn't generalize to the other two shapes the same issue names.
  `aggregate_fast_path` already has to special-case COUNT-star's decode-free
  loop (`need_decode`, `lib/sql/exec.ml:11465`) for the seq-scan case; the
  same shape applies unchanged to an index lookup.
- **(2) and (3) are still worth having independently.** `Op_limit`
  early-stop fixes every `LIMIT` query in the engine, most of which are not
  aggregates; sort elision fixes every `ORDER BY` that an access path
  already satisfies. Neither should be *narrowed* to "the mechanism MIN/MAX
  rides on" — that would make item (1)'s correctness depend on two other
  in-flight designs landing in a specific shape, when it doesn't need to.

**Recommendation: bespoke short-circuit, as a third arm of
`aggregate_fast_path`/`run_aggregate_fast_path`, parallel to (and reusing as
much machinery as possible from) the existing seq-scan fast path.** It is
smaller, has no dependency on (2) or (3) landing in any particular shape,
and reaches the actual "skip the table tree" target directly instead of
through two layers of indirection. (2) and (3) remain valuable improvements
to the *general* `stream_index_lookup`/`Op_limit`/`Op_sort` paths — a plain
`SELECT no_o_id FROM new_order WHERE ... ORDER BY no_o_id LIMIT 1` (no
aggregate) still wants both — but MIN/MAX/COUNT/EXISTS over an index lookup
should not be defined in terms of them.

## Detecting the shape

Three additions to `aggregate_fast_path`'s child match, each gated on
`group_cols = [] && having = None && agg_windows = []` exactly as today:

**1. `COUNT(*)` / `COUNT(col)` over `Op_index_lookup` (optionally
`Op_filter`-wrapped).** No column requirement beyond what a residual filter
reads — this is the seq-scan fast path's `run_aggregate_fast_path`
unmodified except for *which tree* it seeks. `agg_tree_id` becomes
`idx_tree` instead of the table tree's storage id, and the per-entry loop
counts/decodes `Index_key.decode`'d columns instead of `rh_get`'d table
rows. `COUNT(*)` needs no decode at all — the existing `need_decode = false`
branch already fires straight off `S.seek_next`'s raw key, so it costs
nothing beyond walking `idx_tree` instead of the table tree; that alone
turns the table-tree touch into an index-only touch (#546-style: it still
walks every matching key, so `index_entries` is unchanged, but
`rows_examined`/table fetches drop to 0).

**2. `MIN`/`MAX` on a single column, where that column is *the next
unconstrained key column of the index* — no `range`, no residual
predicate beyond the equality prefix `keys` already encodes.** This is the
one that needs a genuine seek-direction trick, not just "skip the table
fetch": `MIN` needs the index's forward order (already what `rh_seek_ge`
walks), reads the trailing column off the *first* qualifying entry via
`Index_key.decode`, and stops — one seek, one entry, no loop. `MAX` needs
the reverse: seek to the *last* key in the equality-prefix range and step
backward one entry. `stream_index_lookup` only ever walks forward
(`S.seek_next`); a reverse seek/step primitive does not exist on this path
today and would need to be added (or approximated by seeking to the
successor prefix and stepping back once — check what `S.t`'s cursor API
already offers before assuming a new primitive is needed).

   Detecting "next unconstrained key column" needs the index's full column
   list, which `Op_index_lookup` does not carry (`lib/sql/plan.ml:176-185`
   carries only `keys` — the bound prefix — and `range` — the bound on the
   column after it — not the index's remaining columns). The planner has to
   go back to `Cat.index_info.idx_columns` (looked up via `idx_tree`, e.g.
   through whatever gave `access_path_for_eqs` the index in the first place)
   and compare position `List.length keys` in that list against the
   aggregate's `col_ord`. `range = None` is required — if a range bound is
   present, the "first/last qualifying entry" is no longer simply "first/last
   key in the seek", and this design does not attempt that composition.

**3. `EXISTS` / a plain existence check over an `Op_index_lookup`.** This one
is mostly already fast: `eval_exists_subquery` (`lib/sql/exec.ml:9699-9715`)
already pulls at most one row via `Lwt_stream.get stream` and abandons the
rest under an owned #493 snapshot — no #677-style leak risk, because the
snapshot's teardown doesn't depend on stream exhaustion. The one remaining
waste is that the single row it *does* pull still pays one `rh_get` it
doesn't need — existence needs only "did the seek find a qualifying key",
not the row's contents. This is a much smaller win than (1)/(2) above (one
table fetch instead of up to N) and is naturally covered by whatever
skip-the-`rh_get` primitive (1) introduces on `stream_index_lookup`, applied
to `EXISTS`'s single pulled row — not a separate mechanism.

## NULL safety

The hazard: an index entry whose indexed column is NULL still gets seeked
and would, if read carelessly, corrupt a MIN/MAX or inflate a COUNT that
should have excluded it.

Concretely, this is safer than it looks, for a reason specific to how NULLs
enter this index range in the first place:

- **The equality-bound prefix (`keys`) already excludes NULLs.**
  `stream_index_lookup`'s `index_lookup_values` (`lib/sql/exec.ml` — the
  match right after `s_opt`) returns `None` — no rows at all — the moment
  *any* bound value is NULL: "A NULL or type-mismatched value anywhere in the
  key kills the whole conjunction." TPC-C's `no_w_id`/`no_d_id` prefix is
  never NULL in practice, but the fast path must not *assume* that; it
  inherits the same `index_lookup_values` gate the general path already uses,
  so this is free.
- **The column actually being read (MIN/MAX's target, or the columns COUNT
  filters/decodes) is a different question**, and per `CLAUDE.md`'s NaN
  section, `Index_key.encode_value`'s NULL and NaN both collapse to the same
  `0x00` byte — `Index_key.decode` cannot tell a NULL trailing column from a
  NaN trailing column apart; both decode to `IK_null`. For `COUNT(col)`
  (which must exclude NULL but count NaN) this is a real hazard if the
  covering path is used unconditionally.
- **The gate: only take this fast path when the target column is `NOT
  NULL`.** `Row.column.not_null` (`lib/encoding/row.ml:20`) is a static,
  always-available bit — no probe row needed. Restricting `MIN`/`MAX` and
  `COUNT(col)`'s covering path to `NOT NULL`-declared columns sidesteps the
  NULL-vs-NaN ambiguity entirely: a NOT NULL column can still legally hold
  NaN (NaN is a value, not an absence, per the #536 decision quoted at
  length in `CLAUDE.md`), and `MIN`/`MAX`/`COUNT(col)` over a NOT NULL column
  never need to distinguish "NULL, skip" from "NaN, keep" because the NULL
  case cannot occur — `0x00` in that column's slot can only mean NaN.
  `no_o_id` is TPC-C's actual case and is the table's primary-key-adjacent
  ordering column, declared NOT NULL, so this is not a hypothetical
  restriction for the motivating query — it is the query. A nullable indexed
  column simply falls back to the general path (or, for MIN/MAX, could later
  decode-and-check `IK_null` against the same `not_null_exempt`-style
  reasoning `Exec.not_null_violation` uses elsewhere — out of scope for this
  first cut; ship the conservative gate first).
- **`COUNT(*)`** needs no per-column NULL reasoning at all — it counts
  entries, and an index by construction has exactly one entry per row that
  satisfies the WHERE prefix (NULLs in *other*, non-key columns are
  irrelevant; NULLs in the equality-bound prefix columns are already
  excluded by `index_lookup_values` above).
- **`EXISTS`** needs no per-column reasoning either — existence of a
  qualifying key is unaffected by what any column (indexed or not) holds.

This also means the NOT NULL gate must be checked against the **declared**
column, not inferred from the index. `idx_unique` (`Cat.index_info`) says
nothing about nullability of a non-PK column; `Row.column.not_null` is the
only correct source, reached via the table's `table_meta.columns` at the
column ordinal the aggregate targets — already available, since
`Op_index_lookup` carries `table_meta` for exactly this kind of lookup.

## Partial index-prefix coverage

Two distinct "doesn't fully cover" cases, both already have a clean fallback:

- **A residual predicate beyond the equality prefix** (`Op_filter { child =
  Op_index_lookup }`, mirroring the existing `Op_filter { child =
  Op_seq_scan }` arm) — the filter may read columns beyond the index's own
  columns, which are not in `ikey` and would need the table row anyway. The
  correct rule, matching the seq-scan fast path's existing `need_decode`
  logic: if the predicate (or the aggregate's `arg_expr`, for an expression
  argument) reads only columns that are part of the index's key encoding
  (checkable against `idx_columns`' ordinals), the covering path still
  applies and both predicate and aggregate evaluate off `Index_key.decode`'d
  values with no `rh_get`. If it reads *any* column outside the index, this
  falls back to the existing full-row fast path (still index-seeked, not
  seq-scanned — `run_aggregate_fast_path` would need a variant that seeks
  `idx_tree` for iteration order but still `rh_get`s each row, which is
  strictly better than today's `stream_index_lookup`-then-materialize even
  without full covering) or, at minimum, to today's general path unchanged.
  **Recommend shipping only the "predicate reads no non-index columns" case
  first** — the TPC-C motivating query has no residual predicate at all
  (`no_w_id`/`no_d_id` are both in `keys`), and the index-seek-but-still-fetch
  middle case is a separate, smaller optimization not needed to close this
  issue's benchmark gap.
- **A `range` on the column immediately after the equality prefix** (#517) —
  for `COUNT(*)`/`COUNT(col with no arg needing beyond-index reads)` this is
  fine, the range only narrows which entries are walked and every entry
  still needs no table fetch. For `MIN`/`MAX`, as noted above, this design
  does not attempt the "first/last entry within a sub-range" composition in
  this first cut — a `range` present routes to the general path. This is a
  narrower target than TPC-C's Delivery query needs (no `range`, just the
  two-column equality prefix), and revisiting it is cheap once the
  unconditional-first-cut version is landed and measured.

## Correctness hazards to watch during implementation

- **Byte-identity with `aggregate_one`/`make_agg_acc_over_values`.** The
  `#247` comment at `lib/sql/exec.ml:11233` is explicit that the fast-path
  accumulator logic "MUST stay byte-identical to `aggregate_one`". A new
  index-covering variant is a *third* place computing MIN/MAX/COUNT and must
  not duplicate the comparison/accumulation logic — reuse
  `make_agg_acc_over_values` over `Row.value`s decoded from `Index_key`
  rather than writing new comparison code, the same way the seq-scan fast
  path reuses it over table-row values.
- **`DISTINCT`.** `make_agg_acc`'s existing DISTINCT handling
  (`lib/sql/exec.ml:11254-11269`) wraps the getter's result through a dedup
  filter; this composes fine with an index-column getter, but `COUNT(DISTINCT
  col)` over an index only needs no table fetch if `col` is itself an index
  column — same non-index-column fallback rule as above.
- **Transaction visibility (#262).** `run_aggregate_fast_path` folds over
  `rh_begin store mode`, which is what makes an uncommitted-but-open-txn
  write visible to the fast path. Any `idx_tree`-seeking variant must open
  the read handle the same way — a naive "read the index tree directly"
  implementation that skips `rh_begin` would silently miss uncommitted rows
  written earlier in the same transaction, which is exactly #262's bug in a
  new place.
- **Expression indexes.** `Cat.index_info.idx_expr_flags` marks columns that
  are `expr SQL`, not a plain column reference. `MIN(no_o_id)` needs
  `no_o_id` to be a *plain* indexed column at the target ordinal, not an
  expression that merely evaluates to the same values — an expression-index
  column's encoded bytes are the expression's *result*, which happens to
  equal what a plain-column MIN would want here, but the general rule (an
  aggregate's `arg_expr`, if any, must be recognized as "the same expression
  as the index's expr-column" — full expression-equality, not name matching)
  is more machinery than this first cut should take on. Gate the fast path to
  plain (`idx_expr_flags` false) columns only.
- **Partial indexes.** `idx_where_sql`/`idx_where_expr` — an index built with
  a `WHERE` clause does not contain an entry for every row, so a `COUNT(*)`
  or `MIN` restricted to *that* index's entries is not the same as the
  unrestricted aggregate the SQL asked for, unless the query's own WHERE
  clause happens to imply the index's partial condition (which
  `access_path_for_eqs` may or may not already require to choose the index
  at all — check whether a partial index can even be chosen as the access
  path for an unconstrained-beyond-the-partial-condition query before
  worrying about this; if `choose_access_path` already refuses to pick a
  partial index unless the query's WHERE subsumes it, this hazard is already
  closed by the existing access-path selection and needs no new gate here —
  confirm during implementation rather than assuming either way).
- **GENERATED columns.** The "plain column" gate above (`idx_expr_flags`
  false) is not the same test as "not a GENERATED column". `Sema.bind_create_index`
  (`lib/sql/sema.ml:3742-3792`, "Phase 35 Task 2") accepts `CREATE INDEX` on a
  VIRTUAL generated column, and a plain reference to one binds through the
  same `Ast.E_col`/`E_tbl_col` case as an ordinary column
  (`sema.ml:3788-3790`), which is exactly what sets `col_expr_flags = false`.
  So a NOT NULL VIRTUAL generated column indexed by name passes this design's
  plain-column gate even though, per #567/#629, a VIRTUAL cell's *row-store*
  cell is `V_null` by construction — its real value only exists via
  `Exec.decode_with_virtual`/`with_computed_virtuals` recomputation. This
  design's whole mechanism is to read `Index_key.decode`'d bytes and skip
  `rh_get`+decode entirely, so it needs its own answer for where a VIRTUAL
  column's *index-stored* value comes from and whether it agrees with what
  the general (row-decoding) path would return — #629 is precisely a bug
  where a check ran against the wrong copy of a VIRTUAL column's value, so
  this is not a hypothetical concern to wave off.

  **STORED generated columns need no special handling.** `compute_stored_generated_cols`
  materializes the computed value into the row's own cell before the row (and
  its index entries) are written, at every row-store write site (`execute_insert`,
  `execute_upsert_update`, `update_col_in_tx`, `apply_update_row` — see
  `CLAUDE.md`'s #629 section). By the time index-key extraction runs, a STORED
  column's cell holds a real value exactly like any other column's; the
  covering fast path needs no STORED-specific gate.

  **VIRTUAL generated columns are included in v1, on an explicit equivalence
  argument, not excluded.** Read the write paths directly rather than
  inferring from the expression-index gate:
  - `execute_create_index`'s backfill (`exec.ml:4589`) computes the row for
    key extraction via `decode_with_virtual_cols`, which is *the same function*
    `decode_with_virtual` uses to serve an ordinary row read — not merely an
    analogous one.
  - The ongoing INSERT/UPDATE index-maintenance sites (`exec.ml:3889-3890`,
    `4380`, `4662`, `4801`, `6067`) compute the row for key extraction via
    `with_computed_virtuals`/`with_computed_virtuals_cols`, which call the
    identical underlying `compute_virtual_generated_cols[_cols]` that
    `decode_with_virtual` calls.

  So the value written into a VIRTUAL column's index entry and the value
  `decode_with_virtual` would recompute for that same row are produced by
  *the same function evaluated against the same base-column values* — not
  independently-derived numbers that happen to agree today. They can diverge
  only if the base-column values feeding the expression change on disk
  without the index being re-keyed at the same time, and the engine's write
  path never does that: `write_row_rekeyed`'s index loop deletes the old key
  and inserts the new one in the same transaction as the row-store write, for
  every write path that can touch a base column a VIRTUAL expression reads.
  That is the same "index and row store move together" invariant the rest of
  this design already leans on (e.g. the #262 visibility hazard above), not a
  new one. The one caveat worth stating rather than silently assuming: this
  equivalence requires the generated expression to be a *pure* function of
  the row's own columns. Granary does not currently reject a non-deterministic
  expression (e.g. one calling `random()`) in a GENERATED column definition;
  such a column is already unstable under the general path (`decode_with_virtual`
  recomputes it fresh on every read, so two ordinary `SELECT`s can already
  disagree), and the covering fast path would freeze it to its write-time
  value instead — a different kind of wrong answer, but not a new hazard this
  design introduces, since the column was already non-deterministic before
  this design existed. Treat "GENERATED expressions are deterministic" as an
  existing, implicit assumption of the whole generated-column feature, not a
  new one this design is signing up for.

  **Decision: both STORED and VIRTUAL generated columns are eligible for the
  covering fast path in v1**, gated the same way as any other column (plain
  reference, `idx_expr_flags` false, `not_null`, all-in-index-prefix). No
  `table_meta.columns` stored/generated exclusion is needed. Pin the VIRTUAL
  case explicitly in the test plan below rather than relying on the general
  column tests to exercise it incidentally.
- **Columnstore scoping.** This design is implicitly row-store-only
  throughout (`table_meta`, `rh_get`, `idx_tree` as a B-tree index), and that
  is correct by construction, not by omission: `Sema.bind_create_index`
  refuses `CREATE INDEX` outright on a columnar table
  (`Cat.is_columnar meta` check, `sema.ml:3750-3755`), so no secondary index
  tree — and therefore no `Op_index_lookup` — can ever exist over a
  `USING COLUMNSTORE` table. `planner.ml`'s columnstore access paths
  (`Op_col_seq_scan` etc.) never produce `Op_index_lookup`, and there is no
  path by which one could reach a columnar table's tree id. This design's
  scope needs no explicit columnstore exclusion because the shape it targets
  is unreachable there.

## Suggested test plan

Correctness (mirroring `test_limit_early_stop_677.ml`'s structure):

- `MIN`/`MAX` over an indexed NOT NULL column, with and without a residual
  predicate that reads only index columns vs. one that reads a non-index
  column (forces fallback) — assert identical results to the pre-fix
  behavior (disable the fast path via whatever env/flag mirrors
  `GRANARY_AGG_FASTPATH=0`, per `CLAUDE.md:185`'s existing knob, and diff).
- `COUNT(*)` and `COUNT(col)` over an index lookup, including an empty
  result set (zero matching entries — must return 0/NULL correctly, not
  crash on an unseeded accumulator).
- `COUNT(DISTINCT col)` over an index column.
- A nullable indexed column: confirm the fast path is **not** taken (fall
  back to general path) and results still match — this is the regression
  test for the NULL/NaN hazard above. Include a NaN value in that column and
  confirm it's counted/compared correctly through the fallback.
- A `range` (#517) present alongside `MIN`/`MAX`: confirm fallback to general
  path per the scoping decision above, with correct results.
- `EXISTS` over an index lookup, with 0 and >0 matching entries.
- Expression-index and partial-index tables: confirm fallback (once the
  partial-index question above is resolved one way or the other).
- A NOT NULL VIRTUAL generated column, indexed by name (`CREATE INDEX ON t(v)`
  where `v` is `... GENERATED ALWAYS AS (expr) VIRTUAL NOT NULL`), exercised
  through `MIN`, `MAX`, `COUNT(*)`, `COUNT(v)` and `EXISTS` — assert the fast
  path is taken (it passes the plain-column gate per the GENERATED-columns
  hazard above) and that results are identical with the fast path enabled
  vs. `GRANARY_AGG_FASTPATH=0`, the same differential technique already used
  for the NOT-NULL/NaN case. Include a case where a prior `UPDATE` changed a
  base column the VIRTUAL expression reads, to exercise the "index and
  row-store move together" claim rather than only testing freshly-inserted
  rows. Also cover a STORED generated column indexed by name, for symmetry,
  though no divergence is expected there.

Perf regression, `Db.query_with_stats`-based, same shape as
`index_lookup_stops_early` in `test_limit_early_stop_677.ml` and the
existing `test_index_entries_546.ml` pattern:

- Seed a table+index at TPC-C-district scale (e.g. 900 rows in-prefix, ~9000
  total, matching #677's own numbers), run `SELECT MIN(no_o_id) FROM t WHERE
  <two-column prefix>`, and assert `rows_examined` (table fetches) is 0 or
  bounded to O(1) — not 900. `index_entries` may legitimately still be 900
  for `COUNT(*)`/unbounded `MIN` search if no reverse-seek primitive lands
  (every entry still gets walked, just not fetched) — assert on
  `rows_examined` specifically, and treat `index_entries` reduction for
  `MIN`/`MAX` as a stretch goal contingent on the reverse-seek primitive
  from item 2 above, not a hard requirement of the first cut.
- Wire into the existing `GRANARY_BENCH_*`-style neutralizer convention if
  the assertion turns out to have wall-clock variance — `rows_examined` is a
  count, not a timing, so it likely does not need one (same reasoning as
  `test_correlated_exists_493`'s `active_reader_count`), but confirm during
  implementation which counters are true counts vs. sampled/timed.

## Open questions for whoever implements this

1. Does `S.t`'s cursor API already expose a reverse seek/step (`seek_le` +
   `seek_prev`, or similar), or does `MAX` need one added? This determines
   whether `MAX`'s "one seek, one entry" claim is actually O(1) or needs new
   storage-layer surface.
2. Confirm the partial-index access-path-selection question above by reading
   `access_path_for_eqs`/`choose_access_path` rather than assuming.
3. Whether `agg_tree_id`/`Cat.row_storage`-style plumbing that
   `run_aggregate_fast_path` uses for the table tree has an equivalent
   accessor for "the index's storage tree id and its full column list
   together" (today `Op_index_lookup` only carries `idx_tree` as a bare int,
   not an `index_info`) — this may want the planner to attach more of
   `Cat.index_info` to `Op_index_lookup` at plan time, rather than having the
   exec-time fast path re-look-up the index by tree id.
