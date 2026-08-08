# #674 (3 of 3): sort elision when an access path already satisfies ORDER BY

Split from #677 (which itself split from #674's Delivery investigation). This
spec covers only item (3): `EXPLAIN SELECT no_o_id FROM new_order WHERE
no_w_id = 1 AND no_d_id = 5 ORDER BY no_o_id LIMIT 1` plans `Limit(1) <-
Project <- Sort <- IndexLookup(new_order)` even though the composite index
`(no_w_id, no_d_id, no_o_id)` already returns this district's rows in
`no_o_id` order. Item (1) (covering-index shortcut for MIN/MAX/COUNT) stays
open under #677. Item (2) (`Op_limit` early-stop) shipped separately on
`fix/677-limit-early-stop` and is not touched here — this design assumes it
lands, and one of its correctness hazards is exactly the composition of the
two.

**This document is design only. No implementation.**

## Problem

`Op_sort` (`lib/sql/exec.ml`, `stream_sort` — reached from the `Op_sort`
case of `to_stream`) materializes its entire child stream before it can
produce its first output row: it has to see every row before it knows which
one sorts first. So even after #677 makes `Op_limit` stop pulling its own
child early, an `Op_sort` sitting between `Op_limit` and the access path
defeats it completely — `Op_limit` asks for 1 row, `Op_sort` answers only
after pulling all 900.

The seek itself is already correct (#508's composite-key prefix seek): for
`WHERE no_w_id = 1 AND no_d_id = 5`, `access_path_for_eqs`
(`lib/sql/planner.ml:571`) walks the index forward from the encoded
`(1, 5)` prefix and returns exactly the 900 rows in that district, in
ascending index-key order — which for the trailing column `no_o_id` (nothing
else varies within the seek) is exactly `ORDER BY no_o_id ASC`. The planner
just never asks whether the access path it already chose made the `Op_sort`
redundant.

## Where `Op_sort` gets inserted today

Two independent insertion points in `lib/sql/planner.ml`, and they matter
differently to this fix:

- **Non-aggregated `SELECT`** (`plan_select`, `make_sort` at line 2175):
  `Op_sort { keys; child = after_window }`, inserted **below** the
  projection (`after_sort` feeds `plan_projection`). This is the shape the
  issue's `EXPLAIN` shows and the only one this design targets.
- **Post-aggregation `SELECT`** (`plan_post_agg_sort`, line 2037) and
  **compound queries** (`plan_compound`, line 2690): both sort a fully
  materialized intermediate (aggregate output, or a UNION/INTERSECT/EXCEPT
  result) that carries no access-path ordering information at all — there is
  nothing to elide there, and this design does not touch them.
- `Op_update`/`Op_delete` carry their own `order`/`limit`/`offset` fields
  (`plan_update`/`plan_delete`) but — checked against `lib/sql/exec.ml` —
  their DML execution paths do not currently build an `Op_sort` at all for
  small `ORDER BY ... LIMIT` write shapes; that machinery is out of scope
  here and unaffected by this change either way.

So the only insertion point this design proposes to skip is `make_sort` in
`plan_select`, and only when its child, `after_window`, is (transitively) a
`plan_base` result with no join, no window, and no aggregation between it
and the sort.

## What "natural order" means here, and why it's derivable without new plan-op fields

`Op_index_lookup.keys` is `(int * Row.ty * expr) list` — the *table*-column
ordinals of the equality-pinned prefix, in **index-column order**
(`plan.mli:233`). Critically, `find_index_for_eqs` / `index_is_seekable`
(`planner.ml:145`, `:357`) only ever pick an index that is **plain-column**
(`idx_expr_flags` all false) and **non-partial** (`idx_where_sql = None`) as
a `Seek_index` access path — an expression index or a `WHERE`-filtered index
is never returned as a seek at all. So every `Op_index_lookup` this design
will ever see already satisfies "every index column maps cleanly back to a
table column ordinal," with no new guard needed.

That means the "natural order" of a chosen `Seek_index` access path is fully
recoverable after the fact, from information already on the plan node plus
one catalog lookup:

1. Look up the `Cat.index_info` for `idx_tree` (`Cat.indexes_for_table`,
   filtered on `idx_tree_id`).
2. `prefix_len = List.length keys` — how many leading `idx_columns` are
   pinned to a constant. Constant columns don't determine row order (every
   surviving row shares the same value there), so they contribute nothing to
   the *comparison* but also cannot break an ordering claim about what comes
   after them.
3. Map `idx_columns` (a `string list` of column names, since the index is
   plain-column) `[prefix_len ..]` to table-column ordinals via
   `table_meta.columns`, the same lookup `prefix_for_index`
   (`planner.ml:330`) already does internally — this is the **suffix
   ordinal sequence**, and the walk produces rows in ascending order on it
   (composite-key lexicographic order, standard B-tree semantics: the index
   key bytes are `encode(pinned cols) ++ encode(suffix cols) ++ rowid`, and
   `rh_seek_ge`/`seek_next` walks that byte order forward).
4. A `range` on `idx_columns[prefix_len]` (#517) narrows the *span* the walk
   covers but never changes the *order* — the walk is still ascending on the
   suffix sequence. This is exactly the case the issue's example does *not*
   need (it has two equalities, no range), but it should not accidentally
   become ineligible either.

`Op_rowid_lookup` is the other seekable shape and is even simpler: it
returns at most one row, so *any* `ORDER BY` over its output is trivially
already satisfied. Worth handling as a one-line special case (no need to
consult a suffix sequence at all).

`Op_seq_scan` walks the table B-tree in **rowid order**, which for a rowid
table with an `INTEGER PRIMARY KEY` alias column *is* ascending order on
that column — but nothing currently exposes "rowid order" as a queryable
column in the absence of an alias, and confirming the alias-column case
needs its own audit of how `stream_seq_scan` iterates. **Left out of v1**
(see Out of scope); the issue's own motivating case is the index path.

## Mechanism (proposed)

A pure function, no new `Plan.op` fields:

```ocaml
(* In planner.ml, near access_path_for_eqs / choose_access_path. *)

(* The column-ordinal sequence a chosen access path is guaranteed to produce
   in ascending order, or [None] if it makes no such guarantee.  [`All] means
   "every row is guaranteed sorted, trivially" (a single-row lookup); [`Cols
   ords] means "ascending on this ordinal sequence, in order, for as many
   columns as the caller needs." *)
let natural_order cat (op : Plan.op) : [ `All | `Cols of int list ] option =
  match op with
  | Plan.Op_rowid_lookup _ -> Some `All
  | Plan.Op_index_lookup { idx_tree; keys; table_meta; _ } ->
    (match
       Cat.indexes_for_table cat ~table:table_meta.Cat.name
       |> List.find_opt (fun (i : Cat.index_info) -> i.Cat.idx_tree_id = idx_tree)
     with
     | None -> None (* defensive; every Op_index_lookup came from a real index *)
     | Some idx ->
       let prefix_len = List.length keys in
       let suffix_names = List.filteri (fun i _ -> i >= prefix_len) idx.Cat.idx_columns in
       let col_ordinal name =
         List.find_index (fun (c : Row.column) -> c.Row.name = name) table_meta.Cat.columns
       in
       (* index_is_seekable already guarantees every name resolves; List.map
          rather than filter_map keeps that guarantee checkable instead of
          silently degrading the suffix if it ever stops holding. *)
       Some (`Cols (List.map (fun n -> Option.get (col_ordinal n)) suffix_names)))
  | _ -> None

(* Does [order]'s key sequence match a prefix of [natural_order]'s guarantee?
   Every key must be a bare column reference (no expression), ASC, and
   NULLS FIRST (explicit or defaulted) — see Correctness hazards for why
   both restrictions are load-bearing. *)
let order_satisfied_by_natural_order natural (order : Sema.bound_order_key list) =
  let key_ok (bkey : Sema.bound_order_key) ord =
    match bkey.Sema.key with
    | Sema.BE_col c ->
      c = ord
      && bkey.Sema.dir = Ast.Asc
      && (bkey.Sema.nulls = None || bkey.Sema.nulls = Some `Nulls_first)
    | _ -> false
  in
  match natural with
  | Some `All -> true
  | Some (`Cols ords) ->
    (try List.for_all2 key_ok order (List.filteri (fun i _ -> i < List.length order) ords)
     with Invalid_argument _ -> false (* order longer than the guaranteed suffix *))
  | None -> false
```

`plan_select`'s `make_sort` becomes:

```ocaml
let make_sort child =
  let keys = plan_sort_keys ~order ~windows ~n_input_cols in
  if keys = []
  then child
  else if
    (not has_joins)
    && windows = []
    && (match cat with
        | Some c -> order_satisfied_by_natural_order (natural_order c base) order
        | None -> false)
  then child (* elided: access path already produces this order *)
  else Plan.Op_sort { keys; child }
```

Note `natural_order` is checked against `base` (the raw access-path op,
before `plan_base` wraps it in a residual `Op_filter`), not against
`after_window` (`make_sort`'s actual argument) — `Op_filter` only *drops*
rows, it never reorders survivors, so the check is sound either way, but
checking `base` avoids writing a `natural_order` case for `Op_filter` at
all.

## Correctness hazards

- **DESC is categorically excluded, not a missing case.** `stream_index_lookup`
  (`exec.ml:10530`) only ever walks forward — `rh_seek_ge` then repeated
  `seek_next`. There is no reverse-walk primitive (`rh_seek_le` /
  `seek_prev`-equivalent) anywhere on this path. `key_ok` above hard-codes
  `dir = Ast.Asc`; an all-`DESC` `ORDER BY` cannot be elided until a reverse
  index walk exists, which is its own scoped piece of surgery (a new exec
  primitive plus its own reader-cleanup discipline, the same class of care
  #677 took for early-stop) and is explicitly **not** part of this design.

- **NULLS placement must match the index's own tie-break, not just direction.**
  The index encoding puts a NULL indexed value at byte `0x00`
  (`Index_key.encode_value`), which sorts first — matching the engine's
  **default** `NULLS FIRST` for `ASC` (`order_dir_nulls`, `planner.ml:1841`).
  An explicit `ORDER BY col ASC NULLS LAST` does *not* match what the index
  walk actually produces and must not be elided. `key_ok` checks this
  explicitly (`nulls = None || nulls = Some \`Nulls_first`) rather than
  assuming the default always applies.

- **A `range` on the first suffix column does not disqualify elision, and a
  test should say so explicitly** — it's tempting to assume "the seek isn't
  a pure equality prefix, so skip it," but `range_for_index` only moves the
  walk's start/end, never its order (composite-key comparison is still
  strictly ascending across the narrowed span). Conflating "narrowed" with
  "unordered" would silently forfeit the range-bounded case, which is common
  in TPC-C-shaped WHERE clauses (`WHERE w_id = ? AND d_id = ? AND o_id > ?
  ORDER BY o_id`).

- **A skipped-but-unconsumed leading column breaks the suffix, and this is
  the sharpest correctness trap.** `find_index_for_eqs`/`prefix_for_index`
  stop extending the equality prefix at the *first* index column with no
  matching `WHERE` equality (`prefix_for_index`, `planner.ml:330`) — they
  never skip a gap and pin a later column. So `keys`'s length is exactly
  "how many leading index columns are pinned," and `idx_columns[prefix_len]`
  is genuinely the first *unpinned* column, with no hidden gap to account
  for. This invariant is enforced entirely inside `prefix_for_index` — this
  design does not need to re-derive it, only must not silently start relying
  on `keys` being contiguous-but-possibly-sparse without re-checking that
  function if it ever changes.

- **`has_joins` must gate this off entirely for v1, not just get "handled
  conservatively."** A nested-loop join probes the right table once per left
  row and appends right columns — it does preserve left-row order — but a
  **hash join** materializes its build side into a hash table with no
  ordering guarantee at all, and *which* strategy `plan_join` picks is a
  cost-model decision (`nlj_probe_cost_ratio` et al.) made independently of
  whether an `ORDER BY` exists. Eliding a sort under a join would silently
  couple sort correctness to a cost-model heuristic that has never had to be
  order-preserving and is free to change. Excluding `has_joins` entirely
  removes that coupling rather than trying to characterize it.

- **`windows <> []` must also gate this off, for a different reason:**
  `stream_window` (`exec.ml:12566`) already fully materializes its child via
  `Lwt_stream.to_list` before it can compute any window function, so eliding
  the sort below it buys nothing (the drain still happens one level up) —
  excluding it is a no-cost simplification, not a missed opportunity.

- **Interaction with #677 (`Op_limit` early-stop) is a straight composition,
  not a new hazard, but it's the reason this fix matters.** With the sort
  elided, `plan_select`'s output for the issue's example collapses from
  `Limit(1) <- Project <- Sort <- IndexLookup` to `Limit(1) <- Project <-
  IndexLookup`. #677's early-stop machinery (the `stream_cleanup_key`
  registry) is keyed off which scanner is directly reachable under
  `Op_limit`'s child construction — it does not care how many operators sit
  between `Op_limit` and the scanner, only that the scanner is reached
  during that one `to_stream` call. `Op_project` is not one of the 3 lazy
  cursor-holding scanners #677 catalogued and does not need to register
  anything itself; it simply passes pulls through to its child. So the two
  fixes compose with no shared surface to keep in sync — #677 already ships
  independently of this, and this design does not need to touch
  `stream_cleanup_key` at all. (Verify this claim once #677 has landed on
  `main`, since this document was written while it was still on
  `fix/677-limit-early-stop`.)

- **`plan_dml_seek` (`Op_update`/`Op_delete`'s own small `order`) is
  unaffected, deliberately.** Checked against `exec.ml`: neither DML
  execution path currently builds an `Op_sort` node for its `order` field at
  all for the seekable case — that's a separate, likely-also-missing
  optimization, not something this change touches or could regress.

- **What "no catalog" (`plan_select_no_cat`) means for this feature.**
  `plan_select_no_cat` is the sub-catalog SELECT path (no index lookups at
  all, `chain_joins_no_cat`) — it has no `Seek_index`/`Seek_rowid` to reason
  about, so this optimization is inherently inapplicable there. The
  `~cat:None` branch of `make_sort`'s guard above should therefore just fall
  through to the unmodified `Op_sort` path (which is what the sketch already
  does), not be treated as a case to "fix" separately.

## Out of scope

- **DESC / reverse index walk** (see hazards above) — needs a new exec
  primitive, tracked as a follow-on if this lands and the DESC case turns
  out to matter in practice (TPC-C's own queries are all ASC).
- **`Op_seq_scan` / rowid-order elision** for `ORDER BY <rowid alias column>`
  with no `WHERE` — plausible, not audited here, and not part of #677's
  motivating case.
- **Joins** — excluded structurally (see hazard above), not partially
  handled.
- **Post-aggregation sort / compound-query sort** — no access-path ordering
  information reaches either insertion point; nothing to elide.
- **Item (1) from #677** (covering-index shortcut for MIN/MAX/COUNT so
  `stream_index_lookup` skips the `rh_get` table fetch entirely) — a
  different, deeper change, tracked on #677 itself. This design does not
  assume it; sort elision is worth doing on its own; combined with #677's
  `Op_limit` early-stop it already takes the issue's own example from
  scanning all 900 rows to reading exactly 1.

## Suggested test plan

Modeled on `test/test_limit_early_stop_677.ml`'s two-part shape (correctness
first, perf claim second, both in one file so a future change that breaks
either is caught by the same run):

- **Correctness — elision must never change results, only the plan.**
  - The issue's own case: `SELECT no_o_id FROM new_order WHERE no_w_id = ?
    AND no_d_id = ? ORDER BY no_o_id LIMIT 1` against a seeded multi-district
    table; compare row-for-row against the same query with elision
    hypothetically disabled (or simply: against a known-good expected row,
    since correctness doesn't depend on how the row was produced).
  - A **range-bounded** variant: `WHERE w_id = ? AND d_id = ? AND o_id > ?
    ORDER BY o_id` — pins the "range doesn't disqualify elision" hazard.
  - A **NULLS LAST** variant on an otherwise-eligible ASC key — must **not**
    elide; assert results still come out correctly sorted (correctness holds
    either way; this is really checking the plan didn't wrongly elide, via
    the rows-examined assertion below, not via row values which look
    identical whether or not elision fired).
  - A **DESC** variant on an otherwise-eligible key — must not elide, same
    reasoning.
  - A **joined** variant with an otherwise-eligible ORDER BY on the driving
    table — must not elide (guards the `has_joins` gate specifically, since
    it's the easiest one to accidentally loosen later).
  - `ORDER BY` naming more columns than the index suffix has (index has 3
    columns after the prefix, `ORDER BY` names 4) — must not elide; the last
    key has nothing to fall back on.
  - `ORDER BY` on an **expression**, not a bare column (`ORDER BY no_o_id +
    0`) — must not elide (`key_ok`'s `Sema.BE_col` match already refuses
    this, but it's cheap to pin explicitly since #579's mixed-type
    comparator bugs show expression-vs-column confusion is a recurring class
    of mistake in this codebase).

- **The perf claim — `Db.query_with_stats`, not wall-clock**, exactly
  #677's approach and for the same reason (CLAUDE.md's scaling-gate
  discipline: a deterministic count survives a loaded CI runner where a
  wall-clock ratio does not). Seed a table with many districts so an
  unelided plan would need to fully materialize `Op_sort`'s child; assert
  `rows_examined` stays near `offset + limit` (e.g. `<= 5` for a `LIMIT 1`
  over a district with hundreds of rows) rather than the district's full
  row count. This assertion is **expected RED until both this change and
  #677 have landed** — sort elision alone removes the `Op_sort` node but
  #677's early-stop is what actually makes `Op_limit` stop pulling early;
  without it, `Op_limit` still drains everything via `Lwt_stream.to_list`,
  just with one fewer intermediate buffer. Word this test's assertion and
  comment accordingly so a partial landing order doesn't read as a
  regression.

- **`EXPLAIN` shape test** — the most direct regression guard for the exact
  defect described: assert the planned tree for the issue's query is
  `Limit <- Project <- IndexLookup` (no `Op_sort` node), and that the
  DESC/joined/NULLS-LAST/range-exceeding variants above still show
  `Op_sort` in their plans. `Op_explain` already exists as a plan wrapper
  (`Plan.Op_explain`), so this is a string/structural match against its
  output rather than new plan-introspection machinery.

- No new `GRANARY_BENCH_*` gate is proposed: `rows_examined` is a
  deterministic per-row counter fed by `incr_examined`/`incr_index_entries`
  (`exec.ml:10588`/`:10596`), not a wall-clock measurement, so it can run
  armed on every PR the way `test_not_null_600`/`test_correlated_exists_493`
  do — no `bench-nightly.yml`-only treatment needed.
