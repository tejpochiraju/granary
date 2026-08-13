# `Btree.max_key` / `Store.max_key` — O(log n) rightmost descent for the rowid recompute

Issue: #716 item 1. Refs #714 (the measurement), #293/#299 (why the recompute
exists), #706 (the CAS this does not touch), #589 (the failure mode a wrong
`max_key` would reproduce).

Date: 2026-08-12

## The problem

`Cat.recompute_rowid_counters_after_rollback` calls `recover_next_rowid` once
per table whose rowid counter the rolled-back transaction bumped, to re-derive
`max(rowid) + 1` from the data tree. A TPC-C NewOrder bumps three
(`orders`, `new_order`, `order_line`), and at W=1 `order_line` holds ~300k rows.

Measured (#714, `bench/results/2026-08-11-tpcc-stmt-profile-granary.csv`,
W=1, one terminal, 10 s): **4 `ROLLBACK` calls — 1.2% of transactions — take
25.72% of all NewOrder service time, at 358.916 ms each**, against a `new_order`
p50 of 9.8 ms. They are visibly the run's p99 (345.5 ms) and max (381.5 ms).

### The cost is worse than "an O(n) walk"

#716 describes the mechanism as a full forward scan to find the maximum key.
That is what `recover_next_rowid` (`lib/catalog/catalog.ml:2062-2080`) reads
like, but the dominant cost is one level down: **`Store.cursor_open` materialises
the entire tree into an OCaml list before the caller sees a single entry.**
`drain_btree_cursor` (`lib/store/store.ml:2902`) loops `Btree.cursor_next` into
an accumulator, and both the `Ro` and `Rw` Btree arms call it
(`store.ml:2940`, `:2964`). So the scan allocates ~300k `(bytes * bytes)`
tuples — keys *and* values, none of which the caller reads — and only then walks
the list.

The streaming path added by #233/#481 (`S.seek_ge` / `S.seek_next`) exists and
avoids the drain, but neither of these call sites uses it. Streaming would fix
the allocation and leave the traversal O(n); a rightmost descent fixes both.

### Two call sites, not one

`Cat.max_rowid_in_txn` (`lib/catalog/catalog.ml:2850-2877`) is the same
drain-and-walk over a data tree, for the `sqlite_sequence` lower-clamp path. Its
comment already names the missing primitive:

> The store has no `cursor_last`/`cursor_prev`, so this reuses the forward walk
> from `recover_next_rowid`.

Both call sites want one thing: the tree's maximum key.

## Scope decision: `max_key`, not `cursor_last` / `cursor_prev`

#716 proposes adding `S.cursor_last` and `cursor_prev`. That is a reverse
*traversal* API, and neither call site traverses — each reads exactly one key
and stops. Backward traversal needs either prev-leaf links (which the page
format does not carry) or a persistent descent stack, and nothing in the tree
needs it today; reverse index walks / `ORDER BY DESC` (#681/#682) would be its
first real consumer, and that is a separate design.

This spec adds a single value at each layer:

```
val Btree.max_key : t -> (bytes option, error) result Lwt.t
val Store.max_key : _ txn -> tree_id -> bytes option Lwt.t
```

## The correctness hazard: an empty rightmost leaf

`Btree.rightmost_append_cursor` (`lib/storage/btree.ml:1251`) already descends
the rightmost spine, and `append_cursor_max_key` (`:1305`) already returns the
tree's rightmost key. Reusing it looks like the whole fix, and it is wrong.

It answers `None` in **two** distinct situations that it does not distinguish:

1. `t.root_page = 0L` — the tree is genuinely empty (`btree.ml:1252-1253`);
2. the rightmost leaf has `n_keys = 0` (`:1267-1268`) — which says nothing about
   the leaves to its left.

Case 2 is reachable. `btree.ml` contains **no merge, rebalance or underflow
handling at all**; `del_from_leaf` (`:1329`) collapses the tree to
`root_page = 0L` only for a single empty *root* leaf (`:1358`, guarded by
`path = []`), and otherwise rewrites the leaf in place and leaves it linked in
its parent at zero keys (`:1363-1364`). So deleting a table's highest-keyed rows
— once the tree has split into two or more leaves — leaves an empty rightmost
leaf above live data.

Conflating the two is a silent-corruption bug, not a slow path:
`recover_next_rowid` would read `None` as "empty tree", return
`empty_next_rowid`, and the next insert would seed at rowid 1 and overwrite live
rows — **#589's symptom reached by a new route**.

`rightmost_append_cursor` itself is correct and is **not changed**. Its `None`
means "no append fast path available", the caller falls back to the general
path, and nothing is lost. The defect only appears when `None` is reinterpreted
as "no keys exist".

### Resolution: right-to-left descent

`Btree.max_key` descends from the root, and at a branch tries children
right-to-left, taking the first subtree that yields a key:

```ocaml
let rec max_key_of pid =
  read pid with
  | Leaf   -> if n_keys = 0 then None else Some (last key in page)
  | Branch -> first_some (map max_key_of (right_page :: rev separators' left_child))
```

Normally this touches one page per level and returns from the rightmost leaf —
O(log n). It degrades only by the number of *empty pages* it has to skip, never
to a full entry scan, and it cannot mistake an empty leaf for an empty tree.

## Components

### 1. `Btree.max_key : t -> (bytes option, error) result Lwt.t`

- Pages are read through `Pager.read_borrow ?snapshot_frames:t.snapshot_frames
  ?pin_set:t.pin_set` — the identical call `rightmost_append_cursor` makes, so
  snapshot isolation and pin accounting are inherited rather than re-derived.
- `root_page = 0L` → `Ok None`.
- `Leaf`: `n_keys = 0` → `None`; otherwise the last key, located by the same
  in-page forward entry scan `rightmost_append_cursor` uses (`btree.ml:1271-1281`).
  That scan is bounded by entries *per page*, not by table size.
- `Branch`: `decode_branch_entries` (`:323`), then children in right-to-left
  order — `common.right_page` first, then each entry's `left_child` in reverse.
- A non-tree page keeps the existing `Tree_corrupt "non-tree page in tree"`
  error.

### 2. `Store.max_key : _ txn -> tree_id -> bytes option Lwt.t`

Mirrors `cursor_open`'s four-arm structure (`store.ml:2914-2967`) so the
snapshot rules of the two cannot drift apart:

| arm | source |
|---|---|
| `Ro` + `Mem` | `rs_mem_snap` via `mem_tree_snap` → `Bytes_map.max_binding_opt` |
| `Rw` + `Mem` | RW shadow via `shadow_get`, else `mem_tree` → `Bytes_map.max_binding_opt` |
| `Ro` + `Btree` | `bt_get_tree_ro` → `Btree.max_key` |
| `Rw` + `Btree` | `bt_get_tree` → `Btree.max_key` |

The `Ro` + `Mem` arm must read the snapshot captured at `ro_begin`, not the live
tree: #178's note on `cursor_open` (`store.ml:2920-2923`) applies unchanged —
otherwise a concurrent writer's uncommitted rows leak in and survive its
rollback. Errors map through `unwrap_error` / `map_btree_err` as its neighbours do.

`Bytes_map` is `Map.Make (Bytes)` (`store.ml:25`), so `max_binding_opt` is the
map's own O(log n) rightmost descent — the Mem arm is not a scan either.

### 3. Call sites

**`Cat.recover_next_rowid`** (`catalog.ml:2062-2080`): the
`cursor_open` / `cursor_first` / `walk` / `cursor_close` block becomes one
`S.max_key` call inside the same `S.with_ro`. Same `Rowid.decode k + 1`, same
`empty_next_rowid` on `None`.

**`Cat.max_rowid_in_txn`** (`catalog.ml:2861-2876`): same substitution, on the
caller's existing txn. Same `Rowid.decode`, same `None` on empty. Its comment
(`:2846-2848`) asserts the store has no such primitive and becomes false — it is
rewritten, not left in place.

The header comment on `recompute_rowid_counters_after_rollback`
(`catalog.ml:2356-2361`) argues for restricting the recompute to the bumped set
because "`recover_next_rowid` is an O(n) tree walk". The restriction stays
correct and stays; the premise sentence is updated to match what the function now
costs.

### Explicitly unchanged

- The #706 compare-and-swap publish, and its `expected` capture point.
- The #299 AUTOINCREMENT branch (`read_committed_next_rowid`), which does not
  scan at all.
- The `rowid_bumped` restriction and `Lwt_list.filter_map_s`'s sequencing.
- `rightmost_append_cursor` and the append fast path.
- Every other `S.cursor_open` caller. The drain is a real cost at ~20 other
  sites, but converting them is #233/#481's line of work, not this one.

This change makes each recompute scan cheap. It does not change which tables are
scanned, when, or how the result is published.

## Testing

All new tests live in one new file, `test/test_max_key_716.ml`, so the design's
motivating hazard and its coverage stay findable together. Unit cases:

1. **Empty rightmost leaf** — the regression that motivates the design. Insert
   enough rows to split into ≥2 leaves, delete the tail rows so the rightmost
   leaf is empty, assert `max_key` returns the surviving maximum. A descent that
   reused `rightmost_append_cursor` fails here.
2. Empty tree (`root_page = 0L`) → `None`.
3. Single root leaf, populated → its last key.
4. All keys deleted → `None`.
5. Multiple consecutive empty rightmost leaves (delete a long tail) → still the
   surviving maximum.
6. Negative rowids — `Rowid.encode`'s offset-binary ordering means byte order is
   rowid order; a table of only negative rowids must return the correct maximum.
7. Both backends: every case runs against `Mem` and `Btree`.

Property (QCheck): over a random sequence of inserts and deletes, `max_key`
equals the last key of a full forward `cursor_open` scan. This pins the new path
against the old one directly.

Integration:

8. `recover_next_rowid` after a rollback that inserted rows — counter reverts to
   the committed `max(rowid) + 1`, unchanged behaviour.
9. Same, on a table whose tail rows were previously deleted and committed — the
   counter must not collapse to `empty_next_rowid` (test 1's hazard, end to end).
10. `max_rowid_in_txn`'s `sqlite_sequence` lower-clamp path still clamps.

Existing coverage that must stay green:
`test/test_rowid_counter_ownership_632.ml`,
`test/test_rollback_recompute_publish_race_706.ml`,
`test/test_worker_handle_589.ml`, and `test_catalog.ml`'s mirror-recovery tests.

## Measurement

Before/after wall-clock of a `ROLLBACK` that bumped a rowid counter on a table
with ~300k rows, taken on both sides of the change. Reported in the PR body and
on #716. **Not a CI timing gate** — no `GRANARY_BENCH_*` knob, no assertion; the
gates in `CLAUDE.md` are the complete set and this adds none.

The full #714 TPC-C profile re-run (#716 item 3) is deliberately a **follow-up**:
it is a 10 s benchmark run plus a `docs/benchmarks/BENCHMARKS-TPCC.md` rewrite,
and folding it in here would put the engine change and the doc revision in one
review.

## Expected effect, and its ceiling

Three ~O(n) drains per rollback become three root-to-leaf descents. On the #714
profile that is 25.72% of NewOrder service time — 1435 ms of a 10 s run — very
nearly all of which should disappear.

That does **not** convert one-for-one into throughput, and the PR must not claim
it does. The #714 profiler records *service time*; the in-lock / out-of-lock
split is unmeasured (#716's own caveat), and the recompute runs **after**
`S.rollback` has released the writer lock (`catalog.ml:2352-2354`, #706's note).
So the direct win is latency — the `new_order` p99/max outliers — plus whatever
the removed ~300k-tuple allocation returns in GC pressure. At `TERMINALS > 1` it
additionally stops a rolling-back terminal from occupying a scheduler slot for
~359 ms while siblings wait, but quantifying that needs the profile re-run.
