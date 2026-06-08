# Same-Txn Dirty Page Reuse (#297)

**Date:** 2026-06-08 · **Issue:** #297 · **Status:** Design (approved)

## Problem

Every B-tree mutation follows a strict copy-on-write (CoW) pattern:

1. `Pager.free` the old page (stamped with `current_txn_id`)
2. `Pager.alloc` a new page — either from the freelist or file extension
3. `Pager.write_owned` with the new content

The freelist guard at `Freelist.pop` requires `freed_at_txn_id < min_safe_txn_id`. At `rw_begin`, `alloc_min_safe` is set to `current_rw_txn_id` (when no reader is active). Since freed pages are also stamped `current_rw_txn_id`, the guard evaluates to `current_txn_id < current_txn_id = false`. Every `alloc` therefore extends the file, causing the database to grow linearly with the number of mutations within a single transaction, regardless of total data size.

**Observed impact:** Batch insert (5000 rows in 1 txn) is ~44× slower than SQLite on NVMe. The CoW write-amplification is the dominant constant factor (#231).

## Design

### Core change: relax the freelist guard at `rw_begin`

**File:** `lib/store/store.ml` line 1152

```ocaml
(* Before *)
| None -> current_rw_txn_id

(* After *)
| None -> Int64.succ current_rw_txn_id
```

When no reader is active, `alloc_min_safe` becomes `current_txn_id + 1`. The guard `freed_at_txn_id < alloc_min_safe` now evaluates to `current_txn_id < current_txn_id + 1 = true`, allowing pages freed in the current transaction to be reused by subsequent `alloc` calls.

### Safety analysis

**Writer path** — `Pager.read` checks `t.dirty` first (writer/no-snapshot path). When a page is freed (remains in `t.dirty` with old content) and then re-allocated from the freelist, the subsequent `write_owned` calls `Hashtbl.replace t.dirty page_id buf`, overwriting the stale in-txn content with the new content. The writer always sees the correct state.

**Snapshot readers** — The snapshot read path (`read ?snapshot_frames`) never consults `t.dirty`. It resolves pages via WAL frames ≤ the snapshot bound, then the shared cache, then the block device. A reused page_id's old committed content is in the cache (if it was a pre-txn allocation) or never existed (if allocated this txn). In either case, snapshot readers see committed pre-txn state, which is the correct behavior.

**Rollback** — Rollback restores the freelist snapshot taken at `rw_begin` (`lib/store/store.ml:1796-1800`), undoing all in-txn free/reuse operations. `clear_dirty` discards all in-txn content. The page_id returns to whatever state it had in the freelist at `rw_begin`. No correctness issue.

**Savepoints** — Savepoint creation snapshots the freelist (same mechanism as `rw_begin`). Savepoint rollback restores the freelist + `n_pages` + dirty set via `dirty_restore`. Pages freed-and-reused within the savepoint window are correctly undone. The existing code at `store.ml:2108` documents the "harmless storage leak" of orphan pages from file extension — this is unchanged.

**File truncation on rollback** — Pages allocated by file extension during the txn and then freed, reused, and later rolled back become orphans in the file. This is the same pattern as today (harmless leak, bounded by the aborted txn's write volume). No change in behavior.

**Freelist ordering** — `Freelist.pop` uses `Txn_map.min_binding` which returns the oldest-txn freed page. Pages freed at `current_txn_id` are the newest entries. If pages from an older txn_id exist in the freelist, they are returned first. The freed-at-current pages are returned only after older entries are exhausted. This is correct: older pages are safer to reuse (fewer active readers reference them).

**Int64 overflow** — `Int64.succ` on `Int64.max_int` wraps to `Int64.min_int`, which would block all freelist reuse. In practice the txn counter never approaches `2^63-1`, so this is a theoretical concern with no real impact.

### Not in scope (future work)

**Btree-level in-place rewrite** — After verifying the improvement from the guard relaxation, a further optimization would detect when a page being replaced was allocated in the current txn (either via file extension or freelist reuse) and skip the free+alloc cycle entirely, rewriting the page in-place. This requires adding a `txn_owned` tracking mechanism to the pager and modifying `btree.ml`'s write path. Deferred to a follow-up issue.

## Testing

### New tests

1. **`test_pager.ml`**: Free page X at txn N, alloc with no active reader — assert the alloc returns page X (not a fresh page from file extension).

2. **`test_store_btree.ml`**: In a single RW txn, insert K rows and verify `n_pages` growth is bounded by tree depth, not K × depth.

3. **`test_write_alloc.ml`**: Extend `test_insert_allocation_bounded` to measure within-txn page growth (file-size before/after multi-insert in one txn).

### Existing regression tests

All existing tests must pass without changes:
- `test_freelist.ml` (freelist unit tests — `test_pop_equal_not_reusable` tests the `freed_at < min_safe` guard, which still holds for cross-txn reuse)
- `test_pager.ml` (`test_free_then_alloc_same_txn` will need to be updated to expect page 3 instead of page 5 from file extension)
- `test_overflow.ml` (overflow chain free+reuse patterns)
- `test_store_btree.ml` (savepoint tests, concurrent reader tests)

## Benchmarks

The batch-insert workload (`insert_batch`, 5000 rows in 1 txn, currently ~44× SQLite) should improve meaningfully due to eliminated file-extension overhead per row. Cross-txn workloads (autocommit insert, point lookup, range scan) are unaffected.