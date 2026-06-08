# Same-Txn Dirty Page Reuse (#297)

**Date:** 2026-06-08 · **Issue:** #297 · **Status:** Design (approved)

## Problem

Every B-tree mutation follows a strict copy-on-write (CoW) pattern:

1. `Pager.free` the old page (stamped with `current_txn_id`)
2. `Pager.alloc` a new page — either from the freelist or file extension
3. `Pager.write_owned` with the new content

The freelist guard at `Freelist.pop` requires `freed_at_txn_id < min_safe_txn_id`. At `rw_begin`, `alloc_min_safe` is set to `current_rw_txn_id` (when no reader is active). Since freed pages are also stamped `current_rw_txn_id`, the guard evaluates to `current_txn_id < current_txn_id = false`. Every `alloc` therefore extends the file, causing the database to grow linearly with the number of mutations within a single transaction, regardless of total data size.

**Observed impact:** Batch insert (5000 rows in 1 txn) is ~44× slower than SQLite on NVMe. The CoW write-amplification is the dominant constant factor (#231).

## Design (final implementation)

### Core change: txn-owned page pool

Instead of relaxing the freelist guard (which caused cursor corruption during
mid-scan mutations), the implementation adds a **txn-owned page pool** to the
pager:

1. At `rw_begin`, capture `n_pages_at_rw_begin` — the page count at txn start.
2. Pages allocated by file extension above this threshold are "txn-owned".
3. When freed, txn-owned pages go to a separate `txn_owned_pool` rather than
   the main freelist.
4. `Pager.alloc` checks the txn-owned pool first, then the main freelist.
5. Pages from the committed tree are freed to the main freelist as before
   (only reusable by future transactions).

This avoids cursor snapshot corruption: in-flight cursors reference committed-tree
page IDs, which are NEVER reused within the txn. Only newly-allocated page IDs
(which no cursor could reference) are recycled.

### Safety analysis

**Writer path** — When a txn-owned page is freed and immediately reused,
`write_owned` calls `Hashtbl.replace t.dirty page_id buf`, overwriting the
stale in-txn content with the new content. The writer always sees the correct
state.

**Snapshot readers** — The snapshot read path never consults `t.dirty`. It
resolves pages via WAL frames ≤ the snapshot bound. Committed-tree pages are
never reused within the txn, so snapshot readers see consistent pre-txn state.
Txn-owned pages didn't exist at ro_begin time so no reader references them.

**Rollback** — `clear_dirty` resets the txn-owned pool (txn-owned pages were
never part of a committed tree). The main freelist is restored from the
rw_begin snapshot. No correctness issue.

**Savepoints** — Savepoint creation snapshots the txn-owned pool alongside
the dirty set and freelist. Savepoint rollback restores all three. The existing
code documents the "harmless storage leak" of orphan pages — unchanged.

**In-flight cursors** — The txn-owned approach is correct because cursors
hold page references to committed-tree pages. Those pages go to the main
freelist (same-txn reuse blocked by the `freed_at < min_safe` guard). Only
file-extension page IDs enter the txn-owned pool, and no cursor could hold
them (they didn't exist at cursor-open time).

### Int64 overflow — not applicable (the approach uses a separate pool, not
arithmetic on txn IDs).

## Testing

### New tests

1. **`test_store_btree.ml`**: `test_same_txn_page_reuse_bounded` — insert 200
   rows in a single txn and verify `n_pages` growth is < 100 (bounded by tree
   depth × splits, not N × depth).

2. **`test_btree.ml`**: `test_root_page_changes` updated — instead of asserting
   the root page changes on every put, verify both keys are readable (the root
   may stay the same when txn-owned pages are reused in place).

### Existing regression tests

All existing tests pass without changes:
- `test_freelist.ml` — freelist unit tests unchanged (main freelist guard unaffected)
- `test_pager.ml` — unchanged (freelist guard `freed_at < min_safe` still holds; txn-owned pool is a separate mechanism)
- `test_overflow.ml` — unchanged
- `test_store_btree.ml` — unchanged (savepoint, concurrent reader tests unaffected)

## Benchmarks

The batch-insert workload (`insert_batch`, 5000 rows in 1 txn, currently ~44× SQLite) should improve meaningfully due to eliminated file-extension overhead per row. Cross-txn workloads (autocommit insert, point lookup, range scan) are unaffected.