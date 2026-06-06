# #283 — Route all catalog cache mutations through one ambient-aware wrapper

**Date:** 2026-06-05
**Issue:** tej/sqlite_ocaml_port#283 (architectural follow-up from PR #278 review)
**Subsumes:** the cache-undo design half of #279, #280, #282 (all already shipped as targeted fixes; this consolidates them)

## Problem

#269 added a schema-cache **undo log** so DDL run inside an explicit `BEGIN … COMMIT`
can be rolled back: a DDL statement mutates the in-memory catalog hashtables before
the store commits, so a `ROLLBACK` must reverse those mutations to keep the cache in
agreement with the rolled-back store.

The undo log is a hand-maintained parallel ledger. Every catalog mutation site must
*remember* to (a) thread the ambient `?txn`, (b) mutate the cache after the store
write, and (c) register the matching inverse closure. This is error-prone and already
asymmetric:

- `create_table`/`create_index`/`add_column`/`drop_column`/`rename_table`/
  `rename_column`/`create_fts_table` register undo **internally** in `catalog.ml`,
  each behind a `match txn with Some -> register_schema_undo … | None -> ()` guard.
- `drop_table`/`drop_index` register undo **externally** in `exec.ml`
  (`execute_drop_table` at `exec.ml:5301`, `execute_drop_index` at `exec.ml:5320`),
  via the `restore_table_cache`/`restore_index_cache` helpers.
- `register_tag` (the #174 tree-tag page-stamp) is a *second* piece of in-memory
  state that `add_column`/`drop_column`/`rename_column` must each remember to revert
  by hand inside their undo closures.
- The three cache hashtables (`cache`, `indexes`, `fts`) are public fields of
  `Catalog.t`, writable from anywhere — nothing structurally prevents a future
  mutation site from writing the cache and forgetting its undo.

Three txn-acquisition idioms also coexist (`exec.with_ddl_txn`, `acquire_txn` +
ad-hoc `Lwt.catch`, and raw `Cat.*`-opens-its-own-`rw_begin`), but those govern store
transactions; this spec targets the **cache-undo** half of the problem.

## Goal

Make "mutate the catalog cache without registering its reversal" **structurally
impossible** — enforced by the OCaml module system, not by convention or review.
Pure refactor: zero observable behavior change. Close the latent gaps the
consolidation reveals (the drop_table/drop_index internal-vs-external asymmetry; the
hand-written tree-tag reversals).

## Design

### 1. A signature-sealed `Schema_cache` module owning the cache + undo log

Introduce `module Schema_cache : sig … end = struct … end` **nested in `catalog.ml`**,
defined immediately after the `table_meta` / `idx_origin` / `index_info` /
`fts_table_meta` type declarations. Its struct privately owns:

- the three hashtables: `tables`, `indexes`, `fts`;
- the undo machinery currently on `Catalog.t`: `schema_undo`, `schema_savepoints`,
  `schema_txn_poisoned`, `rowid_bumped_in_txn`.

The ascribed signature exposes **only** reads, paired mutators, and lifecycle ops.
The hashtables never appear in the signature, so no code outside the module — even
elsewhere in `catalog.ml` — can write them. That is the type-level guarantee.

**Why nested module, not a separate `schema_cache.ml` file:** the cache types and
the store functor application (`S`) live in `catalog.ml`. A separate file would force
extracting all of them into a shared `catalog_types` module first — a much larger,
riskier diff for the *same* sealing guarantee. Signature ascription seals within a
file exactly as it does across one.

After the refactor `Catalog.t` holds a single `sc : Schema_cache.t` field in place of
the four moved fields (plus the unchanged `store`, `fk_enforcement`,
`recursive_triggers`, `defer_fks_pragma`, `pending_fk_checks`, `last_inserted_rowid`).

### 2. Auto-inverse mutators

Each mutator captures the **prior** binding and synthesizes its own inverse — callers
never write an undo closure again:

```
put_table    sc ~name meta   (* prior = find_opt name; replace; push inverse:
                                Some m -> replace m | None -> remove name *)
remove_table sc ~name        (* prior captured; remove; inverse restores prior *)
put_index / remove_index / put_fts / remove_fts   (* identical shape *)
```

The inverse is computed from prior state, so it is correct for create (prior=None →
inverse removes), drop (prior=Some → inverse restores), and replace-in-place
(add/drop/rename column → inverse restores old meta). Composite operations decompose
into primitive calls, each self-undoing: e.g. `rename_table` = `remove_table old` +
`put_table new` + per-index `put_index` back-reference updates.

The undo log representation is **unchanged**: a single growing `(unit -> unit) list`,
savepoint markers recorded as physical suffixes compared with `==`, poison flag
snapshotted alongside. Only ownership moves inside the module and the closures are now
machine-generated rather than hand-written. The `==` physical-suffix invariant is
preserved because the list still only grows by prepending.

**Tree-tag folding.** `put_table` also re-stamps the #174 tree-tag for the new meta;
its auto-inverse re-stamps the *prior* meta's tag (or, for a create whose prior=None,
the inverse simply removes the cache entry — the tag for an unreferenced rolled-back
tree_id is harmless per the existing #174 reasoning at `catalog.ml:1439-1445`). This
makes tag-consistency automatic and deletes the hand-written `register_tag` pairs in
`add_column`/`drop_column`/`rename_column`. `Schema_cache.create` takes a
`~stamp:(table_meta -> unit)` callback wired to `register_tag store` at catalog-init
time, so the module needs no textual dependency on `register_tag`'s later definition.

### 3. Two legitimate non-closure reversals, modeled explicitly

Both are *registered* reversal strategies, not raw writes — there is no unprotected
cache write anywhere.

- **Autocommit DDL** — the `?txn=None` branches that open and `commit` their own
  writer txn. The write is durable, so there is nothing to roll back. Modeled as
  `put_table_durable` / `remove_table_durable` / `put_index_durable` / etc.: apply +
  stamp, push **no** undo. Correct by construction because `?txn=None` ⟺ no ambient
  writer txn (a nested `rw_begin` would deadlock against the held writer lock
  otherwise), so the undo log is necessarily empty in this mode.
- **Rowid counter** — `next_rowid_in_txn` / `bump_next_rowid_in_txn` keep the #293
  recompute-on-rollback strategy (re-derive `max(rowid)+1` from the rolled-back tree
  for only the bumped tables, rather than snapshotting per-DML counter deltas).
  Modeled as `bump_rowid sc ~name meta` → update cache + record into the rowid-dirty
  set. The autocommit `next_rowid` (durable self-commit) uses a durable counter set.

### 4. Reads

`Schema_cache` exposes the read accessors the rest of `catalog.ml` needs over the
private hashtables: `find_table`, `find_index`, `find_fts`, `mem_table`, `mem_index`,
`fold_tables`, `fold_indexes`, `fold_fts` (covering `list_tables`, `indexes_for_table`,
`fingerprints_by_tree_id`, `pp`, `find_index_covering_cols`, etc.). These are
read-only and never touch the undo log.

### 5. Lifecycle

`Schema_cache` exposes `commit`, `rollback`, `savepoint_begin`, `savepoint_rollback`,
`savepoint_release`, `mark_poisoned`, `is_poisoned`, `take_rowid_bumped`,
`recompute_after_rollback`-supporting accessor. The existing public `Catalog`
functions (`commit_schema_changes`, `rollback_schema_changes`, `savepoint_*_schema`,
`mark_schema_txn_poisoned`, `schema_txn_poisoned`,
`recompute_rowid_counters_after_rollback`) **stay** — the db layer drives them — but
become thin delegations into `Schema_cache`. Their semantics and doc comments are
preserved verbatim.

### 6. Call-site changes and deletions

- **`catalog.ml` mutators** (`create_table`, `create_index`, `add_column`,
  `drop_column`, `rename_table`/`finish_rename`, `rename_column`, `create_fts_table`,
  `drop_table`, `drop_index`, `register_ephemeral`/`unregister_ephemeral`,
  `next_rowid`/`next_rowid_in_txn`/`bump_next_rowid_in_txn`): drop the
  `match txn with Some/None` undo bookkeeping and the direct `Hashtbl.*` calls;
  route through the appropriate `Schema_cache` mutator (in-txn → undo variant,
  autocommit → durable variant, rowid → `bump_rowid`, CTE sentinel → durable).
- **`exec.ml:5301` and `exec.ml:5320`**: delete the external `register_schema_undo`
  blocks. `drop_table`/`drop_index` now self-register via `remove_table`/
  `remove_index`. This closes the headline asymmetry. `execute_drop_table` no longer
  needs to read `restore_meta`/`dropped_idxs` for undo purposes.
- **Delete from the public API** (`catalog.mli`): `register_schema_undo`,
  `restore_table_cache`, `restore_index_cache` — no longer reachable or needed.
- **`drop_table`/`drop_index` signature**: they still take the borrowed `tx` for the
  store-side deletes; the cache side now goes through `Schema_cache.remove_*` which
  captures the prior entry itself, so `execute_drop_table`'s pre-mutation snapshot of
  `restore_meta`/`dropped_idxs` is removed.

### 7. Gaps closed

- The drop_table/drop_index internal-vs-external undo asymmetry — eliminated; all
  mutators self-register uniformly.
- The hand-written tree-tag reversals in the three ALTER paths — folded into
  `put_table`, so a future ALTER cannot forget to re-stamp.
- Any future cache mutation site physically cannot skip its reversal — the only way
  to write the cache is through the sealed mutators.

## Testing

Pure refactor — the primary safety net is the full existing suite (~1,771 tests),
notably `test_ddl_txn_269` (DDL-in-txn rollback, ALTER, savepoints), the DROP-undo
cases (#279), the savepoint schema-undo cases (#280/#295), and the rowid-rollback
cases (#293). All must stay green with no assertion changes.

Add a focused `test_schema_cache_283.ml` driven entirely through the public `Db`/SQL
API (no peeking at internals) asserting cache⇄store agreement after `ROLLBACK` and
`ROLLBACK TO SAVEPOINT` for each mutator kind:

1. `CREATE TABLE` in txn → ROLLBACK → table absent from cache and store.
2. `DROP TABLE` (with a dependent index) in txn → ROLLBACK → table and index both
   reappear, queryable.
3. `CREATE INDEX` / `DROP INDEX` in txn → ROLLBACK → index state restored.
4. `ALTER TABLE ADD/DROP/RENAME COLUMN` in txn → ROLLBACK → original columns and the
   table fingerprint restored (schema-shape round-trips; a subsequent query sees the
   old shape).
5. Mixed DDL across nested `SAVEPOINT`s → `ROLLBACK TO` an inner savepoint → only
   post-savepoint DDL reversed, pre-savepoint DDL retained.
6. `CREATE TABLE` then INSERT (rowid bump) in txn → ROLLBACK → next allocation reuses
   the rolled-back rowid (#293 parity preserved through the new `bump_rowid` path).

## Non-goals

- Unifying the three *store*-txn acquisition idioms (`with_ddl_txn` vs `acquire_txn`
  vs raw `rw_begin`). This spec consolidates the **cache-undo** ledger; store-txn
  acquisition stays as-is (`with_ddl_txn` already picks borrowed-vs-fresh correctly).
- Enforcing the store-write-before-cache-mutate ordering in the type system. That
  ordering is already correct at every site and `with_ddl_txn` rolls the store back
  on error before the cache undo replays; encoding it structurally is disproportionate
  to its risk. It stays a documented per-mutator convention.
- AUTOINCREMENT (unimplemented in this engine) — unaffected.
