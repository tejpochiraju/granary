# #240 — Expose the set of tables mutated by a write statement

**Issue:** tej/sqlite_ocaml_port#240 · **Cross-repo:** tej/camel#56
**Status:** design approved, ready for implementation plan
**Date:** 2026-06-17

## Problem

camel's read-side result cache (tej/camel#56) invalidates by table-version. Its
constrained first cut observes every mutation at *its own* write path, so it
needs no signal from sqlocaml. That model breaks the moment in-engine cascades
exist: a write to table `A` that fires a trigger or FK cascade silently mutating
table `B` leaves camel's cache for `B` stale — camel issued one write (`A`) but
`B` changed inside the engine without its knowledge.

The engine already knows which tables a statement actually touched (it tracks row
changes for trigger/upsert/cascade hooks). #240 exposes that set so an external
cache can invalidate exactly the tables that changed, regardless of
triggers/cascades.

This unlocks relaxing camel's "no triggers/cascades" constraint (the hybrid
invalidation path). It is **not** a blocker for the constrained first cut.

## Decisions (from brainstorming)

| Question | Decision |
|---|---|
| API shape | **Parallel `_with_dirty` variants** — mirror #239's `query_with_stats`/`iter_with_stats`. Existing signatures untouched; opt-in; pull-based. |
| Collection type | **`type dirty_tables = string list`** — deduplicated, sorted. Light to consume; no abstract-module / `pp` / merlint burden. |
| Internal tables | **User tables only** — filter names with the SQLite-reserved `sqlite_` prefix (the only prefix sema forbids to user tables). camel caches user-table queries; internal churn is noise. |

Rejected: a registered global callback (push-based, decorrelated from the call,
charges every write incl. internal ones, ordering subtleties under nested
triggers). Rejected: shipping both now (YAGNI).

## Mechanism

Reuse #239's accumulator pattern verbatim:

- An ambient mutable accumulator carried on an `Lwt.key` — call it
  `dirty_tables_key : (string, unit) Hashtbl.t Lwt.key` (or a `string`-set ref).
  A `Hashtbl`/set gives O(1) dedup; sorted-list materialization happens once at
  the API boundary.
- A `mark_dirty name` helper that mirrors `incr_examined`: `match Lwt.get
  dirty_tables_key with None -> () | Some acc -> add acc name`. No-op when no
  accumulator is installed (i.e. for plain `execute`/`run` callers), so the hot
  path of non-opted-in writes is untouched.
- The `_with_dirty` entry points install a fresh accumulator via
  `Lwt.with_value dirty_tables_key (Some acc)` around the statement, then drain
  it: filter out reserved-prefix names, dedup, sort → `string list`.

### Why this is cheap (the #240 performance question)

- **Common write path** (plain INSERT/UPDATE/DELETE, no triggers/cascades): one
  `mark_dirty` per statement, before the row loop. The inner scan/write loop is
  never touched. Off the read path entirely (no #239/#231 budget impact).
- **Trigger bodies** call `Exec.query ~mode:(In_txn tx)`, which re-enters the
  same `execute_insert`/`execute_update`/`execute_delete` functions — so marking
  at those three functions covers trigger DML for free.
- **FK cascades** use dedicated helpers (`cascade_apply_set_null/default`,
  `cascade_delete_*`, `cascade_update_*`) with the child `table_meta` in scope;
  these don't re-enter the three mains, so they get their own `mark_dirty`. The
  set dedupes, so the mark is hoisted above each helper's per-row loop where
  possible — at worst a `Hashtbl.replace` per cascaded row, dwarfed by the
  per-row B-tree write that path already performs.

Net: ~6 one-line mark sites + one accumulator + one filter. No mutable set
threaded through signatures — the `Lwt.key` makes it ambient, exactly as #239
did for stats.

## Mark sites (to confirm precisely during TDD)

1. `execute_insert` (`lib/sql/exec.ml`) — main table; also covers REPLACE
   (displaced rows are in the *same* table) and trigger-body inserts.
2. `execute_update` — main table + trigger-body updates.
3. `execute_delete` — main table + trigger-body deletes.
4. FK cascade helpers (`cascade_apply_set_null`, `cascade_apply_set_default`,
   and the cascade delete/update paths) — child table(s).

Triggers re-enter 1–3; cascades are covered by 4. Exact lines pinned in the
implementation plan via failing tests first.

## Public API (`lib/db/db.mli`)

```ocaml
(** #240: the set of user tables whose rows a write statement actually mutated,
    including tables touched indirectly by triggers and FK cascades.  Sorted,
    deduplicated; internal/system tables (the SQLite-reserved [sqlite_] prefix)
    are excluded.  Enables an external read cache to invalidate exactly the
    tables that changed.  See {!execute_with_dirty}. *)
type dirty_tables = string list

val execute_with_dirty : t -> string -> (dirty_tables, error) result Lwt.t
val execute_change_count_with_dirty :
  t -> string -> (int * dirty_tables, error) result Lwt.t
val run_with_dirty : stmt -> params:value list -> (int * dirty_tables, error) result Lwt.t
```

Each has a `(** … *)` doc comment (merlint requires doc comments on every public
`val`). The exec-layer accumulator type, key, and `mark_dirty` live in
`lib/sql/exec.ml` alongside the #239 `query_stats` machinery; `db.ml` re-exports
the list type and installs/drains the accumulator in the three wrappers.

### Caller contract (camel)

- Opt-in: camel calls `execute_with_dirty` / `run_with_dirty` for writes it wants
  tracked; plain `execute`/`run` are unchanged and carry no accumulator.
- Returned list contains **every** user table whose rows changed during the
  statement — main table, REPLACE-displaced rows, FK cascades, trigger-body DML —
  deduped and sorted. Empty list ⇒ no user-table rows changed (e.g. a write that
  matched nothing, or touched only internal tables).
- The set is correlated with exactly that call's result (no global side channel).
- Pure read statements never produce a dirty set (these variants are write-only).

## Testing

100% line coverage on the new code; QCheck where a property fits.

- Plain INSERT/UPDATE/DELETE → set is exactly the target table.
- INSERT … ON CONFLICT REPLACE displacing rows → still just the target table.
- No-op write (WHERE matches nothing) → empty set.
- FK `ON DELETE CASCADE` / `ON UPDATE CASCADE` parent write → set includes the
  child table(s).
- FK `SET NULL` / `SET DEFAULT` → set includes the child table(s).
- Trigger whose body writes a *different* table → set includes both.
- Nested/recursive triggers → all distinct tables present, deduped.
- AUTOINCREMENT insert → set is `["t"]`, **not** `["sqlite_sequence"; "t"]`
  (internal-table filter; `sqlite_sequence` is a view anyway).
- `run_with_dirty` on a prepared multi-step write → correct set.
- Determinism: the list is sorted and duplicate-free (QCheck over random
  multi-table trigger fan-out).
- Existing `execute`/`run` behaviour and `query_with_stats` unaffected.

## Out of scope

- A push/callback API (deferred unless camel needs it).
- Per-statement cost stats (that is #239, already shipped).
- Surfacing row-level deltas (only the table *set* is exposed).
