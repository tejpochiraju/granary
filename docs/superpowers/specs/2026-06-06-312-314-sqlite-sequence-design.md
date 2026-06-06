# Design: sqlite_sequence + AUTOINCREMENT follow-ups (#312, #314)

Date: 2026-06-06
Issues: #312 (four deferred surfaces), #314 (mirror reconstruction of high-water)
Builds on: #299 / PR #313 (AUTOINCREMENT, merged to main at 3268faa)

## Background

PR #313 shipped `INTEGER PRIMARY KEY AUTOINCREMENT` end-to-end. The AUTOINCREMENT
high-water counter lives in `Cat.table_meta.next_rowid` (the single source of
truth), persisted as a trailing field of the `_sys_tables` value encoding and, as
of v2, the redundant catalog mirror (flag only, **not** the counter). `next_rowid`
is the live fast-path counter used by `alloc_rowid` / `bump_next_rowid_in_txn`,
with transactional revert handled by `recompute_rowid_counters_after_rollback`
(#313) and `ROLLBACK TO SAVEPOINT` snapshot/restore (#303).

This work delivers the five deliberately-deferred surfaces, all chosen at full
SQLite fidelity:

1. Queryable + writable `sqlite_sequence` table (#312.1)
2. `Db.dump` emission of `sqlite_sequence` rows (#312.1)
3. `INTEGER PRIMARY KEY DESC` as a non-alias (#312.2)
4. Table-constraint AUTOINCREMENT form `PRIMARY KEY(col AUTOINCREMENT)` (#312.3)
5. `SQLITE_FULL` on AUTOINCREMENT exhaustion (#312.4)
6. High-water survives mirror reconstruction (#314)

## Key architecture decisions (locked with user)

- **`sqlite_sequence` is a VIEW over `next_rowid`, not a real btree table.**
  `next_rowid` remains the single source of truth — no dual state. The table is
  synthesized at query time exactly like the existing `sqlite_master` virtual
  table (`sema.ml` `sqlite_master_meta`, tree_id sentinel `-2`, rows built in
  `exec.ml` `stream_sqlite_master`).
- **#314 is fixed independently** by persisting `next_rowid` in the mirror for
  AUTOINCREMENT tables only (issue #314 option 2), NOT "for free" via a real
  table — because the view is not stored, the recovery source must be the
  persisted counter.
- **`INTEGER PRIMARY KEY DESC` reuses the existing non-alias machinery**
  (implicit `__pk_*` unique index + auto hidden rowid, today's `TEXT PRIMARY KEY`
  path). No descending B-tree indexes are implemented: an ascending unique index
  enforces the identical constraint; sort direction affects only `ORDER BY`
  optimization, never query results.

## Part 1 — Queryable + writable `sqlite_sequence`

### Read path

- Add `sqlite_sequence_meta` in `sema.ml` alongside `sqlite_master_meta`:
  tree_id sentinel `-3`, columns `name TEXT`, `seq INTEGER`. Name resolution
  returns it for `"sqlite_sequence"` (case-insensitive).
- Planner (`planner.ml` `make_scan`): tree_id `-3` → new `Plan.Op_sqlite_sequence`.
- Exec `stream_sqlite_sequence`: enumerate AUTOINCREMENT tables whose
  `next_rowid <> empty_next_rowid`, emit `(name, next_rowid - 1)`. Tables with no
  insert yet are absent — matches SQLite (the row appears on first insert).
- `stream_sqlite_master` emits a synthesized `sqlite_sequence` row
  (`type=table`, `name=sqlite_sequence`, `tbl_name=sqlite_sequence`,
  `rootpage` = a stable sentinel, `sql='CREATE TABLE sqlite_sequence(name,seq)'`)
  whenever ≥1 AUTOINCREMENT table exists in the catalog.

### Write path

A focused DML interpreter for statements targeting `sqlite_sequence` (it is
synthesized, so the generic DML path cannot apply). Supported forms — exactly
those SQLite's `.dump` and ordinary use emit:

- `UPDATE sqlite_sequence SET seq = N [WHERE name = 't']`
- `DELETE FROM sqlite_sequence [WHERE name = 't']`
- `INSERT INTO sqlite_sequence(name, seq) VALUES('t', N)` and positional
  `INSERT INTO sqlite_sequence VALUES('t', N)`

Anything more exotic (joins, subqueries, additional predicates, other SET
targets) → a clear "unsupported operation on sqlite_sequence" error. This subset
is documented.

Semantics follow SQLite's effective rule `next = max(requested_seq, max(rowid)) + 1`:

- **Set / insert**, common **raise** path: `next_rowid := max(N + 1, next_rowid)`
  — no tree scan, since `next_rowid` already equals `max(rowid) + 1` (its
  high-water invariant). This is exactly `max(N, max(rowid)) + 1`.
- **Set / insert**, **lower** request (`N + 1 < next_rowid`): clamp to
  `max(rowid) + 1`, obtained with a single seek-to-last on the data tree. Lets a
  user lower the counter down to (but not below) the live max rowid, matching
  SQLite. Lowering below `max(rowid)` has no observable effect — same as SQLite.
- **DELETE**: `next_rowid := empty_next_rowid` (next insert recomputes from data)
  — matches SQLite resetting the sequence by removing its row.
- Writes target **AUTOINCREMENT tables only**; unknown or non-AUTOINCREMENT
  names → error. `next_rowid` stays the single source of truth.
- All writes go through the active transaction via `bump`/`set` so they revert on
  ROLLBACK exactly like the underlying counter (reusing #313/#303 machinery).

## Part 1b — `Db.dump` emission

In `db.ml` `dump`, after a table's DDL (and matching SQLite's ordering), emit for
the database as a whole:

```
DELETE FROM sqlite_sequence;
INSERT INTO sqlite_sequence VALUES('t1', seq1);
INSERT INTO sqlite_sequence VALUES('t2', seq2);
```

one INSERT per AUTOINCREMENT table whose counter is seeded
(`next_rowid <> empty_next_rowid`), `seq = next_rowid - 1`. This is what makes a
committed-delete high-water round-trip; replaying data rows alone only restores
`max(rowid) + 1`. Emitted only when ≥1 such table exists, so non-AUTOINCREMENT
databases are unchanged.

## Part 2 — `INTEGER PRIMARY KEY DESC` as non-alias

- **Parser** (`parser.mly:~589`): the parsed `is_desc` bit is currently dropped.
  Thread it into the AST. `column_def` gains a `pk_desc : bool` field (default
  false). `DESC + AUTOINCREMENT` stays rejected at parse (already done).
- **Schema persistence** (`row.mli` / column encoding in `catalog.ml`): persist
  `pk_desc` as a trailing backward-compatible field (absence ⇒ false), the same
  pattern used for `without_rowid` and `autoincrement`. Required because
  `compute_rowid_alias_col` is recomputed from `Row.column` on every reopen — the
  DESC-ness must survive or the table would reload as an alias.
- **Alias computation** (`catalog.ml:472` `compute_rowid_alias_col`): when the
  single INTEGER PK column has `pk_desc = true`, return `None`. The table then
  falls through to the existing non-alias path — auto-allocated hidden rowid plus
  the implicit `__pk_*` unique index from `sema.ml` `auto_unique_indexes`. No new
  storage code.
- **DDL round-trip** (`exec.ml` `ddl_of_table`): emit `DESC` after the column's
  `PRIMARY KEY` so reopen reproduces the non-alias.
- **Documented deviation**: the implicit unique index is stored ascending.
  Uniqueness is identical; only `ORDER BY id DESC` optimization differs, never
  results. Noted in a code comment.

## Part 3 — Table-constraint AUTOINCREMENT form

- **Parser**: extend the `PRIMARY KEY(...)` table-constraint production to accept
  `PRIMARY KEY(col AUTOINCREMENT)`, carrying the AUTOINCREMENT flag through the
  same channel the column form uses.
- **Validation**: reuse the existing `sema` checks (single-column, ascending,
  INTEGER PK, rowid table). `PRIMARY KEY(a, b AUTOINCREMENT)` (composite +
  AUTOINCREMENT) stays a syntax/validation error, matching SQLite.

## Part 4 — `SQLITE_FULL` on AUTOINCREMENT exhaustion

- When an AUTOINCREMENT allocation would need to exceed `Int64.max_int` (counter
  already held at `max_int` and that row exists), raise an error with SQLite's
  wording (`database or disk is full`) instead of silently holding the counter
  and risking a collision.
- Scoped to AUTOINCREMENT. Plain rowid tables keep current behavior; SQLite's
  random-rowid probing for plain tables at exhaustion is explicitly out of scope
  (noted in a code comment).

## #314 — high-water survives mirror reconstruction

- Persist `next_rowid` in the catalog mirror **for AUTOINCREMENT tables only**, as
  a trailing field (mirror version bump v2 → v3, absence ⇒ counter not present).
  Written at the sites that already rewrite the `_sys_tables` counter on bump.
  Accepts the per-insert mirror write for these (rare, opt-in) tables.
- `recover_next_rowid` (`catalog.ml:~1365`): for an AUTOINCREMENT table, restore
  the persisted counter from the mirror instead of `max(rowid) + 1`. Non-
  AUTOINCREMENT tables unchanged (still recompute from data).

## Testing (project bar: 100% coverage + QCheck fuzzing)

- **sqlite_sequence read**: SELECT visibility; absent-until-first-insert;
  `seq = next_rowid - 1`; appears in `sqlite_master` once an AUTOINCREMENT table
  exists.
- **sqlite_sequence write**: UPDATE/DELETE/INSERT semantics including the
  lower-clamp corner (lower to between max(rowid) and high-water; lower below
  max(rowid) is a no-op); rejection of exotic DML and non-AUTOINCREMENT names;
  ROLLBACK reverts a sqlite_sequence write.
- **dump round-trip**: dump → reload preserves a committed-delete high-water
  (insert N rows, delete the top, dump, reload, assert next insert does not reuse).
- **DESC non-alias**: rowid is hidden/auto; uniqueness enforced; reopen stays
  non-alias; DDL round-trip emits and re-parses `DESC`; QCheck property that a
  DESC INTEGER PK rejects duplicates.
- **Table-constraint AUTOINCREMENT**: parses; same validation rejections as the
  column form (non-INTEGER, WITHOUT ROWID, composite).
- **SQLITE_FULL**: AUTOINCREMENT exhaustion raises the correct error; a plain
  rowid table at max_int is unaffected.
- **#314**: corrupt the primary `_sys_tables` row, force reconstruction from the
  mirror, assert the high-water (including a committed-delete high-water)
  survives. Extend the existing `mirror_preserves_autoincrement` test.

## Out of scope

- Real-btree `sqlite_sequence` as source of truth (rejected: dual-state risk,
  rewrites #299/#303/#313 transactional machinery).
- Descending B-tree indexes (not needed for correctness).
- Random-rowid probing for plain (non-AUTOINCREMENT) rowid tables at exhaustion.
