# DROP REACTIVE VIEW (#469) — design

Date: 2026-07-30
Issue: tej/granary#469 (raised in review of PR #467 / #437; pre-existing since #427)

## Problem

A reactive view, once created, can never be retired. `Catalog.remove_reactive_view`
(`lib/catalog/catalog.mli:613`) has no callers anywhere in `lib/`, `bin/`, or `test/`,
and there is no `DROP REACTIVE VIEW` statement.

Two consequences:

1. **No removal path.** A config-driven caller cannot un-wire a view it no longer
   wants maintained, and every commit keeps paying that view's maintenance cost.
2. **Direct materialisation drops desynchronise the registry.** `DROP TABLE _rv_<name>`
   removes the materialisation but leaves the in-memory `reactive_views` entry, so
   `Db.is_reactive_view db "<name>"` still answers `true` for a view with no
   materialisation — the mirror image of the false positive #437 set out to
   eliminate. Flush behaviour against a missing `_rv_` table is untested.

#467 made the registry a *public* contract (`Db.reactive_view_names`,
`Db.is_reactive_view`, `register_view_callback`'s `Error (`Unknown_view _)`), so the
gap is now observable by callers.

## Design

### 1. Grammar and plumbing

`DROP REACTIVE VIEW [IF EXISTS] <name>`.

A new `drop_reactive_view` rule sits alongside `drop_view` in `lib/sql/parser.mly` and
is carried through the existing statement pipeline:

```
Ast.S_drop_reactive_view { name; if_exists }
  -> Sema.BS_drop_reactive_view { name; if_exists }
  -> Plan.Op_drop_reactive_view { name; if_exists }
  -> db.ml: !rv_drop_hook top ~name ~if_exists
```

`REACTIVE` is already a lexer token (`lib/sql/lexer.mll:188`) and is already listed
among the non-reserved identifiers in `parser.mly`, so adding the rule introduces no
keyword regression.

The hook indirection mirrors the existing `rv_create_hook` / `rv_flush_hook` /
`rv_load_hook`: the reactive-view machinery is defined below the statement-dispatch
layer in `db.ml`, and only the top-level handle holds the registry.

`Exec` needs the new op in its three existing DDL match sites (`op_name`, and the two
DDL-passthrough branches) exactly as `Op_drop_view` appears there.

### 2. `rv_drop top ~name ~if_exists` — the single teardown path

Runs with `rv_refreshing <- true` under `Lwt.finalize`, exactly like `rv_create`, so
its own DDL is recognised as internal.

Steps, in order:

1. Not present in `top.reactive_views`:
   - `if_exists` → `Ok ()`
   - otherwise → `Error (Runtime "no such reactive view: <name>")`
2. Remove the registry entry. This discards `rv_callbacks` and, for a `RV_delta` view,
   the `Rv.Agg_engine` state with it — there is no separate delta-state store to clean.
3. Drop any `rv_pending` accumulator entries whose base table is no longer referenced
   by any *remaining* registered view. (Tables still referenced by another view keep
   their pending changes.)
4. `DROP TABLE _rv_<name>` via internal DML — the same call `rv_refresh_one` already
   makes when it re-types a placeholder materialisation.
5. `Cat.remove_reactive_view top.store ~name` — its first caller.

Ordering is registry-first deliberately: a failure part-way through leaves a view that
is *gone* rather than one that is registered but unmaintainable. `rv_load` on the next
open finds no catalog row and does not re-register it; a leftover `_rv_<name>` table is
inert (`rv_load`'s authority is the catalog row, not the table name).

### 3. Internal-table guard on `DROP TABLE`

`DROP TABLE _rv_<name>` errors with:

```
table '_rv_x' is an internal reactive-view materialisation; use DROP REACTIVE VIEW x
```

- The guard is skipped when `top.rv_refreshing` is set — which is exactly how internal
  teardown and re-type writes are already distinguished from user statements.
- It fires only when `<name>` is present in the registry, so a pre-existing user table
  literally named `_rv_foo` with no corresponding reactive view stays droppable.
- Scope is `DROP TABLE` only. `INSERT`/`UPDATE`/`ALTER` against `_rv_` tables are not
  touched by this change (see Non-goals).

### 4. `DROP VIEW` on a reactive view

Today `DROP VIEW x` on a reactive view silently succeeds as a no-op: reactive views
live in `top.reactive_views`, not `t.views`, so the `Hashtbl.remove t.views name` finds
nothing and `Cat.remove_view` deletes a row that was never written.

The `Op_drop_view` branch gains a check: if `is_reactive_view t name`, error with

```
'x' is a reactive view; use DROP REACTIVE VIEW x
```

This fires even under `IF EXISTS`, since the named object demonstrably exists — the
error is "wrong statement for this object", not "object missing".

### 5. Transactionality

`DROP REACTIVE VIEW` follows `CREATE REACTIVE VIEW`, not `DROP VIEW`: it applies
immediately and is **not** staged through `staged_schema_change`.

Rationale: both create and drop perform internal DML/DDL against `_rv_` tables that
`staged_schema_change`'s in-memory `~undo` cannot reverse, and #269 (DDL-in-txn
deadlocks) makes staging the riskier of the two options.

Consequence, to be documented in `db.mli` and asserted by a test: a `ROLLBACK` after
`DROP REACTIVE VIEW` does **not** resurrect the view. This is symmetric with
`CREATE REACTIVE VIEW`, which a `ROLLBACK` likewise does not undo.

## Testing

All in `test/test_reactive_view_427.ml`, alongside the existing #427/#437 coverage.

1. After `DROP REACTIVE VIEW x`: `x` absent from `Db.reactive_view_names`,
   `Db.is_reactive_view` → `false`, and `_rv_x` absent from the catalog.
2. A callback registered before the drop stops firing on subsequent base-table writes.
3. `register_view_callback ~view_name:"x"` → `Error (`Unknown_view "x")` after the drop.
4. Durability: drop, close, reopen — `rv_load` does not resurrect the view.
5. `DROP REACTIVE VIEW nosuch` errors; `DROP REACTIVE VIEW IF EXISTS nosuch` succeeds.
6. `DROP TABLE _rv_x` on a live reactive view is rejected; a plain user table named
   `_rv_y` with no registry entry still drops successfully.
7. `DROP VIEW x` on a reactive view errors with the hint.
8. Create → drop → re-create `x` with a *different* query re-materialises correctly
   (proves no stale registry, catalog, or `_rv_` state survives the drop).
9. `ROLLBACK` after a drop does not resurrect the view (documents §5).

Both `REFRESH FULL` and `REFRESH DELTA` views are covered — the delta path is the one
with engine state to discard.

## Non-goals

- Cascading `DROP TABLE _rv_<name>` into deregistration (rejected: a second removal
  path to keep correct, and it makes `DROP TABLE` reactive-view-aware).
- `DROP VIEW` as an alias for `DROP REACTIVE VIEW` (rejected: `CREATE` is already
  asymmetric, so `DROP` should be too).
- Guarding `INSERT`/`UPDATE`/`DELETE`/`ALTER` against `_rv_` tables.
- Making reactive-view DDL transactional (§5).
- #429 and the broader `sqlite_`-style internal-table filter of #240 remain open.

Closes #469.
