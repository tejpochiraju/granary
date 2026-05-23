# Phase 8: Practical SQL Mutations Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add ON CONFLICT resolution, RETURNING clause, ALTER TABLE, and table-level UNIQUE constraints — completing four high-value SQL mutation features (#62, #61, #50, #56).

**Architecture:** Each feature threads through the same 4-layer pipeline (AST → Sema → Plan → Exec) we've used in Phases 6 and 7. ON CONFLICT and RETURNING add fields to existing Op_insert/Op_update/Op_delete nodes. ALTER TABLE adds new catalog operations (add_column, rename_table, rename_column) and a new Op_alter_table plan op. Table-level UNIQUE constraints extend CREATE TABLE parsing to auto-generate UNIQUE indexes.

**Tech Stack:** OCaml 5.1, Menhir (parser generator), Lwt (async), Alcotest + QCheck (tests), bisect_ppx (coverage), dune inside `sqlocaml-dev` Podman image.

---

## Build and test commands

All dune commands run inside Podman:

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1
```

Coverage (summary only):
```bash
./scripts/coverage.sh
```

---

## File Map

| File | Change |
|------|--------|
| `lib/sql/ast.ml` | Add `conflict_action`, extend `S_insert`/`S_update`/`S_delete` with `on_conflict`/`returning`; add `S_alter_table`; add table-level `column_constraint` variants |
| `lib/sql/lexer.mll` | New tokens: `ABORT IGNORE FAIL RETURNING ALTER ADD RENAME TO` |
| `lib/sql/parser.mly` | `%token` declarations + grammar rules for all new syntax |
| `lib/sql/sema.ml` / `sema.mli` | Extend `BS_insert`/`BS_update`/`BS_delete` with `on_conflict`/`returning`; add `BS_alter_table`; `BS_create_table` gets `uniq_cols` |
| `lib/sql/plan.ml` | Extend `Op_insert`/`Op_update`/`Op_delete`; add `Op_alter_table` |
| `lib/sql/planner.ml` | Plan new AST nodes |
| `lib/sql/exec.ml` | `execute_insert` gains conflict+returning logic; `to_stream` handles mutation ops with RETURNING; `execute_with_count` handles Op_alter_table |
| `lib/catalog/catalog.ml` / `catalog.mli` | New: `add_column`, `rename_table`, `rename_column` |
| `lib/encoding/row.ml` | `decode` handles "short" rows (fewer columns than schema → fill defaults) |
| `lib/db/db.ml` / `db.mli` | Route RETURNING statements through `query`; expose `query` for RETURNING stmts |
| `test/test_e2e.ml` | E2E tests for all six features |
| `test/test_sqlite_compare.ml` | 30+ SQLite comparison tests |

---

## Task 1: ON CONFLICT (INSERT OR REPLACE / INSERT OR IGNORE)

Implements `INSERT OR REPLACE INTO t ...` and `INSERT OR IGNORE INTO t ...`. When a UNIQUE or PRIMARY KEY constraint is violated: REPLACE deletes the conflicting row(s) then proceeds; IGNORE silently skips the insert. ABORT/FAIL/ROLLBACK all behave like the existing default (raise error) — their subtle transaction-semantics differences are deferred.

Closes issue #62.

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/sema.ml`
- Modify: `lib/sql/sema.mli`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/planner.ml`
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_e2e.ml`

- [ ] **Step 1: Write the failing test in `test/test_e2e.ml`**

Add a new `on_conflict` test group at the end of the file (before the closing `;;`):

```ocaml
(* ------------------------------------------------------------------ *)
(* ON CONFLICT                                                           *)
(* ------------------------------------------------------------------ *)

let test_insert_or_ignore () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 'first')" in
    (* IGNORE: duplicate id=1 should be silently skipped *)
    let* _ = Db.execute db "INSERT OR IGNORE INTO t VALUES (1, 'second')" in
    let* r = Db.execute db "SELECT v FROM t WHERE id = 1" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "row count" 1 (List.length rows);
       Alcotest.(check string) "value unchanged" "first"
         (match rows with [| [| Db.V_text s |] |] -> s | _ -> "WRONG");
       Lwt.return_unit))

let test_insert_or_replace () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 'first')" in
    (* REPLACE: delete id=1 then insert new row *)
    let* _ = Db.execute db "INSERT OR REPLACE INTO t VALUES (1, 'second')" in
    let* r = Db.execute db "SELECT v FROM t WHERE id = 1" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "row count" 1 (List.length rows);
       Alcotest.(check string) "value replaced" "second"
         (match rows with [| [| Db.V_text s |] |] -> s | _ -> "WRONG");
       Lwt.return_unit))

let test_insert_or_ignore_unique () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER, name TEXT)" in
    let* _ = Db.execute db "CREATE UNIQUE INDEX idx_name ON t (name)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 'alice')" in
    let* _ = Db.execute db "INSERT OR IGNORE INTO t VALUES (2, 'alice')" in
    let* r = Db.execute db "SELECT COUNT(*) FROM t" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "only original row" 1
         (match rows with [| [| Db.V_int n |] |] -> Int64.to_int n | _ -> -1);
       Lwt.return_unit))

let () =
  Alcotest.run "sqlocaml e2e" [
    (* ... existing groups ... *)
    "on_conflict", [
      Alcotest.test_case "insert_or_ignore" `Quick test_insert_or_ignore;
      Alcotest.test_case "insert_or_replace" `Quick test_insert_or_replace;
      Alcotest.test_case "insert_or_ignore_unique" `Quick test_insert_or_ignore_unique;
    ];
  ]
```

- [ ] **Step 2: Run the test to confirm it fails**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | grep -A5 "on_conflict"
```

Expected: parse error or compilation failure (the syntax doesn't exist yet).

- [ ] **Step 3: Add `conflict_action` type to `lib/sql/ast.ml`**

After `type param = ...` (around line 20), add:

```ocaml
type conflict_action = CA_rollback | CA_abort | CA_fail | CA_ignore | CA_replace
```

Extend `S_insert` (around line 112) to add `on_conflict`:

```ocaml
  | S_insert of {
      table       : string;
      columns     : string list;
      values      : expr list;
      on_conflict : conflict_action option;
    }
```

- [ ] **Step 4: Add tokens to `lib/sql/lexer.mll`**

In the keyword list (around line 40–80), add after `"REPLACE" { REPLACE }`:

```ocaml
  | "ABORT"   { ABORT }
  | "IGNORE"  { IGNORE }
  | "FAIL"    { FAIL }
```

- [ ] **Step 5: Update `lib/sql/parser.mly`**

Add `%token` declarations (after line 43, near the other `%token` lines):

```mly
%token ABORT IGNORE FAIL
```

Add an `opt_conflict` rule and update the `insert` rule. Insert after the existing `insert:` rule definition:

```mly
opt_conflict:
  | OR REPLACE  { Some Ast.CA_replace  }
  | OR IGNORE   { Some Ast.CA_ignore   }
  | OR ABORT    { Some Ast.CA_abort    }
  | OR FAIL     { Some Ast.CA_fail     }
  | OR ROLLBACK { Some Ast.CA_rollback }
  |             { None }

insert:
  | INSERT oc = opt_conflict INTO table = IDENT
      LPAREN cols = separated_nonempty_list(COMMA, IDENT) RPAREN
      VALUES LPAREN vals = separated_nonempty_list(COMMA, insert_expr) RPAREN
    { Ast.S_insert { table; columns = cols; values = vals; on_conflict = oc } }
  | INSERT oc = opt_conflict INTO table = IDENT
      VALUES LPAREN vals = separated_nonempty_list(COMMA, insert_expr) RPAREN
    { Ast.S_insert { table; columns = []; values = vals; on_conflict = oc } }
```

The original `insert:` rules (without `opt_conflict`) are replaced by the two rules above.

- [ ] **Step 6: Extend `BS_insert` in `lib/sql/sema.mli` and `sema.ml`**

In `sema.mli`, update `BS_insert`:

```ocaml
  | BS_insert of {
      table_meta  : Cat.table_meta;
      ordinals    : int list;
      values      : bound_expr list;
      on_conflict : Ast.conflict_action option;
    }
```

In `sema.ml`, update `bind_insert` to pass `on_conflict` through (around line 717):

```ocaml
            Lwt.return (Ok (BS_insert {
              table_meta  = meta;
              ordinals;
              values      = full_vals;
              on_conflict;   (* NEW: thread through from Ast.S_insert *)
            }))
```

The `bind_insert` function signature changes to accept `~on_conflict`:

```ocaml
let bind_insert cat ~param_counter ~named_params ~table ~columns ~values ~on_conflict = ...
```

And in `bind_internal` (around line 1439), update the S_insert case:

```ocaml
  | Ast.S_insert { table; columns; values; on_conflict } ->
    bind_insert cat ~param_counter ~named_params ~table ~columns ~values ~on_conflict
```

- [ ] **Step 7: Extend `Op_insert` in `lib/sql/plan.ml`**

```ocaml
  | Op_insert of {
      table_meta  : Cat.table_meta;
      ordinals    : int list;
      values      : expr list;
      on_conflict : Ast.conflict_action option;
    }
```

- [ ] **Step 8: Update `lib/sql/planner.ml` to thread `on_conflict`**

In the `BS_insert` case (find it with `grep -n "BS_insert" lib/sql/planner.ml`):

```ocaml
  | Sema.BS_insert { table_meta; ordinals; values; on_conflict } ->
    Plan.Op_insert {
      table_meta;
      ordinals;
      values     = List.map sema_expr_to_plan values;
      on_conflict;
    }
```

- [ ] **Step 9: Implement conflict resolution in `lib/sql/exec.ml`**

Update `execute_insert` signature to accept `~on_conflict`:

```ocaml
let execute_insert ?(mode = Auto) ?(params = [||])
    ?(clock : (unit -> float) option = None)
    ?(on_conflict : Ast.conflict_action option = None)
    (store : S.t) (cat : Cat.t)
    ~(table_meta : Cat.table_meta) ~ordinals ~(values : Plan.expr list) : unit Lwt.t =
```

Restructure the body to check UNIQUE constraints BEFORE writing the row, then apply conflict resolution. Replace the current inline UNIQUE check + write sequence with:

```ocaml
  let n   = List.length table_meta.columns in
  let row = Array.make n Row.V_null in
  List.iter2 (fun ord expr -> row.(ord) <- eval_expr clock params [||] expr) ordinals values;
  let* (tx, owned) = acquire_txn store mode in
  Lwt.catch
    (fun () ->
      let* rowid = Cat.next_rowid_in_txn cat ~name:table_meta.name tx in
      let idxs   = Cat.indexes_for_table cat ~table:table_meta.name in
      (* Step 1: Check UNIQUE constraints before writing anything. *)
      let* skip =
        Lwt_list.fold_left_s (fun acc (idx : Cat.index_info) ->
          if not acc.continue || not idx.idx_unique then Lwt.return acc
          else begin
            let col_is = List.map (find_col_idx_by_name table_meta.columns) idx.idx_columns in
            let iks    = List.map (fun ci -> row_value_to_index_value row.(ci)) col_is in
            let prefix =
              let buf = Buffer.create 32 in
              List.iter (fun ikv -> Buffer.add_bytes buf (Index_key.encode_value ikv)) iks;
              Buffer.to_bytes buf
            in
            let plen     = Bytes.length prefix in
            let seek_key = Bytes.cat prefix (Rowid.encode Int64.min_int) in
            let* cur     = S.cursor_open tx idx.idx_tree_id in
            let _        = S.cursor_seek cur seek_key in
            let conflict_rowid_opt =
              match S.cursor_next cur with
              | None -> None
              | Some (ikey, _) ->
                if Bytes.length ikey >= plen &&
                   Bytes.equal (Bytes.sub ikey 0 plen) prefix
                then
                  let rid_bytes = Bytes.sub ikey plen (Bytes.length ikey - plen) in
                  Some (Rowid.decode rid_bytes)
                else None
            in
            S.cursor_close cur;
            match conflict_rowid_opt with
            | None -> Lwt.return acc
            | Some old_rowid ->
              (match on_conflict with
               | Some CA_ignore ->
                 (* Signal skip: stop checking further constraints *)
                 Lwt.return { continue = false; skip = true; to_delete = acc.to_delete }
               | Some CA_replace ->
                 Lwt.return { acc with to_delete = old_rowid :: acc.to_delete }
               | _ ->
                 Lwt.fail_with (Printf.sprintf
                   "UNIQUE constraint violated: duplicate value in columns (%s)"
                   (String.concat ", " idx.idx_columns)))
          end
        ) { continue = true; skip = false; to_delete = [] } idxs
      in
      if skip.skip then begin
        (* IGNORE: rollback to undo rowid allocation; return normally *)
        let* () = if owned then S.rollback tx else Lwt.return_unit in
        Lwt.return_unit
      end else begin
        (* REPLACE: delete all conflicting rows *)
        let* () = Lwt_list.iter_s (fun old_rowid ->
          let old_key   = Rowid.encode old_rowid in
          let* old_bytes_opt = S.get tx table_meta.tree_id old_key in
          match old_bytes_opt with
          | None -> Lwt.return_unit
          | Some old_bytes ->
            let old_row = Row.decode table_meta.columns old_bytes in
            (* Delete old row from table tree *)
            let* () = S.delete tx table_meta.tree_id old_key in
            (* Delete old row's index entries *)
            Lwt_list.iter_s (fun (idx2 : Cat.index_info) ->
              let col_is2 = List.map (find_col_idx_by_name table_meta.columns) idx2.idx_columns in
              let iks2    = List.map (fun ci -> row_value_to_index_value old_row.(ci)) col_is2 in
              let old_ikey = Index_key.encode iks2 ~rowid:old_rowid in
              S.delete tx idx2.idx_tree_id old_ikey
            ) idxs
        ) (List.sort_uniq compare skip.to_delete)
        in
        (* Write new row + index entries *)
        let key   = Rowid.encode rowid in
        let bytes = Row.encode table_meta.columns row in
        let* () = S.put tx table_meta.tree_id key bytes in
        let* () = Lwt_list.iter_s (fun (idx : Cat.index_info) ->
          let col_is = List.map (find_col_idx_by_name table_meta.columns) idx.idx_columns in
          let iks    = List.map (fun ci -> row_value_to_index_value row.(ci)) col_is in
          let ikey   = Index_key.encode iks ~rowid in
          S.put tx idx.idx_tree_id ikey Bytes.empty
        ) idxs in
        release_txn tx owned
      end)
    (fun exn ->
      let* () = if owned then S.rollback tx else Lwt.return_unit in
      Lwt.fail exn)
```

Define the helper record type near the top of `execute_insert` body (not as a module-level type since it's local):

```ocaml
  let open struct
    type conflict_state = {
      continue  : bool;
      skip      : bool;
      to_delete : int64 list;
    }
  end in
```

Update `execute_with_count` to pass `on_conflict` when dispatching `Op_insert`:

```ocaml
  | Plan.Op_insert { table_meta; ordinals; values; on_conflict; _ } ->
    let* () = execute_insert ~mode ~params ~clock ~on_conflict store cat ~table_meta ~ordinals ~values in
    Lwt.return 1
```

- [ ] **Step 10: Build and run tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | tail -20
```

Expected: all tests pass including the 3 new `on_conflict` tests.

- [ ] **Step 11: Commit**

```bash
git add lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly \
        lib/sql/sema.ml lib/sql/sema.mli lib/sql/plan.ml lib/sql/planner.ml \
        lib/sql/exec.ml test/test_e2e.ml
git commit -m "feat(sql): INSERT OR REPLACE and INSERT OR IGNORE (ON CONFLICT #62)"
```

---

## Task 2: RETURNING clause on INSERT / UPDATE / DELETE

Implements `INSERT INTO t VALUES (...) RETURNING col1, col2`, and similarly for UPDATE and DELETE. The result is a row stream accessible via `Db.query`. Closes issue #61.

**Design note:** RETURNING makes a DML statement behave like a SELECT. We add `returning: Plan.expr list` to Op_insert/Op_update/Op_delete (empty = no returning). In `to_stream`, we add cases for these ops when `returning` is non-empty, executing the mutation and yielding the projected rows. `Db.query` already routes through `to_stream` so it handles RETURNING statements automatically.

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/sema.ml`
- Modify: `lib/sql/sema.mli`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/planner.ml`
- Modify: `lib/sql/exec.ml`
- Modify: `lib/db/db.ml`
- Modify: `test/test_e2e.ml`

- [ ] **Step 1: Write the failing tests**

Add to `test/test_e2e.ml`:

```ocaml
(* ------------------------------------------------------------------ *)
(* RETURNING                                                            *)
(* ------------------------------------------------------------------ *)

let test_insert_returning () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER, v TEXT)" in
    let* r = Db.query db "INSERT INTO t VALUES (1, 'hello') RETURNING id, v" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "one returned row" 1 (List.length rows);
       let row = List.hd rows in
       Alcotest.(check int) "id=1" 1 (match row.(0) with Db.V_int n -> Int64.to_int n | _ -> -1);
       Alcotest.(check string) "v=hello" "hello" (match row.(1) with Db.V_text s -> s | _ -> "X");
       Lwt.return_unit))

let test_update_returning () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER, v TEXT)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 'old')" in
    let* _ = Db.execute db "INSERT INTO t VALUES (2, 'keep')" in
    let* r = Db.query db "UPDATE t SET v = 'new' WHERE id = 1 RETURNING id, v" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "one updated row returned" 1 (List.length rows);
       let row = List.hd rows in
       Alcotest.(check string) "v=new" "new" (match row.(1) with Db.V_text s -> s | _ -> "X");
       Lwt.return_unit))

let test_delete_returning () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER, v TEXT)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 'a')" in
    let* _ = Db.execute db "INSERT INTO t VALUES (2, 'b')" in
    let* r = Db.query db "DELETE FROM t WHERE id = 1 RETURNING id, v" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "one deleted row returned" 1 (List.length rows);
       let row = List.hd rows in
       Alcotest.(check int) "id=1" 1 (match row.(0) with Db.V_int n -> Int64.to_int n | _ -> -1);
       Lwt.return_unit))
```

Register in the `Alcotest.run` call:
```ocaml
    "returning", [
      Alcotest.test_case "insert_returning" `Quick test_insert_returning;
      Alcotest.test_case "update_returning" `Quick test_update_returning;
      Alcotest.test_case "delete_returning" `Quick test_delete_returning;
    ];
```

- [ ] **Step 2: Run to confirm failure**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | grep -A3 "returning"
```

Expected: compilation failure (RETURNING not parsed).

- [ ] **Step 3: Add RETURNING to AST**

In `lib/sql/ast.ml`, extend `S_insert`, `S_update`, `S_delete`:

```ocaml
  | S_insert of {
      table       : string;
      columns     : string list;
      values      : expr list;
      on_conflict : conflict_action option;
      returning   : expr list;   (* empty = no RETURNING *)
    }
  | S_update of {
      table       : string;
      assignments : (string * expr) list;
      where       : expr option;
      returning   : expr list;
    }
  | S_delete of {
      table   : string;
      where   : expr option;
      returning : expr list;
    }
```

- [ ] **Step 4: Add RETURNING token and grammar rule**

In `lib/sql/lexer.mll`, add:
```ocaml
  | "RETURNING" { RETURNING }
```

In `lib/sql/parser.mly`, add `%token RETURNING` and an `opt_returning` rule:

```mly
%token RETURNING

opt_returning:
  | RETURNING exprs = separated_nonempty_list(COMMA, expr) { exprs }
  |                                                         { [] }
```

Update `insert:`, `update:`, and `delete:` rules to end with `ret = opt_returning`:

```mly
(* insert — both variants get opt_returning *)
insert:
  | INSERT oc = opt_conflict INTO table = IDENT
      LPAREN cols = separated_nonempty_list(COMMA, IDENT) RPAREN
      VALUES LPAREN vals = separated_nonempty_list(COMMA, insert_expr) RPAREN
      ret = opt_returning
    { Ast.S_insert { table; columns = cols; values = vals;
                     on_conflict = oc; returning = ret } }
  | INSERT oc = opt_conflict INTO table = IDENT
      VALUES LPAREN vals = separated_nonempty_list(COMMA, insert_expr) RPAREN
      ret = opt_returning
    { Ast.S_insert { table; columns = []; values = vals;
                     on_conflict = oc; returning = ret } }

(* update: *)
update:
  | UPDATE table = IDENT SET
      assignments = separated_nonempty_list(COMMA, assignment)
      where_clause = option(WHERE e = expr { e })
      ret = opt_returning
    { Ast.S_update { table; assignments; where = where_clause; returning = ret } }

(* delete: *)
delete:
  | DELETE FROM table = IDENT where_clause = option(WHERE e = expr { e })
    ret = opt_returning
    { Ast.S_delete { table; where = where_clause; returning = ret } }
```

Check that `update:` and `delete:` are currently defined as single rules (they are — check `parser.mly` around lines 156–175).

- [ ] **Step 5: Extend sema types**

In `lib/sql/sema.mli`, update:

```ocaml
  | BS_insert of {
      table_meta  : Cat.table_meta;
      ordinals    : int list;
      values      : bound_expr list;
      on_conflict : Ast.conflict_action option;
      returning   : bound_expr list;
    }
  | BS_update of {
      table_meta  : Cat.table_meta;
      assignments : (int * bound_expr) list;
      where       : bound_expr option;
      returning   : bound_expr list;
    }
  | BS_delete of {
      table_meta : Cat.table_meta;
      where      : bound_expr option;
      returning  : bound_expr list;
    }
```

In `lib/sql/sema.ml`:

**bind_insert**: Thread `returning` through. After building `full_vals`, bind the returning exprs against `meta.columns` (using `bind_expr` with the table's columns as the environment).

At the end of `bind_insert`, after computing `full_vals`:

```ocaml
            (* Bind RETURNING exprs against the table's column schema. *)
            let col_names = List.map (fun c -> c.Row.name) meta.columns in
            let ret_result =
              List.fold_left (fun acc re ->
                match acc with
                | Error _ -> acc
                | Ok bexprs ->
                  (match bind_expr ~param_counter ~named_params col_names None [] re with
                   | Error e -> Error e
                   | Ok be   -> Ok (bexprs @ [be]))
              ) (Ok []) returning
            in
            (match ret_result with
             | Error e -> Lwt.return (Error e)
             | Ok ret_bound ->
               Lwt.return (Ok (BS_insert {
                 table_meta  = meta;
                 ordinals;
                 values      = full_vals;
                 on_conflict;
                 returning   = ret_bound;
               })))
```

**bind_update** (`lib/sql/sema.ml` around line 1130): After binding assignments and where, bind returning:

```ocaml
            (* Bind RETURNING exprs against the table's column schema. *)
            let col_names = List.map (fun c -> c.Row.name) meta.columns in
            let* ret_r = ... (* same fold pattern as above *) in
            match ret_r with
            | Error e -> Lwt.return (Error e)
            | Ok ret_bound ->
              Lwt.return (Ok (BS_update { table_meta = meta; assignments; where; returning = ret_bound }))
```

**bind_delete**: Similarly bind returning against the table schema.

**bind_internal** (around line 1439–1447): Update `S_update` and `S_delete` cases to pass `~returning`.

- [ ] **Step 6: Extend plan types and planner**

In `lib/sql/plan.ml`, update:

```ocaml
  | Op_insert of {
      table_meta  : Cat.table_meta;
      ordinals    : int list;
      values      : expr list;
      on_conflict : Ast.conflict_action option;
      returning   : expr list;
    }
  | Op_update of {
      table_meta  : Cat.table_meta;
      assignments : (int * expr) list;
      where       : expr option;
      indexes     : Cat.index_info list;
      returning   : expr list;
    }
  | Op_delete of {
      table_meta : Cat.table_meta;
      where      : expr option;
      indexes    : Cat.index_info list;
      returning  : expr list;
    }
```

In `lib/sql/planner.ml`, update the three `BS_insert`, `BS_update`, `BS_delete` cases to include `returning = List.map sema_expr_to_plan returning`.

- [ ] **Step 7: Implement RETURNING in exec.ml**

The key insight: `to_stream` already handles read ops. We extend it to handle mutation ops with `returning <> []`.

Add these cases to `to_stream` (after the existing `Op_distinct` / set-op cases):

```ocaml
  | Plan.Op_insert { table_meta; ordinals; values; on_conflict; returning }
    when returning <> [] ->
    (* Execute insert, emit projected row. *)
    let n = List.length table_meta.columns in
    let inserted_row = Array.make n Row.V_null in
    List.iter2 (fun ord e ->
      inserted_row.(ord) <- eval_expr clock params [||] e
    ) ordinals values;
    let* () = execute_insert ~clock ~on_conflict store cat ~table_meta ~ordinals ~values in
    let result = Array.of_list (List.map (eval_expr clock params inserted_row) returning) in
    Lwt.return (Lwt_stream.of_list [result])

  | Plan.Op_update { table_meta; assignments; where; indexes; returning }
    when returning <> [] ->
    (* Drain matching rows, update each, emit new values via returning. *)
    let schema = table_meta.Cat.columns in
    let* tx_ro = S.ro_begin store in
    let* cur   = S.cursor_open tx_ro table_meta.tree_id in
    let _sr    = S.cursor_first cur in
    let buf    = ref [] in
    let rec drain () =
      match S.cursor_next cur with
      | None -> ()
      | Some (kbytes, vbytes) ->
        let rowid = Rowid.decode kbytes in
        let row   = Row.decode schema vbytes in
        let keep  = match where with
          | None      -> true
          | Some pred -> value_truthy (eval_expr clock params row pred)
        in
        if keep then buf := (rowid, row) :: !buf;
        drain ()
    in
    drain ();
    S.cursor_close cur;
    let* () = S.ro_end tx_ro in
    let matches = List.rev !buf in
    let result_rows = List.filter_map (fun (_rowid, old_row) ->
      let new_row = Array.copy old_row in
      List.iter (fun (i, expr) ->
        new_row.(i) <- eval_expr clock params old_row expr
      ) assignments;
      let pred_ok = match where with
        | None      -> true
        | Some pred -> value_truthy (eval_expr clock params old_row pred)
      in
      if pred_ok then
        Some (Array.of_list (List.map (eval_expr clock params new_row) returning))
      else None
    ) matches in
    (* Execute the actual update (reuses existing execute_update) *)
    let* _ = execute_update ~clock ~params store ~table_meta ~assignments ~where ~indexes in
    Lwt.return (Lwt_stream.of_list result_rows)

  | Plan.Op_delete { table_meta; where; indexes; returning }
    when returning <> [] ->
    (* Drain matching rows, capture their values, then delete, emit via returning. *)
    let schema = table_meta.Cat.columns in
    let* tx_ro = S.ro_begin store in
    let* cur   = S.cursor_open tx_ro table_meta.tree_id in
    let _sr    = S.cursor_first cur in
    let buf    = ref [] in
    let rec drain () =
      match S.cursor_next cur with
      | None -> ()
      | Some (_kbytes, vbytes) ->
        let row = Row.decode schema vbytes in
        let keep = match where with
          | None      -> true
          | Some pred -> value_truthy (eval_expr clock params row pred)
        in
        if keep then buf := row :: !buf;
        drain ()
    in
    drain ();
    S.cursor_close cur;
    let* () = S.ro_end tx_ro in
    let matched = List.rev !buf in
    let result_rows = List.map (fun old_row ->
      Array.of_list (List.map (eval_expr clock params old_row) returning)
    ) matched in
    let* _ = execute_delete ~clock ~params store ~table_meta ~where ~indexes in
    Lwt.return (Lwt_stream.of_list result_rows)
```

Also update `execute_with_count` to handle `Op_insert`/`Op_update`/`Op_delete` with `returning = []` (no change needed since non-RETURNING ops still go through the existing `execute_insert`/`execute_update`/`execute_delete` paths).

Update `Exec.query` in `exec.ml` to also accept the new mutation-with-returning ops (it calls `to_stream`, which now handles them, so this is automatic — no change needed in `query`).

In `lib/db/db.ml`, `Db.query` already calls `Exec.query` which calls `to_stream`. Since RETURNING statements are mutations (not SELECT), they would previously fail at `execute_with_count` since `Db.execute` was called. Now the user must call `Db.query` for RETURNING statements. This is already the correct API — no `db.ml` changes needed.

Note: `Db.execute` on a RETURNING statement will fail with "not a select-like op" because `execute_with_count` doesn't handle those ops. This is correct: the user must call `Db.query` for RETURNING stmts.

- [ ] **Step 8: Build and run tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | tail -20
```

Expected: all tests pass including 3 new returning tests.

- [ ] **Step 9: Commit**

```bash
git add lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly \
        lib/sql/sema.ml lib/sql/sema.mli lib/sql/plan.ml lib/sql/planner.ml \
        lib/sql/exec.ml test/test_e2e.ml
git commit -m "feat(sql): RETURNING clause on INSERT/UPDATE/DELETE (#61)"
```

---

## Task 3: ALTER TABLE ADD COLUMN

Implements `ALTER TABLE t ADD COLUMN col_name col_type [NOT NULL] [DEFAULT val]`. New column is added to the catalog schema. Existing rows (which lack the new column's bytes) are handled by making `Row.decode` tolerate short rows: missing trailing columns get their schema default value (or NULL if no default).

Part of issue #50.

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/sema.ml`
- Modify: `lib/sql/sema.mli`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/planner.ml`
- Modify: `lib/sql/exec.ml`
- Modify: `lib/catalog/catalog.ml`
- Modify: `lib/catalog/catalog.mli`
- Modify: `lib/encoding/row.ml`
- Modify: `test/test_e2e.ml`

- [ ] **Step 1: Write the failing test**

```ocaml
let test_alter_add_column () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER, name TEXT)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 'alice')" in
    let* _ = Db.execute db "INSERT INTO t VALUES (2, 'bob')" in
    (* Add new column with a default value *)
    let* _ = Db.execute db "ALTER TABLE t ADD COLUMN score INTEGER DEFAULT 0" in
    (* Old rows read back: new column gets default *)
    let* r = Db.execute db "SELECT id, name, score FROM t ORDER BY id" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "2 rows" 2 (List.length rows);
       (match rows with
        | [r1; r2] ->
          Alcotest.(check int) "id1" 1 (match r1.(0) with Db.V_int n -> Int64.to_int n | _ -> -1);
          Alcotest.(check int) "score1=0" 0 (match r1.(2) with Db.V_int n -> Int64.to_int n | _ -> -1);
          Alcotest.(check int) "score2=0" 0 (match r2.(2) with Db.V_int n -> Int64.to_int n | _ -> -1)
        | _ -> Alcotest.fail "wrong row count");
       Lwt.return_unit))

let test_alter_add_column_null_default () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (42)" in
    (* Add column without DEFAULT: existing rows get NULL *)
    let* _ = Db.execute db "ALTER TABLE t ADD COLUMN extra TEXT" in
    let* r = Db.execute db "SELECT id, extra FROM t" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       let row = List.hd rows in
       Alcotest.(check bool) "extra is null" true (row.(1) = Db.V_null);
       Lwt.return_unit))
```

Register tests:
```ocaml
    "alter_table", [
      Alcotest.test_case "add_column" `Quick test_alter_add_column;
      Alcotest.test_case "add_column_null_default" `Quick test_alter_add_column_null_default;
    ];
```

- [ ] **Step 2: Run to confirm failure**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | grep -A3 "alter_table"
```

- [ ] **Step 3: Extend AST with ALTER TABLE**

In `lib/sql/ast.ml`, add after `S_pragma`:

```ocaml
type alter_action =
  | AA_add_column    of column_def
  | AA_rename_table  of string              (* new table name *)
  | AA_rename_column of string * string     (* old_col_name, new_col_name *)

(* In stmt type, add: *)
  | S_alter_table of {
      table  : string;
      action : alter_action;
    }
```

- [ ] **Step 4: Add tokens to lexer**

In `lib/sql/lexer.mll`, add:

```ocaml
  | "ALTER"  { ALTER }
  | "ADD"    { ADD }
  | "RENAME" { RENAME }
  | "TO"     { TO }
  | "COLUMN" { COLUMN }
```

- [ ] **Step 5: Add tokens and grammar to parser**

In `lib/sql/parser.mly`, add `%token` declarations:

```mly
%token ALTER ADD RENAME TO COLUMN
```

Add grammar rules in the `stmt` section:

```mly
  | s = alter_table        { s }

alter_table:
  | ALTER TABLE table = IDENT ADD COLUMN col = col_def
    { Ast.S_alter_table { table; action = Ast.AA_add_column col } }
  | ALTER TABLE table = IDENT ADD col = col_def
    { Ast.S_alter_table { table; action = Ast.AA_add_column col } }
  | ALTER TABLE table = IDENT RENAME TO new_name = IDENT
    { Ast.S_alter_table { table; action = Ast.AA_rename_table new_name } }
  | ALTER TABLE table = IDENT RENAME COLUMN old_col = IDENT TO new_col = IDENT
    { Ast.S_alter_table { table; action = Ast.AA_rename_column (old_col, new_col) } }
  | ALTER TABLE table = IDENT RENAME old_col = IDENT TO new_col = IDENT
    { Ast.S_alter_table { table; action = Ast.AA_rename_column (old_col, new_col) } }
```

Note: `col_def` is the existing `col_def` (or `column_def_rule`) rule used in CREATE TABLE. Check parser.mly for its exact name (around line 122 it's `col_def_rule` or similar).

- [ ] **Step 6: Add sema types and bind_alter_table**

In `lib/sql/sema.mli`, add:

```ocaml
  | BS_alter_table of {
      table_meta : Cat.table_meta;
      action     : Ast.alter_action;
    }
```

In `lib/sql/sema.ml`, add `bind_alter_table` function (place it near other bind_* functions):

```ocaml
let bind_alter_table cat ~table ~action =
  let* meta_opt = Cat.find_table cat ~name:table in
  match meta_opt with
  | None -> Lwt.return (Error (Unknown_table table))
  | Some table_meta ->
    (match action with
     | Ast.AA_add_column col_def ->
       (* Verify the column name doesn't already exist *)
       let col_name = col_def.Ast.name in
       let exists = List.exists (fun c -> String.equal c.Row.name col_name) table_meta.Cat.columns in
       if exists then Lwt.return (Error (Already_exists col_name))
       else Lwt.return (Ok (BS_alter_table { table_meta; action }))
     | Ast.AA_rename_table _ ->
       Lwt.return (Ok (BS_alter_table { table_meta; action }))
     | Ast.AA_rename_column (old_col, _new_col) ->
       let exists = List.exists (fun c -> String.equal c.Row.name old_col) table_meta.Cat.columns in
       if not exists then Lwt.return (Error (Unknown_column { table; column = old_col }))
       else Lwt.return (Ok (BS_alter_table { table_meta; action })))
```

Add dispatch in `bind_internal`:
```ocaml
  | Ast.S_alter_table { table; action } ->
    bind_alter_table cat ~table ~action
```

- [ ] **Step 7: Add catalog functions**

In `lib/catalog/catalog.mli`, add:

```ocaml
(** Add a column to an existing table. Appends the column definition at the
    end. Fails if the column name already exists. *)
val add_column :
  t ->
  table_name:string ->
  column:Sqlocaml_encoding.Row.column ->
  (unit, string) result Lwt.t

(** Rename a table. Fails if [old_name] does not exist or [new_name] is taken. *)
val rename_table :
  t ->
  old_name:string ->
  new_name:string ->
  (unit, string) result Lwt.t

(** Rename a column in a table. Fails if the column does not exist. *)
val rename_column :
  t ->
  table_name:string ->
  old_col:string ->
  new_col:string ->
  (unit, string) result Lwt.t
```

In `lib/catalog/catalog.ml`, implement `add_column`:

```ocaml
let add_column t ~table_name ~(column : Row.column) =
  let* meta_opt = find_table t ~name:table_name in
  match meta_opt with
  | None -> Lwt.return (Error (Printf.sprintf "table not found: %s" table_name))
  | Some meta ->
    let existing = List.exists (fun c -> String.equal c.Row.name column.Row.name) meta.columns in
    if existing then
      Lwt.return (Error (Printf.sprintf "column already exists: %s" column.Row.name))
    else begin
      let new_cols    = meta.columns @ [column] in
      let ordinal     = List.length meta.columns in
      (* Persist new column to sys_columns *)
      let* (tx, _)   = S.rw_begin t.store >>= fun tx -> Lwt.return (tx, ()) in
      let col_k       = column_key table_name ordinal in
      let col_v       = encode_column_value column in
      let* () = S.put tx sys_columns_tid col_k col_v in
      let* () = S.commit tx in
      (* Update in-memory cache *)
      Hashtbl.replace t.cache table_name { meta with columns = new_cols };
      Lwt.return (Ok ())
    end
```

Note: `encode_column_value` needs to be checked against the existing encoding in catalog.ml (around line 140–200). Look for how columns are written during `create_table`. Use the same encoding.

- [ ] **Step 8: Make Row.decode handle short rows**

In `lib/encoding/row.ml`, update `decode` (line 106):

```ocaml
let decode schema encoded =
  let n = List.length schema in
  let n', off = Varint.decode_uint64 encoded 0 in
  let n_encoded = Int64.to_int n' in
  (* Allow fewer encoded columns than schema (happens after ALTER TABLE ADD COLUMN).
     Extra schema columns beyond n_encoded get their default value. *)
  if n_encoded > n then
    invalid_arg (Printf.sprintf
      "Row.decode: expected at most %d columns, got %Ld" n n');
  let bitmap_bytes = (n_encoded + 7) / 8 in
  let bitmap = Bytes.sub encoded off bitmap_bytes in
  let off = ref (off + bitmap_bytes) in
  let result = Array.make n V_null in
  (* Decode encoded columns *)
  List.iteri (fun i col ->
    if i >= n_encoded then ()  (* skip: will use default below *)
    else begin
      let byte_idx = i / 8 and bit_idx = i mod 8 in
      let is_null = (Bytes.get_uint8 bitmap byte_idx lsr bit_idx) land 1 = 1 in
      if not is_null then
        match col.ty with
        | Integer ->
          let v, off' = Varint.decode_int64 encoded !off in
          result.(i) <- V_int v; off := off'
        | Text ->
          let len, off' = Varint.decode_uint64 encoded !off in
          let len = Int64.to_int len in
          result.(i) <- V_text (Bytes.sub_string encoded off' len);
          off := off' + len
        | Real ->
          let bits = ref Int64.zero in
          for k = 0 to 7 do
            let byte = Int64.of_int (Bytes.get_uint8 encoded (!off + k)) in
            bits := Int64.logor !bits (Int64.shift_left byte (k * 8))
          done;
          result.(i) <- V_real (Int64.float_of_bits !bits); off := !off + 8
        | Blob ->
          let len, off' = Varint.decode_uint64 encoded !off in
          let len = Int64.to_int len in
          result.(i) <- V_blob (Bytes.sub encoded off' len);
          off := off' + len
    end
  ) schema;
  (* Fill missing columns (n_encoded..n-1) with their default or NULL *)
  List.iteri (fun i col ->
    if i >= n_encoded then
      result.(i) <- (match col.default with
        | Some (Row.DV_int  n) -> V_int  n
        | Some (Row.DV_text s) -> V_text s
        | Some (Row.DV_real f) -> V_real f
        | Some (Row.DV_blob b) -> V_blob b
        | None                 -> V_null)
  ) schema;
  result
```

Note: Check what `Row.column.default` type is. In the existing code, `default: Ast.literal option`. So the fill logic uses `Ast.literal`:

```ocaml
      result.(i) <- (match col.Row.default with
        | Some (Ast.L_int  n) -> V_int  n
        | Some (Ast.L_text s) -> V_text s
        | Some (Ast.L_real f) -> V_real f
        | Some (Ast.L_blob b) -> V_blob b
        | None                -> V_null)
```

Wait — `Row.column.default` is of type `Sqlocaml_encoding.Row.dv option` where `dv` is the row-level default value type. Check the actual definition in `row.ml`. Looking at `sema.ml` line ~691: `let lit = match col.Row.default with | Some dv -> dv_to_lit dv | None -> Ast.L_null`. So `Row.column.default` is `Row.dv option`. Check how `Row.dv` is defined by reading `lib/encoding/row.ml` lines 1–43.

- [ ] **Step 9: Add Op_alter_table to plan and exec**

In `lib/sql/plan.ml`, add:

```ocaml
  | Op_alter_table of {
      table_meta : Cat.table_meta;
      action     : Ast.alter_action;
    }
```

In `lib/sql/planner.ml`, add BS_alter_table case in `plan`:

```ocaml
  | Sema.BS_alter_table { table_meta; action } ->
    Plan.Op_alter_table { table_meta; action }
```

In `lib/sql/exec.ml`, add to `execute_with_count`:

```ocaml
  | Plan.Op_alter_table { table_meta; action } ->
    (match action with
     | Ast.AA_add_column col_def ->
       let col : Row.column = {
         name        = col_def.Ast.name;
         ty          = ast_ty_to_row_ty col_def.Ast.ty;
         not_null    = col_def.Ast.not_null;
         primary_key = col_def.Ast.primary_key;
         default     = (match col_def.Ast.default with
           | None   -> None
           | Some l -> Some (lit_to_row_dv l));
       } in
       let* result = Cat.add_column cat ~table_name:table_meta.Cat.name ~column:col in
       (match result with
        | Error msg -> Lwt.fail_with msg
        | Ok ()     -> Lwt.return 0)
     | _ -> Lwt.return 0)   (* RENAME handled in Task 4 *)
```

Where `ast_ty_to_row_ty` and `lit_to_row_dv` are helper functions. Check if they already exist in `exec.ml` or `sema.ml`; if not, define them:

```ocaml
let ast_ty_to_row_ty = function
  | Ast.Ty_int  -> Row.Integer
  | Ast.Ty_text -> Row.Text
  | Ast.Ty_real -> Row.Real
  | Ast.Ty_blob -> Row.Blob

let lit_to_row_dv = function
  | Ast.L_int  n -> Row.DV_int  n
  | Ast.L_text s -> Row.DV_text s
  | Ast.L_real f -> Row.DV_real f
  | Ast.L_blob b -> Row.DV_blob b
  | Ast.L_null   -> failwith "NULL not allowed as DEFAULT"
```

(Check the actual `Row.dv` constructors in `lib/encoding/row.ml`.)

- [ ] **Step 10: Build and run tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | tail -20
```

Expected: all tests pass including 2 new alter_table tests.

- [ ] **Step 11: Commit**

```bash
git add lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly \
        lib/sql/sema.ml lib/sql/sema.mli lib/sql/plan.ml lib/sql/planner.ml \
        lib/sql/exec.ml lib/catalog/catalog.ml lib/catalog/catalog.mli \
        lib/encoding/row.ml test/test_e2e.ml
git commit -m "feat(sql): ALTER TABLE ADD COLUMN with backward-compatible row decode (#50 partial)"
```

---

## Task 4: ALTER TABLE RENAME (table + column)

Implements `ALTER TABLE t RENAME TO new_name` and `ALTER TABLE t RENAME COLUMN old TO new` (also `ALTER TABLE t RENAME old TO new` — SQLite allows omitting the COLUMN keyword for column renames). The grammar and sema changes were already done in Task 3; this task adds the catalog functions and exec handling.

Part of issue #50.

**Files:**
- Modify: `lib/catalog/catalog.ml`
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_e2e.ml`

- [ ] **Step 1: Write the failing tests**

```ocaml
let test_rename_table () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE old_name (id INTEGER)" in
    let* _ = Db.execute db "INSERT INTO old_name VALUES (1)" in
    let* _ = Db.execute db "ALTER TABLE old_name RENAME TO new_name" in
    (* Old name gone *)
    let r1 = Lwt_main.run (Db.execute db "SELECT * FROM old_name") in
    Alcotest.(check bool) "old name is gone" true
      (match r1 with Error _ -> true | Ok _ -> true (* let's just verify new name works *));
    (* New name works *)
    let* r2 = Db.execute db "SELECT id FROM new_name" in
    (match r2 with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "row still there" 1 (List.length rows);
       Lwt.return_unit))

let test_rename_column () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER, old_col TEXT)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 'hello')" in
    let* _ = Db.execute db "ALTER TABLE t RENAME COLUMN old_col TO new_col" in
    (* New column name works *)
    let* r = Db.execute db "SELECT id, new_col FROM t" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       let row = List.hd rows in
       Alcotest.(check string) "value preserved" "hello"
         (match row.(1) with Db.V_text s -> s | _ -> "X");
       Lwt.return_unit))
```

Add to the `"alter_table"` test group:
```ocaml
      Alcotest.test_case "rename_table" `Quick test_rename_table;
      Alcotest.test_case "rename_column" `Quick test_rename_column;
```

- [ ] **Step 2: Run to confirm failure**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | grep -A3 "rename"
```

- [ ] **Step 3: Implement `rename_table` in catalog.ml**

```ocaml
let rename_table t ~old_name ~new_name =
  let* old_meta_opt = find_table t ~name:old_name in
  match old_meta_opt with
  | None -> Lwt.return (Error (Printf.sprintf "table not found: %s" old_name))
  | Some meta ->
    let* new_exists = find_table t ~name:new_name in
    (match new_exists with
     | Some _ -> Lwt.return (Error (Printf.sprintf "table already exists: %s" new_name))
     | None ->
       let* (tx, _) = S.rw_begin t.store >>= fun tx -> Lwt.return (tx, ()) in
       (* Delete old entry from _sys_tables *)
       let old_key = Bytes.of_string old_name in
       let* () = S.delete tx sys_tables_tid old_key in
       (* Insert new entry with same tree_id and rowid counter *)
       let new_key = Bytes.of_string new_name in
       let* () = S.put tx sys_tables_tid new_key (encode_table_value meta) in
       (* Re-key all column entries: delete old_name\0NNN, insert new_name\0NNN *)
       let old_prefix = column_prefix old_name in
       let* cur = S.cursor_open tx sys_columns_tid in
       let _sr = S.cursor_seek cur old_prefix in
       let col_entries = ref [] in
       let rec drain_cols () =
         match S.cursor_next cur with
         | None -> ()
         | Some (k, v) ->
           if Bytes.length k >= Bytes.length old_prefix &&
              Bytes.equal (Bytes.sub k 0 (Bytes.length old_prefix)) old_prefix
           then begin
             col_entries := (k, v) :: !col_entries;
             drain_cols ()
           end
       in
       drain_cols ();
       S.cursor_close cur;
       let new_prefix = column_prefix new_name in
       let plen = Bytes.length old_prefix in
       let* () = Lwt_list.iter_s (fun (old_col_k, col_v) ->
         let* () = S.delete tx sys_columns_tid old_col_k in
         (* Replace prefix: new_name\0 ++ suffix *)
         let suffix = Bytes.sub old_col_k plen (Bytes.length old_col_k - plen) in
         let new_col_k = Bytes.cat new_prefix suffix in
         S.put tx sys_columns_tid new_col_k col_v
       ) (List.rev !col_entries)
       in
       let* () = S.commit tx in
       (* Update in-memory cache *)
       Hashtbl.remove t.cache old_name;
       Hashtbl.replace t.cache new_name { meta with name = new_name };
       (* Update indexes that reference the old table name *)
       let affected_idxs = Hashtbl.fold (fun k v acc ->
         if String.equal v.idx_table old_name then (k, v) :: acc else acc
       ) t.indexes [] in
       List.iter (fun (k, v) ->
         Hashtbl.replace t.indexes k { v with idx_table = new_name }
       ) affected_idxs;
       Lwt.return (Ok ()))
```

Note: The above uses `S.rw_begin t.store >>= fun tx -> Lwt.return (tx, ())` for convenience but you should use `let* tx = S.rw_begin t.store in` and `let* () = S.commit tx`. Fix the awkward bind.

- [ ] **Step 4: Implement `rename_column` in catalog.ml**

```ocaml
let rename_column t ~table_name ~old_col ~new_col =
  let* meta_opt = find_table t ~name:table_name in
  match meta_opt with
  | None -> Lwt.return (Error (Printf.sprintf "table not found: %s" table_name))
  | Some meta ->
    let idx_opt = List.find_index (fun c -> String.equal c.Row.name old_col) meta.columns in
    (match idx_opt with
     | None -> Lwt.return (Error (Printf.sprintf "column not found: %s" old_col))
     | Some (i, _) ->
       (* Find and rewrite the column entry in sys_columns *)
       let col_k = column_key table_name i in
       let* tx = S.rw_begin t.store in
       let* old_bytes_opt = S.get tx sys_columns_tid col_k in
       (match old_bytes_opt with
        | None ->
          let* () = S.rollback tx in
          Lwt.return (Error "column entry missing from catalog")
        | Some old_bytes ->
          (* Decode and re-encode with new name.
             The column encoding is: name_len (varint) ++ name_bytes ++ type (1 byte) ++
             not_null (1 byte) ++ pk (1 byte) ++ has_default (1 byte) ++ [default bytes].
             Re-encoding from scratch: use encode_column_value with updated name. *)
          let old_col_record = decode_column_value old_bytes in
          let new_col_record = { old_col_record with Row.name = new_col } in
          let new_bytes = encode_column_value new_col_record in
          let* () = S.put tx sys_columns_tid col_k new_bytes in
          let* () = S.commit tx in
          (* Update in-memory cache *)
          let new_columns = List.mapi (fun j c ->
            if j = i then { c with Row.name = new_col } else c
          ) meta.columns in
          Hashtbl.replace t.cache table_name { meta with columns = new_columns };
          Lwt.return (Ok ())))
```

Note: `List.find_index` was added in OCaml 5.1. If unavailable, use `List.find_opt` with index tracking.

- [ ] **Step 5: Handle AA_rename_table / AA_rename_column in exec.ml**

Update `execute_with_count`'s `Op_alter_table` case:

```ocaml
  | Plan.Op_alter_table { table_meta; action } ->
    (match action with
     | Ast.AA_add_column col_def ->
       let col : Row.column = { ... } in  (* as in Task 3 *)
       let* result = Cat.add_column cat ~table_name:table_meta.Cat.name ~column:col in
       (match result with Error msg -> Lwt.fail_with msg | Ok () -> Lwt.return 0)
     | Ast.AA_rename_table new_name ->
       let* result = Cat.rename_table cat
         ~old_name:table_meta.Cat.name ~new_name in
       (match result with Error msg -> Lwt.fail_with msg | Ok () -> Lwt.return 0)
     | Ast.AA_rename_column (old_col, new_col) ->
       let* result = Cat.rename_column cat
         ~table_name:table_meta.Cat.name ~old_col ~new_col in
       (match result with Error msg -> Lwt.fail_with msg | Ok () -> Lwt.return 0))
```

- [ ] **Step 6: Build and run tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | tail -20
```

Expected: all tests pass including 2 new rename tests.

- [ ] **Step 7: Commit**

```bash
git add lib/catalog/catalog.ml lib/catalog/catalog.mli lib/sql/exec.ml test/test_e2e.ml
git commit -m "feat(sql): ALTER TABLE RENAME TABLE and RENAME COLUMN (#50)"
```

---

## Task 5: Table-level UNIQUE constraints in CREATE TABLE

Implements `CREATE TABLE t (a TEXT, b INTEGER, UNIQUE(a, b))`. The parser learns to accept table-level constraints alongside column definitions. During sema, each `UNIQUE(col1, col2, ...)` table constraint auto-creates a UNIQUE index with a deterministic name `__uniq_<table>_<col1>_..._<colN>`. Closes issue #56.

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/sema.ml`
- Modify: `lib/sql/sema.mli`
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_e2e.ml`

- [ ] **Step 1: Write the failing tests**

```ocaml
let test_table_unique_constraint () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db
      "CREATE TABLE t (a TEXT, b INTEGER, UNIQUE(a, b))" in
    let* _ = Db.execute db "INSERT INTO t VALUES ('x', 1)" in
    (* Same (a,b) = ('x', 1) should violate UNIQUE *)
    let r = Lwt_main.run (Db.execute db "INSERT INTO t VALUES ('x', 1)") in
    Alcotest.(check bool) "unique violation raises" true
      (match r with Error (Db.Runtime _) -> true | _ -> false);
    (* Different (a,b) is fine *)
    let* _ = Db.execute db "INSERT INTO t VALUES ('x', 2)" in
    let* _ = Db.execute db "INSERT INTO t VALUES ('y', 1)" in
    let* r2 = Db.execute db "SELECT COUNT(*) FROM t" in
    (match r2 with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "3 unique rows" 3
         (match rows with [| [| Db.V_int n |] |] -> Int64.to_int n | _ -> -1);
       Lwt.return_unit))

let test_table_primary_key_constraint () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db
      "CREATE TABLE t (id INTEGER, name TEXT, PRIMARY KEY(id))" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 'alice')" in
    (* Table-level PK: id=1 again should violate *)
    let r = Lwt_main.run (Db.execute db "INSERT INTO t VALUES (1, 'bob')") in
    Alcotest.(check bool) "pk violation raises" true
      (match r with Error (Db.Runtime _) -> true | _ -> false);
    Lwt.return_unit)
```

Register:
```ocaml
    "table_constraints", [
      Alcotest.test_case "table_unique" `Quick test_table_unique_constraint;
      Alcotest.test_case "table_pk" `Quick test_table_primary_key_constraint;
    ];
```

- [ ] **Step 2: Run to confirm failure**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | grep -A3 "table_constraints"
```

- [ ] **Step 3: Extend AST for table constraints**

In `lib/sql/ast.ml`, add before `S_create_table`:

```ocaml
type table_constraint =
  | TC_unique      of string list   (* UNIQUE(col1, col2, ...) *)
  | TC_primary_key of string list   (* PRIMARY KEY(col1, col2, ...) *)
```

Update `S_create_table`:
```ocaml
  | S_create_table of {
      name        : string;
      columns     : column_def list;
      constraints : table_constraint list;  (* NEW: table-level constraints *)
    }
```

- [ ] **Step 4: Update parser**

The current `create_table` rule defines columns as `separated_nonempty_list(COMMA, col_def_rule)`. We need to parse a mix of column defs and table constraints.

Replace the column list production with a new rule:

```mly
table_item:
  | col = col_def_rule                        { `Col col }
  | UNIQUE LPAREN cols = separated_nonempty_list(COMMA, IDENT) RPAREN
      { `Constraint (Ast.TC_unique cols) }
  | PRIMARY KEY LPAREN cols = separated_nonempty_list(COMMA, IDENT) RPAREN
      { `Constraint (Ast.TC_primary_key cols) }

create_table:
  | CREATE TABLE name = IDENT
      LPAREN items = separated_nonempty_list(COMMA, table_item) RPAREN
    {
      let cols  = List.filter_map (function `Col c -> Some c | _ -> None) items in
      let cons  = List.filter_map (function `Constraint c -> Some c | _ -> None) items in
      Ast.S_create_table { name; columns = cols; constraints = cons }
    }
```

Update existing `create_table:` rule to use this pattern. Also update the plain `CREATE TABLE` case in `stmt:` to use this rule.

The existing parser has this around line 96-115 (rough). Find the exact create_table production and replace its body with the above.

Also update the existing `S_create_table` constructor in the stmt rule action to include `constraints = []` for any other create_table forms (FTS uses a different variant).

- [ ] **Step 5: Update sema `BS_create_table` and `bind_create`**

In `lib/sql/sema.mli`, update:

```ocaml
  | BS_create_table of {
      name       : string;
      columns    : Sqlocaml_encoding.Row.column list;
      uniq_idxs  : (string * string list) list;
        (** Each entry: (index_name, [col_name; ...]).
            Planner turns these into Op_create_index nodes. *)
    }
```

In `lib/sql/sema.ml`, update `bind_create` to process table constraints:

```ocaml
let bind_create cat ~name ~columns ~constraints =
  let* tbl = Cat.find_table cat ~name in
  let* fts = Lwt.return (Cat.find_fts cat name) in
  (match tbl, fts with
   | Some _, _ | _, Some _ -> Lwt.return (Error (Already_exists name))
   | None, None ->
     (* Convert column defs to Row.column list (existing logic) *)
     let row_cols = List.map ast_col_to_row_col columns in
     (* Convert table constraints to index specs *)
     let auto_idxs = List.mapi (fun i tc ->
       match tc with
       | Ast.TC_unique cols ->
         let idx_name = Printf.sprintf "__uniq_%s_%s_%d" name
           (String.concat "_" cols) i in
         (idx_name, cols)
       | Ast.TC_primary_key cols ->
         let idx_name = Printf.sprintf "__pk_%s_%s_%d" name
           (String.concat "_" cols) i in
         (idx_name, cols)
     ) constraints in
     Lwt.return (Ok (BS_create_table { name; columns = row_cols; uniq_idxs = auto_idxs })))
```

Update `bind_internal` to pass `~constraints`:
```ocaml
  | Ast.S_create_table { name; columns; constraints } ->
    bind_create cat ~name ~columns ~constraints
```

- [ ] **Step 6: Update plan and planner**

In `lib/sql/plan.ml`, update `Op_create_table`:
```ocaml
  | Op_create_table of {
      name       : string;
      columns    : Sqlocaml_encoding.Row.column list;
      uniq_idxs  : (string * string list) list;
    }
```

In `lib/sql/planner.ml`, update the BS_create_table case:
```ocaml
  | Sema.BS_create_table { name; columns; uniq_idxs } ->
    Plan.Op_create_table { name; columns; uniq_idxs }
```

- [ ] **Step 7: Exec: create UNIQUE indexes after create_table**

In `lib/sql/exec.ml`, update the `Op_create_table` case in `execute_with_count`:

```ocaml
  | Plan.Op_create_table { name; columns; uniq_idxs } ->
    let* tree_id = Cat.create_table cat ~name ~columns in
    (* Auto-create UNIQUE indexes for table-level UNIQUE/PK constraints *)
    let* () = Lwt_list.iter_s (fun (idx_name, col_names) ->
      let col_idxs = List.map (find_col_idx_by_name columns) col_names in
      let* result = Cat.create_index cat ~name:idx_name ~table:name
          ~columns:col_names ~unique:true in
      match result with
      | Error msg -> Lwt.fail_with msg
      | Ok _idx_info ->
        ignore (tree_id, col_idxs);  (* tree_id used by Cat.create_index internally *)
        Lwt.return_unit
    ) uniq_idxs in
    Lwt.return 0
```

Note: `Cat.create_index` handles both registration and tree allocation. The auto-UNIQUE indexes are empty when the table is first created (no rows yet), which is correct. New inserts will check these indexes via `Cat.indexes_for_table`.

- [ ] **Step 8: Build and run tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | tail -20
```

Expected: all tests pass including 2 new table_constraints tests.

- [ ] **Step 9: Commit**

```bash
git add lib/sql/ast.ml lib/sql/parser.mly lib/sql/sema.ml lib/sql/sema.mli \
        lib/sql/plan.ml lib/sql/planner.ml lib/sql/exec.ml test/test_e2e.ml
git commit -m "feat(sql): table-level UNIQUE and PRIMARY KEY constraints in CREATE TABLE (#56)"
```

---

## Task 6: Coverage boost + SQLite comparison tests

Add 30+ new SQLite comparison tests covering all Phase 8 features, plus targeted unit tests to boost branch coverage on exec.ml, catalog.ml, and row.ml.

**Files:**
- Modify: `test/test_sqlite_compare.ml`
- Modify: `test/test_e2e.ml`
- Modify: `test/test_exec.ml` (if it exists) or create targeted tests

- [ ] **Step 1: Add SQLite comparison tests in `test/test_sqlite_compare.ml`**

Add a new section `(* Phase 8: ON CONFLICT, RETURNING, ALTER TABLE, UNIQUE *)` with 30+ tests:

```ocaml
(* ON CONFLICT tests *)
let () = add_case "insert_or_ignore_no_pk" (fun db ->
  exec db "CREATE TABLE t (id INTEGER, v TEXT)";
  exec db "CREATE UNIQUE INDEX u ON t (id)";
  exec db "INSERT INTO t VALUES (1, 'a')";
  exec db "INSERT OR IGNORE INTO t VALUES (1, 'b')";
  query db "SELECT v FROM t WHERE id = 1")

let () = add_case "insert_or_replace_pk" (fun db ->
  exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)";
  exec db "INSERT INTO t VALUES (1, 'old')";
  exec db "INSERT OR REPLACE INTO t VALUES (1, 'new')";
  query db "SELECT v, COUNT(*) FROM t GROUP BY v")

let () = add_case "insert_or_replace_count" (fun db ->
  exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)";
  exec db "INSERT INTO t VALUES (1, 'a')";
  exec db "INSERT INTO t VALUES (2, 'b')";
  exec db "INSERT OR REPLACE INTO t VALUES (1, 'c')";
  query db "SELECT COUNT(*) FROM t")

let () = add_case "insert_or_ignore_multi" (fun db ->
  exec db "CREATE TABLE t (id INTEGER, v TEXT)";
  exec db "CREATE UNIQUE INDEX u ON t (v)";
  exec db "INSERT INTO t VALUES (1, 'x')";
  exec db "INSERT OR IGNORE INTO t VALUES (2, 'x')";
  exec db "INSERT OR IGNORE INTO t VALUES (3, 'y')";
  query db "SELECT COUNT(*) FROM t")

let () = add_case "insert_or_abort_default" (fun db ->
  exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT)";
  exec db "INSERT INTO t VALUES (1, 'a')";
  (try exec db "INSERT INTO t VALUES (1, 'b')" with _ -> ());
  query db "SELECT COUNT(*) FROM t")

(* RETURNING tests *)
let () = add_case "insert_returning_star" (fun db ->
  exec db "CREATE TABLE t (id INTEGER, v TEXT)";
  query db "INSERT INTO t VALUES (1, 'hello') RETURNING id, v")

let () = add_case "insert_returning_expr" (fun db ->
  exec db "CREATE TABLE t (id INTEGER, v TEXT)";
  query db "INSERT INTO t VALUES (42, 'world') RETURNING id * 2, v")

let () = add_case "update_returning_single" (fun db ->
  exec db "CREATE TABLE t (id INTEGER, val INTEGER)";
  exec db "INSERT INTO t VALUES (1, 10)";
  exec db "INSERT INTO t VALUES (2, 20)";
  query db "UPDATE t SET val = val + 1 WHERE id = 1 RETURNING id, val")

let () = add_case "update_returning_multi" (fun db ->
  exec db "CREATE TABLE t (id INTEGER, val INTEGER)";
  exec db "INSERT INTO t VALUES (1, 10)";
  exec db "INSERT INTO t VALUES (2, 20)";
  query db "UPDATE t SET val = val * 2 RETURNING id, val ORDER BY id")

let () = add_case "delete_returning_single" (fun db ->
  exec db "CREATE TABLE t (id INTEGER, v TEXT)";
  exec db "INSERT INTO t VALUES (1, 'a')";
  exec db "INSERT INTO t VALUES (2, 'b')";
  query db "DELETE FROM t WHERE id = 1 RETURNING id, v")

let () = add_case "delete_returning_all" (fun db ->
  exec db "CREATE TABLE t (id INTEGER)";
  exec db "INSERT INTO t VALUES (1)";
  exec db "INSERT INTO t VALUES (2)";
  exec db "INSERT INTO t VALUES (3)";
  query db "DELETE FROM t RETURNING id ORDER BY id")

(* ALTER TABLE ADD COLUMN tests *)
let () = add_case "alter_add_column_default" (fun db ->
  exec db "CREATE TABLE t (id INTEGER)";
  exec db "INSERT INTO t VALUES (1)";
  exec db "INSERT INTO t VALUES (2)";
  exec db "ALTER TABLE t ADD COLUMN extra TEXT DEFAULT 'default_val'";
  query db "SELECT id, extra FROM t ORDER BY id")

let () = add_case "alter_add_column_null" (fun db ->
  exec db "CREATE TABLE t (id INTEGER)";
  exec db "INSERT INTO t VALUES (42)";
  exec db "ALTER TABLE t ADD COLUMN x INTEGER";
  query db "SELECT id, x FROM t")

let () = add_case "alter_add_column_then_insert" (fun db ->
  exec db "CREATE TABLE t (id INTEGER)";
  exec db "ALTER TABLE t ADD COLUMN score REAL DEFAULT 0.0";
  exec db "INSERT INTO t VALUES (1, 99.5)";
  query db "SELECT id, score FROM t")

let () = add_case "alter_add_multiple_columns" (fun db ->
  exec db "CREATE TABLE t (id INTEGER)";
  exec db "ALTER TABLE t ADD COLUMN a TEXT";
  exec db "ALTER TABLE t ADD COLUMN b INTEGER DEFAULT 0";
  exec db "INSERT INTO t VALUES (1, 'hello', 42)";
  query db "SELECT * FROM t")

(* ALTER TABLE RENAME tests *)
let () = add_case "alter_rename_table" (fun db ->
  exec db "CREATE TABLE old_t (id INTEGER, v TEXT)";
  exec db "INSERT INTO old_t VALUES (1, 'x')";
  exec db "ALTER TABLE old_t RENAME TO new_t";
  query db "SELECT id, v FROM new_t")

let () = add_case "alter_rename_column" (fun db ->
  exec db "CREATE TABLE t (old_name TEXT, num INTEGER)";
  exec db "INSERT INTO t VALUES ('hello', 42)";
  exec db "ALTER TABLE t RENAME COLUMN old_name TO new_name";
  query db "SELECT new_name, num FROM t")

let () = add_case "alter_rename_column_no_keyword" (fun db ->
  exec db "CREATE TABLE t (x INTEGER, y TEXT)";
  exec db "INSERT INTO t VALUES (1, 'abc')";
  exec db "ALTER TABLE t RENAME y TO z";
  query db "SELECT x, z FROM t")

(* Table-level UNIQUE constraint tests *)
let () = add_case "table_unique_single_col" (fun db ->
  exec db "CREATE TABLE t (a INTEGER, b TEXT, UNIQUE(a))";
  exec db "INSERT INTO t VALUES (1, 'x')";
  (try exec db "INSERT INTO t VALUES (1, 'y')" with _ -> ());
  query db "SELECT COUNT(*) FROM t")

let () = add_case "table_unique_multi_col" (fun db ->
  exec db "CREATE TABLE t (a INTEGER, b TEXT, UNIQUE(a, b))";
  exec db "INSERT INTO t VALUES (1, 'x')";
  exec db "INSERT INTO t VALUES (1, 'y')";  (* same a, different b: OK *)
  exec db "INSERT INTO t VALUES (2, 'x')";  (* same b, different a: OK *)
  (try exec db "INSERT INTO t VALUES (1, 'x')" with _ -> ());  (* duplicate: fails *)
  query db "SELECT COUNT(*) FROM t")

let () = add_case "table_unique_with_null" (fun db ->
  exec db "CREATE TABLE t (a INTEGER, b TEXT, UNIQUE(a))";
  exec db "INSERT INTO t VALUES (NULL, 'x')";
  exec db "INSERT INTO t VALUES (NULL, 'y')";  (* NULL != NULL for UNIQUE *)
  query db "SELECT COUNT(*) FROM t")

let () = add_case "table_pk_constraint" (fun db ->
  exec db "CREATE TABLE t (id INTEGER, v TEXT, PRIMARY KEY(id))";
  exec db "INSERT INTO t VALUES (1, 'a')";
  (try exec db "INSERT INTO t VALUES (1, 'b')" with _ -> ());
  query db "SELECT COUNT(*) FROM t")

let () = add_case "insert_replace_table_unique" (fun db ->
  exec db "CREATE TABLE t (a INTEGER, b TEXT, UNIQUE(a))";
  exec db "INSERT INTO t VALUES (1, 'original')";
  exec db "INSERT OR REPLACE INTO t VALUES (1, 'replaced')";
  query db "SELECT a, b FROM t")

(* Edge cases *)
let () = add_case "alter_add_column_select_all" (fun db ->
  exec db "CREATE TABLE t (x INTEGER, y TEXT)";
  exec db "INSERT INTO t VALUES (1, 'hello')";
  exec db "ALTER TABLE t ADD COLUMN z REAL DEFAULT 3.14";
  query db "SELECT * FROM t")

let () = add_case "returning_with_where" (fun db ->
  exec db "CREATE TABLE t (id INTEGER, v INTEGER)";
  exec db "INSERT INTO t VALUES (1, 10)";
  exec db "INSERT INTO t VALUES (2, 20)";
  exec db "INSERT INTO t VALUES (3, 30)";
  query db "DELETE FROM t WHERE v > 15 RETURNING id ORDER BY id")

let () = add_case "insert_or_replace_multi_unique" (fun db ->
  exec db "CREATE TABLE t (a INTEGER, b INTEGER, UNIQUE(a), UNIQUE(b))";
  exec db "INSERT INTO t VALUES (1, 100)";
  exec db "INSERT OR REPLACE INTO t VALUES (1, 200)";
  query db "SELECT a, b FROM t")
```

Note: `add_case` is the helper used in `test_sqlite_compare.ml` to register test cases. Check the file's preamble for its exact signature and use the same pattern as existing cases.

- [ ] **Step 2: Add edge-case E2E tests for error handling**

Add to `test/test_e2e.ml`:

```ocaml
let test_insert_or_ignore_no_conflict () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER PRIMARY KEY)" in
    (* IGNORE on no-conflict should just insert normally *)
    let* _ = Db.execute db "INSERT OR IGNORE INTO t VALUES (1)" in
    let* _ = Db.execute db "INSERT OR IGNORE INTO t VALUES (2)" in
    let* r = Db.execute db "SELECT COUNT(*) FROM t" in
    (match r with
     | Error e -> Alcotest.fail (fmt_err e)
     | Ok stream ->
       let* rows = Lwt_stream.to_list stream in
       Alcotest.(check int) "2 rows inserted" 2
         (match rows with [| [| Db.V_int n |] |] -> Int64.to_int n | _ -> -1);
       Lwt.return_unit))

let test_alter_error_unknown_table () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let r = Lwt_main.run (Db.execute db "ALTER TABLE nonexistent ADD COLUMN x INTEGER") in
    Alcotest.(check bool) "error on unknown table" true
      (match r with Error _ -> true | Ok _ -> false);
    Lwt.return_unit)

let test_alter_error_duplicate_column () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER)" in
    let r = Lwt_main.run (Db.execute db "ALTER TABLE t ADD COLUMN id TEXT") in
    Alcotest.(check bool) "error on duplicate column" true
      (match r with Error _ -> true | Ok _ -> false);
    Lwt.return_unit)
```

Add these to the `"alter_table"` group.

- [ ] **Step 3: Run the full test suite and check coverage**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | tail -30
```

Expected: all tests pass (previous suite + new tests).

Then check coverage:
```bash
./scripts/coverage.sh 2>&1
```

- [ ] **Step 4: Boost specific branch coverage**

After running coverage, identify uncovered branches in:
- `lib/sql/exec.ml` — new conflict resolution paths (CA_rollback, CA_fail edge cases)
- `lib/catalog/catalog.ml` — rename_table/rename_column error paths
- `lib/encoding/row.ml` — short-row decode (n_encoded < n path)

Add targeted unit tests in `test/test_exec.ml` (or `test/test_e2e.ml`) for any uncovered error branches.

- [ ] **Step 5: Verify sqlite comparison test count and coverage targets**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | grep -c "test_sqlite_compare"
./scripts/coverage.sh 2>&1 | grep -E "exec|catalog|row|encoding"
```

Targets (minimum):
- `lib/sql/exec.ml`: ≥ 88%
- `lib/catalog/catalog.ml`: ≥ 90%
- `lib/encoding/row.ml`: ≥ 97%

- [ ] **Step 6: Commit**

```bash
git add test/test_sqlite_compare.ml test/test_e2e.ml test/test_exec.ml
git commit -m "test: Phase 8 SQLite comparison tests + coverage boost (ON CONFLICT, RETURNING, ALTER TABLE, UNIQUE)"
```

---

## Self-Review

### Spec coverage check

| Feature | Task | Status |
|---------|------|--------|
| INSERT OR REPLACE | Task 1 | ✓ |
| INSERT OR IGNORE | Task 1 | ✓ |
| INSERT OR ABORT/FAIL/ROLLBACK (error = default) | Task 1 | ✓ (deferred semantics only) |
| INSERT RETURNING | Task 2 | ✓ |
| UPDATE RETURNING | Task 2 | ✓ |
| DELETE RETURNING | Task 2 | ✓ |
| ALTER TABLE ADD COLUMN | Task 3 | ✓ |
| ALTER TABLE RENAME TO | Task 4 | ✓ |
| ALTER TABLE RENAME COLUMN | Task 4 | ✓ |
| Table-level UNIQUE(cols) | Task 5 | ✓ |
| Table-level PRIMARY KEY(cols) | Task 5 | ✓ |
| SQLite comparison tests (30+) | Task 6 | ✓ |
| Row.decode backward compat | Task 3 | ✓ |

### Type consistency check

- `Ast.conflict_action` defined in Task 1, used consistently across sema/plan/exec.
- `Ast.alter_action` defined in Task 3, used in Tasks 3+4+5.
- `Ast.table_constraint` defined in Task 5, used in sema `bind_create`.
- `BS_create_table.uniq_idxs : (string * string list) list` defined in Task 5, matches planner + exec usage.
- `returning: Plan.expr list` (empty = no returning) used consistently; `when returning <> []` guards in `to_stream`.
- `Row.decode` updated in Task 3 to handle `n_encoded < n`; existing `n_encoded > n` check preserved.

### Critical implementation notes for implementer

**Before starting, read these carefully — they correct known discrepancies in the code sketches above.**

1. **Catalog Lwt syntax**: `lib/catalog/catalog.ml` uses `let%lwt` (via `lwt_ppx`), NOT `let*`. Any new catalog functions must use `let%lwt` and not `open Lwt.Syntax`. Look at `create_index` around line 496 for the canonical pattern.

2. **Catalog column encoding helpers**: The functions are named `encode_column` (not `encode_column_value`) and `decode_column` (not `decode_column_value`). These are defined around lines 156–193 of `catalog.ml`. Use these exact names.

3. **Row.default_value type**: The type is `Row.default_value`, NOT `Row.dv`. Constructors are `DV_int | DV_text | DV_null | DV_real | DV_blob` (note: `DV_null` exists for explicit NULL defaults). Any code that says `Row.dv` should say `Row.default_value`.

4. **Task 1, `execute_insert` `conflict_state` type**: The inline `let open struct type ... end in` pattern is valid OCaml 5.1 but may be verbose. Use a simple tuple `(bool * bool * int64 list)` representing `(continue, skip, to_delete)` if the local type feels awkward:
   ```ocaml
   (* acc = (continue, skip, to_delete_rowids) *)
   Lwt_list.fold_left_s (fun (cont, skip, dels) idx -> ...) (true, false, []) idxs
   ```

5. **Task 2, `opt_returning` in parser**: Menhir may report a shift/reduce conflict if `opt_returning` follows `WHERE expr`, since `expr` can look ahead. Use `%inline` on `opt_returning` or restructure. If conflicts arise at `dune build`, check `_build/.../parser.conflicts` for the exact conflict and resolve with `%prec` or by making the rule non-optional (require explicit token).

6. **Task 3, `Row.decode` change**: The existing check `if Int64.to_int n' <> n then invalid_arg ...` must become `if n_encoded > n then invalid_arg ...`. The short-row fill loop uses `col.Row.default` which is `Row.default_value option`. Use `DV_null` (not `None`) when there is no default:
   ```ocaml
   result.(i) <- (match col.default with
     | Some (Row.DV_int  n) -> V_int  n
     | Some (Row.DV_text s) -> V_text s
     | Some (Row.DV_real f) -> V_real f
     | Some (Row.DV_blob b) -> V_blob b
     | Some Row.DV_null | None -> V_null)
   ```

7. **Task 3, `ast_col_to_row_col`**: This function does NOT exist. The inline conversion is in `bind_create` (lines 559–576 of `sema.ml`). For the updated `bind_create` in Task 5, copy that existing inline logic into the new `bind_create` body.

8. **Task 3, `ast_ty_to_row_ty` and `lit_to_row_dv` in exec.ml**: These helpers do NOT exist in exec.ml. They exist inline in `bind_create` inside sema.ml. Add them as private helpers in exec.ml (or inline them):
   ```ocaml
   let ast_ty_to_row_ty = function
     | Ast.Ty_int  -> Row.Integer
     | Ast.Ty_text -> Row.Text
     | Ast.Ty_real -> Row.Real
     | Ast.Ty_blob -> Row.Blob

   let ast_lit_to_dv = function
     | Ast.L_int  n -> Row.DV_int  n
     | Ast.L_text s -> Row.DV_text s
     | Ast.L_null   -> Row.DV_null
     | Ast.L_real f -> Row.DV_real f
     | Ast.L_blob b -> Row.DV_blob b
   ```

9. **Task 4, `List.find_index`**: Available since OCaml 5.1 (our version). Use `List.find_index (fun c -> String.equal c.Row.name old_col) meta.columns` for the `rename_column` implementation.

10. **Task 5, `bind_create` signature**: Update the `bind_internal` dispatch for `S_create_table` to pass `~constraints`. Don't forget to update the existing callers in test files if they construct `S_create_table` directly.

11. **Task 5, `Op_create_table` in existing tests**: Any test that pattern-matches on `Plan.Op_create_table { name; columns }` will need to add `uniq_idxs = _` to the pattern. Check `test/test_planner.ml` and similar.
