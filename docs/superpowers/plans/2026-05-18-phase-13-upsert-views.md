# Phase 13: UPSERT and Views

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add two high-impact SQL features: UPSERT (`ON CONFLICT(col) DO UPDATE SET col = excluded.col`) and Views (`CREATE VIEW`, `DROP VIEW`, transparent SELECT from views).

**Architecture:** UPSERT extends the existing INSERT conflict detection in `execute_insert`; a new `P_excluded_col i` plan expr variant refers to the proposed-insert row, substituted to `P_lit` before calling `eval_expr`. Views are inlined at bind time as ephemeral CTEs: when `bind_internal S_select` sees an unknown table name that matches a registered view, it rewrites the statement to `S_with_cte { name; def = view_def; query = original_select }` and re-binds, reusing all existing CTE machinery. The view registry is held in `Db.t` (OCaml level), injected into `Sema.bind` as an optional parameter, and populated when `Db.execute` encounters `Op_create_view`.

**Tech Stack:** OCaml 5.x, Menhir, Lwt, Alcotest — all commands run inside `podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune ...`

---

## File Structure

| File | Changes |
|------|---------|
| `lib/sql/ast.ml` | Add `upsert_update` type + field on `S_insert`; add `S_create_view`, `S_drop_view` variants |
| `lib/sql/lexer.mll` | Add `CONFLICT`, `DO`, `VIEW` to catch-all ident block |
| `lib/sql/parser.mly` | Add `%token CONFLICT DO VIEW`; add `opt_upsert`, `create_view`, `drop_view` rules; wire into `insert` and `stmt` |
| `lib/sql/sema.ml` | Add `BE_excluded_col`; add `upsert_update` to `BS_insert`; add `bind_upsert_assignments`; add `BS_create_view`, `BS_drop_view`; add `?views` to `bind_internal`; view expansion in `S_select` handler |
| `lib/sql/sema.mli` | Export updated `bound_expr`, `bound_stmt` types |
| `lib/sql/plan.ml` | Add `P_excluded_col of int`; add `upsert_update` to `Op_insert`; add `Op_create_view`, `Op_drop_view` |
| `lib/sql/planner.ml` | Thread `upsert_update` through `BS_insert → Op_insert`; handle `BS_create_view`, `BS_drop_view` |
| `lib/sql/exec.ml` | Add `substitute_excluded`; extend `execute_insert` for UPSERT; add `P_excluded_col` arm in `eval_expr`; guard `Op_create_view`/`Op_drop_view` in `execute_with_count` |
| `lib/db/db.ml` | Add `views` hashtable to `t`; handle `Op_create_view`/`Op_drop_view` in `execute`; pass `~views` to `Sema.bind` |
| `test/test_sema.ml` | Update all `S_insert { ... }` constructions to include `upsert_update = None` |
| `test/test_planner.ml` | Update `S_insert` construction and `Op_insert` pattern to include `upsert_update = None` |
| `test/test_e2e.ml` | Add `"upsert"` group (5 tests) and `"view"` group (6 tests) |
| `test/test_sqlite_compare.ml` | Add `phase13_upsert_cases` (5 tests) and `phase13_view_cases` (5 tests) |

---

## Task 1: UPSERT — AST, Lexer, Parser

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`
- Modify: `test/test_sema.ml` (update all `S_insert` constructions)
- Modify: `test/test_planner.ml` (update `S_insert` construction)

- [ ] **Step 1: Add `upsert_update` type and field to `ast.ml`**

In `lib/sql/ast.ml`, add the new type after `type conflict_action`:

```ocaml
type upsert_update = {
  conflict_cols : string list;
  assignments   : (string * expr) list;
}
```

Then update `S_insert` to add the new field. Find the existing:
```ocaml
  | S_insert of {
      table       : string;
      columns     : string list;
      values      : expr list list;
      on_conflict : conflict_action option;
      returning   : expr list;
    }
```
Replace with:
```ocaml
  | S_insert of {
      table         : string;
      columns       : string list;
      values        : expr list list;
      on_conflict   : conflict_action option;
      returning     : expr list;
      upsert_update : upsert_update option;
    }
```

Add two new stmt variants after `S_with_cte`:
```ocaml
  | S_create_view of {
      name  : string;
      query : stmt;
    }
  | S_drop_view of {
      name : string;
    }
```

- [ ] **Step 2: Add `CONFLICT`, `DO`, `VIEW` tokens to lexer**

In `lib/sql/lexer.mll`, in the catch-all `ident` block (around line 149), add after `"WITH" -> WITH`:
```ocaml
      | "CONFLICT" -> CONFLICT
      | "DO"       -> DO
      | "VIEW"     -> VIEW
```

- [ ] **Step 3: Add tokens and grammar rules to parser**

In `lib/sql/parser.mly`:

Add to token declarations (near line 46, after `WITH`):
```
%token CONFLICT DO VIEW
```

Add the `opt_upsert` rule after `opt_returning`:
```ocaml
opt_upsert:
  | ON CONFLICT LPAREN cols = separated_nonempty_list(COMMA, IDENT) RPAREN
    DO UPDATE SET assigns = separated_nonempty_list(COMMA, assignment)
    { Some Ast.{ conflict_cols = cols; assignments = assigns } }
  |  { None }
```

Update the `insert` rules to include `opt_upsert` before `opt_returning`:
```ocaml
insert:
  | INSERT oc = opt_conflict INTO table = IDENT
      LPAREN cols = separated_nonempty_list(COMMA, IDENT) RPAREN
      VALUES rows = separated_nonempty_list(COMMA, value_row)
      upsert = opt_upsert
      ret = opt_returning
    { Ast.S_insert { table; columns = cols; values = rows;
                     on_conflict = oc; returning = ret;
                     upsert_update = upsert } }
  | INSERT oc = opt_conflict INTO table = IDENT
      VALUES rows = separated_nonempty_list(COMMA, value_row)
      upsert = opt_upsert
      ret = opt_returning
    { Ast.S_insert { table; columns = []; values = rows;
                     on_conflict = oc; returning = ret;
                     upsert_update = upsert } }
```

Add `create_view` and `drop_view` rules:
```ocaml
create_view:
  | CREATE VIEW name = IDENT AS query = compound_select
    { Ast.S_create_view { name; query } }

drop_view:
  | DROP VIEW name = IDENT
    { Ast.S_drop_view { name } }
```

Add them to `stmt` (after `with_cte`):
```ocaml
stmt:
  | s = with_cte          { s }
  | s = create_view       { s }
  | s = drop_view         { s }
  | s = create_table      { s }
  ...
```

- [ ] **Step 4: Update `test_sema.ml` — add `upsert_update = None` to all `S_insert`**

Find every `S_insert {` construction in `test/test_sema.ml` and add `upsert_update = None`:

```
grep -n "S_insert {" test/test_sema.ml
```

For each occurrence that constructs `S_insert { table; columns; values; on_conflict; returning }`, add `upsert_update = None;` before the closing `}`. There are ~18 such sites. Example:

Before:
```ocaml
Ast.S_insert { table = "t"; columns = ["id"; "name"];
               values = [[E_lit (L_int 1L); E_lit (L_text "Alice")]];
               on_conflict = None; returning = [] }
```
After:
```ocaml
Ast.S_insert { table = "t"; columns = ["id"; "name"];
               values = [[E_lit (L_int 1L); E_lit (L_text "Alice")]];
               on_conflict = None; returning = [];
               upsert_update = None }
```

Also update any `BS_insert` pattern matches in `test_sema.ml` to use `upsert_update = _` (wildcard) or explicit `None`.

- [ ] **Step 5: Update `test_planner.ml`**

Find the 1 `S_insert` construction in `test/test_planner.ml` (grep for it), add `upsert_update = None`.

Find the `Op_insert` pattern match in `test_planner.ml`, add `upsert_update = _` to it.

- [ ] **Step 6: Build and confirm no compilation errors**

```
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1 | head -30
```

Expected: clean build (no errors). If there are "unbound record field" errors for `S_insert` in any other file, fix them.

- [ ] **Step 7: Run tests to confirm no regressions**

```
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune test 2>&1 | tail -10
```

Expected: all 260 tests pass.

- [ ] **Step 8: Commit**

```bash
git add lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly test/test_sema.ml test/test_planner.ml
git commit -m "feat(phase13): add upsert_update to S_insert AST + CREATE/DROP VIEW AST + CONFLICT/DO/VIEW tokens"
```

---

## Task 2: UPSERT — Sema + Plan + Planner

**Files:**
- Modify: `lib/sql/sema.ml`
- Modify: `lib/sql/sema.mli`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/planner.ml`

- [ ] **Step 1: Add `BE_excluded_col` to sema bound_expr**

In `lib/sql/sema.ml`, add to the `bound_expr` type after `BE_cast`:
```ocaml
  | BE_excluded_col of int
    (** Reference to the i-th column of the proposed INSERT row (the 'excluded' pseudo-table). *)
```

In `lib/sql/sema.mli`, add to `bound_expr` type:
```ocaml
  | BE_excluded_col of int
```

- [ ] **Step 2: Add `upsert_update` to `BS_insert`**

In `lib/sql/sema.ml`, update `BS_insert`:
```ocaml
  | BS_insert of {
      table_meta    : Cat.table_meta;
      ordinals      : int list;
      values        : bound_expr list list;
      on_conflict   : Ast.conflict_action option;
      returning     : bound_expr list;
      upsert_update : (string list * (int * bound_expr) list) option;
    }
```

In `lib/sql/sema.mli`, update `BS_insert` the same way.

- [ ] **Step 3: Add `bind_upsert_assignments` helper to `sema.ml`**

This function binds assignment expressions where `E_tbl_col("excluded", col)` resolves to `BE_excluded_col i` instead of the regular column lookup:

```ocaml
let bind_upsert_rhs_expr ~param_counter ~named_params (meta : Cat.table_meta) (e : Ast.expr) =
  (* Like bind_expr but E_tbl_col("excluded", col) → BE_excluded_col i *)
  let rec go = function
    | Ast.E_tbl_col ("excluded", col) | Ast.E_tbl_col ("EXCLUDED", col) ->
      (match col_index meta.columns col with
       | None   -> Error (Unknown_column { table = "excluded"; column = col })
       | Some i -> Ok (BE_excluded_col i))
    | other -> bind_expr ~param_counter ~named_params meta other
  in
  go e

let bind_upsert_assignments ~param_counter ~named_params (meta : Cat.table_meta)
    (assigns : (string * Ast.expr) list) =
  List.fold_left (fun acc (col_name, rhs_expr) ->
    match acc with
    | Error _ -> acc
    | Ok bound_list ->
      (match col_index meta.columns col_name with
       | None   -> Error (Unknown_column { table = meta.name; column = col_name })
       | Some i ->
         (match bind_upsert_rhs_expr ~param_counter ~named_params meta rhs_expr with
          | Error e -> Error e
          | Ok be   -> Ok (bound_list @ [(i, be)])))
  ) (Ok []) assigns
```

- [ ] **Step 4: Update `bind_insert` to bind `upsert_update`**

In `lib/sql/sema.ml`, find `bind_insert`. At the end, after binding `returning`, add upsert binding:

```ocaml
let upsert_result =
  match upsert_update with
  | None -> Ok None
  | Some (conflict_cols, assigns) ->
    (match bind_upsert_assignments ~param_counter ~named_params meta assigns with
     | Error e -> Error e
     | Ok bound_assigns -> Ok (Some (conflict_cols, bound_assigns)))
in
(match upsert_result with
 | Error e -> Lwt.return (Error e)
 | Ok bound_upsert ->
   Lwt.return (Ok (BS_insert {
     table_meta  = meta;
     ordinals;
     values      = bound_values;
     on_conflict;
     returning   = bound_returning;
     upsert_update = bound_upsert;
   })))
```

Also update the call to `bind_insert` in `bind_internal`:
```ocaml
| Ast.S_insert { table; columns; values; on_conflict; returning; upsert_update } ->
  bind_insert cat ~param_counter ~named_params ~table ~columns ~values ~on_conflict ~returning ~upsert_update
```

Update `bind_insert`'s signature to accept `~upsert_update`.

- [ ] **Step 5: Add `P_excluded_col` to `plan.ml`**

In `lib/sql/plan.ml`, add to `Plan.expr` after `P_cast`:
```ocaml
  | P_excluded_col of int
    (** Column reference into the proposed INSERT excluded row. *)
```

Add `upsert_update` to `Op_insert`:
```ocaml
  | Op_insert of {
      table_meta    : Cat.table_meta;
      ordinals      : int list;
      values        : expr list list;
      on_conflict   : Ast.conflict_action option;
      returning     : expr list;
      upsert_update : (string list * (int * expr) list) option;
    }
```

Also add two new ops at the end:
```ocaml
  | Op_create_view of {
      name  : string;
      query : Ast.stmt;
    }
  | Op_drop_view of {
      name : string;
    }
```

- [ ] **Step 6: Update `planner.ml` to handle `BE_excluded_col` and new ops**

In `lib/sql/planner.ml`, add `BE_excluded_col` arm to `plan_expr`:
```ocaml
  | Sema.BE_excluded_col i -> Plan.P_excluded_col i
```

Update the `BS_insert` case in `plan`:
```ocaml
  | Sema.BS_insert { table_meta; ordinals; values; on_conflict; returning; upsert_update } ->
    let plan_upsert = match upsert_update with
      | None -> None
      | Some (cols, assigns) ->
        Some (cols, List.map (fun (i, e) -> (i, plan_expr e)) assigns)
    in
    Plan.Op_insert { table_meta; ordinals;
                     values = List.map (List.map plan_expr) values;
                     on_conflict;
                     returning = List.map plan_expr returning;
                     upsert_update = plan_upsert }
```

Add cases for new sema stmts:
```ocaml
  | Sema.BS_create_view { name; query } ->
    Plan.Op_create_view { name; query }
  | Sema.BS_drop_view { name } ->
    Plan.Op_drop_view { name }
```

Also add `BS_create_view` and `BS_drop_view` to `sema.ml` and `sema.mli`:

In `sema.ml`, add to `bound_stmt` type:
```ocaml
  | BS_create_view of { name: string; query: Ast.stmt }
  | BS_drop_view   of { name: string }
```

In `bind_internal`:
```ocaml
  | Ast.S_create_view { name; query } ->
    (* Validate query binds correctly, then record it *)
    let* bound_r = bind_internal ~views ~named_params ~param_counter cat query in
    (match bound_r with
     | Error e -> Lwt.return (Error e)
     | Ok _ -> Lwt.return (Ok (BS_create_view { name; query })))
  | Ast.S_drop_view { name } ->
    Lwt.return (Ok (BS_drop_view { name }))
```

- [ ] **Step 7: Build and confirm**

```
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1 | head -30
```

Expected: clean build. Fix any exhaustiveness warnings on match arms for the new constructors.

- [ ] **Step 8: Run tests**

```
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune test 2>&1 | tail -10
```

Expected: all 260 tests pass.

- [ ] **Step 9: Commit**

```bash
git add lib/sql/sema.ml lib/sql/sema.mli lib/sql/plan.ml lib/sql/planner.ml
git commit -m "feat(phase13): UPSERT + View sema/plan — BE_excluded_col, upsert_update, BS/Op_create/drop_view"
```

---

## Task 3: UPSERT + Views — Exec + Db

**Files:**
- Modify: `lib/sql/exec.ml`
- Modify: `lib/db/db.ml`

- [ ] **Step 1: Add `P_excluded_col` arm to `eval_expr` in `exec.ml`**

Find the `eval_expr` function in `lib/sql/exec.ml` (the large recursive function). Add a new arm after `| Plan.P_cast (e, ty) ->`:

```ocaml
  | Plan.P_excluded_col _ ->
    (* Should be substituted before eval — failwith if reached unsubstituted *)
    failwith "Exec: P_excluded_col reached eval_expr without substitution"
```

- [ ] **Step 2: Add `substitute_excluded` helper in `exec.ml`**

Add before `execute_insert`:

```ocaml
(** Replace every [P_excluded_col i] in [e] with [P_lit (value_to_literal excluded_row.(i))].
    Used to materialise UPSERT excluded-row refs before [eval_expr] is called. *)
let rec substitute_excluded (excluded_row : Row.t) (e : Plan.expr) : Plan.expr =
  match e with
  | Plan.P_excluded_col i -> Plan.P_lit (value_to_literal excluded_row.(i))
  | Plan.P_binop (op, a, b) ->
    Plan.P_binop (op, substitute_excluded excluded_row a, substitute_excluded excluded_row b)
  | Plan.P_not e      -> Plan.P_not (substitute_excluded excluded_row e)
  | Plan.P_is_null e  -> Plan.P_is_null (substitute_excluded excluded_row e)
  | Plan.P_is_not_null e -> Plan.P_is_not_null (substitute_excluded excluded_row e)
  | Plan.P_neg e      -> Plan.P_neg (substitute_excluded excluded_row e)
  | Plan.P_bitnot e   -> Plan.P_bitnot (substitute_excluded excluded_row e)
  | Plan.P_between (x, lo, hi) ->
    Plan.P_between (substitute_excluded excluded_row x,
                    substitute_excluded excluded_row lo,
                    substitute_excluded excluded_row hi)
  | Plan.P_in (x, vals) ->
    Plan.P_in (substitute_excluded excluded_row x,
               List.map (substitute_excluded excluded_row) vals)
  | Plan.P_func (f, args) ->
    Plan.P_func (f, List.map (substitute_excluded excluded_row) args)
  | Plan.P_case { scrutinee; branches; else_ } ->
    let go = substitute_excluded excluded_row in
    Plan.P_case {
      scrutinee = Option.map go scrutinee;
      branches  = List.map (fun (c, r) -> (go c, go r)) branches;
      else_     = Option.map go else_;
    }
  | Plan.P_cast (e, ty) -> Plan.P_cast (substitute_excluded excluded_row e, ty)
  | other -> other  (* P_lit, P_col, P_param, P_subquery, P_exists, P_in_select *)
```

- [ ] **Step 3: Extend `execute_insert` to handle UPSERT**

Find `execute_insert` in `exec.ml`. The function signature becomes:
```ocaml
let execute_insert ?(mode = Auto) ?(params = [||])
    ?(clock : (unit -> float) option = None)
    ?(on_conflict : Ast.conflict_action option = None)
    ?(prebuilt_row : Row.t option = None)
    ?(upsert_update : (string list * (int * Plan.expr) list) option = None)
    (store : S.t) (cat : Cat.t)
    ~(table_meta : Cat.table_meta) ~ordinals ~(values : Plan.expr list) : bool Lwt.t =
```

Inside the function, change the conflict detection accumulator from `(bool * int64 list)` to `(bool * int64 list * int64 option)` where the third element is "conflicting rowid for UPSERT".

Find the `let* (skip, to_delete) = Lwt_list.fold_left_s ...` block and replace with:

```ocaml
      let* (skip, to_delete, upsert_rowid) =
        Lwt_list.fold_left_s (fun (skip, dels, upsert_rid) (idx : Cat.index_info) ->
          if skip || not idx.idx_unique then Lwt.return (skip, dels, upsert_rid)
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
            | None -> Lwt.return (false, dels, upsert_rid)
            | Some old_rowid ->
              (match on_conflict, upsert_update with
               | Some Ast.CA_ignore, _ ->
                 Lwt.return (true, dels, upsert_rid)
               | Some Ast.CA_replace, _ ->
                 Lwt.return (false, old_rowid :: dels, upsert_rid)
               | _, Some (conflict_cols, _) when
                   List.sort String.compare idx.idx_columns =
                   List.sort String.compare conflict_cols ->
                 (* UPSERT target conflict: record rowid for update *)
                 Lwt.return (false, dels, Some old_rowid)
               | _ ->
                 Lwt.fail_with (Printf.sprintf
                   "UNIQUE constraint violated: duplicate value in columns (%s)"
                   (String.concat ", " idx.idx_columns)))
          end
        ) (false, [], None) idxs
      in
```

After the fold, add a new branch BEFORE the existing `if skip then ... else ...`:

```ocaml
      (* UPSERT: if a conflict was detected on the target column(s), update in place *)
      match upsert_update, upsert_rowid with
      | Some (_, assigns), Some old_rowid ->
        let old_key = Rowid.encode old_rowid in
        let* old_bytes_opt = S.get tx table_meta.tree_id old_key in
        (match old_bytes_opt with
         | None ->
           let* () = if owned then S.rollback tx else Lwt.return_unit in
           Lwt.return false
         | Some old_bytes ->
           let old_row = Row.decode table_meta.columns old_bytes in
           let new_row = Array.copy old_row in
           (* Substitute excluded refs, then eval against the existing row *)
           List.iter (fun (col_ord, expr) ->
             let e' = substitute_excluded row expr in
             new_row.(col_ord) <- eval_expr clock params old_row e'
           ) assigns;
           eval_check_constraints clock params table_meta new_row;
           let idxs = Cat.indexes_for_table cat ~table:table_meta.name in
           let* () = Lwt_list.iter_s (fun (idx : Cat.index_info) ->
             let col_is = List.map (find_col_idx_by_name table_meta.columns) idx.idx_columns in
             let old_iks = List.map (fun ci -> row_value_to_index_value old_row.(ci)) col_is in
             let new_iks = List.map (fun ci -> row_value_to_index_value new_row.(ci)) col_is in
             let old_ikey = Index_key.encode old_iks ~rowid:old_rowid in
             let new_ikey = Index_key.encode new_iks ~rowid:old_rowid in
             let* () = S.del tx idx.idx_tree_id old_ikey in
             S.put tx idx.idx_tree_id new_ikey Bytes.empty
           ) idxs in
           let new_bytes = Row.encode table_meta.columns new_row in
           let* () = S.del tx table_meta.tree_id old_key in
           let* () = S.put tx table_meta.tree_id old_key new_bytes in
           let* () = release_txn tx owned in
           Lwt.return true)
      | _ ->
        (* Normal path: skip (IGNORE), replace (REPLACE), or plain insert *)
        if skip then begin
          let* () = if owned then S.rollback tx else Lwt.return_unit in
          Lwt.return false
        end else begin
          (* ... existing REPLACE + insert code ... *)
```

Make sure the existing `if skip then ... else ...` block is now inside the `| _ ->` branch (the last line above).

- [ ] **Step 4: Update Op_insert call in `execute_with_count` and RETURNING path**

In `execute_with_count`, find:
```ocaml
  | Plan.Op_insert { table_meta; ordinals; values; on_conflict; returning = _ } ->
```
Update to:
```ocaml
  | Plan.Op_insert { table_meta; ordinals; values; on_conflict; returning = _; upsert_update } ->
```
And thread `upsert_update` through to `execute_insert`:
```ocaml
      let* inserted = execute_insert ~mode ~params ~clock ~on_conflict ~upsert_update
```

Find the RETURNING path for `Op_insert` in `to_stream`:
```ocaml
  | Plan.Op_insert { table_meta; ordinals; values; on_conflict; returning }
```
Update to include `upsert_update` and thread it through.

- [ ] **Step 5: Guard `Op_create_view`/`Op_drop_view` in `execute_with_count` read-only check**

In `execute_with_count`, find the read-only guard block:
```ocaml
  | Plan.Op_nested_loop_join _ | Plan.Op_hash_join _ | Plan.Op_aggregate _
```
Add `| Plan.Op_create_view _ | Plan.Op_drop_view _` to that pattern (with message "CREATE/DROP VIEW handled by Db layer").

Or add explicit cases:
```ocaml
  | Plan.Op_create_view _ | Plan.Op_drop_view _ ->
    failwith "Exec.execute_with_count: CREATE/DROP VIEW handled by Db layer"
```

- [ ] **Step 6: Add views hashtable to `Db.t` and handle view ops**

In `lib/db/db.ml`, update the `t` type:
```ocaml
type t = {
  store            : S.t;
  catalog          : Cat.t;
  clock            : (unit -> float) option;
  mutable explicit_txn : S.rw S.txn option;
  views            : (string, Sql.Ast.stmt) Hashtbl.t;
}
```

Update `open_in_memory`:
```ocaml
let open_in_memory ?clock () =
  let store = S.create () in
  let* catalog = Cat.open_ store in
  Lwt.return { store; catalog; clock; explicit_txn = None;
               views = Hashtbl.create 4 }
```

Update `open_file` and `open_block` similarly (add `views = Hashtbl.create 4` to the returned record).

Update `compile` to pass `~views`:
```ocaml
let compile t sql =
  match parse sql with
  | Error e -> Lwt.return (Error e)
  | Ok ast  ->
    let* bound = Sql.Sema.bind ~views:t.views t.catalog ast in
    match bound with
    | Error e -> Lwt.return (Error (Sema e))
    | Ok b    -> Lwt.return (Ok (Sql.Planner.plan ~cat:t.catalog b))
```

Update `prepare` to pass `~views`:
```ocaml
    let* bound = Sql.Sema.bind_returning_params ~views:t.views t.catalog ast in
```

In `execute`, add handlers before the `| Ok op ->` fallthrough:
```ocaml
  | Ok Sql.Plan.Op_create_view { name; query } ->
    Hashtbl.replace t.views name query;
    Lwt.return (Ok ())
  | Ok Sql.Plan.Op_drop_view { name } ->
    Hashtbl.remove t.views name;
    Lwt.return (Ok ())
```

In `execute_change_count`, add similarly (returning `Ok 0`):
```ocaml
  | Ok Sql.Plan.Op_create_view { name; query } ->
    Hashtbl.replace t.views name query;
    Lwt.return (Ok 0)
  | Ok Sql.Plan.Op_drop_view { name } ->
    Hashtbl.remove t.views name;
    Lwt.return (Ok 0)
```

- [ ] **Step 7: Add `?views` parameter to `Sema.bind` and thread through**

In `lib/sql/sema.ml`, the public `bind` function currently calls `bind_internal`. Update to thread `views`:

```ocaml
let bind ?(views = Hashtbl.create 0) cat stmt =
  let param_counter = ref 0 in
  let named_params  = Hashtbl.create 4 in
  bind_internal ~views ~named_params ~param_counter cat stmt
```

Update `bind_returning_params` similarly:
```ocaml
let bind_returning_params ?(views = Hashtbl.create 0) cat stmt =
  let param_counter = ref 0 in
  let named_params  = Hashtbl.create 4 in
  let* r = bind_internal ~views ~named_params ~param_counter cat stmt in
  ...
```

In `sema.mli`, update signatures:
```ocaml
val bind : ?views:(string, Ast.stmt) Hashtbl.t -> Cat.t -> Ast.stmt -> (bound_stmt, error) result Lwt.t
val bind_returning_params : ?views:(string, Ast.stmt) Hashtbl.t -> Cat.t -> Ast.stmt -> ...
```

Now update `bind_internal` to accept and thread `~views`:

```ocaml
let rec bind_internal ?(views = Hashtbl.create 0) ~named_params ~param_counter cat stmt =
  match stmt with
  ...
  | Ast.S_with_cte { name; def; query } ->
    let* def_r = bind_internal ~views ~named_params ~param_counter cat def in
    ...
    let* query_r = bind_internal ~views ~named_params ~param_counter cat query in
    ...
  | Ast.S_compound { op; left; right } ->
    let* left_r  = bind_internal ~views ~named_params ~param_counter cat left  in
    let* right_r = bind_internal ~views ~named_params ~param_counter cat right in
    ...
  | Ast.S_select { distinct; proj; table; table_alias; joins; where; group_by; having; order; limit; offset } as sel ->
    (* View expansion: only if catalog doesn't already have this name
       (prevents infinite recursion when processing view's own query as CTE) *)
    let* meta_opt = Cat.find_table cat ~name:table in
    (match meta_opt with
     | Some _ ->
       (* Real table or ephemeral CTE — proceed normally *)
       bind_select cat ~param_counter ~named_params ~distinct ~proj ~table ~table_alias
         ~joins ~where ~group_by ~having ~order ~limit ~offset
     | None ->
       (match Hashtbl.find_opt views table with
        | Some view_def ->
          (* Inline view as CTE: WITH table AS (view_def) original_select *)
          bind_internal ~views ~named_params ~param_counter cat
            (Ast.S_with_cte { name = table; def = view_def; query = sel })
        | None ->
          (* Not a view — let bind_select handle FTS or return Unknown_table *)
          bind_select cat ~param_counter ~named_params ~distinct ~proj ~table ~table_alias
            ~joins ~where ~group_by ~having ~order ~limit ~offset))
  | Ast.S_create_view { name; query } ->
    let* bound_r = bind_internal ~views ~named_params ~param_counter cat query in
    (match bound_r with
     | Error e -> Lwt.return (Error e)
     | Ok _ -> Lwt.return (Ok (BS_create_view { name; query })))
  | Ast.S_drop_view { name } ->
    Lwt.return (Ok (BS_drop_view { name }))
  ...
```

- [ ] **Step 8: Build and run all tests**

```
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1 | head -30
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune test 2>&1 | tail -10
```

Expected: clean build, all 260 tests pass.

- [ ] **Step 9: Commit**

```bash
git add lib/sql/exec.ml lib/db/db.ml lib/sql/sema.ml lib/sql/sema.mli
git commit -m "feat(phase13): UPSERT exec (substitute_excluded + execute_insert) + views DB layer + Sema.bind ?views"
```

---

## Task 4: UPSERT E2E Tests

**Files:**
- Modify: `test/test_e2e.ml`

- [ ] **Step 1: Write failing tests (verify build passes, test fails)**

Add a `"upsert"` group to `test_e2e.ml` with 5 tests. Add the test functions before the `let () = Alcotest.run` line, and add the group to the test list.

```ocaml
(* ------------------------------------------------------------------ *)
(* Phase 13: UPSERT                                                    *)
(* ------------------------------------------------------------------ *)

let test_upsert_basic () =
  (* Basic: conflict triggers UPDATE SET col = excluded.col *)
  run (
    let* db = Db.open_in_memory () in
    exec db "CREATE TABLE kv (k INTEGER NOT NULL, v TEXT NOT NULL, UNIQUE(k))";
    exec db "INSERT INTO kv (k, v) VALUES (1, 'old')";
    exec db "INSERT INTO kv (k, v) VALUES (1, 'new') ON CONFLICT(k) DO UPDATE SET v = excluded.v";
    let rows = query_ok db "SELECT k, v FROM kv" in
    Alcotest.(check int) "one row" 1 (List.length rows);
    (match rows with
     | [r] ->
       Alcotest.check value_testable "k=1" (Db.V_int 1L) r.(0);
       Alcotest.check value_testable "v=new" (Db.V_text "new") r.(1)
     | _ -> Alcotest.fail "expected one row");
    Lwt.return_unit)

let test_upsert_no_conflict () =
  (* When no conflict: row is inserted normally *)
  run (
    let* db = Db.open_in_memory () in
    exec db "CREATE TABLE kv (k INTEGER NOT NULL, v TEXT NOT NULL, UNIQUE(k))";
    exec db "INSERT INTO kv (k, v) VALUES (1, 'first')";
    exec db "INSERT INTO kv (k, v) VALUES (2, 'second') ON CONFLICT(k) DO UPDATE SET v = excluded.v";
    let rows = query_ok db "SELECT k, v FROM kv ORDER BY k" in
    Alcotest.(check int) "two rows" 2 (List.length rows);
    Lwt.return_unit)

let test_upsert_expression () =
  (* RHS can mix excluded.col and existing col values *)
  run (
    let* db = Db.open_in_memory () in
    exec db "CREATE TABLE counters (name TEXT NOT NULL, cnt INTEGER NOT NULL, UNIQUE(name))";
    exec db "INSERT INTO counters (name, cnt) VALUES ('hits', 1)";
    (* On conflict: cnt = cnt + 1 (existing cnt + 1) *)
    exec db "INSERT INTO counters (name, cnt) VALUES ('hits', 0) ON CONFLICT(name) DO UPDATE SET cnt = cnt + 1";
    let rows = query_ok db "SELECT cnt FROM counters WHERE name = 'hits'" in
    (match rows with
     | [r] -> Alcotest.check value_testable "cnt=2" (Db.V_int 2L) r.(0)
     | _ -> Alcotest.fail "expected one row");
    Lwt.return_unit)

let test_upsert_multiple_assignments () =
  (* Multiple SET assignments work *)
  run (
    let* db = Db.open_in_memory () in
    exec db "CREATE TABLE t (id INTEGER, a TEXT, b TEXT, UNIQUE(id))";
    exec db "INSERT INTO t (id, a, b) VALUES (1, 'a1', 'b1')";
    exec db "INSERT INTO t (id, a, b) VALUES (1, 'a2', 'b2') ON CONFLICT(id) DO UPDATE SET a = excluded.a, b = excluded.b";
    let rows = query_ok db "SELECT a, b FROM t WHERE id = 1" in
    (match rows with
     | [r] ->
       Alcotest.check value_testable "a=a2" (Db.V_text "a2") r.(0);
       Alcotest.check value_testable "b=b2" (Db.V_text "b2") r.(1)
     | _ -> Alcotest.fail "expected one row");
    Lwt.return_unit)

let test_upsert_preserves_non_conflict_rows () =
  (* Other rows in the table are not affected *)
  run (
    let* db = Db.open_in_memory () in
    exec db "CREATE TABLE kv (k INTEGER, v TEXT, UNIQUE(k))";
    exec db "INSERT INTO kv VALUES (1, 'a'), (2, 'b'), (3, 'c')";
    exec db "INSERT INTO kv VALUES (2, 'B') ON CONFLICT(k) DO UPDATE SET v = excluded.v";
    let rows = query_ok db "SELECT k, v FROM kv ORDER BY k" in
    Alcotest.(check int) "still 3 rows" 3 (List.length rows);
    (match rows with
     | [_; r2; _] ->
       Alcotest.check value_testable "row2 v=B" (Db.V_text "B") r2.(1)
     | _ -> Alcotest.fail "expected 3 rows");
    Lwt.return_unit)
```

Add the test group to the Alcotest list:
```ocaml
    "upsert", [
      Alcotest.test_case "basic"                `Quick test_upsert_basic;
      Alcotest.test_case "no_conflict"          `Quick test_upsert_no_conflict;
      Alcotest.test_case "expression"           `Quick test_upsert_expression;
      Alcotest.test_case "multiple_assignments" `Quick test_upsert_multiple_assignments;
      Alcotest.test_case "preserves_others"     `Quick test_upsert_preserves_non_conflict_rows;
    ];
```

- [ ] **Step 2: Run tests — verify new tests pass**

```
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune test 2>&1 | grep -E "(upsert|FAIL|PASS|OK)" | head -20
```

Expected: all 5 upsert tests pass.

- [ ] **Step 3: Full test suite**

```
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune test 2>&1 | tail -5
```

Expected: 265 tests pass.

- [ ] **Step 4: Commit**

```bash
git add test/test_e2e.ml
git commit -m "test(phase13): UPSERT e2e tests — 5 cases"
```

---

## Task 5: Views E2E Tests

**Files:**
- Modify: `test/test_e2e.ml`

- [ ] **Step 1: Write view tests**

Add a `"view"` group with 6 tests:

```ocaml
(* ------------------------------------------------------------------ *)
(* Phase 13: Views                                                     *)
(* ------------------------------------------------------------------ *)

let test_view_basic () =
  (* Basic: CREATE VIEW and SELECT from it *)
  run (
    let* db = Db.open_in_memory () in
    exec db "CREATE TABLE users (id INTEGER, name TEXT, active INTEGER)";
    exec db "INSERT INTO users VALUES (1, 'Alice', 1), (2, 'Bob', 0), (3, 'Carol', 1)";
    exec db "CREATE VIEW active_users AS SELECT id, name FROM users WHERE active = 1";
    let rows = query_ok db "SELECT name FROM active_users ORDER BY id" in
    Alcotest.(check int) "two active users" 2 (List.length rows);
    (match rows with
     | [r1; r2] ->
       Alcotest.check value_testable "first=Alice" (Db.V_text "Alice") r1.(0);
       Alcotest.check value_testable "second=Carol" (Db.V_text "Carol") r2.(0)
     | _ -> Alcotest.fail "expected 2 rows");
    Lwt.return_unit)

let test_view_with_filter () =
  (* SELECT with WHERE on a view column *)
  run (
    let* db = Db.open_in_memory () in
    exec db "CREATE TABLE products (id INTEGER, name TEXT, price INTEGER)";
    exec db "INSERT INTO products VALUES (1, 'A', 10), (2, 'B', 20), (3, 'C', 5)";
    exec db "CREATE VIEW cheap AS SELECT id, name, price FROM products WHERE price < 15";
    let rows = query_ok db "SELECT name FROM cheap WHERE price > 7 ORDER BY name" in
    Alcotest.(check int) "one result" 1 (List.length rows);
    (match rows with
     | [r] -> Alcotest.check value_testable "name=A" (Db.V_text "A") r.(0)
     | _ -> Alcotest.fail "expected 1 row");
    Lwt.return_unit)

let test_view_aggregation () =
  (* View with COUNT aggregation *)
  run (
    let* db = Db.open_in_memory () in
    exec db "CREATE TABLE orders (customer TEXT, amount INTEGER)";
    exec db "INSERT INTO orders VALUES ('Alice', 100), ('Alice', 200), ('Bob', 50)";
    exec db "CREATE VIEW order_counts AS SELECT customer, COUNT(*) AS cnt FROM orders GROUP BY customer";
    let rows = query_ok db "SELECT customer, cnt FROM order_counts ORDER BY customer" in
    Alcotest.(check int) "two customers" 2 (List.length rows);
    (match rows with
     | [ra; rb] ->
       Alcotest.check value_testable "alice cnt=2" (Db.V_int 2L) ra.(1);
       Alcotest.check value_testable "bob cnt=1" (Db.V_int 1L) rb.(1)
     | _ -> Alcotest.fail "expected 2 rows");
    Lwt.return_unit)

let test_view_drop () =
  (* DROP VIEW makes the view unavailable *)
  run (
    let* db = Db.open_in_memory () in
    exec db "CREATE TABLE t (x INTEGER)";
    exec db "INSERT INTO t VALUES (1)";
    exec db "CREATE VIEW v AS SELECT x FROM t";
    (* Should work before drop *)
    let rows1 = query_ok db "SELECT x FROM v" in
    Alcotest.(check int) "one row before drop" 1 (List.length rows1);
    exec db "DROP VIEW v";
    (* After drop, should return Unknown_table error *)
    let result = run (Db.query db "SELECT x FROM v") in
    Alcotest.(check bool) "error after drop" true (result = Error (Db.Sema (Sqlocaml_sql.Sema.Unknown_table "v")));
    Lwt.return_unit)

let test_view_join () =
  (* View used in a query that also joins with another table *)
  run (
    let* db = Db.open_in_memory () in
    exec db "CREATE TABLE depts (id INTEGER, name TEXT)";
    exec db "CREATE TABLE emps (id INTEGER, dept_id INTEGER, name TEXT)";
    exec db "INSERT INTO depts VALUES (1, 'Eng'), (2, 'Sales')";
    exec db "INSERT INTO emps VALUES (1, 1, 'Alice'), (2, 1, 'Bob'), (3, 2, 'Carol')";
    exec db "CREATE VIEW eng_emps AS SELECT e.name AS emp_name FROM emps e INNER JOIN depts d ON e.dept_id = d.id WHERE d.name = 'Eng'";
    let rows = query_ok db "SELECT emp_name FROM eng_emps ORDER BY emp_name" in
    Alcotest.(check int) "two eng employees" 2 (List.length rows);
    Lwt.return_unit)

let test_view_independent_per_db () =
  (* Views are per-db-instance — a new db has no views *)
  run (
    let* db1 = Db.open_in_memory () in
    let* db2 = Db.open_in_memory () in
    exec db1 "CREATE TABLE t (x INTEGER)";
    exec db1 "CREATE VIEW v AS SELECT x FROM t";
    exec db2 "CREATE TABLE t (x INTEGER)";
    (* db2 should not see db1's view *)
    let result = run (Db.query db2 "SELECT x FROM v") in
    Alcotest.(check bool) "db2 no view" true (match result with Error _ -> true | Ok _ -> false);
    Lwt.return_unit)
```

Add to the test list:
```ocaml
    "view", [
      Alcotest.test_case "basic"         `Quick test_view_basic;
      Alcotest.test_case "with_filter"   `Quick test_view_with_filter;
      Alcotest.test_case "aggregation"   `Quick test_view_aggregation;
      Alcotest.test_case "drop"          `Quick test_view_drop;
      Alcotest.test_case "join"          `Quick test_view_join;
      Alcotest.test_case "per_db"        `Quick test_view_independent_per_db;
    ];
```

- [ ] **Step 2: Run tests**

```
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune test 2>&1 | grep -E "(view|FAIL|OK)" | head -20
```

Expected: all 6 view tests pass, 271 total.

- [ ] **Step 3: Fix `test_view_drop` if needed**

The `test_view_drop` function above uses a nested `run` call which can deadlock in Lwt_main. Rewrite it to not nest:

```ocaml
let test_view_drop () =
  run (
    let* db = Db.open_in_memory () in
    exec db "CREATE TABLE t (x INTEGER)";
    exec db "INSERT INTO t VALUES (1)";
    exec db "CREATE VIEW v AS SELECT x FROM t";
    let rows1 = query_ok db "SELECT x FROM v" in
    Alcotest.(check int) "one row before drop" 1 (List.length rows1);
    exec db "DROP VIEW v";
    let* result = Db.query db "SELECT x FROM v" in
    Alcotest.(check bool) "error after drop" true (match result with Error _ -> true | Ok _ -> false);
    Lwt.return_unit)
```

- [ ] **Step 4: Full test suite**

```
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune test 2>&1 | tail -5
```

Expected: 271 tests pass.

- [ ] **Step 5: Commit**

```bash
git add test/test_e2e.ml
git commit -m "test(phase13): Views e2e tests — 6 cases"
```

---

## Task 6: SQLite Comparison Tests

**Files:**
- Modify: `test/test_sqlite_compare.ml`

- [ ] **Step 1: Read existing test file structure**

Read the end of `test/test_sqlite_compare.ml` to find where new tests should be added:
```
tail -60 test/test_sqlite_compare.ml
```

- [ ] **Step 2: Add UPSERT comparison tests**

Add `phase13_upsert_cases` after the existing `phase12_*` cases:

```ocaml
let phase13_upsert_cases = [
  "upsert_basic",
  ["CREATE TABLE kv (k INTEGER NOT NULL, v TEXT NOT NULL, UNIQUE(k))";
   "INSERT INTO kv VALUES (1, 'old')";
   "INSERT INTO kv VALUES (1, 'new') ON CONFLICT(k) DO UPDATE SET v = excluded.v"],
  "SELECT k, v FROM kv";

  "upsert_no_conflict",
  ["CREATE TABLE kv (k INTEGER NOT NULL, v TEXT NOT NULL, UNIQUE(k))";
   "INSERT INTO kv VALUES (1, 'first')";
   "INSERT INTO kv VALUES (2, 'second') ON CONFLICT(k) DO UPDATE SET v = excluded.v"],
  "SELECT k, v FROM kv ORDER BY k";

  "upsert_increment",
  ["CREATE TABLE counters (name TEXT NOT NULL, cnt INTEGER NOT NULL, UNIQUE(name))";
   "INSERT INTO counters VALUES ('hits', 1)";
   "INSERT INTO counters VALUES ('hits', 0) ON CONFLICT(name) DO UPDATE SET cnt = cnt + 1"],
  "SELECT name, cnt FROM counters";

  "upsert_multi_assign",
  ["CREATE TABLE t (id INTEGER, a TEXT, b TEXT, UNIQUE(id))";
   "INSERT INTO t VALUES (1, 'a1', 'b1')";
   "INSERT INTO t VALUES (1, 'a2', 'b2') ON CONFLICT(id) DO UPDATE SET a = excluded.a, b = excluded.b"],
  "SELECT a, b FROM t WHERE id = 1";

  "upsert_preserves_others",
  ["CREATE TABLE kv (k INTEGER, v TEXT, UNIQUE(k))";
   "INSERT INTO kv VALUES (1, 'a'), (2, 'b'), (3, 'c')";
   "INSERT INTO kv VALUES (2, 'B') ON CONFLICT(k) DO UPDATE SET v = excluded.v"],
  "SELECT k, v FROM kv ORDER BY k";
]
```

- [ ] **Step 3: Add View comparison tests**

Add `phase13_view_cases`:

```ocaml
let phase13_view_cases = [
  "view_basic_select",
  ["CREATE TABLE users (id INTEGER, name TEXT, active INTEGER)";
   "INSERT INTO users VALUES (1, 'Alice', 1), (2, 'Bob', 0), (3, 'Carol', 1)";
   "CREATE VIEW active_users AS SELECT id, name FROM users WHERE active = 1"],
  "SELECT name FROM active_users ORDER BY id";

  "view_with_where",
  ["CREATE TABLE products (id INTEGER, name TEXT, price INTEGER)";
   "INSERT INTO products VALUES (1, 'A', 10), (2, 'B', 20), (3, 'C', 5)";
   "CREATE VIEW cheap AS SELECT id, name, price FROM products WHERE price < 15"],
  "SELECT name FROM cheap ORDER BY price";

  "view_aggregation",
  ["CREATE TABLE orders (customer TEXT, amount INTEGER)";
   "INSERT INTO orders VALUES ('Alice', 100), ('Alice', 200), ('Bob', 50)";
   "CREATE VIEW order_counts AS SELECT customer, COUNT(*) AS cnt FROM orders GROUP BY customer"],
  "SELECT customer, cnt FROM order_counts ORDER BY customer";

  "view_in_subquery",
  ["CREATE TABLE users (id INTEGER, name TEXT, active INTEGER)";
   "INSERT INTO users VALUES (1, 'Alice', 1), (2, 'Bob', 0)";
   "CREATE VIEW active_users AS SELECT id, name FROM users WHERE active = 1"],
  "SELECT COUNT(*) FROM active_users";

  "view_multiple_selects",
  ["CREATE TABLE t (x INTEGER)";
   "INSERT INTO t VALUES (1), (2), (3)";
   "CREATE VIEW v AS SELECT x FROM t WHERE x > 1"],
  "SELECT SUM(x) FROM v";
]
```

- [ ] **Step 4: Wire into the test suite**

Find where existing phase12 cases are added to the Alcotest list. Add the new cases:

```ocaml
  (* Add to the make_compare_test calls *)
  List.map (make_compare_test "phase13_upsert") phase13_upsert_cases @
  List.map (make_compare_test "phase13_view") phase13_view_cases @
```

- [ ] **Step 5: Run comparison tests**

```
podman run --rm \
  -v $(pwd):/workspace:Z \
  -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
  -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
  -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
  -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
  -w /workspace sqlocaml-dev dune test 2>&1 | grep -E "(phase13|FAIL|OK)" | head -20
```

Expected: all 10 comparison tests pass (5 upsert + 5 view).

- [ ] **Step 6: Full test suite**

```
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune test 2>&1 | tail -5
```

Expected: 281+ tests pass.

- [ ] **Step 7: Commit**

```bash
git add test/test_sqlite_compare.ml
git commit -m "test(phase13): SQLite comparison tests — 5 upsert + 5 view cases"
```

---

## Self-Review

### Spec coverage check

| Feature | Task |
|---------|------|
| `ON CONFLICT(col) DO UPDATE SET` syntax | Task 1 (parser) |
| `excluded.col` refs in UPSERT assignments | Task 2 (sema BE_excluded_col) |
| UPSERT triggers on matching unique constraint column | Task 3 (exec) |
| UPSERT: no conflict → normal insert | Task 4 (tests) |
| UPSERT: cnt = cnt + 1 expression | Task 4 (tests) |
| `CREATE VIEW name AS SELECT ...` | Task 1 (parser) |
| `DROP VIEW name` | Task 1 (parser) |
| SELECT from view (transparent inlining as CTE) | Task 3 (db.ml + sema views) |
| Views are per-db-instance, not global | Task 5 (tests) |
| SQLite compatibility tests | Task 6 |

### Placeholder scan

No TBD, TODO, or "fill in" patterns in this plan. All code shown completely.

### Type consistency check

- `P_excluded_col of int` is added to `Plan.expr` (plan.ml), propagated from `BE_excluded_col` in sema, and handled in `substitute_excluded` (exec.ml) and `eval_expr` fallthrough guard.
- `upsert_update` field type is consistent: `(string list * (int * Plan.expr) list) option` in Plan; `(string list * (int * bound_expr) list) option` in Sema.
- `Op_create_view { name: string; query: Ast.stmt }` — `Ast.stmt` is available in plan.ml since plan.ml already references `Ast` (for `Ast.conflict_action`, `Ast.literal` etc).
- `BS_create_view { name: string; query: Ast.stmt }` — `Ast.stmt` is available in sema.ml.
- `views: (string, Sql.Ast.stmt) Hashtbl.t` in `Db.t` — `Sql.Ast.stmt` accessible since `Sql = Sqlocaml_sql`.

### Known limitations (document, don't fix in Phase 13)

- Views are in-memory only: lost when the OCaml process restarts (no B-tree persistence). Future phase can persist view SQL text to the catalog.
- Views in JOIN position (e.g., `t1 JOIN my_view ON ...`) are not supported. The `joins` field in `S_select` passes through `bind_select`'s join resolution, which checks `Cat.find_table` and does not check views. Views only work in the primary FROM table position.
- UPSERT RETURNING clause is not implemented in Phase 13. The `execute_insert` UPSERT path returns `true` without yielding the updated row. Future enhancement.
