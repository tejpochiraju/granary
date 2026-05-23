# Phase 23: FK Referential Actions (CASCADE, SET NULL, SET DEFAULT) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend FOREIGN KEY enforcement beyond RESTRICT to implement CASCADE, SET NULL, and SET DEFAULT referential actions on DELETE and UPDATE, tracked in Forgejo issue #130.

**Architecture:** `fk_action` is defined in `catalog.ml` (single canonical definition for persistence). `ast.ml` re-declares it as a type alias with the same constructors using OCaml's type alias syntax (`type fk_action = Sqlocaml_catalog.Catalog.fk_action = | FA_no_action | ...`). The sema/plan fk_constraints tuple extends from `(string * string * string)` to `(string * string * string * Cat.fk_action * Cat.fk_action)`. Three new in-tx helpers (`scan_child_rows_tx`, `delete_row_in_tx`, `update_col_in_tx`) perform cascade operations within the existing RW transaction. The pre-RW RESTRICT/NO_ACTION check is restructured to skip CASCADE/SET_NULL/SET_DEFAULT; those are applied in the write phase.

**Tech Stack:** OCaml 5.x, dune 3.x, lwt, menhir, alcotest. Build: `podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune ...`. Run e2e tests: `podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe 2>&1 | tail -5`.

---

## File Map

| File | Change |
|------|--------|
| `lib/catalog/catalog.mli` | Add `fk_action` type; add `fk_on_delete`/`fk_on_update` to `fk_constraint` |
| `lib/catalog/catalog.ml` | Add `fk_action` type; extend `fk_constraint`; update encode/decode |
| `lib/sql/ast.ml` | Add `fk_action` type alias; extend `TC_foreign_key`; extend `column_def.fk_ref` to 4-tuple |
| `lib/sql/lexer.mll` | Add `CASCADE` and `RESTRICT` to ident block |
| `lib/sql/parser.mly` | Add `%token CASCADE RESTRICT`; update `Col_fk_ref`; add `fk_ref_action` + `fk_on_clauses` rules; update REFERENCES and FOREIGN KEY clauses |
| `lib/sql/sema.mli` | Change `fk_constraints` from 3-tuple list to 5-tuple list |
| `lib/sql/sema.ml` | Pass through `on_delete`/`on_update` in both col-level and table-level FK extraction |
| `lib/sql/plan.ml` | Change `fk_constraints` to 5-tuple list |
| `lib/sql/exec.ml` | Add `scan_child_rows_tx`, `delete_row_in_tx`, `update_col_in_tx`; restructure FK checks in `execute_delete` and `execute_update`; update `Op_create_table` to persist actions |
| `test/test_e2e.ml` | 8 new tests in `"fk_delete_update"` suite |
| `test/test_sqlite_compare.ml` | New `phase23_cascade_cases` list + registration |

---

### Task 1: Add `fk_action` type to catalog and extend `fk_constraint`

**Files:**
- Modify: `lib/catalog/catalog.mli`
- Modify: `lib/catalog/catalog.ml`

- [ ] **Step 1: Add `fk_action` type and extend `fk_constraint` in catalog.mli**

In `lib/catalog/catalog.mli`, before the existing `type fk_constraint = {` block (line 7), insert:

```ocaml
type fk_action =
  | FA_no_action
  | FA_restrict
  | FA_cascade
  | FA_set_null
  | FA_set_default
```

Replace the existing `type fk_constraint = { ... }` with:

```ocaml
type fk_constraint = {
  fk_local_col    : string;
  fk_parent_table : string;
  fk_parent_col   : string;
  fk_on_delete    : fk_action;
  fk_on_update    : fk_action;
}
```

- [ ] **Step 2: Add same types in catalog.ml**

In `lib/catalog/catalog.ml`, after the `module Varint = ...` line (line 3), insert:

```ocaml
type fk_action =
  | FA_no_action
  | FA_restrict
  | FA_cascade
  | FA_set_null
  | FA_set_default
```

Replace the existing `type fk_constraint = { ... }` block (around line 23) with:

```ocaml
type fk_constraint = {
  fk_local_col    : string;
  fk_parent_table : string;
  fk_parent_col   : string;
  fk_on_delete    : fk_action;
  fk_on_update    : fk_action;
}
```

- [ ] **Step 3: Replace encode_fks / decode_fks in catalog.ml**

Find the existing `encode_fks` and `decode_fks` functions (around line 514) and replace them entirely:

```ocaml
let fk_action_to_string = function
  | FA_no_action   -> "no_action"
  | FA_restrict    -> "restrict"
  | FA_cascade     -> "cascade"
  | FA_set_null    -> "set_null"
  | FA_set_default -> "set_default"

let fk_action_of_string = function
  | "no_action"   -> FA_no_action
  | "cascade"     -> FA_cascade
  | "set_null"    -> FA_set_null
  | "set_default" -> FA_set_default
  | _             -> FA_restrict

let encode_fks fks =
  let lines = List.map (fun fk ->
    String.concat "\t" [
      fk.fk_local_col;
      fk.fk_parent_table;
      fk.fk_parent_col;
      fk_action_to_string fk.fk_on_delete;
      fk_action_to_string fk.fk_on_update;
    ]
  ) fks in
  Bytes.of_string (String.concat "\n" lines)

let decode_fks bytes =
  let s = Bytes.to_string bytes in
  if s = "" then []
  else
    List.filter_map (fun line ->
      match String.split_on_char '\t' line with
      | [lc; pt; pc] ->
        Some { fk_local_col = lc; fk_parent_table = pt; fk_parent_col = pc;
               fk_on_delete = FA_restrict; fk_on_update = FA_restrict }
      | [lc; pt; pc; od; ou] ->
        Some { fk_local_col = lc; fk_parent_table = pt; fk_parent_col = pc;
               fk_on_delete = fk_action_of_string od;
               fk_on_update = fk_action_of_string ou }
      | _ -> None
    ) (String.split_on_char '\n' s)
```

- [ ] **Step 4: Build catalog layer**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build lib/catalog/ 2>&1
```

Expected: succeeds.

- [ ] **Step 5: Commit**

```bash
git add lib/catalog/catalog.ml lib/catalog/catalog.mli
git commit -m "feat(phase23): add fk_action type and extend fk_constraint with on_delete/on_update [#130]"
```

---

### Task 2: Extend AST types for fk_action

**Files:**
- Modify: `lib/sql/ast.ml`

- [ ] **Step 1: Add fk_action type alias in ast.ml**

In `lib/sql/ast.ml`, after the `type join_kind = Inner | Left` line (around line 114), add:

```ocaml
type fk_action = Sqlocaml_catalog.Catalog.fk_action =
  | FA_no_action
  | FA_restrict
  | FA_cascade
  | FA_set_null
  | FA_set_default
```

This re-declares `fk_action` as an alias of the catalog type. Code in the sql layer can then write `Ast.FA_cascade` as shorthand.

- [ ] **Step 2: Extend TC_foreign_key**

Find the `TC_foreign_key` record (around line 119) and add `on_delete`/`on_update`:

```ocaml
type table_constraint =
  | TC_unique      of string list
  | TC_primary_key of string list
  | TC_foreign_key of {
      local_cols   : string list;
      parent_table : string;
      parent_cols  : string list;
      on_delete    : fk_action;
      on_update    : fk_action;
    }
```

- [ ] **Step 3: Extend fk_ref in column_def**

Find `fk_ref` in `column_def` (around line 321). Change from `(string * string) option` to a 4-tuple:

```ocaml
  fk_ref      : (string * string * fk_action * fk_action) option;
  (** [(parent_table, parent_col, on_delete, on_update)]. None = no FK. *)
```

- [ ] **Step 4: Verify build error list (not full success yet)**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build lib/sql/ 2>&1 | head -20
```

Expected: errors in `parser.mly` and `sema.ml` about `fk_ref` tuple mismatches. NOT errors in `ast.ml` itself.

- [ ] **Step 5: Commit**

```bash
git add lib/sql/ast.ml
git commit -m "feat(phase23): add fk_action alias and extend TC_foreign_key, column_def.fk_ref in ast.ml [#130]"
```

---

### Task 3: Lexer and parser for ON DELETE / ON UPDATE clauses

**Files:**
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`

- [ ] **Step 1: Add CASCADE and RESTRICT to lexer ident block**

In `lib/sql/lexer.mll`, inside the `String.uppercase_ascii id` match (ident fallback), add before the `| _ -> IDENT id` fallback:

```ocaml
      | "CASCADE"  -> CASCADE
      | "RESTRICT" -> RESTRICT
```

- [ ] **Step 2: Declare CASCADE and RESTRICT tokens in parser.mly**

In `lib/sql/parser.mly`, after the existing `%token TRIGGER BEFORE AFTER` line, add:

```
%token CASCADE RESTRICT
```

- [ ] **Step 3: Update Col_fk_ref variant in parser.mly header**

In the `%{ ... %}` header block at the top of `parser.mly`, change line 9 from:

```ocaml
| Col_fk_ref  of string * string option  (* parent_table, parent_col *)
```

to:

```ocaml
| Col_fk_ref  of string * string option * Ast.fk_action * Ast.fk_action
```

- [ ] **Step 4: Add fk_ref_action and fk_on_clauses grammar rules**

In `lib/sql/parser.mly`, add these rules after the existing `trigger_body` rule (near the bottom):

```
(* Single referential action: CASCADE | RESTRICT | SET NULL | SET DEFAULT | NO ACTION *)
fk_ref_action:
  | CASCADE       { Ast.FA_cascade }
  | RESTRICT      { Ast.FA_restrict }
  | SET NULL      { Ast.FA_set_null }
  | SET DEFAULT   { Ast.FA_set_default }
  | IDENT IDENT   { if String.uppercase_ascii $1 = "NO" then Ast.FA_no_action
                    else Ast.FA_restrict }
  | IDENT         { if String.uppercase_ascii $1 = "RESTRICT" then Ast.FA_restrict
                    else Ast.FA_restrict }

(* Optional ON DELETE / ON UPDATE pair in any order *)
fk_on_clauses:
  | ON DELETE od = fk_ref_action ON UPDATE ou = fk_ref_action { (od, ou) }
  | ON UPDATE ou = fk_ref_action ON DELETE od = fk_ref_action { (od, ou) }
  | ON DELETE od = fk_ref_action  { (od, Ast.FA_restrict) }
  | ON UPDATE ou = fk_ref_action  { (Ast.FA_restrict, ou) }
  |                               { (Ast.FA_restrict, Ast.FA_restrict) }
```

- [ ] **Step 5: Update column-level REFERENCES rules**

In the `column_constraint` rule, replace the two existing REFERENCES alternatives (lines 279-280):

```
  | REFERENCES t = IDENT oc = fk_on_clauses
    { let (od, ou) = oc in Col_fk_ref (t, None, od, ou) }
  | REFERENCES t = IDENT LPAREN c = IDENT RPAREN oc = fk_on_clauses
    { let (od, ou) = oc in Col_fk_ref (t, Some c, od, ou) }
```

- [ ] **Step 6: Update column_def fk_ref extraction**

In the `column_def` rule (around line 261), update the `fk_ref` fold:

```ocaml
      let fk_ref      = List.fold_left (fun acc c ->
          match c with
          | Col_fk_ref (t, col_opt, od, ou) ->
            Some (t, Option.value ~default:"" col_opt, od, ou)
          | _ -> acc) None cs in
```

- [ ] **Step 7: Update table-level FOREIGN KEY rule**

In the `table_item` rule (around line 216), replace the `FOREIGN KEY ... REFERENCES ...` alternative:

```
  | FOREIGN KEY LPAREN local_cols = separated_nonempty_list(COMMA, IDENT) RPAREN
      REFERENCES parent_table = IDENT LPAREN parent_cols = separated_nonempty_list(COMMA, IDENT) RPAREN
      oc = fk_on_clauses
    { let (on_delete, on_update) = oc in
      TI_constraint (Ast.TC_foreign_key {
        local_cols; parent_table; parent_cols; on_delete; on_update;
      }) }
```

- [ ] **Step 8: Verify parser builds (expect sema errors, not parser errors)**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build lib/sql/ 2>&1 | head -20
```

Expected: parser builds without errors. Remaining errors should be in `sema.ml` about fk tuple shape.

- [ ] **Step 9: Commit**

```bash
git add lib/sql/lexer.mll lib/sql/parser.mly
git commit -m "feat(phase23): add CASCADE/RESTRICT tokens and ON DELETE/UPDATE clause to parser [#130]"
```

---

### Task 4: Wire fk actions through sema → plan → planner → exec Op_create_table

**Files:**
- Modify: `lib/sql/sema.mli`
- Modify: `lib/sql/sema.ml`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/exec.ml` (Op_create_table section only)

- [ ] **Step 1: Update fk_constraints tuple in sema.mli**

In `lib/sql/sema.mli`, find `BS_create_table` (around line 91). Change:

```ocaml
      fk_constraints : (string * string * string) list;
        (** [(local_col, parent_table, parent_col)] *)
```

to:

```ocaml
      fk_constraints : (string * string * string * Sqlocaml_catalog.Catalog.fk_action * Sqlocaml_catalog.Catalog.fk_action) list;
        (** [(local_col, parent_table, parent_col, on_delete, on_update)] *)
```

- [ ] **Step 2: Update fk_constraints tuple in plan.ml**

In `lib/sql/plan.ml`, find `Op_create_table`. Change:

```ocaml
      fk_constraints : (string * string * string) list;
```

to:

```ocaml
      fk_constraints : (string * string * string * Sqlocaml_catalog.Catalog.fk_action * Sqlocaml_catalog.Catalog.fk_action) list;
```

- [ ] **Step 3: Update col_fks extraction in sema.ml**

In `lib/sql/sema.ml`, find the `col_fks_result` fold (around line 944). Change the `fk_ref` pattern:

```ocaml
      let col_fks_result =
        List.fold_left (fun acc (cd : Ast.column_def) ->
          match acc with
          | Error _ as e -> e
          | Ok fks ->
            (match cd.Ast.fk_ref with
             | None -> Ok fks
             | Some (parent_table, "", _, _) ->
               Error (Unsupported (Printf.sprintf
                 "FOREIGN KEY on '%s': explicit parent column required, write REFERENCES %s(col)"
                 cd.Ast.name parent_table))
             | Some (parent_table, parent_col, od, ou) ->
               Ok (fks @ [(cd.Ast.name, parent_table, parent_col, od, ou)]))
        ) (Ok []) columns
      in
```

- [ ] **Step 4: Update tbl_fks extraction in sema.ml**

In `lib/sql/sema.ml`, find the `tbl_fks_result` fold (around line 963). Change the TC_foreign_key pattern:

```ocaml
      let tbl_fks_result =
        List.fold_left (fun acc c ->
          match acc with
          | Error _ as e -> e
          | Ok fks ->
            (match c with
             | Ast.TC_foreign_key { local_cols; parent_table; parent_cols; on_delete; on_update } ->
               (match local_cols, parent_cols with
                | [lc], [pc] -> Ok (fks @ [(lc, parent_table, pc, on_delete, on_update)])
                | _ ->
                  Error (Unsupported "multi-column FOREIGN KEY constraints are not yet supported"))
             | _ -> Ok fks)
        ) (Ok []) constraints
      in
```

- [ ] **Step 5: Update Op_create_table fk_list construction in exec.ml**

In `lib/sql/exec.ml`, find the `fk_list` construction in the `Op_create_table` handler (around line 1903):

Replace:
```ocaml
          let fk_list = List.map (fun (lc, pt, pc) ->
            Cat.{ fk_local_col = lc; fk_parent_table = pt; fk_parent_col = pc }
          ) fk_constraints in
```

With:
```ocaml
          let fk_list = List.map (fun (lc, pt, pc, od, ou) ->
            Cat.{ fk_local_col = lc; fk_parent_table = pt; fk_parent_col = pc;
                  fk_on_delete = od; fk_on_update = ou }
          ) fk_constraints in
```

- [ ] **Step 6: Build and run all tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe 2>&1 | tail -10
```

Expected: all 348 existing tests pass. The planner.ml passes the tuple through opaquely; if it needs an update, the type error will show in the build and the fix is to update the `fk_constraints` field in the `Op_create_table` constructor in `planner.ml` to match (it should be automatic since the tuple type is structural, not named).

- [ ] **Step 7: Commit**

```bash
git add lib/sql/sema.mli lib/sql/sema.ml lib/sql/plan.ml lib/sql/exec.ml
git commit -m "feat(phase23): wire fk_action through sema→plan→exec Op_create_table [#130]"
```

---

### Task 5: Exec helper functions for in-transaction cascade operations

**Files:**
- Modify: `lib/sql/exec.ml`

- [ ] **Step 1: Add three helper functions after fk_child_has_ref in exec.ml**

In `lib/sql/exec.ml`, after the existing `fk_child_has_ref` function (around line 1565), add:

```ocaml
(** Scan [child_meta] using an existing RW transaction for rows where
    [child_col_idx] equals [parent_val]. Returns (rowid, row) list. *)
let scan_child_rows_tx tx (child_meta : Cat.table_meta) ~child_col_idx ~(parent_val : Row.value) =
  let schema = child_meta.Cat.columns in
  let* cur   = S.cursor_open tx child_meta.Cat.tree_id in
  let _sr    = S.cursor_first cur in
  let buf    = ref [] in
  let rec scan () =
    match S.cursor_next cur with
    | None -> ()
    | Some (kbytes, vbytes) ->
      let rowid = Rowid.decode kbytes in
      let row   = Row.decode schema vbytes in
      if compare_values row.(child_col_idx) parent_val = 0 then
        buf := (rowid, row) :: !buf;
      scan ()
  in
  scan ();
  S.cursor_close cur;
  Lwt.return (List.rev !buf)

(** Delete a single row and its index entries within an existing RW transaction. *)
let delete_row_in_tx tx (cat : Cat.t) (meta : Cat.table_meta) ~rowid ~(row : Row.t) =
  let rowid_key  = Rowid.encode rowid in
  let child_idxs = Cat.indexes_for_table cat ~table:meta.Cat.name in
  let* () = Lwt_list.iter_s (fun (idx : Cat.index_info) ->
    let col_is   = List.map (find_col_idx_by_name meta.Cat.columns) idx.idx_columns in
    let iks      = List.map (fun ci -> row_value_to_index_value row.(ci)) col_is in
    let old_ikey = Index_key.encode iks ~rowid in
    S.del tx idx.idx_tree_id old_ikey
  ) child_idxs in
  S.del tx meta.Cat.tree_id rowid_key

(** Update one column to [new_val] in a row within an existing RW transaction.
    Also updates index entries for any index that covers [col_idx]. *)
let update_col_in_tx tx (cat : Cat.t) (meta : Cat.table_meta) ~rowid ~(row : Row.t) ~col_idx ~new_val =
  let schema     = meta.Cat.columns in
  let rowid_key  = Rowid.encode rowid in
  let new_row    = Array.copy row in
  new_row.(col_idx) <- new_val;
  let child_idxs = Cat.indexes_for_table cat ~table:meta.Cat.name in
  let* () = Lwt_list.iter_s (fun (idx : Cat.index_info) ->
    let col_is = List.map (find_col_idx_by_name schema) idx.idx_columns in
    if not (List.mem col_idx col_is) then Lwt.return_unit
    else begin
      let old_iks  = List.map (fun ci -> row_value_to_index_value row.(ci)) col_is in
      let new_iks  = List.map (fun ci -> row_value_to_index_value new_row.(ci)) col_is in
      let old_ikey = Index_key.encode old_iks ~rowid in
      let new_ikey = Index_key.encode new_iks ~rowid in
      let* () = S.del tx idx.idx_tree_id old_ikey in
      S.put tx idx.idx_tree_id new_ikey Bytes.empty
    end
  ) child_idxs in
  let new_bytes = Row.encode schema new_row in
  S.put tx meta.Cat.tree_id rowid_key new_bytes
```

- [ ] **Step 2: Build to verify helpers compile**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build lib/sql/ 2>&1
```

Expected: succeeds (helpers are defined but not yet called).

- [ ] **Step 3: Commit**

```bash
git add lib/sql/exec.ml
git commit -m "feat(phase23): add scan_child_rows_tx, delete_row_in_tx, update_col_in_tx in exec.ml [#130]"
```

---

### Task 6: Implement cascade actions in execute_delete

**Files:**
- Modify: `lib/sql/exec.ml`

- [ ] **Step 1: Restructure the FK pre-check block in execute_delete**

In `lib/sql/exec.ml`, find `execute_delete`. The FK parent-side check block starts around line 1784 with:

```ocaml
    (* FK parent-side check: fail if any child row references a to-be-deleted row. *)
    let* child_refs = build_child_refs cat ~parent_table_name:table_meta.Cat.name in
    let* () =
      if child_refs = [] then Lwt.return_unit
      else
        Lwt_list.iter_s (fun (_rowid, row) ->
          Lwt_list.iter_s (fun (child_meta, fks) ->
            Lwt_list.iter_s (fun (fk : Cat.fk_constraint) ->
              let parent_col_idx = find_col_idx_by_name table_meta.Cat.columns fk.fk_parent_col in
              let parent_val = row.(parent_col_idx) in
              (match parent_val with
               | Row.V_null -> Lwt.return_unit
               | _ ->
                 let child_col_idx = find_col_idx_by_name child_meta.Cat.columns fk.fk_local_col in
                 let* has_ref = fk_child_has_ref store child_meta ~child_col_idx ~parent_val in
                 if has_ref then
                   Lwt.fail_with (Printf.sprintf
                     "FOREIGN KEY constraint failed: '%s.%s' is still referenced by '%s.%s'"
                     table_meta.Cat.name fk.fk_parent_col
                     child_meta.Cat.name fk.fk_local_col)
                 else Lwt.return_unit)
            ) fks
          ) child_refs
        ) matches
    in
```

Replace with (only RESTRICT/NO_ACTION fail early; CASCADE/SET_NULL/SET_DEFAULT are handled in the write phase):

```ocaml
    (* FK pre-check: fail immediately for RESTRICT/NO_ACTION.
       CASCADE/SET_NULL/SET_DEFAULT are applied inside the RW transaction below. *)
    let* child_refs = build_child_refs cat ~parent_table_name:table_meta.Cat.name in
    let* () =
      if child_refs = [] then Lwt.return_unit
      else
        Lwt_list.iter_s (fun (_rowid, row) ->
          Lwt_list.iter_s (fun (child_meta, fks) ->
            Lwt_list.iter_s (fun (fk : Cat.fk_constraint) ->
              match fk.fk_on_delete with
              | Cat.FA_cascade | Cat.FA_set_null | Cat.FA_set_default -> Lwt.return_unit
              | Cat.FA_restrict | Cat.FA_no_action ->
                let parent_col_idx = find_col_idx_by_name table_meta.Cat.columns fk.fk_parent_col in
                let parent_val = row.(parent_col_idx) in
                (match parent_val with
                 | Row.V_null -> Lwt.return_unit
                 | _ ->
                   let child_col_idx = find_col_idx_by_name child_meta.Cat.columns fk.fk_local_col in
                   let* has_ref = fk_child_has_ref store child_meta ~child_col_idx ~parent_val in
                   if has_ref then
                     Lwt.fail_with (Printf.sprintf
                       "FOREIGN KEY constraint failed: '%s.%s' is still referenced by '%s.%s'"
                       table_meta.Cat.name fk.fk_parent_col
                       child_meta.Cat.name fk.fk_local_col)
                   else Lwt.return_unit)
            ) fks
          ) child_refs
        ) matches
    in
```

- [ ] **Step 2: Add cascade execution inside the RW transaction per-row loop**

In `execute_delete`, inside the `Lwt.catch (fun () -> ...)` block, find the inner `Lwt_list.iter_s` that processes each row:

```ocaml
        let* () =
          Lwt_list.iter_s (fun (rowid, row) ->
            let rowid_key = Rowid.encode rowid in
            (* Remove index entries for this row. *)
            let* () = Lwt_list.iter_s (fun (idx : Cat.index_info) ->
              ...
```

Prepend the cascade execution before the `let rowid_key = ...`:

```ocaml
        let* () =
          Lwt_list.iter_s (fun (rowid, row) ->
            (* Apply FK cascade actions (CASCADE / SET NULL / SET DEFAULT) within same tx. *)
            let* () =
              if child_refs = [] then Lwt.return_unit
              else
                Lwt_list.iter_s (fun (child_meta, fks) ->
                  Lwt_list.iter_s (fun (fk : Cat.fk_constraint) ->
                    let parent_col_idx = find_col_idx_by_name table_meta.Cat.columns fk.fk_parent_col in
                    let parent_val = row.(parent_col_idx) in
                    (match parent_val with
                     | Row.V_null -> Lwt.return_unit
                     | _ ->
                       let child_col_idx = find_col_idx_by_name child_meta.Cat.columns fk.fk_local_col in
                       (match fk.fk_on_delete with
                        | Cat.FA_restrict | Cat.FA_no_action -> Lwt.return_unit
                        | Cat.FA_cascade ->
                          let* child_rows = scan_child_rows_tx tx child_meta ~child_col_idx ~parent_val in
                          Lwt_list.iter_s (fun (crid, crow) ->
                            delete_row_in_tx tx cat child_meta ~rowid:crid ~row:crow
                          ) child_rows
                        | Cat.FA_set_null ->
                          let* child_rows = scan_child_rows_tx tx child_meta ~child_col_idx ~parent_val in
                          Lwt_list.iter_s (fun (crid, crow) ->
                            update_col_in_tx tx cat child_meta ~rowid:crid ~row:crow
                              ~col_idx:child_col_idx ~new_val:Row.V_null
                          ) child_rows
                        | Cat.FA_set_default ->
                          let col = List.nth child_meta.Cat.columns child_col_idx in
                          let default_val = match col.Row.default with
                            | None               -> Row.V_null
                            | Some Row.DV_int  n -> Row.V_int  n
                            | Some Row.DV_text s -> Row.V_text s
                            | Some Row.DV_real f -> Row.V_real f
                            | Some Row.DV_blob b -> Row.V_blob b
                            | Some Row.DV_null   -> Row.V_null
                          in
                          let* child_rows = scan_child_rows_tx tx child_meta ~child_col_idx ~parent_val in
                          Lwt_list.iter_s (fun (crid, crow) ->
                            update_col_in_tx tx cat child_meta ~rowid:crid ~row:crow
                              ~col_idx:child_col_idx ~new_val:default_val
                          ) child_rows))
                  ) fks
                ) child_refs
            in
            let rowid_key = Rowid.encode rowid in
            (* Remove index entries for this row. *)
            let* () = Lwt_list.iter_s (fun (idx : Cat.index_info) ->
              let col_is = List.map (find_col_idx_by_name schema) idx.idx_columns in
              let iks = List.map (fun ci -> row_value_to_index_value row.(ci)) col_is in
              let old_ikey = Index_key.encode iks ~rowid in
              S.del tx idx.idx_tree_id old_ikey
            ) indexes in
            S.del tx table_meta.tree_id rowid_key
          ) matches
        in
```

- [ ] **Step 3: Run all e2e tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe 2>&1 | tail -10
```

Expected: all 348 tests pass.

- [ ] **Step 4: Commit**

```bash
git add lib/sql/exec.ml
git commit -m "feat(phase23): implement CASCADE/SET NULL/SET DEFAULT in execute_delete [#130]"
```

---

### Task 7: Implement cascade actions in execute_update

**Files:**
- Modify: `lib/sql/exec.ml`

- [ ] **Step 1: Restructure FK pre-check block in execute_update**

In `lib/sql/exec.ml`, find `execute_update`. The FK parent-side check block starts around line 1609 with:

```ocaml
    (* FK parent-side check: fail if updating a referenced parent column. *)
    let* child_refs = build_child_refs cat ~parent_table_name:table_meta.Cat.name in
    let* () =
      if child_refs = [] then Lwt.return_unit
      else
        Lwt_list.iter_s (fun (_rowid, old_row) ->
          let new_row = Array.copy old_row in
          List.iter (fun (i, expr) ->
            new_row.(i) <- eval_expr clock params old_row expr
          ) assignments;
          Lwt_list.iter_s (fun (child_meta, fks) ->
            Lwt_list.iter_s (fun (fk : Cat.fk_constraint) ->
              let parent_col_idx = find_col_idx_by_name table_meta.Cat.columns fk.fk_parent_col in
              let old_val = old_row.(parent_col_idx) in
              let new_val = new_row.(parent_col_idx) in
              if compare_values old_val new_val = 0 then Lwt.return_unit
              else
                (match old_val with
                 | Row.V_null -> Lwt.return_unit
                 | _ ->
                   let child_col_idx = find_col_idx_by_name child_meta.Cat.columns fk.fk_local_col in
                   let* has_ref = fk_child_has_ref store child_meta ~child_col_idx ~parent_val:old_val in
                   if has_ref then
                     Lwt.fail_with (Printf.sprintf
                       "FOREIGN KEY constraint failed: update to '%s.%s' is referenced by '%s.%s'"
                       table_meta.Cat.name fk.fk_parent_col
                       child_meta.Cat.name fk.fk_local_col)
                   else Lwt.return_unit)
            ) fks
          ) child_refs
        ) matches
    in
```

Replace with (only RESTRICT/NO_ACTION fail early):

```ocaml
    (* FK pre-check: fail for RESTRICT/NO_ACTION when referenced key changes.
       CASCADE/SET_NULL/SET_DEFAULT applied inside the RW transaction below. *)
    let* child_refs = build_child_refs cat ~parent_table_name:table_meta.Cat.name in
    let* () =
      if child_refs = [] then Lwt.return_unit
      else
        Lwt_list.iter_s (fun (_rowid, old_row) ->
          let new_row = Array.copy old_row in
          List.iter (fun (i, expr) ->
            new_row.(i) <- eval_expr clock params old_row expr
          ) assignments;
          Lwt_list.iter_s (fun (child_meta, fks) ->
            Lwt_list.iter_s (fun (fk : Cat.fk_constraint) ->
              match fk.fk_on_update with
              | Cat.FA_cascade | Cat.FA_set_null | Cat.FA_set_default -> Lwt.return_unit
              | Cat.FA_restrict | Cat.FA_no_action ->
                let parent_col_idx = find_col_idx_by_name table_meta.Cat.columns fk.fk_parent_col in
                let old_val = old_row.(parent_col_idx) in
                let new_val = new_row.(parent_col_idx) in
                if compare_values old_val new_val = 0 then Lwt.return_unit
                else
                  (match old_val with
                   | Row.V_null -> Lwt.return_unit
                   | _ ->
                     let child_col_idx = find_col_idx_by_name child_meta.Cat.columns fk.fk_local_col in
                     let* has_ref = fk_child_has_ref store child_meta ~child_col_idx ~parent_val:old_val in
                     if has_ref then
                       Lwt.fail_with (Printf.sprintf
                         "FOREIGN KEY constraint failed: update to '%s.%s' is referenced by '%s.%s'"
                         table_meta.Cat.name fk.fk_parent_col
                         child_meta.Cat.name fk.fk_local_col)
                     else Lwt.return_unit)
            ) fks
          ) child_refs
        ) matches
    in
```

- [ ] **Step 2: Add cascade execution in the second pass (actual update loop) in execute_update**

In `execute_update`, inside the RW `Lwt.catch` block, find the second pass `Lwt_list.iter_s` that applies updates (around line 1698). This loop has the pattern:

```ocaml
        let* () =
          Lwt_list.iter_s (fun (rowid, old_row) ->
            let new_row = Array.copy old_row in
            List.iter (fun (i, expr) ->
              new_row.(i) <- eval_expr clock params old_row expr
            ) assignments;
            (* Check constraints, update index entries, S.put ... *)
```

After computing `new_row` (after the `List.iter ... assignments` line) and before any index updates or `S.put`, insert:

```ocaml
            (* Apply FK cascade UPDATE actions (CASCADE / SET NULL / SET DEFAULT). *)
            let* () =
              if child_refs = [] then Lwt.return_unit
              else
                Lwt_list.iter_s (fun (child_meta, fks) ->
                  Lwt_list.iter_s (fun (fk : Cat.fk_constraint) ->
                    let parent_col_idx = find_col_idx_by_name table_meta.Cat.columns fk.fk_parent_col in
                    let old_val = old_row.(parent_col_idx) in
                    let new_val = new_row.(parent_col_idx) in
                    if compare_values old_val new_val = 0 then Lwt.return_unit
                    else
                      (match old_val with
                       | Row.V_null -> Lwt.return_unit
                       | _ ->
                         let child_col_idx = find_col_idx_by_name child_meta.Cat.columns fk.fk_local_col in
                         (match fk.fk_on_update with
                          | Cat.FA_restrict | Cat.FA_no_action -> Lwt.return_unit
                          | Cat.FA_cascade ->
                            let* child_rows = scan_child_rows_tx tx child_meta ~child_col_idx ~parent_val:old_val in
                            Lwt_list.iter_s (fun (crid, crow) ->
                              update_col_in_tx tx cat child_meta ~rowid:crid ~row:crow
                                ~col_idx:child_col_idx ~new_val
                            ) child_rows
                          | Cat.FA_set_null ->
                            let* child_rows = scan_child_rows_tx tx child_meta ~child_col_idx ~parent_val:old_val in
                            Lwt_list.iter_s (fun (crid, crow) ->
                              update_col_in_tx tx cat child_meta ~rowid:crid ~row:crow
                                ~col_idx:child_col_idx ~new_val:Row.V_null
                            ) child_rows
                          | Cat.FA_set_default ->
                            let col = List.nth child_meta.Cat.columns child_col_idx in
                            let default_val = match col.Row.default with
                              | None               -> Row.V_null
                              | Some Row.DV_int  n -> Row.V_int  n
                              | Some Row.DV_text s -> Row.V_text s
                              | Some Row.DV_real f -> Row.V_real f
                              | Some Row.DV_blob b -> Row.V_blob b
                              | Some Row.DV_null   -> Row.V_null
                            in
                            let* child_rows = scan_child_rows_tx tx child_meta ~child_col_idx ~parent_val:old_val in
                            Lwt_list.iter_s (fun (crid, crow) ->
                              update_col_in_tx tx cat child_meta ~rowid:crid ~row:crow
                                ~col_idx:child_col_idx ~new_val:default_val
                            ) child_rows))
                  ) fks
                ) child_refs
            in
```

Important: insert this AFTER computing `new_row` but BEFORE the existing index update code for the parent row, so cascade sees the old parent key value (`old_val`) and child FK rows are updated before the parent row write.

- [ ] **Step 3: Build and run all e2e tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe 2>&1 | tail -10
```

Expected: all 348 tests pass.

- [ ] **Step 4: Commit**

```bash
git add lib/sql/exec.ml
git commit -m "feat(phase23): implement CASCADE/SET NULL/SET DEFAULT in execute_update [#130]"
```

---

### Task 8: E2E tests for all FK referential actions

**Files:**
- Modify: `test/test_e2e.ml`

All new tests follow the exact same pattern as existing FK tests (e.g. `test_fk_delete_referenced_parent_fails`): wrap in `Lwt_main.run`, define a local `exec` helper, use `Db.query` + `Lwt_main.run (Lwt_stream.to_list s)`, and use pattern matching for value extraction.

- [ ] **Step 1: Add 8 test functions after test_fk_update_unreferenced_col_ok**

```ocaml
let test_fk_cascade_delete () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let exec sql =
      let* r = Db.execute db sql in
      (match r with Ok () -> () | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e);
      Lwt.return_unit
    in
    let* () = exec "CREATE TABLE cpar (id INTEGER PRIMARY KEY)" in
    let* () = exec "CREATE TABLE cchi (id INTEGER, pid INTEGER REFERENCES cpar(id) ON DELETE CASCADE)" in
    let* () = exec "INSERT INTO cpar VALUES (1)" in
    let* () = exec "INSERT INTO cpar VALUES (2)" in
    let* () = exec "INSERT INTO cchi VALUES (10, 1)" in
    let* () = exec "INSERT INTO cchi VALUES (11, 1)" in
    let* () = exec "INSERT INTO cchi VALUES (12, 2)" in
    let* () = exec "DELETE FROM cpar WHERE id = 1" in
    let* r = Db.query db "SELECT COUNT(*) FROM cchi" in
    let rows = match r with Ok s -> Lwt_main.run (Lwt_stream.to_list s) | Error e -> Alcotest.failf "%a" Db.pp_error e in
    Alcotest.(check int) "cascade deleted 2 rows, 1 remains" 1
      (match rows with [[|Db.V_int n|]] -> Int64.to_int n | _ -> -1);
    Lwt.return_unit)

let test_fk_cascade_delete_no_children () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let exec sql =
      let* r = Db.execute db sql in
      (match r with Ok () -> () | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e);
      Lwt.return_unit
    in
    let* () = exec "CREATE TABLE npar (id INTEGER PRIMARY KEY)" in
    let* () = exec "CREATE TABLE nchi (id INTEGER, pid INTEGER REFERENCES npar(id) ON DELETE CASCADE)" in
    let* () = exec "INSERT INTO npar VALUES (1)" in
    let* () = exec "DELETE FROM npar WHERE id = 1" in
    let* r = Db.query db "SELECT COUNT(*) FROM npar" in
    let rows = match r with Ok s -> Lwt_main.run (Lwt_stream.to_list s) | Error e -> Alcotest.failf "%a" Db.pp_error e in
    Alcotest.(check int) "parent deleted when no children" 0
      (match rows with [[|Db.V_int n|]] -> Int64.to_int n | _ -> -1);
    Lwt.return_unit)

let test_fk_cascade_update () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let exec sql =
      let* r = Db.execute db sql in
      (match r with Ok () -> () | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e);
      Lwt.return_unit
    in
    let* () = exec "CREATE TABLE upar (id INTEGER PRIMARY KEY)" in
    let* () = exec "CREATE TABLE uchi (id INTEGER, pid INTEGER REFERENCES upar(id) ON UPDATE CASCADE)" in
    let* () = exec "INSERT INTO upar VALUES (1)" in
    let* () = exec "INSERT INTO uchi VALUES (10, 1)" in
    let* () = exec "INSERT INTO uchi VALUES (11, 1)" in
    let* () = exec "UPDATE upar SET id = 99 WHERE id = 1" in
    let* r = Db.query db "SELECT pid FROM uchi ORDER BY id" in
    let rows = match r with Ok s -> Lwt_main.run (Lwt_stream.to_list s) | Error e -> Alcotest.failf "%a" Db.pp_error e in
    Alcotest.(check int) "2 child rows updated" 2 (List.length rows);
    Alcotest.check value_testable "child1 pid=99" (Db.V_int 99L) (List.nth rows 0).(0);
    Alcotest.check value_testable "child2 pid=99" (Db.V_int 99L) (List.nth rows 1).(0);
    Lwt.return_unit)

let test_fk_set_null_delete () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let exec sql =
      let* r = Db.execute db sql in
      (match r with Ok () -> () | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e);
      Lwt.return_unit
    in
    let* () = exec "CREATE TABLE snpar (id INTEGER PRIMARY KEY)" in
    let* () = exec "CREATE TABLE snchi (id INTEGER, pid INTEGER REFERENCES snpar(id) ON DELETE SET NULL)" in
    let* () = exec "INSERT INTO snpar VALUES (1)" in
    let* () = exec "INSERT INTO snchi VALUES (10, 1)" in
    let* () = exec "DELETE FROM snpar WHERE id = 1" in
    let* r = Db.query db "SELECT pid IS NULL FROM snchi" in
    let rows = match r with Ok s -> Lwt_main.run (Lwt_stream.to_list s) | Error e -> Alcotest.failf "%a" Db.pp_error e in
    Alcotest.(check int) "child pid set to null" 1
      (match rows with [[|Db.V_int n|]] -> Int64.to_int n | _ -> -1);
    Lwt.return_unit)

let test_fk_set_null_update () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let exec sql =
      let* r = Db.execute db sql in
      (match r with Ok () -> () | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e);
      Lwt.return_unit
    in
    let* () = exec "CREATE TABLE snupar (id INTEGER PRIMARY KEY)" in
    let* () = exec "CREATE TABLE snuchi (id INTEGER, pid INTEGER REFERENCES snupar(id) ON UPDATE SET NULL)" in
    let* () = exec "INSERT INTO snupar VALUES (1)" in
    let* () = exec "INSERT INTO snuchi VALUES (10, 1)" in
    let* () = exec "UPDATE snupar SET id = 99 WHERE id = 1" in
    let* r = Db.query db "SELECT pid IS NULL FROM snuchi" in
    let rows = match r with Ok s -> Lwt_main.run (Lwt_stream.to_list s) | Error e -> Alcotest.failf "%a" Db.pp_error e in
    Alcotest.(check int) "child pid set to null after parent update" 1
      (match rows with [[|Db.V_int n|]] -> Int64.to_int n | _ -> -1);
    Lwt.return_unit)

let test_fk_set_default_delete () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let exec sql =
      let* r = Db.execute db sql in
      (match r with Ok () -> () | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e);
      Lwt.return_unit
    in
    let* () = exec "CREATE TABLE sdpar (id INTEGER PRIMARY KEY)" in
    let* () = exec "INSERT INTO sdpar VALUES (0)" in
    let* () = exec "INSERT INTO sdpar VALUES (1)" in
    let* () = exec "CREATE TABLE sdchi (id INTEGER, pid INTEGER DEFAULT 0 REFERENCES sdpar(id) ON DELETE SET DEFAULT)" in
    let* () = exec "INSERT INTO sdchi VALUES (10, 1)" in
    let* () = exec "DELETE FROM sdpar WHERE id = 1" in
    let* r = Db.query db "SELECT pid FROM sdchi" in
    let rows = match r with Ok s -> Lwt_main.run (Lwt_stream.to_list s) | Error e -> Alcotest.failf "%a" Db.pp_error e in
    Alcotest.(check int) "child pid reset to default 0" 0
      (match rows with [[|Db.V_int n|]] -> Int64.to_int n | _ -> -1);
    Lwt.return_unit)

let test_fk_no_action_blocks_delete () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let exec sql =
      let* r = Db.execute db sql in
      (match r with Ok () -> () | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e);
      Lwt.return_unit
    in
    let* () = exec "CREATE TABLE napar (id INTEGER PRIMARY KEY)" in
    let* () = exec "INSERT INTO napar VALUES (1)" in
    let* () = exec "CREATE TABLE nachi (id INTEGER, pid INTEGER REFERENCES napar(id) ON DELETE NO ACTION)" in
    let* () = exec "INSERT INTO nachi VALUES (10, 1)" in
    let* result = Db.execute db "DELETE FROM napar WHERE id = 1" in
    Alcotest.(check bool) "NO ACTION blocks delete" true (Result.is_error result);
    Lwt.return_unit)

let test_fk_cascade_table_level_fk () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let exec sql =
      let* r = Db.execute db sql in
      (match r with Ok () -> () | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e);
      Lwt.return_unit
    in
    let* () = exec "CREATE TABLE tlpar (id INTEGER PRIMARY KEY)" in
    let* () = exec "CREATE TABLE tlchi (id INTEGER, pid INTEGER, FOREIGN KEY (pid) REFERENCES tlpar(id) ON DELETE CASCADE)" in
    let* () = exec "INSERT INTO tlpar VALUES (1)" in
    let* () = exec "INSERT INTO tlchi VALUES (10, 1)" in
    let* () = exec "DELETE FROM tlpar WHERE id = 1" in
    let* r = Db.query db "SELECT COUNT(*) FROM tlchi" in
    let rows = match r with Ok s -> Lwt_main.run (Lwt_stream.to_list s) | Error e -> Alcotest.failf "%a" Db.pp_error e in
    Alcotest.(check int) "table-level CASCADE FK deletes child" 0
      (match rows with [[|Db.V_int n|]] -> Int64.to_int n | _ -> -1);
    Lwt.return_unit)
```

- [ ] **Step 2: Register 8 tests in the fk_delete_update suite**

In `test/test_e2e.ml`, find the `"fk_delete_update"` group registration (around line 5471). Add after the existing `update_no_ref_ok` entry:

```ocaml
    Alcotest.test_case "cascade_delete"         `Quick test_fk_cascade_delete;
    Alcotest.test_case "cascade_delete_empty"   `Quick test_fk_cascade_delete_no_children;
    Alcotest.test_case "cascade_update"         `Quick test_fk_cascade_update;
    Alcotest.test_case "set_null_delete"        `Quick test_fk_set_null_delete;
    Alcotest.test_case "set_null_update"        `Quick test_fk_set_null_update;
    Alcotest.test_case "set_default_delete"     `Quick test_fk_set_default_delete;
    Alcotest.test_case "no_action_blocks"       `Quick test_fk_no_action_blocks_delete;
    Alcotest.test_case "cascade_table_level_fk" `Quick test_fk_cascade_table_level_fk;
```

- [ ] **Step 3: Run all e2e tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe 2>&1 | tail -15
```

Expected: 348 + 8 = 356 tests pass.

- [ ] **Step 4: Commit**

```bash
git add test/test_e2e.ml
git commit -m "feat(phase23): e2e tests for CASCADE DELETE/UPDATE, SET NULL, SET DEFAULT, NO ACTION [#130]"
```

---

### Task 9: SQLite comparison tests

**Files:**
- Modify: `test/test_sqlite_compare.ml`

The comparison test format uses records: `{ name = "..."; setup = ["sql1"; "sql2"; ...]; query = "SELECT ..."; unordered = bool }`.

- [ ] **Step 1: Add phase23_cascade_cases list**

In `test/test_sqlite_compare.ml`, after the `phase22_trigger_cases` list (after line 2992), add:

```ocaml
let phase23_cascade_cases = [
  { name = "cascade_delete_basic";
    setup = [
      "CREATE TABLE par (id INTEGER PRIMARY KEY)";
      "CREATE TABLE chi (id INTEGER, pid INTEGER REFERENCES par(id) ON DELETE CASCADE)";
      "INSERT INTO par VALUES (1)";
      "INSERT INTO par VALUES (2)";
      "INSERT INTO chi VALUES (10, 1)";
      "INSERT INTO chi VALUES (11, 1)";
      "INSERT INTO chi VALUES (12, 2)";
      "DELETE FROM par WHERE id = 1";
    ];
    query = "SELECT COUNT(*) FROM chi";
    unordered = false };

  { name = "cascade_delete_all";
    setup = [
      "CREATE TABLE par2 (id INTEGER PRIMARY KEY)";
      "CREATE TABLE chi2 (id INTEGER, pid INTEGER REFERENCES par2(id) ON DELETE CASCADE)";
      "INSERT INTO par2 VALUES (1)";
      "INSERT INTO par2 VALUES (2)";
      "INSERT INTO chi2 VALUES (10, 1)";
      "INSERT INTO chi2 VALUES (20, 2)";
      "DELETE FROM par2";
    ];
    query = "SELECT COUNT(*) FROM chi2";
    unordered = false };

  { name = "cascade_update";
    setup = [
      "CREATE TABLE par3 (id INTEGER PRIMARY KEY)";
      "CREATE TABLE chi3 (id INTEGER, pid INTEGER REFERENCES par3(id) ON UPDATE CASCADE)";
      "INSERT INTO par3 VALUES (1)";
      "INSERT INTO chi3 VALUES (10, 1)";
      "INSERT INTO chi3 VALUES (11, 1)";
      "UPDATE par3 SET id = 99 WHERE id = 1";
    ];
    query = "SELECT pid FROM chi3 ORDER BY id";
    unordered = false };

  { name = "set_null_on_delete";
    setup = [
      "CREATE TABLE par4 (id INTEGER PRIMARY KEY)";
      "CREATE TABLE chi4 (id INTEGER, pid INTEGER REFERENCES par4(id) ON DELETE SET NULL)";
      "INSERT INTO par4 VALUES (1)";
      "INSERT INTO chi4 VALUES (10, 1)";
      "DELETE FROM par4 WHERE id = 1";
    ];
    query = "SELECT pid IS NULL FROM chi4";
    unordered = false };

  { name = "cascade_table_level_fk";
    setup = [
      "CREATE TABLE par5 (id INTEGER PRIMARY KEY)";
      "CREATE TABLE chi5 (id INTEGER, pid INTEGER, FOREIGN KEY (pid) REFERENCES par5(id) ON DELETE CASCADE)";
      "INSERT INTO par5 VALUES (1)";
      "INSERT INTO chi5 VALUES (10, 1)";
      "INSERT INTO chi5 VALUES (11, 1)";
      "DELETE FROM par5";
    ];
    query = "SELECT COUNT(*) FROM chi5";
    unordered = false };
]
```

- [ ] **Step 2: Register phase23 cases**

In `test/test_sqlite_compare.ml`, add after the `"phase22_trigger"` entry:

```ocaml
    "phase23_cascade",         List.map make_test phase23_cascade_cases;
```

- [ ] **Step 3: Run SQLite comparison tests**

```bash
podman run --rm \
  -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z \
  -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
  -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
  -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
  -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
  -w /workspace sqlocaml-dev dune exec test/test_sqlite_compare.exe 2>&1 | tail -20
```

Expected: 12 pre-existing failures unchanged; all 5 new phase23 cases pass.

Note: sqlite3 enforces FK constraints only when `PRAGMA foreign_keys = ON`. Check how the comparison harness enables FK enforcement — if needed, add `"PRAGMA foreign_keys = ON"` as the first setup statement. Look at the `make_test` helper in `test_sqlite_compare.ml` to understand how setup runs.

- [ ] **Step 4: Final e2e run**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe 2>&1 | tail -5
```

Expected: 356 tests pass.

- [ ] **Step 5: Commit**

```bash
git add test/test_sqlite_compare.ml
git commit -m "feat(phase23): SQLite comparison tests for FK CASCADE, SET NULL, table-level CASCADE [#130]"
```

---

## Self-Review

**Spec coverage:**
- ✅ CASCADE DELETE: Tasks 5 + 6
- ✅ CASCADE UPDATE: Tasks 5 + 7
- ✅ SET NULL DELETE: Task 6
- ✅ SET NULL UPDATE: Task 7
- ✅ SET DEFAULT DELETE: Task 6
- ✅ SET DEFAULT UPDATE: Task 7
- ✅ NO ACTION (= RESTRICT, non-deferrable): Tasks 6 + 7 (pre-check path)
- ✅ Column-level REFERENCES ON DELETE/UPDATE: Task 3
- ✅ Table-level FOREIGN KEY ON DELETE/UPDATE: Task 3
- ✅ Persist on_delete/on_update in catalog: Task 1 + 4 (Op_create_table)
- ✅ Backward-compatible catalog decode (3-field → default restrict): Task 1
- ✅ E2E tests: Task 8 (8 tests)
- ✅ SQLite comparison tests: Task 9 (5 tests)

**Accepted limitations (not in scope):**
- DEFERRABLE INITIALLY DEFERRED: deferred to later phase
- Nested cascade chains: not supported; only direct children are cascaded
- Multi-column FK cascade: still returns Unsupported (existing error preserved)
- sqlocaml enables FK enforcement by default; sqlite3 requires `PRAGMA foreign_keys = ON` — handled in comparison test setup if needed

**Type consistency:**
- `Cat.fk_action` constructors `FA_no_action | FA_restrict | FA_cascade | FA_set_null | FA_set_default` — consistent across catalog.ml/mli, ast.ml alias, sema.ml, exec.ml
- fk_constraints 5-tuple `(string * string * string * Cat.fk_action * Cat.fk_action)` — consistent across sema.mli, sema.ml, plan.ml, exec.ml Op_create_table
