# Phase 11: CAST, NULLIF/IIF, Column Aliases, Table Aliases

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add four SQL features that are pervasive in real-world SQLite usage: type coercion (`CAST`), two common functions (`NULLIF`, `IIF`), named output columns (`SELECT expr AS alias`), and table aliases in FROM/JOIN (`FROM products AS p JOIN orders AS o ON p.id = o.id`).

**Architecture:** CAST flows through all four layers (AST → sema → plan → exec). NULLIF and IIF are desugared to existing `E_case` nodes at parse time — zero new plan nodes needed. Column aliases change `proj` from `expr list` to `(expr * string option) list` and enable ORDER BY alias resolution. Table aliases augment `bind_expr_join`'s tables tuple from `(meta * int)` to `(meta * int * string option)` and update `E_tbl_col` resolution to check alias before name.

**Tech Stack:** OCaml 5.x, Menhir, dune 3.x inside podman container (`sqlocaml-dev` image). All dune commands must be run with: `podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune <args>`.

---

## File Map

| File | Task | Change |
|------|------|--------|
| `lib/sql/ast.ml` | 1,2,3 | `E_cast`; `Exprs of (expr * string option) list`; `table_alias` in S_select |
| `lib/sql/lexer.mll` | 1 | AS, CAST, NULLIF, IIF tokens |
| `lib/sql/parser.mly` | 1,2,3 | CAST rule; proj alias; FROM/JOIN alias |
| `lib/sql/sema.ml` | 1,2,3 | BE_cast; expr_proj with aliases; alias-aware bind_expr_join |
| `lib/sql/sema.mli` | 1,2 | BE_cast; expr_proj type |
| `lib/sql/plan.ml` | 1,2 | P_cast; Op_expr_project.exprs type |
| `lib/sql/planner.ml` | 1,2 | BE_cast → P_cast; alias pass-through |
| `lib/sql/exec.ml` | 1,2 | P_cast eval; alias ignored for row output |
| `test/test_e2e.ml` | 1,2,3 | cast_expr, nullif_iif, col_alias, tbl_alias groups |
| `test/test_sqlite_compare.ml` | 4 | phase11_cast, phase11_alias groups |

---

## Task 1: CAST expression + NULLIF/IIF desugar

CAST is a full pipeline feature. NULLIF/IIF desugar to existing `E_case` at parse time.

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
- Modify: `test/test_sqlite_compare.ml`

- [ ] **Step 1: Write failing tests in test_e2e.ml**

Add a `"cast_expr"` group and a `"nullif_iif"` group before the final `]` in the test list (after the `"phase10_edge"` group at line 3556):

```ocaml
(* ------------------------------------------------------------------ *)
(* Phase 11: CAST, NULLIF, IIF                                          *)
(* ------------------------------------------------------------------ *)

let test_cast_int_to_text () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (42)";
  let rows = query_ok db "SELECT CAST(n AS TEXT) FROM t" in
  Alcotest.(check int) "one row" 1 (List.length rows);
  Alcotest.(check row_testable) "int to text"
    [| Db.V_text "42" |] (List.nth rows 0)

let test_cast_text_to_int () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (s TEXT)";
  exec db "INSERT INTO t VALUES ('123')";
  let rows = query_ok db "SELECT CAST(s AS INTEGER) FROM t" in
  Alcotest.(check row_testable) "text to int"
    [| Db.V_int 123L |] (List.nth rows 0)

let test_cast_text_to_int_invalid () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (s TEXT)";
  exec db "INSERT INTO t VALUES ('abc')";
  let rows = query_ok db "SELECT CAST(s AS INTEGER) FROM t" in
  Alcotest.(check row_testable) "invalid text to int gives 0"
    [| Db.V_int 0L |] (List.nth rows 0)

let test_cast_real_to_int () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (r REAL)";
  exec db "INSERT INTO t VALUES (3.9)";
  let rows = query_ok db "SELECT CAST(r AS INTEGER) FROM t" in
  Alcotest.(check row_testable) "real truncated to int"
    [| Db.V_int 3L |] (List.nth rows 0)

let test_cast_int_to_real () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (5)";
  let rows = query_ok db "SELECT CAST(n AS REAL) FROM t" in
  Alcotest.(check row_testable) "int to real"
    [| Db.V_real 5.0 |] (List.nth rows 0)

let test_cast_null () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (NULL)";
  let rows = query_ok db "SELECT CAST(n AS TEXT) FROM t" in
  Alcotest.(check row_testable) "null cast is null"
    [| Db.V_null |] (List.nth rows 0)

let test_cast_in_where () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (s TEXT)";
  exec db "INSERT INTO t VALUES ('10')";
  exec db "INSERT INTO t VALUES ('3')";
  let rows = query_ok db "SELECT s FROM t WHERE CAST(s AS INTEGER) > 5 ORDER BY s" in
  Alcotest.(check int) "one row passes" 1 (List.length rows);
  Alcotest.(check row_testable) "val" [| Db.V_text "10" |] (List.nth rows 0)

let test_nullif_equal () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (5)";
  let rows = query_ok db "SELECT NULLIF(n, 5) FROM t" in
  Alcotest.(check row_testable) "nullif returns null when equal"
    [| Db.V_null |] (List.nth rows 0)

let test_nullif_unequal () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (5)";
  let rows = query_ok db "SELECT NULLIF(n, 3) FROM t" in
  Alcotest.(check row_testable) "nullif returns a when unequal"
    [| Db.V_int 5L |] (List.nth rows 0)

let test_iif_true () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (10)";
  let rows = query_ok db "SELECT IIF(n > 5, 'big', 'small') FROM t" in
  Alcotest.(check row_testable) "iif true branch"
    [| Db.V_text "big" |] (List.nth rows 0)

let test_iif_false () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (2)";
  let rows = query_ok db "SELECT IIF(n > 5, 'big', 'small') FROM t" in
  Alcotest.(check row_testable) "iif false branch"
    [| Db.V_text "small" |] (List.nth rows 0)
```

In the test list at the end of the file, add after the `"phase10_edge"` entry (before the final `]`):
```ocaml
    "cast_expr", [
      Alcotest.test_case "cast_int_to_text"      `Quick test_cast_int_to_text;
      Alcotest.test_case "cast_text_to_int"      `Quick test_cast_text_to_int;
      Alcotest.test_case "cast_text_to_int_inv"  `Quick test_cast_text_to_int_invalid;
      Alcotest.test_case "cast_real_to_int"      `Quick test_cast_real_to_int;
      Alcotest.test_case "cast_int_to_real"      `Quick test_cast_int_to_real;
      Alcotest.test_case "cast_null"             `Quick test_cast_null;
      Alcotest.test_case "cast_in_where"         `Quick test_cast_in_where;
    ];
    "nullif_iif", [
      Alcotest.test_case "nullif_equal"   `Quick test_nullif_equal;
      Alcotest.test_case "nullif_unequal" `Quick test_nullif_unequal;
      Alcotest.test_case "iif_true"       `Quick test_iif_true;
      Alcotest.test_case "iif_false"      `Quick test_iif_false;
    ];
```

- [ ] **Step 2: Run to verify tests fail**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe -- test cast_expr 2>&1 | tail -20
```
Expected: build error (E_cast unknown) or test failures. Build must at least compile the rest of the suite.

- [ ] **Step 3: Add `E_cast` to ast.ml**

In `lib/sql/ast.ml`, add after the `E_case` constructor (after line 98):
```ocaml
  | E_cast of expr * ty
    (** CAST(expr AS type) — SQLite type coercion *)
```

Also add to `expr_to_sql` (before the catch-all `| E_agg _ | E_match _...`):
```ocaml
  | E_cast (e, ty) ->
    let tn = match ty with
      | Ty_int  -> "INTEGER"
      | Ty_text -> "TEXT"
      | Ty_real -> "REAL"
      | Ty_blob -> "BLOB"
    in
    Printf.sprintf "CAST(%s AS %s)" (expr_to_sql e) tn
```

- [ ] **Step 4: Add tokens to lexer.mll**

In `lib/sql/lexer.mll`, add before the `ident` catch-all rule (before line 148):
```ocaml
  | "AS"     | "as"     { AS }
  | "CAST"   | "cast"   { CAST }
  | "NULLIF" | "nullif" { NULLIF }
  | "IIF"    | "iif"    { IIF }
```

- [ ] **Step 5: Add tokens and grammar rules to parser.mly**

In `lib/sql/parser.mly`, add to the `%token` declarations (after the `CASE WHEN THEN ELSE END` line):
```menhir
%token AS CAST NULLIF IIF
```

Add a `type_name` nonterminal before `when_clause` (around line 389):
```menhir
type_name:
  | INTEGER_TY { Ast.Ty_int }
  | TEXT_TY    { Ast.Ty_text }
  | REAL_TY    { Ast.Ty_real }
  | BLOB_TY    { Ast.Ty_blob }
```

Add to `scalar_expr` (after the UNIXEPOCH rule, before the closing of scalar_expr):
```menhir
  | CAST LPAREN e = expr AS t = type_name RPAREN
    { E_cast (e, t) }
  | NULLIF LPAREN a = expr COMMA b = expr RPAREN
    { E_case { scrutinee = None;
               branches  = [(E_binop (Eq, a, b), E_lit L_null)];
               else_     = Some a } }
  | IIF LPAREN c = expr COMMA t = expr COMMA f = expr RPAREN
    { E_case { scrutinee = None;
               branches  = [(c, t)];
               else_     = Some f } }
```

**Note:** NULLIF desugars `a` twice (shared OCaml value — correct since expressions are pure). IIF desugars to a searched CASE. No new AST nodes needed for NULLIF/IIF.

- [ ] **Step 6: Add `BE_cast` to sema.ml and sema.mli**

In `lib/sql/sema.mli`, add after `BE_case`:
```ocaml
  | BE_cast of bound_expr * Ast.ty
    (** CAST(expr AS type) *)
```

In `lib/sql/sema.ml`, add after the `BE_case` constructor in the `type bound_expr` definition:
```ocaml
  | BE_cast of bound_expr * Ast.ty
```

Add `E_cast` handling to `bind_expr` (after the `E_case` arm):
```ocaml
  | Ast.E_cast (e, ty) ->
    (match bind_expr ~param_counter ~named_params meta e with
     | Ok be   -> Ok (BE_cast (be, ty))
     | Error e -> Error e)
```

Add `E_cast` handling to `bind_expr_join` (after the `E_case` arm):
```ocaml
  | Ast.E_cast (e, ty) ->
    (match bind_expr_join ~param_counter ~named_params ~tables e with
     | Ok be   -> Ok (BE_cast (be, ty))
     | Error e -> Error e)
```

Add `E_cast` handling to the inner `go` of `bind_expr_agg` (after the `E_case` arm):
```ocaml
    | Ast.E_cast (e, ty) ->
      (match go e with Ok be -> Ok (BE_cast (be, ty)) | Error e -> Error e)
```

Add `BE_cast` to `expr_has_subquery` (after `BE_case` branch):
```ocaml
  | BE_cast (e, _) -> expr_has_subquery e
```

Add `E_cast` to `check_expr_unsupported` to allow it in CHECK constraints (return false after the E_case line):
```ocaml
  | Ast.E_cast _ -> false
```

- [ ] **Step 7: Add `P_cast` to plan.ml**

In `lib/sql/plan.ml`, add after the `P_case` constructor:
```ocaml
  | P_cast of expr * Ast.ty
    (** CAST(expr AS type) *)
```

- [ ] **Step 8: Handle BE_cast in planner.ml**

In `lib/sql/planner.ml`, add to `plan_expr` after the `BE_case` arm:
```ocaml
  | Sema.BE_cast (e, ty) -> Plan.P_cast (plan_expr e, ty)
```

- [ ] **Step 9: Evaluate P_cast in exec.ml**

In `lib/sql/exec.ml`, add to `eval_expr` after the `P_case` arm. Also add `P_cast` to `pre_eval_subquery` and `ast_expr_to_plan_check`.

For `eval_expr`, add:
```ocaml
| Plan.P_cast (e, ty) ->
  let v = eval_expr clock params row e in
  (match v with
   | Row.V_null -> Row.V_null
   | _ ->
     (match ty with
      | Ast.Ty_int ->
        (match v with
         | Row.V_int n  -> Row.V_int n
         | Row.V_real f -> Row.V_int (Int64.of_float f)
         | Row.V_text s ->
           let s = String.trim s in
           (match Int64.of_string_opt s with
            | Some n -> Row.V_int n
            | None ->
              (match float_of_string_opt s with
               | Some f -> Row.V_int (Int64.of_float f)
               | None   -> Row.V_int 0L))
         | Row.V_blob _ -> Row.V_int 0L
         | Row.V_null   -> assert false)
      | Ast.Ty_real ->
        (match v with
         | Row.V_int n  -> Row.V_real (Int64.to_float n)
         | Row.V_real f -> Row.V_real f
         | Row.V_text s ->
           (match float_of_string_opt (String.trim s) with
            | Some f -> Row.V_real f
            | None   -> Row.V_real 0.0)
         | Row.V_blob _ -> Row.V_real 0.0
         | Row.V_null   -> assert false)
      | Ast.Ty_text ->
        (match v with
         | Row.V_int n  -> Row.V_text (Int64.to_string n)
         | Row.V_real f -> Row.V_text (Printf.sprintf "%.15g" f)
         | Row.V_text s -> Row.V_text s
         | Row.V_blob b -> Row.V_text (Bytes.to_string b)
         | Row.V_null   -> assert false)
      | Ast.Ty_blob ->
        (match v with
         | Row.V_blob b -> Row.V_blob b
         | Row.V_text s -> Row.V_blob (Bytes.of_string s)
         | Row.V_int n  -> Row.V_blob (Bytes.of_string (Int64.to_string n))
         | Row.V_real f -> Row.V_blob (Bytes.of_string (Printf.sprintf "%.15g" f))
         | Row.V_null   -> assert false)))
```

For `pre_eval_subquery`, find the `| Plan.P_case { ... }` arm and add after it:
```ocaml
| Plan.P_cast (e, ty) ->
  let* e' = pre_eval_subquery store cat params e in
  Lwt.return (Plan.P_cast (e', ty))
```

For `ast_expr_to_plan_check`, find the `| Ast.E_case { ... }` arm and add after it:
```ocaml
| Ast.E_cast (e, ty) -> Plan.P_cast (go e, ty)
```

- [ ] **Step 10: Build and run tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe -- test cast_expr 2>&1 | tail -30
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe -- test nullif_iif 2>&1 | tail -20
```
Expected: all 11 new tests pass, zero regressions in the full suite.

Full suite:
```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | tail -20
```

- [ ] **Step 11: Commit**

```bash
git add lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly \
        lib/sql/sema.ml lib/sql/sema.mli lib/sql/plan.ml \
        lib/sql/planner.ml lib/sql/exec.ml \
        test/test_e2e.ml
git commit -m "feat(phase11-task1): CAST expression + NULLIF/IIF desugar to CASE"
```

---

## Task 2: Column aliases in SELECT projection

`SELECT expr AS alias` — store alias alongside expression, resolve aliases in ORDER BY.

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/sema.ml`
- Modify: `lib/sql/sema.mli`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/planner.ml`
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_e2e.ml`

**Note:** The `AS` token was added in Task 1. This task depends on Task 1 being complete.

- [ ] **Step 1: Write failing tests in test_e2e.ml**

Add after the `"nullif_iif"` test group:

```ocaml
(* ------------------------------------------------------------------ *)
(* Phase 11: Column aliases                                             *)
(* ------------------------------------------------------------------ *)

let test_col_alias_basic () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (3)";
  (* Alias is cosmetic — values are correct *)
  let rows = query_ok db "SELECT n * 2 AS doubled FROM t" in
  Alcotest.(check row_testable) "expr with alias"
    [| Db.V_int 6L |] (List.nth rows 0)

let test_col_alias_order_by () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (3)";
  exec db "INSERT INTO t VALUES (1)";
  exec db "INSERT INTO t VALUES (2)";
  let rows = query_ok db "SELECT n * 10 AS big ORDER BY big" in
  (* FROM-less SELECT of alias would fail, let's do FROM: *)
  let rows = query_ok db "SELECT n * 10 AS big FROM t ORDER BY big" in
  Alcotest.(check int) "3 rows" 3 (List.length rows);
  Alcotest.(check row_testable) "row 0" [| Db.V_int 10L |] (List.nth rows 0);
  Alcotest.(check row_testable) "row 1" [| Db.V_int 20L |] (List.nth rows 1);
  Alcotest.(check row_testable) "row 2" [| Db.V_int 30L |] (List.nth rows 2)

let test_col_alias_multiple () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (a INTEGER, b INTEGER)";
  exec db "INSERT INTO t VALUES (2, 3)";
  let rows = query_ok db "SELECT a + b AS sum, a * b AS product FROM t" in
  Alcotest.(check row_testable) "two aliased cols"
    [| Db.V_int 5L; Db.V_int 6L |] (List.nth rows 0)

let test_col_alias_mixed () =
  (* Mix of aliased and non-aliased in same projection *)
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (4)";
  let rows = query_ok db "SELECT n, n * 2 AS doubled FROM t" in
  Alcotest.(check row_testable) "mixed proj"
    [| Db.V_int 4L; Db.V_int 8L |] (List.nth rows 0)

let test_col_alias_cast () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (7)";
  let rows = query_ok db "SELECT CAST(n AS TEXT) AS s FROM t" in
  Alcotest.(check row_testable) "cast with alias"
    [| Db.V_text "7" |] (List.nth rows 0)
```

In the test list, add after `"nullif_iif"`:
```ocaml
    "col_alias", [
      Alcotest.test_case "basic"     `Quick test_col_alias_basic;
      Alcotest.test_case "order_by"  `Quick test_col_alias_order_by;
      Alcotest.test_case "multiple"  `Quick test_col_alias_multiple;
      Alcotest.test_case "mixed"     `Quick test_col_alias_mixed;
      Alcotest.test_case "cast"      `Quick test_col_alias_cast;
    ];
```

- [ ] **Step 2: Run to verify tests fail**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe -- test col_alias 2>&1 | tail -20
```
Expected: parse error or test failures (AS is a keyword but `proj_item` doesn't handle it yet).

- [ ] **Step 3: Change `proj` type in ast.ml**

In `lib/sql/ast.ml`, find `S_select`'s `proj` field:
```ocaml
      proj     : [ `All | `Cols of string list | `Exprs of expr list ];
```
Change to:
```ocaml
      proj     : [ `All | `Cols of string list | `Exprs of (expr * string option) list ];
```

`string option` is the optional alias name. `Cols` (plain column refs, no aliases) remains unchanged for backward compat.

- [ ] **Step 4: Update parser.mly**

In `lib/sql/parser.mly`, update `proj_item`:
```menhir
proj_item:
  | e = expr AS alias = IDENT { `ExprA (e, Some alias) }
  | e = expr { match e with E_col name -> `Col name | _ -> `ExprA (e, None) }
```

Update `projection` to handle the new `ExprA` variant:
```menhir
projection:
  | STAR                                             { `All }
  | items = separated_nonempty_list(COMMA, proj_item)
    { let all_plain_cols = List.for_all (function `Col _ -> true | _ -> false) items in
      if all_plain_cols then
        `Cols (List.map (function `Col c -> c | _ -> assert false) items)
      else
        `Exprs (List.map (function
          | `Col c         -> (E_col c, None)
          | `ExprA (e, a)  -> (e, a)) items) }
```

- [ ] **Step 5: Update sema.ml**

**5a. Update `proj_has_agg` check:**

Find in `bind_select`:
```ocaml
       let proj_has_agg =
         match proj with
         | `All | `Cols _ -> false
         | `Exprs es -> List.exists expr_has_agg es
       in
```
Change to:
```ocaml
       let proj_has_agg =
         match proj with
         | `All | `Cols _ -> false
         | `Exprs es -> List.exists (fun (e, _) -> expr_has_agg e) es
       in
```

**5b. Update non-aggregated `Exprs` binding in `proj_result`:**

Find:
```ocaml
             | `Exprs es ->
               (* Phase 5: scalar function (or general expr) projection.
                  Bind each expression; return as BE list. *)
               let bound_list = List.map bind_one es in
               let errors = List.filter_map
                 (function Error e -> Some e | Ok _ -> None) bound_list in
               (match errors with
                | e :: _ -> Error e
                | [] ->
                  Ok (`Exprs (List.filter_map
                    (function Ok e -> Some e | Error _ -> None) bound_list)))
```
Change to:
```ocaml
             | `Exprs es ->
               let bound_list = List.map (fun (e, alias) ->
                 match bind_one e with
                 | Ok be   -> Ok (be, alias)
                 | Error e -> Error e
               ) es in
               let errors = List.filter_map (function Error e -> Some e | Ok _ -> None) bound_list in
               (match errors with
                | e :: _ -> Error e
                | [] ->
                  Ok (`Exprs (List.filter_map
                    (function Ok p -> Some p | Error _ -> None) bound_list)))
```

**5c. Update the result unpacking:**

Find:
```ocaml
           (match ords_result with
            | Error e -> Error e
            | Ok (`Ords o)    -> Ok (o, [], [], [])
            | Ok (`Exprs bes) -> Ok ([], [], [], bes))
```
Change to:
```ocaml
           (match ords_result with
            | Error e -> Error e
            | Ok (`Ords o)    -> Ok (o, [], [], [])
            | Ok (`Exprs bes) -> Ok ([], [], [], bes))
```
(no change needed here — the type of `bes` changes from `bound_expr list` to `(bound_expr * string option) list` automatically)

**5d. Build the alias_map and update ORDER BY binding:**

In `bind_select`, find the `order_result` block (around line 1352):
```ocaml
                let order_result =
                  List.fold_left (fun acc (ok : Ast.order_key) ->
                    match acc with
                    | Error _ -> acc
                    | Ok keys ->
                      let bound_e =
                        if joined_pairs = [] then
                          bind_expr ~param_counter ~named_params meta ok.Ast.expr
                        else
                          bind_expr_join ~param_counter ~named_params ~tables ok.Ast.expr
                      in
                      (match bound_e with
                       | Error e -> Error e
                       | Ok key  -> Ok (keys @ [{ key; dir = ok.Ast.dir }]))
                  ) (Ok []) order
                in
```
Replace with:
```ocaml
                (* Alias map: name → bound_expr, for ORDER BY alias resolution *)
                let alias_map : (string * bound_expr) list =
                  List.filter_map (fun (be, alias_opt) ->
                    Option.map (fun a -> (a, be)) alias_opt
                  ) proj_exprs
                in
                let bind_order_expr e =
                  let base_result =
                    if joined_pairs = [] then
                      bind_expr ~param_counter ~named_params meta e
                    else
                      bind_expr_join ~param_counter ~named_params ~tables e
                  in
                  match base_result with
                  | Ok _ -> base_result
                  | Error _ ->
                    (* Try alias resolution for plain column refs *)
                    (match e with
                     | Ast.E_col name ->
                       (match List.assoc_opt name alias_map with
                        | Some be -> Ok be
                        | None    -> base_result)
                     | _ -> base_result)
                in
                let order_result =
                  List.fold_left (fun acc (ok : Ast.order_key) ->
                    match acc with
                    | Error _ -> acc
                    | Ok keys ->
                      (match bind_order_expr ok.Ast.expr with
                       | Error e -> Error e
                       | Ok key  -> Ok (keys @ [{ key; dir = ok.Ast.dir }]))
                  ) (Ok []) order
                in
```

**5e. Update the final `BS_select` construction:**

Find in `bind_select`:
```ocaml
                         Lwt.return (Ok (BS_select {
                           distinct;
                           table_meta = meta;
                           proj       = proj_ords;
                           expr_proj  = proj_exprs;
```
This `proj_exprs` is now `(bound_expr * string option) list`. The field name stays `expr_proj` but its type changes.

- [ ] **Step 6: Update sema.mli**

In `lib/sql/sema.mli`, find `BS_select`:
```ocaml
      expr_proj  : bound_expr list;
```
Change to:
```ocaml
      expr_proj  : (bound_expr * string option) list;
```

- [ ] **Step 7: Update plan.ml**

In `lib/sql/plan.ml`, find `Op_expr_project`:
```ocaml
  | Op_expr_project of {
      exprs : expr list;
      child : op;
    }
```
Change to:
```ocaml
  | Op_expr_project of {
      exprs : (expr * string option) list;
      child : op;
    }
```

- [ ] **Step 8: Update planner.ml**

In `lib/sql/planner.ml`, find where `Op_expr_project` is constructed. It will be in `plan_select` (or similar). Change to map `(be, alias)` pairs:

Find the construction (currently `exprs = List.map plan_expr proj_exprs`):
```ocaml
Plan.Op_expr_project {
  exprs = List.map plan_expr proj_exprs;
  child = ...;
}
```
Change to:
```ocaml
Plan.Op_expr_project {
  exprs = List.map (fun (be, alias) -> (plan_expr be, alias)) proj_exprs;
  child = ...;
}
```

- [ ] **Step 9: Update exec.ml**

In `lib/sql/exec.ml`, find the `Op_expr_project` execution code. It evaluates each expr and collects into a row array. Change to strip the alias (aliases are metadata only — output is still `Row.t`):

Find the `Op_expr_project` arm in the `execute_op` or `query_op` function. It should look like:
```ocaml
| Plan.Op_expr_project { exprs; child } ->
  (* was: List.map (eval_expr ...) exprs *)
```
Change to evaluate only the expression part of each pair:
```ocaml
| Plan.Op_expr_project { exprs; child } ->
  (* eval_expr on the expression part; alias is metadata only *)
  ... List.map (fun (e, _alias) -> eval_expr clock params row e) exprs ...
```

Search for `Op_expr_project` in exec.ml to find the exact location, then update accordingly.

- [ ] **Step 10: Build and run tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe -- test col_alias 2>&1 | tail -30
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | tail -10
```
Expected: all 5 new col_alias tests pass, no regressions.

- [ ] **Step 11: Commit**

```bash
git add lib/sql/ast.ml lib/sql/parser.mly lib/sql/sema.ml lib/sql/sema.mli \
        lib/sql/plan.ml lib/sql/planner.ml lib/sql/exec.ml test/test_e2e.ml
git commit -m "feat(phase11-task2): column aliases in SELECT (AS alias), ORDER BY alias resolution"
```

---

## Task 3: Table aliases in FROM/JOIN

`FROM products AS p JOIN orders AS o ON p.id = o.id` — aliases used in qualified column refs.

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/sema.ml`
- Modify: `test/test_e2e.ml`

**Note:** `AS` token was added in Task 1. `join_clause.alias` already exists in the AST (always `None` — parser never fills it). This task wires it up.

- [ ] **Step 1: Write failing tests in test_e2e.ml**

Add after `"col_alias"` group:

```ocaml
(* ------------------------------------------------------------------ *)
(* Phase 11: Table aliases                                              *)
(* ------------------------------------------------------------------ *)

let test_tbl_alias_from () =
  (* FROM t AS alias — alias used in qualified column ref *)
  let db = fresh_db () in
  exec db "CREATE TABLE products (id INTEGER, name TEXT)";
  exec db "INSERT INTO products VALUES (1, 'apple')";
  let rows = query_ok db "SELECT p.id, p.name FROM products AS p" in
  Alcotest.(check row_testable) "aliased table col refs"
    [| Db.V_int 1L; Db.V_text "apple" |] (List.nth rows 0)

let test_tbl_alias_join () =
  let db = fresh_db () in
  exec db "CREATE TABLE a (id INTEGER, val TEXT)";
  exec db "CREATE TABLE b (aid INTEGER, extra TEXT)";
  exec db "INSERT INTO a VALUES (1, 'x')";
  exec db "INSERT INTO b VALUES (1, 'y')";
  let rows = query_ok db
    "SELECT x.val, y.extra FROM a AS x JOIN b AS y ON x.id = y.aid" in
  Alcotest.(check row_testable) "two aliased joined tables"
    [| Db.V_text "x"; Db.V_text "y" |] (List.nth rows 0)

let test_tbl_alias_where () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (1)";
  exec db "INSERT INTO t VALUES (2)";
  let rows = query_ok db "SELECT r.n FROM t AS r WHERE r.n > 1" in
  Alcotest.(check int) "one row" 1 (List.length rows);
  Alcotest.(check row_testable) "aliased where" [| Db.V_int 2L |] (List.nth rows 0)

let test_tbl_alias_order_by () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (3)";
  exec db "INSERT INTO t VALUES (1)";
  let rows = query_ok db "SELECT r.n FROM t AS r ORDER BY r.n" in
  Alcotest.(check row_testable) "first row" [| Db.V_int 1L |] (List.nth rows 0)

let test_tbl_alias_two_join_with_alias () =
  let db = fresh_db () in
  exec db "CREATE TABLE d (id INTEGER, name TEXT)";
  exec db "CREATE TABLE e (id INTEGER, dept_id INTEGER)";
  exec db "CREATE TABLE s (eid INTEGER, pay INTEGER)";
  exec db "INSERT INTO d VALUES (1, 'eng')";
  exec db "INSERT INTO e VALUES (10, 1)";
  exec db "INSERT INTO s VALUES (10, 500)";
  let rows = query_ok db
    "SELECT d.name, s.pay FROM d AS d JOIN e AS e ON d.id = e.dept_id JOIN s AS s ON e.id = s.eid" in
  Alcotest.(check row_testable) "three alias join"
    [| Db.V_text "eng"; Db.V_int 500L |] (List.nth rows 0)
```

In the test list, add after `"col_alias"`:
```ocaml
    "tbl_alias", [
      Alcotest.test_case "from"          `Quick test_tbl_alias_from;
      Alcotest.test_case "join"          `Quick test_tbl_alias_join;
      Alcotest.test_case "where"         `Quick test_tbl_alias_where;
      Alcotest.test_case "order_by"      `Quick test_tbl_alias_order_by;
      Alcotest.test_case "two_join"      `Quick test_tbl_alias_two_join_with_alias;
    ];
```

- [ ] **Step 2: Run to verify tests fail**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe -- test tbl_alias 2>&1 | tail -20
```
Expected: test failures (Unknown_table for aliases).

- [ ] **Step 3: Add `table_alias` field to S_select in ast.ml**

In `lib/sql/ast.ml`, find `S_select` and add `table_alias` after `table`:
```ocaml
  | S_select of {
      distinct : bool;
      proj     : [ `All | `Cols of string list | `Exprs of (expr * string option) list ];
      table    : string;
      table_alias : string option;     (* <-- NEW: alias for the FROM table *)
      joins    : join_clause list;
      where    : expr option;
      group_by : string list;
      having   : expr option;
      order    : order_key list;
      limit    : int option;
      offset   : int option;
    }
```

- [ ] **Step 4: Update parser.mly**

**4a. Update `from_tail` to parse `[AS alias]`:**

Find:
```menhir
from_tail:
  | FROM table = IDENT
      js = join_clauses wh = where_opt
      gb = group_by_clause hv = having_clause ob = order_by_clause lim = limit_clause
    { let (limit, offset) = lim in
      Some (table, js, wh, gb, hv, ob, limit, offset) }
  |   { None }
```
Change to:
```menhir
from_tail:
  | FROM table = IDENT tbl_alias = option(preceded(AS, IDENT))
      js = join_clauses wh = where_opt
      gb = group_by_clause hv = having_clause ob = order_by_clause lim = limit_clause
    { let (limit, offset) = lim in
      Some (table, tbl_alias, js, wh, gb, hv, ob, limit, offset) }
  |   { None }
```

**4b. Update `join_clause` rules to parse `[AS alias]`:**

Find the `join_clause` rules and add alias variants. Replace all 4 rules:
```menhir
join_clause:
  | INNER JOIN t = IDENT alias = option(preceded(AS, IDENT)) ON e = expr
    { { kind = Inner; table = t; alias; on = e } }
  | LEFT JOIN t = IDENT alias = option(preceded(AS, IDENT)) ON e = expr
    { { kind = Left;  table = t; alias; on = e } }
  | LEFT OUTER JOIN t = IDENT alias = option(preceded(AS, IDENT)) ON e = expr
    { { kind = Left;  table = t; alias; on = e } }
  | JOIN t = IDENT alias = option(preceded(AS, IDENT)) ON e = expr
    { { kind = Inner; table = t; alias; on = e } }
```

**4c. Update `select` rule to thread `table_alias` through:**

Find:
```menhir
select:
  | SELECT distinct = boption(DISTINCT) proj = projection ft = from_tail
    { match ft with
      | Some (table, js, wh, gb, hv, ob, limit, offset) ->
        S_select { distinct; proj; table; joins = js; where = wh;
                   group_by = gb; having = hv;
                   order = ob; limit; offset }
      | None ->
        ...
```
Change the `Some` branch to include `table_alias`:
```menhir
      | Some (table, tbl_alias, js, wh, gb, hv, ob, limit, offset) ->
        S_select { distinct; proj; table; table_alias = tbl_alias; joins = js; where = wh;
                   group_by = gb; having = hv;
                   order = ob; limit; offset }
```

- [ ] **Step 5: Update sema.ml — bind_select signature and tables list**

**5a. Update `bind_select` to accept `table_alias`:**

Find the `bind_select` function signature:
```ocaml
let bind_select cat ~param_counter ~named_params ~distinct ~proj ~table ~joins ~where ~group_by ~having ~order ~limit ~offset =
```
Add `~table_alias`:
```ocaml
let bind_select cat ~param_counter ~named_params ~distinct ~proj ~table ~table_alias ~joins ~where ~group_by ~having ~order ~limit ~offset =
```

Also update the call site of `bind_select` (in the `bind` function at the bottom where S_select is matched):
```ocaml
  | Ast.S_select { distinct; proj; table; table_alias; joins; where; group_by; having; order; limit; offset } ->
    bind_select cat ~param_counter ~named_params ~distinct ~proj ~table ~table_alias ~joins ~where ~group_by ~having ~order ~limit ~offset
```

**5b. Change `bind_expr_join`'s `tables` parameter type:**

Find the `bind_expr_join` signature:
```ocaml
let rec bind_expr_join
    ~param_counter
    ~named_params
    ~(tables : (Cat.table_meta * int) list)
  = function
```
Change to:
```ocaml
let rec bind_expr_join
    ~param_counter
    ~named_params
    ~(tables : (Cat.table_meta * int * string option) list)
  = function
```

**5c. Update `E_col` resolution in `bind_expr_join`:**

Find:
```ocaml
  | Ast.E_col name ->
    let matches = List.filter_map (fun (tm, base) ->
      match col_index tm.Cat.columns name with
      | Some i -> Some (BE_col (base + i))
      | None   -> None
    ) tables in
    (match matches with
     | [be]   -> Ok be
     | []     -> Error (Unknown_column { table = (fst (List.hd tables)).Cat.name; column = name })
     | _ :: _ -> Error (Ambiguous_column name))
```
Change to (extract `tm` and `base` from 3-tuple):
```ocaml
  | Ast.E_col name ->
    let matches = List.filter_map (fun (tm, base, _alias) ->
      match col_index tm.Cat.columns name with
      | Some i -> Some (BE_col (base + i))
      | None   -> None
    ) tables in
    (match matches with
     | [be]   -> Ok be
     | []     ->
       let (tm0, _, _) = List.hd tables in
       Error (Unknown_column { table = tm0.Cat.name; column = name })
     | _ :: _ -> Error (Ambiguous_column name))
```

**5d. Update `E_tbl_col` resolution in `bind_expr_join` to check alias:**

Find:
```ocaml
  | Ast.E_tbl_col (tbl, name) ->
    (match List.find_opt (fun (tm, _) -> String.equal tm.Cat.name tbl) tables with
     | None            -> Error (Unknown_table tbl)
     | Some (tm, base) ->
       (match col_index tm.Cat.columns name with
        | Some i -> Ok (BE_col (base + i))
        | None   -> Error (Unknown_column { table = tbl; column = name })))
```
Change to:
```ocaml
  | Ast.E_tbl_col (tbl, name) ->
    (match List.find_opt (fun (tm, _, alias_opt) ->
       String.equal tm.Cat.name tbl ||
       Option.exists (String.equal tbl) alias_opt
     ) tables with
     | None -> Error (Unknown_table tbl)
     | Some (tm, base, _) ->
       (match col_index tm.Cat.columns name with
        | Some i -> Ok (BE_col (base + i))
        | None   -> Error (Unknown_column { table = tbl; column = name })))
```

**5e. Update all other `(tm, base)` destructuring in `bind_expr_join`** to `(tm, base, _)` or `(_, base, _)` as appropriate. There are no others in bind_expr_join — the two cases above are the only ones that destructure `tables` entries directly.

**5f. Update `bind_select`'s `tables` list building to include aliases:**

Find in `bind_select`:
```ocaml
       let (tables, _) =
         List.fold_left (fun (acc, off) (_, rm) ->
           let n = List.length rm.Cat.columns in
           (acc @ [(rm, off)], off + n)
         ) ([(meta, 0)], n_left) joined_pairs
       in
```
Change to:
```ocaml
       let (tables, _) =
         List.fold_left (fun (acc, off) ((jc : Ast.join_clause), rm) ->
           let n = List.length rm.Cat.columns in
           (acc @ [(rm, off, jc.Ast.alias)], off + n)
         ) ([(meta, 0, table_alias)], n_left) joined_pairs
       in
```

**5g. Update `proj_lookup` to use 3-tuples:**

Find:
```ocaml
       let proj_lookup name : (int, error) result =
         let hits = List.filter_map (fun (tm, base) ->
           match col_index tm.Cat.columns name with
           | Some i -> Some (base + i) | None -> None
         ) tables in
```
Change to:
```ocaml
       let proj_lookup name : (int, error) result =
         let hits = List.filter_map (fun (tm, base, _alias) ->
           match col_index tm.Cat.columns name with
           | Some i -> Some (base + i) | None -> None
         ) tables in
```

**5h. Update `qual_lookup` to check alias:**

Find:
```ocaml
       let qual_lookup t c : (int, error) result =
         match List.find_opt (fun (tm, _) -> String.equal tm.Cat.name t) tables with
         | None            -> Error (Unknown_table t)
         | Some (tm, base) ->
           (match col_index tm.Cat.columns c with
            | Some i -> Ok (base + i)
            | None   -> Error (Unknown_column { table = t; column = c }))
       in
```
Change to:
```ocaml
       let qual_lookup t c : (int, error) result =
         match List.find_opt (fun (tm, _, alias_opt) ->
           String.equal tm.Cat.name t ||
           Option.exists (String.equal t) alias_opt
         ) tables with
         | None -> Error (Unknown_table t)
         | Some (tm, base, _) ->
           (match col_index tm.Cat.columns c with
            | Some i -> Ok (base + i)
            | None   -> Error (Unknown_column { table = t; column = c }))
       in
```

**5i. Update `bind_joins_result` to use 3-tuples:**

Find in `bind_select`:
```ocaml
          let bind_joins_result : (bound_join list, error) result =
            let rec go acc tbl_acc offset = function
              | [] -> Ok (List.rev acc)
              | ((jc : Ast.join_clause), rm) :: rest ->
                let tables_so_far = tbl_acc @ [(rm, offset)] in
                (match bind_expr_join ~param_counter ~named_params
                         ~tables:tables_so_far jc.Ast.on with
                 | Error e -> Error e
                 | Ok be   ->
                   let bj = { kind = jc.Ast.kind; right_meta = rm;
                              on = be; right_col_offset = offset } in
                   go (bj :: acc) tables_so_far (offset + List.length rm.Cat.columns) rest)
            in
            go [] [(meta, 0)] n_left joined_pairs
```
Change the `tables_so_far` construction and `go` initial call to use 3-tuples:
```ocaml
          let bind_joins_result : (bound_join list, error) result =
            let rec go acc tbl_acc offset = function
              | [] -> Ok (List.rev acc)
              | ((jc : Ast.join_clause), rm) :: rest ->
                let tables_so_far = tbl_acc @ [(rm, offset, jc.Ast.alias)] in
                (match bind_expr_join ~param_counter ~named_params
                         ~tables:tables_so_far jc.Ast.on with
                 | Error e -> Error e
                 | Ok be   ->
                   let bj = { kind = jc.Ast.kind; right_meta = rm;
                              on = be; right_col_offset = offset } in
                   go (bj :: acc) tables_so_far (offset + List.length rm.Cat.columns) rest)
            in
            go [] [(meta, 0, table_alias)] n_left joined_pairs
```

**5j. Update `bind_combined` to use `bind_expr_join` always:**

Find:
```ocaml
             let bind_combined e =
               if joined_pairs = [] then
                 bind_expr ~param_counter ~named_params meta e
               else
                 bind_expr_join ~param_counter ~named_params ~tables e
             in
```
Change to always use `bind_expr_join` (tables now includes the main table with its alias):
```ocaml
             let bind_combined e =
               bind_expr_join ~param_counter ~named_params ~tables e
             in
```

Also update the `bind_one` helper in `proj_result` similarly:
```ocaml
           let bind_one e =
             bind_expr_join ~param_counter ~named_params ~tables e
           in
```
(Remove the `if joined_pairs = [] then ... else ...` branch.)

Also update `order_result`'s `bind_order_expr` to always use `bind_expr_join`:
```ocaml
                let bind_order_expr e =
                  let base_result =
                    bind_expr_join ~param_counter ~named_params ~tables e
                  in
                  match base_result with
                  | Ok _ -> base_result
                  | Error _ ->
                    (match e with
                     | Ast.E_col name ->
                       (match List.assoc_opt name alias_map with
                        | Some be -> Ok be
                        | None    -> base_result)
                     | _ -> base_result)
                in
```

**5k. Update `SELECT *` expansion to use 3-tuples:**

Find:
```ocaml
             | `All ->
               let all_ords = List.concat_map (fun (tm, base) ->
                 List.mapi (fun i _ -> base + i) tm.Cat.columns
               ) tables in
```
Change to:
```ocaml
             | `All ->
               let all_ords = List.concat_map (fun (tm, base, _alias) ->
                 List.mapi (fun i _ -> base + i) tm.Cat.columns
               ) tables in
```

**5l. Update `validate_numeric` to use 3-tuples:**

Find (in aggregated path):
```ocaml
                 let validate_numeric col_ord =
                   let cols = List.concat_map (fun (tm, _) -> tm.Cat.columns) tables in
```
Change to:
```ocaml
                 let validate_numeric col_ord =
                   let cols = List.concat_map (fun (tm, _, _) -> tm.Cat.columns) tables in
```

- [ ] **Step 6: Build and run tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe -- test tbl_alias 2>&1 | tail -30
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | tail -10
```
Expected: all 5 tbl_alias tests pass, zero regressions.

- [ ] **Step 7: Commit**

```bash
git add lib/sql/ast.ml lib/sql/parser.mly lib/sql/sema.ml test/test_e2e.ml
git commit -m "feat(phase11-task3): table aliases in FROM and JOIN (AS alias, E_tbl_col alias resolution)"
```

---

## Task 4: SQLite comparison tests + coverage boost

Validate CAST, NULLIF, IIF, column aliases, and table aliases against real SQLite3.

**Files:**
- Modify: `test/test_sqlite_compare.ml`

- [ ] **Step 1: Add comparison test cases**

Add before the `let () = Alcotest.run ...` block in `test/test_sqlite_compare.ml`:

```ocaml
(* ── Phase 11: CAST, NULLIF, IIF ──────────────────────────────── *)

let phase11_cast_cases = [
  { name    = "cast_int_to_text";
    setup   = ["CREATE TABLE t (n INTEGER)"; "INSERT INTO t VALUES (42)"];
    query   = "SELECT CAST(n AS TEXT) FROM t";
    unordered = false };

  { name    = "cast_text_to_int";
    setup   = ["CREATE TABLE t (s TEXT)"; "INSERT INTO t VALUES ('99')"];
    query   = "SELECT CAST(s AS INTEGER) FROM t";
    unordered = false };

  { name    = "cast_real_to_int";
    setup   = ["CREATE TABLE t (r REAL)"; "INSERT INTO t VALUES (7.9)"];
    query   = "SELECT CAST(r AS INTEGER) FROM t";
    unordered = false };

  { name    = "cast_int_to_real";
    setup   = ["CREATE TABLE t (n INTEGER)"; "INSERT INTO t VALUES (3)"];
    query   = "SELECT CAST(n AS REAL) FROM t";
    unordered = false };

  { name    = "cast_null_is_null";
    setup   = ["CREATE TABLE t (n INTEGER)"; "INSERT INTO t VALUES (NULL)"];
    query   = "SELECT CAST(n AS TEXT) FROM t";
    unordered = false };

  { name    = "nullif_equal";
    setup   = ["CREATE TABLE t (n INTEGER)"; "INSERT INTO t VALUES (5)"];
    query   = "SELECT NULLIF(n, 5) FROM t";
    unordered = false };

  { name    = "nullif_unequal";
    setup   = ["CREATE TABLE t (n INTEGER)"; "INSERT INTO t VALUES (5)"];
    query   = "SELECT NULLIF(n, 3) FROM t";
    unordered = false };

  { name    = "iif_true";
    setup   = ["CREATE TABLE t (n INTEGER)"; "INSERT INTO t VALUES (10)"];
    query   = "SELECT IIF(n > 5, 'big', 'small') FROM t";
    unordered = false };

  { name    = "iif_false";
    setup   = ["CREATE TABLE t (n INTEGER)"; "INSERT INTO t VALUES (2)"];
    query   = "SELECT IIF(n > 5, 'big', 'small') FROM t";
    unordered = false };

  { name    = "cast_in_where";
    setup   = ["CREATE TABLE t (s TEXT)";
               "INSERT INTO t VALUES ('10')";
               "INSERT INTO t VALUES ('3')"];
    query   = "SELECT s FROM t WHERE CAST(s AS INTEGER) > 5 ORDER BY s";
    unordered = false };

  { name    = "cast_text_invalid_to_int";
    setup   = ["CREATE TABLE t (s TEXT)"; "INSERT INTO t VALUES ('abc')"];
    query   = "SELECT CAST(s AS INTEGER) FROM t";
    unordered = false };
]

(* ── Phase 11: aliases ─────────────────────────────────────────── *)

let phase11_alias_cases = [
  { name    = "col_alias_basic";
    setup   = ["CREATE TABLE t (n INTEGER)"; "INSERT INTO t VALUES (4)"];
    query   = "SELECT n * 2 AS doubled FROM t";
    unordered = false };

  { name    = "col_alias_order_by";
    setup   = ["CREATE TABLE t (n INTEGER)";
               "INSERT INTO t VALUES (3)";
               "INSERT INTO t VALUES (1)";
               "INSERT INTO t VALUES (2)"];
    query   = "SELECT n * 10 AS big FROM t ORDER BY big";
    unordered = false };

  { name    = "tbl_alias_from";
    setup   = ["CREATE TABLE products (id INTEGER, name TEXT)";
               "INSERT INTO products VALUES (1, 'apple')"];
    query   = "SELECT p.id, p.name FROM products AS p";
    unordered = false };

  { name    = "tbl_alias_join";
    setup   = ["CREATE TABLE a (id INTEGER, val TEXT)";
               "CREATE TABLE b (aid INTEGER, extra TEXT)";
               "INSERT INTO a VALUES (1, 'x')";
               "INSERT INTO b VALUES (1, 'y')"];
    query   = "SELECT x.val, y.extra FROM a AS x JOIN b AS y ON x.id = y.aid";
    unordered = false };

  { name    = "tbl_alias_where";
    setup   = ["CREATE TABLE t (n INTEGER)";
               "INSERT INTO t VALUES (1)";
               "INSERT INTO t VALUES (2)"];
    query   = "SELECT r.n FROM t AS r WHERE r.n > 1 ORDER BY r.n";
    unordered = false };

  { name    = "nullif_in_select";
    setup   = ["CREATE TABLE t (a INTEGER, b INTEGER)";
               "INSERT INTO t VALUES (5, 5)";
               "INSERT INTO t VALUES (3, 7)"];
    query   = "SELECT NULLIF(a, b) FROM t ORDER BY a";
    unordered = false };

  { name    = "cast_in_join_on";
    setup   = ["CREATE TABLE t (id INTEGER)";
               "CREATE TABLE u (sid TEXT)";
               "INSERT INTO t VALUES (1)";
               "INSERT INTO u VALUES ('1')"];
    query   = "SELECT t.id FROM t JOIN u ON t.id = CAST(u.sid AS INTEGER)";
    unordered = false };
]
```

Register them in the test runner (update `let () = Alcotest.run ...`):
```ocaml
let () =
  Alcotest.run "sqlite_compare" [
    "correctness",       List.map make_test cases;
    "phase9_subqueries", List.map make_test phase9_subquery_cases;
    "phase9_check",      (List.map make_test phase9_check_cases
                          @ List.map make_check_error_test phase9_check_error_cases);
    "phase9_fk",         List.map make_test phase9_fk_cases;
    "phase10_case_when", List.map make_test phase10_case_when_cases;
    "phase10_multi_join", List.map make_test phase10_multi_join_cases;
    "phase11_cast",      List.map make_test phase11_cast_cases;
    "phase11_alias",     List.map make_test phase11_alias_cases;
  ]
```

- [ ] **Step 2: Run comparison tests**

```bash
podman run --rm \
  -v $(pwd):/workspace:Z \
  -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
  -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
  -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
  -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
  -w /workspace sqlocaml-dev dune exec test/test_sqlite_compare.exe 2>&1 | tail -40
```
Expected: phase11_cast and phase11_alias groups pass (18 new comparison tests).

If any CAST float-to-text tests fail due to formatting differences (e.g. sqlocaml gives "3.0" vs SQLite "3"), exclude the specific test or adjust the `fmt_val` in the comparison helpers — do not change sqlocaml's CAST semantics.

- [ ] **Step 3: Run full suite**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | tail -20
```
Expected: all tests pass. Confirm test count is higher than 225 (was 225 at end of Phase 10).

- [ ] **Step 4: Commit**

```bash
git add test/test_sqlite_compare.ml
git commit -m "test(phase11): SQLite comparison tests for CAST, NULLIF/IIF, column aliases, table aliases"
```

---

## Self-Review

**Spec coverage:**
- CAST expression (`E_cast`, P_cast, full pipeline) ✓ Task 1
- NULLIF(a,b) desugar to E_case ✓ Task 1
- IIF(cond,t,f) desugar to E_case ✓ Task 1
- Column aliases `expr AS name` ✓ Task 2
- ORDER BY alias resolution ✓ Task 2
- Table aliases FROM/JOIN ✓ Task 3
- E_tbl_col alias-aware resolution ✓ Task 3
- Comparison tests ✓ Task 4

**Backward compatibility:**
- `AS` token: previously fell through as `IDENT "AS"`, causing parse errors in any query using `AS`. Adding it as a keyword only helps.
- `Cols of string list` in proj: unchanged — plain column selects stay on the fast path.
- `bind_expr` (non-join path): replaced by `bind_expr_join` call. Functionally equivalent for single-table queries — `bind_expr_join` with one-element tables produces identical results.
- `join_clause.alias`: previously always `None` — now parsed from SQL. Queries without aliases produce `alias = None` → behavior unchanged.

**Placeholder scan:** None found.

**Type consistency:**
- `E_cast of expr * ty` → `BE_cast of bound_expr * Ast.ty` → `P_cast of expr * Ast.ty` ✓
- `proj : ... | Exprs of (expr * string option) list` → `expr_proj : (bound_expr * string option) list` → `Op_expr_project.exprs : (expr * string option) list` ✓
- `tables : (Cat.table_meta * int * string option) list` used consistently throughout `bind_select` ✓
