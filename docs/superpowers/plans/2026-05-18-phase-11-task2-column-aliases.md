# Phase 11 Task 2: Column Aliases in SELECT Projection

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `SELECT expr AS alias` support so aliases are stored alongside expressions, enable `ORDER BY alias` resolution, and keep row output as plain `Row.value array`.

**Architecture:** The `AS` token already exists (added in Task 1). The change threads an `(expr * string option)` pair through every layer — AST, sema, plan, planner, exec — replacing the bare `expr list` in `Exprs` projection. ORDER BY resolution tries the alias map as a fallback when column binding fails.

**Tech Stack:** OCaml, Menhir parser, dune build, podman container (`sqlocaml-dev`).

All dune commands run inside podman:
```bash
cd /home/tej/projects/sqlite_ocaml_port
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune <args>
```

---

## File map

| File | Change |
|---|---|
| `lib/sql/ast.ml` | `S_select.proj` `Exprs` branch: `expr list` → `(expr * string option) list` |
| `lib/sql/parser.mly` | `proj_item` and `projection` rules updated to parse `AS alias` |
| `lib/sql/sema.ml` | `expr_proj` field type, `proj_has_agg`, ORDER BY alias fallback |
| `lib/sql/sema.mli` | `BS_select.expr_proj` type update |
| `lib/sql/plan.ml` | `Op_expr_project.exprs` type: `(expr * string option) list` |
| `lib/sql/planner.ml` | Map `(be, alias)` pairs when constructing `Op_expr_project` |
| `lib/sql/exec.ml` | Strip alias in `Op_expr_project` evaluation: `(e, _)` pattern |
| `test/test_e2e.ml` | 5 new `col_alias` tests + registration after `nullif_iif` |

---

### Task 1: Write failing tests

**Files:**
- Modify: `test/test_e2e.ml` (end of file)

- [ ] **Step 1: Find insertion point**

Open `test/test_e2e.ml`. The last test group is `"nullif_iif"` ending around line 3681. Add the new test functions BEFORE the closing `]` of the test list, and add the new test group AFTER the `nullif_iif` block.

- [ ] **Step 2: Add test functions**

In `test/test_e2e.ml`, locate the last test function before the `let () =` or `let tests =` block, then insert:

```ocaml
(* ------------------------------------------------------------------ *)
(* Phase 11: Column aliases                                             *)
(* ------------------------------------------------------------------ *)

let test_col_alias_basic () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (3)";
  let rows = query_ok db "SELECT n * 2 AS doubled FROM t" in
  Alcotest.(check int) "one row" 1 (List.length rows);
  Alcotest.(check row_testable) "expr with alias"
    [| Db.V_int 6L |] (List.nth rows 0)

let test_col_alias_order_by () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (3)";
  exec db "INSERT INTO t VALUES (1)";
  exec db "INSERT INTO t VALUES (2)";
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

- [ ] **Step 3: Register tests in the list**

Find the `"nullif_iif"` block (ends around line 3681) and add after it:

```ocaml
    "col_alias", [
      Alcotest.test_case "basic"     `Quick test_col_alias_basic;
      Alcotest.test_case "order_by"  `Quick test_col_alias_order_by;
      Alcotest.test_case "multiple"  `Quick test_col_alias_multiple;
      Alcotest.test_case "mixed"     `Quick test_col_alias_mixed;
      Alcotest.test_case "cast"      `Quick test_col_alias_cast;
    ];
```

- [ ] **Step 4: Verify build fails (tests exist but feature not implemented)**

```bash
cd /home/tej/projects/sqlite_ocaml_port
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1 | tail -5
```

Expected: build succeeds (tests compile with current AST shape — they will fail at runtime, not compile time, because `AS alias` will be a parse error).

---

### Task 2: Update AST

**Files:**
- Modify: `lib/sql/ast.ml` line 129

- [ ] **Step 1: Change `Exprs` branch type**

In `lib/sql/ast.ml`, change:
```ocaml
      proj     : [ `All | `Cols of string list | `Exprs of expr list ];
```
to:
```ocaml
      proj     : [ `All | `Cols of string list | `Exprs of (expr * string option) list ];
```

- [ ] **Step 2: Build to confirm change propagates**

```bash
cd /home/tej/projects/sqlite_ocaml_port
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1 | head -30
```

Expected: type errors in `parser.mly`, `sema.ml` — confirms the change needs to flow through.

---

### Task 3: Update parser

**Files:**
- Modify: `lib/sql/parser.mly` (the `proj_item` rule around line 363 and `projection` around line 294)

- [ ] **Step 1: Update `proj_item` rule**

Change:
```menhir
proj_item:
  | e = expr { match e with E_col name -> `Col name | _ -> `Expr e }
```
to:
```menhir
proj_item:
  | e = expr AS alias = IDENT { `ExprA (e, Some alias) }
  | e = expr { match e with E_col name -> `Col name | _ -> `ExprA (e, None) }
```

- [ ] **Step 2: Update `projection` rule**

Change:
```menhir
projection:
  | STAR                                             { `All }
  | items = separated_nonempty_list(COMMA, proj_item)
    { let all_cols = List.for_all (function `Col _ -> true | _ -> false) items in
      if all_cols then
        `Cols (List.map (function `Col c -> c | _ -> assert false) items)
      else
        `Exprs (List.map (function
          | `Col c -> E_col c
          | `Expr e -> e) items) }
```
to:
```menhir
projection:
  | STAR                                             { `All }
  | items = separated_nonempty_list(COMMA, proj_item)
    { let all_plain_cols = List.for_all (function `Col _ -> true | _ -> false) items in
      if all_plain_cols then
        `Cols (List.map (function `Col c -> c | _ -> assert false) items)
      else
        `Exprs (List.map (function
          | `Col c         -> (Ast.E_col c, None)
          | `ExprA (e, a)  -> (e, a)) items) }
```

- [ ] **Step 3: Build to confirm parser changes compile**

```bash
cd /home/tej/projects/sqlite_ocaml_port
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1 | head -30
```

Expected: type errors now point to `sema.ml` (the `Exprs es ->` branch that previously bound `es : expr list`).

---

### Task 4: Update sema.ml and sema.mli

**Files:**
- Modify: `lib/sql/sema.ml`
- Modify: `lib/sql/sema.mli`

- [ ] **Step 1: Update `sema.mli` — `expr_proj` field**

In `lib/sql/sema.mli`, change:
```ocaml
      expr_proj  : bound_expr list;
```
to:
```ocaml
      expr_proj  : (bound_expr * string option) list;
```

- [ ] **Step 2: Update `sema.ml` — `BS_select` record field**

In `lib/sql/sema.ml` around line 72, change:
```ocaml
  | BS_select of {
      ...
      expr_proj  : bound_expr list;
```
to:
```ocaml
  | BS_select of {
      ...
      expr_proj  : (bound_expr * string option) list;
```

- [ ] **Step 3: Update `proj_has_agg` check**

In `bind_select`, find:
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

- [ ] **Step 4: Update `Exprs` binding in non-aggregated path**

In `bind_select`, find the non-aggregated `Exprs` branch (around line 1152):
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
               (* Phase 5 / Phase 11: arbitrary expr projection with optional alias. *)
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

- [ ] **Step 5: Update `compound_col_count` in sema.ml**

Find (around line 1713):
```ocaml
  | BS_select { proj; expr_proj; aggs; _ } ->
    if aggs <> [] then List.length aggs
    else if expr_proj <> [] then List.length expr_proj
    else List.length proj
```
Change to:
```ocaml
  | BS_select { proj; expr_proj; aggs; _ } ->
    if aggs <> [] then List.length aggs
    else if expr_proj <> [] then List.length expr_proj
    else List.length proj
```
(No change needed — `List.length` works on `(bound_expr * string option) list` identically.)

- [ ] **Step 6: Add alias_map and alias-aware ORDER BY binding**

In `bind_select`, find the `order_result` computation (around line 1366):
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
                (* alias_map: name → bound_expr for ORDER BY alias resolution *)
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

Note: `proj_exprs` is the variable already in scope at this point — it comes from the unpacking of `proj_result`:
```ocaml
        | Ok (proj_ords, agg_proj_items, proj_aggs, proj_exprs) ->
```
The `alias_map` construction reads from `proj_exprs : (bound_expr * string option) list`.

- [ ] **Step 7: Build to see remaining errors**

```bash
cd /home/tej/projects/sqlite_ocaml_port
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1 | head -40
```

Expected: errors in `planner.ml` and `exec.ml` about `expr_proj` type mismatch.

---

### Task 5: Update plan.ml, planner.ml, exec.ml

**Files:**
- Modify: `lib/sql/plan.ml` line 58–61
- Modify: `lib/sql/planner.ml` (two occurrences of `Op_expr_project`)
- Modify: `lib/sql/exec.ml` line 1742–1748

- [ ] **Step 1: Update `Op_expr_project` in plan.ml**

Change:
```ocaml
  | Op_expr_project of {
      exprs : expr list;
      child : op;
    }
```
to:
```ocaml
  | Op_expr_project of {
      exprs : (expr * string option) list;
      child : op;
    }
```

- [ ] **Step 2: Update planner.ml — first occurrence (plan_select)**

In `plan_select` (around line 225), find:
```ocaml
    else if expr_proj <> [] then
      Plan.Op_expr_project {
        exprs = List.map plan_expr expr_proj;
        child = after_sort;
      }
```
Change to:
```ocaml
    else if expr_proj <> [] then
      Plan.Op_expr_project {
        exprs = List.map (fun (be, alias) -> (plan_expr be, alias)) expr_proj;
        child = after_sort;
      }
```

- [ ] **Step 3: Update planner.ml — second occurrence (no-catalog path)**

In the `None` catalog branch (around line 329), find:
```ocaml
         else if expr_proj <> [] then
           Plan.Op_expr_project {
             exprs = List.map plan_expr expr_proj;
             child = after_sort;
           }
```
Change to:
```ocaml
         else if expr_proj <> [] then
           Plan.Op_expr_project {
             exprs = List.map (fun (be, alias) -> (plan_expr be, alias)) expr_proj;
             child = after_sort;
           }
```

- [ ] **Step 4: Update exec.ml — Op_expr_project execution**

Find (around line 1742):
```ocaml
  | Plan.Op_expr_project { exprs; child } ->
    let* inner = to_stream clock params store ~mode ~cat child in
    let* exprs' = Lwt_list.map_s (pre_eval_subquery clock store params cat) exprs in
    let eval_exprs row =
      Array.of_list (List.map (eval_expr clock params row) exprs')
    in
    Lwt.return (Lwt_stream.map eval_exprs inner)
```
Change to:
```ocaml
  | Plan.Op_expr_project { exprs; child } ->
    let* inner = to_stream clock params store ~mode ~cat child in
    let* exprs' = Lwt_list.map_s
      (fun (e, _alias) -> pre_eval_subquery clock store params cat e)
      exprs
    in
    let eval_exprs row =
      Array.of_list (List.map (eval_expr clock params row) exprs')
    in
    Lwt.return (Lwt_stream.map eval_exprs inner)
```

- [ ] **Step 5: Build — expect clean**

```bash
cd /home/tej/projects/sqlite_ocaml_port
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
```

Expected: no errors.

---

### Task 6: Run tests and verify

- [ ] **Step 1: Run full test suite**

```bash
cd /home/tej/projects/sqlite_ocaml_port
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | tail -20
```

Expected: all 237 previous tests pass, plus 5 new `col_alias` tests pass (242 total).

- [ ] **Step 2: Verify self-review checklist**

- `SELECT n * 2 AS doubled FROM t` — parses, returns `[| V_int 6L |]`
- `SELECT n * 10 AS big FROM t ORDER BY big` — ORDER BY alias resolves, rows sorted
- `SELECT n, n*2 AS doubled FROM t` — mixed: plain col + aliased expr
- `SELECT a, b FROM t` still works (Cols path untouched)
- All 237 existing tests pass
- `expr_proj : (bound_expr * string option) list` in sema.mli

---

### Task 7: Commit

- [ ] **Step 1: Stage and commit**

```bash
cd /home/tej/projects/sqlite_ocaml_port
git add lib/sql/ast.ml lib/sql/parser.mly lib/sql/sema.ml lib/sql/sema.mli \
        lib/sql/plan.ml lib/sql/planner.ml lib/sql/exec.ml test/test_e2e.ml
git commit -m "feat(phase11-task2): column aliases in SELECT (AS alias), ORDER BY alias resolution"
```
