# Phase 9: Subqueries, CHECK Constraints, and FOREIGN KEY Parse

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add non-correlated subqueries (scalar, EXISTS, IN SELECT), CHECK constraint enforcement, and FOREIGN KEY parse-only support — closing issues #52 and #51 while adding high-value query expressiveness.

**Architecture:** Subqueries store the raw `Ast.stmt` in `Plan.expr` nodes (`P_subquery`, `P_exists`, `P_in_select`); a new `pre_eval_subquery` function in exec.ml binds + plans + executes them at runtime via `Sema.bind`/`Planner.plan`/`to_stream` — zero changes to sema or planner async shape. CHECK constraints persist as SQL text in `Row.column.check_sql`, serialized via `Ast.expr_to_sql`; a new `expr_only` parser entry point re-parses and a module-level cache avoids re-parsing on every INSERT/UPDATE. FOREIGN KEY is purely a parser-tolerance feature: `REFERENCES` syntax is parsed and discarded.

**Tech Stack:** OCaml 5.1, Menhir, Lwt, Alcotest + QCheck, bisect_ppx, dune inside `sqlocaml-dev` Podman image.

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
| `lib/sql/ast.ml` | Add `E_subquery/E_exists/E_in_select` to `expr`; add `check: expr option` and `fk_ref: (string * string option) option` to `column_def`; add `expr_to_sql` function |
| `lib/sql/lexer.mll` | Add tokens: `EXISTS`, `CHECK`, `REFERENCES`, `FOREIGN`, `KEY` (KEY already exists as `PRIMARY KEY` but not standalone) |
| `lib/sql/parser.mly` | Add `%token EXISTS CHECK REFERENCES FOREIGN`; add `expr_only` start rule; subquery grammar in `expr`; `CHECK (expr)` and `REFERENCES tbl(col)` in column definitions |
| `lib/sql/sema.ml` / `sema.mli` | Add `BE_subquery/BE_exists/BE_in_select of Ast.stmt` to `bound_expr`; handle in `bind_expr` (3 new cases, sync); handle as `Unsupported` in `bind_expr_join` and `bind_expr_agg`; in `bind_create`, serialize `col_def.check` to `check_sql` via `Ast.expr_to_sql` |
| `lib/sql/plan.ml` | Add `P_subquery/P_exists/P_in_select of Ast.stmt` to `expr` |
| `lib/sql/planner.ml` | Map `BE_subquery → P_subquery` etc. in `plan_expr` |
| `lib/sql/exec.ml` | Add `value_to_literal`; add `pre_eval_subquery` (mutual recursion with `to_stream`); call in `Op_filter`, `Op_expr_project`, `Op_sort`; add `ast_binop_to_plan`, `ast_expr_to_plan_check`, `check_cache`, `eval_check_constraints`; call check validation in `execute_insert` and `execute_update` |
| `lib/encoding/row.ml` | Add `check_sql: string option` to `column` type |
| `lib/catalog/catalog.ml` | Update `encode_column`/`decode_column` with backward-compatible `check_sql` field |
| `test/test_e2e.ml` | New groups: `"subqueries"` (6 tests), `"check_constraints"` (5 tests), `"fk_parse"` (2 tests) |
| `test/test_sqlite_compare.ml` | 24+ new comparison tests for all three features |
| `test/test_parser.ml` | Update patterns to include new `column_def` fields |
| `test/test_sema.ml` | Update patterns for new bound_expr/bound_stmt fields |
| `test/test_exec.ml` | Update Op_* patterns if needed |

---

## Task 1: Subqueries (scalar, EXISTS, IN SELECT)

Non-correlated subqueries only. The inner SELECT is bound + planned + executed at runtime inside `pre_eval_subquery`. Closes new feature (create Forgejo issue during task: "Subqueries: scalar, EXISTS, IN (SELECT)").

**Key design decisions:**
- `bind_expr` stays **synchronous** — new `BE_subquery of Ast.stmt` carries raw AST, not a bound stmt
- `pre_eval_subquery` in exec.ml binds and runs the inner query using `Sema.bind` + `Planner.plan ?cat` + `to_stream`
- Subqueries are evaluated **once per `Op_filter` execution** (before the row scan), not per row — this is correct for non-correlated; correlated subqueries would get wrong results (they're unsupported — users get "unknown column" from the inner query's sema)
- `NOT IN (SELECT ...)` = `E_not (E_in_select (x, inner))`
- Scalar subquery returning 0 rows → NULL; >1 rows → first row (SQLite behaviour)

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

- [ ] **Step 1: Write the failing E2E tests**

Add a new `subqueries` test group at the end of `test/test_e2e.ml` (before `]` of the final list). These test non-correlated scalar subqueries, EXISTS, and IN (SELECT):

```ocaml
(* ------------------------------------------------------------------ *)
(* Subqueries                                                           *)
(* ------------------------------------------------------------------ *)

let test_scalar_subquery () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 10)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (2, 20)" in
    let* rows = Db.query db "SELECT (SELECT max(v) FROM t)" in
    Alcotest.(check (list (array value_t)))
      "scalar subquery returns max"
      [[|V_int 20L|]] rows;
    Lwt.return_unit)

let test_scalar_subquery_null () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)" in
    let* rows = Db.query db "SELECT (SELECT max(v) FROM t)" in
    Alcotest.(check (list (array value_t)))
      "scalar subquery on empty table returns null"
      [[|V_null|]] rows;
    Lwt.return_unit)

let test_exists_subquery () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 10)" in
    let* _ = Db.execute db "CREATE TABLE r (id INTEGER PRIMARY KEY, ref_id INTEGER)" in
    let* _ = Db.execute db "INSERT INTO r VALUES (1, 1)" in
    let* _ = Db.execute db "INSERT INTO r VALUES (2, 99)" in
    (* Select rows from r where ref_id exists in t *)
    let* rows = Db.query db
      "SELECT id FROM r WHERE EXISTS (SELECT 1 FROM t WHERE t.id = 1)" in
    Alcotest.(check int) "exists matches two rows" 2 (List.length rows);
    Lwt.return_unit)

let test_exists_subquery_false () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER PRIMARY KEY)" in
    let* _ = Db.execute db "CREATE TABLE r (id INTEGER PRIMARY KEY)" in
    let* _ = Db.execute db "INSERT INTO r VALUES (1)" in
    let* rows = Db.query db
      "SELECT id FROM r WHERE EXISTS (SELECT 1 FROM t)" in
    Alcotest.(check int) "exists on empty inner table returns 0 rows" 0 (List.length rows);
    Lwt.return_unit)

let test_in_select_subquery () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 10)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (2, 20)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (3, 30)" in
    let* _ = Db.execute db "CREATE TABLE allowed (v INTEGER)" in
    let* _ = Db.execute db "INSERT INTO allowed VALUES (10)" in
    let* _ = Db.execute db "INSERT INTO allowed VALUES (30)" in
    let* rows = Db.query db "SELECT id FROM t WHERE v IN (SELECT v FROM allowed)" in
    let ids = List.map (fun r -> r.(0)) rows in
    Alcotest.(check (list value_t)) "in-select returns matching rows"
      [V_int 1L; V_int 3L] ids;
    Lwt.return_unit)

let test_not_in_select_subquery () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 10)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (2, 20)" in
    let* _ = Db.execute db "INSERT INTO t VALUES (3, 30)" in
    let* _ = Db.execute db "CREATE TABLE excluded (v INTEGER)" in
    let* _ = Db.execute db "INSERT INTO excluded VALUES (20)" in
    let* rows = Db.query db "SELECT id FROM t WHERE v NOT IN (SELECT v FROM excluded)" in
    let ids = List.map (fun r -> r.(0)) rows in
    Alcotest.(check (list value_t)) "not-in-select excludes row 2"
      [V_int 1L; V_int 3L] ids;
    Lwt.return_unit)
```

Then at the end of the test registration list add:
```ocaml
    "subqueries", [
      Alcotest.test_case "scalar_subquery"         `Quick test_scalar_subquery;
      Alcotest.test_case "scalar_subquery_null"    `Quick test_scalar_subquery_null;
      Alcotest.test_case "exists_true"             `Quick test_exists_subquery;
      Alcotest.test_case "exists_false"            `Quick test_exists_subquery_false;
      Alcotest.test_case "in_select"               `Quick test_in_select_subquery;
      Alcotest.test_case "not_in_select"           `Quick test_not_in_select_subquery;
    ];
```

- [ ] **Step 2: Run tests, confirm they fail with a parse error**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | grep -A5 "subqueries"
```

Expected: `Fatal error: exception` or `Parse error` since `EXISTS`, `E_subquery` etc. don't exist yet.

- [ ] **Step 3: Add new AST nodes to `lib/sql/ast.ml`**

Add three new constructors to the `expr` type (after `E_match`):

```ocaml
  | E_subquery  of stmt               (** scalar subquery: (SELECT ...) in expr position *)
  | E_exists    of stmt               (** EXISTS (SELECT ...) *)
  | E_in_select of expr * stmt        (** x IN (SELECT ...) *)
```

No change needed to `column_def` in this task (CHECK is Task 2).

- [ ] **Step 4: Add `EXISTS` token to `lib/sql/lexer.mll`**

In `lib/sql/lexer.mll`, add `EXISTS` to the keyword table. Find the section with `"select" -> SELECT` etc. and add:

```ocaml
      | "exists"     -> EXISTS
      | "EXISTS"     -> EXISTS
```

Note: the lexer has case-insensitive keywords handled by duplicate entries. Add both upper and lower.

- [ ] **Step 5: Add `EXISTS` token to `lib/sql/parser.mly` and add subquery grammar**

In `parser.mly`:

**a)** Add token declaration (at the end of `%token` declarations, before `%%`):
```
%token EXISTS
```

**b)** Add grammar rules in the `expr` rule. Add after the existing `IN` rules (around line 432-435):

```
  (* Subquery forms *)
  | EXISTS LPAREN s = compound_select RPAREN
    { E_exists s }
  | a = expr IN LPAREN s = compound_select RPAREN
    { E_in_select (a, s) }
  | a = expr NOT IN LPAREN s = compound_select RPAREN %prec IN
    { E_not (E_in_select (a, s)) }
  (* Scalar subquery: (SELECT ...) — must come AFTER the plain (expr) rule
     to avoid conflicts. Menhir resolves by lookahead: after LPAREN, if
     next token is SELECT → this rule; otherwise → paren-expr rule. *)
  | LPAREN s = compound_select RPAREN
    { E_subquery s }
```

**Important Menhir note:** The `| LPAREN s = compound_select RPAREN` rule and the existing `| LPAREN e = expr RPAREN { e }` rule will have a shift/reduce conflict at `LPAREN`. Menhir resolves it by the token after `LPAREN`: if `SELECT` → subquery branch; if anything else → expr branch. If Menhir gives an error/warning about this, use a `%prec` annotation or refactor using a `paren_or_subquery` non-terminal:

```
paren_or_subquery:
  | LPAREN s = compound_select RPAREN  { E_subquery s }
  | LPAREN e = expr RPAREN             { e }
```

And change the `expr` rule to use `paren_or_subquery`. Replace `| LPAREN e = expr RPAREN { e }` with `| p = paren_or_subquery { p }`.

- [ ] **Step 6: Build to check parser compiles**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
```

Expected: build succeeds (or shows sema/plan exhaustiveness warnings for missing new AST variants).

- [ ] **Step 7: Add `BE_subquery/BE_exists/BE_in_select` to `lib/sql/sema.ml` and `sema.mli`**

**In `sema.ml`**, add three constructors to `bound_expr` (after `BE_match`):
```ocaml
  | BE_subquery  of Ast.stmt   (** raw inner stmt, bound at exec time *)
  | BE_exists    of Ast.stmt
  | BE_in_select of bound_expr * Ast.stmt
```

**In `bind_expr`**, add three new cases (after the `Ast.E_match` case):
```ocaml
  | Ast.E_subquery inner ->
    Ok (BE_subquery inner)
  | Ast.E_exists inner ->
    Ok (BE_exists inner)
  | Ast.E_in_select (x, inner) ->
    (match bind_expr ~param_counter ~named_params meta x with
     | Error e -> Error e
     | Ok bx   -> Ok (BE_in_select (bx, inner)))
```

**In `bind_expr_join`** (find the match that mirrors `bind_expr`), add:
```ocaml
  | Ast.E_subquery _ | Ast.E_exists _ | Ast.E_in_select _ ->
    Error (Unsupported "subqueries are not supported in JOIN ON conditions")
```

**In `bind_expr_agg`** (find the match there), add:
```ocaml
  | Ast.E_subquery _ | Ast.E_exists _ | Ast.E_in_select _ ->
    Error (Unsupported "subqueries are not supported in aggregate expressions")
```

**In `sema.mli`**, the `bound_expr` type is re-declared; add the three new constructors there too:
```ocaml
  | BE_subquery  of Ast.stmt
  | BE_exists    of Ast.stmt
  | BE_in_select of bound_expr * Ast.stmt
```

- [ ] **Step 8: Add `P_subquery/P_exists/P_in_select` to `lib/sql/plan.ml`**

Add after `P_param`:
```ocaml
  | P_subquery  of Ast.stmt   (** scalar subquery: evaluated at runtime *)
  | P_exists    of Ast.stmt
  | P_in_select of expr * Ast.stmt
```

- [ ] **Step 9: Map new bound_exprs in `lib/sql/planner.ml`**

In `plan_expr`, add three cases (after the `BE_match` failwith):
```ocaml
  | Sema.BE_subquery inner             -> Plan.P_subquery inner
  | Sema.BE_exists inner               -> Plan.P_exists inner
  | Sema.BE_in_select (bx, inner)      -> Plan.P_in_select (plan_expr bx, inner)
```

- [ ] **Step 10: Add `pre_eval_subquery` and wire into `to_stream` in `lib/sql/exec.ml`**

**a)** Add `value_to_literal` helper near the top of exec.ml (after `lit_to_value`):
```ocaml
let value_to_literal : Row.value -> Ast.literal = function
  | Row.V_int n  -> Ast.L_int n
  | Row.V_text s -> Ast.L_text s
  | Row.V_real f -> Ast.L_real f
  | Row.V_blob b -> Ast.L_blob b
  | Row.V_null   -> Ast.L_null
```

**b)** The `pre_eval_subquery` function is mutually recursive with `to_stream`. Change the definition of `to_stream` from `let rec to_stream` to `let rec pre_eval_subquery ... and to_stream ...`.

Add `pre_eval_subquery` just before the existing `let rec to_stream`:

```ocaml
(* ------------------------------------------------------------------ *)
(* Subquery pre-evaluation                                              *)
(* Replaces P_subquery/P_exists/P_in_select nodes in an expression    *)
(* with concrete P_lit / P_in values by running the inner SELECT.     *)
(* Only non-correlated subqueries are supported (Phase 9).            *)
(* ------------------------------------------------------------------ *)

let rec pre_eval_subquery clock store params (cat_opt : Cat.t option) (e : Plan.expr) : Plan.expr Lwt.t =
  match e with
  | Plan.P_subquery inner_ast ->
    (match cat_opt with
     | None -> Lwt.return (Plan.P_lit Ast.L_null)
     | Some cat ->
       let* bound_r = Sema.bind cat inner_ast in
       (match bound_r with
        | Error _ -> Lwt.return (Plan.P_lit Ast.L_null)
        | Ok bound ->
          let op = Planner.plan ~cat bound in
          let* stream = to_stream clock params store ~mode:Auto ~cat:(Some cat) op in
          let* rows = Lwt_stream.to_list stream in
          let v = match rows with
            | []      -> Ast.L_null
            | row :: _ when Array.length row >= 1 -> value_to_literal row.(0)
            | _       -> Ast.L_null
          in
          Lwt.return (Plan.P_lit v)))
  | Plan.P_exists inner_ast ->
    (match cat_opt with
     | None -> Lwt.return (Plan.P_lit (Ast.L_int 0L))
     | Some cat ->
       let* bound_r = Sema.bind cat inner_ast in
       (match bound_r with
        | Error _ -> Lwt.return (Plan.P_lit (Ast.L_int 0L))
        | Ok bound ->
          let op = Planner.plan ~cat bound in
          let* stream = to_stream clock params store ~mode:Auto ~cat:(Some cat) op in
          let* first = Lwt_stream.get stream in
          Lwt.return (Plan.P_lit (Ast.L_int (if first = None then 0L else 1L)))))
  | Plan.P_in_select (x, inner_ast) ->
    (match cat_opt with
     | None -> Lwt.return (Plan.P_in (x, []))
     | Some cat ->
       let* bound_r = Sema.bind cat inner_ast in
       (match bound_r with
        | Error _ -> Lwt.return (Plan.P_in (x, []))
        | Ok bound ->
          let op = Planner.plan ~cat bound in
          let* stream = to_stream clock params store ~mode:Auto ~cat:(Some cat) op in
          let* rows = Lwt_stream.to_list stream in
          let vals = List.filter_map (fun row ->
            if Array.length row >= 1 then Some (Plan.P_lit (value_to_literal row.(0)))
            else None) rows in
          let* x' = pre_eval_subquery clock store params cat_opt x in
          Lwt.return (Plan.P_in (x', vals))))
  | Plan.P_binop (op, a, b) ->
    let* a' = pre_eval_subquery clock store params cat_opt a in
    let* b' = pre_eval_subquery clock store params cat_opt b in
    Lwt.return (Plan.P_binop (op, a', b'))
  | Plan.P_not a ->
    let* a' = pre_eval_subquery clock store params cat_opt a in
    Lwt.return (Plan.P_not a')
  | Plan.P_is_null a ->
    let* a' = pre_eval_subquery clock store params cat_opt a in
    Lwt.return (Plan.P_is_null a')
  | Plan.P_is_not_null a ->
    let* a' = pre_eval_subquery clock store params cat_opt a in
    Lwt.return (Plan.P_is_not_null a')
  | Plan.P_neg a ->
    let* a' = pre_eval_subquery clock store params cat_opt a in
    Lwt.return (Plan.P_neg a')
  | Plan.P_bitnot a ->
    let* a' = pre_eval_subquery clock store params cat_opt a in
    Lwt.return (Plan.P_bitnot a')
  | Plan.P_between (x, lo, hi) ->
    let* x'  = pre_eval_subquery clock store params cat_opt x in
    let* lo' = pre_eval_subquery clock store params cat_opt lo in
    let* hi' = pre_eval_subquery clock store params cat_opt hi in
    Lwt.return (Plan.P_between (x', lo', hi'))
  | Plan.P_in (x, vals) ->
    let* x'    = pre_eval_subquery clock store params cat_opt x in
    let* vals' = Lwt_list.map_s (pre_eval_subquery clock store params cat_opt) vals in
    Lwt.return (Plan.P_in (x', vals'))
  | Plan.P_func (f, args) ->
    let* args' = Lwt_list.map_s (pre_eval_subquery clock store params cat_opt) args in
    Lwt.return (Plan.P_func (f, args'))
  | _ -> Lwt.return e  (* P_lit, P_col, P_param: no subqueries *)

and to_stream ... (* existing function body unchanged *)
```

**c)** In `to_stream`, wire `pre_eval_subquery` into `Op_filter`, `Op_expr_project`, and `Op_sort`:

**Op_filter** (find the existing implementation):
```ocaml
  | Plan.Op_filter { pred; child } ->
    let* inner = to_stream clock params store ~mode ~cat child in
    (* Pre-evaluate non-correlated subquery nodes once (Phase 9) *)
    let* pred' = pre_eval_subquery clock store params cat pred in
    Lwt.return (Lwt_stream.filter (fun row -> value_truthy (eval_expr clock params row pred')) inner)
```

**Op_expr_project** (find the existing implementation):
```ocaml
  | Plan.Op_expr_project { exprs; child } ->
    let* inner = to_stream clock params store ~mode ~cat child in
    let* exprs' = Lwt_list.map_s (pre_eval_subquery clock store params cat) exprs in
    let eval_exprs row =
      Array.of_list (List.map (eval_expr clock params row) exprs')
    in
    Lwt.return (Lwt_stream.map eval_exprs inner)
```

**Op_sort** (find the existing implementation):
```ocaml
  | Plan.Op_sort { keys; child } ->
    let* inner = to_stream clock params store ~mode ~cat child in
    let* rows = Lwt_stream.to_list inner in
    let* keys' = Lwt_list.map_s (fun (e, dir) ->
        let* e' = pre_eval_subquery clock store params cat e in
        Lwt.return (e', dir)) keys in
    let cmp a b =
      List.fold_left (fun acc (key, dir) ->
        if acc <> 0 then acc
        else
          let va = eval_expr clock params a key
          and vb = eval_expr clock params b key in
          let c = compare_values va vb in
          if dir = `Asc then c else -c
      ) 0 keys'
    in
    let sorted = List.sort cmp rows in
    Lwt.return (Lwt_stream.of_list sorted)
```

**Important:** `pre_eval_subquery` must be defined as `let rec pre_eval_subquery ... and to_stream ...` so they are mutually recursive. Change the existing `let rec to_stream` to `and to_stream` and add `let rec pre_eval_subquery` before it.

**d)** Add `Sema` and `Planner` module references at the top of exec.ml:
```ocaml
module Sema    = Sqlocaml_sql.Sema    (* wrong — same library, use directly *)
```

Actually exec.ml is in the same library as sema.ml and planner.ml. In OCaml, modules in the same library are referenced directly without qualification: `Sema.bind` and `Planner.plan`. No import needed. Just make sure the dune build can resolve them (compilation order: sema, planner, then exec).

Check the dune file for `lib/sql/` — it does NOT specify `(modules ...)`, so all modules are included and OCaml determines order from usage. With `exec.ml` referencing `Sema` and `Planner`, dune will compile sema.ml and planner.ml before exec.ml automatically.

- [ ] **Step 11: Build and run tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | tail -20
```

Expected: all 6 new subquery tests pass. Total test count ≥ 329.

- [ ] **Step 12: Commit**

```bash
git add lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly \
        lib/sql/sema.ml lib/sql/sema.mli lib/sql/plan.ml lib/sql/planner.ml \
        lib/sql/exec.ml test/test_e2e.ml
git commit -m "feat(phase9-task1): non-correlated subqueries (scalar, EXISTS, IN SELECT)"
```

---

## Task 2: CHECK Constraints (#52)

Persistent CHECK constraint enforcement on INSERT and UPDATE. The check SQL is stored as text in `Row.column.check_sql`, serialized at CREATE TABLE time via `Ast.expr_to_sql`, and parsed+bound at first execution using a module-level cache in exec.ml. A new `expr_only` parser entry point enables re-parsing SQL text into `Ast.expr`.

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/sema.ml`
- Modify: `lib/encoding/row.ml`
- Modify: `lib/catalog/catalog.ml`
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_e2e.ml`

- [ ] **Step 1: Write failing CHECK tests**

Add to `test/test_e2e.ml`:

```ocaml
(* ------------------------------------------------------------------ *)
(* CHECK constraints                                                     *)
(* ------------------------------------------------------------------ *)

let test_check_insert_ok () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db
      "CREATE TABLE prices (id INTEGER PRIMARY KEY, amount REAL CHECK (amount > 0))" in
    let* n = Db.execute db "INSERT INTO prices VALUES (1, 9.99)" in
    Alcotest.(check int) "valid row inserted" 1 n;
    Lwt.return_unit)

let test_check_insert_violation () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db
      "CREATE TABLE prices (id INTEGER PRIMARY KEY, amount REAL CHECK (amount > 0))" in
    Lwt.catch
      (fun () ->
        let* _ = Db.execute db "INSERT INTO prices VALUES (1, -5.0)" in
        Alcotest.fail "expected CHECK violation")
      (fun _exn -> Lwt.return_unit))

let test_check_update_violation () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db
      "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER CHECK (v >= 0))" in
    let* _ = Db.execute db "INSERT INTO t VALUES (1, 5)" in
    Lwt.catch
      (fun () ->
        let* _ = Db.execute db "UPDATE t SET v = -1 WHERE id = 1" in
        Alcotest.fail "expected CHECK violation on update")
      (fun _exn -> Lwt.return_unit))

let test_check_null_allowed () =
  Lwt_main.run (
    (* SQLite CHECK: NULL in CHECK expr → passes (SQLite allows NULL values through CHECK) *)
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db
      "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER CHECK (v > 0))" in
    let* n = Db.execute db "INSERT INTO t VALUES (1, NULL)" in
    Alcotest.(check int) "null passes CHECK" 1 n;
    Lwt.return_unit)

let test_check_persisted () =
  Lwt_main.run (
    let tmpfile = Filename.temp_file "sqlocaml_check_" ".db" in
    Fun.protect ~finally:(fun () -> try Unix.unlink tmpfile with _ -> ()) (fun () ->
      let* db = Db.open_file tmpfile in
      let* _ = Db.execute db
        "CREATE TABLE t (id INTEGER PRIMARY KEY, v INTEGER CHECK (v > 0))" in
      let* _ = Db.execute db "INSERT INTO t VALUES (1, 10)" in
      let* () = Db.close db in
      (* Reopen and verify CHECK is still enforced *)
      let* db2 = Db.open_file tmpfile in
      Lwt.catch
        (fun () ->
          let* _ = Db.execute db2 "INSERT INTO t VALUES (2, -1)" in
          Alcotest.fail "expected CHECK to persist after reopen")
        (fun _exn ->
          let* () = Db.close db2 in
          Lwt.return_unit)))
```

Register the group:
```ocaml
    "check_constraints", [
      Alcotest.test_case "insert_ok"           `Quick test_check_insert_ok;
      Alcotest.test_case "insert_violation"    `Quick test_check_insert_violation;
      Alcotest.test_case "update_violation"    `Quick test_check_update_violation;
      Alcotest.test_case "null_allowed"        `Quick test_check_null_allowed;
      Alcotest.test_case "persisted"           `Quick test_check_persisted;
    ];
```

- [ ] **Step 2: Add `check` to `Ast.column_def` and `expr_to_sql` to `lib/sql/ast.ml`**

**a)** Extend `column_def`:
```ocaml
type column_def = {
  name        : string;
  ty          : ty;
  not_null    : bool;
  primary_key : bool;
  default     : literal option;
  check       : expr option;   (* None = no CHECK constraint *)
}
```

**b)** Add `expr_to_sql` and helpers at the end of `ast.ml`:
```ocaml
let binop_to_sql = function
  | Eq -> "=" | Ne -> "!=" | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "%"
  | And -> "AND" | Or -> "OR" | Concat -> "||"
  | Bit_and -> "&" | Bit_or -> "|" | Lshift -> "<<" | Rshift -> ">>"
  | Like -> "LIKE" | Glob -> "GLOB"

let func_to_sql = function
  | Fn_length -> "LENGTH" | Fn_lower -> "LOWER" | Fn_upper -> "UPPER"
  | Fn_abs -> "ABS" | Fn_coalesce -> "COALESCE" | Fn_ifnull -> "IFNULL"
  | Fn_substr -> "SUBSTR" | Fn_trim -> "TRIM" | Fn_ltrim -> "LTRIM"
  | Fn_rtrim -> "RTRIM" | Fn_replace -> "REPLACE" | Fn_instr -> "INSTR"
  | Fn_round -> "ROUND" | Fn_typeof -> "TYPEOF"
  | Fn_date -> "DATE" | Fn_time -> "TIME" | Fn_datetime -> "DATETIME"
  | Fn_strftime -> "STRFTIME" | Fn_julianday -> "JULIANDAY"
  | Fn_unixepoch -> "UNIXEPOCH"

let rec expr_to_sql = function
  | E_lit (L_int n)  -> Int64.to_string n
  | E_lit (L_text s) ->
    Printf.sprintf "'%s'" (String.concat "''" (String.split_on_char '\'' s))
  | E_lit L_null     -> "NULL"
  | E_lit (L_real f) -> Printf.sprintf "%g" f
  | E_lit (L_blob _) -> "X''"
  | E_col name       -> name
  | E_tbl_col (t, c) -> Printf.sprintf "%s.%s" t c
  | E_param Param_anon -> "?"
  | E_param (Param_index i) -> Printf.sprintf "?%d" i
  | E_param (Param_name n)  -> Printf.sprintf ":%s" n
  | E_binop (op, a, b) ->
    Printf.sprintf "(%s %s %s)" (expr_to_sql a) (binop_to_sql op) (expr_to_sql b)
  | E_not e          -> Printf.sprintf "NOT (%s)" (expr_to_sql e)
  | E_is_null e      -> Printf.sprintf "(%s) IS NULL" (expr_to_sql e)
  | E_is_not_null e  -> Printf.sprintf "(%s) IS NOT NULL" (expr_to_sql e)
  | E_neg e          -> Printf.sprintf "(-(%s))" (expr_to_sql e)
  | E_bitnot e       -> Printf.sprintf "(~(%s))" (expr_to_sql e)
  | E_between (x, lo, hi) ->
    Printf.sprintf "(%s) BETWEEN (%s) AND (%s)"
      (expr_to_sql x) (expr_to_sql lo) (expr_to_sql hi)
  | E_in (x, vals) ->
    Printf.sprintf "(%s) IN (%s)" (expr_to_sql x)
      (String.concat ", " (List.map expr_to_sql vals))
  | E_func (f, args) ->
    Printf.sprintf "%s(%s)" (func_to_sql f)
      (String.concat ", " (List.map expr_to_sql args))
  | E_agg _ | E_match _ | E_subquery _ | E_exists _ | E_in_select _ ->
    failwith "expr_to_sql: unsupported expression form"
```

- [ ] **Step 3: Add `CHECK` token to `lib/sql/lexer.mll`**

Add to the keyword table:
```ocaml
      | "check"      -> CHECK
      | "CHECK"      -> CHECK
```

- [ ] **Step 4: Update `lib/sql/parser.mly` with CHECK grammar and `expr_only` entry**

**a)** Add token declarations:
```
%token CHECK
%start <Ast.expr> expr_only
```

**b)** Add `expr_only` rule (just before `%%` or after `stmt_eof`):
```
expr_only:
  | e = expr EOF { e }
```

**c)** Update the `col_constraint` type and rules. Find the `col_constraint` type in the header and add:
```ocaml
  type col_constraint =
    | Col_not_null
    | Col_primary_key
    | Col_default of literal
    | Col_check   of expr          (* NEW *)
```

**d)** Find the `col_constraints` rule in the parser (handles `NOT NULL`, `PRIMARY KEY`, `DEFAULT`) and add:
```
  | CHECK LPAREN e = expr RPAREN  { Col_check e }
```

**e)** Update the column_def construction from col_constraints. Find where the `column_def` record is built from the constraint list and add handling for `Col_check`:

```ocaml
(* In the create_table column rule, build column_def from constraints.
   The current code sets not_null, primary_key, default. Add check: *)
let check = List.fold_left (fun acc c ->
  match c with Col_check e -> Some e | _ -> acc) None constraints in
{ name = n; ty = t; not_null; primary_key; default; check }
```

Look for the existing column construction:
```ocaml
      let not_null    = List.mem Col_not_null cs in
      let primary_key = List.mem Col_primary_key cs in
      let default     = List.fold_left (fun acc c ->
          match c with Col_default l -> Some l | _ -> acc) None cs in
      TI_col { name = n; ty; not_null; primary_key; default }
```

Change to:
```ocaml
      let not_null    = List.mem Col_not_null cs in
      let primary_key = List.mem Col_primary_key cs in
      let default     = List.fold_left (fun acc c ->
          match c with Col_default l -> Some l | _ -> acc) None cs in
      let check       = List.fold_left (fun acc c ->
          match c with Col_check e -> Some e | _ -> acc) None cs in
      TI_col { name = n; ty; not_null; primary_key; default; check }
```

- [ ] **Step 5: Fix all existing `column_def` construction sites in parser.mly**

Find every place in parser.mly that constructs a `column_def` record (grep for `{ name =`) and add `check = None` to each one that doesn't already have it. There may be alternate rules (for `ALTER TABLE ADD COLUMN` etc.) that need updating too.

- [ ] **Step 6: Update `lib/encoding/row.ml` — add `check_sql` to `column` type**

Change:
```ocaml
type column = {
  name        : string;
  ty          : ty;
  not_null    : bool;
  primary_key : bool;
  default     : default_value option;
}
```

To:
```ocaml
type column = {
  name        : string;
  ty          : ty;
  not_null    : bool;
  primary_key : bool;
  default     : default_value option;
  check_sql   : string option;  (* None = no CHECK; SQL text of the CHECK expr *)
}
```

- [ ] **Step 7: Update all `Row.{ ... }` construction sites in the codebase**

After adding `check_sql` to `Row.column`, every place that constructs a `Row.column` record (in sema.ml, catalog.ml, test files) needs `check_sql = None` (or the appropriate value).

Find all sites:
```bash
grep -rn "Row\.\s*{" /home/tej/projects/sqlite_ocaml_port/lib/ /home/tej/projects/sqlite_ocaml_port/test/ | grep -v "_build" | head -40
```

Update each one that doesn't include `check_sql`.

The most important site is in `sema.ml`'s `bind_create` (where columns are built):
```ocaml
    let row_cols = List.map (fun (c : Ast.column_def) ->
      Row.{ name        = c.name;
            ty          = (match c.ty with ...);
            not_null    = c.not_null;
            primary_key = c.primary_key;
            default     = Option.map ast_lit_to_dv c.default;
            check_sql   = Option.map Ast.expr_to_sql c.check }   (* NEW *)
    ) columns in
```

- [ ] **Step 8: Update `lib/catalog/catalog.ml` — `encode_column`/`decode_column` for `check_sql`**

**In `encode_column`**, add the new field at the end (backward compatible — new field appended):
```ocaml
let encode_column (col : Row.column) =
  let buf = Buffer.create 16 in
  Varint.encode_uint64 buf (Int64.of_int (type_tag col.ty));
  Varint.encode_uint64 buf (Int64.of_int (String.length col.name));
  Buffer.add_string buf col.name;
  Varint.encode_uint64 buf (if col.not_null    then 1L else 0L);
  Varint.encode_uint64 buf (if col.primary_key then 1L else 0L);
  (match col.default with
   | None    -> Varint.encode_uint64 buf 0L
   | Some dv ->
     Varint.encode_uint64 buf 1L;
     encode_default_value buf dv);
  (* Phase 9: check_sql field *)
  (match col.check_sql with
   | None     -> Varint.encode_uint64 buf 0L
   | Some sql ->
     Varint.encode_uint64 buf 1L;
     Varint.encode_uint64 buf (Int64.of_int (String.length sql));
     Buffer.add_string buf sql);
  Buffer.to_bytes buf
```

**In `decode_column`**, add backward-compatible decoding at the end:
```ocaml
(* After decoding default (existing code), add: *)
    (* Phase 9: check_sql (optional, absent in older databases → None) *)
    let bytes_left2 = Bytes.length bytes - off in
    let check_sql =
      if bytes_left2 <= 0 then None
      else
        let has_check, off = Varint.decode_uint64 bytes off in
        if Int64.to_int has_check = 0 then None
        else
          let sql_len, off = Varint.decode_uint64 bytes off in
          let sql = Bytes.sub_string bytes off (Int64.to_int sql_len) in
          ignore off; (* last field, no further reads *)
          Some sql
    in
    Row.{ name; ty = type_of_tag (Int64.to_int tag);
          not_null    = (Int64.to_int nn <> 0);
          primary_key = (Int64.to_int pk <> 0);
          default;
          check_sql }
```

Note: the variable `off` in `decode_column` is not a mutable ref; the pattern `let x, off = ...` rebinds `off`. Make sure to thread `off` correctly through the new code.

Also update every place in catalog.ml that constructs a `Row.column` record inline (e.g., `rename_column` helper that reads and re-encodes) to include `check_sql`.

- [ ] **Step 9: Add CHECK evaluation to `lib/sql/exec.ml`**

**a)** Add `ast_binop_to_plan` mapping after the existing `compare_values` function:
```ocaml
let ast_binop_to_plan : Ast.binop -> Plan.binop = function
  | Ast.Eq -> Plan.Eq | Ast.Ne -> Plan.Ne | Ast.Lt -> Plan.Lt | Ast.Le -> Plan.Le
  | Ast.Gt -> Plan.Gt | Ast.Ge -> Plan.Ge
  | Ast.Add -> Plan.Add | Ast.Sub -> Plan.Sub
  | Ast.Mul -> Plan.Mul | Ast.Div -> Plan.Div
  | Ast.And -> Plan.And | Ast.Or  -> Plan.Or
  | Ast.Concat  -> Plan.Concat | Ast.Mod -> Plan.Mod
  | Ast.Bit_and -> Plan.Bit_and | Ast.Bit_or -> Plan.Bit_or
  | Ast.Lshift  -> Plan.Lshift  | Ast.Rshift -> Plan.Rshift
  | Ast.Like -> Plan.Like | Ast.Glob -> Plan.Glob
```

**b)** Add `ast_expr_to_plan_check` (simplified binder for CHECK exprs — no sema needed):
```ocaml
let rec ast_expr_to_plan_check (columns : Row.column list) (e : Ast.expr) : Plan.expr =
  match e with
  | Ast.E_lit l       -> Plan.P_lit l
  | Ast.E_col name    -> Plan.P_col (find_col_idx_by_name columns name)
  | Ast.E_tbl_col (_, name) -> Plan.P_col (find_col_idx_by_name columns name)
  | Ast.E_binop (op, a, b) ->
    Plan.P_binop (ast_binop_to_plan op,
                  ast_expr_to_plan_check columns a,
                  ast_expr_to_plan_check columns b)
  | Ast.E_not e      -> Plan.P_not (ast_expr_to_plan_check columns e)
  | Ast.E_is_null e  -> Plan.P_is_null (ast_expr_to_plan_check columns e)
  | Ast.E_is_not_null e -> Plan.P_is_not_null (ast_expr_to_plan_check columns e)
  | Ast.E_neg e      -> Plan.P_neg (ast_expr_to_plan_check columns e)
  | Ast.E_bitnot e   -> Plan.P_bitnot (ast_expr_to_plan_check columns e)
  | Ast.E_between (x, lo, hi) ->
    Plan.P_between (ast_expr_to_plan_check columns x,
                    ast_expr_to_plan_check columns lo,
                    ast_expr_to_plan_check columns hi)
  | Ast.E_in (x, vals) ->
    Plan.P_in (ast_expr_to_plan_check columns x,
               List.map (ast_expr_to_plan_check columns) vals)
  | Ast.E_func (f, args) ->
    Plan.P_func (f, List.map (ast_expr_to_plan_check columns) args)
  | _ -> failwith "ast_expr_to_plan_check: unsupported expression in CHECK"
```

**c)** Add the check expression cache (module-level):
```ocaml
let check_expr_cache : (string * int, Plan.expr) Hashtbl.t = Hashtbl.create 16
```

**d)** Add `compile_check_expr` that parses + binds + caches:
```ocaml
let compile_check_expr (table_name : string) (col_idx : int)
    (columns : Row.column list) (check_sql : string) : Plan.expr =
  let key = (table_name, col_idx) in
  match Hashtbl.find_opt check_expr_cache key with
  | Some e -> e
  | None ->
    let lexbuf = Lexing.from_string check_sql in
    let ast_expr = Parser.expr_only Lexer.read lexbuf in
    let plan_expr = ast_expr_to_plan_check columns ast_expr in
    Hashtbl.add check_expr_cache key plan_expr;
    plan_expr
```

**e)** Add `eval_check_constraints`:
```ocaml
let eval_check_constraints
    (clock : (unit -> float) option)
    (params : Row.value array)
    (table_meta : Cat.table_meta)
    (row : Row.t) : unit =
  List.iteri (fun i (col : Row.column) ->
    match col.check_sql with
    | None -> ()
    | Some check_sql ->
      let check_plan = compile_check_expr table_meta.name i table_meta.columns check_sql in
      let result = eval_expr clock params row check_plan in
      (* SQLite behaviour: NULL in CHECK → passes (not a violation) *)
      if result <> Row.V_null && not (value_truthy result) then
        failwith (Printf.sprintf "CHECK constraint failed: %s.%s" table_meta.name col.name)
  ) table_meta.columns
```

**f)** In `execute_insert`, call `eval_check_constraints` after building the row and BEFORE writing it (right after any `not_null` check that may exist, before the UNIQUE check):

Find in `execute_insert` the section after the row is built (around line 790, after the `List.iter2 ... r.(ord) <- eval_expr ...`):
```ocaml
  (* Validate CHECK constraints *)
  eval_check_constraints clock params table_meta row;
```

**g)** In `execute_update`, call `eval_check_constraints` on each `new_row` after computing it. Find the section in the update loop where `new_row` is computed (around line 1023), after `List.iter (fun (i, expr) -> new_row.(i) <- eval_expr ...) assignments;`:
```ocaml
    (* Validate CHECK constraints on new row *)
    eval_check_constraints clock params table_meta new_row;
```

- [ ] **Step 10: Build and run tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | tail -20
```

Expected: all 5 new check constraint tests pass. Total ≥ 334 tests.

- [ ] **Step 11: Commit**

```bash
git add lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly \
        lib/sql/sema.ml lib/sql/exec.ml \
        lib/encoding/row.ml lib/catalog/catalog.ml \
        test/test_e2e.ml
git commit -m "feat(phase9-task2): CHECK constraint enforcement on INSERT/UPDATE, persistent via catalog (#52)"
```

---

## Task 3: FOREIGN KEY Parse-Only (#51)

Parse `REFERENCES table(col)` and `REFERENCES table` syntax in column definitions. Discard it — no storage in catalog, no enforcement. This unblocks schema definitions that use FK syntax without causing parse errors.

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`
- Modify: `test/test_e2e.ml`

- [ ] **Step 1: Write failing FK parse tests**

Add to `test/test_e2e.ml`:

```ocaml
(* ------------------------------------------------------------------ *)
(* FOREIGN KEY parse-only                                               *)
(* ------------------------------------------------------------------ *)

let test_fk_parse_create () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)" in
    (* FK syntax must be accepted without error *)
    let* _ = Db.execute db
      "CREATE TABLE orders (id INTEGER PRIMARY KEY, user_id INTEGER REFERENCES users(id))" in
    let* n = Db.execute db "INSERT INTO orders VALUES (1, 1)" in
    Alcotest.(check int) "insert into FK table works (no enforcement)" 1 n;
    Lwt.return_unit)

let test_fk_parse_no_col () =
  Lwt_main.run (
    let* db = Db.open_in_memory () in
    let* _ = Db.execute db "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT)" in
    (* REFERENCES without explicit column is also valid syntax *)
    let* _ = Db.execute db
      "CREATE TABLE orders (id INTEGER PRIMARY KEY, user_id INTEGER REFERENCES users)" in
    let* n = Db.execute db "INSERT INTO orders VALUES (1, 999)" in
    (* No FK enforcement — 999 doesn't exist in users but insert succeeds *)
    Alcotest.(check int) "insert without FK enforcement" 1 n;
    Lwt.return_unit)
```

Register:
```ocaml
    "fk_parse", [
      Alcotest.test_case "fk_references_col"     `Quick test_fk_parse_create;
      Alcotest.test_case "fk_references_no_col"  `Quick test_fk_parse_no_col;
    ];
```

- [ ] **Step 2: Add `REFERENCES` and `FOREIGN` tokens to `lib/sql/lexer.mll`**

```ocaml
      | "references"  -> REFERENCES
      | "REFERENCES"  -> REFERENCES
      | "foreign"     -> FOREIGN
      | "FOREIGN"     -> FOREIGN
```

- [ ] **Step 3: Add tokens and grammar to `lib/sql/parser.mly`**

**a)** Token declarations:
```
%token REFERENCES FOREIGN
```

**b)** Add an optional `opt_fk_ref` rule:
```
opt_fk_ref:
  |                                       { () }
  | REFERENCES t = IDENT                 { ignore t }
  | REFERENCES t = IDENT LPAREN c = IDENT RPAREN  { ignore t; ignore c }
```

**c)** In the `col_constraint` rule (same place as `NOT NULL`, `PRIMARY KEY`, etc.), add after the existing constraints:
```
  | REFERENCES t = IDENT               { ignore t; (* FK parse-only *) }
  | REFERENCES t = IDENT LPAREN c = IDENT RPAREN { ignore t; ignore c }
```

Wait — `col_constraint` produces a `col_constraint` value. Since FK is parse-only (no value), we need a way to express "nothing". The cleanest way: add a `Col_fk_ref` constructor to `col_constraint` in the parser header:

In parser.mly header:
```ocaml
  type col_constraint =
    | Col_not_null
    | Col_primary_key
    | Col_default of literal
    | Col_check   of expr
    | Col_fk_ref             (* parse-only, no semantic meaning *)
```

Then in grammar:
```
  | REFERENCES _ = IDENT               { Col_fk_ref }
  | REFERENCES _ = IDENT LPAREN _ = IDENT RPAREN { Col_fk_ref }
```

The existing code that builds `column_def` from constraints ignores unknown variants via `fold_left`, so `Col_fk_ref` is naturally discarded.

- [ ] **Step 4: Build and run tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | tail -20
```

Expected: both FK parse tests pass. Total ≥ 336 tests.

- [ ] **Step 5: Commit**

```bash
git add lib/sql/lexer.mll lib/sql/parser.mly test/test_e2e.ml
git commit -m "feat(phase9-task3): FOREIGN KEY parse-only — REFERENCES syntax accepted without enforcement (#51)"
```

---

## Task 4: SQLite Comparison Tests + Coverage Boost

Add 24+ comparison tests in `test/test_sqlite_compare.ml` to verify behavior matches SQLite for all three Phase 9 features. Also add edge-case E2E tests to push coverage.

**Files:**
- Modify: `test/test_sqlite_compare.ml`
- Modify: `test/test_e2e.ml`

- [ ] **Step 1: Understand the existing test_sqlite_compare.ml pattern**

The file uses a helper that runs a SQL sequence against both SQLite (in-memory) and sqlocaml, comparing results. Study the last 20 comparison tests to understand the exact helper signatures and patterns before writing new ones.

- [ ] **Step 2: Add 8 subquery comparison tests**

Add a `"phase9_subqueries"` group in `test/test_sqlite_compare.ml` with:

1. Scalar subquery: `SELECT (SELECT max(id) FROM t) FROM t` (compares value)
2. Scalar subquery with WHERE: `SELECT id FROM t WHERE id = (SELECT min(id) FROM t)`
3. EXISTS true: `SELECT count(*) FROM t WHERE EXISTS (SELECT 1 FROM t WHERE id = 1)`
4. EXISTS false on empty: `SELECT count(*) FROM t WHERE EXISTS (SELECT 1 FROM empty_t)`
5. IN (SELECT): `SELECT id FROM t WHERE id IN (SELECT id FROM ids)`
6. NOT IN (SELECT): `SELECT id FROM t WHERE id NOT IN (SELECT id FROM excluded)`
7. IN (SELECT) with empty inner: `SELECT id FROM t WHERE id IN (SELECT id FROM empty_t)`
8. Scalar subquery in SELECT list alongside regular columns

- [ ] **Step 3: Add 8 CHECK constraint comparison tests**

Add a `"phase9_check"` group with:

1. Check passes on valid insert
2. Check violation raises error (compare error type)
3. Check with multiple columns in expr
4. Check NULL passes (SQLite allows NULL through CHECK)
5. Check on UPDATE passes for valid new value
6. Check on UPDATE fails for invalid new value
7. Check with BETWEEN expr
8. Check with OR: `CHECK (status = 'active' OR status = 'inactive')`

- [ ] **Step 4: Add 8 FOREIGN KEY comparison tests**

Add a `"phase9_fk"` group with:

1. FK syntax accepted in CREATE TABLE (no error)
2. FK with column name accepted
3. FK without column name accepted
4. Insert into FK table succeeds (no enforcement)
5. Insert "dangling" FK value succeeds (confirming no enforcement)
6. Multi-column FK syntax (if we support it — otherwise document as unsupported)
7. FK combined with NOT NULL
8. FK combined with CHECK

- [ ] **Step 5: Add edge-case E2E tests for coverage**

Add to `test/test_e2e.ml` (in a new `"phase9_edge"` group):

1. `test_subquery_in_and` — `WHERE EXISTS(...) AND col > 5`
2. `test_subquery_in_project` — scalar subquery in SELECT list (`SELECT (SELECT max(v) FROM t), id FROM t`)
3. `test_check_complex_expr` — `CHECK (price > 0 AND price < 10000)`
4. `test_check_function` — `CHECK (LENGTH(name) > 0)`

- [ ] **Step 6: Run all tests and check coverage**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | tail -5
./scripts/coverage.sh
```

Target coverage:
- `lib/sql/exec.ml`: ≥ 89% (same as Phase 8, no regression)
- `lib/catalog/catalog.ml`: ≥ 89% (check_sql encode/decode covered)
- `lib/sql/sema.ml`: ≥ 81%

- [ ] **Step 7: Fix any coverage gaps**

If coverage drops below targets, add targeted tests for the newly uncovered branches:
- `pre_eval_subquery` branches (cat = None, Error from Sema.bind, etc.)
- `compile_check_expr` cache hit path
- `decode_column` backward-compat paths (bytes_left2 = 0)

- [ ] **Step 8: Commit**

```bash
git add test/test_sqlite_compare.ml test/test_e2e.ml
git commit -m "test(phase9): SQLite comparison tests + coverage for subqueries, CHECK, FK parse"
```

---

## Self-Review

**Spec coverage check:**
- Subqueries (scalar, EXISTS, IN SELECT): Task 1 ✓
- CHECK constraint enforcement + persistence: Task 2 ✓
- FOREIGN KEY parse tolerance: Task 3 ✓
- Coverage tests: Task 4 ✓

**Potential issues to watch:**
1. **Menhir shift/reduce conflict** on `LPAREN compound_select RPAREN` vs `LPAREN expr RPAREN`. If Menhir reports this, use the `paren_or_subquery` non-terminal workaround described in Task 1 Step 5.
2. **Mutual recursion in exec.ml**: `pre_eval_subquery` and `to_stream` must be in the same `let rec ... and ...` block. The existing `let rec to_stream` becomes `and to_stream`; `let rec pre_eval_subquery` is added before it.
3. **`check_cache` invalidation**: The module-level `check_cache` Hashtbl persists for the process lifetime. If a table is altered (renamed, column added) and its schema changes, cached check exprs may reference stale column ordinals. For Phase 9, document as a known limitation: CHECK cache is invalidated by process restart. Full cache invalidation on `Op_alter_table` is a Phase 10 concern.
4. **`bind_create` in sema.ml**: The `column_def → Row.column` mapping now includes `check_sql = Option.map Ast.expr_to_sql c.check`. `Ast.expr_to_sql` can raise `failwith` for unsupported expr forms (agg, match, subquery). For Phase 9, these forms are already rejected by the parser (no grammar rule produces them in a CHECK position), so `failwith` is an acceptable guard.
5. **Test updates for exhaustive matches**: test_parser.ml, test_sema.ml, test_exec.ml may have patterns that match `column_def`, `bound_expr`, or `Plan.expr`. After adding new constructors, the OCaml compiler will warn about non-exhaustive matches. Fix by adding wildcard or explicit new cases.
6. **`decode_column` variable shadowing**: OCaml's `let x, off = ...` pattern binds a new `off`. In `decode_column`, each `let ..., off = Varint.decode_uint64 bytes off in` rebinds `off`. The new check_sql decoding block must correctly thread the final `off` from the default decoding.

**No placeholders detected.** All code shown above is complete and specific.
