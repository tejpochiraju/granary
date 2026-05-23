# Phase 10: CASE WHEN Expressions + Multiple JOINs

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `CASE WHEN … THEN … END` expressions and support for more than one JOIN per SELECT query, the two largest gaps from real-world SQLite usage coverage.

**Architecture:**
CASE WHEN flows straight through the standard AST → Sema → Plan → Exec pipeline as a new expression type (`E_case` / `BE_case` / `P_case`).  Multiple JOINs require changing `BS_select.join : bound_join option` to `joins : bound_join list`, generalising `bind_expr_join` to accept an arbitrary table list `~tables : (Cat.table_meta * int) list`, and chaining join operators in the planner with `List.fold_left`.

**Tech Stack:** OCaml, Menhir, Alcotest, Lwt, Podman (all builds inside container)

---

## Environment

All `dune` commands run inside Podman — never on the host:

```bash
# Build
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1

# Test
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | tail -30

# Build + test one-liner
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev sh -c 'dune build 2>&1 && dune runtest 2>&1 | tail -20'
```

## File Map

| Task | File | Change |
|------|------|--------|
| 1 | `lib/sql/ast.ml` | Add `E_case` constructor + `expr_to_sql` branch |
| 1 | `lib/sql/lexer.mll` | Add CASE WHEN THEN ELSE END keywords |
| 1 | `lib/sql/parser.mly` | Add CASE tokens, `case_expr` rule, wire into `expr` |
| 1 | `lib/sql/sema.ml` | Add `BE_case`, handle in `bind_expr`, `bind_expr_join`, `bind_expr_agg`, `expr_has_subquery` |
| 1 | `lib/sql/sema.mli` | Add `BE_case` |
| 1 | `lib/sql/plan.ml` | Add `P_case` |
| 1 | `lib/sql/planner.ml` | Map `BE_case → P_case` |
| 1 | `lib/sql/exec.ml` | Evaluate `P_case` in `eval_expr`, walk in `pre_eval_subquery` |
| 2 | `lib/sql/sema.ml` | Change `bind_expr_join` signature; generalise `bind_select` for N joins |
| 2 | `lib/sql/sema.mli` | Change `BS_select.join → joins` |
| 2 | `lib/sql/planner.ml` | Chain multiple joins with fold in `plan_select` and no-cat path |
| 3 | `test/test_e2e.ml` | New groups: `case_when`, `multi_join` |
| 3 | `test/test_sqlite_compare.ml` | New groups: `phase10_case_when`, `phase10_multi_join` |

---

## Task 1: CASE WHEN expressions

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/sema.ml`
- Modify: `lib/sql/sema.mli`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/planner.ml`
- Modify: `lib/sql/exec.ml`
- Test: `test/test_e2e.ml`

### Background

CASE WHEN has two forms:

```sql
-- Searched form: each WHEN has a boolean predicate
CASE WHEN x > 0 THEN 'pos' WHEN x < 0 THEN 'neg' ELSE 'zero' END

-- Simple form: scrutinee compared to each WHEN value
CASE x WHEN 1 THEN 'one' WHEN 2 THEN 'two' ELSE 'other' END
```

Both use the same AST node `E_case { scrutinee = None | Some expr; branches; else_ }`.  Evaluation: walk branches in order; for searched form match when branch cond is truthy; for simple form match when scrutinee equals branch value (IS semantics — NULL = NULL is true).  If no branch matches, return ELSE result or NULL.

- [ ] **Step 1: Write failing tests**

Add a new `"case_when"` test group to `test/test_e2e.ml`.  Find the `let () = run ...` or `let tests = [...]` section near the bottom and add the group alongside existing ones.

```ocaml
(* ── CASE WHEN ─────────────────────────────────────────────────── *)

let test_case_searched () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (v INTEGER)";
  exec db "INSERT INTO t VALUES (3)";
  exec db "INSERT INTO t VALUES (-1)";
  exec db "INSERT INTO t VALUES (0)";
  let rows = query_ok db
    "SELECT CASE WHEN v > 0 THEN 'pos' WHEN v < 0 THEN 'neg' ELSE 'zero' END FROM t ORDER BY v" in
  let labels = List.map (fun r -> match r.(0) with Db.V_text s -> s | _ -> "?") rows in
  Alcotest.(check (list string)) "case_searched" ["neg"; "zero"; "pos"] labels

let test_case_simple () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (v INTEGER)";
  exec db "INSERT INTO t VALUES (1)";
  exec db "INSERT INTO t VALUES (2)";
  exec db "INSERT INTO t VALUES (3)";
  let rows = query_ok db
    "SELECT CASE v WHEN 1 THEN 'one' WHEN 2 THEN 'two' ELSE 'other' END FROM t ORDER BY v" in
  let labels = List.map (fun r -> match r.(0) with Db.V_text s -> s | _ -> "?") rows in
  Alcotest.(check (list string)) "case_simple" ["one"; "two"; "other"] labels

let test_case_no_else () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (v INTEGER)";
  exec db "INSERT INTO t VALUES (99)";
  let rows = query_ok db "SELECT CASE WHEN v = 0 THEN 'zero' END FROM t" in
  Alcotest.(check (list bool)) "null" [true]
    (List.map (fun r -> r.(0) = Db.V_null) rows)

let test_case_null_scrutinee () =
  (* CASE NULL WHEN NULL THEN 1 END = 1 (IS semantics) *)
  let db = fresh_db () in
  exec db "CREATE TABLE t (v INTEGER)";
  exec db "INSERT INTO t VALUES (NULL)";
  let rows = query_ok db "SELECT CASE v WHEN NULL THEN 1 ELSE 0 END FROM t" in
  let vals = List.map (fun r -> match r.(0) with Db.V_int n -> Int64.to_int n | _ -> -1) rows in
  Alcotest.(check (list int)) "null_scrutinee" [1] vals

let test_case_in_where () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (v INTEGER)";
  exec db "INSERT INTO t VALUES (1)";
  exec db "INSERT INTO t VALUES (2)";
  exec db "INSERT INTO t VALUES (3)";
  let rows = query_ok db
    "SELECT v FROM t WHERE CASE WHEN v > 1 THEN 1 ELSE 0 END = 1 ORDER BY v" in
  let vals = List.map (fun r -> match r.(0) with Db.V_int n -> Int64.to_int n | _ -> -1) rows in
  Alcotest.(check (list int)) "case_in_where" [2; 3] vals

let test_case_nested () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (v INTEGER)";
  exec db "INSERT INTO t VALUES (5)";
  let rows = query_ok db
    "SELECT CASE WHEN v > 0 THEN CASE WHEN v > 3 THEN 'big' ELSE 'small' END ELSE 'neg' END FROM t" in
  let labels = List.map (fun r -> match r.(0) with Db.V_text s -> s | _ -> "?") rows in
  Alcotest.(check (list string)) "nested_case" ["big"] labels

let test_case_arithmetic () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (v INTEGER)";
  exec db "INSERT INTO t VALUES (4)";
  let rows = query_ok db "SELECT CASE WHEN v > 2 THEN v * 10 ELSE v END FROM t" in
  let vals = List.map (fun r -> match r.(0) with Db.V_int n -> Int64.to_int n | _ -> -1) rows in
  Alcotest.(check (list int)) "arithmetic" [40] vals
```

Wire the group into the test runner.  Find where other groups like `"subqueries"` or `"check_constraints"` appear in the test list and add:

```ocaml
"case_when", [
  Alcotest.test_case "case_searched"      `Quick test_case_searched;
  Alcotest.test_case "case_simple"        `Quick test_case_simple;
  Alcotest.test_case "case_no_else"       `Quick test_case_no_else;
  Alcotest.test_case "case_null_scrutinee"`Quick test_case_null_scrutinee;
  Alcotest.test_case "case_in_where"      `Quick test_case_in_where;
  Alcotest.test_case "case_nested"        `Quick test_case_nested;
  Alcotest.test_case "case_arithmetic"    `Quick test_case_arithmetic;
];
```

- [ ] **Step 2: Run tests to confirm they fail**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | grep -A5 "case_when"
```

Expected: parse errors or `Unknown constructor` type errors — tests fail because CASE WHEN is not yet parsed.

- [ ] **Step 3: Add `E_case` to `lib/sql/ast.ml`**

In `ast.ml`, add `E_case` to the `expr` type after `E_in_select`:

```ocaml
  | E_case of {
      scrutinee : expr option;           (** None = searched form, Some = simple form *)
      branches  : (expr * expr) list;    (** (WHEN condition/value, THEN result) *)
      else_     : expr option;
    }
```

Add a branch to `expr_to_sql` before the final `| E_agg _ | ...` catch-all:

```ocaml
  | E_case { scrutinee; branches; else_ } ->
    let scr = match scrutinee with
      | None   -> ""
      | Some e -> " " ^ expr_to_sql e
    in
    let brs = String.concat " " (List.map (fun (cond, res) ->
      Printf.sprintf "WHEN %s THEN %s" (expr_to_sql cond) (expr_to_sql res)
    ) branches) in
    let el = match else_ with
      | None   -> ""
      | Some e -> " ELSE " ^ expr_to_sql e
    in
    Printf.sprintf "CASE%s %s%s END" scr brs el
```

- [ ] **Step 4: Add CASE/WHEN/THEN/ELSE/END tokens to `lib/sql/lexer.mll`**

In `lexer.mll`, find the large keyword match table (where `"EXISTS"`, `"CHECK"`, etc. are defined).  Add five new entries alongside the others:

```ocaml
      | "CASE"       | "case"       -> CASE
      | "WHEN"       | "when"       -> WHEN
      | "THEN"       | "then"       -> THEN
      | "ELSE"       | "else"       -> ELSE
      | "END"        | "end"        -> END
```

- [ ] **Step 5: Add token declarations and grammar to `lib/sql/parser.mly`**

In `parser.mly`, add to the `%token` declarations (near CHECK REFERENCES FOREIGN):

```
%token CASE WHEN THEN ELSE END
```

Add two helper rules before `expr` (place them after `agg_expr` for clarity):

```menhir
when_clause:
  | WHEN cond = expr THEN result = expr { (cond, result) }

else_clause:
  | ELSE e = expr { e }

case_expr:
  | CASE bs = nonempty_list(when_clause) el = option(else_clause) END
    { E_case { scrutinee = None; branches = bs; else_ = el } }
  | CASE scr = expr bs = nonempty_list(when_clause) el = option(else_clause) END
    { E_case { scrutinee = Some scr; branches = bs; else_ = el } }
```

In the `expr` rule, add one line alongside `EXISTS` and `paren_or_subquery`:

```menhir
  | e = case_expr { e }
```

Also add `case_expr` to `between_bound` (so CASE can appear inside BETWEEN ranges):

```menhir
  | e = case_expr { e }
```

- [ ] **Step 6: Build to check for parser conflicts**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
```

Expected: clean build with no shift/reduce conflicts reported.  If Menhir reports a conflict, it will print details — the most likely cause is WHEN being reachable from `expr` (check that WHEN is not listed in any existing `expr` alternative).

- [ ] **Step 7: Add `BE_case` to `lib/sql/sema.ml` and `sema.mli`**

In `sema.ml`, add to `type bound_expr` after `BE_in_select`:

```ocaml
  | BE_case of {
      scrutinee : bound_expr option;
      branches  : (bound_expr * bound_expr) list;
      else_     : bound_expr option;
    }
```

In `sema.mli`, add the same constructor with a short doc comment:

```ocaml
  | BE_case of {
      scrutinee : bound_expr option;
      branches  : (bound_expr * bound_expr) list;
      else_     : bound_expr option;
    }
    (** CASE [scrutinee] WHEN … THEN … [ELSE …] END *)
```

- [ ] **Step 8: Handle `E_case` in `bind_expr` (single-table path)**

In `sema.ml`, find `bind_expr` (the function that binds expressions against a single-table `Cat.table_meta`).  Add a case for `E_case` near the bottom, before or after the `E_subquery`/`E_exists`/`E_in_select` cases:

```ocaml
  | Ast.E_case { scrutinee; branches; else_ } ->
    let scrutinee_result =
      match scrutinee with
      | None   -> Ok None
      | Some e ->
        match bind_expr ~param_counter ~named_params meta e with
        | Ok be   -> Ok (Some be)
        | Error e -> Error e
    in
    (match scrutinee_result with
     | Error e -> Error e
     | Ok bound_scr ->
       let branch_results =
         List.map (fun (cond, res) ->
           match bind_expr ~param_counter ~named_params meta cond,
                 bind_expr ~param_counter ~named_params meta res with
           | Ok bc, Ok br -> Ok (bc, br)
           | Error e, _   -> Error e
           | _, Error e   -> Error e
         ) branches
       in
       let branch_errors = List.filter_map
         (function Error e -> Some e | Ok _ -> None) branch_results in
       (match branch_errors with
        | e :: _ -> Error e
        | [] ->
          let bound_branches =
            List.filter_map (function Ok p -> Some p | Error _ -> None) branch_results
          in
          let else_result =
            match else_ with
            | None   -> Ok None
            | Some e ->
              match bind_expr ~param_counter ~named_params meta e with
              | Ok be   -> Ok (Some be)
              | Error e -> Error e
          in
          (match else_result with
           | Error e -> Error e
           | Ok bound_else ->
             Ok (BE_case { scrutinee = bound_scr; branches = bound_branches; else_ = bound_else }))))
```

- [ ] **Step 9: Handle `E_case` in `bind_expr_join` (two-table path)**

In `sema.ml`, find `bind_expr_join`.  Add a case for `E_case` near the end (before the MATCH/subquery Unsupported cases):

```ocaml
  | Ast.E_case { scrutinee; branches; else_ } ->
    let scrutinee_result =
      match scrutinee with
      | None   -> Ok None
      | Some e ->
        match bind_expr_join ~param_counter ~named_params ~left_meta ~right_meta ~right_offset e with
        | Ok be   -> Ok (Some be)
        | Error e -> Error e
    in
    (match scrutinee_result with
     | Error e -> Error e
     | Ok bound_scr ->
       let branch_results =
         List.map (fun (cond, res) ->
           match bind_expr_join ~param_counter ~named_params ~left_meta ~right_meta ~right_offset cond,
                 bind_expr_join ~param_counter ~named_params ~left_meta ~right_meta ~right_offset res with
           | Ok bc, Ok br -> Ok (bc, br)
           | Error e, _   -> Error e
           | _, Error e   -> Error e
         ) branches
       in
       let branch_errors = List.filter_map
         (function Error e -> Some e | Ok _ -> None) branch_results in
       (match branch_errors with
        | e :: _ -> Error e
        | [] ->
          let bound_branches =
            List.filter_map (function Ok p -> Some p | Error _ -> None) branch_results
          in
          let else_result =
            match else_ with
            | None   -> Ok None
            | Some e ->
              match bind_expr_join ~param_counter ~named_params ~left_meta ~right_meta ~right_offset e with
              | Ok be   -> Ok (Some be)
              | Error e -> Error e
          in
          (match else_result with
           | Error e -> Error e
           | Ok bound_else ->
             Ok (BE_case { scrutinee = bound_scr; branches = bound_branches; else_ = bound_else }))))
```

- [ ] **Step 10: Handle `E_case` in `bind_expr_agg` (aggregate-aware path)**

In `sema.ml`, find `bind_expr_agg`.  Inside the inner `go` function, add a case for `E_case` before the catch-all:

```ocaml
    | Ast.E_case { scrutinee; branches; else_ } ->
      let scrutinee_result =
        match scrutinee with
        | None   -> Ok None
        | Some e -> match go e with Ok be -> Ok (Some be) | Error e -> Error e
      in
      (match scrutinee_result with
       | Error e -> Error e
       | Ok bound_scr ->
         let branch_results =
           List.map (fun (cond, res) ->
             match go cond, go res with
             | Ok bc, Ok br -> Ok (bc, br)
             | Error e, _   -> Error e
             | _, Error e   -> Error e
           ) branches
         in
         let branch_errors = List.filter_map
           (function Error e -> Some e | Ok _ -> None) branch_results in
         (match branch_errors with
          | e :: _ -> Error e
          | [] ->
            let bound_branches =
              List.filter_map (function Ok p -> Some p | Error _ -> None) branch_results
            in
            let else_result =
              match else_ with
              | None   -> Ok None
              | Some e -> match go e with Ok be -> Ok (Some be) | Error e -> Error e
            in
            (match else_result with
             | Error e -> Error e
             | Ok bound_else ->
               Ok (BE_case { scrutinee = bound_scr; branches = bound_branches; else_ = bound_else }))))
```

- [ ] **Step 11: Handle `BE_case` in `expr_has_subquery`**

In `sema.ml`, find `expr_has_subquery` and add:

```ocaml
  | BE_case { scrutinee; branches; else_ } ->
    (match scrutinee with Some e -> expr_has_subquery e | None -> false)
    || List.exists (fun (c, r) -> expr_has_subquery c || expr_has_subquery r) branches
    || (match else_ with Some e -> expr_has_subquery e | None -> false)
```

- [ ] **Step 12: Add `P_case` to `lib/sql/plan.ml`**

In `plan.ml`, add to `type expr` after `P_in_select`:

```ocaml
  | P_case of {
      scrutinee : expr option;
      branches  : (expr * expr) list;
      else_     : expr option;
    }
```

- [ ] **Step 13: Map `BE_case → P_case` in `lib/sql/planner.ml`**

In `planner.ml`, find `plan_expr` and add after the `BE_in_select` case:

```ocaml
  | Sema.BE_case { scrutinee; branches; else_ } ->
    Plan.P_case {
      scrutinee = Option.map plan_expr scrutinee;
      branches  = List.map (fun (c, r) -> (plan_expr c, plan_expr r)) branches;
      else_     = Option.map plan_expr else_;
    }
```

- [ ] **Step 14: Evaluate `P_case` in `lib/sql/exec.ml`**

In `exec.ml`, find `eval_expr`.  After `Plan.P_in` case, add:

```ocaml
  | Plan.P_case { scrutinee; branches; else_ } ->
    let scr_val = Option.map (eval_expr clock params row) scrutinee in
    let rec find_match = function
      | [] ->
        (match else_ with
         | None   -> Row.V_null
         | Some e -> eval_expr clock params row e)
      | (cond, result) :: rest ->
        let matched = match scr_val with
          | None ->
            value_truthy (eval_expr clock params row cond)
          | Some sv ->
            compare_values sv (eval_expr clock params row cond) = 0
        in
        if matched then eval_expr clock params row result
        else find_match rest
    in
    find_match branches
```

Also update the wildcard guard currently at `| Plan.P_subquery _ | Plan.P_exists _ | Plan.P_in_select _ ->` — make sure the new `P_case` is not included in that guard.

- [ ] **Step 15: Walk `P_case` in `pre_eval_subquery`**

In `exec.ml`, find `pre_eval_subquery`.  Add a case for `P_case` (before the `| _ -> Lwt.return e` leaf fallback):

```ocaml
  | Plan.P_case { scrutinee; branches; else_ } ->
    let* scrutinee' =
      match scrutinee with
      | None   -> Lwt.return None
      | Some e ->
        let+ e' = pre_eval_subquery clock params db e in
        Some e'
    in
    let* branches' = Lwt_list.map_s (fun (cond, res) ->
      let* cond' = pre_eval_subquery clock params db cond in
      let+ res'  = pre_eval_subquery clock params db res  in
      (cond', res')
    ) branches in
    let+ else_' =
      match else_ with
      | None   -> Lwt.return None
      | Some e ->
        let+ e' = pre_eval_subquery clock params db e in
        Some e'
    in
    Plan.P_case { scrutinee = scrutinee'; branches = branches'; else_ = else_' }
```

- [ ] **Step 16: Build and run tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev sh -c 'dune build 2>&1 && dune runtest 2>&1 | tail -30'
```

Expected: all `case_when` tests pass; no regressions in existing groups.

- [ ] **Step 17: Commit**

```bash
git add lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly \
        lib/sql/sema.ml lib/sql/sema.mli lib/sql/plan.ml \
        lib/sql/planner.ml lib/sql/exec.ml test/test_e2e.ml
git commit -m "feat(phase10-task1): CASE WHEN expressions (searched and simple form)"
```

---

## Task 2: Multiple JOINs per SELECT

**Files:**
- Modify: `lib/sql/sema.ml`
- Modify: `lib/sql/sema.mli`
- Modify: `lib/sql/planner.ml`
- Test: `test/test_e2e.ml`

### Background

The AST already parses unlimited joins (`joins : join_clause list` in `S_select`).  The bottleneck is in sema.ml which rejects `List.length joins > 1` and stores only `join : bound_join option` in `BS_select`.

The key design changes:
1. `bind_expr_join` changes its signature from `~left_meta ~right_meta ~right_offset` to `~tables : (Cat.table_meta * int) list` where each entry is `(table_meta, base_column_ordinal)`.  The primary table is always first at offset 0.
2. `BS_select.join : bound_join option` becomes `BS_select.joins : bound_join list`.
3. `bind_select` resolves all join table metas up front, builds the `tables` list, then binds each join's ON predicate against the tables visible so far (primary + all previously joined tables + current join table).
4. `plan_select` chains join operators with `List.fold_left`.

- [ ] **Step 1: Write failing tests**

Add a `"multi_join"` test group to `test/test_e2e.ml`:

```ocaml
(* ── Multiple JOINs ──────────────────────────────────────────────── *)

let test_three_table_join () =
  let db = fresh_db () in
  exec db "CREATE TABLE a (id INTEGER, name TEXT)";
  exec db "CREATE TABLE b (aid INTEGER, val INTEGER)";
  exec db "CREATE TABLE c (bid INTEGER, extra TEXT)";
  exec db "INSERT INTO a VALUES (1, 'alice')";
  exec db "INSERT INTO b VALUES (1, 42)";
  exec db "INSERT INTO c VALUES (42, 'extra')";
  let rows = query_ok db
    "SELECT a.name, b.val, c.extra FROM a JOIN b ON a.id = b.aid JOIN c ON b.val = c.bid" in
  Alcotest.(check int) "one row" 1 (List.length rows);
  let row = List.hd rows in
  Alcotest.(check string) "name"  "alice" (match row.(0) with Db.V_text s -> s | _ -> "?");
  Alcotest.(check int)   "val"   42      (match row.(1) with Db.V_int n -> Int64.to_int n | _ -> -1);
  Alcotest.(check string) "extra" "extra" (match row.(2) with Db.V_text s -> s | _ -> "?")

let test_two_joins_with_where () =
  let db = fresh_db () in
  exec db "CREATE TABLE u (id INTEGER, name TEXT)";
  exec db "CREATE TABLE o (uid INTEGER, item TEXT)";
  exec db "CREATE TABLE p (item TEXT, price INTEGER)";
  exec db "INSERT INTO u VALUES (1, 'alice'), (2, 'bob')";
  exec db "INSERT INTO o VALUES (1, 'hat'), (2, 'book')";
  exec db "INSERT INTO p VALUES ('hat', 10), ('book', 5)";
  let rows = query_ok db
    "SELECT u.name, p.price FROM u JOIN o ON u.id = o.uid JOIN p ON o.item = p.item WHERE u.id = 1" in
  Alcotest.(check int) "one row" 1 (List.length rows);
  let row = List.hd rows in
  Alcotest.(check string) "name"  "alice" (match row.(0) with Db.V_text s -> s | _ -> "?");
  Alcotest.(check int)    "price" 10      (match row.(1) with Db.V_int n -> Int64.to_int n | _ -> -1)

let test_two_left_joins () =
  let db = fresh_db () in
  exec db "CREATE TABLE a (id INTEGER)";
  exec db "CREATE TABLE b (aid INTEGER, v TEXT)";
  exec db "CREATE TABLE c (aid INTEGER, w TEXT)";
  exec db "INSERT INTO a VALUES (1), (2)";
  exec db "INSERT INTO b VALUES (1, 'B1')";
  exec db "INSERT INTO c VALUES (2, 'C2')";
  let rows = query_ok db
    "SELECT a.id, b.v, c.w FROM a LEFT JOIN b ON a.id = b.aid LEFT JOIN c ON a.id = c.aid ORDER BY a.id" in
  Alcotest.(check int) "two rows" 2 (List.length rows);
  let r0 = List.nth rows 0 in
  let r1 = List.nth rows 1 in
  Alcotest.(check bool) "b.v row0 not null" true (r0.(1) <> Db.V_null);
  Alcotest.(check bool) "c.w row0 null"     true (r0.(2) = Db.V_null);
  Alcotest.(check bool) "b.v row1 null"     true (r1.(1) = Db.V_null);
  Alcotest.(check bool) "c.w row1 not null" true (r1.(2) <> Db.V_null)

let test_star_three_tables () =
  let db = fresh_db () in
  exec db "CREATE TABLE x (a INTEGER)";
  exec db "CREATE TABLE y (b INTEGER)";
  exec db "CREATE TABLE z (c INTEGER)";
  exec db "INSERT INTO x VALUES (1)";
  exec db "INSERT INTO y VALUES (2)";
  exec db "INSERT INTO z VALUES (3)";
  let rows = query_ok db "SELECT * FROM x JOIN y ON 1=1 JOIN z ON 1=1" in
  Alcotest.(check int) "one row" 1 (List.length rows);
  Alcotest.(check int) "three cols" 3 (Array.length (List.hd rows))

let test_multi_join_unsupported_one () =
  (* Regression: single JOIN still works *)
  let db = fresh_db () in
  exec db "CREATE TABLE a (id INTEGER, v TEXT)";
  exec db "CREATE TABLE b (aid INTEGER, w TEXT)";
  exec db "INSERT INTO a VALUES (1, 'X')";
  exec db "INSERT INTO b VALUES (1, 'Y')";
  let rows = query_ok db "SELECT a.v, b.w FROM a JOIN b ON a.id = b.aid" in
  Alcotest.(check int) "one row" 1 (List.length rows)
```

Wire into test runner:

```ocaml
"multi_join", [
  Alcotest.test_case "three_table_join"    `Quick test_three_table_join;
  Alcotest.test_case "two_joins_with_where"`Quick test_two_joins_with_where;
  Alcotest.test_case "two_left_joins"      `Quick test_two_left_joins;
  Alcotest.test_case "star_three_tables"   `Quick test_star_three_tables;
  Alcotest.test_case "regression_one_join" `Quick test_multi_join_unsupported_one;
];
```

- [ ] **Step 2: Confirm tests fail**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | grep -A5 "multi_join"
```

Expected: `Unsupported: more than one JOIN clause is not supported in Phase 2` errors.

- [ ] **Step 3: Generalise `bind_expr_join` in `lib/sql/sema.ml`**

Replace the existing `bind_expr_join` signature and its column resolution.  The new signature takes `~tables : (Cat.table_meta * int) list`:

Change the function header from:
```ocaml
let rec bind_expr_join
    ~param_counter
    ~named_params
    ~(left_meta : Cat.table_meta)
    ~(right_meta : Cat.table_meta)
    ~(right_offset : int)
  = function
```

To:
```ocaml
let rec bind_expr_join
    ~param_counter
    ~named_params
    ~(tables : (Cat.table_meta * int) list)
  = function
```

Replace the two column resolution cases:

```ocaml
  (* Old E_col case:
     let in_left  = col_index left_meta.columns  name in
     let in_right = col_index right_meta.columns name in
     ...
  *)
  (* New E_col case: *)
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
  (* Old E_tbl_col case used String.equal tbl left_meta.name / right_meta.name *)
  (* New E_tbl_col case: *)
  | Ast.E_tbl_col (tbl, name) ->
    (match List.find_opt (fun (tm, _) -> String.equal tm.Cat.name tbl) tables with
     | None            -> Error (Unknown_table tbl)
     | Some (tm, base) ->
       (match col_index tm.Cat.columns name with
        | Some i -> Ok (BE_col (base + i))
        | None   -> Error (Unknown_column { table = tbl; column = name })))
```

Update every recursive call within `bind_expr_join` from:
```ocaml
bind_expr_join ~param_counter ~named_params ~left_meta ~right_meta ~right_offset X
```
To:
```ocaml
bind_expr_join ~param_counter ~named_params ~tables X
```

There are approximately 15–20 such recursive calls (in E_binop, E_not, E_is_null, E_is_not_null, E_neg, E_bitnot, E_between, E_in, E_func, E_case).

Also update the `E_case` branch you added in Task 1 Step 9 to use `~tables` instead of `~left_meta ~right_meta ~right_offset`.

- [ ] **Step 4: Update `BS_select` in `sema.ml` and `sema.mli`**

In `sema.ml`, in `type bound_stmt`, change the `BS_select` field:
```ocaml
      join       : bound_join option;
```
To:
```ocaml
      joins      : bound_join list;
```

In `sema.mli`, make the same change to `BS_select`:
```ocaml
      join       : bound_join option;   (** Phase 2: single optional JOIN *)
```
→
```ocaml
      joins      : bound_join list;     (** Phase 10: zero or more JOINs *)
```

- [ ] **Step 5: Rewrite `bind_select` to support N joins**

In `sema.ml`, find `bind_select`.

**Remove** the early guard (lines ~907–909):
```ocaml
    if List.length joins > 1 then
      Lwt.return (Error (Unsupported
        "more than one JOIN clause is not supported in Phase 2"))
    else
```

**Replace** the single-join `join_meta_result` fetch with a fold over all join clauses:

Old code (approximately lines 911–920):
```ocaml
    let* join_meta_result =
      match joins with
      | [] -> Lwt.return (Ok None)
      | [ (jc : Ast.join_clause) ] ->
        let* rm_opt = Cat.find_table cat ~name:jc.table in
        (match rm_opt with
         | None -> Lwt.return (Error (Unknown_table jc.table))
         | Some rm -> Lwt.return (Ok (Some (jc, rm))))
      | _ -> Lwt.return (Ok None)
    in
```

New code:
```ocaml
    (* Resolve all join table metas: [(jc, rm); ...] in order *)
    let* joined_pairs_result =
      Lwt_list.fold_left_s (fun acc (jc : Ast.join_clause) ->
        match acc with
        | Error e -> Lwt.return (Error e)
        | Ok pairs ->
          let* rm_opt = Cat.find_table cat ~name:jc.table in
          (match rm_opt with
           | None    -> Lwt.return (Error (Unknown_table jc.table))
           | Some rm -> Lwt.return (Ok (pairs @ [(jc, rm)])))
      ) (Ok []) joins
    in
```

**Replace** the `match join_meta_result with ... | Ok join_info ->` wrapper with `match joined_pairs_result with ... | Ok joined_pairs ->`.

Then **replace** the `n_left / right_offset` setup and the `proj_lookup` / `qual_lookup` closures:

Old:
```ocaml
       let n_left = List.length meta.columns in
       let right_offset = n_left in
       let proj_lookup name : (int, error) result =
         match join_info with
         | None -> ...
         | Some (_jc, rm) -> ... (* searches left then right *)
       in
       let qual_lookup t c : (int, error) result =
         match join_info with
         | None -> ...
         | Some (_jc, rm) -> ... (* checks left or right table name *)
       in
```

New:
```ocaml
       let n_left = List.length meta.columns in
       (* Build tables list: [(meta, 0); (rm0, n_left); (rm1, n_left+n_rm0); ...] *)
       let (tables, _total_width) =
         List.fold_left (fun (acc, off) (_, rm) ->
           let n = List.length rm.Cat.columns in
           (acc @ [(rm, off)], off + n)
         ) ([(meta, 0)], n_left) joined_pairs
       in
       let proj_lookup name : (int, error) result =
         let hits = List.filter_map (fun (tm, base) ->
           match col_index tm.Cat.columns name with
           | Some i -> Some (base + i) | None -> None
         ) tables in
         (match hits with
          | [i]   -> Ok i
          | []    -> Error (Unknown_column { table = meta.Cat.name; column = name })
          | _ :: _ -> Error (Ambiguous_column name))
       in
       let qual_lookup t c : (int, error) result =
         match List.find_opt (fun (tm, _) -> String.equal tm.Cat.name t) tables with
         | None            -> Error (Unknown_table t)
         | Some (tm, base) ->
           (match col_index tm.Cat.columns c with
            | Some i -> Ok (base + i)
            | None   -> Error (Unknown_column { table = t; column = c }))
       in
```

**Replace** the `bind_one` closure used for non-aggregated projection:

Old:
```ocaml
           let bind_one e =
             match join_info with
             | None -> bind_expr ~param_counter ~named_params meta e
             | Some (_jc, rm) ->
               bind_expr_join ~param_counter ~named_params ~left_meta:meta ~right_meta:rm ~right_offset e
           in
```

New:
```ocaml
           let bind_one e =
             if joined_pairs = [] then
               bind_expr ~param_counter ~named_params meta e
             else
               bind_expr_join ~param_counter ~named_params ~tables e
           in
```

**Replace** the `SELECT *` expansion for the join case:

Old:
```ocaml
             | `All ->
               let left_ords = List.mapi (fun i _ -> i) meta.columns in
               (match join_info with
                | None -> Ok (`Ords left_ords)
                | Some (_jc, rm) ->
                  let n_right = List.length rm.columns in
                  let right_ords = List.init n_right (fun i -> right_offset + i) in
                  Ok (`Ords (left_ords @ right_ords)))
```

New:
```ocaml
             | `All ->
               let all_ords = List.concat_map (fun (tm, base) ->
                 List.mapi (fun i _ -> base + i) tm.Cat.columns
               ) tables in
               Ok (`Ords all_ords)
```

**Replace** the `validate_numeric` column access inside the aggregated path.

Old:
```ocaml
                 let cols =
                   match join_info with
                   | None -> meta.columns
                   | Some (_jc, rm) -> meta.columns @ rm.Cat.columns
                 in
```

New:
```ocaml
                 let cols = List.concat_map (fun (tm, _) -> tm.Cat.columns) tables in
```

**Replace** the single `bound_join_result` binding with a loop that binds each join's ON predicate incrementally:

Old:
```ocaml
          let bound_join_result : (bound_join option, error) result =
            match join_info with
            | None -> Ok None
            | Some (jc, rm) ->
              (match bind_expr_join
                       ~param_counter ~named_params
                       ~left_meta:meta ~right_meta:rm ~right_offset jc.Ast.on with
               | Error e -> Error e
               | Ok be ->
                 Ok (Some { kind = jc.Ast.kind; right_meta = rm;
                            on = be; right_col_offset = right_offset }))
          in
          (match bound_join_result with
           | Error e -> Lwt.return (Error e)
           | Ok bound_join ->
```

New:
```ocaml
          (* Bind each join ON predicate against tables visible so far *)
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
          in
          (match bind_joins_result with
           | Error e -> Lwt.return (Error e)
           | Ok bound_joins ->
```

**Replace** the `bind_combined` closure:

Old:
```ocaml
             let bind_combined e =
               match join_info with
               | None -> bind_expr ~param_counter ~named_params meta e
               | Some (_jc, rm) ->
                 bind_expr_join ~param_counter ~named_params ~left_meta:meta ~right_meta:rm ~right_offset e
             in
```

New:
```ocaml
             let bind_combined e =
               if joined_pairs = [] then
                 bind_expr ~param_counter ~named_params meta e
               else
                 bind_expr_join ~param_counter ~named_params ~tables e
             in
```

**Replace** the ORDER BY binding (two instances of `bind_expr_join` with old args):

Old:
```ocaml
                        | Some (_jc, rm) ->
                          bind_expr_join ~param_counter ~named_params ~left_meta:meta
                            ~right_meta:rm ~right_offset ok.Ast.expr
```

New:
```ocaml
                        | _ ->
                          bind_expr_join ~param_counter ~named_params ~tables ok.Ast.expr
```

**Update** the final `BS_select { ... }` construction, changing `join = bound_join` to `joins = bound_joins`.

The closing parens also need to reflect the new nesting (remove one layer since we folded `join_meta_result` + `bound_join_result` into `joined_pairs_result` + `bind_joins_result`).

- [ ] **Step 6: Build to check for compile errors**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
```

Expected: clean build.  Common errors at this point:
- `Unbound record field join` → you forgot to update a site that constructs or matches `BS_select`
- `bind_expr_join` receives old `~left_meta` arg → search for remaining old-style calls

- [ ] **Step 7: Update `plan_select` in `lib/sql/planner.ml`**

Change the `plan_select` signature from `~join : Sema.bound_join option` to `~joins : Sema.bound_join list`:

```ocaml
let plan_select cat
    ~table_meta ~proj ~expr_proj ~where ~order ~limit ~offset ~joins
    ~group_by ~aggs ~having ~agg_proj ~distinct =
  let has_joins = joins <> [] in
  let base =
    if has_joins then
      Plan.Op_seq_scan { table_meta }
    else
      (match where with
       | None -> Plan.Op_seq_scan { table_meta }
       | Some e ->
         (match recognise_eq_col_lit e with
          | Some (col_idx, lit_expr) ->
            (match find_index_on_col cat table_meta col_idx with
             | Some idx ->
               let col_type = (List.nth table_meta.columns col_idx).Row.ty in
               Plan.Op_index_lookup {
                 table_tree = table_meta.tree_id;
                 idx_tree   = idx.Cat.idx_tree_id;
                 col_idx;
                 col_type;
                 lookup_val = plan_expr lit_expr;
                 table_meta;
               }
             | None ->
               Plan.Op_filter {
                 pred = plan_expr e;
                 child = Plan.Op_seq_scan { table_meta };
               })
          | None ->
            Plan.Op_filter {
              pred = plan_expr e;
              child = Plan.Op_seq_scan { table_meta };
            }))
  in
  (* Chain all joins left to right *)
  let (after_joins, _) =
    List.fold_left (fun (op, n_left) (bj : Sema.bound_join) ->
      let joined = plan_join cat bj op n_left in
      let n_left' = n_left + List.length bj.Sema.right_meta.Cat.columns in
      (joined, n_left')
    ) (base, List.length table_meta.columns) joins
  in
  let after_where =
    if has_joins then
      (match where with
       | None   -> after_joins
       | Some e -> Plan.Op_filter { pred = plan_expr e; child = after_joins })
    else
      after_joins  (* single-table: index-based path already applied WHERE above *)
  in
```

The rest of `plan_select` (`is_aggregated`, `make_sort`, projections, etc.) is unchanged.

In the `plan` function's `BS_select` match, update:

Old:
```ocaml
  | Sema.BS_select { distinct; table_meta; proj; expr_proj; where; order; limit; offset;
                     join; group_by; aggs; having; agg_proj } ->
    (match cat with
     | Some cat ->
       plan_select cat ~table_meta ~proj ~expr_proj ~where ~order ~limit ~offset
         ~join ~group_by ~aggs ~having ~agg_proj ~distinct
```

New:
```ocaml
  | Sema.BS_select { distinct; table_meta; proj; expr_proj; where; order; limit; offset;
                     joins; group_by; aggs; having; agg_proj } ->
    (match cat with
     | Some cat ->
       plan_select cat ~table_meta ~proj ~expr_proj ~where ~order ~limit ~offset
         ~joins ~group_by ~aggs ~having ~agg_proj ~distinct
```

**Update the "no catalog" path** in `plan`'s `BS_select` branch (the `| None ->` arm that builds hash-joins manually):

Old:
```ocaml
     | None ->
       let base = Plan.Op_seq_scan { table_meta } in
       let after_join : Plan.op = match join with
         | None -> base
         | Some bj ->
           ...
       in
```

New:
```ocaml
     | None ->
       let base = Plan.Op_seq_scan { table_meta } in
       let (after_joins, _) =
         List.fold_left (fun (op, n_left) (bj : Sema.bound_join) ->
           let n_right_cols = List.length bj.right_meta.Cat.columns in
           let right_offset = bj.right_col_offset in
           let join_kind = match bj.kind with
             | Ast.Inner -> `Inner | Ast.Left -> `Left
           in
           let joined =
             (match recognise_eq_col_col bj.on with
              | Some (a, b) when (a < n_left) && (b >= right_offset) ->
                Plan.Op_hash_join {
                  left = op;
                  right = Plan.Op_seq_scan { table_meta = bj.right_meta };
                  left_key = a; right_key = b - right_offset;
                  join_kind; right_col_offset = right_offset; n_right_cols;
                }
              | Some (a, b) when (b < n_left) && (a >= right_offset) ->
                Plan.Op_hash_join {
                  left = op;
                  right = Plan.Op_seq_scan { table_meta = bj.right_meta };
                  left_key = b; right_key = a - right_offset;
                  join_kind; right_col_offset = right_offset; n_right_cols;
                }
              | _ ->
                let cart = Plan.Op_hash_join {
                  left = op;
                  right = Plan.Op_seq_scan { table_meta = bj.right_meta };
                  left_key = -1; right_key = -1;
                  join_kind; right_col_offset = right_offset; n_right_cols;
                } in
                Plan.Op_filter { pred = plan_expr bj.on; child = cart })
           in
           (joined, n_left + n_right_cols)
         ) (base, List.length table_meta.columns) joins
       in
       let filtered = match where with
         | None   -> after_joins
         | Some e -> Plan.Op_filter { pred = plan_expr e; child = after_joins }
       in
```

Update the rest of the no-catalog path to use `filtered` instead of `filtered` (it should already work; just update the variable name if it was `after_join`).

Also update the `is_aggregated` / sort / project path — it currently uses `join` for some conditions; update to `joins <> []`.

The no-catalog aggregate join path (around lines 256–292) had:
```ocaml
       let after_join : Plan.op = match join with | None -> base | Some bj -> ...
```
Replace entirely with the fold shown above.

- [ ] **Step 8: Build and run tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev sh -c 'dune build 2>&1 && dune runtest 2>&1 | tail -30'
```

Expected: all `multi_join` tests pass; no regressions in `joins`, `group_by`, or existing single-join tests.

- [ ] **Step 9: Commit**

```bash
git add lib/sql/sema.ml lib/sql/sema.mli lib/sql/planner.ml test/test_e2e.ml
git commit -m "feat(phase10-task2): multiple JOINs per SELECT (generalise from 1 to N)"
```

---

## Task 3: SQLite comparison tests + coverage boost

**Files:**
- Modify: `test/test_e2e.ml` (edge cases)
- Modify: `test/test_sqlite_compare.ml` (new phase10 groups)

- [ ] **Step 1: Add phase10 comparison tests to `test/test_sqlite_compare.ml`**

Find the pattern used for `phase9_subquery_cases` and add two new test-case lists at the bottom of the file.

```ocaml
(* ── phase10_case_when comparison cases ───────────────────────── *)

let phase10_case_when_cases = [
  { name    = "searched_basic";
    setup   = ["CREATE TABLE t (v INTEGER)";
               "INSERT INTO t VALUES (3)";
               "INSERT INTO t VALUES (-1)";
               "INSERT INTO t VALUES (0)"];
    queries = ["SELECT CASE WHEN v > 0 THEN 'pos' WHEN v < 0 THEN 'neg' ELSE 'zero' END FROM t ORDER BY v"] };

  { name    = "simple_basic";
    setup   = ["CREATE TABLE t (v INTEGER)";
               "INSERT INTO t VALUES (1)";
               "INSERT INTO t VALUES (2)";
               "INSERT INTO t VALUES (99)"];
    queries = ["SELECT CASE v WHEN 1 THEN 'one' WHEN 2 THEN 'two' ELSE 'other' END FROM t ORDER BY v"] };

  { name    = "no_else_no_match";
    setup   = ["CREATE TABLE t (v INTEGER)"; "INSERT INTO t VALUES (5)"];
    queries = ["SELECT CASE WHEN v = 0 THEN 'zero' END FROM t"] };

  { name    = "case_in_where";
    setup   = ["CREATE TABLE t (v INTEGER)";
               "INSERT INTO t VALUES (1)";
               "INSERT INTO t VALUES (2)";
               "INSERT INTO t VALUES (3)"];
    queries = ["SELECT v FROM t WHERE CASE WHEN v > 1 THEN 1 ELSE 0 END = 1 ORDER BY v"] };

  { name    = "case_arithmetic_result";
    setup   = ["CREATE TABLE t (v INTEGER)"; "INSERT INTO t VALUES (4)"];
    queries = ["SELECT CASE WHEN v > 2 THEN v * 10 ELSE v END FROM t"] };

  { name    = "case_with_null";
    setup   = ["CREATE TABLE t (v INTEGER)"; "INSERT INTO t VALUES (NULL)"];
    queries = ["SELECT CASE WHEN v IS NULL THEN 'is_null' ELSE 'not_null' END FROM t"] };

  { name    = "case_simple_null_scrutinee";
    setup   = ["CREATE TABLE t (v INTEGER)"; "INSERT INTO t VALUES (NULL)"];
    queries = ["SELECT CASE v WHEN NULL THEN 1 ELSE 0 END FROM t"] };

  { name    = "case_in_select_and_where";
    setup   = ["CREATE TABLE t (n INTEGER, s TEXT)";
               "INSERT INTO t VALUES (1, 'a')";
               "INSERT INTO t VALUES (2, 'b')"];
    queries = ["SELECT CASE n WHEN 1 THEN 'first' ELSE 'rest' END, s FROM t ORDER BY n"] };
]

(* ── phase10_multi_join comparison cases ─────────────────────── *)

let phase10_multi_join_cases = [
  { name    = "three_table_join";
    setup   = ["CREATE TABLE a (id INTEGER, name TEXT)";
               "CREATE TABLE b (aid INTEGER, val INTEGER)";
               "CREATE TABLE c (bid INTEGER, extra TEXT)";
               "INSERT INTO a VALUES (1, 'alice')";
               "INSERT INTO b VALUES (1, 42)";
               "INSERT INTO c VALUES (42, 'extra')"];
    queries = ["SELECT a.name, b.val, c.extra FROM a JOIN b ON a.id = b.aid JOIN c ON b.val = c.bid"] };

  { name    = "two_joins_filtered";
    setup   = ["CREATE TABLE u (id INTEGER, name TEXT)";
               "CREATE TABLE o (uid INTEGER, item TEXT)";
               "CREATE TABLE p (item TEXT, price INTEGER)";
               "INSERT INTO u VALUES (1, 'alice'), (2, 'bob')";
               "INSERT INTO o VALUES (1, 'hat'), (2, 'book')";
               "INSERT INTO p VALUES ('hat', 10), ('book', 5)"];
    queries = ["SELECT u.name, p.price FROM u JOIN o ON u.id = o.uid JOIN p ON o.item = p.item ORDER BY u.name"] };

  { name    = "two_left_joins";
    setup   = ["CREATE TABLE a (id INTEGER)";
               "CREATE TABLE b (aid INTEGER, v TEXT)";
               "CREATE TABLE c (aid INTEGER, w TEXT)";
               "INSERT INTO a VALUES (1), (2)";
               "INSERT INTO b VALUES (1, 'B1')";
               "INSERT INTO c VALUES (2, 'C2')"];
    queries = ["SELECT a.id, b.v, c.w FROM a LEFT JOIN b ON a.id = b.aid LEFT JOIN c ON a.id = c.aid ORDER BY a.id"] };

  { name    = "three_join_aggregate";
    setup   = ["CREATE TABLE dept (id INTEGER, name TEXT)";
               "CREATE TABLE emp (id INTEGER, dept_id INTEGER, name TEXT)";
               "CREATE TABLE sal (emp_id INTEGER, amount INTEGER)";
               "INSERT INTO dept VALUES (1, 'eng'), (2, 'sales')";
               "INSERT INTO emp VALUES (1, 1, 'alice'), (2, 1, 'bob'), (3, 2, 'carol')";
               "INSERT INTO sal VALUES (1, 100), (2, 90), (3, 80)"];
    queries = ["SELECT dept.name, COUNT(emp.id) FROM dept JOIN emp ON dept.id = emp.dept_id JOIN sal ON emp.id = sal.emp_id GROUP BY dept.name ORDER BY dept.name"] };

  { name    = "star_projection";
    setup   = ["CREATE TABLE x (a INTEGER)";
               "CREATE TABLE y (b INTEGER)";
               "CREATE TABLE z (c INTEGER)";
               "INSERT INTO x VALUES (1)";
               "INSERT INTO y VALUES (2)";
               "INSERT INTO z VALUES (3)"];
    queries = ["SELECT * FROM x JOIN y ON 1=1 JOIN z ON 1=1"] };

  { name    = "case_in_multi_join";
    setup   = ["CREATE TABLE a (id INTEGER)";
               "CREATE TABLE b (aid INTEGER, v INTEGER)";
               "INSERT INTO a VALUES (1), (2)";
               "INSERT INTO b VALUES (1, 10), (2, 20)"];
    queries = ["SELECT a.id, CASE WHEN b.v > 15 THEN 'big' ELSE 'small' END FROM a JOIN b ON a.id = b.aid ORDER BY a.id"] };
]
```

Wire them into the test runner at the bottom where `phase9_*` groups are registered:

```ocaml
    "phase10_case_when", List.map make_test phase10_case_when_cases;
    "phase10_multi_join", List.map make_test phase10_multi_join_cases;
```

- [ ] **Step 2: Add edge-case tests to `test/test_e2e.ml`**

Add a `"phase10_edge"` group with the following tests:

```ocaml
(* ── Phase 10 edge cases ─────────────────────────────────────────── *)

let test_case_in_order_by () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (v INTEGER)";
  exec db "INSERT INTO t VALUES (1)";
  exec db "INSERT INTO t VALUES (2)";
  exec db "INSERT INTO t VALUES (3)";
  let rows = query_ok db
    "SELECT v FROM t ORDER BY CASE v WHEN 1 THEN 3 WHEN 2 THEN 1 ELSE 2 END" in
  let vals = List.map (fun r -> match r.(0) with Db.V_int n -> Int64.to_int n | _ -> -1) rows in
  Alcotest.(check (list int)) "case_order" [2; 3; 1] vals

let test_case_constant () =
  (* CASE without FROM — const select *)
  let db = fresh_db () in
  exec db "CREATE TABLE t (v INTEGER)";
  exec db "INSERT INTO t VALUES (5)";
  let rows = query_ok db "SELECT CASE WHEN 1 = 1 THEN 42 END FROM t" in
  let vals = List.map (fun r -> match r.(0) with Db.V_int n -> Int64.to_int n | _ -> -1) rows in
  Alcotest.(check (list int)) "const" [42] vals

let test_case_string_comparison () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (s TEXT)";
  exec db "INSERT INTO t VALUES ('hello')";
  exec db "INSERT INTO t VALUES ('world')";
  let rows = query_ok db
    "SELECT CASE s WHEN 'hello' THEN 1 ELSE 0 END FROM t ORDER BY s" in
  let vals = List.map (fun r -> match r.(0) with Db.V_int n -> Int64.to_int n | _ -> -1) rows in
  Alcotest.(check (list int)) "string_case" [1; 0] vals

let test_four_table_join () =
  let db = fresh_db () in
  exec db "CREATE TABLE a (id INTEGER)";
  exec db "CREATE TABLE b (aid INTEGER, bid INTEGER)";
  exec db "CREATE TABLE c (bid INTEGER, cid INTEGER)";
  exec db "CREATE TABLE d (cid INTEGER, v TEXT)";
  exec db "INSERT INTO a VALUES (1)";
  exec db "INSERT INTO b VALUES (1, 2)";
  exec db "INSERT INTO c VALUES (2, 3)";
  exec db "INSERT INTO d VALUES (3, 'found')";
  let rows = query_ok db
    "SELECT d.v FROM a JOIN b ON a.id = b.aid JOIN c ON b.bid = c.bid JOIN d ON c.cid = d.cid" in
  Alcotest.(check int) "one row" 1 (List.length rows);
  Alcotest.(check string) "v" "found" (match (List.hd rows).(0) with Db.V_text s -> s | _ -> "?")

let test_multi_join_order_by () =
  let db = fresh_db () in
  exec db "CREATE TABLE a (id INTEGER)";
  exec db "CREATE TABLE b (aid INTEGER, v INTEGER)";
  exec db "CREATE TABLE c (bid INTEGER, w INTEGER)";
  exec db "INSERT INTO a VALUES (1), (2)";
  exec db "INSERT INTO b VALUES (1, 10), (2, 20)";
  exec db "INSERT INTO c VALUES (10, 100), (20, 200)";
  let rows = query_ok db
    "SELECT a.id, c.w FROM a JOIN b ON a.id = b.aid JOIN c ON b.v = c.bid ORDER BY a.id DESC" in
  let vals = List.map (fun r -> match r.(0) with Db.V_int n -> Int64.to_int n | _ -> -1) rows in
  Alcotest.(check (list int)) "order" [2; 1] vals

let test_case_with_func () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (s TEXT)";
  exec db "INSERT INTO t VALUES ('Hello')";
  let rows = query_ok db
    "SELECT CASE WHEN LENGTH(s) > 3 THEN UPPER(s) ELSE s END FROM t" in
  let labels = List.map (fun r -> match r.(0) with Db.V_text s -> s | _ -> "?") rows in
  Alcotest.(check (list string)) "case_func" ["HELLO"] labels
```

Wire into the test runner:

```ocaml
"phase10_edge", [
  Alcotest.test_case "case_in_order_by"      `Quick test_case_in_order_by;
  Alcotest.test_case "case_constant"         `Quick test_case_constant;
  Alcotest.test_case "case_string_comparison"`Quick test_case_string_comparison;
  Alcotest.test_case "four_table_join"       `Quick test_four_table_join;
  Alcotest.test_case "multi_join_order_by"   `Quick test_multi_join_order_by;
  Alcotest.test_case "case_with_func"        `Quick test_case_with_func;
];
```

- [ ] **Step 3: Run full test suite**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | tail -30
```

Expected: all phase10 groups pass; zero regressions.

- [ ] **Step 4: Check coverage**

```bash
./scripts/coverage.sh 2>&1 | grep -E "exec|sema|planner|catalog"
```

Coverage targets: exec.ml ≥ 88%, sema.ml ≥ 80%, planner.ml ≥ 85%.

- [ ] **Step 5: Commit**

```bash
git add test/test_e2e.ml test/test_sqlite_compare.ml
git commit -m "test(phase10): SQLite comparison tests + edge cases for CASE WHEN and multi-join"
```

---

## Self-Review

### Spec coverage

| Feature | Task |
|---------|------|
| Searched CASE WHEN | Task 1 |
| Simple CASE expr WHEN | Task 1 |
| CASE in WHERE / ORDER BY | Task 3 edge tests |
| Nested CASE | Task 1 test |
| Multiple (N≥3) JOINs | Task 2 |
| Multiple LEFT JOINs | Task 2 |
| `SELECT *` with multi-join | Task 2 test |
| CASE + multi-join together | Task 3 comparison |
| SQLite parity testing | Task 3 |

### Notes for implementer

- `bind_expr_join` is called in ~20 places inside `bind_select`; do a global search for `~left_meta` after your changes to confirm no stragglers.
- The "no catalog" path in `plan`'s `BS_select` is exercised by `test_sema.ml` unit tests — make sure the fold logic there matches the catalog path.
- `between_bound` and `expr` are separate non-terminals in the parser; add `case_expr` to both so CASE can appear inside BETWEEN ranges.
- Menhir conflict check: if you see "1 shift/reduce conflict", read the conflict description — it almost certainly involves WHEN appearing in an unexpected position.  The fix is to ensure WHEN is not in FOLLOW(expr) except in the CASE context.
- Do not change the `E_case` `scrutinee` type from `expr option` — both None (searched) and Some (simple) use the same constructor.
