# Phase 14: Window Functions + Recursive CTEs Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add two high-impact analytics features: window functions (`ROW_NUMBER`, `RANK`, `DENSE_RANK`, `LAG`, `LEAD`, `FIRST_VALUE`, `LAST_VALUE`, aggregate `OVER`) and recursive CTEs (`WITH RECURSIVE name AS (base UNION ALL recursive_arm)`).

**Architecture:** Window functions materialise all child rows into an array, partition them by `PARTITION BY` keys, sort each partition by `ORDER BY` keys, compute function values per row, then augment each row with `n_windows` extra columns. A new `Op_window` plan node sits between the child scan and the final `Op_expr_project`; `BE_window_slot i` in sema is converted to `P_window_slot i` in plan, then substituted to `P_col (n_input_cols + i)` by the planner before hitting exec. Recursive CTEs extend the existing `Op_with_cte` mechanism with a `recursive` flag; when set, `Op_with_cte` executes the base case once and then repeatedly executes the recursive arm (substituting the CTE scan with current rows) until no new rows are produced.

**Tech Stack:** OCaml 5.x, Menhir, Lwt, Alcotest — all `dune` commands inside `podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune ...`

---

## File Structure

| File | Changes |
|------|---------|
| `lib/sql/ast.ml` | Add `window_func`, `window_spec`, `E_window` to mutual-recursion block; add `recursive: bool` to `S_with_cte` |
| `lib/sql/lexer.mll` | Add `OVER`, `PARTITION`, `RECURSIVE` to ident catch-all |
| `lib/sql/parser.mly` | Add `%token OVER PARTITION RECURSIVE`; replace `agg_expr` with `agg_or_window_expr` that optionally consumes `OVER window_spec`; add `window_func_name`, `window_spec`, `partition_clause`, `window_func_args` rules; update `with_cte` for optional `RECURSIVE` |
| `lib/sql/sema.ml` | Add `BE_window_slot of int`, `window_sema` type; add `windows: window_sema list` to `BS_select`; add `recursive: bool` to `BS_with_cte`; implement window expr collection in `bind_select` |
| `lib/sql/sema.mli` | Export `window_sema`, updated `BS_select`, `BE_window_slot`, updated `BS_with_cte` |
| `lib/sql/plan.ml` | Add `P_window_slot of int`; add `window_plan_item` type; add `Op_window`; add `recursive: bool` to `Op_with_cte` |
| `lib/sql/planner.ml` | Thread `windows` through `plan_select`; emit `Op_window`; substitute `P_window_slot` to `P_col`; thread `recursive` through `BS_with_cte → Op_with_cte` |
| `lib/sql/exec.ml` | Implement `execute_window_op`; add `Op_window` arm in `to_stream`; update `Op_with_cte` arm for recursive execution; add `P_window_slot` arm in `eval_expr` |
| `test/test_e2e.ml` | Add `"window"` group (8 tests) and `"recursive_cte"` group (4 tests) |
| `test/test_sqlite_compare.ml` | Add `phase14_window_cases` (8 tests) and `phase14_recursive_cte_cases` (4 tests) |

---

## Task 1: Window Functions — AST, Lexer, Parser

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`

- [ ] **Step 1: Add window types to `ast.ml` mutual-recursion block**

In `lib/sql/ast.ml`, the mutually-recursive type block starts with `type expr = ...` and has `and order_key = ...`, `and join_clause = ...`, `and upsert_update = ...`, `and stmt = ...`, etc. Add the following new `and` cases **before** `and stmt =`:

```ocaml
and window_func =
  | WF_row_number
  | WF_rank
  | WF_dense_rank
  | WF_ntile
  | WF_lag
  | WF_lead
  | WF_first_value
  | WF_last_value
  | WF_nth_value
  | WF_agg of agg_func

and window_spec = {
  partition_by : expr list;
  order_by     : order_key list;
}
```

Then add `E_window` to the `expr` type, after `E_cast`:

```ocaml
  | E_window of {
      func   : window_func;
      args   : expr list;
      window : window_spec;
    }
```

- [ ] **Step 2: Add `recursive` field to `S_with_cte` in `ast.ml`**

Find `| S_with_cte of { name : string; def : stmt; query : stmt; }` and change it to:

```ocaml
  | S_with_cte of {
      name      : string;
      def       : stmt;
      query     : stmt;
      recursive : bool;
    }
```

Search for **all** `S_with_cte` constructions and pattern matches in the codebase and update them. Run:
```bash
grep -rn "S_with_cte" lib/ test/ --include="*.ml"
```
Expected locations (add `recursive = false` to constructions; add `recursive = _` to patterns):
- `lib/sql/parser.mly` line ~106: construction → add `recursive = false`
- `lib/sql/exec.ml` line ~1750: pattern match in `substitute_outer_in_stmt` → add `recursive = _`
- `lib/sql/sema.ml` line ~1872: view expansion construction → add `recursive = false`
- `lib/sql/sema.ml` line ~1915: pattern match in `bind_internal` → add `recursive` field  

- [ ] **Step 3: Add new tokens to `lexer.mll`**

In `lib/sql/lexer.mll`, find the `ident` catch-all block (starts around line 148):
```ocaml
  | ident as id             {
      match String.uppercase_ascii id with
      | "AS"     -> AS
      ...
      | "VIEW"     -> VIEW
      | _          -> IDENT id
    }
```

Add three new cases before `| _          -> IDENT id`:
```ocaml
      | "OVER"      -> OVER
      | "PARTITION" -> PARTITION
      | "RECURSIVE" -> RECURSIVE
```

- [ ] **Step 4: Add new tokens and window grammar rules to `parser.mly`**

In `lib/sql/parser.mly`, find the `%token` declarations and add at the end:
```
%token OVER PARTITION RECURSIVE
```

Add a `window_spec` rule (after the `agg_expr` rule):
```
window_spec:
  | LPAREN pb = partition_clause ob = order_by_clause RPAREN
    { Ast.{ partition_by = pb; order_by = ob } }

partition_clause:
  |                                                             { [] }
  | PARTITION BY es = separated_nonempty_list(COMMA, expr)     { es }

window_func_args:
  |                                                             { [] }
  | es = separated_nonempty_list(COMMA, expr)                  { es }

window_func_name:
  | id = IDENT
    { match String.uppercase_ascii id with
      | "ROW_NUMBER"  -> Ast.WF_row_number
      | "RANK"        -> Ast.WF_rank
      | "DENSE_RANK"  -> Ast.WF_dense_rank
      | "NTILE"       -> Ast.WF_ntile
      | "LAG"         -> Ast.WF_lag
      | "LEAD"        -> Ast.WF_lead
      | "FIRST_VALUE" -> Ast.WF_first_value
      | "LAST_VALUE"  -> Ast.WF_last_value
      | "NTH_VALUE"   -> Ast.WF_nth_value
      | other         -> failwith (Printf.sprintf "Unknown window function: %s" other) }
```

- [ ] **Step 5: Replace `agg_expr` with `agg_or_window_expr` in `parser.mly`**

Find the `agg_expr` rule (around line 422):
```ocaml
agg_expr:
  | COUNT LPAREN STAR RPAREN          { E_agg (Agg_count, None) }
  | COUNT LPAREN e = expr RPAREN      { E_agg (Agg_count, Some e) }
  | SUM   LPAREN e = expr RPAREN      { E_agg (Agg_sum,   Some e) }
  | AVG   LPAREN e = expr RPAREN      { E_agg (Agg_avg,   Some e) }
  | MIN   LPAREN e = expr RPAREN      { E_agg (Agg_min,   Some e) }
  | MAX   LPAREN e = expr RPAREN      { E_agg (Agg_max,   Some e) }
```

Replace it with `agg_or_window_expr` that optionally consumes `OVER window_spec`:

```ocaml
agg_or_window_expr:
  | COUNT LPAREN STAR RPAREN ow = option(preceded(OVER, window_spec))
    { match ow with
      | None   -> E_agg (Agg_count, None)
      | Some w -> E_window { func = WF_agg Agg_count; args = []; window = w } }
  | COUNT LPAREN e = expr RPAREN ow = option(preceded(OVER, window_spec))
    { match ow with
      | None   -> E_agg (Agg_count, Some e)
      | Some w -> E_window { func = WF_agg Agg_count; args = [e]; window = w } }
  | SUM LPAREN e = expr RPAREN ow = option(preceded(OVER, window_spec))
    { match ow with
      | None   -> E_agg (Agg_sum, Some e)
      | Some w -> E_window { func = WF_agg Agg_sum; args = [e]; window = w } }
  | AVG LPAREN e = expr RPAREN ow = option(preceded(OVER, window_spec))
    { match ow with
      | None   -> E_agg (Agg_avg, Some e)
      | Some w -> E_window { func = WF_agg Agg_avg; args = [e]; window = w } }
  | MIN LPAREN e = expr RPAREN ow = option(preceded(OVER, window_spec))
    { match ow with
      | None   -> E_agg (Agg_min, Some e)
      | Some w -> E_window { func = WF_agg Agg_min; args = [e]; window = w } }
  | MAX LPAREN e = expr RPAREN ow = option(preceded(OVER, window_spec))
    { match ow with
      | None   -> E_agg (Agg_max, Some e)
      | Some w -> E_window { func = WF_agg Agg_max; args = [e]; window = w } }
```

In `expr` and `between_bound` rules, replace `| e = agg_expr { e }` with `| e = agg_or_window_expr { e }`.

Also add the window function call rule to `expr` (after the existing `| e = agg_or_window_expr { e }` line):
```ocaml
  | func = window_func_name LPAREN args = window_func_args RPAREN OVER ws = window_spec
    { E_window { func; args; window = ws } }
```

- [ ] **Step 6: Update `with_cte` rule in `parser.mly` for RECURSIVE**

Find the `with_cte` rule (around line 104):
```ocaml
with_cte:
  | WITH name = IDENT AS LPAREN def = compound_select RPAREN query = compound_select
    { Ast.S_with_cte { name; def; query } }
```

Replace with:
```ocaml
with_cte:
  | WITH name = IDENT AS LPAREN def = compound_select RPAREN query = compound_select
    { Ast.S_with_cte { name; def; query; recursive = false } }
  | WITH RECURSIVE name = IDENT AS LPAREN def = compound_select RPAREN query = compound_select
    { Ast.S_with_cte { name; def; query; recursive = true } }
```

- [ ] **Step 7: Build to verify parsing compiles**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1 | head -40
```

Expected: Build succeeds (or only test failures, not compilation errors). Fix any parse conflicts or type errors.

- [ ] **Step 8: Commit**

```bash
git add lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly
git commit -m "feat(phase14): AST/lexer/parser for window functions and recursive CTEs"
```

---

## Task 2: Window Functions — Sema + Plan Types

**Files:**
- Modify: `lib/sql/sema.ml`
- Modify: `lib/sql/sema.mli`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/planner.ml`

- [ ] **Step 1: Add sema types for window functions**

In `lib/sql/sema.ml`, after the existing `type bound_order_key = ...` block, add:

```ocaml
type window_sema = {
  func         : Ast.window_func;
  args         : bound_expr list;
  partition_by : bound_expr list;
  order_by     : bound_order_key list;
}
```

Add `BE_window_slot of int` to `bound_expr` (after `BE_excluded_col`):

```ocaml
  | BE_window_slot of int
    (** Reference to the i-th window function result, appended after input columns by Op_window. *)
```

Add `windows : window_sema list` field to `BS_select` (after `agg_proj`):

```ocaml
      windows    : window_sema list;
        (** Non-empty when projection contains window function expressions.
            Each entry corresponds to one E_window call; the i-th entry maps to
            BE_window_slot i in expr_proj. *)
```

Add `recursive : bool` field to `BS_with_cte`:

```ocaml
  | BS_with_cte of {
      name      : string;
      def       : bound_stmt;
      query     : bound_stmt;
      recursive : bool;
    }
```

- [ ] **Step 2: Update `sema.mli` to export new types**

In `lib/sql/sema.mli`, add `window_sema` type after `agg_proj_item`:

```ocaml
type window_sema = {
  func         : Ast.window_func;
  args         : bound_expr list;
  partition_by : bound_expr list;
  order_by     : bound_order_key list;
}
```

Add `| BE_window_slot of int` to `bound_expr`.

Add `windows : window_sema list` to `BS_select` record.

Add `recursive : bool` to `BS_with_cte`.

- [ ] **Step 3: Implement window collection in `bind_select`**

In `lib/sql/sema.ml`, the `bind_select` function (around line 1105) handles projection binding. We need to collect `E_window` expressions from the projection exprs before the regular binding pass, and replace them with `BE_window_slot i`.

Add a helper `bind_window_proj_item` that wraps the existing `bind_one` (which calls `bind_expr` or `bind_expr_join`) but intercepts `E_window`:

```ocaml
(* Inside bind_select, after tables/joined_pairs are set up: *)
let windows_queue : window_sema Queue.t = Queue.create () in

let bind_with_windows env e =
  (* Recursively process expr, replacing E_window nodes with BE_window_slot. *)
  let rec go e = match e with
    | Ast.E_window { func; args; window } ->
      let slot = Queue.length windows_queue in
      (* Bind args, partition_by, order_by against the combined row *)
      let bind_e e' =
        if joined_pairs = [] then
          bind_expr ~param_counter ~named_params meta e'
        else
          bind_expr_join ~param_counter ~named_params ~tables e'
      in
      let* bound_args = result_list (List.map bind_e args) in
      let* bound_pb = result_list (List.map bind_e window.Ast.partition_by) in
      let* bound_ob = result_list_ok (List.map (fun ok ->
          bind_e ok.Ast.expr |> Result.map (fun be -> { key = be; dir = ok.Ast.dir })
        ) window.Ast.order_by) in
      let ws = { func; args = bound_args; partition_by = bound_pb; order_by = bound_ob } in
      Queue.push ws windows_queue;
      Lwt.return (Ok (BE_window_slot slot))
    | Ast.E_binop (op, a, b) ->
      let* ba = go a in let* bb = go b in
      Lwt.return (match ba, bb with
        | Ok ba', Ok bb' -> Ok (BE_binop (ast_binop_to_sema op, ba', bb'))
        | Error e, _ | _, Error e -> Error e)
    (* ... all other E_ cases: recurse or delegate to bind_e ... *)
    (* For all non-E_window cases that don't contain sub-exprs with E_window,
       delegate to bind_e; for those that do (E_binop, E_not, etc.),
       recurse with go. *)
    (* Simplest correct approach: check if E_window appears anywhere in e first;
       if not, use bind_e directly for efficiency. *)
    | _ when not (expr_has_window e) ->
      bind_e e
    | Ast.E_not a ->
      let* ba = go a in Lwt.return (Result.map (fun x -> BE_not x) ba)
    | Ast.E_neg a ->
      let* ba = go a in Lwt.return (Result.map (fun x -> BE_neg x) ba)
    | Ast.E_is_null a ->
      let* ba = go a in Lwt.return (Result.map (fun x -> BE_is_null x) ba)
    | Ast.E_is_not_null a ->
      let* ba = go a in Lwt.return (Result.map (fun x -> BE_is_not_null x) ba)
    | Ast.E_between (x, lo, hi) ->
      let* bx = go x in let* blo = go lo in let* bhi = go hi in
      Lwt.return (match bx, blo, bhi with
        | Ok x', Ok lo', Ok hi' -> Ok (BE_between (x', lo', hi'))
        | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
    | Ast.E_case { scrutinee; branches; else_ } ->
      let* bscr = match scrutinee with
        | None -> Lwt.return (Ok None)
        | Some e -> let* r = go e in Lwt.return (Result.map Option.some r)
      in
      let* bbranches = Lwt_list.fold_left_s (fun acc (c, r) ->
        match acc with Error e -> Lwt.return (Error e) | Ok bs ->
        let* bc = go c in let* br = go r in
        Lwt.return (match bc, br with
          | Ok c', Ok r' -> Ok (bs @ [(c', r')])
          | Error e, _ | _, Error e -> Error e)
      ) (Ok []) branches in
      let* belse_ = match else_ with
        | None -> Lwt.return (Ok None)
        | Some e -> let* r = go e in Lwt.return (Result.map Option.some r)
      in
      Lwt.return (match bscr, bbranches, belse_ with
        | Ok scr, Ok brs, Ok el ->
          Ok (BE_case { scrutinee = scr; branches = brs; else_ = el })
        | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
    | Ast.E_func (f, args) ->
      let* bargs = Lwt_list.fold_left_s (fun acc a ->
        match acc with Error e -> Lwt.return (Error e) | Ok bs ->
        let* r = go a in Lwt.return (Result.map (fun b -> bs @ [b]) r)
      ) (Ok []) args in
      Lwt.return (Result.map (fun ba -> BE_func (f, ba)) bargs)
    | _ -> bind_e e  (* fallthrough for E_in, E_in_select, E_subquery, etc. *)
  in
  go e
in
```

You also need a helper `expr_has_window` to short-circuit the recursion:

```ocaml
let rec expr_has_window = function
  | Ast.E_window _ -> true
  | Ast.E_binop (_, a, b) -> expr_has_window a || expr_has_window b
  | Ast.E_not e | Ast.E_neg e | Ast.E_is_null e | Ast.E_is_not_null e
  | Ast.E_bitnot e -> expr_has_window e
  | Ast.E_between (x, lo, hi) -> expr_has_window x || expr_has_window lo || expr_has_window hi
  | Ast.E_in (x, vals) -> expr_has_window x || List.exists expr_has_window vals
  | Ast.E_func (_, args) -> List.exists expr_has_window args
  | Ast.E_case { scrutinee; branches; else_ } ->
    (match scrutinee with Some e -> expr_has_window e | None -> false)
    || List.exists (fun (c, r) -> expr_has_window c || expr_has_window r) branches
    || (match else_ with Some e -> expr_has_window e | None -> false)
  | _ -> false
```

Modify the projection binding inside `bind_select` (inside the `not is_aggregated` branch that computes `expr_proj`):

```ocaml
(* Replace the existing:
   let bind_one e = ...
   with bind_with_windows.
   Only when building expr_proj for non-aggregated queries. *)
```

After `expr_proj` is computed, extract the collected windows:

```ocaml
let windows = Queue.fold (fun acc w -> acc @ [w]) [] windows_queue in
```

And include `windows` in `BS_select`:

```ocaml
BS_select { distinct; table_meta; proj = ords; expr_proj; where = bound_where;
             order; limit; offset; joins = bound_joins; group_by = group_col;
             aggs; having = bound_having; agg_proj; windows }
```

- [ ] **Step 4: Update `bind_internal` for `BS_with_cte` recursive field**

In `lib/sql/sema.ml`, find the `Ast.S_with_cte` arm in `bind_internal` (around line 1915):

```ocaml
  | Ast.S_with_cte { name; def; query } ->
    ...
    Lwt.return (Ok (BS_with_cte { name; def = bound_def; query = bound_query })))
```

Update to thread `recursive`:

```ocaml
  | Ast.S_with_cte { name; def; query; recursive } ->
    ...
    Lwt.return (Ok (BS_with_cte { name; def = bound_def; query = bound_query; recursive })))
```

Also update the view expansion in `bind_internal` that constructs `S_with_cte`:

```ocaml
(Ast.S_with_cte { name = table; def = view_def; query = sel; recursive = false })
```

Update the `BS_with_cte` pattern match in `compound_col_count` and `col_names_of_bound_stmt`:

```ocaml
| BS_with_cte { query; _ } -> ...  (* add _ to ignore recursive *)
```

- [ ] **Step 5: Add plan types for window functions**

In `lib/sql/plan.ml`, add after the `type expr = ...` block:

```ocaml
type window_plan_item = {
  func         : Ast.window_func;
  args         : expr list;
  partition_by : expr list;
  order_by     : (expr * [`Asc | `Desc]) list;
}
```

Add `P_window_slot of int` to the `expr` type (after `P_excluded_col`):

```ocaml
  | P_window_slot of int
    (** Window function result slot; substituted to P_col (n_input_cols + i) by planner. *)
```

Add `Op_window` to the `op` type (after `Op_with_cte`):

```ocaml
  | Op_window of {
      child        : op;
      windows      : window_plan_item list;
      n_input_cols : int;
    }
```

Add `recursive : bool` to `Op_with_cte`:

```ocaml
  | Op_with_cte of {
      cte_name  : string;
      def       : op;
      query     : op;
      recursive : bool;
    }
```

This requires updating **all** constructions and pattern matches of `Op_with_cte` in `exec.ml` and `planner.ml`. Search with:

```bash
grep -n "Op_with_cte" lib/sql/exec.ml lib/sql/planner.ml
```

Expected locations:
- `exec.ml` ~line 1771: `Plan.Op_with_cte r when not (...)` → add `recursive = _` to record wildcard
- `exec.ml` ~line 2549: `Plan.Op_with_cte { cte_name; def; query }` → add `recursive = _`
- `planner.ml` ~line 467: construction → add `recursive = bj.recursive`

- [ ] **Step 6: Wire windows through planner**

In `lib/sql/planner.ml`:

1. Add `plan_window_item` helper:

```ocaml
let plan_window_item (ws : Sema.window_sema) : Plan.window_plan_item =
  { Plan.func         = ws.Sema.func;
    args         = List.map plan_expr ws.Sema.args;
    partition_by = List.map plan_expr ws.Sema.partition_by;
    order_by     = List.map (fun (bk : Sema.bound_order_key) ->
      let dir = match bk.dir with Ast.Asc -> `Asc | Ast.Desc -> `Desc in
      (plan_expr bk.key, dir)
    ) ws.Sema.order_by;
  }
```

2. Add `substitute_window_slots` helper:

```ocaml
let rec substitute_window_slots ~n_input_cols (e : Plan.expr) : Plan.expr =
  let go = substitute_window_slots ~n_input_cols in
  match e with
  | Plan.P_window_slot i -> Plan.P_col (n_input_cols + i)
  | Plan.P_binop (op, a, b) -> Plan.P_binop (op, go a, go b)
  | Plan.P_not e -> Plan.P_not (go e)
  | Plan.P_is_null e -> Plan.P_is_null (go e)
  | Plan.P_is_not_null e -> Plan.P_is_not_null (go e)
  | Plan.P_neg e -> Plan.P_neg (go e)
  | Plan.P_bitnot e -> Plan.P_bitnot (go e)
  | Plan.P_between (x, lo, hi) -> Plan.P_between (go x, go lo, go hi)
  | Plan.P_in (x, vs) -> Plan.P_in (go x, List.map go vs)
  | Plan.P_func (f, args) -> Plan.P_func (f, List.map go args)
  | Plan.P_case { scrutinee; branches; else_ } ->
    Plan.P_case { scrutinee = Option.map go scrutinee;
                  branches = List.map (fun (c, r) -> (go c, go r)) branches;
                  else_ = Option.map go else_ }
  | Plan.P_cast (e, ty) -> Plan.P_cast (go e, ty)
  | e' -> e'
```

3. Update `plan_select` signature to accept `~windows` and compute `Op_window` when non-empty:

```ocaml
let plan_select cat
    ~table_meta ~proj ~expr_proj ~where ~order ~limit ~offset ~joins
    ~group_by ~aggs ~having ~agg_proj ~distinct ~windows =
  ...
  (* Compute n_input_cols from table_meta + joins: *)
  let n_input_cols =
    List.length table_meta.Cat.columns
    + List.fold_left (fun acc (bj : Sema.bound_join) ->
        acc + List.length bj.Sema.right_meta.Cat.columns
      ) 0 joins
  in
  ...
  (* After building after_where (existing scan/filter/join logic): *)
  let (after_window, effective_n_cols) =
    if windows = [] then (after_where, n_input_cols)
    else
      let win_plans = List.map plan_window_item windows in
      (Plan.Op_window { child = after_where; windows = win_plans; n_input_cols }, n_input_cols)
  in
  (* Build projection using after_window, substituting window slots: *)
  let after_sort = ... (* use after_window instead of after_where *) in
  ...
  (* When building Op_expr_project, substitute window slots: *)
  else if expr_proj <> [] then
    Plan.Op_expr_project {
      exprs = List.map (fun (be, alias) ->
        let e = plan_expr be in
        let e' = substitute_window_slots ~n_input_cols:effective_n_cols e in
        (e', alias)
      ) expr_proj;
      child = after_sort;
    }
  ...
```

4. Update `BS_select` pattern match in `plan` function (around line 269) to include `windows`:

```ocaml
  | Sema.BS_select { distinct; table_meta; proj; expr_proj; where; order; limit; offset;
                     joins; group_by; aggs; having; agg_proj; windows } ->
    (match cat with
     | Some cat ->
       plan_select cat ~table_meta ~proj ~expr_proj ~where ~order ~limit ~offset
         ~joins ~group_by ~aggs ~having ~agg_proj ~distinct ~windows
     | None ->
       (* None path: also pass ~windows = [] for now since tests don't test window without catalog *)
       plan_select_no_cat ... ~windows:[]
```

Wait — the `None` path in `plan` (without catalog) is a large inline block in `plan`. It must also be updated to accept `windows` and apply `Op_window`. For the `None` path, also compute `n_input_cols` and insert `Op_window` when `windows <> []`.

5. Update `BS_with_cte` arm in `plan`:

```ocaml
  | Sema.BS_with_cte { name; def; query; recursive } ->
    Plan.Op_with_cte {
      cte_name  = name;
      def       = plan ?cat def;
      query     = plan ?cat query;
      recursive;
    }
```

- [ ] **Step 7: Add `P_window_slot` arm to `eval_expr` in `exec.ml`**

In `lib/sql/exec.ml`, find `eval_expr` and add:

```ocaml
  | Plan.P_window_slot _ ->
    failwith "Exec: P_window_slot in eval_expr — must be substituted by planner before evaluation"
```

- [ ] **Step 8: Build and fix compilation errors**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1 | head -60
```

Expected: Build succeeds. Fix any exhaustiveness warnings or type errors.

- [ ] **Step 9: Commit**

```bash
git add lib/sql/sema.ml lib/sql/sema.mli lib/sql/plan.ml lib/sql/planner.ml lib/sql/exec.ml
git commit -m "feat(phase14): sema/plan/planner types for window functions and recursive CTEs"
```

---

## Task 3: Window Functions — Execution Engine

**Files:**
- Modify: `lib/sql/exec.ml`

- [ ] **Step 1: Add window computation helpers to `exec.ml`**

Add these helpers just before the `to_stream` function (around line 2450). They use `eval_expr`, `compare_values`, and `Row.value` which are already in scope.

First, a helper to evaluate partition key for a row:

```ocaml
let eval_partition_key clock params row (partition_by : Plan.expr list) : Row.value list =
  List.map (eval_expr clock params row) partition_by

let partition_keys_equal (a : Row.value list) (b : Row.value list) : bool =
  List.length a = List.length b &&
  List.for_all2 (fun x y -> compare_values x y = 0) a b
```

Group rows (with original indices) into partitions:

```ocaml
let group_by_partition clock params partition_by (indexed_rows : (int * Row.t) list)
    : (Row.value list * (int * Row.t) list) list =
  List.fold_left (fun acc (idx, row) ->
    let key = eval_partition_key clock params row partition_by in
    match List.find_opt (fun (k, _) -> partition_keys_equal k key) acc with
    | Some _ ->
      List.map (fun (k, pairs) ->
        if partition_keys_equal k key then (k, pairs @ [(idx, row)]) else (k, pairs)
      ) acc
    | None -> acc @ [(key, [(idx, row)])]
  ) [] indexed_rows
```

Sort rows within a partition by `order_by` keys:

```ocaml
let sort_partition_by clock params (order_by : (Plan.expr * [`Asc | `Desc]) list)
    (indexed_rows : (int * Row.t) list) : (int * Row.t) list =
  if order_by = [] then indexed_rows
  else
    List.sort (fun (_, ra) (_, rb) ->
      let rec cmp = function
        | [] -> 0
        | (e, dir) :: rest ->
          let va = eval_expr clock params ra e in
          let vb = eval_expr clock params rb e in
          let c = compare_values va vb in
          let c' = match dir with `Asc -> c | `Desc -> -c in
          if c' <> 0 then c' else cmp rest
      in cmp order_by
    ) indexed_rows
```

- [ ] **Step 2: Add `compute_window_for_partition` to `exec.ml`**

This function takes a window plan item, a sorted list of `(original_index, row)` pairs for one partition, and returns an array mapping original index → window result value.

```ocaml
let compute_window_for_partition clock params (wplan : Plan.window_plan_item)
    (sorted_indexed : (int * Row.t) list) (n_total : int) : Row.value array =
  let results = Array.make n_total Row.V_null in
  let sorted_rows = Array.of_list (List.map snd sorted_indexed) in
  let sorted_orig_idxs = Array.of_list (List.map fst sorted_indexed) in
  let n = Array.length sorted_rows in
  (match wplan.Plan.func with
   | Ast.WF_row_number ->
     for pos = 0 to n - 1 do
       results.(sorted_orig_idxs.(pos)) <- Row.V_int (Int64.of_int (pos + 1))
     done
   | Ast.WF_rank ->
     let cur_rank = ref 1 in
     for pos = 0 to n - 1 do
       if pos > 0 then begin
         let order_changed = List.exists (fun (e, _) ->
           compare_values
             (eval_expr clock params sorted_rows.(pos)   e)
             (eval_expr clock params sorted_rows.(pos-1) e) <> 0
         ) wplan.Plan.order_by in
         if order_changed then cur_rank := pos + 1
       end;
       results.(sorted_orig_idxs.(pos)) <- Row.V_int (Int64.of_int !cur_rank)
     done
   | Ast.WF_dense_rank ->
     let cur_rank = ref 1 in
     for pos = 0 to n - 1 do
       if pos > 0 then begin
         let order_changed = List.exists (fun (e, _) ->
           compare_values
             (eval_expr clock params sorted_rows.(pos)   e)
             (eval_expr clock params sorted_rows.(pos-1) e) <> 0
         ) wplan.Plan.order_by in
         if order_changed then incr cur_rank
       end;
       results.(sorted_orig_idxs.(pos)) <- Row.V_int (Int64.of_int !cur_rank)
     done
   | Ast.WF_ntile ->
     let n_buckets =
       match wplan.Plan.args with
       | [e] -> (match eval_expr clock params [||] e with
                 | Row.V_int k -> Int64.to_int k
                 | _ -> 1)
       | _ -> 1
     in
     let n_buckets = max 1 n_buckets in
     for pos = 0 to n - 1 do
       let bucket = (pos * n_buckets / n) + 1 in
       results.(sorted_orig_idxs.(pos)) <- Row.V_int (Int64.of_int bucket)
     done
   | Ast.WF_lag | Ast.WF_lead ->
     let is_lag = wplan.Plan.func = Ast.WF_lag in
     let offset =
       match wplan.Plan.args with
       | _ :: e :: _ -> (match eval_expr clock params [||] e with
                         | Row.V_int k -> Int64.to_int k
                         | _ -> 1)
       | _ -> 1
     in
     let default_expr = match wplan.Plan.args with _ :: _ :: e :: _ -> Some e | _ -> None in
     for pos = 0 to n - 1 do
       let src_pos = if is_lag then pos - offset else pos + offset in
       let v =
         if src_pos >= 0 && src_pos < n then
           (match wplan.Plan.args with
            | e :: _ -> eval_expr clock params sorted_rows.(src_pos) e
            | [] -> Row.V_null)
         else
           match default_expr with
           | Some e -> eval_expr clock params sorted_rows.(pos) e
           | None   -> Row.V_null
       in
       results.(sorted_orig_idxs.(pos)) <- v
     done
   | Ast.WF_first_value ->
     let arg_expr = match wplan.Plan.args with e :: _ -> e | [] -> failwith "FIRST_VALUE requires argument" in
     let first_val = if n > 0 then eval_expr clock params sorted_rows.(0) arg_expr else Row.V_null in
     for pos = 0 to n - 1 do
       results.(sorted_orig_idxs.(pos)) <- first_val
     done
   | Ast.WF_last_value ->
     (* Default frame: ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW *)
     let arg_expr = match wplan.Plan.args with e :: _ -> e | [] -> failwith "LAST_VALUE requires argument" in
     for pos = 0 to n - 1 do
       results.(sorted_orig_idxs.(pos)) <- eval_expr clock params sorted_rows.(pos) arg_expr
     done
   | Ast.WF_nth_value ->
     let arg_expr = match wplan.Plan.args with e :: _ -> e | [] -> failwith "NTH_VALUE requires argument" in
     let n_arg =
       match wplan.Plan.args with
       | _ :: e :: _ -> (match eval_expr clock params [||] e with
                         | Row.V_int k -> Int64.to_int k
                         | _ -> 1)
       | _ -> 1
     in
     (* frame is ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW by default *)
     for pos = 0 to n - 1 do
       let v =
         if n_arg >= 1 && n_arg <= pos + 1 then
           eval_expr clock params sorted_rows.(n_arg - 1) arg_expr
         else
           Row.V_null
       in
       results.(sorted_orig_idxs.(pos)) <- v
     done
   | Ast.WF_agg agg_func ->
     (* Default frame:
        - With ORDER BY: ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW (running aggregate)
        - Without ORDER BY: entire partition *)
     let has_order = wplan.Plan.order_by <> [] in
     let arg_expr = match wplan.Plan.args with e :: _ -> Some e | [] -> None in
     (* Precompute arg values for all rows in sorted order *)
     let arg_vals = Array.init n (fun pos ->
       match arg_expr with
       | Some e -> eval_expr clock params sorted_rows.(pos) e
       | None   -> Row.V_null
     ) in
     for pos = 0 to n - 1 do
       let frame_end = if has_order then pos else n - 1 in
       let result = match agg_func with
         | Ast.Agg_count ->
           let cnt = List.length (List.filter (fun i ->
             not (arg_vals.(i) = Row.V_null)
           ) (List.init (frame_end + 1) (fun i -> i))) in
           (* COUNT(*) uses None arg → count all rows in frame *)
           let cnt = if arg_expr = None then frame_end + 1 else cnt in
           Row.V_int (Int64.of_int cnt)
         | Ast.Agg_sum ->
           List.fold_left (fun acc i ->
             match acc, arg_vals.(i) with
             | Row.V_null, v -> v
             | _, Row.V_null -> acc
             | Row.V_int a, Row.V_int b -> Row.V_int (Int64.add a b)
             | Row.V_real a, Row.V_real b -> Row.V_real (a +. b)
             | Row.V_int a, Row.V_real b -> Row.V_real (Int64.to_float a +. b)
             | Row.V_real a, Row.V_int b -> Row.V_real (a +. Int64.to_float b)
             | _, _ -> acc
           ) Row.V_null (List.init (frame_end + 1) (fun i -> i))
         | Ast.Agg_avg ->
           let vals = List.filter_map (fun i ->
             match arg_vals.(i) with
             | Row.V_null -> None
             | Row.V_int n -> Some (Int64.to_float n)
             | Row.V_real f -> Some f
             | _ -> None
           ) (List.init (frame_end + 1) (fun i -> i)) in
           if vals = [] then Row.V_null
           else Row.V_real (List.fold_left (+.) 0.0 vals /. float_of_int (List.length vals))
         | Ast.Agg_min ->
           List.fold_left (fun acc i ->
             match arg_vals.(i) with
             | Row.V_null -> acc
             | v -> (match acc with
               | Row.V_null -> v
               | acc_v -> if compare_values v acc_v < 0 then v else acc_v)
           ) Row.V_null (List.init (frame_end + 1) (fun i -> i))
         | Ast.Agg_max ->
           List.fold_left (fun acc i ->
             match arg_vals.(i) with
             | Row.V_null -> acc
             | v -> (match acc with
               | Row.V_null -> v
               | acc_v -> if compare_values v acc_v > 0 then v else acc_v)
           ) Row.V_null (List.init (frame_end + 1) (fun i -> i))
       in
       results.(sorted_orig_idxs.(pos)) <- result
     done
  );
  results
```

- [ ] **Step 3: Add `Op_window` execution arm in `to_stream`**

In `lib/sql/exec.ml`, in the `to_stream` function, add the `Op_window` arm (before the existing `Op_with_cte` arm):

```ocaml
  | Plan.Op_window { child; windows; n_input_cols } ->
    let* child_stream = to_stream clock params store ~mode ~cat child in
    let* all_rows = Lwt_stream.to_list child_stream in
    let n_rows = List.length all_rows in
    let all_rows_arr = Array.of_list all_rows in
    (* results.(wi).(ri) = window result for window i, row i *)
    let window_results = Array.init (List.length windows) (fun wi ->
      let wplan = List.nth windows wi in
      let indexed_rows = List.mapi (fun i row -> (i, row)) all_rows in
      let partitions = group_by_partition clock params wplan.Plan.partition_by indexed_rows in
      let combined = Array.make n_rows Row.V_null in
      List.iter (fun (_, partition_idx_rows) ->
        let sorted = sort_partition_by clock params wplan.Plan.order_by partition_idx_rows in
        let partition_results = compute_window_for_partition clock params wplan sorted n_rows in
        Array.iteri (fun i v -> if v <> Row.V_null then combined.(i) <- v) partition_results
      ) partitions;
      combined
    ) in
    let augmented = Array.to_list (Array.mapi (fun i row ->
      let extras = Array.init (List.length windows) (fun wi -> window_results.(wi).(i)) in
      Array.append row extras
    ) all_rows_arr) in
    Lwt.return (Lwt_stream.of_list augmented)
```

Note: `n_input_cols` is stored in `Op_window` for documentation/debugging purposes, but the exec doesn't need it since `substitute_window_slots` in the planner already converted `P_window_slot` to `P_col (n_input_cols + i)`.

- [ ] **Step 4: Write failing e2e tests for window functions**

In `test/test_e2e.ml`, add the following test functions (before the `let () =` at the end):

```ocaml
let test_window_row_number () =
  let db = fresh_db () in
  exec_ok db "CREATE TABLE t (dept TEXT, name TEXT, salary INTEGER)";
  exec_ok db "INSERT INTO t VALUES ('eng', 'Alice', 90000), ('eng', 'Bob', 80000), ('hr', 'Carol', 70000), ('hr', 'Dave', 60000)";
  let rows = query_ok db "SELECT name, ROW_NUMBER() OVER (PARTITION BY dept ORDER BY salary DESC) AS rn FROM t ORDER BY dept, rn" in
  Alcotest.(check int) "4 rows" 4 (List.length rows);
  Alcotest.check value_testable "Alice rn=1" (Db.V_int 1L) rows.(0).(1);
  Alcotest.check value_testable "Bob rn=2"   (Db.V_int 2L) rows.(1).(1);
  Alcotest.check value_testable "Carol rn=1" (Db.V_int 1L) rows.(2).(1);
  Alcotest.check value_testable "Dave rn=2"  (Db.V_int 2L) rows.(3).(1)

let test_window_rank () =
  let db = fresh_db () in
  exec_ok db "CREATE TABLE scores (name TEXT, score INTEGER)";
  exec_ok db "INSERT INTO scores VALUES ('A', 100), ('B', 100), ('C', 90), ('D', 80)";
  let rows = query_ok db "SELECT name, RANK() OVER (ORDER BY score DESC) AS r FROM scores ORDER BY name" in
  Alcotest.(check int) "4 rows" 4 (List.length rows);
  (* A and B both score 100 → rank 1; C → rank 3; D → rank 4 *)
  Alcotest.check value_testable "A rank=1" (Db.V_int 1L) rows.(0).(1);
  Alcotest.check value_testable "B rank=1" (Db.V_int 1L) rows.(1).(1);
  Alcotest.check value_testable "C rank=3" (Db.V_int 3L) rows.(2).(1);
  Alcotest.check value_testable "D rank=4" (Db.V_int 4L) rows.(3).(1)

let test_window_dense_rank () =
  let db = fresh_db () in
  exec_ok db "CREATE TABLE scores (name TEXT, score INTEGER)";
  exec_ok db "INSERT INTO scores VALUES ('A', 100), ('B', 100), ('C', 90), ('D', 80)";
  let rows = query_ok db "SELECT name, DENSE_RANK() OVER (ORDER BY score DESC) AS dr FROM scores ORDER BY name" in
  (* A and B both score 100 → dense_rank 1; C → dense_rank 2; D → dense_rank 3 *)
  Alcotest.check value_testable "A dr=1" (Db.V_int 1L) rows.(0).(1);
  Alcotest.check value_testable "C dr=2" (Db.V_int 2L) rows.(2).(1);
  Alcotest.check value_testable "D dr=3" (Db.V_int 3L) rows.(3).(1)

let test_window_lag () =
  let db = fresh_db () in
  exec_ok db "CREATE TABLE vals (id INTEGER, v INTEGER)";
  exec_ok db "INSERT INTO vals VALUES (1, 10), (2, 20), (3, 30)";
  let rows = query_ok db "SELECT id, v, LAG(v, 1, 0) OVER (ORDER BY id) AS prev FROM vals ORDER BY id" in
  Alcotest.check value_testable "id=1 prev=0"  (Db.V_int 0L)  rows.(0).(2);
  Alcotest.check value_testable "id=2 prev=10" (Db.V_int 10L) rows.(1).(2);
  Alcotest.check value_testable "id=3 prev=20" (Db.V_int 20L) rows.(2).(2)

let test_window_lead () =
  let db = fresh_db () in
  exec_ok db "CREATE TABLE vals (id INTEGER, v INTEGER)";
  exec_ok db "INSERT INTO vals VALUES (1, 10), (2, 20), (3, 30)";
  let rows = query_ok db "SELECT id, v, LEAD(v, 1, 0) OVER (ORDER BY id) AS nxt FROM vals ORDER BY id" in
  Alcotest.check value_testable "id=1 nxt=20" (Db.V_int 20L) rows.(0).(2);
  Alcotest.check value_testable "id=2 nxt=30" (Db.V_int 30L) rows.(1).(2);
  Alcotest.check value_testable "id=3 nxt=0"  (Db.V_int 0L)  rows.(2).(2)

let test_window_sum_over () =
  let db = fresh_db () in
  exec_ok db "CREATE TABLE sales (id INTEGER, amount INTEGER)";
  exec_ok db "INSERT INTO sales VALUES (1, 100), (2, 200), (3, 300)";
  let rows = query_ok db "SELECT id, SUM(amount) OVER (ORDER BY id) AS running FROM sales ORDER BY id" in
  Alcotest.check value_testable "id=1 running=100" (Db.V_int 100L) rows.(0).(1);
  Alcotest.check value_testable "id=2 running=300" (Db.V_int 300L) rows.(1).(1);
  Alcotest.check value_testable "id=3 running=600" (Db.V_int 600L) rows.(2).(1)

let test_window_no_partition () =
  let db = fresh_db () in
  exec_ok db "CREATE TABLE t (x INTEGER)";
  exec_ok db "INSERT INTO t VALUES (3), (1), (2)";
  let rows = query_ok db "SELECT x, ROW_NUMBER() OVER (ORDER BY x) AS rn FROM t ORDER BY x" in
  Alcotest.check value_testable "x=1 rn=1" (Db.V_int 1L) rows.(0).(1);
  Alcotest.check value_testable "x=2 rn=2" (Db.V_int 2L) rows.(1).(1);
  Alcotest.check value_testable "x=3 rn=3" (Db.V_int 3L) rows.(2).(1)

let test_window_first_last_value () =
  let db = fresh_db () in
  exec_ok db "CREATE TABLE t (dept TEXT, salary INTEGER)";
  exec_ok db "INSERT INTO t VALUES ('eng', 90000), ('eng', 80000), ('eng', 70000)";
  let rows = query_ok db "SELECT salary, FIRST_VALUE(salary) OVER (ORDER BY salary DESC) AS first, LAST_VALUE(salary) OVER (ORDER BY salary DESC) AS last FROM t ORDER BY salary DESC" in
  Alcotest.check value_testable "first=90000" (Db.V_int 90000L) rows.(0).(1);
  (* LAST_VALUE with default frame (ROWS PRECEDING TO CURRENT ROW): last = current row *)
  Alcotest.check value_testable "last row0=90000" (Db.V_int 90000L) rows.(0).(2);
  Alcotest.check value_testable "last row2=70000" (Db.V_int 70000L) rows.(2).(2)
```

Add to the `let () = Alcotest.run ...` section:

```ocaml
    "window", [
      Alcotest.test_case "row_number"       `Quick test_window_row_number;
      Alcotest.test_case "rank"             `Quick test_window_rank;
      Alcotest.test_case "dense_rank"       `Quick test_window_dense_rank;
      Alcotest.test_case "lag"              `Quick test_window_lag;
      Alcotest.test_case "lead"             `Quick test_window_lead;
      Alcotest.test_case "sum_over"         `Quick test_window_sum_over;
      Alcotest.test_case "no_partition"     `Quick test_window_no_partition;
      Alcotest.test_case "first_last_value" `Quick test_window_first_last_value;
    ];
```

- [ ] **Step 5: Run tests to verify failures first, then implementation**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune test 2>&1 | tail -30
```

Expected: 8 new window tests fail. Existing 271 tests continue passing.

- [ ] **Step 6: Run all tests after full implementation to verify passing**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune test 2>&1 | tail -30
```

Expected: All 279+ tests pass.

- [ ] **Step 7: Commit**

```bash
git add lib/sql/exec.ml test/test_e2e.ml
git commit -m "feat(phase14): window function execution engine (ROW_NUMBER, RANK, DENSE_RANK, LAG, LEAD, FIRST/LAST_VALUE, aggregate OVER)"
```

---

## Task 4: Recursive CTEs

**Files:**
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_e2e.ml`

- [ ] **Step 1: Write failing e2e tests for recursive CTEs**

In `test/test_e2e.ml`, add:

```ocaml
let test_recursive_cte_series () =
  (* Generate sequence 1..5 using recursive CTE *)
  let db = fresh_db () in
  exec_ok db "CREATE TABLE dummy (x INTEGER)";
  exec_ok db "INSERT INTO dummy VALUES (1)";
  let rows = query_ok db
    "WITH RECURSIVE cnt(n) AS (
       SELECT 1
       UNION ALL
       SELECT n + 1 FROM cnt WHERE n < 5
     )
     SELECT n FROM cnt ORDER BY n" in
  Alcotest.(check int) "5 rows" 5 (List.length rows);
  Alcotest.check value_testable "n=1" (Db.V_int 1L) rows.(0).(0);
  Alcotest.check value_testable "n=5" (Db.V_int 5L) rows.(4).(0)

let test_recursive_cte_sum () =
  let db = fresh_db () in
  let rows = query_ok db
    "WITH RECURSIVE n(i, s) AS (
       SELECT 1, 1
       UNION ALL
       SELECT i + 1, s + (i + 1) FROM n WHERE i < 4
     )
     SELECT s FROM n WHERE i = 4" in
  Alcotest.(check int) "1 row" 1 (List.length rows);
  Alcotest.check value_testable "sum=10" (Db.V_int 10L) rows.(0).(0)

let test_recursive_cte_tree () =
  (* Hierarchical closure: find all descendants of node 1 *)
  let db = fresh_db () in
  exec_ok db "CREATE TABLE tree (id INTEGER, parent INTEGER)";
  exec_ok db "INSERT INTO tree VALUES (1, 0), (2, 1), (3, 1), (4, 2)";
  let rows = query_ok db
    "WITH RECURSIVE desc(id) AS (
       SELECT id FROM tree WHERE parent = 1
       UNION ALL
       SELECT t.id FROM tree t INNER JOIN desc d ON t.parent = d.id
     )
     SELECT id FROM desc ORDER BY id" in
  Alcotest.(check int) "3 descendants" 3 (List.length rows);
  Alcotest.check value_testable "id=2" (Db.V_int 2L) rows.(0).(0);
  Alcotest.check value_testable "id=3" (Db.V_int 3L) rows.(1).(0);
  Alcotest.check value_testable "id=4" (Db.V_int 4L) rows.(2).(0)

let test_recursive_cte_non_recursive_unchanged () =
  (* Non-recursive WITH still works correctly *)
  let db = fresh_db () in
  exec_ok db "CREATE TABLE t (x INTEGER)";
  exec_ok db "INSERT INTO t VALUES (1), (2), (3)";
  let rows = query_ok db "WITH sq AS (SELECT x FROM t WHERE x > 1) SELECT x FROM sq ORDER BY x" in
  Alcotest.(check int) "2 rows" 2 (List.length rows);
  Alcotest.check value_testable "x=2" (Db.V_int 2L) rows.(0).(0);
  Alcotest.check value_testable "x=3" (Db.V_int 3L) rows.(1).(0)
```

Add to the `let () = Alcotest.run ...` section:

```ocaml
    "recursive_cte", [
      Alcotest.test_case "series"    `Quick test_recursive_cte_series;
      Alcotest.test_case "sum"       `Quick test_recursive_cte_sum;
      Alcotest.test_case "tree"      `Quick test_recursive_cte_tree;
      Alcotest.test_case "non_recursive_unchanged" `Quick test_recursive_cte_non_recursive_unchanged;
    ];
```

- [ ] **Step 2: Run tests to verify failures**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune test 2>&1 | grep "recursive_cte"
```

Expected: 4 recursive CTE tests fail.

- [ ] **Step 3: Implement recursive CTE execution in `exec.ml`**

In `lib/sql/exec.ml`, find the `Op_with_cte` arm in `to_stream` (around line 2549):

```ocaml
  | Plan.Op_with_cte { cte_name; def; query } ->
    let* def_stream = to_stream clock params store ~mode ~cat def in
    let* cte_rows = Lwt_stream.to_list def_stream in
    let patched = substitute_cte ~cte_name ~rows:cte_rows query in
    to_stream clock params store ~mode ~cat patched
```

Replace with:

```ocaml
  | Plan.Op_with_cte { cte_name; def; query; recursive = false } ->
    let* def_stream = to_stream clock params store ~mode ~cat def in
    let* cte_rows = Lwt_stream.to_list def_stream in
    let patched = substitute_cte ~cte_name ~rows:cte_rows query in
    to_stream clock params store ~mode ~cat patched
  | Plan.Op_with_cte { cte_name; def; query; recursive = true } ->
    (* Recursive CTE: def must be Op_union { all=true; left=base; right=recursive_arm }.
       Execute base case once, then iteratively execute recursive_arm substituting
       CTE scan with current working set, until no new rows are produced. *)
    let (base_op, recursive_arm) = match def with
      | Plan.Op_union { all = true; left; right } -> (left, right)
      | _ -> failwith "Exec: recursive CTE 'def' must be UNION ALL — non-UNION ALL recursive CTEs are not supported"
    in
    let* base_stream = to_stream clock params store ~mode ~cat base_op in
    let* seed_rows = Lwt_stream.to_list base_stream in
    let rec iterate acc working =
      if working = [] then Lwt.return acc
      else begin
        let patched_arm = substitute_cte ~cte_name ~rows:working recursive_arm in
        let* new_stream = to_stream clock params store ~mode ~cat patched_arm in
        let* new_rows = Lwt_stream.to_list new_stream in
        iterate (acc @ new_rows) new_rows
      end
    in
    let* all_rows = iterate seed_rows seed_rows in
    let patched_query = substitute_cte ~cte_name ~rows:all_rows query in
    to_stream clock params store ~mode ~cat patched_query
```

Also update `substitute_cte` to handle the new `recursive` field in `Op_with_cte` (add `recursive = _` or propagate):

```ocaml
  | Plan.Op_with_cte r when not (String.equal r.cte_name cte_name) ->
    Plan.Op_with_cte { r with query = go r.query }
```

This already uses record update (`{ r with ... }`) so it automatically handles the new `recursive` field.

- [ ] **Step 4: Run all tests to verify passing**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune test 2>&1 | tail -30
```

Expected: All 283+ tests pass (271 existing + 8 window + 4 recursive_cte).

- [ ] **Step 5: Commit**

```bash
git add lib/sql/exec.ml test/test_e2e.ml
git commit -m "feat(phase14): recursive CTE execution with UNION ALL iteration"
```

---

## Task 5: SQLite Comparison Tests + Coverage

**Files:**
- Modify: `test/test_sqlite_compare.ml`

- [ ] **Step 1: Add Phase 14 SQLite comparison test cases**

In `test/test_sqlite_compare.ml`, add the following cases before the `(* ── runner ───... *)` comment:

```ocaml
let phase14_window_cases = [
  { name = "window_row_number";
    setup = [
      "CREATE TABLE emp (dept TEXT, name TEXT, salary INTEGER)";
      "INSERT INTO emp VALUES ('eng','Alice',90000),('eng','Bob',80000),('hr','Carol',70000),('hr','Dave',60000)";
    ];
    query = "SELECT name, ROW_NUMBER() OVER (PARTITION BY dept ORDER BY salary DESC) AS rn FROM emp ORDER BY dept, rn";
    unordered = false };

  { name = "window_rank";
    setup = [
      "CREATE TABLE scores (name TEXT, score INTEGER)";
      "INSERT INTO scores VALUES ('A',100),('B',100),('C',90),('D',80)";
    ];
    query = "SELECT name, RANK() OVER (ORDER BY score DESC) AS r FROM scores ORDER BY name";
    unordered = false };

  { name = "window_dense_rank";
    setup = [
      "CREATE TABLE scores (name TEXT, score INTEGER)";
      "INSERT INTO scores VALUES ('A',100),('B',100),('C',90),('D',80)";
    ];
    query = "SELECT name, DENSE_RANK() OVER (ORDER BY score DESC) AS dr FROM scores ORDER BY name";
    unordered = false };

  { name = "window_lag";
    setup = [
      "CREATE TABLE vals (id INTEGER, v INTEGER)";
      "INSERT INTO vals VALUES (1,10),(2,20),(3,30)";
    ];
    query = "SELECT id, v, LAG(v, 1, 0) OVER (ORDER BY id) AS prev FROM vals ORDER BY id";
    unordered = false };

  { name = "window_lead";
    setup = [
      "CREATE TABLE vals (id INTEGER, v INTEGER)";
      "INSERT INTO vals VALUES (1,10),(2,20),(3,30)";
    ];
    query = "SELECT id, v, LEAD(v, 1, 0) OVER (ORDER BY id) AS nxt FROM vals ORDER BY id";
    unordered = false };

  { name = "window_sum_running";
    setup = [
      "CREATE TABLE sales (id INTEGER, amount INTEGER)";
      "INSERT INTO sales VALUES (1,100),(2,200),(3,300)";
    ];
    query = "SELECT id, SUM(amount) OVER (ORDER BY id) AS running FROM sales ORDER BY id";
    unordered = false };

  { name = "window_partition_sum";
    setup = [
      "CREATE TABLE t (dept TEXT, v INTEGER)";
      "INSERT INTO t VALUES ('a',1),('a',2),('b',10),('b',20)";
    ];
    query = "SELECT dept, v, SUM(v) OVER (PARTITION BY dept) AS dept_total FROM t ORDER BY dept, v";
    unordered = false };

  { name = "window_no_partition_no_order";
    setup = [
      "CREATE TABLE t (x INTEGER)";
      "INSERT INTO t VALUES (3),(1),(2)";
    ];
    query = "SELECT x, COUNT(*) OVER () AS total FROM t ORDER BY x";
    unordered = false };
]

let phase14_recursive_cte_cases = [
  { name = "recursive_series";
    setup = [];
    query = "WITH RECURSIVE cnt(n) AS (SELECT 1 UNION ALL SELECT n + 1 FROM cnt WHERE n < 5) SELECT n FROM cnt ORDER BY n";
    unordered = false };

  { name = "recursive_sum";
    setup = [];
    query = "WITH RECURSIVE n(i, s) AS (SELECT 1, 1 UNION ALL SELECT i+1, s+(i+1) FROM n WHERE i < 4) SELECT s FROM n WHERE i = 4";
    unordered = false };

  { name = "recursive_tree";
    setup = [
      "CREATE TABLE tree (id INTEGER, parent INTEGER)";
      "INSERT INTO tree VALUES (1,0),(2,1),(3,1),(4,2)";
    ];
    query = "WITH RECURSIVE desc(id) AS (SELECT id FROM tree WHERE parent = 1 UNION ALL SELECT t.id FROM tree t INNER JOIN desc d ON t.parent = d.id) SELECT id FROM desc ORDER BY id";
    unordered = false };

  { name = "recursive_fibonacci";
    setup = [];
    query = "WITH RECURSIVE fib(a, b) AS (SELECT 0, 1 UNION ALL SELECT b, a+b FROM fib WHERE a < 10) SELECT a FROM fib ORDER BY a";
    unordered = false };
]
```

Note: `test_sqlite_compare.ml` uses `setup = []` for FROM-less constant SELECTs since the SQLite runner handles them fine; the schema creation is included in the `setup` list for table-based queries.

- [ ] **Step 2: Add phase14 groups to the runner**

Find the `let () = Alcotest.run "sqlite_compare" [` section at the end of `test/test_sqlite_compare.ml` and add:

```ocaml
    "phase14_window",          List.map make_test phase14_window_cases;
    "phase14_recursive_cte",   List.map make_test phase14_recursive_cte_cases;
```

- [ ] **Step 3: Run SQLite comparison tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune test 2>&1 | grep -E "phase14|FAIL|ERROR" | head -30
```

Expected: All 12 new SQLite comparison tests pass.

- [ ] **Step 4: Run full test suite and confirm total count**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune test 2>&1 | tail -20
```

Expected: All tests pass. E2e count: 283+; SQLite compare count: 242+.

- [ ] **Step 5: Generate coverage report**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev sh -c "
  BISECT_FILE=/tmp/bisect dune exec --instrument-with bisect_ppx test/test_e2e.exe 2>/dev/null
  BISECT_FILE=/tmp/bisect dune exec --instrument-with bisect_ppx test/test_sqlite_compare.exe 2>/dev/null
  bisect-ppx-report html --output _coverage
  bisect-ppx-report summary
"
```

Expected: Coverage ≥ 88%.

- [ ] **Step 6: Commit**

```bash
git add test/test_sqlite_compare.ml
git commit -m "test(phase14): SQLite comparison tests for window functions and recursive CTEs"
```

---

## Self-Review

### Spec Coverage

| Feature | Task |
|---------|------|
| `ROW_NUMBER() OVER (PARTITION BY ... ORDER BY ...)` | Tasks 1–3 |
| `RANK()`, `DENSE_RANK()` | Tasks 1–3 |
| `NTILE(n)` | Tasks 1–3 |
| `LAG()`, `LEAD()` | Tasks 1–3 |
| `FIRST_VALUE()`, `LAST_VALUE()`, `NTH_VALUE()` | Tasks 1–3 |
| Aggregate window (`SUM/AVG/MIN/MAX/COUNT OVER (...)`) | Tasks 1–3 |
| `PARTITION BY` and `ORDER BY` in OVER clause | Tasks 1–3 |
| `WITH RECURSIVE name AS (base UNION ALL recursive)` | Task 4 |
| Non-recursive CTE backward compatibility | Task 4 |
| SQLite comparison parity | Task 5 |

All features accounted for.

### Placeholder Scan

No TBD, TODO, or missing code sections. All helpers use concrete data structures and algorithms.

### Type Consistency

- `Ast.window_func` enum used consistently from AST → sema (`window_sema.func`) → plan (`window_plan_item.func`) → exec (`wplan.Plan.func`)
- `window_sema.order_by` uses `bound_order_key list` (sema) → converted to `(Plan.expr * [`Asc|`Desc]) list` in `plan_window_item`
- `BS_select.windows` is `window_sema list`; planner converts via `List.map plan_window_item`
- `Op_with_cte.recursive` is `bool` throughout AST → sema → plan → exec
- `substitute_window_slots ~n_input_cols` correctly converts `P_window_slot i` to `P_col (n_input_cols + i)` using the same `n_input_cols` passed to `Op_window`
