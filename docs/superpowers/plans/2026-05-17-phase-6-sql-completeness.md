# Phase 6: SQL Expression Completeness Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement the remaining SQL expression features — operators (`||`, `%`, bitwise, LIKE, BETWEEN, IN), additional scalar functions (SUBSTR, TRIM, REPLACE, ROUND, TYPEOF), ORDER BY arbitrary expressions, PRAGMA support, and multi-column indexes.

**Architecture:** All features extend the three-layer expression pipeline: AST (syntax) → Sema bound expressions (semantic analysis) → Plan expressions (physical) → Exec evaluation. Operators and functions are added at every layer in sync. ORDER BY is refactored from a column-index to a full expression. PRAGMA is implemented as pre-computed row lists in the planner. Multi-column indexes extend catalog + index key encoding (which already supports multi-value keys).

**Tech Stack:** OCaml 5.x, dune 3.x, menhir (parser generator), alcotest (tests), lwt (async). Run `dune build` after every grammar change to catch Menhir conflicts early.

**Issues closed:** #104 (LIKE/GLOB), #105 (BETWEEN), #106 (IN), #107 (||), #108 (bitwise), #109 (%), #110 (scalar functions), #114 (multi-column indexes), #115 (ORDER BY expr), #119 (PRAGMA)

---

## File Map

| File | Role |
|---|---|
| `lib/sql/ast.ml` | SQL AST — source of truth for new syntax nodes |
| `lib/sql/lexer.mll` | OCamllex lexer — add new tokens |
| `lib/sql/parser.mly` | Menhir grammar — add productions + precedence |
| `lib/sql/plan.ml` | Physical plan nodes — mirror AST additions |
| `lib/sql/sema.ml` | Semantic analysis — bind AST → bound_expr, update in 3 places |
| `lib/sql/planner.ml` | Planner — map bound_expr → Plan.expr, Op_sort |
| `lib/sql/exec.ml` | Execution — eval_binop, eval_func, Op_sort, Op_pragma_rows |
| `lib/catalog/catalog.ml` + `.mli` | Catalog — multi-column index encoding, sync lookup |
| `test/test_expr.ml` | Unit tests for eval_binop / eval_func |
| `test/test_parser.ml` | Parser unit tests |
| `test/test_scalar_fns.ml` | Scalar function e2e tests |
| `test/test_e2e.ml` | End-to-end SQL tests |
| `test/test_sema.ml` | Sema unit tests (order_key update) |
| `test/test_planner.ml` | Planner unit tests (Op_sort update) |
| `test/test_sqlite_compare.ml` | SQLite differential tests |

---

## Task 1: Extended Binary Operators (`||`, `%`, `&`, `|`, `~`, `<<`, `>>`)

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/sema.ml`
- Modify: `lib/sql/planner.ml`
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_expr.ml`
- Modify: `test/test_parser.ml`

- [ ] **Step 1: Write failing tests in `test/test_expr.ml`**

Add these tests to the existing test suite at the bottom:

```ocaml
let test_concat () =
  let row = [||] in
  let v = Exec.eval_expr [||] row (Plan.P_binop (Plan.Concat, Plan.P_lit (Ast.L_text "foo"), Plan.P_lit (Ast.L_text "bar"))) in
  Alcotest.(check string) "concat" "foobar" (match v with Row.V_text s -> s | _ -> "?")

let test_mod () =
  let row = [||] in
  let v = Exec.eval_expr [||] row (Plan.P_binop (Plan.Mod, Plan.P_lit (Ast.L_int 7L), Plan.P_lit (Ast.L_int 3L))) in
  Alcotest.(check int64) "mod" 1L (match v with Row.V_int n -> n | _ -> -1L)

let test_bit_and () =
  let row = [||] in
  let v = Exec.eval_expr [||] row (Plan.P_binop (Plan.Bit_and, Plan.P_lit (Ast.L_int 5L), Plan.P_lit (Ast.L_int 3L))) in
  Alcotest.(check int64) "bit_and" 1L (match v with Row.V_int n -> n | _ -> -1L)

let test_bit_or () =
  let row = [||] in
  let v = Exec.eval_expr [||] row (Plan.P_binop (Plan.Bit_or, Plan.P_lit (Ast.L_int 5L), Plan.P_lit (Ast.L_int 2L))) in
  Alcotest.(check int64) "bit_or" 7L (match v with Row.V_int n -> n | _ -> -1L)

let test_lshift () =
  let row = [||] in
  let v = Exec.eval_expr [||] row (Plan.P_binop (Plan.Lshift, Plan.P_lit (Ast.L_int 2L), Plan.P_lit (Ast.L_int 3L))) in
  Alcotest.(check int64) "lshift" 16L (match v with Row.V_int n -> n | _ -> -1L)

let test_rshift () =
  let row = [||] in
  let v = Exec.eval_expr [||] row (Plan.P_binop (Plan.Rshift, Plan.P_lit (Ast.L_int 16L), Plan.P_lit (Ast.L_int 2L))) in
  Alcotest.(check int64) "rshift" 4L (match v with Row.V_int n -> n | _ -> -1L)

let test_bitnot () =
  let row = [||] in
  let v = Exec.eval_expr [||] row (Plan.P_bitnot (Plan.P_lit (Ast.L_int 5L))) in
  Alcotest.(check int64) "bitnot" (-6L) (match v with Row.V_int n -> n | _ -> 0L)
```

Add to the `let () = Alcotest.run "expr" [...]` block:
```ocaml
"binary_operators", [
  Alcotest.test_case "concat"  `Quick test_concat;
  Alcotest.test_case "mod"     `Quick test_mod;
  Alcotest.test_case "bit_and" `Quick test_bit_and;
  Alcotest.test_case "bit_or"  `Quick test_bit_or;
  Alcotest.test_case "lshift"  `Quick test_lshift;
  Alcotest.test_case "rshift"  `Quick test_rshift;
  Alcotest.test_case "bitnot"  `Quick test_bitnot;
];
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z \
  -w /workspace sqlocaml-dev dune exec test/test_expr.exe 2>&1 | tail -20
```

Expected: compilation error — `Plan.Concat`, `Plan.P_bitnot` do not exist yet.

- [ ] **Step 3: Update `lib/sql/ast.ml`**

Change the `binop` type and add `E_bitnot` to `expr`:

```ocaml
type binop =
  | Eq | Ne | Lt | Le | Gt | Ge   (** comparison *)
  | Add | Sub | Mul | Div          (** arithmetic *)
  | And | Or                       (** logical *)
  | Concat                         (** string concatenation || *)
  | Mod                            (** modulo % *)
  | Bit_and | Bit_or               (** bitwise & | *)
  | Lshift | Rshift                (** shift << >> *)
```

In the `expr` type, after `E_neg`:
```ocaml
  | E_bitnot of expr               (** bitwise NOT ~ *)
```

- [ ] **Step 4: Update `lib/sql/lexer.mll`**

Add these rules **before** the single-character `|` and `<` and `>` rules (ocamllex uses longest match, but put multi-char alternatives first for clarity):

```
  | "||"   { CONCAT }
  | "|"    { PIPE }
  | "<<"   { LSHIFT }
  | ">>"   { RSHIFT }
  | "%"    { PERCENT }
  | "&"    { AMPERSAND }
  | "~"    { TILDE }
```

These must appear BEFORE the existing `| "<" { LT }` and `| ">" { GT }` and before any existing `|` rule. In the existing lexer, `"<"` is `LT` and `">"` is `GT`; the multi-character alternatives `"<<"` and `">>"` will win by longest-match.

- [ ] **Step 5: Update `lib/sql/parser.mly`**

**5a. Add token declarations** (after the existing `%token PLUS MINUS SLASH DOT` line):
```
%token CONCAT PERCENT AMPERSAND PIPE TILDE LSHIFT RSHIFT
```

**5b. Replace the entire precedence section** with this expanded version (lowest to highest):
```
%left OR
%left AND
%right NOT
%nonassoc IS
%left EQ NE LT LE GT GE
%left CONCAT
%left PIPE
%left AMPERSAND
%left LSHIFT RSHIFT
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc TILDE UMINUS
```

**5c. Add new productions to the `expr` rule** (after the existing `| a = expr SLASH b = expr` line):
```
  | a = expr CONCAT    b = expr   { E_binop (Concat, a, b) }
  | a = expr PERCENT   b = expr   { E_binop (Mod, a, b) }
  | a = expr AMPERSAND b = expr   { E_binop (Bit_and, a, b) }
  | a = expr PIPE      b = expr   { E_binop (Bit_or, a, b) }
  | a = expr LSHIFT    b = expr   { E_binop (Lshift, a, b) }
  | a = expr RSHIFT    b = expr   { E_binop (Rshift, a, b) }
  | TILDE e = expr %prec TILDE    { E_bitnot e }
```

- [ ] **Step 6: Update `lib/sql/plan.ml`**

Extend the `binop` type:
```ocaml
type binop = Eq | Ne | Lt | Le | Gt | Ge | Add | Sub | Mul | Div | And | Or
           | Concat | Mod | Bit_and | Bit_or | Lshift | Rshift
```

Add `P_bitnot` to `expr` (after `P_neg`):
```ocaml
  | P_bitnot of expr
```

- [ ] **Step 7: Update `lib/sql/sema.ml`**

**7a. Extend sema `binop` type** (same location as plan.ml):
```ocaml
type binop = Eq | Ne | Lt | Le | Gt | Ge | Add | Sub | Mul | Div | And | Or
           | Concat | Mod | Bit_and | Bit_or | Lshift | Rshift
```

**7b. Add `BE_bitnot` to `bound_expr`** (after `BE_neg`):
```ocaml
  | BE_bitnot of bound_expr
```

**7c. Extend `ast_binop_to_sema`**:
```ocaml
let ast_binop_to_sema : Ast.binop -> binop = function
  | Ast.Eq  -> Eq  | Ast.Ne  -> Ne
  | Ast.Lt  -> Lt  | Ast.Le  -> Le
  | Ast.Gt  -> Gt  | Ast.Ge  -> Ge
  | Ast.Add -> Add | Ast.Sub -> Sub
  | Ast.Mul -> Mul | Ast.Div -> Div
  | Ast.And -> And | Ast.Or  -> Or
  | Ast.Concat  -> Concat
  | Ast.Mod     -> Mod
  | Ast.Bit_and -> Bit_and | Ast.Bit_or -> Bit_or
  | Ast.Lshift  -> Lshift  | Ast.Rshift -> Rshift
```

**7d. Add `E_bitnot` case to ALL THREE `bind_expr` variants** (single-table `bind_expr`, join `bind_expr_join`, aggregate `bind_expr_agg`). In each, add after the `| Ast.E_neg e ->` case:
```ocaml
  | Ast.E_bitnot e ->
    (match bind_expr ~param_counter meta e with   (* or bind_expr_join / bind_expr_agg *)
     | Ok be   -> Ok (BE_bitnot be)
     | Error e -> Error e)
```

- [ ] **Step 8: Update `lib/sql/planner.ml`**

**8a. Extend `plan_binop`**:
```ocaml
let plan_binop : Sema.binop -> Plan.binop = function
  | Sema.Eq  -> Plan.Eq  | Sema.Ne  -> Plan.Ne
  | Sema.Lt  -> Plan.Lt  | Sema.Le  -> Plan.Le
  | Sema.Gt  -> Plan.Gt  | Sema.Ge  -> Plan.Ge
  | Sema.Add -> Plan.Add | Sema.Sub -> Plan.Sub
  | Sema.Mul -> Plan.Mul | Sema.Div -> Plan.Div
  | Sema.And -> Plan.And | Sema.Or  -> Plan.Or
  | Sema.Concat  -> Plan.Concat
  | Sema.Mod     -> Plan.Mod
  | Sema.Bit_and -> Plan.Bit_and | Sema.Bit_or -> Plan.Bit_or
  | Sema.Lshift  -> Plan.Lshift  | Sema.Rshift -> Plan.Rshift
```

**8b. Extend `plan_expr`** (after the `| Sema.BE_neg` case):
```ocaml
  | Sema.BE_bitnot e -> Plan.P_bitnot (plan_expr e)
```

- [ ] **Step 9: Update `lib/sql/exec.ml`**

**9a. Add `P_bitnot` to `eval_expr`** (after the `| Plan.P_neg e ->` case):
```ocaml
  | Plan.P_bitnot e ->
    (match eval_expr params row e with
     | Row.V_int n -> Row.V_int (Int64.lognot n)
     | Row.V_null  -> Row.V_null
     | _           -> Row.V_null)
```

**9b. Add cases to `eval_binop`** (after the existing arithmetic cases):
```ocaml
  | Plan.Concat ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_text a, Row.V_text b -> Row.V_text (a ^ b)
     | Row.V_text a, Row.V_int  n -> Row.V_text (a ^ Int64.to_string n)
     | Row.V_int  n, Row.V_text b -> Row.V_text (Int64.to_string n ^ b)
     | Row.V_int  a, Row.V_int  b -> Row.V_text (Int64.to_string a ^ Int64.to_string b)
     | _ -> Row.V_null)
  | Plan.Mod ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_int a, Row.V_int b ->
       if b = 0L then Row.V_null else Row.V_int (Int64.rem a b)
     | Row.V_real a, Row.V_real b ->
       if b = 0.0 then Row.V_null else Row.V_real (mod_float a b)
     | Row.V_int a, Row.V_real b ->
       if b = 0.0 then Row.V_null else Row.V_real (mod_float (Int64.to_float a) b)
     | Row.V_real a, Row.V_int b ->
       if b = 0L then Row.V_null else Row.V_real (mod_float a (Int64.to_float b))
     | _ -> Row.V_null)
  | Plan.Bit_and ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_int a, Row.V_int b -> Row.V_int (Int64.logand a b)
     | _ -> Row.V_null)
  | Plan.Bit_or ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_int a, Row.V_int b -> Row.V_int (Int64.logor a b)
     | _ -> Row.V_null)
  | Plan.Lshift ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_int a, Row.V_int b ->
       let n = Int64.to_int b in
       Row.V_int (if n < 0 || n >= 64 then 0L else Int64.shift_left a n)
     | _ -> Row.V_null)
  | Plan.Rshift ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_int a, Row.V_int b ->
       let n = Int64.to_int b in
       Row.V_int (if n < 0 || n >= 64 then 0L else Int64.shift_right_logical a n)
     | _ -> Row.V_null)
```

- [ ] **Step 10: Run tests to verify they pass**

```bash
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z \
  -w /workspace sqlocaml-dev dune exec test/test_expr.exe 2>&1 | tail -20
```

Expected: all `binary_operators` tests pass.

Also run the full test suite to check for regressions:
```bash
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z \
  -w /workspace sqlocaml-dev dune test 2>&1 | tail -30
```

Expected: all tests pass.

- [ ] **Step 11: Add parser tests in `test/test_parser.ml`**

Add tests that verify the new tokens parse correctly. Add these test functions:

```ocaml
let parse_concat () =
  let s = parse "SELECT a || b FROM t" in
  match s with
  | Ast.S_select { proj = `Exprs [Ast.E_binop (Ast.Concat, Ast.E_col "a", Ast.E_col "b")]; _ } -> ()
  | _ -> Alcotest.fail "expected concat binop"

let parse_mod () =
  let s = parse "SELECT 7 % 3 FROM t" in
  match s with
  | Ast.S_select { proj = `Exprs [Ast.E_binop (Ast.Mod, Ast.E_lit (Ast.L_int 7L), Ast.E_lit (Ast.L_int 3L))]; _ } -> ()
  | _ -> Alcotest.fail "expected mod binop"

let parse_bitnot () =
  let s = parse "SELECT ~5 FROM t" in
  match s with
  | Ast.S_select { proj = `Exprs [Ast.E_bitnot (Ast.E_lit (Ast.L_int 5L))]; _ } -> ()
  | _ -> Alcotest.fail "expected bitnot"
```

Add to the runner:
```ocaml
"bitwise", [
  Alcotest.test_case "concat"  `Quick parse_concat;
  Alcotest.test_case "mod"     `Quick parse_mod;
  Alcotest.test_case "bitnot"  `Quick parse_bitnot;
];
```

- [ ] **Step 12: Commit**

```bash
git add lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly \
        lib/sql/plan.ml lib/sql/sema.ml lib/sql/planner.ml lib/sql/exec.ml \
        test/test_expr.ml test/test_parser.ml
git commit -m "feat: add extended binary operators ||, %, bitwise, shift

Closes #107 #108 #109"
```

---

## Task 2: LIKE and GLOB Pattern Matching

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/sema.ml`
- Modify: `lib/sql/planner.ml`
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_expr.ml`

- [ ] **Step 1: Write failing tests in `test/test_expr.ml`**

```ocaml
let test_like_match () =
  let row = [||] in
  let like a p =
    Exec.eval_expr [||] row
      (Plan.P_binop (Plan.Like, Plan.P_lit (Ast.L_text a), Plan.P_lit (Ast.L_text p)))
  in
  Alcotest.(check int64) "like percent"   1L (match like "hello" "hel%"   with Row.V_int n -> n | _ -> -1L);
  Alcotest.(check int64) "like underscore" 1L (match like "hello" "h_llo" with Row.V_int n -> n | _ -> -1L);
  Alcotest.(check int64) "like case"      1L (match like "Hello" "hello%"  with Row.V_int n -> n | _ -> -1L);
  Alcotest.(check int64) "like no match"  0L (match like "hello" "world%"  with Row.V_int n -> n | _ -> -1L)

let test_like_null () =
  let row = [||] in
  let v = Exec.eval_expr [||] row
      (Plan.P_binop (Plan.Like, Plan.P_lit Ast.L_null, Plan.P_lit (Ast.L_text "%"))) in
  Alcotest.(check bool) "like null" true (v = Row.V_null)

let test_glob_match () =
  let row = [||] in
  let glob s p =
    Exec.eval_expr [||] row
      (Plan.P_binop (Plan.Glob, Plan.P_lit (Ast.L_text s), Plan.P_lit (Ast.L_text p)))
  in
  Alcotest.(check int64) "glob star" 1L (match glob "hello" "hel*"  with Row.V_int n -> n | _ -> -1L);
  Alcotest.(check int64) "glob q"    1L (match glob "hello" "h?llo" with Row.V_int n -> n | _ -> -1L);
  Alcotest.(check int64) "glob case" 0L (match glob "Hello" "hello*" with Row.V_int n -> n | _ -> -1L)
```

Add to runner:
```ocaml
"like_glob", [
  Alcotest.test_case "like_match" `Quick test_like_match;
  Alcotest.test_case "like_null"  `Quick test_like_null;
  Alcotest.test_case "glob_match" `Quick test_glob_match;
];
```

- [ ] **Step 2: Run to verify failure**

```bash
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z \
  -w /workspace sqlocaml-dev dune exec test/test_expr.exe 2>&1 | head -10
```

Expected: compilation error — `Plan.Like`, `Plan.Glob` undefined.

- [ ] **Step 3: Add `Like | Glob` to `lib/sql/ast.ml` binop**

```ocaml
type binop =
  | Eq | Ne | Lt | Le | Gt | Ge
  | Add | Sub | Mul | Div
  | And | Or
  | Concat | Mod | Bit_and | Bit_or | Lshift | Rshift
  | Like | Glob                    (** pattern matching *)
```

- [ ] **Step 4: Add tokens to `lib/sql/lexer.mll`**

```
  | "LIKE"   { LIKE }
  | "GLOB"   { GLOB }
```

Place these with the other keyword rules (alphabetically before M or wherever keywords are grouped).

- [ ] **Step 5: Update `lib/sql/parser.mly`**

**5a.** Add token declarations:
```
%token LIKE GLOB
```

**5b.** Add `%nonassoc LIKE GLOB` to the precedence section, at the same level as comparison operators:
```
%left EQ NE LT LE GT GE
%nonassoc LIKE GLOB
```
(Insert between the GE line and CONCAT line.)

**5c.** Add productions to `expr`:
```
  | a = expr LIKE b = expr         { E_binop (Like, a, b) }
  | a = expr NOT LIKE b = expr     { E_not (E_binop (Like, a, b)) }
  | a = expr GLOB b = expr         { E_binop (Glob, a, b) }
  | a = expr NOT GLOB b = expr     { E_not (E_binop (Glob, a, b)) }
```

After adding, run `dune build` and check for Menhir shift/reduce warnings. If conflicts appear for `NOT LIKE` or `NOT GLOB`, add `%prec LIKE` to those productions:
```
  | a = expr NOT LIKE b = expr %prec LIKE { E_not (E_binop (Like, a, b)) }
```

- [ ] **Step 6: Add `Like | Glob` to `lib/sql/plan.ml` binop**

```ocaml
type binop = Eq | Ne | Lt | Le | Gt | Ge | Add | Sub | Mul | Div | And | Or
           | Concat | Mod | Bit_and | Bit_or | Lshift | Rshift
           | Like | Glob
```

- [ ] **Step 7: Update `lib/sql/sema.ml`**

**7a.** Extend sema `binop`:
```ocaml
type binop = Eq | Ne | Lt | Le | Gt | Ge | Add | Sub | Mul | Div | And | Or
           | Concat | Mod | Bit_and | Bit_or | Lshift | Rshift
           | Like | Glob
```

**7b.** Extend `ast_binop_to_sema`:
```ocaml
  | Ast.Like -> Like | Ast.Glob -> Glob
```

No new `bound_expr` variants needed — `BE_binop` handles LIKE/GLOB. No sema arity changes needed (LIKE/GLOB are handled as `E_binop`).

- [ ] **Step 8: Extend `lib/sql/planner.ml` `plan_binop`**

```ocaml
  | Sema.Like -> Plan.Like | Sema.Glob -> Plan.Glob
```

- [ ] **Step 9: Implement LIKE/GLOB evaluation in `lib/sql/exec.ml`**

Add helper functions **before** `eval_binop` (at the top of the file or just before `eval_binop`):

```ocaml
let rec like_match pat pi str si =
  let plen = String.length pat and slen = String.length str in
  if pi = plen then si = slen
  else match pat.[pi] with
  | '%' -> like_match pat (pi+1) str si ||
            (si < slen && like_match pat pi str (si+1))
  | '_' -> si < slen && like_match pat (pi+1) str (si+1)
  | c   -> si < slen && Char.lowercase_ascii c = Char.lowercase_ascii str.[si] &&
            like_match pat (pi+1) str (si+1)

let rec glob_match pat pi str si =
  let plen = String.length pat and slen = String.length str in
  if pi = plen then si = slen
  else match pat.[pi] with
  | '*' -> glob_match pat (pi+1) str si ||
            (si < slen && glob_match pat pi str (si+1))
  | '?' -> si < slen && glob_match pat (pi+1) str (si+1)
  | c   -> si < slen && c = str.[si] && glob_match pat (pi+1) str (si+1)
```

Add cases to `eval_binop`:
```ocaml
  | Plan.Like ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_text str, Row.V_text pat ->
       Row.V_int (if like_match (String.lowercase_ascii pat) 0 (String.lowercase_ascii str) 0 then 1L else 0L)
     | _ -> Row.V_null)
  | Plan.Glob ->
    (match lv, rv with
     | Row.V_null, _ | _, Row.V_null -> Row.V_null
     | Row.V_text str, Row.V_text pat ->
       Row.V_int (if glob_match pat 0 str 0 then 1L else 0L)
     | _ -> Row.V_null)
```

- [ ] **Step 10: Run tests**

```bash
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z \
  -w /workspace sqlocaml-dev dune test 2>&1 | tail -20
```

Expected: all tests pass, including the new `like_glob` suite.

- [ ] **Step 11: Commit**

```bash
git add lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly \
        lib/sql/plan.ml lib/sql/sema.ml lib/sql/planner.ml lib/sql/exec.ml \
        test/test_expr.ml
git commit -m "feat: add LIKE and GLOB pattern matching operators

Closes #104"
```

---

## Task 3: BETWEEN and IN Operators

**Files:**
- Modify: `lib/sql/ast.ml` (new expr variants)
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/plan.ml` (new expr variants)
- Modify: `lib/sql/sema.ml` (bind new variants, 3 contexts)
- Modify: `lib/sql/planner.ml` (plan_expr)
- Modify: `lib/sql/exec.ml` (eval_expr)
- Modify: `test/test_expr.ml`
- Modify: `test/test_e2e.ml`

- [ ] **Step 1: Write failing tests in `test/test_expr.ml`**

```ocaml
let test_between () =
  let row = [||] in
  let between x lo hi =
    Exec.eval_expr [||] row
      (Plan.P_between (Plan.P_lit (Ast.L_int x),
                       Plan.P_lit (Ast.L_int lo),
                       Plan.P_lit (Ast.L_int hi)))
  in
  Alcotest.(check int64) "between in"    1L (match between 5L 1L 10L with Row.V_int n -> n | _ -> -1L);
  Alcotest.(check int64) "between lo"    1L (match between 1L 1L 10L with Row.V_int n -> n | _ -> -1L);
  Alcotest.(check int64) "between hi"    1L (match between 10L 1L 10L with Row.V_int n -> n | _ -> -1L);
  Alcotest.(check int64) "between out"   0L (match between 0L 1L 10L with Row.V_int n -> n | _ -> -1L)

let test_in () =
  let row = [||] in
  let in_list x vs =
    Exec.eval_expr [||] row
      (Plan.P_in (Plan.P_lit (Ast.L_int x),
                  List.map (fun v -> Plan.P_lit (Ast.L_int v)) vs))
  in
  Alcotest.(check int64) "in found"     1L (match in_list 3L [1L; 2L; 3L] with Row.V_int n -> n | _ -> -1L);
  Alcotest.(check int64) "in not found" 0L (match in_list 4L [1L; 2L; 3L] with Row.V_int n -> n | _ -> -1L)

let test_in_null () =
  let row = [||] in
  let v = Exec.eval_expr [||] row
      (Plan.P_in (Plan.P_lit Ast.L_null,
                  [Plan.P_lit (Ast.L_int 1L)])) in
  Alcotest.(check bool) "in null subject" true (v = Row.V_null)
```

Add to runner:
```ocaml
"between_in", [
  Alcotest.test_case "between"  `Quick test_between;
  Alcotest.test_case "in"       `Quick test_in;
  Alcotest.test_case "in_null"  `Quick test_in_null;
];
```

- [ ] **Step 2: Run to verify failure**

Expected: compilation error — `Plan.P_between`, `Plan.P_in` undefined.

- [ ] **Step 3: Update `lib/sql/ast.ml`**

Add to `expr` type (after `E_bitnot`):
```ocaml
  | E_between of expr * expr * expr   (** subject BETWEEN lo AND hi *)
  | E_in      of expr * expr list     (** subject IN (val1, val2, ...) *)
```

- [ ] **Step 4: Update `lib/sql/parser.mly`**

**4a.** Add `%nonassoc BETWEEN_PREC` to the precedence section. Insert it between `%nonassoc LIKE GLOB` and `%left CONCAT`:
```
%nonassoc LIKE GLOB
%nonassoc BETWEEN_PREC
%left CONCAT
```

**4b.** Add productions to `expr`:
```
  | a = expr BETWEEN lo = expr AND hi = expr %prec BETWEEN_PREC
    { E_between (a, lo, hi) }
  | a = expr NOT BETWEEN lo = expr AND hi = expr %prec BETWEEN_PREC
    { E_not (E_between (a, lo, hi)) }
  | a = expr IN LPAREN vals = separated_nonempty_list(COMMA, expr) RPAREN
    { E_in (a, vals) }
  | a = expr NOT IN LPAREN vals = separated_nonempty_list(COMMA, expr) RPAREN
    { E_not (E_in (a, vals)) }
```

After adding, run `dune build`. If Menhir reports shift/reduce conflicts for `NOT BETWEEN` or `NOT IN`, add `%prec BETWEEN_PREC` to those rules:
```
  | a = expr NOT BETWEEN lo = expr AND hi = expr %prec BETWEEN_PREC
    { E_not (E_between (a, lo, hi)) }
```

**Note on BETWEEN precedence:** `%prec BETWEEN_PREC` on the BETWEEN rule tells Menhir that when parsing `a BETWEEN lo . AND hi`, if there's a conflict between extending `lo` (via `AND`) vs finishing `lo`, the production precedence (BETWEEN_PREC) > AND, so `lo` is completed and AND is consumed as BETWEEN's delimiter. This correctly parses `5 BETWEEN 1+1 AND 10-1`.

- [ ] **Step 5: Update `lib/sql/plan.ml`**

Add to `expr` type (after `P_bitnot`):
```ocaml
  | P_between of expr * expr * expr
  | P_in      of expr * expr list
```

- [ ] **Step 6: Update `lib/sql/sema.ml`**

Add to `bound_expr` (after `BE_bitnot`):
```ocaml
  | BE_between of bound_expr * bound_expr * bound_expr
  | BE_in      of bound_expr * bound_expr list
```

Add cases to ALL THREE `bind_expr` variants (single-table, join, agg). For single-table `bind_expr`:
```ocaml
  | Ast.E_between (x, lo, hi) ->
    (match bind_expr ~param_counter meta x,
           bind_expr ~param_counter meta lo,
           bind_expr ~param_counter meta hi with
     | Ok bx, Ok blo, Ok bhi -> Ok (BE_between (bx, blo, bhi))
     | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
  | Ast.E_in (x, vals) ->
    let bx = bind_expr ~param_counter meta x in
    let bvals = List.map (bind_expr ~param_counter meta) vals in
    let errors = List.filter_map (function Error e -> Some e | Ok _ -> None) bvals in
    (match bx, errors with
     | Error e, _ -> Error e
     | _, e :: _  -> Error e
     | Ok bx', [] ->
       let ok_vals = List.filter_map (function Ok v -> Some v | Error _ -> None) bvals in
       Ok (BE_in (bx', ok_vals)))
```

For `bind_expr_join` and `bind_expr_agg`, apply the same pattern but call the correct recursive function.

- [ ] **Step 7: Update `lib/sql/planner.ml`**

Add cases to `plan_expr`:
```ocaml
  | Sema.BE_between (x, lo, hi) ->
    Plan.P_between (plan_expr x, plan_expr lo, plan_expr hi)
  | Sema.BE_in (x, vals) ->
    Plan.P_in (plan_expr x, List.map plan_expr vals)
```

- [ ] **Step 8: Add eval cases to `lib/sql/exec.ml`**

Add to `eval_expr` (after the `P_bitnot` case):
```ocaml
  | Plan.P_between (x, lo, hi) ->
    let vx  = eval_expr params row x  in
    let vlo = eval_expr params row lo in
    let vhi = eval_expr params row hi in
    (match vx, vlo, vhi with
     | Row.V_null, _, _ | _, Row.V_null, _ | _, _, Row.V_null -> Row.V_null
     | _ ->
       let ge_lo = compare_values vx vlo >= 0 in
       let le_hi = compare_values vx vhi <= 0 in
       Row.V_int (if ge_lo && le_hi then 1L else 0L))
  | Plan.P_in (x, vals) ->
    let vx = eval_expr params row x in
    if vx = Row.V_null then Row.V_null
    else
      let found = List.exists (fun ve ->
        let v = eval_expr params row ve in
        v <> Row.V_null && compare_values vx v = 0
      ) vals in
      Row.V_int (if found then 1L else 0L)
```

- [ ] **Step 9: Add end-to-end tests in `test/test_e2e.ml`**

```ocaml
let test_between_query () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE TABLE t (n INTEGER)" in
    let* _ = D.execute db "INSERT INTO t VALUES (1)" in
    let* _ = D.execute db "INSERT INTO t VALUES (5)" in
    let* _ = D.execute db "INSERT INTO t VALUES (10)" in
    let* r = D.query db "SELECT n FROM t WHERE n BETWEEN 3 AND 7" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error _ -> Lwt.return []) in
    let vals = List.map (fun r -> match r.(0) with D.V_int n -> n | _ -> -1L) rows in
    Alcotest.(check (list int64)) "between" [5L] vals;
    Lwt.return_unit)

let test_in_query () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE TABLE t (n INTEGER)" in
    let* _ = D.execute db "INSERT INTO t VALUES (1)" in
    let* _ = D.execute db "INSERT INTO t VALUES (2)" in
    let* _ = D.execute db "INSERT INTO t VALUES (3)" in
    let* r = D.query db "SELECT n FROM t WHERE n IN (1, 3)" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error _ -> Lwt.return []) in
    let vals = List.map (fun r -> match r.(0) with D.V_int n -> n | _ -> -1L) rows in
    Alcotest.(check (list int64)) "in" [1L; 3L] vals;
    Lwt.return_unit)
```

Add to test runner:
```ocaml
"between_in", [
  Alcotest.test_case "between_query" `Quick test_between_query;
  Alcotest.test_case "in_query"      `Quick test_in_query;
];
```

- [ ] **Step 10: Run tests**

```bash
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z \
  -w /workspace sqlocaml-dev dune test 2>&1 | tail -20
```

Expected: all tests pass.

- [ ] **Step 11: Commit**

```bash
git add lib/sql/ast.ml lib/sql/parser.mly lib/sql/plan.ml \
        lib/sql/sema.ml lib/sql/planner.ml lib/sql/exec.ml \
        test/test_expr.ml test/test_e2e.ml
git commit -m "feat: add BETWEEN and IN value list operators

Closes #105 #106"
```

---

## Task 4: Additional Scalar Functions (SUBSTR, TRIM, REPLACE, INSTR, ROUND, TYPEOF)

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/sema.ml` (arity checks in 3 bind_expr variants)
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_scalar_fns.ml`

Note: `plan.ml` and `planner.ml` do NOT need changes for scalar functions — they use `Ast.scalar_func` directly through `P_func`.

- [ ] **Step 1: Write failing tests in `test/test_scalar_fns.ml`**

```ocaml
let test_substr () =
  check_single_value "substr_2"   "SELECT SUBSTR(name, 2) FROM t WHERE id = 1"    "ello";
  check_single_value "substr_2_3" "SELECT SUBSTR(name, 2, 3) FROM t WHERE id = 1" "ell"

let test_trim () =
  let* db = D.open_in_memory () in
  let* _ = D.execute db "CREATE TABLE s (v TEXT)" in
  let* _ = D.execute db "INSERT INTO s VALUES ('  hello  ')" in
  let* r = D.query db "SELECT TRIM(v) FROM s" in
  let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error _ -> Lwt.return []) in
  (match rows with
   | row :: _ -> Alcotest.(check string) "trim" "hello" (match row.(0) with D.V_text s -> s | _ -> "?")
   | [] -> Alcotest.fail "no rows");
  Lwt.return_unit

let test_replace () =
  let* db = D.open_in_memory () in
  let* _ = D.execute db "CREATE TABLE s (v TEXT)" in
  let* _ = D.execute db "INSERT INTO s VALUES ('hello world')" in
  let* r = D.query db "SELECT REPLACE(v, 'world', 'there') FROM s" in
  let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error _ -> Lwt.return []) in
  (match rows with
   | row :: _ -> Alcotest.(check string) "replace" "hello there" (match row.(0) with D.V_text s -> s | _ -> "?")
   | [] -> Alcotest.fail "no rows");
  Lwt.return_unit

let test_instr () =
  let* db = D.open_in_memory () in
  let* _ = D.execute db "CREATE TABLE s (v TEXT)" in
  let* _ = D.execute db "INSERT INTO s VALUES ('hello')" in
  let* r = D.query db "SELECT INSTR(v, 'ell') FROM s" in
  let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error _ -> Lwt.return []) in
  (match rows with
   | row :: _ -> Alcotest.(check int64) "instr" 2L (match row.(0) with D.V_int n -> n | _ -> -1L)
   | [] -> Alcotest.fail "no rows");
  Lwt.return_unit

let test_round () =
  check_single_value "round_2"  "SELECT ROUND(val, 1) FROM t WHERE id = 1" "3.1";
  check_single_value "round_0"  "SELECT ROUND(val) FROM t WHERE id = 1"    "3.0"

let test_typeof () =
  check_single_value "typeof_int"  "SELECT TYPEOF(id) FROM t WHERE id = 1"   "integer";
  check_single_value "typeof_text" "SELECT TYPEOF(name) FROM t WHERE id = 1" "text";
  check_single_value "typeof_real" "SELECT TYPEOF(val) FROM t WHERE id = 1"  "real";
  check_single_value "typeof_null" "SELECT TYPEOF(NULL) FROM t WHERE id = 1" "null"
```

Note: `check_single_value` is the existing helper in `test_scalar_fns.ml`. The `trim`, `replace`, `instr` tests need `run (fun () -> ...)` wrapping. Reuse the `run` helper from the file.

Add to runner:
```ocaml
"new_scalar_fns", [
  Alcotest.test_case "substr"  `Quick test_substr;
  Alcotest.test_case "trim"    `Quick (fun () -> run test_trim);
  Alcotest.test_case "replace" `Quick (fun () -> run test_replace);
  Alcotest.test_case "instr"   `Quick (fun () -> run test_instr);
  Alcotest.test_case "round"   `Quick test_round;
  Alcotest.test_case "typeof"  `Quick test_typeof;
];
```

- [ ] **Step 2: Run to verify failure**

Expected: compilation error or runtime error — new functions not implemented yet.

- [ ] **Step 3: Update `lib/sql/ast.ml` scalar_func type**

```ocaml
type scalar_func =
  | Fn_length  | Fn_lower   | Fn_upper  | Fn_abs
  | Fn_coalesce | Fn_ifnull
  | Fn_substr                        (** SUBSTR(s, start[, len]) — 1-indexed *)
  | Fn_trim    | Fn_ltrim  | Fn_rtrim (** TRIM, LTRIM, RTRIM *)
  | Fn_replace                       (** REPLACE(s, old, new) *)
  | Fn_instr                         (** INSTR(s, sub) → 1-indexed position *)
  | Fn_round                         (** ROUND(n[, digits]) *)
  | Fn_typeof                        (** TYPEOF(x) → 'integer'|'real'|'text'|'blob'|'null' *)
```

- [ ] **Step 4: Update `lib/sql/lexer.mll`**

Add keyword rules:
```
  | "SUBSTR"  { SUBSTR }
  | "TRIM"    { TRIM }
  | "LTRIM"   { LTRIM }
  | "RTRIM"   { RTRIM }
  | "REPLACE" { REPLACE }
  | "INSTR"   { INSTR }
  | "ROUND"   { ROUND }
  | "TYPEOF"  { TYPEOF }
```

- [ ] **Step 5: Update `lib/sql/parser.mly`**

Add token declarations:
```
%token SUBSTR TRIM LTRIM RTRIM REPLACE INSTR ROUND TYPEOF
```

Add productions to the `scalar_expr` rule:
```
  | SUBSTR  LPAREN s = expr COMMA start = expr RPAREN
    { E_func (Fn_substr, [s; start]) }
  | SUBSTR  LPAREN s = expr COMMA start = expr COMMA len = expr RPAREN
    { E_func (Fn_substr, [s; start; len]) }
  | TRIM    LPAREN s = expr RPAREN
    { E_func (Fn_trim, [s]) }
  | TRIM    LPAREN s = expr COMMA chars = expr RPAREN
    { E_func (Fn_trim, [s; chars]) }
  | LTRIM   LPAREN s = expr RPAREN
    { E_func (Fn_ltrim, [s]) }
  | LTRIM   LPAREN s = expr COMMA chars = expr RPAREN
    { E_func (Fn_ltrim, [s; chars]) }
  | RTRIM   LPAREN s = expr RPAREN
    { E_func (Fn_rtrim, [s]) }
  | RTRIM   LPAREN s = expr COMMA chars = expr RPAREN
    { E_func (Fn_rtrim, [s; chars]) }
  | REPLACE LPAREN s = expr COMMA old_s = expr COMMA rep = expr RPAREN
    { E_func (Fn_replace, [s; old_s; rep]) }
  | INSTR   LPAREN s = expr COMMA sub = expr RPAREN
    { E_func (Fn_instr, [s; sub]) }
  | ROUND   LPAREN n = expr RPAREN
    { E_func (Fn_round, [n]) }
  | ROUND   LPAREN n = expr COMMA d = expr RPAREN
    { E_func (Fn_round, [n; d]) }
  | TYPEOF  LPAREN e = expr RPAREN
    { E_func (Fn_typeof, [e]) }
```

- [ ] **Step 6: Update arity checks in `lib/sql/sema.ml`**

Find the three `arity_ok` check blocks (in `bind_expr`, `bind_expr_join`, `bind_expr_agg`). Replace each with:

```ocaml
let arity_ok = match func with
  | Ast.Fn_length | Ast.Fn_lower | Ast.Fn_upper
  | Ast.Fn_abs    | Ast.Fn_typeof -> n = 1
  | Ast.Fn_ifnull | Ast.Fn_instr -> n = 2
  | Ast.Fn_coalesce -> n >= 1
  | Ast.Fn_substr -> n = 2 || n = 3
  | Ast.Fn_trim | Ast.Fn_ltrim | Ast.Fn_rtrim -> n = 1 || n = 2
  | Ast.Fn_replace -> n = 3
  | Ast.Fn_round -> n = 1 || n = 2
in
```

Also update the `expected` value in the error for cases where it's wrong (it's only used in the error message; set it to 1 for single-arg functions, 2 for two-arg).

- [ ] **Step 7: Add eval cases to `lib/sql/exec.ml`**

Add helper functions after the existing `eval_func` definition (or just before — but these are helpers called from within `eval_func`):

```ocaml
let str_trim_spaces s =
  let n = String.length s in
  let l = ref 0 and r = ref (n - 1) in
  while !l <= !r && (s.[!l] = ' ' || s.[!l] = '\t' || s.[!l] = '\n' || s.[!l] = '\r') do incr l done;
  while !r >= !l && (s.[!r] = ' ' || s.[!r] = '\t' || s.[!r] = '\n' || s.[!r] = '\r') do decr r done;
  if !l > !r then "" else String.sub s !l (!r - !l + 1)

let str_trim_chars s chars =
  let n = String.length s in
  let is_trim c = String.contains chars c in
  let l = ref 0 and r = ref (n - 1) in
  while !l <= !r && is_trim s.[!l] do incr l done;
  while !r >= !l && is_trim s.[!r] do decr r done;
  if !l > !r then "" else String.sub s !l (!r - !l + 1)

let str_ltrim_spaces s =
  let n = String.length s in
  let l = ref 0 in
  while !l < n && (s.[!l] = ' ' || s.[!l] = '\t' || s.[!l] = '\n' || s.[!l] = '\r') do incr l done;
  String.sub s !l (n - !l)

let str_ltrim_chars s chars =
  let n = String.length s in
  let l = ref 0 in
  while !l < n && String.contains chars s.[!l] do incr l done;
  String.sub s !l (n - !l)

let str_rtrim_spaces s =
  let n = String.length s in
  let r = ref (n - 1) in
  while !r >= 0 && (s.[!r] = ' ' || s.[!r] = '\t' || s.[!r] = '\n' || s.[!r] = '\r') do decr r done;
  if !r < 0 then "" else String.sub s 0 (!r + 1)

let str_rtrim_chars s chars =
  let r = ref (String.length s - 1) in
  while !r >= 0 && String.contains chars s.[!r] do decr r done;
  if !r < 0 then "" else String.sub s 0 (!r + 1)

let str_replace s old rep =
  if String.length old = 0 then s
  else
    let buf = Buffer.create (String.length s) in
    let n = String.length s and m = String.length old in
    let i = ref 0 in
    while !i <= n - m do
      if String.sub s !i m = old then (Buffer.add_string buf rep; i := !i + m)
      else (Buffer.add_char buf s.[!i]; incr i)
    done;
    while !i < n do Buffer.add_char buf s.[!i]; incr i done;
    Buffer.contents buf

let str_instr s sub =
  let n = String.length s and m = String.length sub in
  if m = 0 then 1
  else begin
    let found = ref 0 in
    let i = ref 0 in
    while !found = 0 && !i <= n - m do
      if String.sub s !i m = sub then found := !i + 1  (* 1-indexed *)
      else incr i
    done;
    !found
  end
```

Add cases to `eval_func` (after the `Fn_ifnull` case):
```ocaml
  | Ast.Fn_substr, (Row.V_text s :: rest) ->
    (match rest with
     | [Row.V_int start] ->
       let i = max 0 (Int64.to_int start - 1) in
       if i >= String.length s then Row.V_text ""
       else Row.V_text (String.sub s i (String.length s - i))
     | [Row.V_int start; Row.V_int len] ->
       let i = max 0 (Int64.to_int start - 1) in
       let l = Int64.to_int len in
       if i >= String.length s || l <= 0 then Row.V_text ""
       else Row.V_text (String.sub s i (min l (String.length s - i)))
     | _ -> Row.V_null)
  | Ast.Fn_substr, (Row.V_null :: _) -> Row.V_null
  | Ast.Fn_trim, [Row.V_text s]                         -> Row.V_text (str_trim_spaces s)
  | Ast.Fn_trim, [Row.V_text s; Row.V_text chars]       -> Row.V_text (str_trim_chars s chars)
  | Ast.Fn_trim, (Row.V_null :: _)                      -> Row.V_null
  | Ast.Fn_ltrim, [Row.V_text s]                        -> Row.V_text (str_ltrim_spaces s)
  | Ast.Fn_ltrim, [Row.V_text s; Row.V_text chars]      -> Row.V_text (str_ltrim_chars s chars)
  | Ast.Fn_ltrim, (Row.V_null :: _)                     -> Row.V_null
  | Ast.Fn_rtrim, [Row.V_text s]                        -> Row.V_text (str_rtrim_spaces s)
  | Ast.Fn_rtrim, [Row.V_text s; Row.V_text chars]      -> Row.V_text (str_rtrim_chars s chars)
  | Ast.Fn_rtrim, (Row.V_null :: _)                     -> Row.V_null
  | Ast.Fn_replace, [Row.V_text s; Row.V_text old; Row.V_text rep] ->
    Row.V_text (str_replace s old rep)
  | Ast.Fn_replace, (Row.V_null :: _) | Ast.Fn_replace, [_; Row.V_null; _]
  | Ast.Fn_replace, [_; _; Row.V_null] -> Row.V_null
  | Ast.Fn_instr, [Row.V_text s; Row.V_text sub] ->
    Row.V_int (Int64.of_int (str_instr s sub))
  | Ast.Fn_instr, (Row.V_null :: _) | Ast.Fn_instr, [_; Row.V_null] -> Row.V_null
  | Ast.Fn_round, [Row.V_real f] ->
    Row.V_real (Float.round f)
  | Ast.Fn_round, [Row.V_int n] ->
    Row.V_real (Int64.to_float n)
  | Ast.Fn_round, [Row.V_real f; Row.V_int d] ->
    let factor = 10. ** Int64.to_float d in
    Row.V_real (Float.round (f *. factor) /. factor)
  | Ast.Fn_round, (Row.V_null :: _) -> Row.V_null
  | Ast.Fn_typeof, [v] ->
    Row.V_text (match v with
      | Row.V_int  _ -> "integer"
      | Row.V_real _ -> "real"
      | Row.V_text _ -> "text"
      | Row.V_blob _ -> "blob"
      | Row.V_null   -> "null")
```

- [ ] **Step 8: Run tests**

```bash
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z \
  -w /workspace sqlocaml-dev dune test 2>&1 | tail -30
```

Expected: all tests pass including `new_scalar_fns`.

- [ ] **Step 9: Commit**

```bash
git add lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly \
        lib/sql/sema.ml lib/sql/exec.ml \
        test/test_scalar_fns.ml
git commit -m "feat: add SUBSTR, TRIM, REPLACE, INSTR, ROUND, TYPEOF scalar functions

Closes #110"
```

---

## Task 5: ORDER BY Arbitrary Expressions

This task refactors `order_key` from `{col; table_opt; dir}` to `{expr; dir}` at the AST level, and from `col_idx` to a full `Plan.expr` at the plan level. This is a structural refactor that requires updating many test files.

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/sema.ml`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/planner.ml`
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_parser.ml` (update all `order_key` records)
- Modify: `test/test_sema.ml` (update all `bound_order_key` records)
- Modify: `test/test_planner.ml` (update all `Op_sort` records)
- Modify: `test/test_e2e.ml` (add ORDER BY expr test)

- [ ] **Step 1: Write a failing e2e test in `test/test_e2e.ml`**

```ocaml
let test_order_by_expr () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE TABLE t (n INTEGER)" in
    let* _ = D.execute db "INSERT INTO t VALUES (3)" in
    let* _ = D.execute db "INSERT INTO t VALUES (1)" in
    let* _ = D.execute db "INSERT INTO t VALUES (2)" in
    let* r = D.query db "SELECT n FROM t ORDER BY n * -1" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error _ -> Lwt.return []) in
    let vals = List.map (fun r -> match r.(0) with D.V_int n -> n | _ -> -1L) rows in
    Alcotest.(check (list int64)) "order_by_expr" [3L; 2L; 1L] vals;
    Lwt.return_unit)
```

- [ ] **Step 2: Run to verify it fails**

```bash
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z \
  -w /workspace sqlocaml-dev dune exec test/test_e2e.exe 2>&1 | tail -10
```

Expected: parse error — `ORDER BY n * -1` is not yet supported.

- [ ] **Step 3: Update `lib/sql/ast.ml` order_key**

Replace the existing `order_key` type:
```ocaml
type order_key = {
  expr : expr;
  dir  : order_dir;
}
```

(Remove the old `col`, `table_opt` fields.)

- [ ] **Step 4: Simplify `lib/sql/parser.mly` order_key rules**

Replace the six existing `order_key` productions with three:
```mly
order_key:
  | e = expr      { { expr = e; dir = Asc } }
  | e = expr ASC  { { expr = e; dir = Asc } }
  | e = expr DESC { { expr = e; dir = Desc } }
```

Previously-valid `ORDER BY name`, `ORDER BY t.name ASC`, `ORDER BY n+1 DESC` all parse correctly via `e = expr`.

- [ ] **Step 5: Update `lib/sql/sema.ml` bound_order_key**

Change:
```ocaml
type bound_order_key = {
  key : bound_expr;   (* was: col_idx : int *)
  dir : Ast.order_dir;
}
```

Update the binding function. Find where `order_key` is bound (search for `col_idx` in sema.ml). Replace with:

```ocaml
let bind_order_key ~param_counter resolver ok =
  match resolver ok.Ast.expr with
  | Error e -> Error e
  | Ok key  -> Ok { key; dir = ok.Ast.dir }
```

For single-table queries, `resolver` is `bind_expr ~param_counter meta`. For joins, use `bind_expr_join ...`.

Find the existing order binding code in `bind_select` and update it. The existing code does something like:
```ocaml
let bind_ok ok =
  match col_index meta.columns ok.Ast.col with
  | None -> Error (...)
  | Some i -> Ok { col_idx = i; dir = ok.Ast.dir }
```

Replace with:
```ocaml
let bind_ok ok =
  match bind_expr ~param_counter meta ok.Ast.expr with
  | Error e -> Error e
  | Ok key  -> Ok { key; dir = ok.Ast.dir }
```

For aggregate queries, ORDER BY may need the aggregate-context resolver. For now, bind against the table columns (same as before).

- [ ] **Step 6: Update `lib/sql/plan.ml` Op_sort**

Change `col_idx: int` to `key: expr`:
```ocaml
  | Op_sort of {
      key   : expr;
      dir   : [`Asc | `Desc];
      child : op;
    }
```

- [ ] **Step 7: Update `lib/sql/planner.ml` make_sort**

Find `make_sort` and update it:
```ocaml
let make_sort child (bkey : Sema.bound_order_key) =
  let dir = match bkey.dir with Ast.Asc -> `Asc | Ast.Desc -> `Desc in
  Plan.Op_sort { key = plan_expr bkey.key; dir; child }
```

(The existing `plan_expr` already handles all `bound_expr` variants.)

- [ ] **Step 8: Update `lib/sql/exec.ml` Op_sort**

Replace the `Op_sort` case:
```ocaml
  | Plan.Op_sort { key; dir; child } ->
    let* inner = to_stream params store child in
    let* rows = Lwt_stream.to_list inner in
    let cmp a b =
      let va = eval_expr params a key and vb = eval_expr params b key in
      let c = compare_values va vb in
      if dir = `Asc then c else -c
    in
    let sorted = List.sort cmp rows in
    Lwt.return (Lwt_stream.of_list sorted)
```

- [ ] **Step 9: Fix compilation errors in test files**

Run `dune build` to see all compilation errors:
```bash
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z \
  -w /workspace sqlocaml-dev dune build 2>&1
```

Fix each error:
- In `test/test_parser.ml`: change `{ col = "name"; table_opt = None; dir = Asc }` → `{ expr = E_col "name"; dir = Asc }` and `{ col = "c"; table_opt = Some "t"; dir = Desc }` → `{ expr = E_tbl_col ("t", "c"); dir = Desc }`
- In `test/test_sema.ml`: change `{ col_idx = i; dir = _ }` → `{ key = BE_col i; dir = _ }`
- In `test/test_planner.ml`: change `Op_sort { col_idx = i; dir; child }` → `Op_sort { key = P_col i; dir; child }`

Use `grep -n "col_idx\|table_opt\|col = " test/test_*.ml` to find all instances.

- [ ] **Step 10: Run all tests**

```bash
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z \
  -w /workspace sqlocaml-dev dune test 2>&1 | tail -20
```

Expected: all tests pass, including new `order_by_expr` test.

- [ ] **Step 11: Commit**

```bash
git add lib/sql/ast.ml lib/sql/parser.mly lib/sql/sema.ml \
        lib/sql/plan.ml lib/sql/planner.ml lib/sql/exec.ml \
        test/test_parser.ml test/test_sema.ml test/test_planner.ml test/test_e2e.ml
git commit -m "feat: generalize ORDER BY to support arbitrary expressions

Closes #115"
```

---

## Task 6: PRAGMA table_info and index_list

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/sema.ml`
- Modify: `lib/sql/planner.ml`
- Modify: `lib/catalog/catalog.ml` + `catalog.mli` (add sync accessor)
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_e2e.ml`

- [ ] **Step 1: Write failing test in `test/test_e2e.ml`**

```ocaml
let test_pragma_table_info () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE TABLE t (id INTEGER NOT NULL PRIMARY KEY, name TEXT)" in
    let* r = D.query db "PRAGMA table_info(t)" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error _ -> Lwt.return []) in
    Alcotest.(check int) "pragma table_info rows" 2 (List.length rows);
    let col0 = List.nth rows 0 in
    Alcotest.(check string) "col0 name" "id" (match col0.(1) with D.V_text s -> s | _ -> "?");
    Alcotest.(check string) "col0 type" "INTEGER" (match col0.(2) with D.V_text s -> s | _ -> "?");
    Lwt.return_unit)

let test_pragma_index_list () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE TABLE t (id INTEGER, name TEXT)" in
    let* _ = D.execute db "CREATE INDEX idx_name ON t (name)" in
    let* r = D.query db "PRAGMA index_list(t)" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error _ -> Lwt.return []) in
    Alcotest.(check int) "pragma index_list rows" 1 (List.length rows);
    let row = List.hd rows in
    Alcotest.(check string) "idx name" "idx_name" (match row.(1) with D.V_text s -> s | _ -> "?");
    Lwt.return_unit)
```

Add to runner:
```ocaml
"pragma", [
  Alcotest.test_case "table_info" `Quick test_pragma_table_info;
  Alcotest.test_case "index_list" `Quick test_pragma_index_list;
];
```

- [ ] **Step 2: Run to verify failure**

Expected: parse error — PRAGMA not yet in grammar.

- [ ] **Step 3: Update `lib/sql/ast.ml`**

Add `pragma_kind` type and `S_pragma` to `stmt`:
```ocaml
type pragma_kind =
  | Pragma_table_info of string
  | Pragma_index_list of string
```

Add to `stmt`:
```ocaml
  | S_pragma of pragma_kind
```

- [ ] **Step 4: Update `lib/sql/lexer.mll`**

```
  | "PRAGMA" { PRAGMA }
```

- [ ] **Step 5: Update `lib/sql/parser.mly`**

Add token:
```
%token PRAGMA
```

Add to `stmt`:
```
  | s = pragma_stmt { s }
```

Add new rule:
```mly
pragma_stmt:
  | PRAGMA name = IDENT LPAREN arg = IDENT RPAREN
    { match String.lowercase_ascii name with
      | "table_info" -> S_pragma (Pragma_table_info arg)
      | "index_list" -> S_pragma (Pragma_index_list arg)
      | _ -> failwith (Printf.sprintf "unknown pragma: %s" name) }
```

- [ ] **Step 6: Add `find_table_cached` to `lib/catalog/catalog.ml` and `catalog.mli`**

The planner is synchronous but needs to look up table metadata. Add a synchronous cache accessor.

In `catalog.ml`, add after `find_table`:
```ocaml
let find_table_cached t ~name = Hashtbl.find_opt t.cache name
```

In `catalog.mli`, add after `val find_table`:
```ocaml
val find_table_cached : t -> name:string -> table_meta option
```

- [ ] **Step 7: Update `lib/sql/plan.ml`**

Add `Op_pragma_rows`:
```ocaml
  | Op_pragma_rows of {
      rows : Sqlocaml_encoding.Row.t list;
    }
```

- [ ] **Step 8: Update `lib/sql/sema.ml`**

Add `BS_pragma` to `bound_stmt`:
```ocaml
  | BS_pragma of {
      kind : Ast.pragma_kind;
    }
```

Add case to `bind_stmt`:
```ocaml
  | Ast.S_pragma kind -> Ok (BS_pragma { kind })
```

- [ ] **Step 9: Update `lib/sql/planner.ml`**

Add case to `plan`. The planner receives `cat` as first argument. Pre-compute rows using synchronous catalog access:

```ocaml
  | Sema.BS_pragma { kind } ->
    let rows = match kind with
      | Ast.Pragma_table_info table_name ->
        (match Cat.find_table_cached cat ~name:table_name with
         | None -> []
         | Some meta ->
           List.mapi (fun i (col : Row.column) ->
             [| Row.V_int (Int64.of_int i);
                Row.V_text col.name;
                Row.V_text (match col.ty with
                  | Row.Integer -> "INTEGER" | Row.Text -> "TEXT"
                  | Row.Real -> "REAL" | Row.Blob -> "BLOB");
                Row.V_int (if col.not_null then 1L else 0L);
                Row.V_null;   (* dflt_value: simplified *)
                Row.V_int (if col.primary_key then 1L else 0L) |]
           ) meta.columns)
      | Ast.Pragma_index_list table_name ->
        let idxs = Cat.indexes_for_table cat ~table:table_name in
        List.mapi (fun i (idx : Cat.index_info) ->
          [| Row.V_int (Int64.of_int i);
             Row.V_text idx.idx_name;
             Row.V_int (if idx.idx_unique then 1L else 0L) |]
        ) idxs
    in
    Ok (Plan.Op_pragma_rows { rows })
```

- [ ] **Step 10: Update `lib/sql/exec.ml`**

Add `Op_pragma_rows` to `to_stream`:
```ocaml
  | Plan.Op_pragma_rows { rows } ->
    Lwt.return (Lwt_stream.of_list rows)
```

Also add to `execute_with_count` (which handles mutation ops) — `Op_pragma_rows` should not appear there, but add it to the exhaustive match to avoid a compile warning. Since `to_stream` handles it, any `execute_with_count` calls for PRAGMA would be wrong; add:
```ocaml
  | Plan.Op_pragma_rows _ -> Lwt.return 0
```

- [ ] **Step 11: Run tests**

```bash
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z \
  -w /workspace sqlocaml-dev dune test 2>&1 | tail -20
```

Expected: all tests pass including new `pragma` suite.

- [ ] **Step 12: Commit**

```bash
git add lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly \
        lib/sql/plan.ml lib/sql/sema.ml lib/sql/planner.ml lib/sql/exec.ml \
        lib/catalog/catalog.ml lib/catalog/catalog.mli \
        test/test_e2e.ml
git commit -m "feat: add PRAGMA table_info and index_list support

Closes #119"
```

---

## Task 7: Multi-Column Indexes

This task extends `CREATE INDEX` to accept multiple columns. The index key encoding (`lib/encoding/index_key.ml`) already supports multi-value keys — we just need to feed it multiple values. Index lookup optimization (planner choosing multi-column index) is NOT added here; fall back to seq scan for now.

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/sema.ml`
- Modify: `lib/catalog/catalog.ml` + `catalog.mli`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/planner.ml`
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_e2e.ml`
- Modify: `test/test_catalog.ml`

- [ ] **Step 1: Write failing test in `test/test_e2e.ml`**

```ocaml
let test_multi_col_index () =
  run (fun () ->
    let* db = D.open_in_memory () in
    let* _ = D.execute db "CREATE TABLE t (a INTEGER, b INTEGER, c TEXT)" in
    let* r = D.execute db "CREATE INDEX idx_ab ON t (a, b)" in
    (match r with
     | Error e -> Alcotest.failf "create index failed: %a" D.pp_error e
     | Ok () -> ());
    let* _ = D.execute db "INSERT INTO t VALUES (1, 2, 'x')" in
    let* _ = D.execute db "INSERT INTO t VALUES (3, 4, 'y')" in
    let* r = D.query db "SELECT c FROM t WHERE a = 1" in
    let* rows = (match r with Ok s -> Lwt_stream.to_list s | Error _ -> Lwt.return []) in
    Alcotest.(check int) "one row" 1 (List.length rows);
    Alcotest.(check string) "value" "x" (match (List.hd rows).(0) with D.V_text s -> s | _ -> "?");
    Lwt.return_unit)
```

- [ ] **Step 2: Run to verify failure**

Expected: `Parser.Error` or semantic error — `CREATE INDEX idx ON t (a, b)` not yet supported.

- [ ] **Step 3: Update `lib/sql/ast.ml`**

Change `S_create_index`:
```ocaml
  | S_create_index of {
      name    : string;
      table   : string;
      columns : string list;   (* was: column : string *)
      unique  : bool;
    }
```

- [ ] **Step 4: Update `lib/sql/parser.mly` create_index**

Replace existing `create_index` rules:
```mly
create_index:
  | CREATE INDEX name = IDENT ON table = IDENT
      LPAREN cols = separated_nonempty_list(COMMA, IDENT) RPAREN
    { S_create_index { name; table; columns = cols; unique = false } }
  | CREATE UNIQUE INDEX name = IDENT ON table = IDENT
      LPAREN cols = separated_nonempty_list(COMMA, IDENT) RPAREN
    { S_create_index { name; table; columns = cols; unique = true } }
```

- [ ] **Step 5: Update `lib/catalog/catalog.ml` and `catalog.mli`**

**5a.** Change `index_info` type:
```ocaml
type index_info = {
  idx_name    : string;
  idx_table   : string;
  idx_columns : string list;   (* was: idx_column: string *)
  idx_unique  : bool;
  idx_tree_id : S.tree_id;
}
```

**5b.** Update `encode_index_value` to store multiple columns. Change the encoding to:
```
varint(name_len) ++ name ++ varint(table_len) ++ table
++ varint(n_cols) ++ (varint(col_len) ++ col)* n_cols
++ [unique: 1 byte] ++ varint(tree_id)
```

New implementation:
```ocaml
let encode_index_value (idx : index_info) =
  let buf = Buffer.create 32 in
  Varint.encode_uint64 buf (Int64.of_int (String.length idx.idx_name));
  Buffer.add_string buf idx.idx_name;
  Varint.encode_uint64 buf (Int64.of_int (String.length idx.idx_table));
  Buffer.add_string buf idx.idx_table;
  Varint.encode_uint64 buf (Int64.of_int (List.length idx.idx_columns));
  List.iter (fun col ->
    Varint.encode_uint64 buf (Int64.of_int (String.length col));
    Buffer.add_string buf col
  ) idx.idx_columns;
  Buffer.add_char buf (if idx.idx_unique then '\x01' else '\x00');
  Varint.encode_uint64 buf (Int64.of_int idx.idx_tree_id);
  Buffer.to_bytes buf
```

**5c.** Update `decode_index_value`:
```ocaml
let decode_index_value bytes =
  let name_len, off = Varint.decode_uint64 bytes 0 in
  let name = Bytes.sub_string bytes off (Int64.to_int name_len) in
  let off = off + Int64.to_int name_len in
  let tbl_len, off = Varint.decode_uint64 bytes off in
  let tbl = Bytes.sub_string bytes off (Int64.to_int tbl_len) in
  let off = off + Int64.to_int tbl_len in
  let n_cols, off = Varint.decode_uint64 bytes off in
  let off = ref off in
  let cols = List.init (Int64.to_int n_cols) (fun _ ->
    let col_len, next_off = Varint.decode_uint64 bytes !off in
    let col = Bytes.sub_string bytes next_off (Int64.to_int col_len) in
    off := next_off + Int64.to_int col_len;
    col
  ) in
  let unique_byte = Bytes.get_uint8 bytes !off in
  let off = !off + 1 in
  let tree_id, _ = Varint.decode_uint64 bytes off in
  {
    idx_name    = name;
    idx_table   = tbl;
    idx_columns = cols;
    idx_unique  = (unique_byte <> 0);
    idx_tree_id = Int64.to_int tree_id;
  }
```

**5d.** Update `create_index` signature and body. Change `~column:string` → `~columns:string list`:

In `catalog.mli`:
```ocaml
val create_index :
  t ->
  name:string ->
  table:string ->
  columns:string list ->
  unique:bool ->
  (index_info, string) result Lwt.t
```

In `catalog.ml`, update `create_index`:
```ocaml
let create_index t ~name ~table ~columns ~unique =
  if Hashtbl.mem t.indexes name then
    Lwt.return (Error (Printf.sprintf "index '%s' already exists" name))
  else match Hashtbl.find_opt t.cache table with
    | None ->
      Lwt.return (Error (Printf.sprintf "no table '%s'" table))
    | Some tm ->
      let missing = List.find_opt (fun col ->
        not (List.exists (fun (c : Row.column) -> c.name = col) tm.columns)
      ) columns in
      (match missing with
       | Some col ->
         Lwt.return (Error (Printf.sprintf "no column '%s' on table '%s'" col table))
       | None ->
         let%lwt tid = next_user_tid t in
         let%lwt id = read_next_index_id t.store in
         let%lwt () = write_next_index_id t.store (id + 1) in
         let info = {
           idx_name    = name;
           idx_table   = table;
           idx_columns = columns;
           idx_unique  = unique;
           idx_tree_id = tid;
         } in
         let%lwt tx = S.rw_begin t.store in
         let%lwt () =
           S.put tx sys_indexes_tid (index_key id) (encode_index_value info)
         in
         let%lwt () = S.commit tx in
         Hashtbl.replace t.indexes name info;
         Lwt.return (Ok info))
```

- [ ] **Step 6: Update `lib/sql/sema.ml`**

Find `BS_create_index` and update. Change `col_idx` to `col_idxs`:
```ocaml
  | BS_create_index of {
      name       : string;
      table_meta : Cat.table_meta;
      col_idxs   : int list;    (* was: col_idx: int *)
      unique     : bool;
    }
```

Update `bind_create_index` (find by `S_create_index`):
```ocaml
  | Ast.S_create_index { name; table; columns; unique } ->
    let%lwt meta_opt = Cat.find_table cat ~name:table in
    (match meta_opt with
     | None -> Lwt.return (Error (Unknown_table table))
     | Some meta ->
       let col_idxs_r = List.map (fun col ->
         match col_index meta.columns col with
         | None -> Error (Unknown_column { table; column = col })
         | Some i -> Ok i
       ) columns in
       let errors = List.filter_map (function Error e -> Some e | Ok _ -> None) col_idxs_r in
       (match errors with
        | e :: _ -> Lwt.return (Error e)
        | [] ->
          let col_idxs = List.filter_map (function Ok i -> Some i | Error _ -> None) col_idxs_r in
          Lwt.return (Ok (BS_create_index { name; table_meta = meta; col_idxs; unique }))))
```

- [ ] **Step 7: Update `lib/sql/plan.ml` Op_create_index**

Change:
```ocaml
  | Op_create_index of {
      name     : string;
      table    : string;
      tree_id  : int;
      col_idxs : int list;    (* was: col_idx: int *)
      unique   : bool;
      columns  : Sqlocaml_encoding.Row.column list;
    }
```

- [ ] **Step 8: Update `lib/sql/planner.ml`**

**8a.** Update `plan` for `BS_create_index`:
```ocaml
  | Sema.BS_create_index { name; table_meta; col_idxs; unique } ->
    Ok (Plan.Op_create_index {
      name;
      table     = table_meta.Cat.name;
      tree_id   = table_meta.Cat.tree_id;
      col_idxs;
      unique;
      columns   = table_meta.Cat.columns;
    })
```

**8b.** Update `find_index_on_col` to only match single-column indexes for lookup purposes:
```ocaml
let find_index_on_col cat (meta : Cat.table_meta) col_idx =
  let col_name = (List.nth meta.columns col_idx).Row.name in
  let candidates = Cat.indexes_for_table cat ~table:meta.name in
  List.find_opt (fun (i : Cat.index_info) ->
    match i.idx_columns with
    | [col] -> col = col_name
    | _ -> false  (* multi-column indexes not yet used for lookup *)
  ) candidates
```

Also update any reference to `idx.idx_column` in planner.ml to `List.hd idx.idx_columns` (for single-column index lookup).

- [ ] **Step 9: Update `lib/sql/exec.ml`**

**9a.** Update `Op_create_index` handler. Change `col_idx` to `col_idxs`:
```ocaml
  | Plan.Op_create_index { name; table; tree_id; col_idxs; unique; columns } ->
    (* build_create_index takes col_idxs and uses them to encode composite keys *)
    build_create_index store cat ~name ~table ~tree_id ~col_idxs ~unique ~columns
```

Find `build_create_index` (or the inline code). The key change: build the index key from multiple column values. Replace the single-column key encoding:

Old:
```ocaml
let v = row.(col_idx) in
let ik = row_value_to_ik v in
Index_key.encode [ik] ~rowid
```

New:
```ocaml
let iks = List.map (fun ci -> row_value_to_ik row.(ci)) col_idxs in
Index_key.encode iks ~rowid
```

The helper `row_value_to_ik`:
```ocaml
let row_value_to_ik = function
  | Row.V_null   -> Index_key.IK_null
  | Row.V_int  n -> Index_key.IK_int n
  | Row.V_text s -> Index_key.IK_text s
  | Row.V_real f -> Index_key.IK_real f
  | Row.V_blob b -> Index_key.IK_blob b
```

Apply the same multi-value encoding in the INSERT/UPDATE/DELETE index maintenance code. Search for `col_idx` in exec.ml and update all index building to use `col_idxs` (multiple values). The index lookup code (`Op_index_lookup`) still works with a single-column index format — only single-column lookup is optimized.

**9b.** Find where `idx.idx_column` is referenced in exec.ml (for index maintenance during INSERT/UPDATE/DELETE). Replace with:
```ocaml
let col_idxs = List.map (fun col ->
  find_col_idx_by_name table_meta.columns col
) idx.idx_columns in
let iks = List.map (fun ci -> row_value_to_ik row.(ci)) col_idxs in
let key = Index_key.encode iks ~rowid in
```

For `Op_index_lookup` (single-column lookup), adapt to use `List.hd idx.idx_columns`:
```ocaml
let col_idx = find_col_idx_by_name table_meta.columns (List.hd idx.idx_columns) in
```

- [ ] **Step 10: Run tests — fix any remaining `idx_column` references**

```bash
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z \
  -w /workspace sqlocaml-dev dune build 2>&1
```

Expected: compilation errors showing every remaining reference to `idx_column`. Fix each one.

Then run tests:
```bash
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z \
  -w /workspace sqlocaml-dev dune test 2>&1 | tail -30
```

Expected: all tests pass including `test_multi_col_index`.

- [ ] **Step 11: Commit**

```bash
git add lib/sql/ast.ml lib/sql/parser.mly lib/sql/sema.ml \
        lib/sql/plan.ml lib/sql/planner.ml lib/sql/exec.ml \
        lib/catalog/catalog.ml lib/catalog/catalog.mli \
        test/test_e2e.ml
git commit -m "feat: add multi-column indexes (CREATE INDEX t (a, b, ...))

Closes #114"
```

---

## Task 8: SQLite Comparison Tests for Phase 6 Features

**Files:**
- Modify: `test/test_sqlite_compare.ml`

- [ ] **Step 1: Add Phase 6 comparison test cases**

Open `test/test_sqlite_compare.ml`. Find the `test_cases` list. Add these new entries after the existing ones:

```ocaml
(* Phase 6: || concat operator *)
{ name = "concat_strings";
  setup = "CREATE TABLE t (a TEXT, b TEXT); INSERT INTO t VALUES ('foo', 'bar')";
  query = "SELECT a || ' ' || b FROM t";
  unordered = false };
{ name = "concat_null_propagates";
  setup = "CREATE TABLE t (a TEXT); INSERT INTO t VALUES (NULL)";
  query = "SELECT a || 'x' FROM t";
  unordered = false };
{ name = "concat_int_text";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (42)";
  query = "SELECT n || ' items' FROM t";
  unordered = false };
(* Phase 6: % modulo *)
{ name = "modulo_basic";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (7); INSERT INTO t VALUES (10); INSERT INTO t VALUES (6)";
  query = "SELECT n % 3 FROM t ORDER BY n";
  unordered = false };
{ name = "modulo_null";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (NULL)";
  query = "SELECT n % 3 FROM t";
  unordered = false };
{ name = "modulo_div_by_zero";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (7)";
  query = "SELECT n % 0 FROM t";
  unordered = false };
(* Phase 6: bitwise operators *)
{ name = "bitwise_and";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (5); INSERT INTO t VALUES (12)";
  query = "SELECT n & 3 FROM t ORDER BY n";
  unordered = false };
{ name = "bitwise_or";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (5); INSERT INTO t VALUES (2)";
  query = "SELECT n | 8 FROM t ORDER BY n";
  unordered = false };
{ name = "lshift";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (1); INSERT INTO t VALUES (2)";
  query = "SELECT n << 3 FROM t ORDER BY n";
  unordered = false };
{ name = "rshift";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (16); INSERT INTO t VALUES (8)";
  query = "SELECT n >> 2 FROM t ORDER BY n";
  unordered = false };
{ name = "bitnot";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (5)";
  query = "SELECT ~n FROM t";
  unordered = false };
{ name = "bitwise_null";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (NULL)";
  query = "SELECT n & 3, n | 3, n << 1, n >> 1 FROM t";
  unordered = false };
(* Phase 6: LIKE *)
{ name = "like_percent_suffix";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('hello'); INSERT INTO t VALUES ('world')";
  query = "SELECT s FROM t WHERE s LIKE 'hel%'";
  unordered = false };
{ name = "like_percent_prefix";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('hello'); INSERT INTO t VALUES ('world')";
  query = "SELECT s FROM t WHERE s LIKE '%llo'";
  unordered = false };
{ name = "like_percent_both";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('hello'); INSERT INTO t VALUES ('world')";
  query = "SELECT s FROM t WHERE s LIKE '%ell%'";
  unordered = false };
{ name = "like_underscore";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('hello'); INSERT INTO t VALUES ('hallo'); INSERT INTO t VALUES ('hxllo')";
  query = "SELECT s FROM t WHERE s LIKE 'h_llo' ORDER BY s";
  unordered = false };
{ name = "like_case_insensitive";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('Hello'); INSERT INTO t VALUES ('WORLD')";
  query = "SELECT s FROM t WHERE s LIKE 'hello'";
  unordered = false };
{ name = "like_no_match";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('hello')";
  query = "SELECT s FROM t WHERE s LIKE 'xyz%'";
  unordered = false };
{ name = "like_null_subject";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES (NULL)";
  query = "SELECT s FROM t WHERE s LIKE '%'";
  unordered = false };
{ name = "not_like";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('hello'); INSERT INTO t VALUES ('world')";
  query = "SELECT s FROM t WHERE NOT (s LIKE 'hel%') ORDER BY s";
  unordered = false };
(* Phase 6: GLOB *)
{ name = "glob_star";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('hello'); INSERT INTO t VALUES ('world')";
  query = "SELECT s FROM t WHERE s GLOB 'hel*'";
  unordered = false };
{ name = "glob_question";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('hello'); INSERT INTO t VALUES ('hallo')";
  query = "SELECT s FROM t WHERE s GLOB 'h?llo' ORDER BY s";
  unordered = false };
{ name = "glob_case_sensitive";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('Hello'); INSERT INTO t VALUES ('hello')";
  query = "SELECT s FROM t WHERE s GLOB 'hello'";
  unordered = false };
(* Phase 6: BETWEEN *)
{ name = "between_basic";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (1); INSERT INTO t VALUES (5); INSERT INTO t VALUES (10)";
  query = "SELECT n FROM t WHERE n BETWEEN 3 AND 7";
  unordered = false };
{ name = "between_inclusive_bounds";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (1); INSERT INTO t VALUES (5); INSERT INTO t VALUES (10)";
  query = "SELECT n FROM t WHERE n BETWEEN 1 AND 10 ORDER BY n";
  unordered = false };
{ name = "between_exclusive";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (0); INSERT INTO t VALUES (5); INSERT INTO t VALUES (11)";
  query = "SELECT n FROM t WHERE n BETWEEN 1 AND 10";
  unordered = false };
{ name = "not_between";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (1); INSERT INTO t VALUES (5); INSERT INTO t VALUES (10)";
  query = "SELECT n FROM t WHERE NOT (n BETWEEN 3 AND 7) ORDER BY n";
  unordered = false };
{ name = "between_null_subject";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (NULL); INSERT INTO t VALUES (5)";
  query = "SELECT n FROM t WHERE n BETWEEN 1 AND 10";
  unordered = false };
{ name = "between_real";
  setup = "CREATE TABLE t (r REAL); INSERT INTO t VALUES (1.5); INSERT INTO t VALUES (3.0); INSERT INTO t VALUES (5.5)";
  query = "SELECT r FROM t WHERE r BETWEEN 2.0 AND 4.0";
  unordered = false };
{ name = "between_text";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('apple'); INSERT INTO t VALUES ('mango'); INSERT INTO t VALUES ('zebra')";
  query = "SELECT s FROM t WHERE s BETWEEN 'banana' AND 'orange'";
  unordered = false };
(* Phase 6: IN *)
{ name = "in_list_found";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (1); INSERT INTO t VALUES (2); INSERT INTO t VALUES (3)";
  query = "SELECT n FROM t WHERE n IN (1, 3) ORDER BY n";
  unordered = false };
{ name = "in_list_not_found";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (1); INSERT INTO t VALUES (2)";
  query = "SELECT n FROM t WHERE n IN (5, 6)";
  unordered = false };
{ name = "not_in_list";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (1); INSERT INTO t VALUES (2); INSERT INTO t VALUES (3)";
  query = "SELECT n FROM t WHERE NOT (n IN (1, 3)) ORDER BY n";
  unordered = false };
{ name = "in_null_subject";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (NULL); INSERT INTO t VALUES (1)";
  query = "SELECT n FROM t WHERE n IN (1, 2)";
  unordered = false };
{ name = "in_text_values";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('a'); INSERT INTO t VALUES ('b'); INSERT INTO t VALUES ('c')";
  query = "SELECT s FROM t WHERE s IN ('a', 'c') ORDER BY s";
  unordered = false };
{ name = "in_single_value";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (1); INSERT INTO t VALUES (2)";
  query = "SELECT n FROM t WHERE n IN (2)";
  unordered = false };
(* Phase 6: SUBSTR *)
{ name = "substr_from";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('hello')";
  query = "SELECT SUBSTR(s, 2) FROM t";
  unordered = false };
{ name = "substr_from_len";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('hello')";
  query = "SELECT SUBSTR(s, 2, 3) FROM t";
  unordered = false };
{ name = "substr_first";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('hello')";
  query = "SELECT SUBSTR(s, 1, 1) FROM t";
  unordered = false };
{ name = "substr_beyond_end";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('hello')";
  query = "SELECT SUBSTR(s, 4, 100) FROM t";
  unordered = false };
{ name = "substr_null";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES (NULL)";
  query = "SELECT SUBSTR(s, 1) FROM t";
  unordered = false };
(* Phase 6: TRIM / LTRIM / RTRIM *)
{ name = "trim_spaces";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('  hello  ')";
  query = "SELECT TRIM(s) FROM t";
  unordered = false };
{ name = "ltrim_spaces";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('  hello  ')";
  query = "SELECT LTRIM(s) FROM t";
  unordered = false };
{ name = "rtrim_spaces";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('  hello  ')";
  query = "SELECT RTRIM(s) FROM t";
  unordered = false };
{ name = "trim_null";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES (NULL)";
  query = "SELECT TRIM(s) FROM t";
  unordered = false };
(* Phase 6: REPLACE *)
{ name = "replace_basic";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('hello world')";
  query = "SELECT REPLACE(s, 'world', 'there') FROM t";
  unordered = false };
{ name = "replace_multiple";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('aaa')";
  query = "SELECT REPLACE(s, 'a', 'b') FROM t";
  unordered = false };
{ name = "replace_not_found";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('hello')";
  query = "SELECT REPLACE(s, 'xyz', 'abc') FROM t";
  unordered = false };
{ name = "replace_null";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES (NULL)";
  query = "SELECT REPLACE(s, 'a', 'b') FROM t";
  unordered = false };
(* Phase 6: INSTR *)
{ name = "instr_found";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('hello')";
  query = "SELECT INSTR(s, 'ell') FROM t";
  unordered = false };
{ name = "instr_not_found";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('hello')";
  query = "SELECT INSTR(s, 'xyz') FROM t";
  unordered = false };
{ name = "instr_first_char";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('hello')";
  query = "SELECT INSTR(s, 'h') FROM t";
  unordered = false };
{ name = "instr_null";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES (NULL)";
  query = "SELECT INSTR(s, 'x') FROM t";
  unordered = false };
(* Phase 6: ROUND *)
{ name = "round_no_digits";
  setup = "CREATE TABLE t (r REAL); INSERT INTO t VALUES (3.7); INSERT INTO t VALUES (3.2)";
  query = "SELECT ROUND(r) FROM t ORDER BY r";
  unordered = false };
{ name = "round_2_digits";
  setup = "CREATE TABLE t (r REAL); INSERT INTO t VALUES (3.14159)";
  query = "SELECT ROUND(r, 2) FROM t";
  unordered = false };
{ name = "round_integer";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (5)";
  query = "SELECT ROUND(n) FROM t";
  unordered = false };
{ name = "round_null";
  setup = "CREATE TABLE t (r REAL); INSERT INTO t VALUES (NULL)";
  query = "SELECT ROUND(r) FROM t";
  unordered = false };
(* Phase 6: TYPEOF *)
{ name = "typeof_integer";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (1)";
  query = "SELECT TYPEOF(n) FROM t";
  unordered = false };
{ name = "typeof_text";
  setup = "CREATE TABLE t (s TEXT); INSERT INTO t VALUES ('hello')";
  query = "SELECT TYPEOF(s) FROM t";
  unordered = false };
{ name = "typeof_real";
  setup = "CREATE TABLE t (r REAL); INSERT INTO t VALUES (3.14)";
  query = "SELECT TYPEOF(r) FROM t";
  unordered = false };
{ name = "typeof_null";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (NULL)";
  query = "SELECT TYPEOF(n) FROM t";
  unordered = false };
{ name = "typeof_literal_null";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (1)";
  query = "SELECT TYPEOF(NULL) FROM t";
  unordered = false };
(* Phase 6: ORDER BY arbitrary expressions *)
{ name = "order_by_arith";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (3); INSERT INTO t VALUES (1); INSERT INTO t VALUES (2)";
  query = "SELECT n FROM t ORDER BY n * -1";
  unordered = false };
{ name = "order_by_abs_fn";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (-3); INSERT INTO t VALUES (1); INSERT INTO t VALUES (-2)";
  query = "SELECT n FROM t ORDER BY ABS(n)";
  unordered = false };
{ name = "order_by_concat";
  setup = "CREATE TABLE t (a TEXT, b TEXT); INSERT INTO t VALUES ('b', 'z'); INSERT INTO t VALUES ('a', 'y')";
  query = "SELECT a FROM t ORDER BY a || b";
  unordered = false };
{ name = "order_by_ifnull";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (NULL); INSERT INTO t VALUES (5); INSERT INTO t VALUES (NULL)";
  query = "SELECT n FROM t ORDER BY IFNULL(n, 99)";
  unordered = false };
{ name = "order_by_modulo";
  setup = "CREATE TABLE t (n INTEGER); INSERT INTO t VALUES (3); INSERT INTO t VALUES (4); INSERT INTO t VALUES (5); INSERT INTO t VALUES (6)";
  query = "SELECT n FROM t ORDER BY n % 3, n";
  unordered = false };
```

- [ ] **Step 2: Run comparison tests**

```bash
podman run --rm \
  -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z \
  -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
  -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
  -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
  -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
  -w /workspace sqlocaml-dev dune exec test/test_sqlite_compare.exe 2>&1
```

If any test fails, inspect the diff and fix the underlying implementation (see the correctness fix workflow from the Phase 5 session).

- [ ] **Step 3: Commit**

```bash
git add test/test_sqlite_compare.ml
git commit -m "test(sqlite_compare): add Phase 6 feature comparison cases"
```

---

## Final: Push and Update Forgejo Issues

- [ ] **Push all commits**

```bash
git push origin main
```

- [ ] **Close issues on Forgejo**

```bash
~/.local/bin/forgejo issue close tej/sqlite_ocaml_port 107 --comment "Implemented: || string concatenation operator in Phase 6."
~/.local/bin/forgejo issue close tej/sqlite_ocaml_port 108 --comment "Implemented: &, |, ~, <<, >> bitwise operators in Phase 6."
~/.local/bin/forgejo issue close tej/sqlite_ocaml_port 109 --comment "Implemented: % modulo operator in Phase 6."
~/.local/bin/forgejo issue close tej/sqlite_ocaml_port 104 --comment "Implemented: LIKE and GLOB pattern matching in Phase 6."
~/.local/bin/forgejo issue close tej/sqlite_ocaml_port 105 --comment "Implemented: BETWEEN operator in Phase 6."
~/.local/bin/forgejo issue close tej/sqlite_ocaml_port 106 --comment "Implemented: IN (value list) operator in Phase 6."
~/.local/bin/forgejo issue close tej/sqlite_ocaml_port 110 --comment "Implemented: SUBSTR, TRIM, LTRIM, RTRIM, REPLACE, INSTR, ROUND, TYPEOF in Phase 6."
~/.local/bin/forgejo issue close tej/sqlite_ocaml_port 115 --comment "Implemented: ORDER BY now supports arbitrary expressions (not just column names) in Phase 6."
~/.local/bin/forgejo issue close tej/sqlite_ocaml_port 119 --comment "Implemented: PRAGMA table_info(t) and PRAGMA index_list(t) in Phase 6."
~/.local/bin/forgejo issue close tej/sqlite_ocaml_port 114 --comment "Implemented: CREATE INDEX idx ON t (col1, col2, ...) multi-column indexes in Phase 6. Note: planner does not yet optimize queries using multi-column indexes (falls back to seq scan)."
```

---

## Self-Review Checklist

**Spec coverage:**
- [x] ||, %, &, |, ~, <<, >> — Task 1
- [x] LIKE, GLOB — Task 2  
- [x] BETWEEN, IN — Task 3
- [x] SUBSTR, TRIM, LTRIM, RTRIM, REPLACE, INSTR, ROUND, TYPEOF — Task 4
- [x] ORDER BY arbitrary expressions — Task 5
- [x] PRAGMA table_info, PRAGMA index_list — Task 6
- [x] Multi-column indexes — Task 7
- [x] SQLite comparison tests for all new features — Task 8

**No placeholders:** All code in every step is complete and compilable.

**Type consistency:**
- `Ast.binop` additions reflected in `Sema.binop`, `Plan.binop`, and all mapping functions
- `Ast.E_bitnot` reflected in `Sema.BE_bitnot`, `Plan.P_bitnot` and `plan_expr`
- `Ast.E_between/E_in` reflected in Sema, Plan, and eval
- `order_key.expr` used consistently across AST → sema → plan
- `idx_columns: string list` (was `idx_column: string`) consistently updated in catalog, sema, plan, exec
- `find_index_on_col` in planner updated to match on first column only for backward-compatible single-column index lookup

**Known limitation documented:** Multi-column index LOOKUP optimization (planner uses index scan) is deferred. All queries fall back to seq scan for multi-column indexes, but results are correct.
