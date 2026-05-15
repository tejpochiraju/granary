# Phase 2 — Query Depth Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add UPDATE, DELETE, expression comparisons, INNER/LEFT JOIN, GROUP BY, HAVING, and the five standard aggregates (COUNT, SUM, AVG, MIN, MAX), turning sqlocaml into a database capable of realistic CRUD and analytical queries.

**Architecture:** Each feature is a vertical slice through the same four-layer pipeline — Lexer → Parser → Sema → Planner → Exec. UPDATE and DELETE are mutation operators that use the existing rw_txn. JOINs introduce a new physical operator type (NestedLoopJoin for indexed joins, HashJoin for non-indexed). GROUP BY + aggregates are a new accumulation operator that materializes sorted groups and emits one row per group. All operators remain Lwt-coloured; Phase 3 will add explicit transaction demarcation.

**Architecture ref:** `docs/specs/2026-05-10-design.md` — sections 7 (SQL layer), 7.3 (planner rules), 7.4 (exec operators).

**Phase 1 outcome:** CoW B+-tree on Unix_file, CREATE TABLE / INSERT / SELECT (WHERE, ORDER BY, LIMIT, OFFSET) / CREATE INDEX / REAL / BLOB, ~657 tests, 90.20% handwritten coverage.

**Tech Stack:** OCaml 5.x · dune 3.x · alcotest · qcheck-alcotest · menhir · lwt · Podman (all builds in container)

**Testing discipline:** 100% behavioral coverage goal. Every module gets Alcotest unit tests + QCheck property tests. Slow and thorough — completeness over speed. **QCheck: 10,000 trials per property.**

---

## Build environment (unchanged from Phase 0/1)

```bash
podman run --rm -v "$(pwd)":/workspace:Z -w /workspace sqlocaml-dev dune runtest
```

Shell wrapper (add to session):
```bash
dune() { podman run --rm -v "$(pwd)":/workspace:Z -w /workspace sqlocaml-dev dune "$@"; }
export -f dune
```

---

## Phase 2 scope — what changes

| Layer | Changes |
|---|---|
| `lib/sql/ast.ml` | `S_update`, `S_delete`; richer `expr` (comparisons, arithmetic, `AND`/`OR`/`NOT`, `IS NULL`, `IS NOT NULL`, qualified `table.col`); `S_select` gains `joins`, `group_by`, `having` |
| `lib/sql/lexer.mll` | UPDATE, SET, DELETE, FROM, JOIN, INNER, LEFT, OUTER, ON, AND, OR, NOT, IS, NULL (bare keyword), GROUP, HAVING, COUNT, SUM, AVG, MIN, MAX, `!=`/`<>`/`<`/`>`/`<=`/`>=`, `+`/`-`/`*`/`/` |
| `lib/sql/parser.mly` | productions for all above |
| `lib/sql/sema.ml` | bind UPDATE/DELETE; resolve qualified column refs; type-check arithmetic/comparison; validate GROUP BY cols; validate aggregate call sites |
| `lib/sql/plan.ml` | `Op_update`, `Op_delete`, `Op_nested_loop_join`, `Op_hash_join`, `Op_aggregate` |
| `lib/sql/planner.ml` | UPDATE/DELETE planning; JOIN planning rules; GROUP BY → Op_aggregate |
| `lib/sql/exec.ml` | execute Op_update, Op_delete, Op_nested_loop_join, Op_hash_join, Op_aggregate |
| `test/` | new test files per task |

---

## Phase 2 file structure

New test files (existing files extended in-place for SQL layer):

```
test/
├── test_update.ml          (new — unit + e2e for UPDATE)
├── test_delete.ml          (new — unit + e2e for DELETE)
├── test_expr.ml            (new — extended expression tests)
├── test_join.ml            (new — JOIN unit + e2e)
├── test_aggregate.ml       (new — GROUP BY / HAVING / aggregate tests)
```

Existing test files extended:
- `test/test_lexer.ml` — new keyword and operator tokens
- `test/test_parser.ml` — new grammar productions
- `test/test_sema.ml` — new sema validations
- `test/test_planner.ml` — new plan shapes
- `test/test_exec.ml` — new operator execution
- `test/test_e2e.ml` — integration tests per feature
- `test/test_conformance.ml` — conformance cases for new features on both backends

---

## Tasks

---

### Task 1: Richer expression language

**Context:** Phase 1 expressions are limited to `E_eq` and `E_col`/`E_lit`. Phase 2 SQL requires comparisons (`<`, `>`, `<=`, `>=`, `!=`), logical connectives (`AND`, `OR`, `NOT`), null checks (`IS NULL`, `IS NOT NULL`), and arithmetic (`+`, `-`, `*`, `/`). This is the foundation everything else in Phase 2 builds on.

**Files to modify:**
- `lib/sql/ast.ml` — extend `expr`
- `lib/sql/lexer.mll` — new tokens
- `lib/sql/parser.mly` — new productions and precedence
- `lib/sql/sema.ml` — extend `bind_expr`, `bound_expr`
- `lib/sql/plan.ml` — extend `Plan.expr`
- `lib/sql/exec.ml` — extend `eval_expr`

**`ast.ml` — new `expr` constructors:**

```ocaml
type binop =
  | Eq | Ne | Lt | Le | Gt | Ge   (* comparison *)
  | Add | Sub | Mul | Div          (* arithmetic *)
  | And | Or                       (* logical *)

type expr =
  | E_lit    of literal
  | E_col    of string                   (* unqualified: col *)
  | E_tbl_col of string * string         (* qualified: table.col *)
  | E_binop  of binop * expr * expr
  | E_not    of expr
  | E_is_null    of expr
  | E_is_not_null of expr
  | E_neg    of expr                     (* unary minus *)
```

**`lexer.mll` — new tokens:**
Add (uppercase only):
- `AND`, `OR`, `NOT`, `IS`, `NULL` (bare), `IN`
- Operators: `!=` or `<>` → `NE`, `<` → `LT`, `>` → `GT`, `<=` → `LE`, `>=` → `GE`
- Arithmetic: `+` → `PLUS`, `-` → `MINUS`, `*` → `STAR`, `/` → `SLASH`
- Note: `NULL` keyword is already lexed as `NULL` in the literal. `IS` is new.

Check the existing lexer for any conflicts before adding. `*` in `SELECT *` is already lexed; reuse the `STAR` token if it exists or rename.

**`parser.mly` — expression grammar with precedence:**

Add `%left OR`, `%left AND`, `%right NOT`, `%left LT GT LE GE NE EQ`, `%left PLUS MINUS`, `%left STAR SLASH`, `%nonassoc UMINUS` precedence levels (lowest to highest).

```menhir
expr:
  | literal                          { E_lit $1 }
  | IDENT                            { E_col $1 }
  | IDENT DOT IDENT                  { E_tbl_col ($1, $3) }
  | expr AND expr                    { E_binop (And, $1, $3) }
  | expr OR expr                     { E_binop (Or, $1, $3) }
  | NOT expr                         { E_not $2 }
  | expr EQ  expr                    { E_binop (Eq, $1, $3) }
  | expr NE  expr                    { E_binop (Ne, $1, $3) }
  | expr LT  expr                    { E_binop (Lt, $1, $3) }
  | expr LE  expr                    { E_binop (Le, $1, $3) }
  | expr GT  expr                    { E_binop (Gt, $1, $3) }
  | expr GE  expr                    { E_binop (Ge, $1, $3) }
  | expr PLUS  expr                  { E_binop (Add, $1, $3) }
  | expr MINUS expr                  { E_binop (Sub, $1, $3) }
  | expr STAR  expr                  { E_binop (Mul, $1, $3) }
  | expr SLASH expr                  { E_binop (Div, $1, $3) }
  | MINUS expr %prec UMINUS          { E_neg $2 }
  | expr IS NULL                     { E_is_null $1 }
  | expr IS NOT NULL                 { E_is_not_null $1 }
  | LPAREN expr RPAREN               { $2 }
```

**`sema.ml` — extend `bound_expr` and `bind_expr`:**

```ocaml
type binop = Eq | Ne | Lt | Le | Gt | Ge | Add | Sub | Mul | Div | And | Or

type bound_expr =
  | BE_lit        of Ast.literal
  | BE_col        of int
  | BE_binop      of binop * bound_expr * bound_expr
  | BE_not        of bound_expr
  | BE_is_null    of bound_expr
  | BE_is_not_null of bound_expr
  | BE_neg        of bound_expr
```

Type checking rules for `bind_expr`:
- Arithmetic (`Add/Sub/Mul/Div`): both operands must be numeric (`Ty_int` or `Ty_real`). If one is `Ty_real`, result is `Ty_real`; else `Ty_int`. NULL bypasses.
- Comparisons: both operands same type (or NULL on either side → allow; result is bool-ish `Ty_int`).
- `AND`/`OR`/`NOT`: operands are treated as boolean (any type accepted; 0/NULL = false, else true).
- `IS NULL` / `IS NOT NULL`: any type; result is bool (`Ty_int`).
- `E_neg`: operand must be numeric.

**`Plan.expr` — extend to match:**

```ocaml
type binop = Eq | Ne | Lt | Le | Gt | Ge | Add | Sub | Mul | Div | And | Or

type expr =
  | P_lit        of Ast.literal
  | P_col        of int
  | P_binop      of binop * expr * expr
  | P_not        of expr
  | P_is_null    of expr
  | P_is_not_null of expr
  | P_neg        of expr
```

**`exec.ml` — extend `eval_expr`:**

```ocaml
let rec eval_expr row = function
  | P_lit l -> lit_to_value l
  | P_col i -> row.(i)
  | P_neg e ->
    (match eval_expr row e with
     | V_int n  -> V_int (Int64.neg n)
     | V_real f -> V_real (-. f)
     | V_null   -> V_null
     | _ -> failwith "unary minus on non-numeric")
  | P_is_null e ->
    (match eval_expr row e with V_null -> V_int 1L | _ -> V_int 0L)
  | P_is_not_null e ->
    (match eval_expr row e with V_null -> V_int 0L | _ -> V_int 1L)
  | P_not e ->
    if value_truthy (eval_expr row e) then V_int 0L else V_int 1L
  | P_binop (op, l, r) ->
    eval_binop op (eval_expr row l) (eval_expr row r)

and eval_binop op lv rv =
  match op with
  | And -> if value_truthy lv && value_truthy rv then V_int 1L else V_int 0L
  | Or  -> if value_truthy lv || value_truthy rv then V_int 1L else V_int 0L
  | Eq  -> if compare_values lv rv = 0 && lv <> V_null then V_int 1L else V_int 0L
  | Ne  -> if compare_values lv rv <> 0 && lv <> V_null && rv <> V_null then V_int 1L else V_int 0L
  | Lt  -> cmp_op lv rv ( < )
  | Le  -> cmp_op lv rv ( <= )
  | Gt  -> cmp_op lv rv ( > )
  | Ge  -> cmp_op lv rv ( >= )
  | Add -> arith_op lv rv Int64.add ( +. )
  | Sub -> arith_op lv rv Int64.sub ( -. )
  | Mul -> arith_op lv rv Int64.mul ( *. )
  | Div -> arith_op lv rv
             (fun a b -> if b = 0L then failwith "division by zero" else Int64.div a b)
             ( /. )
```

`compare_values` already exists (from Op_sort). Reuse it. For `Eq` comparison: NULL = anything is false (SQL semantics).

- [ ] **Step 1:** Read current `ast.ml`, `lexer.mll`, `parser.mly`, `sema.ml`, `plan.ml`, `exec.ml` fully.
- [ ] **Step 2:** Extend `lib/sql/ast.ml` with `binop` type and new `expr` constructors.
- [ ] **Step 3:** Extend `lib/sql/lexer.mll` — add AND, OR, NOT, IS, NE (`!=` and `<>`), LT, GT, LE, GE, PLUS, MINUS (note: MINUS already exists for negative literals — check how to reconcile), STAR (check if `*` already has a token), SLASH, DOT.
- [ ] **Step 4:** Extend `lib/sql/parser.mly` — add precedence declarations, expression grammar. Ensure `SELECT *` still works (STAR is the wildcard).
- [ ] **Step 5:** Extend `lib/sql/sema.ml` — add `bound_expr` variants, update `bind_expr` with type checking for all new forms.
- [ ] **Step 6:** Extend `lib/sql/plan.ml` — add `binop` type and new `Plan.expr` constructors.
- [ ] **Step 7:** Update `lib/sql/planner.ml` — translate `Sema.bound_expr` to `Plan.expr` for new forms.
- [ ] **Step 8:** Extend `lib/sql/exec.ml` — implement `eval_expr` and `eval_binop` for new forms.
- [ ] **Step 9:** `dune runtest` — all existing ~657 tests still pass.
- [ ] **Step 10:** Create `test/test_expr.ml` and add to `test/dune`:
  - Unit: `NOT (1 = 1)` → false
  - Unit: `1 < 2` → true; `2 < 1` → false
  - Unit: `NULL < 5` → NULL (false in filter context)
  - Unit: `1 + 2` → `V_int 3L`
  - Unit: `3.0 * 2.0` → `V_real 6.0`
  - Unit: `1 + NULL` → `V_null`
  - Unit: `-5` unary negation → `V_int (-5L)`
  - Unit: `col IS NULL` on null value → true
  - Unit: `col IS NOT NULL` on non-null → true
  - Unit: `1 = 1 AND 2 = 2` → true
  - Unit: `1 = 2 OR 3 = 3` → true
  - Unit: `0 = 0 AND 1 = 2` → false
  - Unit: division by zero → raises Failure
  - QCheck: `a < b ↔ compare_values a b < 0` for random int pairs
  - QCheck: `a + b` arithmetic round-trip for int64 pairs
- [ ] **Step 11:** Extend `test/test_lexer.ml` — new tokens
- [ ] **Step 12:** Extend `test/test_parser.ml` — parse `WHERE a > 5 AND b IS NOT NULL`
- [ ] **Step 13:** Extend `test/test_e2e.ml` — `SELECT * FROM t WHERE n > 2 AND n < 5`
- [ ] **Step 14:** `dune runtest` — all pass.
- [ ] **Step 15:** Commit: `feat(sql): rich expression language — AND/OR/NOT/comparisons/arithmetic/IS NULL`.

---

### Task 2: UPDATE

**Context:** `UPDATE table SET col = expr [, col = expr]* [WHERE expr]`. This is a mutation operator: it scans the table, applies the WHERE filter, and for each matching row writes an updated version back (old row deleted, new row inserted with the same rowid). Index entries must also be updated.

**Files to modify:**
- `lib/sql/ast.ml` — add `S_update`
- `lib/sql/lexer.mll` — add UPDATE, SET keywords
- `lib/sql/parser.mly` — UPDATE production
- `lib/sql/sema.ml` — `BS_update`, bind assignments
- `lib/sql/plan.ml` — `Op_update`
- `lib/sql/planner.ml` — plan UPDATE
- `lib/sql/exec.ml` — execute Op_update
- Create: `test/test_update.ml`
- Update: `test/dune`

**`ast.ml`:**
```ocaml
| S_update of {
    table       : string;
    assignments : (string * expr) list;   (* (col_name, new_value_expr) list *)
    where       : expr option;
  }
```

**`sema.ml`:**
```ocaml
| BS_update of {
    table_meta  : Cat.table_meta;
    assignments : (int * bound_expr) list;  (* (col_ordinal, new_value_expr) *)
    where       : bound_expr option;
  }
```

Sema validation:
- Table must exist.
- Each assigned column must exist in the table schema.
- Each assigned expr's type must match the column type (NULL allowed).
- `where` bound against table schema.

**`plan.ml`:**
```ocaml
| Op_update of {
    table_meta  : Cat.table_meta;
    assignments : (int * Plan.expr) list;
    where       : Plan.expr option;
    indexes     : Cat.index_info list;  (* indexes on this table, for updating *)
  }
```

**`exec.ml` — Op_update execution:**
1. Open rw cursor on table tree.
2. `cursor_first`, then loop via `cursor_next`.
3. For each row, decode it, evaluate WHERE (skip if false).
4. For each matching row:
   a. Compute new field values by evaluating each assignment expr against the OLD row.
   b. Delete old index entries: for each index, encode old key, `Store.del` from index tree.
   c. `Store.del` old row from table tree (by rowid key).
   d. `Store.put` new row into table tree (same rowid, new encoded value).
   e. Insert new index entries: for each index, encode new key, `Store.put` into index tree.
5. Return count of rows updated.

**Important:** Mutate via a two-pass approach if needed (collect rowids, then update) to avoid cursor invalidation after puts/dels. In Phase 1, CoW semantics mean in-place updates are safe if you drain the cursor into a list first.

**Steps:**
- [ ] **Step 1:** Extend `ast.ml` with `S_update`.
- [ ] **Step 2:** Add UPDATE, SET keywords to `lexer.mll`.
- [ ] **Step 3:** Add UPDATE production to `parser.mly`: `UPDATE IDENT SET separated_nonempty_list(COMMA, assignment) where_clause SEMI? EOF`. Where `assignment ::= IDENT EQ expr`.
- [ ] **Step 4:** Add `BS_update` to `sema.ml`, implement `bind_update`.
- [ ] **Step 5:** Add `Op_update` to `plan.ml`.
- [ ] **Step 6:** Add UPDATE planning to `planner.ml`.
- [ ] **Step 7:** Implement `Op_update` in `exec.ml`. Drain cursor to list first, then apply updates. For each row: del old, put new, update indexes.
- [ ] **Step 8:** `dune runtest` — all pass.
- [ ] **Step 9:** Create `test/test_update.ml`, add to `test/dune`:
  - Unit: UPDATE all rows; verify via SELECT.
  - Unit: UPDATE with WHERE; only matching rows changed.
  - Unit: UPDATE with expression: `SET n = n + 1`.
  - Unit: UPDATE with NULL assignment: `SET s = NULL`.
  - Unit: UPDATE non-existent table → sema error.
  - Unit: UPDATE non-existent column → sema error.
  - Unit: UPDATE type mismatch → sema error.
  - Unit: UPDATE table with index — index entries updated correctly.
  - Unit: UPDATE returns count of rows affected.
  - QCheck: random inserts, random UPDATE on subset; verify SELECT sees new values and old values unchanged.
- [ ] **Step 10:** Extend `test/test_conformance.ml` — add UPDATE conformance case on both backends.
- [ ] **Step 11:** `dune runtest` — all pass.
- [ ] **Step 12:** Commit: `feat(sql): UPDATE with WHERE and expression assignments`.

---

### Task 3: DELETE

**Context:** `DELETE FROM table [WHERE expr]`. Simpler than UPDATE: scan, filter, delete matching rows and their index entries.

**Files to modify:** Same layers as Task 2 but for DELETE.

**`ast.ml`:**
```ocaml
| S_delete of {
    table : string;
    where : expr option;
  }
```

**`sema.ml`:**
```ocaml
| BS_delete of {
    table_meta : Cat.table_meta;
    where      : bound_expr option;
  }
```

**`plan.ml`:**
```ocaml
| Op_delete of {
    table_meta : Cat.table_meta;
    where      : Plan.expr option;
    indexes    : Cat.index_info list;
  }
```

**`exec.ml` — Op_delete execution:**
1. Drain cursor into list of `(rowid, row)` pairs.
2. For each matching row:
   a. Delete index entries (all indexes on the table).
   b. `Store.del` from table tree by rowid key.
3. Return count deleted.

**Steps:**
- [ ] **Step 1:** Extend `ast.ml`, add DELETE, FROM keywords (FROM already exists — check), add to `lexer.mll` only if missing.
- [ ] **Step 2:** Add DELETE production to `parser.mly`: `DELETE FROM IDENT where_clause SEMI? EOF`.
- [ ] **Step 3:** Add `BS_delete` to `sema.ml`, implement `bind_delete`.
- [ ] **Step 4:** Add `Op_delete` to `plan.ml`, add DELETE planning to `planner.ml`.
- [ ] **Step 5:** Implement `Op_delete` in `exec.ml`.
- [ ] **Step 6:** `dune runtest` — all pass.
- [ ] **Step 7:** Create `test/test_delete.ml`, add to `test/dune`:
  - Unit: DELETE all rows; SELECT returns empty.
  - Unit: DELETE with WHERE; only matching rows removed.
  - Unit: DELETE from non-existent table → sema error.
  - Unit: DELETE with index — index entries removed.
  - Unit: DELETE returns count of rows deleted.
  - Unit: DELETE `WHERE col IS NULL` — removes only null rows.
  - QCheck: random inserts, random DELETE on subset; remaining rows accessible; deleted rows absent.
- [ ] **Step 8:** Extend `test/test_conformance.ml` — DELETE conformance case.
- [ ] **Step 9:** `dune runtest` — all pass.
- [ ] **Step 10:** Commit: `feat(sql): DELETE FROM with WHERE`.

---

### Task 4: NOT NULL / DEFAULT column constraints

**Context:** Phase 1 schema supports `not_null: bool` in `column_def` but never enforces it at insert time, and DEFAULT is not parsed. Phase 2 adds enforcement of NOT NULL on INSERT/UPDATE and parsing of `DEFAULT literal` in CREATE TABLE.

**Files to modify:**
- `lib/sql/ast.ml` — add `default: literal option` to `column_def`
- `lib/sql/parser.mly` — add DEFAULT literal in column_def
- `lib/sql/sema.ml` — enforce NOT NULL at INSERT; apply DEFAULT when column omitted from INSERT
- `lib/encoding/row.ml` / `lib/catalog/catalog.ml` — store/load default value

**`ast.ml` column_def:**
```ocaml
type column_def = {
  name        : string;
  ty          : ty;
  not_null    : bool;
  primary_key : bool;
  default     : literal option;  (* new *)
}
```

**Catalog storage:** Encode default value in the `_sys_columns` row. Use a flag byte: `0` = no default, `1` = has default (followed by encoded literal).

**Sema changes:**
- On INSERT: if a column is omitted from the column list, use its DEFAULT if present, else NULL. If the column is NOT NULL and value would be NULL, return `Type_mismatch`/`Constraint` error.
- On UPDATE: if assigning NULL to a NOT NULL column, return error.

**Parser:** Add to column constraint parsing:
```
| DEFAULT literal  -> set default
```

**Steps:**
- [ ] **Step 1:** Extend `ast.ml` column_def with `default`.
- [ ] **Step 2:** Extend `parser.mly` — parse `DEFAULT literal` as column constraint.
- [ ] **Step 3:** Extend `catalog.ml` — encode/decode default value in column metadata.
- [ ] **Step 4:** Extend `sema.ml` — enforce NOT NULL on INSERT; apply DEFAULT for omitted columns.
- [ ] **Step 5:** `dune runtest` — all existing tests pass (default is `None`, behavior unchanged).
- [ ] **Step 6:** Add tests in `test/test_e2e.ml`:
  - `CREATE TABLE t (n INTEGER NOT NULL)` then `INSERT INTO t (n) VALUES (NULL)` → error.
  - `CREATE TABLE t (n INTEGER DEFAULT 42, s TEXT)` then `INSERT INTO t (s) VALUES ('x')` → row has n=42, s='x'.
  - `CREATE TABLE t (n INTEGER NOT NULL DEFAULT 0)` then `INSERT INTO t (s) VALUES ('x')` — if column omitted and default 0, inserts fine.
  - UPDATE `SET n = NULL` on NOT NULL column → error.
  - QCheck: all inserts that omit a DEFAULT column produce the default value on SELECT.
- [ ] **Step 7:** `dune runtest` — all pass.
- [ ] **Step 8:** Commit: `feat(sql): NOT NULL enforcement + DEFAULT literal column constraint`.

---

### Task 5: INNER JOIN and LEFT JOIN

**Context:** `SELECT ... FROM t1 [INNER|LEFT [OUTER]] JOIN t2 ON expr`. Phase 2 supports two-table joins. Planner rule: if the join predicate is equality on an indexed column of the inner table, emit `Op_nested_loop_join` (index-seek inner); otherwise `Op_hash_join` (build hash table from smaller side, probe with outer).

**Files to modify:**
- `lib/sql/ast.ml` — add join kind, `S_select` gains `joins` field
- `lib/sql/lexer.mll` — JOIN, INNER, LEFT, OUTER, ON keywords
- `lib/sql/parser.mly` — join clause production
- `lib/sql/sema.ml` — resolve two-table column references; bind join condition
- `lib/sql/plan.ml` — `Op_nested_loop_join`, `Op_hash_join`
- `lib/sql/planner.ml` — join planning rules
- `lib/sql/exec.ml` — execute both join operators
- Create: `test/test_join.ml`

**`ast.ml`:**
```ocaml
type join_kind = Inner | Left

type join_clause = {
  kind  : join_kind;
  table : string;
  on    : expr;
}

(* Extend S_select: *)
| S_select of {
    proj   : [`All | `Cols of string list];
    table  : string;
    joins  : join_clause list;   (* new — empty = no join *)
    where  : expr option;
    order  : order_key list;
    limit  : int option;
    offset : int option;
  }
```

**Sema — two-table column resolution:**

When `joins` is non-empty, column references must be resolved against both tables. Qualified refs (`table.col`) are resolved directly. Unqualified refs are resolved by searching all tables in order; ambiguous refs (found in multiple tables) are an error.

**`sema.ml` — `bound_stmt` extension:**
```ocaml
| BS_select of {
    ...existing fields...
    join       : bound_join option;
  }

type bound_join = {
  kind       : Ast.join_kind;
  right_meta : Cat.table_meta;
  on         : bound_expr;
  right_col_offset : int;  (* column index offset for right table in combined row *)
}
```

**Plan operators:**
```ocaml
| Op_nested_loop_join of {
    outer      : op;            (* left table scan *)
    inner_meta : Cat.table_meta;
    idx_tree   : int;           (* index on inner table's join col *)
    inner_col_idx : int;        (* join column ordinal in inner table *)
    outer_col_idx : int;        (* join column ordinal in outer row *)
    join_kind  : [`Inner | `Left];
    right_col_offset : int;
  }
| Op_hash_join of {
    outer      : op;
    inner      : op;
    outer_key  : int;           (* col ordinal in outer row *)
    inner_key  : int;           (* col ordinal in inner row *)
    join_kind  : [`Inner | `Left];
    right_col_offset : int;
    n_inner_cols : int;
  }
```

**Planner rules:**
- If join predicate is `outer_col = inner_col` AND inner_col has an index → `Op_nested_loop_join` (drive outer via SeqScan, probe inner via index).
- Otherwise → `Op_hash_join` (build hash table from inner side, probe with outer rows).
- LEFT JOIN: if inner probe finds no match, emit outer row padded with NULLs for inner columns.

**`exec.ml` — Op_nested_loop_join:**
```
for each outer_row in outer_stream:
  seek inner index tree at outer_row[outer_col_idx]
  for each matching inner_row:
    emit concat(outer_row, inner_row)
  if LEFT JOIN and no inner rows found:
    emit concat(outer_row, [NULL * n_inner_cols])
```

**`exec.ml` — Op_hash_join:**
```
build: drain inner stream into Hashtbl keyed by inner_row[inner_key]
probe: for each outer_row:
  lookup outer_row[outer_key] in hash table
  for each matching inner_row: emit concat(outer_row, inner_row)
  if LEFT JOIN and no match: emit concat(outer_row, [NULL * n_inner_cols])
```

**Steps:**
- [ ] **Step 1:** Extend `ast.ml` with `join_kind`, `join_clause`, extend `S_select`.
- [ ] **Step 2:** Add JOIN, INNER, LEFT, OUTER, ON keywords to `lexer.mll`.
- [ ] **Step 3:** Add join clause production to `parser.mly`. Join clause is optional: `(join_clause)*` appended after the FROM table.
- [ ] **Step 4:** Extend `sema.ml` — two-table column resolution, `bound_join`, validate ON expression.
- [ ] **Step 5:** Add `Op_nested_loop_join` and `Op_hash_join` to `plan.ml`.
- [ ] **Step 6:** Extend `planner.ml` — join planning: try indexed NLJ first, fall back to HashJoin.
- [ ] **Step 7:** Implement `Op_nested_loop_join` in `exec.ml`.
- [ ] **Step 8:** Implement `Op_hash_join` in `exec.ml`.
- [ ] **Step 9:** `dune runtest` — all pass.
- [ ] **Step 10:** Create `test/test_join.ml`, add to `test/dune`:
  - Unit: INNER JOIN on matching rows returns cross product of matches.
  - Unit: INNER JOIN on non-matching rows returns empty.
  - Unit: LEFT JOIN — outer rows without inner match appear with NULLs.
  - Unit: LEFT JOIN — outer rows with inner match appear normally.
  - Unit: Indexed NLJ — verify planner selects nested-loop when inner col has index.
  - Unit: Hash join — verify planner selects hash join when no index.
  - Unit: JOIN with WHERE filter (applied after join).
  - Unit: JOIN result ORDER BY.
  - Unit: Ambiguous column reference (same col name in both tables) → sema error.
  - Unit: Unknown table in JOIN → sema error.
  - QCheck: INNER JOIN result is a subset of the Cartesian product filtered by predicate.
  - QCheck: LEFT JOIN result size >= INNER JOIN result size.
- [ ] **Step 11:** Extend `test/test_conformance.ml` — JOIN cases on both backends.
- [ ] **Step 12:** `dune runtest` — all pass.
- [ ] **Step 13:** Commit: `feat(sql): INNER JOIN and LEFT JOIN (nested-loop + hash join)`.

---

### Task 6: GROUP BY, HAVING, and aggregates

**Context:** `SELECT agg(col), ... FROM t [WHERE] GROUP BY col [HAVING expr]`. Five aggregates: `COUNT(*)`, `COUNT(col)`, `SUM(col)`, `AVG(col)`, `MIN(col)`, `MAX(col)`. Phase 2: single-column GROUP BY; multi-column is Phase 3.

**Files to modify:**
- `lib/sql/ast.ml` — `agg_func` type, `expr` gains `E_agg`; `S_select` gains `group_by`, `having`
- `lib/sql/lexer.mll` — COUNT, SUM, AVG, MIN (already a keyword? check), MAX, GROUP, HAVING, STAR in agg context
- `lib/sql/parser.mly` — aggregate call, GROUP BY, HAVING
- `lib/sql/sema.ml` — validate aggregate calls; GROUP BY column; HAVING expr
- `lib/sql/plan.ml` — `Op_aggregate`
- `lib/sql/planner.ml` — plan GROUP BY
- `lib/sql/exec.ml` — execute Op_aggregate
- Create: `test/test_aggregate.ml`

**`ast.ml`:**
```ocaml
type agg_func = Count | Sum | Avg | Min | Max

(* Extend expr: *)
| E_agg of agg_func * expr option   (* None = COUNT(*) *)

(* Extend S_select: *)
| S_select of {
    ...
    group_by : string list;   (* column names; empty = no GROUP BY *)
    having   : expr option;
  }
```

**Sema validation for aggregates:**
- Aggregate calls are only valid in the SELECT projection and HAVING clause.
- If GROUP BY is present: every non-aggregate column in SELECT must appear in GROUP BY.
- If no GROUP BY but aggregates are present: the whole table is one group (emit one row).
- HAVING expr may reference aggregates or GROUP BY columns.
- `COUNT(*)` counts all rows (including NULLs). `COUNT(col)` counts non-NULL values.
- `SUM`/`AVG` on non-numeric column → type error.

**`sema.ml` — bound_stmt extension:**
```ocaml
type agg_expr =
  | AE_col  of int                    (* non-aggregate GROUP BY col *)
  | AE_agg  of Ast.agg_func * int option  (* aggregate: None = COUNT(*) *)

| BS_select of {
    ...
    group_by : int option;           (* col ordinal; None = no GROUP BY *)
    proj_agg : agg_expr list;        (* projection expressions *)
    having   : bound_expr option;
  }
```

**`plan.ml`:**
```ocaml
| Op_aggregate of {
    child    : op;
    group_col : int option;           (* ordinal in child row; None = one group *)
    aggs     : (Ast.agg_func * int option) list; (* func, col ordinal (None=COUNT(*)) *)
    having   : Plan.expr option;
  }
```

**`exec.ml` — Op_aggregate execution:**
```
1. Materialize child stream into list of rows.
2. If group_col = None: all rows are one group.
   Else: sort rows by group_col (stable sort), then scan groups by equality.
3. For each group:
   a. Compute each aggregate: COUNT/SUM/AVG/MIN/MAX over the group rows.
   b. Build output row: [group_col_value; agg1_value; agg2_value; ...]
   c. Apply HAVING filter on output row.
   d. Emit if HAVING is true (or absent).

Aggregate semantics:
- COUNT(*): count of rows in group.
- COUNT(col): count of non-NULL values.
- SUM(col): sum of non-NULL numeric values; NULL if all NULL.
- AVG(col): sum / count_non_null; NULL if all NULL.
- MIN(col) / MAX(col): min/max of non-NULL values using compare_values; NULL if all NULL.
```

**Steps:**
- [ ] **Step 1:** Extend `ast.ml` with `agg_func`, `E_agg`, extend `S_select` with `group_by` and `having`.
- [ ] **Step 2:** Add COUNT, SUM, AVG, MAX, GROUP, HAVING keywords to `lexer.mll`. Check if MIN and MAX conflict with anything. Note: `MIN` could conflict with MINUS abbreviation — use full keyword.
- [ ] **Step 3:** Add `agg_call` production to `parser.mly`. `SELECT COUNT(*), SUM(n) FROM t GROUP BY s HAVING COUNT(*) > 1`.
- [ ] **Step 4:** Extend `sema.ml` — validate aggregates; `proj_agg` binding; GROUP BY resolution; HAVING binding.
- [ ] **Step 5:** Add `Op_aggregate` to `plan.ml` and GROUP BY planning to `planner.ml`.
- [ ] **Step 6:** Implement `Op_aggregate` in `exec.ml`.
- [ ] **Step 7:** `dune runtest` — all pass.
- [ ] **Step 8:** Create `test/test_aggregate.ml`, add to `test/dune`:
  - Unit: `SELECT COUNT(*) FROM t` → 1 row with count.
  - Unit: `SELECT COUNT(*) FROM t` on empty table → 0.
  - Unit: `SELECT SUM(n) FROM t` → sum of all n values.
  - Unit: `SELECT AVG(n) FROM t` → average (as REAL).
  - Unit: `SELECT MIN(n), MAX(n) FROM t` → min and max.
  - Unit: `COUNT(col)` skips NULLs; `COUNT(*)` includes NULLs.
  - Unit: `GROUP BY s` produces one row per distinct s value.
  - Unit: `GROUP BY s HAVING COUNT(*) > 1` filters groups.
  - Unit: GROUP BY with ORDER BY — groups sorted.
  - Unit: Aggregate on non-numeric column `SUM(text_col)` → sema error.
  - Unit: Non-GROUP-BY column in SELECT → sema error.
  - QCheck: `SUM(n)` equals manual sum over all n values for random inserts.
  - QCheck: `COUNT(*)` equals number of inserted rows.
  - QCheck: `GROUP BY` partitions rows correctly — no row appears in two groups.
- [ ] **Step 9:** Extend `test/test_conformance.ml` — aggregate cases on both backends.
- [ ] **Step 10:** `dune runtest` — all pass.
- [ ] **Step 11:** Commit: `feat(sql): GROUP BY, HAVING, COUNT/SUM/AVG/MIN/MAX aggregates`.

---

### Task 7: DROP TABLE and DROP INDEX

**Context:** `DROP TABLE name` and `DROP INDEX name`. These remove the catalog entry and mark the associated tree_id(s) as freed (lazy — actual page reclamation deferred to freelist Phase 3).

**Files to modify:**
- `lib/sql/ast.ml` — `S_drop_table`, `S_drop_index`
- `lib/sql/lexer.mll` — DROP keyword
- `lib/sql/parser.mly` — DROP TABLE / DROP INDEX productions
- `lib/sql/sema.ml` — validate table/index exists
- `lib/sql/plan.ml` — `Op_drop_table`, `Op_drop_index`
- `lib/sql/planner.ml` — plan drop ops
- `lib/sql/exec.ml` — execute drop ops
- `lib/catalog/catalog.ml` / `catalog.mli` — `drop_table`, `drop_index`

**`ast.ml`:**
```ocaml
| S_drop_table of { name: string }
| S_drop_index of { name: string }
```

**`catalog.ml` additions:**
```ocaml
val drop_table : t -> Store.rw Store.txn -> name:string -> (unit, string) result Lwt.t
val drop_index : t -> Store.rw Store.txn -> name:string -> (unit, string) result Lwt.t
```

`drop_table`: removes the entry from `_sys_tables` and all associated columns from `_sys_columns`. Marks table tree_id as unused (for now: just remove from catalog; Phase 3 will reclaim pages).

`drop_index`: removes from `_sys_indexes`. Marks index tree_id as unused.

**Steps:**
- [ ] **Step 1:** Extend `ast.ml`, add DROP keyword to `lexer.mll`, add productions to `parser.mly`.
- [ ] **Step 2:** Extend `sema.ml` with `BS_drop_table` / `BS_drop_index` validation.
- [ ] **Step 3:** Add `Op_drop_table` / `Op_drop_index` to `plan.ml`; planning to `planner.ml`.
- [ ] **Step 4:** Extend `catalog.ml` — `drop_table` and `drop_index`.
- [ ] **Step 5:** Implement in `exec.ml`.
- [ ] **Step 6:** `dune runtest` — all pass.
- [ ] **Step 7:** Add tests in `test/test_e2e.ml`:
  - `DROP TABLE` then `SELECT` → sema error (unknown table).
  - `DROP TABLE` non-existent → sema error.
  - `DROP INDEX` then `SELECT WHERE indexed_col = val` → falls back to seq scan (no crash).
  - `DROP TABLE` that has indexes → indexes removed from catalog too.
- [ ] **Step 8:** `dune runtest` — all pass.
- [ ] **Step 9:** Commit: `feat(sql): DROP TABLE and DROP INDEX`.

---

### Task 8: Coverage cleanup and ROADMAP update

**Context:** After adding all new operators, ensure coverage stays high and ROADMAP reflects Phase 2 completion.

**Steps:**
- [ ] **Step 1:** Run `./scripts/coverage.sh html` to identify gaps.
- [ ] **Step 2:** For each real uncovered branch, write a targeted test in the appropriate file.
- [ ] **Step 3:** Document irreducible Lwt artifacts with inline comments.
- [ ] **Step 4:** Update `ROADMAP.md`:
  - Mark Phase 2 checkbox `[x]`.
  - Check off: UPDATE, DELETE, GROUP BY, HAVING, COUNT/SUM/AVG/MIN/MAX, INNER JOIN, LEFT JOIN, NOT NULL enforcement, DEFAULT literals, DROP TABLE, DROP INDEX.
- [ ] **Step 5:** `dune runtest` — all pass.
- [ ] **Step 6:** Commit: `chore: Phase 2 complete — coverage, ROADMAP update`.

---

## Invariants to maintain throughout Phase 2

1. **All Phase 0 + Phase 1 tests continue passing** after every task.
2. **TDD:** write tests before or alongside implementation.
3. **QCheck: 10,000 trials per property.**
4. **No polymorphic equality** on bytes, Row.ty, or value types — use explicit match or dedicated compare functions.
5. **Podman only.** All `dune build` and `dune runtest` in container.
6. **Commit after each task.** One commit per task minimum.
7. **No bisect_ppx regressions.** Coverage must stay ≥ 88% handwritten after each task.

---

## Open questions to resolve during Phase 2

- **Aggregate result type for AVG:** AVG over INTEGER columns — should the result always be REAL, or INTEGER when evenly divisible? Recommendation: always REAL (matches SQLite behavior). Document in sema.
- **NULL ordering in GROUP BY:** NULLs form their own group in SQL standard. Implement accordingly.
- **Multi-table projection ambiguity:** `SELECT *` with a JOIN returns all columns from all tables. Column ordering: left table cols first, right table cols appended. Document.
- **Hash join build side:** Without statistics, we always build the hash table from the right (inner) side. Document this as a Phase 2 simplification; cost-based choice is Phase 3+.
- **DISTINCT:** Not in Phase 2 scope per ROADMAP. If a test query naturally calls for it, document and skip.
- **Arithmetic overflow:** For `SUM` over large integer columns, overflow is possible. In Phase 2, let OCaml `Int64` overflow silently (same as SQLite). Document.
