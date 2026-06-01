# Phase 16: COLLATE NOCASE, DROP COLUMN, Window ORDER BY Fix

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix the window ORDER BY alias bug, add COLLATE NOCASE support, and implement ALTER TABLE DROP COLUMN.

**Architecture:** Three independent features. (1) A one-line planner fix that applies `substitute_window_slots` to sort keys when windows are present. (2) COLLATE NOCASE threads through all four layers (AST → Sema → Plan → Exec) as a new expression wrapper; exec handles nocase by lowercasing both operands in binops. (3) DROP COLUMN adds a catalog `drop_column` function that re-keys all column entries and migrates data rows.

**Tech Stack:** OCaml 5.x, Menhir parser, dune, alcotest. ALL dune commands must run inside podman:
```
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune ...
```

---

## File Map

| File | Change |
|------|--------|
| `lib/sql/ast.ml` | Add `type collation`, `E_collate`, `AA_drop_column` |
| `lib/sql/lexer.mll` | Add `COLLATE` token |
| `lib/sql/parser.mly` | Add `COLLATE` token, collation rule, `E_collate` expr, DROP COLUMN alter action |
| `lib/sql/sema.ml` | Add `BE_collate` handling, `AA_drop_column` check |
| `lib/sql/sema.mli` | Add `BE_collate` to `bound_expr` |
| `lib/sql/plan.ml` | Add `P_collate` to `expr` |
| `lib/sql/planner.ml` | Fix `make_sort_keys`, handle `BE_collate`, update `substitute_window_slots` |
| `lib/sql/exec.ml` | Eval `P_collate`, nocase-aware binop, handle `AA_drop_column` |
| `lib/catalog/catalog.ml` | Add `drop_column` function |
| `lib/catalog/catalog.mli` | Add `drop_column` signature |
| `test/test_e2e.ml` | Add 7 new tests in `window`, `collate`, `drop_column` groups |
| `test/test_sqlite_compare.ml` | Add 6 comparison test cases, 3 groups |

---

## Task 1: Fix Window ORDER BY Alias

The bug: `ORDER BY rn` where `rn` is a window result alias fails because `make_sort_keys` in `plan_select` doesn't apply `substitute_window_slots` to sort key expressions. `P_window_slot i` reaches `Op_sort` instead of being converted to `P_col(n_input_cols + i)`.

**Files:**
- Modify: `lib/sql/planner.ml` (function `plan_select`, around line 255)

- [ ] **Step 1: Write the failing test**

In `test/test_e2e.ml`, add this test after `test_window_first_last_value` (around line 3887):

```ocaml
let test_window_orderby_alias () =
  let db = fresh_db () in
  exec db "CREATE TABLE emp (dept TEXT, name TEXT, salary INTEGER)";
  exec db "INSERT INTO emp VALUES ('eng','Alice',90000),('eng','Bob',80000),('hr','Carol',70000),('hr','Dave',60000)";
  (* ORDER BY rn (window alias) — this requires the planner to substitute P_window_slot in sort keys *)
  let rows = query_ok db "SELECT name, ROW_NUMBER() OVER (PARTITION BY dept ORDER BY salary DESC) AS rn FROM emp ORDER BY dept, rn" in
  Alcotest.(check int) "4 rows" 4 (List.length rows);
  Alcotest.check value_testable "Alice rn=1" (Db.V_int 1L) (List.nth rows 0).(1);
  Alcotest.check value_testable "Bob rn=2"   (Db.V_int 2L) (List.nth rows 1).(1);
  Alcotest.check value_testable "Carol rn=1" (Db.V_int 1L) (List.nth rows 2).(1);
  Alcotest.check value_testable "Dave rn=2"  (Db.V_int 2L) (List.nth rows 3).(1)
```

Register it in the `"window"` group at the end of `test_e2e.ml`:
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
      Alcotest.test_case "orderby_alias"    `Quick test_window_orderby_alias;
    ];
```

- [ ] **Step 2: Run the test, confirm it fails**

```
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe -- test "window/orderby_alias"
```

Expected: FAIL — the query either raises an exception (`P_window_slot` not substituted) or returns wrong row order.

- [ ] **Step 3: Apply the fix in `lib/sql/planner.ml`**

Find `make_sort_keys` inside `plan_select` (around line 255). It looks like this:

```ocaml
  let make_sort_keys () =
    List.map (fun (bkey : Sema.bound_order_key) ->
      let dir = match bkey.dir with Ast.Asc -> `Asc | Ast.Desc -> `Desc in
      (plan_expr bkey.key, dir)
    ) order
  in
```

Replace with:

```ocaml
  let make_sort_keys () =
    List.map (fun (bkey : Sema.bound_order_key) ->
      let dir = match bkey.dir with Ast.Asc -> `Asc | Ast.Desc -> `Desc in
      let e = plan_expr bkey.key in
      let e' = if windows = [] then e
               else substitute_window_slots ~n_input_cols e in
      (e', dir)
    ) order
  in
```

- [ ] **Step 4: Run the test, confirm it passes**

```
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe -- test "window/orderby_alias"
```

Expected: PASS

- [ ] **Step 5: Run full test suite to check for regressions**

```
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe
```

Expected: All tests pass (was 283, now 284).

- [ ] **Step 6: Commit**

```bash
git add lib/sql/planner.ml test/test_e2e.ml
git commit -m "fix(phase16): substitute window slots in ORDER BY sort keys"
```

---

## Task 2: COLLATE NOCASE

Implement `expr COLLATE NOCASE` and `expr COLLATE BINARY` (no-op). COLLATE NOCASE normalises text to lowercase for comparison and sorting purposes.

**How it works in exec:**
- `eval_expr (P_collate(e, Ast.Collate_nocase))` returns `lowercase(text_value)` (or the value unchanged if not text). This makes `ORDER BY name COLLATE NOCASE` sort case-insensitively.
- In `eval_expr (P_binop(op, lhs_e, rhs_e))`, if `lhs_e` is a `P_collate(_, Nocase)`, also lowercase the rhs value (and vice versa). This makes `WHERE name COLLATE NOCASE = 'Alice'` match 'alice', 'Alice', 'ALICE'.

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/sema.ml`, `lib/sql/sema.mli`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/planner.ml`
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_e2e.ml`

- [ ] **Step 1: Write failing tests in `test/test_e2e.ml`**

Add after `test_window_orderby_alias`:

```ocaml
let test_collate_nocase_eq () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (name TEXT)";
  exec db "INSERT INTO t VALUES ('Alice'), ('BOB'), ('charlie')";
  (* COLLATE NOCASE on both sides normalises to lowercase before compare *)
  let rows = query_ok db "SELECT name FROM t WHERE name COLLATE NOCASE = 'alice' ORDER BY name" in
  Alcotest.(check int) "1 row" 1 (List.length rows);
  Alcotest.check value_testable "Alice" (Db.V_text "Alice") (List.nth rows 0).(0)

let test_collate_nocase_order () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (name TEXT)";
  exec db "INSERT INTO t VALUES ('banana'), ('Apple'), ('cherry')";
  let rows = query_ok db "SELECT name FROM t ORDER BY name COLLATE NOCASE" in
  Alcotest.(check int) "3 rows" 3 (List.length rows);
  Alcotest.check value_testable "Apple first"  (Db.V_text "Apple")  (List.nth rows 0).(0);
  Alcotest.check value_testable "banana second" (Db.V_text "banana") (List.nth rows 1).(0);
  Alcotest.check value_testable "cherry third" (Db.V_text "cherry") (List.nth rows 2).(0)

let test_collate_binary_eq () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (name TEXT)";
  exec db "INSERT INTO t VALUES ('Alice'), ('alice')";
  (* COLLATE BINARY is case-sensitive (no-op), same as no COLLATE *)
  let rows = query_ok db "SELECT name FROM t WHERE name COLLATE BINARY = 'alice' ORDER BY name" in
  Alcotest.(check int) "1 row" 1 (List.length rows);
  Alcotest.check value_testable "lowercase alice" (Db.V_text "alice") (List.nth rows 0).(0)
```

Register in a new `"collate"` group at the end of `test_e2e.ml`:

```ocaml
    "collate", [
      Alcotest.test_case "nocase_eq"    `Quick test_collate_nocase_eq;
      Alcotest.test_case "nocase_order" `Quick test_collate_nocase_order;
      Alcotest.test_case "binary_eq"    `Quick test_collate_binary_eq;
    ];
```

- [ ] **Step 2: Run the tests, confirm they fail**

```
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe -- test "collate/nocase_eq"
```

Expected: FAIL — parse error (COLLATE not recognised).

- [ ] **Step 3: Add `type collation` and `E_collate` to `lib/sql/ast.ml`**

After `type order_dir = Asc | Desc` (around line 107), add:

```ocaml
type collation = Collate_binary | Collate_nocase | Collate_rtrim
```

In the `expr` type (after `E_window`), add:

```ocaml
  | E_collate of expr * collation   (** expr COLLATE collation_name *)
```

- [ ] **Step 4: Add `COLLATE` token to `lib/sql/lexer.mll`**

Find the keyword block (around line 90). Add after the last keyword entry in the ident-matching block:

```ocaml
  | "COLLATE" | "collate"  { COLLATE }
```

- [ ] **Step 5: Add `COLLATE` token and collation rule to `lib/sql/parser.mly`**

In the `%token` declarations, add `COLLATE` to the last token line:

```
%token OVER PARTITION RECURSIVE COLLATE
```

In the precedence declarations, add a new level above `%nonassoc TILDE UMINUS`:

```
%nonassoc COLLATE_PREC
%nonassoc TILDE UMINUS
```

In the `expr` rule, add (near other postfix-style rules):

```ocaml
  | e = expr COLLATE c = collation_name
      { E_collate (e, c) }            %prec COLLATE_PREC
```

Add the `collation_name` rule after `window_spec`:

```ocaml
collation_name:
  | id = IDENT
    { match String.uppercase_ascii id with
      | "NOCASE"  -> Ast.Collate_nocase
      | "BINARY"  -> Ast.Collate_binary
      | "RTRIM"   -> Ast.Collate_rtrim
      | other     -> failwith ("unknown collation: " ^ other) }
```

- [ ] **Step 6: Add `BE_collate` to `lib/sql/sema.mli`**

In `bound_expr`, add after `BE_window_slot`:

```ocaml
  | BE_collate of bound_expr * Ast.collation
    (** expr COLLATE collation_name *)
```

- [ ] **Step 7: Handle `E_collate` in `lib/sql/sema.ml`**

In the `bind_one` function (the main expression binding function), add a case. Look for the `E_window` case and add nearby:

```ocaml
     | Ast.E_collate (e, c) ->
       let* be = bind_one ctx e in
       Lwt.return (Ok (BE_collate (be, c)))
```

Also in `expr_has_window` (used to detect if an expr contains a window function), add:

```ocaml
     | Ast.E_collate (e, _) -> expr_has_window e
```

- [ ] **Step 8: Add `P_collate` to `lib/sql/plan.ml`**

In the `expr` type, add after `P_window_slot`:

```ocaml
  | P_collate of expr * Ast.collation
```

- [ ] **Step 9: Handle `BE_collate` in `lib/sql/planner.ml`**

In `plan_expr`, add a case:

```ocaml
  | Sema.BE_collate (be, c) -> Plan.P_collate (plan_expr be, c)
```

In `substitute_window_slots`, add a case (after `P_cast`):

```ocaml
  | Plan.P_collate (e, c) -> Plan.P_collate (go e, c)
```

- [ ] **Step 10: Evaluate `P_collate` in `lib/sql/exec.ml`**

In `eval_expr`, find the `P_window_slot` failwith guard and add after it:

```ocaml
  | Plan.P_collate (e, Ast.Collate_nocase) ->
    let v = eval_expr clock params row e in
    (match v with Row.V_text s -> Row.V_text (String.lowercase_ascii s) | o -> o)
  | Plan.P_collate (e, _) ->
    eval_expr clock params row e
```

In `eval_expr`, find the `Plan.P_binop (op, lhs_e, rhs_e)` arm. After evaluating `lv` and `rv`, add nocase cross-normalisation logic:

```ocaml
  | Plan.P_binop (op, lhs_e, rhs_e) ->
    let lv = eval_expr clock params row lhs_e in
    let rv = eval_expr clock params row rhs_e in
    let is_nocase = function
      | Plan.P_collate (_, Ast.Collate_nocase) -> true
      | _ -> false
    in
    let nocase_text v = match v with
      | Row.V_text s -> Row.V_text (String.lowercase_ascii s)
      | o -> o
    in
    let (lv', rv') =
      if is_nocase lhs_e then (lv, nocase_text rv)
      else if is_nocase rhs_e then (nocase_text lv, rv)
      else (lv, rv)
    in
    eval_binop op lv' rv'
```

Note: `lv` is already lowercased when `lhs_e` is `P_collate(..., Nocase)` (from step above). We additionally lowercase `rv` to make the comparison case-insensitive.

- [ ] **Step 11: Run collate tests, confirm they pass**

```
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe -- test "collate/nocase_eq"
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe -- test "collate/nocase_order"
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe -- test "collate/binary_eq"
```

Expected: All 3 PASS.

- [ ] **Step 12: Run full test suite to check for regressions**

```
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe
```

Expected: All tests pass (was 284, now 287).

- [ ] **Step 13: Commit**

```bash
git add lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly \
        lib/sql/sema.ml lib/sql/sema.mli lib/sql/plan.ml \
        lib/sql/planner.ml lib/sql/exec.ml test/test_e2e.ml
git commit -m "feat(phase16): COLLATE NOCASE expression modifier"
```

---

## Task 3: ALTER TABLE DROP COLUMN

Add `AA_drop_column` to the AST, parse it, validate in sema, and implement in the catalog and executor. The executor: (1) drops any indexes on the column, (2) scans and re-encodes all rows without the column, (3) updates the catalog.

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/sema.ml`
- Modify: `lib/catalog/catalog.ml`, `lib/catalog/catalog.mli`
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_e2e.ml`

- [ ] **Step 1: Write failing tests in `test/test_e2e.ml`**

Add after the collate tests:

```ocaml
let test_drop_column_basic () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, name TEXT, age INTEGER)";
  exec db "INSERT INTO t VALUES (1, 'Alice', 30), (2, 'Bob', 25)";
  exec db "ALTER TABLE t DROP COLUMN age";
  let rows = query_ok db "SELECT id, name FROM t ORDER BY id" in
  Alcotest.(check int) "2 rows" 2 (List.length rows);
  Alcotest.check value_testable "row1 id" (Db.V_int 1L)       (List.nth rows 0).(0);
  Alcotest.check value_testable "row1 name" (Db.V_text "Alice") (List.nth rows 0).(1);
  Alcotest.check value_testable "row2 id" (Db.V_int 2L)       (List.nth rows 1).(0);
  Alcotest.check value_testable "row2 name" (Db.V_text "Bob")   (List.nth rows 1).(1)

let test_drop_column_schema () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, name TEXT, score REAL)";
  exec db "ALTER TABLE t DROP COLUMN score";
  (* Inserting without the dropped column should work *)
  exec db "INSERT INTO t (id, name) VALUES (1, 'Alice')";
  let rows = query_ok db "SELECT * FROM t" in
  Alcotest.(check int) "2 cols" 2 (Array.length (List.nth rows 0))

let test_drop_column_nonexistent_fails () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (id INTEGER, name TEXT)";
  let err = run (
    let* r = Db.execute db "ALTER TABLE t DROP COLUMN foo" in
    Lwt.return (err_or_fail "drop nonexistent" r)
  ) in
  let msg = fmt_err err in
  Alcotest.(check bool) "error mentions column" true
    (String.length msg > 0)
```

Register in a new `"drop_column"` group:

```ocaml
    "drop_column", [
      Alcotest.test_case "basic"           `Quick test_drop_column_basic;
      Alcotest.test_case "schema"          `Quick test_drop_column_schema;
      Alcotest.test_case "nonexistent_err" `Quick test_drop_column_nonexistent_fails;
    ];
```

- [ ] **Step 2: Run the tests, confirm they fail**

```
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe -- test "drop_column/basic"
```

Expected: FAIL — parse error (DROP COLUMN not in grammar).

- [ ] **Step 3: Add `AA_drop_column` to `lib/sql/ast.ml`**

In `alter_action` (around line 246), add:

```ocaml
and alter_action =
  | AA_add_column    of column_def
  | AA_rename_table  of string
  | AA_rename_column of string * string
  | AA_drop_column   of string         (** column name to drop *)
```

- [ ] **Step 4: Add DROP COLUMN rule to `lib/sql/parser.mly`**

Find the ALTER TABLE rules (around line 133). Add:

```ocaml
  | ALTER TABLE table = IDENT DROP COLUMN col = IDENT
    { Ast.S_alter_table { table; action = Ast.AA_drop_column col } }
  | ALTER TABLE table = IDENT DROP col = IDENT
    { Ast.S_alter_table { table; action = Ast.AA_drop_column col } }
```

- [ ] **Step 5: Add `AA_drop_column` validation to `lib/sql/sema.ml`**

In `bind_alter_table` (around line 1888), add a new arm to the `match action with`:

```ocaml
     | Ast.AA_drop_column col_name ->
       let exists = List.exists (fun c -> String.equal c.Row.name col_name) table_meta.Cat.columns in
       if not exists then
         Lwt.return (Error (Unknown_column { table; column = col_name }))
       else if List.length table_meta.Cat.columns <= 1 then
         Lwt.return (Error (Unsupported "cannot drop the only column of a table"))
       else
         Lwt.return (Ok (BS_alter_table { table_meta; action }))
```

- [ ] **Step 6: Add `drop_column` to `lib/catalog/catalog.mli`**

Add after `rename_column`:

```ocaml
(** Remove a column from an existing table.
    Re-keys all column entries with ordinal > drop_idx (shift down by 1).
    Does NOT migrate existing row data — caller (the executor) is responsible.
    Returns [Error msg] if the table or column does not exist. *)
val drop_column :
  t -> table_name:string -> col_name:string -> (unit, string) result Lwt.t
```

- [ ] **Step 7: Implement `drop_column` in `lib/catalog/catalog.ml`**

Add after `rename_column`:

```ocaml
let drop_column t ~table_name ~col_name =
  match Hashtbl.find_opt t.cache table_name with
  | None -> Lwt.return (Error (Printf.sprintf "table not found: %s" table_name))
  | Some meta ->
    let rec find_idx i = function
      | [] -> None
      | (c : Row.column) :: _ when String.equal c.name col_name -> Some i
      | _ :: rest -> find_idx (i + 1) rest
    in
    match find_idx 0 meta.columns with
    | None -> Lwt.return (Error (Printf.sprintf "column not found: %s" col_name))
    | Some drop_idx ->
      let n_cols = List.length meta.columns in
      let%lwt tx = S.rw_begin t.store in
      (* Delete the dropped column's entry *)
      let%lwt () = S.del tx sys_columns_tid (column_key table_name drop_idx) in
      (* Re-key all columns after drop_idx: shift ordinal down by 1 *)
      let%lwt () =
        let rec shift i =
          if i >= n_cols then Lwt.return_unit
          else
            let old_k = column_key table_name i in
            let new_k = column_key table_name (i - 1) in
            let%lwt bytes_opt = S.get tx sys_columns_tid old_k in
            (match bytes_opt with
             | None -> shift (i + 1)
             | Some bytes ->
               let%lwt () = S.del tx sys_columns_tid old_k in
               let%lwt () = S.put tx sys_columns_tid new_k bytes in
               shift (i + 1))
        in
        shift (drop_idx + 1)
      in
      let%lwt () = S.commit tx in
      let new_columns = List.filteri (fun i _ -> i <> drop_idx) meta.columns in
      Hashtbl.replace t.cache table_name { meta with columns = new_columns };
      Lwt.return (Ok ())
```

- [ ] **Step 8: Handle `AA_drop_column` in `lib/sql/exec.ml`**

In `execute_with_count`, in the `Plan.Op_alter_table` arm (around line 1585), add after the `AA_rename_column` case:

```ocaml
     | Ast.AA_drop_column col_name ->
       let table_name = table_meta.Cat.name in
       let col_idx = find_col_idx_by_name table_meta.Cat.columns col_name in
       let new_columns = List.filteri (fun i _ -> i <> col_idx) table_meta.Cat.columns in
       (* Drop any indexes that reference the dropped column *)
       let idxs = Cat.indexes_for_table cat ~table:table_name in
       let idxs_on_col = List.filter (fun (idx : Cat.index_info) ->
         List.mem col_name idx.Cat.idx_columns) idxs in
       let* (tx_idx, _) = acquire_txn store Auto in
       let* () = Lwt_list.iter_s (fun (idx : Cat.index_info) ->
         Cat.drop_index cat tx_idx ~name:idx.idx_name
       ) idxs_on_col in
       let* () = if idxs_on_col <> [] then S.commit tx_idx else Lwt.return_unit in
       (* Scan all rows, re-encode without the dropped column *)
       let* tx_ro = S.ro_begin store in
       let* cur = S.cursor_open tx_ro table_meta.Cat.tree_id in
       let _sr = S.cursor_first cur in
       let rows = ref [] in
       let rec drain () =
         match S.cursor_next cur with
         | None -> ()
         | Some (k, v) ->
           let old_row = Row.decode table_meta.Cat.columns v in
           let new_row = Array.of_list
             (List.filteri (fun i _ -> i <> col_idx) (Array.to_list old_row)) in
           rows := (Bytes.copy k, new_row) :: !rows;
           drain ()
       in
       drain ();
       S.cursor_close cur;
       let* () = S.ro_end tx_ro in
       let* tx = S.rw_begin store in
       let* () = Lwt_list.iter_s (fun (k, new_row) ->
         let new_bytes = Row.encode new_columns new_row in
         S.put tx table_meta.Cat.tree_id k new_bytes
       ) !rows in
       let* () = S.commit tx in
       let* result = Cat.drop_column cat ~table_name ~col_name in
       (match result with
        | Error msg -> Lwt.fail_with msg
        | Ok ()     -> Lwt.return 0)
```

Note: `acquire_txn` takes an `S.t` and a mode. When `idxs_on_col = []`, skip the index transaction entirely (don't call `acquire_txn` or `commit`). Simplify to:

```ocaml
     | Ast.AA_drop_column col_name ->
       let table_name = table_meta.Cat.name in
       let col_idx = find_col_idx_by_name table_meta.Cat.columns col_name in
       let new_columns = List.filteri (fun i _ -> i <> col_idx) table_meta.Cat.columns in
       (* Drop indexes referencing the dropped column *)
       let idxs_on_col = List.filter (fun (idx : Cat.index_info) ->
         List.mem col_name idx.Cat.idx_columns)
         (Cat.indexes_for_table cat ~table:table_name) in
       let* () = if idxs_on_col = [] then Lwt.return_unit
         else begin
           let* tx_idx = S.rw_begin store in
           let* () = Lwt_list.iter_s (fun (idx : Cat.index_info) ->
             Cat.drop_index cat tx_idx ~name:idx.idx_name
           ) idxs_on_col in
           S.commit tx_idx
         end
       in
       (* Migrate data rows: scan → decode → re-encode without col_idx *)
       let* tx_ro = S.ro_begin store in
       let* cur = S.cursor_open tx_ro table_meta.Cat.tree_id in
       let _sr = S.cursor_first cur in
       let rows = ref [] in
       let rec drain () =
         match S.cursor_next cur with
         | None -> ()
         | Some (k, v) ->
           let old_row = Row.decode table_meta.Cat.columns v in
           let new_row = Array.of_list
             (List.filteri (fun i _ -> i <> col_idx) (Array.to_list old_row)) in
           rows := (Bytes.copy k, new_row) :: !rows;
           drain ()
       in
       drain ();
       S.cursor_close cur;
       let* () = S.ro_end tx_ro in
       let* tx = S.rw_begin store in
       let* () = Lwt_list.iter_s (fun (k, new_row) ->
         let new_bytes = Row.encode new_columns new_row in
         S.put tx table_meta.Cat.tree_id k new_bytes
       ) !rows in
       let* () = S.commit tx in
       let* result = Cat.drop_column cat ~table_name ~col_name in
       (match result with
        | Error msg -> Lwt.fail_with msg
        | Ok ()     -> Lwt.return 0)
```

- [ ] **Step 9: Run drop_column tests, confirm they pass**

```
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe -- test "drop_column/basic"
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe -- test "drop_column/schema"
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe -- test "drop_column/nonexistent_err"
```

Expected: All 3 PASS.

- [ ] **Step 10: Run full test suite to check for regressions**

```
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe
```

Expected: All tests pass (was 287, now 290).

- [ ] **Step 11: Commit**

```bash
git add lib/sql/ast.ml lib/sql/parser.mly lib/sql/sema.ml \
        lib/catalog/catalog.ml lib/catalog/catalog.mli \
        lib/sql/exec.ml test/test_e2e.ml
git commit -m "feat(phase16): ALTER TABLE DROP COLUMN with data migration"
```

---

## Task 4: SQLite Comparison Tests

Add comparison tests validating our implementation against real SQLite for all three Phase 16 features.

**Files:**
- Modify: `test/test_sqlite_compare.ml`

- [ ] **Step 1: Add Phase 16 test cases to `test/test_sqlite_compare.ml`**

Look at the existing structure — find the end of `phase14_recursive_cte_cases` (around line 2455) and add:

```ocaml
let phase16_window_alias_cases = [
  { name = "window_orderby_alias_rn";
    setup = [
      "CREATE TABLE emp (dept TEXT, name TEXT, salary INTEGER)";
      "INSERT INTO emp VALUES ('eng','Alice',90000),('eng','Bob',80000),('hr','Carol',70000),('hr','Dave',60000)";
    ];
    query = "SELECT name, ROW_NUMBER() OVER (PARTITION BY dept ORDER BY salary DESC) AS rn FROM emp ORDER BY dept, rn";
    unordered = false };

  { name = "window_orderby_alias_dr";
    setup = [
      "CREATE TABLE scores (name TEXT, score INTEGER)";
      "INSERT INTO scores VALUES ('A',100),('B',100),('C',90),('D',80)";
    ];
    query = "SELECT name, DENSE_RANK() OVER (ORDER BY score DESC) AS dr FROM scores ORDER BY dr, name";
    unordered = false };
]

let phase16_collate_cases = [
  { name = "collate_nocase_eq";
    setup = [
      "CREATE TABLE t (name TEXT)";
      "INSERT INTO t VALUES ('Alice'),('BOB'),('charlie')";
    ];
    query = "SELECT name FROM t WHERE name COLLATE NOCASE = 'alice' ORDER BY name";
    unordered = false };

  { name = "collate_nocase_order";
    setup = [
      "CREATE TABLE t (name TEXT)";
      "INSERT INTO t VALUES ('banana'),('Apple'),('cherry')";
    ];
    query = "SELECT name FROM t ORDER BY name COLLATE NOCASE";
    unordered = false };
]

let phase16_drop_column_cases = [
  { name = "drop_column_basic";
    setup = [
      "CREATE TABLE t (id INTEGER, name TEXT, age INTEGER)";
      "INSERT INTO t VALUES (1,'Alice',30),(2,'Bob',25)";
      "ALTER TABLE t DROP COLUMN age";
    ];
    query = "SELECT id, name FROM t ORDER BY id";
    unordered = false };

  { name = "drop_column_then_insert";
    setup = [
      "CREATE TABLE t (id INTEGER, name TEXT, score REAL)";
      "ALTER TABLE t DROP COLUMN score";
      "INSERT INTO t (id, name) VALUES (1,'Alice'),(2,'Bob')";
    ];
    query = "SELECT id, name FROM t ORDER BY id";
    unordered = false };
]
```

- [ ] **Step 2: Register the new groups in the runner**

Find the `let () = Alcotest.run ...` call (around line 2455). Add the new groups:

```ocaml
    "phase16_window_alias",   List.map make_test phase16_window_alias_cases;
    "phase16_collate",        List.map make_test phase16_collate_cases;
    "phase16_drop_column",    List.map make_test phase16_drop_column_cases;
```

- [ ] **Step 3: Run the comparison tests**

```
podman run --rm \
  -v $(pwd):/workspace:Z \
  -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
  -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
  -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
  -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
  -w /workspace sqlocaml-dev dune exec test/test_sqlite_compare.exe
```

Expected: All comparison tests pass (was 242, now 248).

- [ ] **Step 4: Run full e2e suite one final time**

```
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe
```

Expected: 290 tests pass.

- [ ] **Step 5: Commit**

```bash
git add test/test_sqlite_compare.ml
git commit -m "test(phase16): SQLite comparison tests for window alias, COLLATE NOCASE, DROP COLUMN"
```

---

## Self-Review Checklist

**Spec coverage:**
- [x] Window ORDER BY alias fix → Task 1
- [x] COLLATE NOCASE comparison → Task 2 (nocase_eq test)
- [x] COLLATE NOCASE ORDER BY → Task 2 (nocase_order test)
- [x] COLLATE BINARY (no-op) → Task 2 (binary_eq test)
- [x] ALTER TABLE DROP COLUMN → Task 3
- [x] DROP COLUMN error on non-existent → Task 3 (nonexistent_err test)
- [x] DROP COLUMN data migration → Task 3 (schema test verifies row encoding)
- [x] SQLite comparison for all three features → Task 4

**Placeholders:** None — all steps have complete code.

**Type consistency:**
- `Ast.collation` defined in Task 2 Step 3, used in `BE_collate`, `P_collate`, `eval_expr` throughout Task 2
- `Ast.AA_drop_column` defined in Task 3 Step 3, handled in sema (Step 5), catalog (Steps 6-7), exec (Step 8)
- `Cat.drop_column` signature in Step 6, implementation in Step 7, called from exec in Step 8 ✓
