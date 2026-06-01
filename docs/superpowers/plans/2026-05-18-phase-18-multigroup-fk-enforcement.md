# Phase 18: Multi-Column GROUP BY and Foreign Key Enforcement

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add (1) multi-column GROUP BY and HAVING, and (2) FK constraint enforcement (INSERT-time RESTRICT) to the sqlocaml SQL engine.

**Architecture:** Multi-column GROUP BY replaces the single `group_col : int option` field in `Op_aggregate` with `group_cols : int list`, extending the grouping key to a tuple; the sort/split logic in exec.ml uses lexicographic key comparison. FK enforcement threads parsed `REFERENCES` metadata from AST column_def through sema → catalog (stored in sys_meta_tid under a per-table key) → exec, which checks parent-row existence on every INSERT that has a non-NULL FK column value.

**Tech Stack:** OCaml 5.x, dune 3.x, menhir (parser), alcotest (tests). All dune commands inside podman: `podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune …`

---

## Background: codebase conventions

- **Build**: `podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest`
- **SQLite compare**:
  ```bash
  podman run --rm \
    -v $(pwd):/workspace:Z \
    -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
    -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
    -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
    -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
    -w /workspace sqlocaml-dev dune exec test/test_sqlite_compare.exe
  ```
- **Current counts** (Phase 17 end): 301 e2e + 257 SQLite compare = 558 total, all passing.
- **System tree IDs**: 0=sys_tables, 1=sys_columns, 2=sys_indexes, 3=sys_meta, 4=sys_fts, 5=sys_views. User tables start at 16.

---

## Files modified by feature

| Feature | Files |
|---------|-------|
| Multi-col GROUP BY | `lib/sql/plan.ml`, `lib/sql/sema.ml`, `lib/sql/sema.mli`, `lib/sql/planner.ml`, `lib/sql/exec.ml`, `test/test_e2e.ml` |
| FK enforcement | `lib/sql/ast.ml`, `lib/sql/parser.mly`, `lib/sql/sema.ml`, `lib/sql/sema.mli`, `lib/sql/plan.ml`, `lib/sql/planner.ml`, `lib/sql/exec.ml`, `lib/catalog/catalog.ml`, `lib/catalog/catalog.mli`, `test/test_e2e.ml` |
| SQLite compare | `test/test_sqlite_compare.ml` |

---

## Task 1: Multi-Column GROUP BY

**Goal:** `SELECT dept, role, COUNT(*) FROM emp GROUP BY dept, role` — group by any number of columns and project all of them.

**Current gap:** `sema.ml:1247` rejects GROUP BY with > 1 column with `Unsupported "GROUP BY with more than one column is not supported in Phase 2"`. `Op_aggregate` carries `group_col : int option` (single column). `PI_group_col` is a unit variant (no index).

**Changes needed:**

1. `plan.ml`: `group_col : int option` → `group_cols : int list`. `PI_group_col` unit → `PI_group_col of int`.
2. `sema.ml` + `sema.mli`: `group_by : int option` → `group_cols : int list`. Remove the single-col restriction. Update HAVING resolver to accept any GROUP BY column.
3. `planner.ml`: Build `Op_aggregate` with `group_cols`. Project group columns using `PI_group_col i`.
4. `exec.ml`: Group by tuple key, output n group columns prepended to agg values.

**Files:**
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/sema.ml`
- Modify: `lib/sql/sema.mli`
- Modify: `lib/sql/planner.ml`
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_e2e.ml`

---

- [ ] **Step 1: Write failing tests**

Add to `test/test_e2e.ml` (read existing GROUP BY tests first for the exact pattern):

```ocaml
let test_group_by_two_cols () =
  let db = fresh_db () in
  run (
    let open Lwt.Syntax in
    let exec sql = let* r = Db.execute db sql in
      (match r with Ok () -> () | Error e -> failwith (Format.asprintf "%a" Db.pp_error e));
      Lwt.return_unit in
    let* () = exec "CREATE TABLE emp (dept TEXT, role TEXT, salary INTEGER)" in
    let* () = exec "INSERT INTO emp VALUES ('eng', 'dev', 100)" in
    let* () = exec "INSERT INTO emp VALUES ('eng', 'dev', 120)" in
    let* () = exec "INSERT INTO emp VALUES ('eng', 'mgr', 200)" in
    let* () = exec "INSERT INTO emp VALUES ('hr',  'dev', 80)" in
    let* rows_r = Db.query db
      "SELECT dept, role, COUNT(*) FROM emp GROUP BY dept, role ORDER BY dept, role" in
    let rows = match rows_r with Ok s -> Lwt_main.run (Lwt_stream.to_list s)
      | Error e -> failwith (Format.asprintf "%a" Db.pp_error e) in
    let triples = List.map (function
      | [Db.V_text d; Db.V_text r; Db.V_int c] -> (d, r, Int64.to_int c)
      | _ -> ("?","?",0)) rows in
    Alcotest.(check (list (triple string string int)))
      "two-col group" [("eng","dev",2);("eng","mgr",1);("hr","dev",1)] triples;
    Lwt.return_unit)

let test_group_by_two_cols_sum () =
  let db = fresh_db () in
  run (
    let open Lwt.Syntax in
    let exec sql = let* r = Db.execute db sql in
      (match r with Ok () -> () | Error e -> failwith (Format.asprintf "%a" Db.pp_error e));
      Lwt.return_unit in
    let* () = exec "CREATE TABLE emp2 (dept TEXT, role TEXT, salary INTEGER)" in
    let* () = exec "INSERT INTO emp2 VALUES ('eng', 'dev', 100)" in
    let* () = exec "INSERT INTO emp2 VALUES ('eng', 'dev', 120)" in
    let* () = exec "INSERT INTO emp2 VALUES ('eng', 'mgr', 200)" in
    let* rows_r = Db.query db
      "SELECT dept, role, SUM(salary) FROM emp2 GROUP BY dept, role ORDER BY dept, role" in
    let rows = match rows_r with Ok s -> Lwt_main.run (Lwt_stream.to_list s)
      | Error e -> failwith (Format.asprintf "%a" Db.pp_error e) in
    let triples = List.map (function
      | [Db.V_text d; Db.V_text r; Db.V_int s] -> (d, r, Int64.to_int s)
      | _ -> ("?","?",0)) rows in
    Alcotest.(check (list (triple string string int)))
      "two-col sum" [("eng","dev",220);("eng","mgr",200)] triples;
    Lwt.return_unit)

let test_group_by_three_cols () =
  let db = fresh_db () in
  run (
    let open Lwt.Syntax in
    let exec sql = let* r = Db.execute db sql in
      (match r with Ok () -> () | Error e -> failwith (Format.asprintf "%a" Db.pp_error e));
      Lwt.return_unit in
    let* () = exec "CREATE TABLE emp3 (a TEXT, b TEXT, c TEXT, n INTEGER)" in
    let* () = exec "INSERT INTO emp3 VALUES ('x','y','z',1)" in
    let* () = exec "INSERT INTO emp3 VALUES ('x','y','z',2)" in
    let* () = exec "INSERT INTO emp3 VALUES ('x','y','w',3)" in
    let* rows_r = Db.query db
      "SELECT a, b, c, SUM(n) FROM emp3 GROUP BY a, b, c ORDER BY c" in
    let rows = match rows_r with Ok s -> Lwt_main.run (Lwt_stream.to_list s)
      | Error e -> failwith (Format.asprintf "%a" Db.pp_error e) in
    let quads = List.map (function
      | [Db.V_text a; Db.V_text b; Db.V_text c; Db.V_int n] -> (a,b,c,Int64.to_int n)
      | _ -> ("?","?","?",0)) rows in
    Alcotest.(check (list (pair (pair string string) (pair string int))))
      "three-col" [(("x","y"),("w",3));(("x","y"),("z",3))] quads;
    Lwt.return_unit)
```

Register all three in a `"group_by_multi"` test group.

Run to confirm they fail:
```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe -- test group_by_multi -v 2>&1 | tail -10
```

Expected: FAIL with `Sema error: unsupported: GROUP BY with more than one column`.

- [ ] **Step 2: Update `plan.ml`**

In `lib/sql/plan.ml`, change `Op_aggregate`:

```ocaml
  | Op_aggregate of {
      child      : op;
      group_cols : int list;        (* was: group_col : int option *)
      aggs       : agg_spec list;
      having     : expr option;
      proj       : proj_item list;
    }
```

Change `proj_item`:

```ocaml
and proj_item =
  | PI_group_col of int   (* index into group_cols list; was unit variant PI_group_col *)
  | PI_agg_slot  of int
```

**Breaking change:** Every place that constructs or pattern-matches `Op_aggregate` or `PI_group_col` must be updated. The compiler will tell you exactly where. Find them with:
```bash
grep -rn "PI_group_col\|group_col\|Op_aggregate" lib/ test/ | grep -v "_build"
```

- [ ] **Step 3: Update `sema.mli`**

In `lib/sql/sema.mli`, find the `BS_select` (or equivalent) bound select type and update `group_by : int option` → `group_cols : int list`. Also find `bound_agg_query` or wherever group_col appears in sema types:

```ocaml
(* Find: group_by : int option, change to: *)
group_cols : int list;
```

Run `grep -n "group_col\|group_by" lib/sql/sema.mli` to find the exact field.

- [ ] **Step 4: Update `sema.ml`**

**4a.** Remove the single-column restriction (around line 1247):

```ocaml
(* OLD — delete this: *)
| _ -> Error (Unsupported "GROUP BY with more than one column is not supported in Phase 2")
```

**4b.** Replace the single-col `group_col_result` binding (around lines 1240–1249) with multi-col:

```ocaml
let group_cols_result : (int list, error) result =
  List.fold_left (fun acc col_name ->
    match acc with
    | Error _ as e -> e
    | Ok indices ->
      (match proj_lookup col_name with
       | Error _ ->
         (* Try qualified lookup *)
         (match tables |> List.find_map (fun (tbl, _) ->
           match qual_lookup tbl col_name with Ok i -> Some i | Error _ -> None) with
          | Some i -> Ok (indices @ [i])
          | None -> Error (Unknown_column { table = ""; column = col_name }))
       | Ok i -> Ok (indices @ [i]))
  ) (Ok []) group_by
in
(match group_cols_result with
 | Error e -> Lwt.return (Error e)
 | Ok group_cols ->
```

**4c.** Update `offset_for_aggs`:

```ocaml
(* OLD: let offset_for_aggs = match group_col with Some _ -> 1 | None -> 0 in *)
let offset_for_aggs = List.length group_cols in
```

**4d.** Update HAVING resolver to allow any GROUP BY column:

```ocaml
let having_resolver_unqual name =
  match proj_lookup name with
  | Error e -> Error e
  | Ok i ->
    (match List.index_opt (fun x -> x = i) group_cols with
     | Some pos -> Ok pos   (* group key at position pos *)
     | None -> Error (Unsupported (Printf.sprintf
                        "HAVING references non-grouped column '%s'" name)))
in
```

Note: `List.index_opt` doesn't exist in OCaml stdlib. Implement it inline:
```ocaml
let find_pos lst v =
  let rec go i = function
    | [] -> None
    | x :: _ when x = v -> Some i
    | _ :: rest -> go (i + 1) rest
  in go 0 lst
in
let having_resolver_unqual name =
  match proj_lookup name with
  | Error e -> Error e
  | Ok i ->
    (match find_pos group_cols i with
     | Some pos -> Ok pos
     | None -> Error (Unsupported (Printf.sprintf
                        "HAVING references non-grouped column '%s'" name)))
in
```

**4e.** Update projection binding: where `PI_group_col` is emitted, change to `PI_group_col pos` where `pos` is the index in `group_cols`:

```ocaml
(* In the column projection binding — find where AP_group_col is produced *)
(* When projecting a GROUP BY column, find its position in group_cols *)
| Ok i when (find_pos group_cols i <> None) ->
  Ok (AP_group_col (Option.get (find_pos group_cols i)))
```

Actually read the existing code carefully around lines 1450–1460 to understand the exact structure before modifying.

**4f.** Update the result to pass `group_cols` (not `group_col`) to the planner.

- [ ] **Step 5: Update `planner.ml`**

**5a.** In `plan_select` (find the `Op_aggregate` construction), change:

```ocaml
(* OLD: *)
Plan.Op_aggregate {
  child = after_where;
  group_col = (match group_col with Some i -> Some i | None -> None);
  aggs = ...;
  ...
}

(* NEW: *)
Plan.Op_aggregate {
  child = after_where;
  group_cols = group_cols;
  aggs = ...;
  ...
}
```

**5b.** Where `PI_group_col` is constructed, change from `PI_group_col` to `PI_group_col i` where `i` comes from the sema `AP_group_col i` variant.

- [ ] **Step 6: Update `exec.ml`**

Replace the `Op_aggregate` handler. The key changes:

```ocaml
| Plan.Op_aggregate { child; group_cols; aggs; having; proj } ->
  let* inner = to_stream clock params store ~mode ~cat child in
  let* rows = Lwt_stream.to_list inner in
  let n_group_cols = List.length group_cols in
  let group_keys_of_row row = List.map (fun i -> row.(i)) group_cols in
  let compare_group_keys ka kb =
    List.fold_left2 (fun acc a b ->
      if acc <> 0 then acc else compare_values a b
    ) 0 ka kb
  in
  let groups : (Row.value list * Row.t list) list =
    if group_cols = [] then
      [ ([], rows) ]
    else begin
      let sorted = List.stable_sort (fun a b ->
        compare_group_keys (group_keys_of_row a) (group_keys_of_row b)
      ) rows in
      let rec group_runs acc cur_key cur_rows = function
        | [] ->
          (match cur_rows with
           | [] -> List.rev acc
           | _  -> List.rev ((cur_key, List.rev cur_rows) :: acc))
        | r :: rest ->
          let k = group_keys_of_row r in
          if compare_group_keys k cur_key = 0 && cur_rows <> [] then
            group_runs acc cur_key (r :: cur_rows) rest
          else
            let acc' = if cur_rows = [] then acc
                       else (cur_key, List.rev cur_rows) :: acc in
            group_runs acc' k [r] rest
      in
      group_runs [] [] [] sorted
    end
  in
  (* compute_agg unchanged *)
  let compute_agg (spec : Plan.agg_spec) (group_rows : Row.t list) : Row.value =
    ... (* keep existing implementation *)
  in
  let agg_output_rows =
    List.map (fun (group_key, group_rows) ->
      let agg_vals = List.map (fun spec -> compute_agg spec group_rows) aggs in
      (* Output row: [key0; key1; ...; agg0; agg1; ...] *)
      Array.of_list (group_key @ agg_vals)
    ) groups
  in
  (* HAVING unchanged in structure, but note agg_row layout has changed *)
  let after_having = ... in
  let final_rows =
    List.map (fun agg_row ->
      Array.of_list (List.map (function
        | Plan.PI_group_col i -> agg_row.(i)           (* i-th group key *)
        | Plan.PI_agg_slot k  -> agg_row.(n_group_cols + k)  (* k-th aggregate *)
      ) proj)
    ) after_having
  in
  Lwt.return (Lwt_stream.of_list final_rows)
```

The `compute_agg` implementation is unchanged. The `after_having` HAVING filter logic uses `eval_expr` on `agg_row` — this still works because `BE_col i` in HAVING refers to slot `i` in `agg_row`, and the HAVING resolver in sema now maps GROUP BY column names to positions 0..n-1.

- [ ] **Step 7: Fix compile errors**

Build and fix any remaining compile errors from the structural change:
```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1 | head -40
```

Likely breakage: `test_planner.ml`, `test_exec.ml`, `test_sema.ml` — any pattern match on `PI_group_col` (now `PI_group_col _`) or `group_col` → `group_cols`.

- [ ] **Step 8: Run all tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest
```

Expected: 304+ tests pass (301 existing + 3 new GROUP BY tests).

- [ ] **Step 9: Commit**

```bash
git add lib/sql/plan.ml lib/sql/sema.ml lib/sql/sema.mli \
        lib/sql/planner.ml lib/sql/exec.ml test/test_e2e.ml \
        test/test_planner.ml test/test_exec.ml test/test_sema.ml  # if touched
git commit -m "feat(phase18): multi-column GROUP BY"
```

---

## Task 2: Foreign Key Enforcement (INSERT-time RESTRICT)

**Goal:** `REFERENCES parent(col)` in column definitions is parsed with metadata, stored in the catalog, and checked on every INSERT — if the FK column is not NULL, a matching row in the parent table must exist.

**Current state:** Column-level `REFERENCES t` and `REFERENCES t(c)` are parsed as `Col_fk_ref` and silently ignored. Table-level `FOREIGN KEY (c) REFERENCES t(c)` is not parsed at all.

**Scope:** INSERT enforcement only. DELETE/UPDATE CASCADE/RESTRICT deferred.

**Storage:** FK constraints are serialized as text and stored in `sys_meta_tid` under key `"fk:" ++ table_name`. Format: newline-separated entries, each `local_col\tparent_table\tparent_col`.

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/sema.ml`
- Modify: `lib/sql/sema.mli`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/planner.ml`
- Modify: `lib/sql/exec.ml`
- Modify: `lib/catalog/catalog.ml`
- Modify: `lib/catalog/catalog.mli`
- Modify: `test/test_e2e.ml`

---

- [ ] **Step 1: Write failing tests**

```ocaml
let test_fk_insert_valid () =
  let db = fresh_db () in
  run (
    let open Lwt.Syntax in
    let exec sql = let* r = Db.execute db sql in
      (match r with Ok () -> () | Error e -> failwith (Format.asprintf "%a" Db.pp_error e));
      Lwt.return_unit in
    let* () = exec "CREATE TABLE parent (id INTEGER PRIMARY KEY, name TEXT)" in
    let* () = exec "INSERT INTO parent VALUES (1, 'alice')" in
    let* () = exec "CREATE TABLE child (id INTEGER, parent_id INTEGER REFERENCES parent(id))" in
    (* Valid FK insert — parent row exists *)
    let* () = exec "INSERT INTO child VALUES (10, 1)" in
    let* rows_r = Db.query db "SELECT COUNT(*) FROM child" in
    let rows = match rows_r with Ok s -> Lwt_main.run (Lwt_stream.to_list s)
      | Error e -> failwith (Format.asprintf "%a" Db.pp_error e) in
    Alcotest.(check (list (list string)))
      "child row inserted" [["1"]]
      (List.map (List.map (fun v -> match v with Db.V_int n -> Int64.to_string n | _ -> "?")) rows);
    Lwt.return_unit)

let test_fk_insert_invalid () =
  let db = fresh_db () in
  run (
    let open Lwt.Syntax in
    let exec sql = let* r = Db.execute db sql in
      (match r with Ok () -> () | Error e -> failwith (Format.asprintf "%a" Db.pp_error e));
      Lwt.return_unit in
    let* () = exec "CREATE TABLE parent2 (id INTEGER PRIMARY KEY)" in
    let* () = exec "INSERT INTO parent2 VALUES (1)" in
    let* () = exec "CREATE TABLE child2 (id INTEGER, parent_id INTEGER REFERENCES parent2(id))" in
    (* Invalid FK insert — no parent row with id=99 *)
    let* result = Db.execute db "INSERT INTO child2 VALUES (10, 99)" in
    (match result with
     | Error _ -> ()
     | Ok () -> Alcotest.fail "expected FK violation error");
    Lwt.return_unit)

let test_fk_null_allowed () =
  let db = fresh_db () in
  run (
    let open Lwt.Syntax in
    let exec sql = let* r = Db.execute db sql in
      (match r with Ok () -> () | Error e -> failwith (Format.asprintf "%a" Db.pp_error e));
      Lwt.return_unit in
    let* () = exec "CREATE TABLE parent3 (id INTEGER PRIMARY KEY)" in
    let* () = exec "INSERT INTO parent3 VALUES (1)" in
    let* () = exec "CREATE TABLE child3 (id INTEGER, parent_id INTEGER REFERENCES parent3(id))" in
    (* NULL FK column is always valid (SQLite semantics) *)
    let* () = exec "INSERT INTO child3 VALUES (10, NULL)" in
    let* rows_r = Db.query db "SELECT COUNT(*) FROM child3" in
    let rows = match rows_r with Ok s -> Lwt_main.run (Lwt_stream.to_list s)
      | Error e -> failwith (Format.asprintf "%a" Db.pp_error e) in
    Alcotest.(check (list (list string)))
      "null fk allowed" [["1"]]
      (List.map (List.map (fun v -> match v with Db.V_int n -> Int64.to_string n | _ -> "?")) rows);
    Lwt.return_unit)
```

Register in a `"foreign_key"` test group.

Run to confirm FAIL (parser currently ignores FK metadata, so the invalid insert would succeed):
```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe -- test foreign_key -v 2>&1 | tail -10
```

- [ ] **Step 2: Update AST — add FK metadata to `column_def`**

In `lib/sql/ast.ml`, update `column_def`:

```ocaml
and column_def = {
  name        : string;
  ty          : ty;
  not_null    : bool;
  primary_key : bool;
  default     : literal option;
  check       : expr option;
  fk_ref      : (string * string) option;
  (** Foreign key reference: (parent_table, parent_col).
      None = no FK constraint on this column. *)
}
```

Also add a table-level FK constraint variant to `table_constraint`:

```ocaml
type table_constraint =
  | TC_unique      of string list
  | TC_primary_key of string list
  | TC_foreign_key of {
      local_cols   : string list;
      parent_table : string;
      parent_cols  : string list;
    }
```

**Breaking change:** Every place that constructs `column_def` without `fk_ref` will fail to compile. Run:
```bash
grep -rn "column_def\|{ name.*ty\|name =.*ty =" lib/ test/ | grep -v "_build"
```
and add `fk_ref = None` to each literal construction.

- [ ] **Step 3: Update `parser.mly`**

**3a.** Update `col_constraint` type in the parser header (`%{ ... %}`) to carry FK metadata:

```ocaml
  type col_constraint =
    | Col_not_null
    | Col_primary_key
    | Col_default of literal
    | Col_check   of expr
    | Col_fk_ref  of string * string option  (* parent_table, parent_col *)
```

**3b.** Update the FK parsing rules (currently at lines 217–218) to carry the referenced table/column:

```ocaml
  | REFERENCES t = IDENT
    { Col_fk_ref (t, None) }
  | REFERENCES t = IDENT LPAREN c = IDENT RPAREN
    { Col_fk_ref (t, Some c) }
```

**3c.** Update the `column_def` production (around line 198) to extract `fk_ref`:

```ocaml
  (* In the column_def construction: *)
  let fk_ref = List.fold_left (fun acc c ->
    match c with Col_fk_ref (t, col) -> Some (t, Option.value ~default:name col) | _ -> acc
  ) None cs in
  { name; ty; not_null; primary_key; default; check; fk_ref }
```

Note: when `REFERENCES t` (no column specified), we default the parent column to the local column name (common convention). The `name` in scope is the column name.

**3d.** Add `table_item` rule for table-level FK (add after the existing `TC_primary_key` rule):

```ocaml
  | FOREIGN KEY LPAREN local_cols = separated_nonempty_list(COMMA, IDENT) RPAREN
      REFERENCES parent_table = IDENT LPAREN parent_cols = separated_nonempty_list(COMMA, IDENT) RPAREN
    { TI_constraint (Ast.TC_foreign_key {
        local_cols;
        parent_table;
        parent_cols;
      }) }
```

This requires `FOREIGN` and `KEY` tokens. `FOREIGN` is already declared. Add `%token KEY`:
```
%token KEY
```
And in lexer.mll:
```
| "KEY" | "key" { KEY }
```

- [ ] **Step 4: Add FK type to catalog**

In `lib/catalog/catalog.mli`, add a new type and functions:

```ocaml
type fk_constraint = {
  fk_local_col   : string;
  fk_parent_table: string;
  fk_parent_col  : string;
}

(** Persist FK constraints for a table.
    Stored in sys_meta_tid under key ["fk:" ++ table_name]. *)
val save_fk_constraints :
  t -> table_name:string -> fks:fk_constraint list -> unit Lwt.t

(** Load FK constraints for a table. Returns [] if none stored. *)
val load_fk_constraints :
  t -> table_name:string -> fk_constraint list Lwt.t
```

In `lib/catalog/catalog.ml`, implement:

```ocaml
type fk_constraint = {
  fk_local_col   : string;
  fk_parent_table: string;
  fk_parent_col  : string;
}

let fk_meta_key table_name =
  Bytes.of_string ("fk:" ^ table_name)

(* Encode: newline-separated "local\tparent_table\tparent_col" entries *)
let encode_fks fks =
  let lines = List.map (fun fk ->
    fk.fk_local_col ^ "\t" ^ fk.fk_parent_table ^ "\t" ^ fk.fk_parent_col
  ) fks in
  Bytes.of_string (String.concat "\n" lines)

let decode_fks bytes =
  let s = Bytes.to_string bytes in
  if s = "" then []
  else
    List.filter_map (fun line ->
      match String.split_on_char '\t' line with
      | [lc; pt; pc] -> Some { fk_local_col = lc; fk_parent_table = pt; fk_parent_col = pc }
      | _ -> None
    ) (String.split_on_char '\n' s)

let save_fk_constraints t ~table_name ~fks =
  let key = fk_meta_key table_name in
  let%lwt tx = S.rw_begin t.store in
  let%lwt () = if fks = [] then S.del tx sys_meta_tid key
               else S.put tx sys_meta_tid key (encode_fks fks) in
  S.commit tx

let load_fk_constraints t ~table_name =
  let key = fk_meta_key table_name in
  let%lwt tx = S.ro_begin t.store in
  let%lwt v = S.get tx sys_meta_tid key in
  let%lwt () = S.ro_end tx in
  Lwt.return (match v with None -> [] | Some b -> decode_fks b)
```

- [ ] **Step 5: Propagate FK through sema**

In `lib/sql/sema.mli`, update `BS_create_table` to carry FK constraints:

```ocaml
  | BS_create_table of {
      name          : string;
      columns       : Sqlocaml_encoding.Row.column list;
      uniq_idxs     : (string * string list) list;
      if_not_exists : bool;
      fk_constraints: (string * string * string) list;
      (** [(local_col, parent_table, parent_col)] *)
    }
```

In `lib/sql/sema.ml`, find where `BS_create_table` is built (grep for `BS_create_table`). Add FK extraction:

```ocaml
(* Extract FK constraints from column defs *)
let fk_constraints =
  List.filter_map (fun (cd : Ast.column_def) ->
    match cd.Ast.fk_ref with
    | None -> None
    | Some (parent_table, parent_col) ->
      Some (cd.Ast.name, parent_table, parent_col)
  ) s.Ast.columns
  @
  (* Table-level FK constraints *)
  List.filter_map (function
    | Ast.TC_foreign_key { local_cols; parent_table; parent_cols } ->
      (match local_cols, parent_cols with
       | [lc], [pc] -> Some (lc, parent_table, pc)
       | lc :: _, pc :: _ -> Some (lc, parent_table, pc)
       (* Multi-col FK: only take first pair for now, multi-col FK deferred *)
       | _ -> None)
    | _ -> None
  ) s.Ast.constraints
in
Lwt.return (Ok (BS_create_table { name = s.Ast.name; columns; uniq_idxs; if_not_exists = s.Ast.if_not_exists; fk_constraints }))
```

**Note:** We do NOT validate that the parent table/column exists at CREATE TABLE time (SQLite doesn't either, and order of table creation can vary). FK validation happens at INSERT time.

- [ ] **Step 6: Propagate FK through plan**

In `lib/sql/plan.ml`, update `Op_create_table`:

```ocaml
  | Op_create_table of {
      name           : string;
      columns        : Sqlocaml_encoding.Row.column list;
      uniq_idxs      : (string * string list) list;
      if_not_exists  : bool;
      fk_constraints : (string * string * string) list;
      (** [(local_col, parent_table, parent_col)] *)
    }
```

In `lib/sql/planner.ml`, propagate `fk_constraints` from `BS_create_table` to `Op_create_table`.

Also update `Op_insert` in `plan.ml` to carry the FK constraints (needed so exec can check them without re-loading from catalog):

```ocaml
  | Op_insert of {
      table_meta     : Cat.table_meta;
      ordinals       : int list;
      values         : expr list list;
      on_conflict    : Ast.conflict_action option;
      returning      : expr list;
      upsert_update  : (string list * (int * expr) list) option;
      fk_constraints : Cat.fk_constraint list;
      (** FK constraints on this table, needed for INSERT enforcement. *)
    }
```

In `planner.ml`, when building `Op_insert`, load FK constraints. Since `planner.ml` has access to `cat`, use `Cat.load_fk_constraints` — but that's Lwt! The planner currently runs synchronously. 

**Alternative:** Store FK constraints in `Cat.table_meta` so they're available without Lwt. Add `fk_constraints : fk_constraint list` to `Cat.table_meta`. Load them during `Cat.open_` (in `load_all_tables`).

Actually, looking at the catalog more carefully, the simplest approach is to load FK constraints into `Cat.table_meta` at open time, making them available synchronously. This means:
1. Add `fk_constraints : fk_constraint list` to `Cat.table_meta` (in catalog.ml and catalog.mli)
2. In `load_all_tables`, for each table, also load its FK constraints
3. In `Cat.create_table`, also call `save_fk_constraints` and update the cache

This is the cleanest design. Let's do this instead.

**Revised Step 5: Add fk_constraints to table_meta**

In `lib/catalog/catalog.mli`:

```ocaml
type table_meta = {
  name            : string;
  tree_id         : Sqlocaml_store.Store.tree_id;
  columns         : Sqlocaml_encoding.Row.column list;
  next_rowid      : int64;
  fk_constraints  : fk_constraint list;
}
```

In `lib/catalog/catalog.ml`, update `load_all_tables` to also load FK constraints for each table (make it call `load_fk_constraints_sync` using an already-open RO tx, or re-open after). For simplicity, load them separately after loading tables:

```ocaml
let open_ store =
  let%lwt cache_without_fks = load_all_tables store in
  (* Load FK constraints for each table and add to table_meta *)
  let%lwt () = Hashtbl.iter_lwt (fun name meta ->
    let%lwt fks = load_fk_constraints_raw store name in
    Hashtbl.replace cache_without_fks name { meta with fk_constraints = fks };
    Lwt.return_unit
  ) cache_without_fks in
  ...
```

Actually OCaml's `Hashtbl` doesn't have `iter_lwt`. Use a fold:

```ocaml
let open_ store =
  let%lwt cache = load_all_tables store in  (* returns Hashtbl with fk_constraints = [] *)
  (* For each table, load its FK constraints *)
  let names = Hashtbl.fold (fun k _ acc -> k :: acc) cache [] in
  let%lwt () = Lwt_list.iter_s (fun name ->
    let%lwt fks = load_fk_constraints_raw store name in
    (match Hashtbl.find_opt cache name with
     | Some meta -> Hashtbl.replace cache name { meta with fk_constraints = fks }
     | None -> ());
    Lwt.return_unit
  ) names in
  let%lwt indexes = load_all_indexes store in
  let%lwt fts = load_all_fts store in
  Lwt.return { store; cache; indexes; fts }
```

And `load_fk_constraints_raw` reads directly from the store (no `Cat.t` needed):
```ocaml
let load_fk_constraints_raw store table_name =
  let key = fk_meta_key table_name in
  let%lwt tx = S.ro_begin store in
  let%lwt v = S.get tx sys_meta_tid key in
  let%lwt () = S.ro_end tx in
  Lwt.return (match v with None -> [] | Some b -> decode_fks b)
```

Now `Cat.table_meta` always has `fk_constraints` populated. The planner can use `table_meta.Cat.fk_constraints` directly (synchronous).

- [ ] **Step 7: Persist FK in exec on CREATE TABLE**

In `lib/sql/exec.ml`, find the `Op_create_table` handler. After creating the table, persist FK constraints:

```ocaml
| Plan.Op_create_table { name; columns; uniq_idxs; if_not_exists; fk_constraints } ->
  if if_not_exists && Cat.table_exists cat ~name then Lwt.return 0
  else begin
    (* existing create_table logic ... *)
    let fk_list = List.map (fun (lc, pt, pc) ->
      Cat.{ fk_local_col = lc; fk_parent_table = pt; fk_parent_col = pc }
    ) fk_constraints in
    let* () = if fk_list = [] then Lwt.return_unit
              else Cat.save_fk_constraints cat ~table_name:name ~fks:fk_list in
    Lwt.return rows_affected
  end
```

Also update the in-memory cache entry after saving:
```ocaml
(* After Cat.create_table and Cat.save_fk_constraints, the table_meta in cache
   needs fk_constraints populated. Cat.create_table already adds to cache with
   fk_constraints = []. Update it: *)
(match Cat.find_table_cached cat ~name with
 | Some meta ->
   (* internal: use catalog's register_ephemeral trick or just re-lookup *)
   (* Actually, simplest: add a Cat.set_fk_constraints function *)
 | None -> ())
```

**Simpler approach:** Add `set_fk_constraints : t -> table_name:string -> fk_constraint list -> unit` to catalog:
```ocaml
let set_fk_constraints t ~table_name ~fks =
  match Hashtbl.find_opt t.cache table_name with
  | None -> ()
  | Some meta -> Hashtbl.replace t.cache table_name { meta with fk_constraints = fks }
```

Call this in exec after `save_fk_constraints`:
```ocaml
Cat.set_fk_constraints cat ~table_name:name ~fks:fk_list;
```

- [ ] **Step 8: Enforce FK on INSERT**

In `lib/sql/exec.ml`, find the `Op_insert` handler. After evaluating each row's values and before writing to the store, check FK constraints:

```ocaml
(* FK enforcement helper — check all FK constraints for a given row *)
let check_fk_constraints ~table_meta ~row_vals =
  (* row_vals : (col_ordinal * Row.value) list — the values being inserted *)
  let fks = table_meta.Cat.fk_constraints in
  if fks = [] then Lwt.return_unit
  else
    Lwt_list.iter_s (fun (fk : Cat.fk_constraint) ->
      (* Find the local column ordinal *)
      let local_col_idx = List.find_opt (fun (c : Row.column) ->
        String.equal c.name fk.fk_local_col
      ) table_meta.Cat.columns
      |> Option.map (fun c ->
           let rec find_idx i = function
             | [] -> 0
             | (col : Row.column) :: _ when String.equal col.name fk.fk_local_col -> i
             | _ :: rest -> find_idx (i + 1) rest
           in find_idx 0 table_meta.Cat.columns)
      in
      match local_col_idx with
      | None -> Lwt.return_unit  (* column not found — skip *)
      | Some idx ->
        let v = List.assoc_opt idx row_vals |> Option.value ~default:Row.V_null in
        (match v with
         | Row.V_null -> Lwt.return_unit  (* NULL FK is always valid *)
         | fk_val ->
           (* Look up parent table *)
           (match Cat.find_table_cached cat ~name:fk.fk_parent_table with
            | None ->
              Lwt.fail_with (Printf.sprintf "FK: parent table '%s' not found"
                               fk.fk_parent_table)
            | Some parent_meta ->
              (* Find parent col index *)
              let parent_idx =
                let rec fi i = function
                  | [] -> 0
                  | (c : Row.column) :: _ when String.equal c.name fk.fk_parent_col -> i
                  | _ :: rest -> fi (i + 1) rest
                in fi 0 parent_meta.Cat.columns
              in
              (* Scan parent table for a row where parent_col = fk_val *)
              let* tx = S.ro_begin store in
              let* cur = S.cursor_open tx parent_meta.Cat.tree_id in
              let _sr = S.cursor_first cur in
              let found = ref false in
              let rec scan () =
                match S.cursor_next cur with
                | None -> ()
                | Some (_k, v) ->
                  let parent_row = Row.decode parent_meta.Cat.columns v in
                  if compare_values parent_row.(parent_idx) fk_val = 0 then
                    found := true
                  else if not !found then scan ()
              in
              scan ();
              S.cursor_close cur;
              let* () = S.ro_end tx in
              if !found then Lwt.return_unit
              else Lwt.fail_with (Printf.sprintf
                     "FOREIGN KEY constraint failed: no matching row in '%s' for %s=%s"
                     fk.fk_parent_table fk.fk_parent_col
                     (match fk_val with
                      | Row.V_int n -> Int64.to_string n
                      | Row.V_text s -> Printf.sprintf "'%s'" s
                      | _ -> "?"))))
    ) fks
```

**Important:** This does a full table scan for each FK check per row — O(parent_rows) per insert. Acceptable for correctness. For Phase 18, add a TODO comment: "Use index lookup when parent_col is indexed."

Integrate into the Op_insert handler. Find where rows are being inserted (after value evaluation, before `S.put`), add:
```ocaml
(* Check FK constraints before writing *)
let row_vals = List.mapi (fun i v -> (i, v)) (Array.to_list row_array) in
let* () = check_fk_constraints ~table_meta ~row_vals in
(* then proceed with S.put *)
```

Actually, the `row_vals` representation needs to match how the FK checker finds values. The row is built as an `Array.t` with positions matching `table_meta.columns`. So:
```ocaml
let* () = check_fk_constraints_arr ~table_meta ~row_arr in
```

Where:
```ocaml
let check_fk_constraints_arr ~table_meta ~row_arr =
  let fks = table_meta.Cat.fk_constraints in
  if fks = [] then Lwt.return_unit
  else
    Lwt_list.iter_s (fun (fk : Cat.fk_constraint) ->
      let find_col_idx name =
        let rec fi i = function
          | [] -> None
          | (c : Row.column) :: _ when String.equal c.name name -> Some i
          | _ :: rest -> fi (i + 1) rest
        in fi 0 table_meta.Cat.columns
      in
      match find_col_idx fk.fk_local_col with
      | None -> Lwt.return_unit
      | Some idx ->
        let v = row_arr.(idx) in
        (match v with
         | Row.V_null -> Lwt.return_unit
         | fk_val ->
           (match Cat.find_table_cached cat ~name:fk.fk_parent_table with
            | None -> Lwt.fail_with (Printf.sprintf "FK: parent table '%s' not found" fk.fk_parent_table)
            | Some parent_meta ->
              (match find_col_idx fk.fk_parent_col with   (* BUG: should use parent_meta columns *)
               | _ ->
               let parent_col_idx = 
                 let rec fi i = function
                   | [] -> 0
                   | (c : Row.column) :: _ when String.equal c.name fk.fk_parent_col -> i
                   | _ :: rest -> fi (i + 1) rest
                 in fi 0 parent_meta.Cat.columns
               in
               let* tx = S.ro_begin store in
               let* cur = S.cursor_open tx parent_meta.Cat.tree_id in
               let _sr = S.cursor_first cur in
               let found = ref false in
               let rec scan () =
                 if !found then ()
                 else match S.cursor_next cur with
                 | None -> ()
                 | Some (_k, v) ->
                   let parent_row = Row.decode parent_meta.Cat.columns v in
                   if compare_values parent_row.(parent_col_idx) fk_val = 0 then
                     found := true
                   else scan ()
               in
               scan ();
               S.cursor_close cur;
               let* () = S.ro_end tx in
               if !found then Lwt.return_unit
               else Lwt.fail_with (Printf.sprintf "FOREIGN KEY constraint failed: '%s.%s' has no row matching %s"
                      fk.fk_parent_table fk.fk_parent_col
                      (Row.value_to_string fk_val))))  (* use whatever value printer exists *)
    ) fks
```

Look at exec.ml for the value-to-string function — there's probably one in the error formatting or use `Format.asprintf "%a" Row.pp_value v` if a printer exists, or just sprintf inline.

- [ ] **Step 9: Run all tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest
```

Expected: 307+ tests pass.

- [ ] **Step 10: Commit**

```bash
git add lib/sql/ast.ml lib/sql/parser.mly lib/sql/sema.ml lib/sql/sema.mli \
        lib/sql/plan.ml lib/sql/planner.ml lib/sql/exec.ml \
        lib/catalog/catalog.ml lib/catalog/catalog.mli \
        lib/sql/lexer.mll test/test_e2e.ml
git commit -m "feat(phase18): FOREIGN KEY enforcement (INSERT-time RESTRICT)"
```

---

## Task 3: SQLite Comparison Tests

**Goal:** Add SQLite comparison tests for multi-column GROUP BY and FK enforcement.

**Files:**
- Modify: `test/test_sqlite_compare.ml`

---

- [ ] **Step 1: Read test structure**

Read the top of `test/test_sqlite_compare.ml` to confirm the test case record fields. Find the `phase17_*` groups to understand the pattern.

- [ ] **Step 2: Add phase18 test cases**

```ocaml
let phase18_multigroup_cases = [
  (* Two-column GROUP BY *)
  { setup = ["CREATE TABLE emp (dept TEXT, role TEXT, n INTEGER)";
             "INSERT INTO emp VALUES ('eng','dev',1)";
             "INSERT INTO emp VALUES ('eng','dev',2)";
             "INSERT INTO emp VALUES ('eng','mgr',3)";
             "INSERT INTO emp VALUES ('hr','dev',4)"];
    query = "SELECT dept, role, COUNT(*) FROM emp GROUP BY dept, role ORDER BY dept, role";
    label = "two_col_group_count" };
  { setup = ["CREATE TABLE t2 (a TEXT, b TEXT, v INTEGER)";
             "INSERT INTO t2 VALUES ('x','p',10)";
             "INSERT INTO t2 VALUES ('x','p',20)";
             "INSERT INTO t2 VALUES ('x','q',30)";
             "INSERT INTO t2 VALUES ('y','p',40)"];
    query = "SELECT a, b, SUM(v) FROM t2 GROUP BY a, b ORDER BY a, b";
    label = "two_col_group_sum" };
  { setup = ["CREATE TABLE t3 (a INTEGER, b INTEGER, c INTEGER)";
             "INSERT INTO t3 VALUES (1,1,10)";
             "INSERT INTO t3 VALUES (1,1,20)";
             "INSERT INTO t3 VALUES (1,2,30)";
             "INSERT INTO t3 VALUES (2,1,40)"];
    query = "SELECT a, b, AVG(c) FROM t3 GROUP BY a, b ORDER BY a, b";
    label = "two_col_group_avg" };
]

let phase18_fk_cases = [
  (* Basic FK: valid insert *)
  { setup = ["CREATE TABLE p (id INTEGER PRIMARY KEY)";
             "INSERT INTO p VALUES (1)";
             "INSERT INTO p VALUES (2)";
             "CREATE TABLE c (id INTEGER, pid INTEGER REFERENCES p(id))";
             "INSERT INTO c VALUES (10, 1)";
             "INSERT INTO c VALUES (20, 2)"];
    query = "SELECT COUNT(*) FROM c";
    label = "fk_valid_inserts" };
  (* FK with NULL allowed *)
  { setup = ["CREATE TABLE p2 (id INTEGER PRIMARY KEY)";
             "INSERT INTO p2 VALUES (1)";
             "CREATE TABLE c2 (id INTEGER, pid INTEGER REFERENCES p2(id))";
             "INSERT INTO c2 VALUES (10, NULL)";
             "INSERT INTO c2 VALUES (20, 1)"];
    query = "SELECT COUNT(*) FROM c2";
    label = "fk_null_allowed" };
]
```

- [ ] **Step 3: Register groups**

Find where `phase17_*` groups are registered and add after them:

```ocaml
"phase18_multigroup",  List.map make_test phase18_multigroup_cases;
"phase18_fk",          List.map make_test phase18_fk_cases;
```

- [ ] **Step 4: Run SQLite comparison tests**

```bash
podman run --rm \
  -v $(pwd):/workspace:Z \
  -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
  -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
  -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
  -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
  -w /workspace sqlocaml-dev dune exec test/test_sqlite_compare.exe 2>&1 | tail -10
```

All new tests should pass. Note: FK violation tests cannot be in SQLite compare (because they test error behavior, not row output). The FK compare tests test only the valid-insert path.

- [ ] **Step 5: Run full test suite**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest
```

- [ ] **Step 6: Commit**

```bash
git add test/test_sqlite_compare.ml
git commit -m "test(phase18): SQLite comparison tests for multi-column GROUP BY and FK enforcement"
```

---

## Self-Review Checklist

**Spec coverage:**
- [x] Multi-column GROUP BY: `GROUP BY a, b, c` with any number of columns
- [x] HAVING with multi-column GROUP BY (resolver accepts any GROUP BY column)
- [x] FK column-level: `REFERENCES parent(col)` in column definition
- [x] FK table-level: `FOREIGN KEY (col) REFERENCES parent(col)` as table constraint
- [x] FK NULL: NULL FK column always passes constraint
- [x] FK valid insert: matching parent row exists → insert succeeds
- [x] FK invalid insert: no matching parent row → insert fails with error
- [x] FK metadata persisted to sys_meta_tid, loaded on DB reopen
- [x] FK metadata in table_meta for synchronous access in planner/exec
- [x] SQLite comparison tests for multi-group and valid FK paths

**Potential issues:**
1. **`group_col : int option` removal breaks test_planner.ml, test_exec.ml, test_sema.ml.** The compiler will report exactly where. Add `group_cols = [old_int]` everywhere.
2. **`PI_group_col` unit → `PI_group_col of int` breaks pattern matches.** Update all `PI_group_col ->` to `PI_group_col _ ->` or `PI_group_col i ->`.
3. **FK scan opens a RO transaction inside an ongoing RW transaction.** The store allows nested read transactions. Verify by checking `Store.ro_begin` signature — it should be fine (read-only transactions don't conflict with each other or with writer).
4. **`Lwt_list.iter_s` ordering.** FK checks run sequentially per constraint. If there are multiple FK columns, they all get checked. Each opens its own RO txn — this is slightly inefficient but correct.
5. **Table-level FK with multiple local/parent cols.** The plan notes only the first pair is taken for multi-col FK (plan step 5, sema). This is a known limitation — add a TODO comment.
6. **`column_def` fk_ref field breaks all tests that construct column_def directly.** Check test_sema.ml, test_planner.ml.
7. **`Row.value_to_string` may not exist.** Use format string inline or use `Printf.sprintf` matching on the value variant.
