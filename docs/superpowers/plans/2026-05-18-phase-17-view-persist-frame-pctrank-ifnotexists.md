# Phase 17: View Persistence, Window Frame Spec, PERCENT_RANK/CUME_DIST, IF NOT EXISTS

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add four SQL features: (1) persist CREATE VIEW definitions to the B-tree so they survive DB close/reopen; (2) window frame spec `ROWS/RANGE BETWEEN … AND …`; (3) `PERCENT_RANK()` and `CUME_DIST()` window functions; (4) `CREATE TABLE/INDEX IF NOT EXISTS`.

**Architecture:** Each feature threads through the standard AST→Sema→Plan→Exec pipeline. View persistence adds a new system B-tree (`sys_views_tid = 5`) in the catalog; views are stored as their original SQL text and re-parsed on DB open. Frame spec adds `frame_spec option` to `window_spec` in the AST and propagates through sema/plan/exec; the frame bounds are resolved in `compute_window_for_partition`. PERCENT_RANK/CUME_DIST are new `window_func` variants with O(n²) naive implementations (acceptable for typical window sizes). IF NOT EXISTS adds a boolean flag to create DDL nodes and short-circuits the executor if the object already exists.

**Tech Stack:** OCaml 5.x, dune 3.x, menhir (parser), alcotest (tests). All dune commands run inside podman: `podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune …`

---

## Files modified by feature

| Feature | Files |
|---------|-------|
| View persistence | `lib/catalog/catalog.mli`, `lib/catalog/catalog.ml`, `lib/db/db.ml`, `test/test_e2e.ml` |
| Window frame | `lib/sql/ast.ml`, `lib/sql/lexer.mll`, `lib/sql/parser.mly`, `lib/sql/sema.mli`, `lib/sql/sema.ml`, `lib/sql/plan.ml`, `lib/sql/planner.ml`, `lib/sql/exec.ml`, `test/test_e2e.ml` |
| PERCENT_RANK/CUME_DIST | `lib/sql/ast.ml`, `lib/sql/parser.mly`, `lib/sql/exec.ml`, `test/test_e2e.ml` |
| IF NOT EXISTS | `lib/sql/ast.ml`, `lib/sql/lexer.mll`, `lib/sql/parser.mly`, `lib/sql/sema.mli`, `lib/sql/sema.ml`, `lib/sql/plan.ml`, `lib/sql/planner.ml`, `lib/sql/exec.ml`, `lib/catalog/catalog.ml`, `lib/catalog/catalog.mli`, `test/test_e2e.ml` |
| SQLite compare tests | `test/test_sqlite_compare.ml` |

---

## Background: codebase conventions

- **Build**: every dune command runs inside podman. The canonical test command is:
  ```bash
  podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest
  ```
- **SQLite compare test** command (needs sqlite3 mounted):
  ```bash
  podman run --rm \
    -v $(pwd):/workspace:Z \
    -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
    -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
    -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
    -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
    -w /workspace sqlocaml-dev dune exec test/test_sqlite_compare.exe
  ```
- The catalog has system tree IDs 0–4 (`sys_tables_tid=0`, `sys_columns_tid=1`, `sys_indexes_tid=2`, `sys_meta_tid=3`, `sys_fts_tid=4`). `sys_views_tid=5` is the next available slot.
- `t.views` in `Db.t` is `(string, Ast.stmt) Hashtbl.t` — view name → parsed query AST.
- `window_plan_item` in `plan.ml` mirrors `window_sema` in `sema.mli`.
- `compute_window_for_partition` in `exec.ml` (~line 1999) handles per-partition window computation.

---

## Task 1: View Persistence

**Goal:** When `CREATE VIEW v AS SELECT …` is executed on a file-based DB, the view SQL is stored in the `sys_views_tid` B-tree. On `open_file`/`open_block`, views are loaded and re-parsed into `t.views`.

**Files:**
- Modify: `lib/catalog/catalog.mli`
- Modify: `lib/catalog/catalog.ml`
- Modify: `lib/db/db.ml`
- Modify: `test/test_e2e.ml`

---

- [ ] **Step 1: Write failing test**

Add to `test/test_e2e.ml` before the final `let () = …` runner:

```ocaml
(* View persistence: create view in file DB, close, reopen, query *)
let test_view_persistence () =
  let tmpfile = Filename.temp_file "sqlocaml_view_persist_" ".db" in
  Fun.protect ~finally:(fun () -> try Unix.unlink tmpfile with _ -> ()) (fun () ->
    Lwt_main.run (
      let* db_res = Db.open_file ~path:tmpfile in
      let db = match db_res with Ok d -> d | Error _ -> failwith "open1 failed" in
      let* _ = Db.execute db "CREATE TABLE t (id INTEGER, name TEXT)" in
      let* _ = Db.execute db "INSERT INTO t VALUES (1, 'alice')" in
      let* _ = Db.execute db "INSERT INTO t VALUES (2, 'bob')" in
      let* _ = Db.execute db "CREATE VIEW v AS SELECT name FROM t WHERE id = 1" in
      let* () = Db.close db in
      (* Reopen and query the view *)
      let* db2_res = Db.open_file ~path:tmpfile in
      let db2 = match db2_res with Ok d -> d | Error _ -> failwith "open2 failed" in
      let* rows_r = Db.query db2 "SELECT name FROM v" in
      let rows = match rows_r with Ok r -> Lwt_main.run (Lwt_stream.to_list r) | Error _ -> failwith "query failed" in
      let* () = Db.close db2 in
      Alcotest.(check (list (list string)))
        "view persisted" [["alice"]] (List.map (List.map (fun v -> Db.(match v with V_text s -> s | _ -> "?"))) rows);
      Lwt.return_unit
    ))
```

Wait — the structure of `test_e2e.ml` uses `run` helper. Look at how other tests in the file call `Db.close`:

```ocaml
(* Correct test: use run and look at the actual helpers used in the file *)
let test_view_persistence () =
  let tmpfile = Filename.temp_file "sqlocaml_view_persist_" ".db" in
  Fun.protect ~finally:(fun () -> try Unix.unlink tmpfile with _ -> ()) (fun () ->
    let run_inner f = Lwt_main.run (f ()) in
    run_inner (fun () ->
      let open Lwt.Syntax in
      let* db_res = Db.open_file ~path:tmpfile in
      let db = match db_res with Ok d -> d | Error _ -> failwith "open1 failed" in
      let exec sql = let* r = Db.execute db sql in match r with
        | Ok () -> Lwt.return_unit | Error e -> Lwt.fail_with (Format.asprintf "%a" Db.pp_error e) in
      let* () = exec "CREATE TABLE t (id INTEGER, name TEXT)" in
      let* () = exec "INSERT INTO t VALUES (1, 'alice')" in
      let* () = exec "INSERT INTO t VALUES (2, 'bob')" in
      let* () = exec "CREATE VIEW v AS SELECT name FROM t WHERE id = 1" in
      let* () = Db.close db in
      let* db2_res = Db.open_file ~path:tmpfile in
      let db2 = match db2_res with Ok d -> d | Error _ -> failwith "open2 failed" in
      let* rows_r = Db.query db2 "SELECT name FROM v" in
      let rows = match rows_r with Ok s -> s | Error e -> Lwt.return (Lwt_stream.of_list []) |> ignore; failwith (Format.asprintf "%a" Db.pp_error e) in
      let* row_list = Lwt_stream.to_list rows in
      let* () = Db.close db2 in
      let names = List.map (function [Db.V_text s] -> s | _ -> "?") row_list in
      Alcotest.(check (list string)) "view persisted" ["alice"] names;
      Lwt.return_unit))
```

Actually, to get the correct helper pattern, read the test file around line 1258–1350 where existing file-based persistence tests live, and follow the exact same pattern. The key elements are: `Filename.temp_file`, `Fun.protect`, `Lwt_main.run`, `Db.open_file`, helper closures.

The test MUST fail right now (view query will return 0 rows since view is lost on close/reopen).

- [ ] **Step 2: Run test to verify it fails**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe -- test 'view/persistence' -v
```

Expected: FAIL — view not found or returns 0 rows.

- [ ] **Step 3: Add `sys_views_tid` and catalog functions**

In `lib/catalog/catalog.ml`, after line 10 (`let sys_fts_tid : S.tree_id = 4`):

```ocaml
let sys_views_tid : S.tree_id = 5
```

Then add these three functions **before** the `open_` function (around line 435):

```ocaml
let load_all_views store =
  let%lwt tx = S.ro_begin store in
  let%lwt cur = S.cursor_open tx sys_views_tid in
  let _sr = S.cursor_first cur in
  let pairs = ref [] in
  let rec walk () =
    match S.cursor_next cur with
    | None -> ()
    | Some (k, v) ->
      pairs := (Bytes.to_string k, Bytes.to_string v) :: !pairs;
      walk ()
  in
  walk ();
  S.cursor_close cur;
  let%lwt () = S.ro_end tx in
  Lwt.return (List.rev !pairs)

let persist_view store ~name ~sql =
  let%lwt tx = S.rw_begin store in
  let%lwt () = S.put tx sys_views_tid (Bytes.of_string name) (Bytes.of_string sql) in
  S.commit tx

let remove_view store ~name =
  let%lwt tx = S.rw_begin store in
  let%lwt () = S.del tx sys_views_tid (Bytes.of_string name) in
  S.commit tx
```

- [ ] **Step 4: Add function signatures to `catalog.mli`**

Append to `lib/catalog/catalog.mli`:

```ocaml
(** Load all persisted view definitions.
    Returns [(view_name, create_view_sql)] pairs. *)
val load_all_views : Sqlocaml_store.Store.t -> (string * string) list Lwt.t

(** Persist a view's SQL text (keyed by name) to the sys_views B-tree. *)
val persist_view : Sqlocaml_store.Store.t -> name:string -> sql:string -> unit Lwt.t

(** Remove a view's SQL text from the sys_views B-tree. *)
val remove_view : Sqlocaml_store.Store.t -> name:string -> unit Lwt.t
```

Note: these functions take `Store.t` directly (not `Cat.t`) to avoid needing a catalog reference for a simple B-tree operation.

- [ ] **Step 5: Wire view persistence into `db.ml`**

In `lib/db/db.ml`, modify `open_file` and `open_block` to load views after opening the catalog. Add a helper:

```ocaml
let load_views_into_hashtbl store views_tbl =
  let open Lwt.Syntax in
  let* pairs = Cat.load_all_views store in
  List.iter (fun (name, sql) ->
    match
      let lexbuf = Lexing.from_string sql in
      Sql.Parser.stmt_eof Sql.Lexer.token lexbuf
    with
    | Sql.Ast.S_create_view { query; _ } ->
      Hashtbl.replace views_tbl name query
    | _ -> ()
    | exception _ -> ()
  ) pairs;
  Lwt.return_unit
```

Modify `open_file`:

```ocaml
let open_file ~path =
  let* result = S.open_file ~path in
  match result with
  | Error e ->
    let msg = Format.asprintf "%a" S.pp_error e in
    Lwt.return (Error (Runtime msg))
  | Ok store ->
    let* catalog = Cat.open_ store in
    let views = Hashtbl.create 4 in
    let* () = load_views_into_hashtbl store views in
    Lwt.return (Ok { store; catalog; clock = None; explicit_txn = None; views })
```

Modify `open_block` similarly (add `load_views_into_hashtbl store views` after creating `views`).

Then modify `execute` (around line 123) for view ops:

```ocaml
  | Ok Sql.Plan.Op_create_view { name; query } ->
    Hashtbl.replace t.views name query;
    let* () = Cat.persist_view t.store ~name ~sql in
    Lwt.return (Ok ())
  | Ok Sql.Plan.Op_drop_view { name } ->
    Hashtbl.remove t.views name;
    let* () = Cat.remove_view t.store ~name in
    Lwt.return (Ok ())
```

Do the same in `execute_change_count` (around line 165):

```ocaml
  | Ok Sql.Plan.Op_create_view { name; query } ->
    Hashtbl.replace t.views name query;
    let* () = Cat.persist_view t.store ~name ~sql in
    Lwt.return (Ok 0)
  | Ok Sql.Plan.Op_drop_view { name } ->
    Hashtbl.remove t.views name;
    let* () = Cat.remove_view t.store ~name in
    Lwt.return (Ok 0)
```

- [ ] **Step 6: Run tests to verify passing**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest
```

Expected: ALL tests pass. The new `view/persistence` test should now pass.

- [ ] **Step 7: Commit**

```bash
git add lib/catalog/catalog.mli lib/catalog/catalog.ml lib/db/db.ml test/test_e2e.ml
git commit -m "feat(phase17): persist CREATE VIEW definitions to sys_views B-tree"
```

---

## Task 2: Window Frame Spec (ROWS/RANGE BETWEEN … AND …)

**Goal:** Allow window aggregate functions to use an explicit frame clause, e.g.:
```sql
SELECT SUM(val) OVER (ORDER BY dt ROWS BETWEEN 6 PRECEDING AND CURRENT ROW)
```

Currently the frame is hard-coded: no ORDER BY → entire partition; with ORDER BY → ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW.

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/sema.mli`
- Modify: `lib/sql/sema.ml`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/planner.ml`
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_e2e.ml`

---

- [ ] **Step 1: Write failing tests**

Add to `test/test_e2e.ml`:

```ocaml
let test_window_rows_frame () =
  let db = fresh_db () in
  run (
    let open Lwt.Syntax in
    let exec sql = Db.execute db sql >>= function
      | Ok () -> Lwt.return_unit
      | Error e -> Lwt.fail_with (Format.asprintf "%a" Db.pp_error e) in
    let* () = exec "CREATE TABLE nums (n INTEGER)" in
    let* () = exec "INSERT INTO nums VALUES (1)" in
    let* () = exec "INSERT INTO nums VALUES (2)" in
    let* () = exec "INSERT INTO nums VALUES (3)" in
    let* () = exec "INSERT INTO nums VALUES (4)" in
    let* () = exec "INSERT INTO nums VALUES (5)" in
    let* rows_r = Db.query db
      "SELECT n, SUM(n) OVER (ORDER BY n ROWS BETWEEN 1 PRECEDING AND CURRENT ROW) AS s FROM nums ORDER BY n" in
    let rows = match rows_r with Ok s -> Lwt_main.run (Lwt_stream.to_list s) | Error e -> failwith (Format.asprintf "%a" Db.pp_error e) in
    (* Row 1: sum(1) = 1, Row 2: sum(1,2) = 3, Row 3: sum(2,3) = 5, Row 4: sum(3,4) = 7, Row 5: sum(4,5) = 9 *)
    let pairs = List.map (function
      | [Db.V_int n; Db.V_int s] -> (Int64.to_int n, Int64.to_int s)
      | _ -> (-1, -1)) rows in
    Alcotest.(check (list (pair int int)))
      "rows 1-preceding" [(1,1);(2,3);(3,5);(4,7);(5,9)] pairs;
    Lwt.return_unit)

let test_window_rows_unbounded () =
  let db = fresh_db () in
  run (
    let open Lwt.Syntax in
    let exec sql = Db.execute db sql >>= function
      | Ok () -> Lwt.return_unit
      | Error e -> Lwt.fail_with (Format.asprintf "%a" Db.pp_error e) in
    let* () = exec "CREATE TABLE nums (n INTEGER)" in
    let* () = exec "INSERT INTO nums VALUES (10)" in
    let* () = exec "INSERT INTO nums VALUES (20)" in
    let* () = exec "INSERT INTO nums VALUES (30)" in
    let* rows_r = Db.query db
      "SELECT n, SUM(n) OVER (ORDER BY n ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS s FROM nums ORDER BY n" in
    let rows = match rows_r with Ok s -> Lwt_main.run (Lwt_stream.to_list s) | Error e -> failwith (Format.asprintf "%a" Db.pp_error e) in
    let pairs = List.map (function
      | [Db.V_int n; Db.V_int s] -> (Int64.to_int n, Int64.to_int s)
      | _ -> (-1, -1)) rows in
    Alcotest.(check (list (pair int int)))
      "unbounded preceding running sum" [(10,10);(20,30);(30,60)] pairs;
    Lwt.return_unit)
```

These should fail at parse time (unknown syntax).

- [ ] **Step 2: Run tests to verify they fail**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe -- test 'window/rows_frame' 'window/rows_unbounded' -v
```

Expected: FAIL (parse error or syntax error).

- [ ] **Step 3: Add AST types for frame spec**

In `lib/sql/ast.ml`, after line `type collation = …` (around line 63), add:

```ocaml
type frame_unit = Frame_rows | Frame_range

type frame_bound =
  | FB_unbounded_preceding
  | FB_preceding of int         (** N PRECEDING *)
  | FB_current_row
  | FB_following of int         (** N FOLLOWING *)
  | FB_unbounded_following

type frame_spec = {
  unit  : frame_unit;
  start : frame_bound;
  end_  : frame_bound;
}
```

Then update `window_spec` (around line 123) to add a `frame` field:

```ocaml
and window_spec = {
  partition_by : expr list;
  order_by     : order_key list;
  frame        : frame_spec option;   (** None = SQLite default *)
}
```

**Important:** Adding `frame` to the record breaks any code that constructs `window_spec` without the field. There is exactly **one** such place in `parser.mly`:
```ocaml
{ Ast.{ partition_by = pb; order_by = ob } }
```
This will be updated in Task 2 Step 5.

- [ ] **Step 4: Add `PRECEDING` and `FOLLOWING` tokens to lexer**

In `lib/sql/lexer.mll`, add to the keyword block (near the other SQL keywords):

```
| "PRECEDING" | "preceding" { PRECEDING }
| "FOLLOWING" | "following" { FOLLOWING }
```

- [ ] **Step 5: Add frame parsing to parser**

In `lib/sql/parser.mly`:

1. Add tokens declaration (on the same line as similar tokens or on its own line):
```
%token PRECEDING FOLLOWING
```

2. Update `window_spec` rule to accept optional frame spec:
```ocaml
window_spec:
  | LPAREN pb = partition_clause ob = order_by_clause fs = option(frame_spec) RPAREN
    { Ast.{ partition_by = pb; order_by = ob; frame = fs } }
```

3. Add `frame_spec` and `frame_bound` rules (add after `window_spec`):
```ocaml
frame_spec:
  | unit_id = IDENT BETWEEN start = frame_bound AND end_ = frame_bound
    { let unit = match String.uppercase_ascii unit_id with
        | "ROWS"  -> Ast.Frame_rows
        | "RANGE" -> Ast.Frame_range
        | other   -> failwith (Printf.sprintf "expected ROWS or RANGE, got: %s" other)
      in
      Ast.{ unit; start; end_ } }

frame_bound:
  | u = IDENT PRECEDING
    { match String.uppercase_ascii u with
      | "UNBOUNDED" -> Ast.FB_unbounded_preceding
      | other -> failwith (Printf.sprintf "expected UNBOUNDED PRECEDING, got: %s PRECEDING" other) }
  | n = INT_LIT PRECEDING { Ast.FB_preceding (Int64.to_int n) }
  | c = IDENT r = IDENT
    { match String.uppercase_ascii c, String.uppercase_ascii r with
      | "CURRENT", "ROW" -> Ast.FB_current_row
      | _ -> failwith (Printf.sprintf "expected CURRENT ROW, got: %s %s" c r) }
  | n = INT_LIT FOLLOWING { Ast.FB_following (Int64.to_int n) }
  | u = IDENT FOLLOWING
    { match String.uppercase_ascii u with
      | "UNBOUNDED" -> Ast.FB_unbounded_following
      | other -> failwith (Printf.sprintf "expected UNBOUNDED FOLLOWING, got: %s FOLLOWING" other) }
```

**Note on grammar:** `BETWEEN` is reused from the `expr` grammar. This is fine here because `frame_spec` is only reachable from inside `window_spec` which is `LPAREN … RPAREN`. The `AND` between frame bounds is the same token as logical AND — no conflict because frame_bound is unambiguous.

- [ ] **Step 6: Propagate frame through sema**

In `lib/sql/sema.mli`, update `window_sema` (around line 51):
```ocaml
type window_sema = {
  func         : Ast.window_func;
  args         : bound_expr list;
  partition_by : bound_expr list;
  order_by     : bound_order_key list;
  frame        : Ast.frame_spec option;
}
```

In `lib/sql/sema.ml`, update the `ws` construction in `bind_ww` (around line 1301):
```ocaml
let ws = { func; args = bound_args;
           partition_by = bound_pb;
           order_by = bound_ob;
           frame = window.Ast.frame } in
```

- [ ] **Step 7: Propagate frame through plan**

In `lib/sql/plan.ml`, update `window_plan_item` (around line 39):
```ocaml
type window_plan_item = {
  func         : Ast.window_func;
  args         : expr list;
  partition_by : expr list;
  order_by     : (expr * [`Asc | `Desc]) list;
  frame        : Ast.frame_spec option;
}
```

In `lib/sql/planner.ml`, update `plan_window_item` (around line 148):
```ocaml
let plan_window_item (ws : Sema.window_sema) : Plan.window_plan_item =
  { Plan.func         = ws.Sema.func;
    args         = List.map plan_expr ws.Sema.args;
    partition_by = List.map plan_expr ws.Sema.partition_by;
    order_by     = List.map (fun (bk : Sema.bound_order_key) ->
      let dir = match bk.Sema.dir with Ast.Asc -> `Asc | Ast.Desc -> `Desc in
      (plan_expr bk.Sema.key, dir)
    ) ws.Sema.order_by;
    frame        = ws.Sema.frame;
  }
```

- [ ] **Step 8: Implement frame bounds in exec**

In `lib/sql/exec.ml`, the `WF_agg` case inside `compute_window_for_partition` (~line 2128) currently has:

```ocaml
| Ast.WF_agg agg_func ->
  let has_order = wplan.Plan.order_by <> [] in
  ...
  for pos = 0 to n - 1 do
    let frame_end = if has_order then pos else n - 1 in
    let indices = List.init (frame_end + 1) (fun i -> i) in
    ...
```

Replace the `frame_end` computation with a helper that respects `wplan.Plan.frame`:

```ocaml
| Ast.WF_agg agg_func ->
  let has_order = wplan.Plan.order_by <> [] in
  let arg_expr = match wplan.Plan.args with e :: _ -> Some e | [] -> None in
  let arg_vals = Array.init n (fun pos ->
    match arg_expr with
    | Some e -> eval_expr clock params sorted_rows.(pos) e
    | None   -> Row.V_null
  ) in
  let resolve_bound bound pos =
    match bound with
    | Ast.FB_unbounded_preceding -> 0
    | Ast.FB_preceding k         -> max 0 (pos - k)
    | Ast.FB_current_row         -> pos
    | Ast.FB_following k         -> min (n - 1) (pos + k)
    | Ast.FB_unbounded_following -> n - 1
  in
  for pos = 0 to n - 1 do
    let (frame_start, frame_end) = match wplan.Plan.frame with
      | None ->
        let fe = if has_order then pos else n - 1 in
        (0, fe)
      | Some spec ->
        (resolve_bound spec.Ast.start pos, resolve_bound spec.Ast.end_ pos)
    in
    let frame_start = max 0 frame_start in
    let frame_end   = min (n - 1) frame_end in
    let indices = if frame_start > frame_end then []
                  else List.init (frame_end - frame_start + 1) (fun i -> frame_start + i) in
    let result = match agg_func with
      (* ... keep existing agg_func match unchanged, but use `indices` from above ... *)
```

**Careful:** the existing `indices` was `List.init (frame_end + 1) (fun i -> i)` (always starting from 0). With ROWS frame, `indices` now starts from `frame_start`. Keep the rest of the `agg_func` match arms unchanged — they all use `indices` correctly.

- [ ] **Step 9: Run tests to verify passing**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest
```

Expected: ALL tests pass (new `window/rows_frame` and `window/rows_unbounded` pass; all prior tests still pass).

- [ ] **Step 10: Commit**

```bash
git add lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly \
        lib/sql/sema.mli lib/sql/sema.ml lib/sql/plan.ml \
        lib/sql/planner.ml lib/sql/exec.ml test/test_e2e.ml
git commit -m "feat(phase17): ROWS/RANGE BETWEEN window frame spec"
```

---

## Task 3: PERCENT_RANK and CUME_DIST Window Functions

**Goal:** Support `PERCENT_RANK() OVER (…)` and `CUME_DIST() OVER (…)` window functions, which return values in [0,1].

- `PERCENT_RANK(pos)` = `(rank - 1) / (n - 1)`, where `rank` = 1 + count of rows with strictly smaller ORDER BY key. If `n = 1`, returns 0.0.
- `CUME_DIST(pos)` = `(count of rows with ORDER BY key ≤ current) / n`.

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_e2e.ml`

---

- [ ] **Step 1: Write failing tests**

Add to `test/test_e2e.ml`:

```ocaml
let test_window_percent_rank () =
  let db = fresh_db () in
  run (
    let open Lwt.Syntax in
    let exec sql = Db.execute db sql >>= function
      | Ok () -> Lwt.return_unit
      | Error e -> Lwt.fail_with (Format.asprintf "%a" Db.pp_error e) in
    let* () = exec "CREATE TABLE scores (s INTEGER)" in
    let* () = exec "INSERT INTO scores VALUES (10)" in
    let* () = exec "INSERT INTO scores VALUES (20)" in
    let* () = exec "INSERT INTO scores VALUES (20)" in
    let* () = exec "INSERT INTO scores VALUES (30)" in
    let* rows_r = Db.query db
      "SELECT s, PERCENT_RANK() OVER (ORDER BY s) AS pr FROM scores ORDER BY s" in
    let rows = match rows_r with Ok s -> Lwt_main.run (Lwt_stream.to_list s) | Error e -> failwith (Format.asprintf "%a" Db.pp_error e) in
    (* n=4; ranks: 10->rank1 pr=0/3=0.0, 20->rank2 pr=1/3≈0.333, 20->rank2 pr=1/3≈0.333, 30->rank4 pr=3/3=1.0 *)
    let pairs = List.map (function
      | [Db.V_int s; Db.V_real pr] -> (Int64.to_int s, Float.round (pr *. 1000.0) /. 1000.0)
      | _ -> (-1, -1.0)) rows in
    Alcotest.(check (list (pair int (float 0.001))))
      "percent_rank" [(10, 0.0); (20, 0.333); (20, 0.333); (30, 1.0)] pairs;
    Lwt.return_unit)

let test_window_cume_dist () =
  let db = fresh_db () in
  run (
    let open Lwt.Syntax in
    let exec sql = Db.execute db sql >>= function
      | Ok () -> Lwt.return_unit
      | Error e -> Lwt.fail_with (Format.asprintf "%a" Db.pp_error e) in
    let* () = exec "CREATE TABLE scores (s INTEGER)" in
    let* () = exec "INSERT INTO scores VALUES (10)" in
    let* () = exec "INSERT INTO scores VALUES (20)" in
    let* () = exec "INSERT INTO scores VALUES (20)" in
    let* () = exec "INSERT INTO scores VALUES (30)" in
    let* rows_r = Db.query db
      "SELECT s, CUME_DIST() OVER (ORDER BY s) AS cd FROM scores ORDER BY s" in
    let rows = match rows_r with Ok s -> Lwt_main.run (Lwt_stream.to_list s) | Error e -> failwith (Format.asprintf "%a" Db.pp_error e) in
    (* n=4; 10: 1 row ≤ 10 → 1/4=0.25; 20: 3 rows ≤ 20 → 3/4=0.75; 30: 4 rows ≤ 30 → 4/4=1.0 *)
    let pairs = List.map (function
      | [Db.V_int s; Db.V_real cd] -> (Int64.to_int s, Float.round (cd *. 1000.0) /. 1000.0)
      | _ -> (-1, -1.0)) rows in
    Alcotest.(check (list (pair int (float 0.001))))
      "cume_dist" [(10, 0.25); (20, 0.75); (20, 0.75); (30, 1.0)] pairs;
    Lwt.return_unit)
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe -- test 'window/percent_rank' 'window/cume_dist' -v
```

Expected: FAIL (parse error — unknown window function).

- [ ] **Step 3: Add AST variants**

In `lib/sql/ast.ml`, update `window_func` to add two new variants:

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
  | WF_percent_rank    (** PERCENT_RANK() *)
  | WF_cume_dist       (** CUME_DIST() *)
```

- [ ] **Step 4: Add to parser**

In `lib/sql/parser.mly`, update `window_func_name` (around line 476):

```ocaml
window_func_name:
  | id = IDENT
    { match String.uppercase_ascii id with
      | "ROW_NUMBER"    -> Ast.WF_row_number
      | "RANK"          -> Ast.WF_rank
      | "DENSE_RANK"    -> Ast.WF_dense_rank
      | "NTILE"         -> Ast.WF_ntile
      | "LAG"           -> Ast.WF_lag
      | "LEAD"          -> Ast.WF_lead
      | "FIRST_VALUE"   -> Ast.WF_first_value
      | "LAST_VALUE"    -> Ast.WF_last_value
      | "NTH_VALUE"     -> Ast.WF_nth_value
      | "PERCENT_RANK"  -> Ast.WF_percent_rank
      | "CUME_DIST"     -> Ast.WF_cume_dist
      | other           -> failwith (Printf.sprintf "Unknown window function: %s" other) }
```

- [ ] **Step 5: Implement in exec**

In `lib/sql/exec.ml`, inside `compute_window_for_partition` (~line 2005), add two new match cases **before** `WF_agg`:

```ocaml
   | Ast.WF_percent_rank ->
     (* PERCENT_RANK = (rank - 1) / (n - 1); rank = 1 + #{rows with strictly smaller ORDER BY key} *)
     let order_vals pos =
       List.map (fun (e, _) -> eval_expr clock params sorted_rows.(pos) e) wplan.Plan.order_by
     in
     for pos = 0 to n - 1 do
       let rank =
         if wplan.Plan.order_by = [] then 1
         else
           let cur_vals = order_vals pos in
           1 + List.length (List.filter (fun i ->
             let cv = List.combine (order_vals i) cur_vals in
             List.exists (fun (a, b) -> compare_values a b < 0) [List.hd cv] &&
             List.for_all (fun (a, b) -> compare_values a b <= 0) cv
           ) (List.init n (fun i -> i)))
       in
       let pct = if n <= 1 then 0.0
                 else Float.of_int (rank - 1) /. Float.of_int (n - 1) in
       results.(sorted_orig_idxs.(pos)) <- Row.V_real pct
     done

   | Ast.WF_cume_dist ->
     (* CUME_DIST = (# rows with ORDER BY key ≤ current) / n *)
     let order_vals pos =
       List.map (fun (e, _) -> eval_expr clock params sorted_rows.(pos) e) wplan.Plan.order_by
     in
     for pos = 0 to n - 1 do
       let cur_vals = order_vals pos in
       let count =
         if wplan.Plan.order_by = [] then n
         else
           List.length (List.filter (fun i ->
             let cmp = List.fold_left2 (fun acc a b ->
               if acc <> 0 then acc else compare_values a b
             ) 0 (order_vals i) cur_vals in
             cmp <= 0
           ) (List.init n (fun i -> i)))
       in
       let cd = Float.of_int count /. Float.of_int n in
       results.(sorted_orig_idxs.(pos)) <- Row.V_real cd
     done
```

**Note on PERCENT_RANK implementation:** The rank calculation above is a bit complex. A cleaner O(n²) implementation:

```ocaml
   | Ast.WF_percent_rank ->
     let peer_vals pos =
       List.map (fun (e, _) -> eval_expr clock params sorted_rows.(pos) e) wplan.Plan.order_by
     in
     for pos = 0 to n - 1 do
       let rank =
         if wplan.Plan.order_by = [] then 1
         else begin
           (* rank = 1 + number of rows that sort strictly before pos *)
           let strictly_before i =
             let cmp = List.fold_left2 (fun acc a b ->
               if acc <> 0 then acc else compare_values a b
             ) 0 (peer_vals i) (peer_vals pos) in
             cmp < 0
           in
           1 + List.length (List.filter strictly_before (List.init pos (fun i -> i)))
         end
       in
       let pct = if n <= 1 then 0.0
                 else Float.of_int (rank - 1) /. Float.of_int (n - 1) in
       results.(sorted_orig_idxs.(pos)) <- Row.V_real pct
     done

   | Ast.WF_cume_dist ->
     let peer_vals pos =
       List.map (fun (e, _) -> eval_expr clock params sorted_rows.(pos) e) wplan.Plan.order_by
     in
     for pos = 0 to n - 1 do
       let at_or_before i =
         if wplan.Plan.order_by = [] then true
         else
           let cmp = List.fold_left2 (fun acc a b ->
             if acc <> 0 then acc else compare_values a b
           ) 0 (peer_vals i) (peer_vals pos) in
           cmp <= 0
       in
       let count = List.length (List.filter at_or_before (List.init n (fun i -> i))) in
       results.(sorted_orig_idxs.(pos)) <- Row.V_real (Float.of_int count /. Float.of_int n)
     done
```

Note: `List.fold_left2` requires both lists to have the same length — this is guaranteed because both `peer_vals i` and `peer_vals pos` evaluate the same `wplan.Plan.order_by` list. If `order_by = []`, both lists are `[]` and `List.fold_left2` returns `0` — handled by the `if wplan.Plan.order_by = [] then` branches above.

- [ ] **Step 6: Run tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest
```

Expected: ALL tests pass.

- [ ] **Step 7: Commit**

```bash
git add lib/sql/ast.ml lib/sql/parser.mly lib/sql/exec.ml test/test_e2e.ml
git commit -m "feat(phase17): PERCENT_RANK and CUME_DIST window functions"
```

---

## Task 4: CREATE TABLE/INDEX IF NOT EXISTS

**Goal:** Support `CREATE TABLE IF NOT EXISTS t (…)` and `CREATE INDEX IF NOT EXISTS idx ON t (…)`. When the table/index already exists and `IF NOT EXISTS` is specified, silently succeed instead of raising an error.

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/sema.mli`
- Modify: `lib/sql/sema.ml`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/planner.ml`
- Modify: `lib/sql/exec.ml`
- Modify: `lib/catalog/catalog.ml` (add `table_exists` helper)
- Modify: `lib/catalog/catalog.mli`
- Modify: `test/test_e2e.ml`

---

- [ ] **Step 1: Write failing tests**

Add to `test/test_e2e.ml`:

```ocaml
let test_create_table_if_not_exists () =
  let db = fresh_db () in
  run (
    let open Lwt.Syntax in
    let exec sql = Db.execute db sql >>= function
      | Ok () -> Lwt.return_unit
      | Error e -> Lwt.fail_with (Format.asprintf "%a" Db.pp_error e) in
    let* () = exec "CREATE TABLE t (id INTEGER)" in
    (* Second CREATE IF NOT EXISTS should succeed silently *)
    let* () = exec "CREATE TABLE IF NOT EXISTS t (id INTEGER)" in
    let* () = exec "INSERT INTO t VALUES (42)" in
    let* rows_r = Db.query db "SELECT id FROM t" in
    let rows = match rows_r with Ok s -> Lwt_main.run (Lwt_stream.to_list s) | Error e -> failwith (Format.asprintf "%a" Db.pp_error e) in
    Alcotest.(check int) "one row" 1 (List.length rows);
    Lwt.return_unit)

let test_create_index_if_not_exists () =
  let db = fresh_db () in
  run (
    let open Lwt.Syntax in
    let exec sql = Db.execute db sql >>= function
      | Ok () -> Lwt.return_unit
      | Error e -> Lwt.fail_with (Format.asprintf "%a" Db.pp_error e) in
    let* () = exec "CREATE TABLE t (id INTEGER)" in
    let* () = exec "CREATE INDEX idx_t ON t (id)" in
    (* Second CREATE INDEX IF NOT EXISTS should succeed silently *)
    let* () = exec "CREATE INDEX IF NOT EXISTS idx_t ON t (id)" in
    Lwt.return_unit)

let test_create_table_without_if_not_exists_fails () =
  let db = fresh_db () in
  run (
    let open Lwt.Syntax in
    let exec sql = Db.execute db sql >>= function
      | Ok () -> Lwt.return_unit
      | Error e -> Lwt.fail_with (Format.asprintf "%a" Db.pp_error e) in
    let* () = exec "CREATE TABLE t (id INTEGER)" in
    let* result = Db.execute db "CREATE TABLE t (id INTEGER)" in
    (match result with
     | Error _ -> ()
     | Ok () -> Alcotest.fail "expected error when creating duplicate table");
    Lwt.return_unit)
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev \
  dune exec test/test_e2e.exe -- test 'if_not_exists/table' -v
```

Expected: FAIL (parse error).

- [ ] **Step 3: Update AST**

In `lib/sql/ast.ml`, update `S_create_table` to add `if_not_exists`:

```ocaml
  | S_create_table of {
      name           : string;
      columns        : column_def list;
      constraints    : table_constraint list;
      if_not_exists  : bool;
    }
```

Update `S_create_index`:

```ocaml
  | S_create_index of {
      name           : string;
      table          : string;
      columns        : string list;
      unique         : bool;
      if_not_exists  : bool;
    }
```

**Note:** These record changes will break every place that constructs these AST nodes. The places are: `parser.mly` (in `create_table` and `create_index` rules) and `test/test_sema.ml` (if it constructs `S_create_table` directly). Check with:
```bash
grep -rn "S_create_table\|S_create_index" lib/ test/ | grep -v "_build"
```

- [ ] **Step 4: Add IF token to lexer**

In `lib/sql/lexer.mll`, add:
```
| "IF" | "if" { IF }
```

Place this **after** any longer keywords that start with IF (like `IFNULL`, `IIF`) to avoid ambiguity. In ocamllex, the longest match wins, so order doesn't matter for correctness, but it's good practice to list longer patterns first.

- [ ] **Step 5: Update parser**

In `lib/sql/parser.mly`:

1. Add token: `%token IF`

2. Update `create_table` rule:

```ocaml
create_table:
  | CREATE TABLE name = IDENT LPAREN items = separated_nonempty_list(COMMA, table_item) RPAREN
    { let cols = List.filter_map (function TI_col c -> Some c | _ -> None) items in
      let cons = List.filter_map (function TI_constraint c -> Some c | _ -> None) items in
      S_create_table { name; columns = cols; constraints = cons; if_not_exists = false } }
  | CREATE TABLE IF NOT EXISTS name = IDENT LPAREN items = separated_nonempty_list(COMMA, table_item) RPAREN
    { let cols = List.filter_map (function TI_col c -> Some c | _ -> None) items in
      let cons = List.filter_map (function TI_constraint c -> Some c | _ -> None) items in
      S_create_table { name; columns = cols; constraints = cons; if_not_exists = true } }
```

3. Update `create_index` rule:

```ocaml
create_index:
  | CREATE INDEX name = IDENT ON table = IDENT
      LPAREN cols = separated_nonempty_list(COMMA, IDENT) RPAREN
    { S_create_index { name; table; columns = cols; unique = false; if_not_exists = false } }
  | CREATE UNIQUE INDEX name = IDENT ON table = IDENT
      LPAREN cols = separated_nonempty_list(COMMA, IDENT) RPAREN
    { S_create_index { name; table; columns = cols; unique = true; if_not_exists = false } }
  | CREATE INDEX IF NOT EXISTS name = IDENT ON table = IDENT
      LPAREN cols = separated_nonempty_list(COMMA, IDENT) RPAREN
    { S_create_index { name; table; columns = cols; unique = false; if_not_exists = true } }
  | CREATE UNIQUE INDEX IF NOT EXISTS name = IDENT ON table = IDENT
      LPAREN cols = separated_nonempty_list(COMMA, IDENT) RPAREN
    { S_create_index { name; table; columns = cols; unique = true; if_not_exists = true } }
```

Also add `%token IF` to the token declarations section in the parser header.

- [ ] **Step 6: Propagate through sema**

In `lib/sql/sema.mli`, find `BS_create_table` and add `if_not_exists`:

```ocaml
  | BS_create_table of {
      name           : string;
      columns        : Sqlocaml_encoding.Row.column list;
      uniq_idxs      : (string * string list) list;
      if_not_exists  : bool;
    }
```

Find `BS_create_index`:

```ocaml
  | BS_create_index of {
      name           : string;
      table_meta     : Sqlocaml_catalog.Catalog.table_meta;
      columns        : string list;
      unique         : bool;
      if_not_exists  : bool;
    }
```

In `lib/sql/sema.ml`, update the `S_create_table` bind arm to pass through `if_not_exists`. Find the line that produces `BS_create_table { name; columns = ...; uniq_idxs = ... }` and add `if_not_exists = s.if_not_exists`. Do the same for `S_create_index` → `BS_create_index`.

- [ ] **Step 7: Propagate through plan**

In `lib/sql/plan.ml`, update `Op_create_table`:

```ocaml
  | Op_create_table of {
      name          : string;
      columns       : Sqlocaml_encoding.Row.column list;
      uniq_idxs     : (string * string list) list;
      if_not_exists : bool;
    }
```

Update `Op_create_index`:

```ocaml
  | Op_create_index of {
      table_meta    : Cat.table_meta;
      name          : string;
      columns       : string list;
      unique        : bool;
      if_not_exists : bool;
    }
```

In `lib/sql/planner.ml`, update the plan creation for `BS_create_table` and `BS_create_index` to include `if_not_exists`.

- [ ] **Step 8: Add `table_exists` to catalog**

In `lib/catalog/catalog.mli`:

```ocaml
(** Check whether a table with [name] already exists in the catalog. *)
val table_exists : t -> name:string -> bool
```

In `lib/catalog/catalog.ml`:

```ocaml
let table_exists t ~name = Hashtbl.mem t.cache name
```

Also add `index_exists`:

```ocaml
(* catalog.mli *)
val index_exists : t -> name:string -> bool

(* catalog.ml *)
let index_exists t ~name = Hashtbl.mem t.indexes name
```

- [ ] **Step 9: Handle IF NOT EXISTS in exec**

In `lib/sql/exec.ml`, find the `Op_create_table` handler. It currently calls `Cat.create_table` which raises `Failure` if the table already exists. Wrap it:

```ocaml
  | Plan.Op_create_table { name; columns; uniq_idxs; if_not_exists } ->
    if if_not_exists && Cat.table_exists cat ~name then
      Lwt.return 0
    else
      (* existing create_table logic unchanged *)
      ...
```

Find `Op_create_index` handler. Wrap similarly:

```ocaml
  | Plan.Op_create_index { table_meta; name; columns; unique; if_not_exists } ->
    if if_not_exists && Cat.index_exists cat ~name then
      Lwt.return 0
    else
      (* existing create_index logic unchanged *)
      ...
```

Look for the `Op_create_table` case in **both** `execute_with_count` and `query` (which delegates to `execute_with_count`). There is one place in `execute_with_count`. Verify it covers both paths.

- [ ] **Step 10: Fix test_sema.ml if needed**

Run `grep -n "S_create_table\|S_create_index" test/test_sema.ml` — if there are direct record constructions, add `if_not_exists = false` to each.

- [ ] **Step 11: Run all tests**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest
```

Expected: ALL tests pass.

- [ ] **Step 12: Commit**

```bash
git add lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly \
        lib/sql/sema.mli lib/sql/sema.ml lib/sql/plan.ml \
        lib/sql/planner.ml lib/sql/exec.ml \
        lib/catalog/catalog.mli lib/catalog/catalog.ml \
        test/test_e2e.ml
git commit -m "feat(phase17): CREATE TABLE/INDEX IF NOT EXISTS"
```

---

## Task 5: SQLite Comparison Tests + Coverage Report

**Goal:** Add SQLite comparison tests for all four Phase 17 features and confirm the full suite still passes.

**Files:**
- Modify: `test/test_sqlite_compare.ml`

---

- [ ] **Step 1: Add Phase 17 comparison test cases**

In `test/test_sqlite_compare.ml`, add test case lists for each feature. Find the section where `phase16_*` tests are defined and add after them:

```ocaml
let phase17_frame_cases = [
  (* ROWS BETWEEN 1 PRECEDING AND CURRENT ROW sliding sum *)
  { setup = ["CREATE TABLE nums (n INTEGER)";
             "INSERT INTO nums VALUES (1)";
             "INSERT INTO nums VALUES (2)";
             "INSERT INTO nums VALUES (3)"];
    query = "SELECT n, SUM(n) OVER (ORDER BY n ROWS BETWEEN 1 PRECEDING AND CURRENT ROW) AS s FROM nums ORDER BY n";
    label = "rows_1_preceding_sum" };
  (* ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW *)
  { setup = ["CREATE TABLE t2 (v INTEGER)";
             "INSERT INTO t2 VALUES (5)";
             "INSERT INTO t2 VALUES (3)";
             "INSERT INTO t2 VALUES (8)"];
    query = "SELECT v, SUM(v) OVER (ORDER BY v ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS s FROM t2 ORDER BY v";
    label = "rows_unbounded_preceding_sum" };
  (* ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING *)
  { setup = ["CREATE TABLE t3 (v INTEGER)";
             "INSERT INTO t3 VALUES (10)";
             "INSERT INTO t3 VALUES (20)";
             "INSERT INTO t3 VALUES (30)"];
    query = "SELECT v, SUM(v) OVER (ORDER BY v ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING) AS s FROM t3 ORDER BY v";
    label = "rows_centered_sum" };
]

let phase17_pctrank_cases = [
  { setup = ["CREATE TABLE sc (s INTEGER)";
             "INSERT INTO sc VALUES (10)";
             "INSERT INTO sc VALUES (20)";
             "INSERT INTO sc VALUES (20)";
             "INSERT INTO sc VALUES (30)"];
    query = "SELECT s, CAST(ROUND(PERCENT_RANK() OVER (ORDER BY s), 3) AS TEXT) AS pr FROM sc ORDER BY s";
    label = "percent_rank" };
  { setup = ["CREATE TABLE sc2 (s INTEGER)";
             "INSERT INTO sc2 VALUES (10)";
             "INSERT INTO sc2 VALUES (20)";
             "INSERT INTO sc2 VALUES (20)";
             "INSERT INTO sc2 VALUES (30)"];
    query = "SELECT s, CAST(ROUND(CUME_DIST() OVER (ORDER BY s), 2) AS TEXT) AS cd FROM sc2 ORDER BY s";
    label = "cume_dist" };
]

let phase17_if_not_exists_cases = [
  { setup = ["CREATE TABLE t (id INTEGER)";
             "INSERT INTO t VALUES (1)"];
    query = "SELECT (SELECT COUNT(*) FROM t) AS cnt";
    label = "ine_base_count";
    (* The IF NOT EXISTS DDL itself is tested in e2e; here we test the query works after *) };
]
```

Note: the test case record format is `{ setup: string list; query: string; label: string }`. Check the actual record type at the top of `test_sqlite_compare.ml` (look for `type test_case` or similar) and use the exact field names.

Then register the new groups in the runner (find where `phase16_*` groups are registered):

```ocaml
"phase17_frame",         List.map make_test phase17_frame_cases;
"phase17_pctrank",       List.map make_test phase17_pctrank_cases;
```

- [ ] **Step 2: Run SQLite comparison tests**

```bash
podman run --rm \
  -v $(pwd):/workspace:Z \
  -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
  -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
  -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
  -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
  -w /workspace sqlocaml-dev dune exec test/test_sqlite_compare.exe
```

Expected: all existing tests + new Phase 17 tests pass.

- [ ] **Step 3: Run full test suite**

```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune runtest
```

Expected: all tests pass.

- [ ] **Step 4: Commit**

```bash
git add test/test_sqlite_compare.ml
git commit -m "test(phase17): SQLite comparison tests for frame spec, PERCENT_RANK, CUME_DIST"
```

---

## Self-Review Checklist

**Spec coverage:**
- [x] View persistence: catalog stores SQL text, loads on open_file/open_block, in-memory DBs unaffected
- [x] ROWS frame: `ROWS BETWEEN N PRECEDING AND CURRENT ROW`, `UNBOUNDED PRECEDING`, `UNBOUNDED FOLLOWING`, `N FOLLOWING`; threaded through all 4 layers
- [x] RANGE frame: parsed with the same rule (frame_unit distinguishes ROWS vs RANGE), though RANGE with numeric bounds defers to ROWS semantics — document this in a comment in exec.ml
- [x] PERCENT_RANK: correct formula (rank-1)/(n-1), handles ties, handles n=1 edge case
- [x] CUME_DIST: correct formula count(≤ current)/n, handles ties
- [x] IF NOT EXISTS: both CREATE TABLE and CREATE INDEX, error still raised without IF NOT EXISTS
- [x] SQLite comparison tests for frame and pctrank/cume_dist

**Potential issues:**
1. **`window_spec` record extension** breaks parser.mly construction and any direct record constructions in tests. All must add `frame = fs` or `frame = None`.
2. **`S_create_table`/`S_create_index` record extension** breaks sema test constructors. Check `test/test_sema.ml` for direct constructions and add `if_not_exists = false`.
3. **`RANGE N PRECEDING/FOLLOWING`** — `frame_bound` with `INT_LIT PRECEDING` is in the grammar, and `Frame_range` is in the type. In exec, when `frame.unit = Frame_range`, use the same `resolve_bound` as `Frame_rows` (they differ only for numeric bounds, which requires comparing ORDER BY values — acceptable approximation for Phase 17). Add a comment to exec.ml: `(* RANGE with numeric bounds approximated as ROWS — full value-based RANGE not implemented *)`.
4. **`List.fold_left2` in PERCENT_RANK/CUME_DIST** requires equal-length lists. This is guaranteed by construction (both lists come from evaluating the same `order_by` list). If `order_by = []`, the function is called with `[]` and `[]`, which is safe.
5. **View persistence and in-memory DB**: `open_in_memory` creates `views = Hashtbl.create 4` without loading from store (correct — in-memory store is always fresh). Only `open_file`/`open_block` call `load_views_into_hashtbl`.
