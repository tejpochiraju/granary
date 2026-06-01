# Phase 21: SAVEPOINT / RELEASE / ROLLBACK TO + FK DELETE/UPDATE RESTRICT Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add SAVEPOINT / RELEASE / ROLLBACK TO nested transaction control (#127) and FK RESTRICT enforcement for DELETE and UPDATE of parent rows (#128).

**Architecture:** SAVEPOINTs are implemented as a snapshot stack on the in-memory Mem backend (B-tree deferred); each SAVEPOINT snapshots all tree contents, ROLLBACK TO restores and keeps the savepoint, RELEASE pops it. FK parent-side enforcement is a reverse-reference scan in execute_delete and execute_update: for each deleted/updated parent row, scan child tables for matching FK values and fail if found.

**Tech Stack:** OCaml 5.x, dune, menhir, alcotest, Lwt, podman build environment

**Build command** (use for every "Run:" step):
```bash
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
```

**Test commands:**
```bash
# e2e tests
podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe 2>&1 | tail -5

# SQLite comparison tests (with sqlite3 mounted)
podman run --rm \
  -v $(pwd):/workspace:Z \
  -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
  -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
  -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
  -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
  -w /workspace sqlocaml-dev dune exec test/test_sqlite_compare.exe 2>&1 | tail -10
```

---

## File Map

| File | Action | Responsibility |
|------|--------|---------------|
| `lib/store/store.ml` | Modify | Add `mem_savepoints` field to `t`; add `savepoint_begin/release/rollback` |
| `lib/store/store.mli` | Modify | Expose the three new savepoint functions |
| `lib/sql/ast.ml` | Modify | Add `S_savepoint`, `S_release`, `S_rollback_to` statement variants |
| `lib/sql/lexer.mll` | Modify | Add `SAVEPOINT`, `RELEASE` keyword tokens |
| `lib/sql/parser.mly` | Modify | Add `%token`, grammar rules for the three new statements |
| `lib/sql/sema.ml` | Modify | Add `BS_savepoint`, `BS_release`, `BS_rollback_to` to bound_stmt |
| `lib/sql/plan.ml` | Modify | Add `Op_savepoint`, `Op_release`, `Op_rollback_to` |
| `lib/sql/planner.ml` | Modify | Map BS_savepoint/release/rollback_to → Op_* |
| `lib/db/db.ml` | Modify | Add `savepoint_names`, `auto_began` to `t`; handle Op_savepoint/release/rollback_to |
| `lib/sql/exec.ml` | Modify | Add FK parent-side check in execute_delete and execute_update |
| `test/test_e2e.ml` | Modify | Add savepoint and FK delete/update tests |
| `test/test_sqlite_compare.ml` | Modify | Add phase21 comparison test cases |

---

## Task 1: Store layer — savepoint snapshot stack

**Files:**
- Modify: `lib/store/store.ml`
- Modify: `lib/store/store.mli`

- [ ] **Step 1: Add `mem_savepoints` field to `t` in store.ml**

  In `lib/store/store.ml`, find the `type t = {` definition (around line 69) and add the new field:

  ```ocaml
  type t = {
    backend  : backend;
    rw_mutex : Lwt_mutex.t;
    mutable mem_rw_snapshot : (tree_id * Bytes.t BytesMap.t) list option;
    (* Savepoint stack for the Mem backend; newest entry at front.
       Each entry is (savepoint_name, snapshot_of_all_trees). *)
    mutable mem_savepoints  : (string * (tree_id * Bytes.t BytesMap.t) list) list;
  }
  ```

- [ ] **Step 2: Initialize `mem_savepoints` in all `t` construction sites**

  Update `create ()` (line ~248):
  ```ocaml
  let create () : t =
    { backend = Mem (Hashtbl.create 16); rw_mutex = Lwt_mutex.create ();
      mem_rw_snapshot = None; mem_savepoints = [] }
  ```

  Update all `Lwt.return_ok { backend = Btree st; rw_mutex = ...; mem_rw_snapshot = None }` sites (4 occurrences) to add `mem_savepoints = []`.  Search for `mem_rw_snapshot = None }` — there are 4 such lines (around lines 333, 354, 397, 414). Add `mem_savepoints = []` to each.

- [ ] **Step 3: Clear savepoints in `rw_begin`, `commit`, `rollback`**

  In `rw_begin` (around line 438), in the `Mem` branch after setting `mem_rw_snapshot`:
  ```ocaml
     | Mem trees ->
       let snap = Hashtbl.fold (fun tid r acc -> (tid, !r) :: acc) trees [] in
       t.mem_rw_snapshot <- Some snap;
       t.mem_savepoints <- []
  ```

  In `commit` (around line 567), in the `Mem` branch:
  ```ocaml
     | Mem _ ->
       t.mem_rw_snapshot <- None;
       t.mem_savepoints <- [];
       Lwt.return_unit
  ```

  In `rollback` (around line 631), in the `Mem` branch, after setting `t.mem_rw_snapshot <- None`:
  Add `t.mem_savepoints <- []` after `t.mem_rw_snapshot <- None`.

- [ ] **Step 4: Add `savepoint_begin`, `savepoint_release`, `savepoint_rollback` to store.ml**

  Add these three functions after `rollback` (after line ~666):

  ```ocaml
  (* ------------------------------------------------------------------ *)
  (* Savepoints (Mem backend only; B-tree deferred)                      *)
  (* ------------------------------------------------------------------ *)

  (** Push a named savepoint: snapshot current Mem tree state. *)
  let savepoint_begin (Rw t : rw txn) name =
    match t.backend with
    | Mem trees ->
      let snap = Hashtbl.fold (fun tid r acc -> (tid, !r) :: acc) trees [] in
      t.mem_savepoints <- (name, snap) :: t.mem_savepoints;
      Lwt.return_unit
    | Btree _ -> Lwt.return_unit   (* B-tree savepoints deferred to a future phase *)

  (** Release the named savepoint and all newer ones (writes are kept). *)
  let savepoint_release (Rw t : rw txn) name =
    match t.backend with
    | Mem _ ->
      let rec drop = function
        | [] -> []
        | (n, _) :: rest when String.equal n name -> rest
        | _ :: rest -> drop rest
      in
      t.mem_savepoints <- drop t.mem_savepoints;
      Lwt.return_unit
    | Btree _ -> Lwt.return_unit

  (** Rollback to the named savepoint: restore snapshot, drop newer savepoints,
      keep the named savepoint so it can be rolled back to again. *)
  let savepoint_rollback (Rw t : rw txn) name =
    match t.backend with
    | Mem trees ->
      let rec find = function
        | [] -> ()   (* savepoint not found — no-op *)
        | (n, snap) :: rest when String.equal n name ->
          (* Restore tree contents to this snapshot. *)
          List.iter (fun (tid, map) ->
            match Hashtbl.find_opt trees tid with
            | None -> ()
            | Some r -> r := map
          ) snap;
          (* Remove trees that were created after this savepoint. *)
          let snap_tids = List.map fst snap in
          Hashtbl.iter (fun tid _ ->
            if not (List.mem tid snap_tids) then
              Hashtbl.remove trees tid
          ) (Hashtbl.copy trees);
          (* Keep the named savepoint at the top so it can be re-used. *)
          t.mem_savepoints <- (name, snap) :: rest
        | _ :: rest -> find rest
      in
      find t.mem_savepoints;
      Lwt.return_unit
    | Btree _ -> Lwt.return_unit
  ```

- [ ] **Step 5: Export the three new functions from store.mli**

  Add to `lib/store/store.mli` after the `rollback` declaration:

  ```ocaml
  (** Push a named savepoint by snapshotting current Mem tree state.
      No-op on the B-tree backend (deferred). *)
  val savepoint_begin   : rw txn -> string -> unit Lwt.t

  (** Release the named savepoint and all newer ones.
      Writes accumulated since the savepoint remain in the outer transaction.
      No-op on the B-tree backend. *)
  val savepoint_release : rw txn -> string -> unit Lwt.t

  (** Restore to the named savepoint, dropping all newer savepoints.
      The named savepoint is kept so ROLLBACK TO can be repeated.
      No-op on the B-tree backend. *)
  val savepoint_rollback : rw txn -> string -> unit Lwt.t
  ```

- [ ] **Step 6: Build to verify store layer compiles**

  Run:
  ```bash
  podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
  ```
  Expected: no errors.

- [ ] **Step 7: Commit**

  ```bash
  git add lib/store/store.ml lib/store/store.mli
  git commit -m "feat(phase21): add savepoint snapshot stack to Store (Mem backend) [#127]"
  ```

---

## Task 2: SQL pipeline — AST → lexer → parser → sema → plan → planner

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/sema.ml`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/planner.ml`

- [ ] **Step 1: Add new statement variants to ast.ml**

  In `lib/sql/ast.ml`, find the `stmt` type (around line 258) where `S_begin`, `S_commit`, `S_rollback` are defined:

  ```ocaml
  | S_begin
  | S_commit
  | S_rollback
  ```

  Add three new variants immediately after `S_rollback`:

  ```ocaml
  | S_begin
  | S_commit
  | S_rollback
  | S_savepoint   of string  (** SAVEPOINT name *)
  | S_release     of string  (** RELEASE name *)
  | S_rollback_to of string  (** ROLLBACK TO name *)
  ```

- [ ] **Step 2: Add SAVEPOINT and RELEASE tokens to lexer.mll**

  In `lib/sql/lexer.mll`, find the keyword matching block (around line 50) where `BEGIN`, `COMMIT`, `ROLLBACK` are handled:

  ```ocaml
  | "BEGIN"    { BEGIN }
  | "COMMIT"   { COMMIT }
  | "ROLLBACK" { ROLLBACK }
  ```

  Add SAVEPOINT and RELEASE immediately after ROLLBACK:

  ```ocaml
  | "BEGIN"     { BEGIN }
  | "COMMIT"    { COMMIT }
  | "ROLLBACK"  { ROLLBACK }
  | "SAVEPOINT" { SAVEPOINT }
  | "RELEASE"   { RELEASE }
  ```

  Note: `TO` is already a token (used in `ALTER TABLE ... RENAME TO`), so no change needed for it.

- [ ] **Step 3: Add tokens and grammar rules to parser.mly**

  In `lib/sql/parser.mly`, find the `%token` declarations (around line 29) where `BEGIN COMMIT ROLLBACK` are declared:

  ```
  %token BEGIN COMMIT ROLLBACK
  ```

  Change to:

  ```
  %token BEGIN COMMIT ROLLBACK SAVEPOINT RELEASE
  ```

  Find the `stmt:` production (around line 93) and add three new alternatives after `rollback_stmt`:

  ```
  stmt:
    | s = with_cte          { s }
    | s = create_view       { s }
    | s = drop_view         { s }
    | s = create_table      { s }
    | s = create_fts_table  { s }
    | s = create_index      { s }
    | s = insert            { s }
    | s = compound_select   { s }
    | s = update            { s }
    | s = delete            { s }
    | s = drop_table        { s }
    | s = drop_index        { s }
    | s = begin_stmt        { s }
    | s = commit_stmt       { s }
    | s = rollback_stmt     { s }
    | s = savepoint_stmt    { s }
    | s = release_stmt      { s }
    | s = rollback_to_stmt  { s }
    | s = pragma_stmt       { s }
    | s = alter_table       { s }
  ```

  Find the `rollback_stmt:` rule (around line 161) and add the three new grammar rules immediately after it:

  ```
  rollback_stmt:
    | ROLLBACK { S_rollback }

  savepoint_stmt:
    | SAVEPOINT name = IDENT { Ast.S_savepoint name }

  release_stmt:
    | RELEASE name = IDENT { Ast.S_release name }

  rollback_to_stmt:
    | ROLLBACK TO name = IDENT { Ast.S_rollback_to name }
  ```

  Note: menhir's LR(1) lookahead distinguishes `ROLLBACK` (followed by non-`TO`) from `ROLLBACK TO` (followed by `TO`). No conflict expected.

- [ ] **Step 4: Add bound_stmt variants to sema.ml**

  In `lib/sql/sema.ml`, find the `bound_stmt` type (around line 137) where `BS_begin`, `BS_commit`, `BS_rollback` are defined:

  ```ocaml
  | BS_begin
  | BS_commit
  | BS_rollback
  ```

  Add three new variants immediately after:

  ```ocaml
  | BS_begin
  | BS_commit
  | BS_rollback
  | BS_savepoint   of string
  | BS_release     of string
  | BS_rollback_to of string
  ```

  Find the `bind` function (around line 2326) where `Ast.S_begin`, `Ast.S_commit`, `Ast.S_rollback` are handled:

  ```ocaml
  | Ast.S_begin    -> Lwt.return (Ok BS_begin)
  | Ast.S_commit   -> Lwt.return (Ok BS_commit)
  | Ast.S_rollback -> Lwt.return (Ok BS_rollback)
  ```

  Add immediately after:

  ```ocaml
  | Ast.S_savepoint name   -> Lwt.return (Ok (BS_savepoint name))
  | Ast.S_release name     -> Lwt.return (Ok (BS_release name))
  | Ast.S_rollback_to name -> Lwt.return (Ok (BS_rollback_to name))
  ```

  Also update `bind_returning_params` if it has the same pattern (search for `Ast.S_begin` in the function and add the same three cases).

- [ ] **Step 5: Add Op variants to plan.ml**

  In `lib/sql/plan.ml`, find `Op_begin`, `Op_commit`, `Op_rollback` (around line 164):

  ```ocaml
  | Op_begin
  | Op_commit
  | Op_rollback
  ```

  Add three new variants immediately after:

  ```ocaml
  | Op_begin
  | Op_commit
  | Op_rollback
  | Op_savepoint   of string
  | Op_release     of string
  | Op_rollback_to of string
  ```

- [ ] **Step 6: Add planner cases to planner.ml**

  In `lib/sql/planner.ml`, find the cases for `Sema.BS_begin`, `Sema.BS_commit`, `Sema.BS_rollback` (around line 508):

  ```ocaml
  | Sema.BS_begin    -> Plan.Op_begin
  | Sema.BS_commit   -> Plan.Op_commit
  | Sema.BS_rollback -> Plan.Op_rollback
  ```

  Add immediately after:

  ```ocaml
  | Sema.BS_savepoint name   -> Plan.Op_savepoint name
  | Sema.BS_release name     -> Plan.Op_release name
  | Sema.BS_rollback_to name -> Plan.Op_rollback_to name
  ```

- [ ] **Step 7: Build to verify the pipeline compiles**

  Run:
  ```bash
  podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
  ```
  Expected: no errors. If menhir reports a conflict, check that `rollback_stmt` and `rollback_to_stmt` are separate non-terminals referenced from `stmt:`.

- [ ] **Step 8: Commit**

  ```bash
  git add lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly lib/sql/sema.ml lib/sql/plan.ml lib/sql/planner.ml
  git commit -m "feat(phase21): SAVEPOINT/RELEASE/ROLLBACK TO through AST→sema→plan pipeline [#127]"
  ```

---

## Task 3: DB layer — savepoint execution

**Files:**
- Modify: `lib/db/db.ml`

- [ ] **Step 1: Add `savepoint_names` and `auto_began` fields to `t`**

  In `lib/db/db.ml`, find the `type t = {` definition (around line 7):

  ```ocaml
  type t = {
    store            : S.t;
    catalog          : Cat.t;
    clock            : (unit -> float) option;
    mutable explicit_txn : S.rw S.txn option;
    views            : (string, Sql.Ast.stmt) Hashtbl.t;
  }
  ```

  Change to:

  ```ocaml
  type t = {
    store            : S.t;
    catalog          : Cat.t;
    clock            : (unit -> float) option;
    mutable explicit_txn    : S.rw S.txn option;
    views            : (string, Sql.Ast.stmt) Hashtbl.t;
    mutable savepoint_names : string list;  (* active savepoints, newest first *)
    mutable auto_began      : bool;         (* txn started implicitly by SAVEPOINT *)
  }
  ```

- [ ] **Step 2: Initialize new fields in all `t` construction sites**

  In `open_in_memory` (around line 29):
  ```ocaml
  Lwt.return { store; catalog; clock; explicit_txn = None; views = Hashtbl.create 4;
               savepoint_names = []; auto_began = false }
  ```

  In `open_file` (around line 57):
  ```ocaml
  Lwt.return (Ok { store; catalog; clock = None; explicit_txn = None; views;
                   savepoint_names = []; auto_began = false })
  ```

  In `open_block` (around line 72):
  ```ocaml
  Lwt.return (Ok { store; catalog; clock = None; explicit_txn = None; views;
                   savepoint_names = []; auto_began = false })
  ```

- [ ] **Step 3: Add savepoint helper functions**

  Add the following three functions after `rollback_txn` (around line 129) in `lib/db/db.ml`:

  ```ocaml
  let savepoint_txn t name =
    let* tx = match t.explicit_txn with
      | Some tx -> Lwt.return tx
      | None ->
        let* tx = S.rw_begin t.store in
        t.explicit_txn <- Some tx;
        t.auto_began <- true;
        Lwt.return tx
    in
    let* () = S.savepoint_begin tx name in
    t.savepoint_names <- name :: t.savepoint_names;
    Lwt.return (Ok ())

  let release_savepoint t name =
    match t.explicit_txn with
    | None -> Lwt.return (Error (Runtime "no active transaction for RELEASE"))
    | Some tx ->
      let* () = S.savepoint_release tx name in
      let rec drop = function
        | [] -> []
        | n :: rest when String.equal n name -> rest
        | _ :: rest -> drop rest
      in
      t.savepoint_names <- drop t.savepoint_names;
      if t.auto_began && t.savepoint_names = [] then begin
        let* () = S.commit tx in
        t.explicit_txn <- None;
        t.auto_began <- false;
        Lwt.return (Ok ())
      end else
        Lwt.return (Ok ())

  let rollback_to_savepoint t name =
    match t.explicit_txn with
    | None -> Lwt.return (Error (Runtime "no active transaction for ROLLBACK TO"))
    | Some tx ->
      let* () = S.savepoint_rollback tx name in
      let rec trim = function
        | [] -> []
        | n :: _ as rest when String.equal n name -> rest
        | _ :: rest -> trim rest
      in
      t.savepoint_names <- trim t.savepoint_names;
      Lwt.return (Ok ())
  ```

- [ ] **Step 4: Handle Op_savepoint/release/rollback_to in `execute`**

  In `lib/db/db.ml`, find the `execute` function (around line 135). It currently has:

  ```ocaml
  | Ok Sql.Plan.Op_begin    -> begin_txn t
  | Ok Sql.Plan.Op_commit   -> commit_txn t
  | Ok Sql.Plan.Op_rollback -> rollback_txn t
  ```

  Add three new arms immediately after:

  ```ocaml
  | Ok Sql.Plan.Op_begin    -> begin_txn t
  | Ok Sql.Plan.Op_commit   -> commit_txn t
  | Ok Sql.Plan.Op_rollback -> rollback_txn t
  | Ok Sql.Plan.Op_savepoint name   -> savepoint_txn t name
  | Ok Sql.Plan.Op_release name     -> release_savepoint t name
  | Ok Sql.Plan.Op_rollback_to name -> rollback_to_savepoint t name
  ```

- [ ] **Step 5: Handle Op_savepoint/release/rollback_to in `execute_change_count`**

  Find `execute_change_count` (around line 175). It has the same pattern. Add:

  ```ocaml
  | Ok Sql.Plan.Op_savepoint name ->
    let* r = savepoint_txn t name in
    (match r with Ok () -> Lwt.return (Ok 0) | Error e -> Lwt.return (Error e))
  | Ok Sql.Plan.Op_release name ->
    let* r = release_savepoint t name in
    (match r with Ok () -> Lwt.return (Ok 0) | Error e -> Lwt.return (Error e))
  | Ok Sql.Plan.Op_rollback_to name ->
    let* r = rollback_to_savepoint t name in
    (match r with Ok () -> Lwt.return (Ok 0) | Error e -> Lwt.return (Error e))
  ```

- [ ] **Step 6: Build**

  ```bash
  podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
  ```
  Expected: clean build.

- [ ] **Step 7: Write failing tests for SAVEPOINT in test_e2e.ml**

  In `test/test_e2e.ml`, add these test functions. Find the `exec` and `query_ok` helpers (already defined at top of file). Add after the last json test function:

  ```ocaml
  (* ── Phase 21: SAVEPOINT / RELEASE / ROLLBACK TO ──────────────── *)

  let test_savepoint_rollback_undoes_insert () =
    Lwt_main.run (
      let* db = Db.open_in_memory () in
      let exec sql = Db.execute db sql >>= fun r ->
        (match r with Ok () -> () | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e);
        Lwt.return_unit
      in
      let* () = exec "CREATE TABLE t (x INTEGER)" in
      let* () = exec "BEGIN" in
      let* () = exec "SAVEPOINT s1" in
      let* () = exec "INSERT INTO t VALUES (1)" in
      let* () = exec "ROLLBACK TO s1" in
      let* () = exec "COMMIT" in
      let* r = Db.query db "SELECT COUNT(*) FROM t" in
      let rows = match r with Ok s -> Lwt_main.run (Lwt_stream.to_list s) | Error e -> Alcotest.failf "%a" Db.pp_error e in
      Alcotest.(check int) "0 rows after rollback to savepoint" 0
        (Int64.to_int (match rows with [[|Db.V_int n|]] -> n | _ -> -1L));
      Lwt.return_unit)

  let test_savepoint_release_keeps_insert () =
    Lwt_main.run (
      let* db = Db.open_in_memory () in
      let exec sql = Db.execute db sql >>= fun r ->
        (match r with Ok () -> () | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e);
        Lwt.return_unit
      in
      let* () = exec "CREATE TABLE t2 (x INTEGER)" in
      let* () = exec "BEGIN" in
      let* () = exec "SAVEPOINT s1" in
      let* () = exec "INSERT INTO t2 VALUES (42)" in
      let* () = exec "RELEASE s1" in
      let* () = exec "COMMIT" in
      let* r = Db.query db "SELECT x FROM t2" in
      let rows = match r with Ok s -> Lwt_main.run (Lwt_stream.to_list s) | Error e -> Alcotest.failf "%a" Db.pp_error e in
      Alcotest.(check int) "1 row after release" 1 (List.length rows);
      Lwt.return_unit)

  let test_savepoint_partial_rollback () =
    Lwt_main.run (
      let* db = Db.open_in_memory () in
      let exec sql = Db.execute db sql >>= fun r ->
        (match r with Ok () -> () | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e);
        Lwt.return_unit
      in
      let* () = exec "CREATE TABLE t3 (x INTEGER)" in
      let* () = exec "BEGIN" in
      let* () = exec "INSERT INTO t3 VALUES (1)" in
      let* () = exec "SAVEPOINT s1" in
      let* () = exec "INSERT INTO t3 VALUES (2)" in
      let* () = exec "ROLLBACK TO s1" in
      let* () = exec "COMMIT" in
      let* r = Db.query db "SELECT COUNT(*) FROM t3" in
      let rows = match r with Ok s -> Lwt_main.run (Lwt_stream.to_list s) | Error e -> Alcotest.failf "%a" Db.pp_error e in
      Alcotest.(check int) "only pre-savepoint row survives" 1
        (Int64.to_int (match rows with [[|Db.V_int n|]] -> n | _ -> -1L));
      Lwt.return_unit)

  let test_savepoint_double_rollback () =
    Lwt_main.run (
      let* db = Db.open_in_memory () in
      let exec sql = Db.execute db sql >>= fun r ->
        (match r with Ok () -> () | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e);
        Lwt.return_unit
      in
      let* () = exec "CREATE TABLE t4 (x INTEGER)" in
      let* () = exec "BEGIN" in
      let* () = exec "SAVEPOINT s" in
      let* () = exec "INSERT INTO t4 VALUES (1)" in
      let* () = exec "ROLLBACK TO s" in
      (* Can rollback to same savepoint again *)
      let* () = exec "INSERT INTO t4 VALUES (2)" in
      let* () = exec "ROLLBACK TO s" in
      let* () = exec "COMMIT" in
      let* r = Db.query db "SELECT COUNT(*) FROM t4" in
      let rows = match r with Ok s -> Lwt_main.run (Lwt_stream.to_list s) | Error e -> Alcotest.failf "%a" Db.pp_error e in
      Alcotest.(check int) "savepoint reusable" 0
        (Int64.to_int (match rows with [[|Db.V_int n|]] -> n | _ -> -1L));
      Lwt.return_unit)
  ```

  Register under a new suite in the `Alcotest.run` call at the bottom:
  ```ocaml
    "savepoint", [
      Alcotest.test_case "rollback_undoes_insert"  `Quick test_savepoint_rollback_undoes_insert;
      Alcotest.test_case "release_keeps_insert"    `Quick test_savepoint_release_keeps_insert;
      Alcotest.test_case "partial_rollback"        `Quick test_savepoint_partial_rollback;
      Alcotest.test_case "double_rollback"         `Quick test_savepoint_double_rollback;
    ];
  ```

- [ ] **Step 8: Run e2e tests — verify they fail first (savepoint suite not present yet)**

  ```bash
  podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe 2>&1 | grep -E "FAIL|Error|savepoint" | head -20
  ```

  If the tests are already registered but the implementation isn't wired, they should fail. If the suite isn't registered yet, dune will report a compile error.

- [ ] **Step 9: Run e2e tests — verify savepoint tests pass**

  ```bash
  podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe 2>&1 | tail -10
  ```
  Expected: all tests pass including the new "savepoint" suite.

- [ ] **Step 10: Add SQLite comparison tests for SAVEPOINT in test_sqlite_compare.ml**

  In `test/test_sqlite_compare.ml`, add after the `phase20_json_mutation_cases` definition:

  ```ocaml
  (* ── Phase 21: SAVEPOINT ────────────────────────────────────────── *)

  let phase21_savepoint_cases = [
    { name = "rollback_undoes_insert";
      setup = [
        "CREATE TABLE sp1 (x INTEGER)";
        "BEGIN";
        "SAVEPOINT s";
        "INSERT INTO sp1 VALUES (1)";
        "ROLLBACK TO s";
        "COMMIT";
      ];
      query = "SELECT COUNT(*) FROM sp1";
      unordered = false };

    { name = "release_keeps_insert";
      setup = [
        "CREATE TABLE sp2 (x INTEGER)";
        "BEGIN";
        "SAVEPOINT s";
        "INSERT INTO sp2 VALUES (42)";
        "RELEASE s";
        "COMMIT";
      ];
      query = "SELECT x FROM sp2";
      unordered = false };

    { name = "partial_rollback";
      setup = [
        "CREATE TABLE sp3 (x INTEGER)";
        "BEGIN";
        "INSERT INTO sp3 VALUES (1)";
        "SAVEPOINT s";
        "INSERT INTO sp3 VALUES (2)";
        "ROLLBACK TO s";
        "COMMIT";
      ];
      query = "SELECT COUNT(*) FROM sp3";
      unordered = false };

    { name = "nested_savepoints";
      setup = [
        "CREATE TABLE sp4 (x INTEGER)";
        "BEGIN";
        "SAVEPOINT outer";
        "INSERT INTO sp4 VALUES (1)";
        "SAVEPOINT inner";
        "INSERT INTO sp4 VALUES (2)";
        "ROLLBACK TO inner";
        "RELEASE inner";
        "COMMIT";
      ];
      query = "SELECT COUNT(*) FROM sp4";
      unordered = false };
  ]
  ```

  In the `Alcotest.run` call at the bottom, add:
  ```ocaml
    "phase21_savepoint",       List.map make_test phase21_savepoint_cases;
  ```

- [ ] **Step 11: Run comparison tests**

  ```bash
  podman run --rm \
    -v $(pwd):/workspace:Z \
    -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
    -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
    -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
    -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
    -w /workspace sqlocaml-dev dune exec test/test_sqlite_compare.exe 2>&1 | tail -10
  ```
  Expected: all tests pass including `phase21_savepoint`.

- [ ] **Step 12: Commit**

  ```bash
  git add lib/db/db.ml test/test_e2e.ml test/test_sqlite_compare.ml
  git commit -m "feat(phase21): SAVEPOINT/RELEASE/ROLLBACK TO db execution + tests [#127]"
  ```

---

## Task 4: FK DELETE/UPDATE RESTRICT enforcement

**Files:**
- Modify: `lib/sql/exec.ml`
- Modify: `test/test_e2e.ml`
- Modify: `test/test_sqlite_compare.ml`

- [ ] **Step 1: Write failing e2e tests for FK DELETE violation**

  In `test/test_e2e.ml`, add after the savepoint tests:

  ```ocaml
  (* ── Phase 21: FK DELETE/UPDATE parent-side enforcement ─────────── *)

  let test_fk_delete_referenced_parent_fails () =
    Lwt_main.run (
      let* db = Db.open_in_memory () in
      let exec sql = Db.execute db sql >>= fun r ->
        (match r with Ok () -> () | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e);
        Lwt.return_unit
      in
      let* () = exec "CREATE TABLE fkp (id INTEGER PRIMARY KEY)" in
      let* () = exec "INSERT INTO fkp VALUES (1)" in
      let* () = exec "CREATE TABLE fkc (id INTEGER, pid INTEGER REFERENCES fkp(id))" in
      let* () = exec "INSERT INTO fkc VALUES (10, 1)" in
      let* result = Db.execute db "DELETE FROM fkp WHERE id = 1" in
      Alcotest.(check bool) "delete referenced parent fails" true (Result.is_error result);
      Lwt.return_unit)

  let test_fk_delete_unreferenced_parent_ok () =
    Lwt_main.run (
      let* db = Db.open_in_memory () in
      let exec sql = Db.execute db sql >>= fun r ->
        (match r with Ok () -> () | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e);
        Lwt.return_unit
      in
      let* () = exec "CREATE TABLE fkp2 (id INTEGER PRIMARY KEY)" in
      let* () = exec "INSERT INTO fkp2 VALUES (1)" in
      let* () = exec "INSERT INTO fkp2 VALUES (2)" in
      let* () = exec "CREATE TABLE fkc2 (id INTEGER, pid INTEGER REFERENCES fkp2(id))" in
      let* () = exec "INSERT INTO fkc2 VALUES (10, 2)" in
      (* id=1 has no child reference — delete must succeed *)
      let* () = exec "DELETE FROM fkp2 WHERE id = 1" in
      let* r = Db.query db "SELECT COUNT(*) FROM fkp2" in
      let rows = match r with Ok s -> Lwt_main.run (Lwt_stream.to_list s) | Error e -> Alcotest.failf "%a" Db.pp_error e in
      Alcotest.(check int) "1 parent row remains" 1
        (Int64.to_int (match rows with [[|Db.V_int n|]] -> n | _ -> -1L));
      Lwt.return_unit)

  let test_fk_delete_null_child_allows_parent_delete () =
    Lwt_main.run (
      let* db = Db.open_in_memory () in
      let exec sql = Db.execute db sql >>= fun r ->
        (match r with Ok () -> () | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e);
        Lwt.return_unit
      in
      let* () = exec "CREATE TABLE fkp3 (id INTEGER PRIMARY KEY)" in
      let* () = exec "INSERT INTO fkp3 VALUES (1)" in
      let* () = exec "CREATE TABLE fkc3 (id INTEGER, pid INTEGER REFERENCES fkp3(id))" in
      let* () = exec "INSERT INTO fkc3 VALUES (10, NULL)" in
      (* NULL child FK — parent delete must succeed *)
      let* () = exec "DELETE FROM fkp3 WHERE id = 1" in
      let* r = Db.query db "SELECT COUNT(*) FROM fkp3" in
      let rows = match r with Ok s -> Lwt_main.run (Lwt_stream.to_list s) | Error e -> Alcotest.failf "%a" Db.pp_error e in
      Alcotest.(check int) "parent deleted when child FK is null" 0
        (Int64.to_int (match rows with [[|Db.V_int n|]] -> n | _ -> -1L));
      Lwt.return_unit)

  let test_fk_update_referenced_col_fails () =
    Lwt_main.run (
      let* db = Db.open_in_memory () in
      let exec sql = Db.execute db sql >>= fun r ->
        (match r with Ok () -> () | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e);
        Lwt.return_unit
      in
      let* () = exec "CREATE TABLE fkpu (id INTEGER PRIMARY KEY)" in
      let* () = exec "INSERT INTO fkpu VALUES (1)" in
      let* () = exec "CREATE TABLE fkcu (pid INTEGER REFERENCES fkpu(id))" in
      let* () = exec "INSERT INTO fkcu VALUES (1)" in
      let* result = Db.execute db "UPDATE fkpu SET id = 99 WHERE id = 1" in
      Alcotest.(check bool) "update referenced parent col fails" true (Result.is_error result);
      Lwt.return_unit)

  let test_fk_update_unreferenced_col_ok () =
    Lwt_main.run (
      let* db = Db.open_in_memory () in
      let exec sql = Db.execute db sql >>= fun r ->
        (match r with Ok () -> () | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e);
        Lwt.return_unit
      in
      let* () = exec "CREATE TABLE fkpu2 (id INTEGER PRIMARY KEY)" in
      let* () = exec "INSERT INTO fkpu2 VALUES (1)" in
      let* () = exec "INSERT INTO fkpu2 VALUES (2)" in
      let* () = exec "CREATE TABLE fkcu2 (pid INTEGER REFERENCES fkpu2(id))" in
      let* () = exec "INSERT INTO fkcu2 VALUES (1)" in
      (* Updating id=2 which is not referenced — must succeed *)
      let* () = exec "UPDATE fkpu2 SET id = 99 WHERE id = 2" in
      let* r = Db.query db "SELECT COUNT(*) FROM fkpu2" in
      let rows = match r with Ok s -> Lwt_main.run (Lwt_stream.to_list s) | Error e -> Alcotest.failf "%a" Db.pp_error e in
      Alcotest.(check int) "unreferenced parent update ok" 2
        (Int64.to_int (match rows with [[|Db.V_int n|]] -> n | _ -> -1L));
      Lwt.return_unit)
  ```

  Register under a new suite:
  ```ocaml
    "fk_delete_update", [
      Alcotest.test_case "delete_ref_fails"         `Quick test_fk_delete_referenced_parent_fails;
      Alcotest.test_case "delete_no_ref_ok"         `Quick test_fk_delete_unreferenced_parent_ok;
      Alcotest.test_case "delete_null_child_ok"     `Quick test_fk_delete_null_child_allows_parent_delete;
      Alcotest.test_case "update_ref_col_fails"     `Quick test_fk_update_referenced_col_fails;
      Alcotest.test_case "update_unref_col_ok"      `Quick test_fk_update_unreferenced_col_ok;
    ];
  ```

- [ ] **Step 2: Run e2e to confirm tests fail (FK not yet enforced on delete/update)**

  ```bash
  podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe 2>&1 | grep -E "fk_delete_update|FAIL" | head -10
  ```
  Expected: `fk_delete_update` tests fail — `delete_ref_fails` and `update_ref_col_fails` pass a statement that should fail but doesn't yet.

- [ ] **Step 3: Add `build_child_refs` helper to exec.ml**

  In `lib/sql/exec.ml`, add the following helper function before `execute_update` (before line 1531). Place it after `execute_create_index` and before `execute_update`:

  ```ocaml
  (** Build the list of (child_table_meta, relevant_fk_constraints) pairs
      for tables that have FK constraints pointing to [parent_table_name]. *)
  let build_child_refs cat ~parent_table_name =
    let* all_tables = Cat.list_tables cat in
    Lwt.return (List.filter_map (fun (child_meta : Cat.table_meta) ->
      let fks = List.filter (fun (fk : Cat.fk_constraint) ->
        String.equal fk.fk_parent_table parent_table_name
      ) child_meta.Cat.fk_constraints in
      if fks = [] then None else Some (child_meta, fks)
    ) all_tables)

  (** Scan [child_meta] for any row where [child_col_idx] equals [parent_val].
      Opens and closes its own RO snapshot. *)
  let fk_child_has_ref store (child_meta : Cat.table_meta) ~child_col_idx ~(parent_val : Row.value) =
    let schema = child_meta.Cat.columns in
    let* ro_tx = S.ro_begin store in
    let* cur   = S.cursor_open ro_tx child_meta.Cat.tree_id in
    let _sr    = S.cursor_first cur in
    let found  = ref false in
    let rec scan () =
      if !found then ()
      else match S.cursor_next cur with
      | None -> ()
      | Some (_k, vbytes) ->
        let row = Row.decode schema vbytes in
        if compare_values row.(child_col_idx) parent_val = 0 then
          found := true
        else scan ()
    in
    scan ();
    S.cursor_close cur;
    let* () = S.ro_end ro_tx in
    Lwt.return !found

  (** Check that [row] can be removed from [table_meta] without violating
      any child FK RESTRICT constraint.  Raises [Failure] if blocked. *)
  let check_no_child_refs store cat (table_meta : Cat.table_meta) (row : Row.t) =
    let* child_refs = build_child_refs cat ~parent_table_name:table_meta.Cat.name in
    Lwt_list.iter_s (fun (child_meta, fks) ->
      Lwt_list.iter_s (fun (fk : Cat.fk_constraint) ->
        let parent_col_idx = find_col_idx_by_name table_meta.Cat.columns fk.fk_parent_col in
        let parent_val = row.(parent_col_idx) in
        (match parent_val with
         | Row.V_null -> Lwt.return_unit
         | _ ->
           let child_col_idx = find_col_idx_by_name child_meta.Cat.columns fk.fk_local_col in
           let* has_ref = fk_child_has_ref store child_meta ~child_col_idx ~parent_val in
           if has_ref then
             Lwt.fail_with (Printf.sprintf
               "FOREIGN KEY constraint failed: '%s.%s' is still referenced by '%s.%s'"
               table_meta.Cat.name fk.fk_parent_col
               child_meta.Cat.name fk.fk_local_col)
           else Lwt.return_unit)
      ) fks
    ) child_refs
  ```

- [ ] **Step 4: Add FK parent-delete check to `execute_delete`**

  In `lib/sql/exec.ml`, find `execute_delete` (around line 1644). The function signature is:

  ```ocaml
  let execute_delete ?(mode = Auto) ?(params = [||])
      ?(clock : (unit -> float) option = None)
      (store : S.t)
      ~(table_meta : Cat.table_meta)
      ~(where : Plan.expr option)
      ~(indexes : Cat.index_info list)
    : int Lwt.t =
  ```

  The function needs `cat` as a new parameter to look up child tables. Change the signature to:

  ```ocaml
  let execute_delete ?(mode = Auto) ?(params = [||])
      ?(clock : (unit -> float) option = None)
      (store : S.t)
      (cat : Cat.t)
      ~(table_meta : Cat.table_meta)
      ~(where : Plan.expr option)
      ~(indexes : Cat.index_info list)
    : int Lwt.t =
  ```

  After the `let matches = List.rev !buf in` and `if n = 0 then Lwt.return 0` check, add the FK check before `acquire_txn`. The current structure is:

  ```ocaml
  let matches = List.rev !buf in
  let n = List.length matches in
  if n = 0 then Lwt.return 0
  else begin
    let* (tx, owned) = acquire_txn store mode in
  ```

  Change to:

  ```ocaml
  let matches = List.rev !buf in
  let n = List.length matches in
  if n = 0 then Lwt.return 0
  else begin
    (* FK parent-side check: fail if any child row references a to-be-deleted row. *)
    let* () =
      Lwt_list.iter_s (fun (_rowid, row) ->
        check_no_child_refs store cat table_meta row
      ) matches
    in
    let* (tx, owned) = acquire_txn store mode in
  ```

- [ ] **Step 5: Fix callers of `execute_delete` (now needs `cat`)**

  Search for all calls to `execute_delete` in exec.ml:

  ```bash
  grep -n "execute_delete" /home/tej/projects/sqlite_ocaml_port/lib/sql/exec.ml
  ```

  There will be calls in `execute_with_count` and `execute`. Add `cat` as a positional argument after `store` at each call site. For example:

  ```ocaml
  (* Before: *)
  execute_delete ~mode ~params ~clock store ~table_meta ~where ~indexes
  (* After: *)
  execute_delete ~mode ~params ~clock store cat ~table_meta ~where ~indexes
  ```

  Also update the `execute_delete` call in the `returning`-producing variant if one exists.

- [ ] **Step 6: Add FK parent-update check to `execute_update`**

  In `lib/sql/exec.ml`, find `execute_update` (around line 1531). Same pattern — add `cat` parameter:

  ```ocaml
  let execute_update ?(mode = Auto) ?(params = [||])
      ?(clock : (unit -> float) option = None)
      (store : S.t)
      (cat : Cat.t)
      ~(table_meta : Cat.table_meta)
      ~(assignments : (int * Plan.expr) list)
      ~(where : Plan.expr option)
      ~(indexes : Cat.index_info list)
    : int Lwt.t =
  ```

  In the first validation pass (around line 1569), after the CHECK constraints block and before the UNIQUE constraints loop, add the FK update check. The first pass already iterates `matches`. At the start of that `Lwt_list.iter_s` block, add:

  ```ocaml
  (* FK parent-side check: fail if updating a referenced parent column. *)
  let* () =
    let* child_refs = build_child_refs cat ~parent_table_name:table_meta.Cat.name in
    if child_refs = [] then Lwt.return_unit
    else
      Lwt_list.iter_s (fun (_rowid, old_row) ->
        let new_row = Array.copy old_row in
        List.iter (fun (i, expr) ->
          new_row.(i) <- eval_expr clock params old_row expr
        ) assignments;
        Lwt_list.iter_s (fun (child_meta, fks) ->
          Lwt_list.iter_s (fun (fk : Cat.fk_constraint) ->
            let parent_col_idx = find_col_idx_by_name table_meta.Cat.columns fk.fk_parent_col in
            let old_val = old_row.(parent_col_idx) in
            let new_val = new_row.(parent_col_idx) in
            if compare_values old_val new_val = 0 then Lwt.return_unit
            else
              (match old_val with
               | Row.V_null -> Lwt.return_unit
               | _ ->
                 let child_col_idx = find_col_idx_by_name child_meta.Cat.columns fk.fk_local_col in
                 let* has_ref = fk_child_has_ref store child_meta ~child_col_idx ~parent_val:old_val in
                 if has_ref then
                   Lwt.fail_with (Printf.sprintf
                     "FOREIGN KEY constraint failed: update to '%s.%s' is referenced by '%s.%s'"
                     table_meta.Cat.name fk.fk_parent_col
                     child_meta.Cat.name fk.fk_local_col)
                 else Lwt.return_unit)
          ) fks
        ) child_refs
      ) matches
  in
  ```

  Insert this block immediately after:
  ```ocaml
  let matches = List.rev !buf in
  let n = List.length matches in
  if n = 0 then Lwt.return 0
  else begin
    let* (tx, owned) = acquire_txn store mode in
  ```

  Wait — for execute_update, the FK check must happen before `acquire_txn` as well, same as delete. Insert it between `let n = ...` and `let* (tx, owned) = ...`.

- [ ] **Step 7: Fix callers of `execute_update` (now needs `cat`)**

  Search for all calls to `execute_update` in exec.ml:

  ```bash
  grep -n "execute_update" /home/tej/projects/sqlite_ocaml_port/lib/sql/exec.ml
  ```

  Add `cat` as a positional argument after `store` at each call site.

- [ ] **Step 8: Build**

  ```bash
  podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
  ```
  Expected: clean build. Fix any type errors from signature changes.

- [ ] **Step 9: Run e2e tests — all should pass**

  ```bash
  podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe 2>&1 | tail -10
  ```
  Expected: all tests pass including `fk_delete_update` suite.

- [ ] **Step 10: Add SQLite comparison tests for FK delete/update (success cases only)**

  In `test/test_sqlite_compare.ml`, add after `phase21_savepoint_cases`:

  ```ocaml
  (* ── Phase 21: FK DELETE/UPDATE parent-side ─────────────────────── *)
  (* Only non-violation cases: SQLite FK enforcement is off by default and
     cannot be enabled per-statement in multi-process test setup.        *)

  let phase21_fk_cases = [
    { name = "delete_unreferenced_parent_ok";
      setup = [
        "CREATE TABLE fkpd (id INTEGER PRIMARY KEY)";
        "INSERT INTO fkpd VALUES (1)";
        "INSERT INTO fkpd VALUES (2)";
        "CREATE TABLE fkcd (pid INTEGER REFERENCES fkpd(id))";
        "INSERT INTO fkcd VALUES (2)";
        "DELETE FROM fkpd WHERE id = 1";
      ];
      query = "SELECT COUNT(*) FROM fkpd";
      unordered = false };

    { name = "update_unreferenced_parent_ok";
      setup = [
        "CREATE TABLE fkpu (id INTEGER PRIMARY KEY)";
        "INSERT INTO fkpu VALUES (1)";
        "INSERT INTO fkpu VALUES (2)";
        "CREATE TABLE fkcu (pid INTEGER REFERENCES fkpu(id))";
        "INSERT INTO fkcu VALUES (1)";
        "UPDATE fkpu SET id = 99 WHERE id = 2";
      ];
      query = "SELECT COUNT(*) FROM fkpu";
      unordered = false };

    { name = "delete_parent_null_child_ok";
      setup = [
        "CREATE TABLE fkpn (id INTEGER PRIMARY KEY)";
        "INSERT INTO fkpn VALUES (1)";
        "CREATE TABLE fkcn (pid INTEGER REFERENCES fkpn(id))";
        "INSERT INTO fkcn VALUES (NULL)";
        "DELETE FROM fkpn WHERE id = 1";
      ];
      query = "SELECT COUNT(*) FROM fkpn";
      unordered = false };
  ]
  ```

  In the `Alcotest.run` call:
  ```ocaml
    "phase21_fk",              List.map make_test phase21_fk_cases;
  ```

- [ ] **Step 11: Run comparison tests**

  ```bash
  podman run --rm \
    -v $(pwd):/workspace:Z \
    -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
    -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
    -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
    -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
    -w /workspace sqlocaml-dev dune exec test/test_sqlite_compare.exe 2>&1 | tail -10
  ```
  Expected: all tests pass including `phase21_fk`.

- [ ] **Step 12: Commit**

  ```bash
  git add lib/sql/exec.ml test/test_e2e.ml test/test_sqlite_compare.ml
  git commit -m "feat(phase21): FK DELETE/UPDATE RESTRICT enforcement + tests [#128]"
  ```

---

## Task 5: Final verification and cleanup

- [ ] **Step 1: Run full test suite**

  ```bash
  podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_e2e.exe 2>&1 | tail -5
  ```
  Expected: `X tests passing` (was 329, now 338+).

  ```bash
  podman run --rm \
    -v $(pwd):/workspace:Z \
    -v /usr/bin/sqlite3:/usr/bin/sqlite3:ro \
    -v /lib/x86_64-linux-gnu/libsqlite3.so.0:/lib/x86_64-linux-gnu/libsqlite3.so.0:ro \
    -v /lib/x86_64-linux-gnu/libreadline.so.8:/lib/x86_64-linux-gnu/libreadline.so.8:ro \
    -v /lib/x86_64-linux-gnu/libtinfo.so.6:/lib/x86_64-linux-gnu/libtinfo.so.6:ro \
    -w /workspace sqlocaml-dev dune exec test/test_sqlite_compare.exe 2>&1 | tail -5
  ```
  Expected: 326+ passing (was 319, 12 pre-existing failures unchanged).

- [ ] **Step 2: Close Forgejo issues**

  ```bash
  ~/.local/bin/forgejo issue edit tej/sqlite_ocaml_port 127 --state=closed
  ~/.local/bin/forgejo issue edit tej/sqlite_ocaml_port 128 --state=closed
  ```

---

## Self-Review

**Spec coverage:**
- ✅ #127 SAVEPOINT name: parser → sema → plan → db.ml calls `S.savepoint_begin`
- ✅ #127 RELEASE name: pops savepoint; auto-commits if txn was implicitly begun
- ✅ #127 ROLLBACK TO name: restores snapshot, keeps savepoint reusable
- ✅ #127 Implicit BEGIN when SAVEPOINT used outside explicit txn (`auto_began`)
- ✅ #128 DELETE of parent row with FK child → RESTRICT error
- ✅ #128 UPDATE of parent FK column when child references old value → RESTRICT error
- ✅ NULL FK child values don't block parent delete/update
- ✅ Unreferenced parent rows can be deleted/updated freely

**Placeholder scan:**
- No placeholders found — all code blocks are complete.

**Type consistency:**
- `S.savepoint_begin/release/rollback` take `rw txn` and `string`, return `unit Lwt.t` — consistent across store.ml, store.mli, db.ml.
- `execute_delete/update` now take `cat` as second positional arg after `store` — callers updated in exec.ml.
- New `Op_savepoint/release/rollback_to of string` are handled before `Op op` fallthrough in db.ml execute/execute_change_count.
