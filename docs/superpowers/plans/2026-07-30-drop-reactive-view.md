# DROP REACTIVE VIEW (#469) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `DROP REACTIVE VIEW [IF EXISTS] <name>` as the single supported way to retire a reactive view, and close the two ways the registry can silently desync from the catalog (`DROP TABLE _rv_<name>`, `DROP VIEW <name>`).

**Architecture:** The statement is threaded through the existing DDL pipeline (`parser.mly` → `Ast` → `Sema` → `Plan` → `Planner` → `db.ml`'s `execute_control_op`), exactly as `DROP VIEW` already is. The teardown itself lives in `db.ml`'s reactive-view driver section as `rv_drop`, reached through a new `rv_drop_hook` ref — the same forward-hook trick `rv_create_hook` / `rv_flush_hook` / `rv_load_hook` already use, because the driver re-enters `execute`/`query` which are defined far below the dispatch point. Two guards in `execute_control_op` reject the alternative removal spellings.

**Tech Stack:** OCaml 5.4, Lwt, Menhir (`lib/sql/parser.mly`), Alcotest + QCheck (`test/`), dune inside the `granary-dev` podman image.

## Global Constraints

- Spec: `docs/superpowers/specs/2026-07-30-drop-reactive-view-design.md`. Read it before starting.
- Branch `feat/469-drop-reactive-view`, worktree `.worktrees/469-drop-rv`. All work happens in the worktree. **Never commit to `main`.**
- **Never run `dune` on the host.** Every build/test command runs inside the container:
  `podman run --rm -v "/home/tej/projects/sqlite_ocaml_port/.worktrees/469-drop-rv:/workspace:z" -w /workspace granary-dev <cmd>`
- Formatting: `sh scripts/check-fmt.sh` (self-wraps podman). `--fix` rewrites. Read the final summary line, not just the exit code: `✓` = full CI parity, `◐` = dune files unverified.
- merlint must report 0 issues for touched files. Every public `val` in an `.mli` needs a `(** … *)` doc comment; max nesting depth 4.
- Error strings are part of the contract and are asserted by tests — copy them verbatim from this plan.
- Do not make reactive-view DDL transactional; do not guard `INSERT`/`UPDATE`/`ALTER` on `_rv_` tables (spec Non-goals).

---

### Task 1: Grammar and plumbing for `DROP REACTIVE VIEW`

Adds the statement end-to-end but with a *stub* driver, so the whole compile path is provably wired before the teardown logic exists. At the end of this task `DROP REACTIVE VIEW x` parses, plans, and succeeds as a no-op.

**Files:**
- Modify: `lib/sql/ast.ml` (near `S_drop_view`, ~line 408)
- Modify: `lib/sql/ast.mli` (near `S_drop_view`, ~line 424)
- Modify: `lib/sql/parser.mly` (`stmt` rule ~line 277; new rule after `drop_view` ~line 452)
- Modify: `lib/sql/sema.ml` (type ~line 340; `bind_internal` ~line 3870)
- Modify: `lib/sql/sema.mli` (type ~line 290)
- Modify: `lib/sql/plan.ml` (~line 335), `lib/sql/plan.mli` (~line 344)
- Modify: `lib/sql/planner.ml` (~line 1002)
- Modify: `lib/sql/exec.ml` (~line 5778, ~6976, ~9857, ~10147)
- Modify: `lib/db/db.ml` (hook ref near `rv_flush_hook` ~line 166; dispatch in `execute_control_op` ~line 1593)
- Test: `test/test_reactive_view_427.ml`

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces:
  - `Ast.S_drop_reactive_view of { name : string; if_exists : bool }`
  - `Sema.BS_drop_reactive_view of { name : string; if_exists : bool }`
  - `Plan.Op_drop_reactive_view of { name : string; if_exists : bool }`
  - `Db.rv_drop_hook : (t -> name:string -> if_exists:bool -> (unit, error) result Lwt.t) ref` (module-internal, not exported in `db.mli`)

- [ ] **Step 1: Write the failing test**

Add to `test/test_reactive_view_427.ml`, after `test_register_reports_unknown_view`:

```ocaml
(* #469: the statement parses, plans, and reaches the driver.  Task 1 only
   proves the pipeline is wired; the teardown assertions live in Task 2. *)
let test_drop_statement_parses () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    exec db "DROP REACTIVE VIEW cnt";
    (* REACTIVE stays usable as an ordinary identifier *)
    exec db "CREATE TABLE reactive (reactive TEXT)";
    exec db "DROP TABLE reactive")
;;
```

Register it in the `"enumeration"` group's list in the `Alcotest.run` suite at the bottom of the file, as the last entry:

```ocaml
        ; Alcotest.test_case "drop statement parses" `Quick test_drop_statement_parses
```

- [ ] **Step 2: Run the test to verify it fails**

Run:
```sh
podman run --rm -v "/home/tej/projects/sqlite_ocaml_port/.worktrees/469-drop-rv:/workspace:z" \
  -w /workspace granary-dev dune test test/test_reactive_view_427.exe
```
Expected: FAIL — `exec "DROP REACTIVE VIEW cnt"` reports a parse error.

- [ ] **Step 3: Add the AST constructor**

In `lib/sql/ast.ml`, immediately after the `S_drop_view` constructor:

```ocaml
  | S_drop_reactive_view of
      { name : string
      ; if_exists : bool
      }
```

In `lib/sql/ast.mli`, the same constructor in the same position, with a doc comment:

```ocaml
  | S_drop_reactive_view of
      { name : string
      ; if_exists : bool
      }
  (** [DROP REACTIVE VIEW [IF EXISTS] name] (#469).  Retires a reactive view:
        deregisters it, drops its [_rv_<name>] materialisation, and removes its
        persisted definition.  Distinct from {!S_drop_view}, which only knows
        about plain SQL views. *)
```

- [ ] **Step 4: Add the grammar rule**

In `lib/sql/parser.mly`, add to the `stmt` rule immediately after the `drop_view` alternative:

```
  | s = drop_reactive_view { s }
```

and add the rule immediately after the `drop_view:` rule:

```
drop_reactive_view:
  | DROP REACTIVE VIEW name = any_ident
    { Ast.S_drop_reactive_view { name; if_exists = false } }
  | DROP REACTIVE VIEW IF EXISTS name = any_ident
    { Ast.S_drop_reactive_view { name; if_exists = true } }
```

No token or `any_ident` change is needed: `REACTIVE` is already a token (`lib/sql/lexer.mll:188`) and already listed among the non-reserved identifiers in `parser.mly` (~line 206 and ~line 419).

- [ ] **Step 5: Thread it through sema**

In `lib/sql/sema.ml`, after `BS_drop_view`:

```ocaml
  | BS_drop_reactive_view of
      { name : string
      ; if_exists : bool
      }
```

and in `bind_internal`, immediately after the `Ast.S_drop_view` case:

```ocaml
  | Ast.S_drop_reactive_view { name; if_exists } ->
    Lwt.return (Ok (BS_drop_reactive_view { name; if_exists }))
```

Note this keeps `if_exists`, unlike `BS_drop_view` which discards it — the reactive driver needs it to decide between `Ok ()` and an error.

Mirror the constructor in `lib/sql/sema.mli` after `BS_drop_view`.

- [ ] **Step 6: Thread it through plan and planner**

In `lib/sql/plan.ml` and `lib/sql/plan.mli`, after `Op_drop_view`:

```ocaml
  | Op_drop_reactive_view of
      { name : string
      ; if_exists : bool
      }
```

In `lib/sql/planner.ml`, after the `Sema.BS_drop_view` case:

```ocaml
  | Sema.BS_drop_reactive_view { name; if_exists } ->
    Plan.Op_drop_reactive_view { name; if_exists }
```

- [ ] **Step 7: Add the four `exec.ml` match arms**

`lib/sql/exec.ml` matches over every plan op in four places. Add the new op beside `Op_drop_view` in each:

1. `op_name` (~line 5778):
```ocaml
  | Plan.Op_drop_reactive_view { name; _ } -> "DropReactiveView(" ^ name ^ ")"
```
2. The DDL-returns-0-rows group (~line 6976) — add `| Plan.Op_drop_reactive_view _` after `| Plan.Op_drop_view _`.
3. The group at ~line 9857 — add `| Plan.Op_drop_reactive_view _` after `| Plan.Op_drop_view _`.
4. The group at ~line 10147 — add `| Plan.Op_drop_reactive_view _` after `| Plan.Op_drop_view _`.

If the compiler reports further non-exhaustive matches, add the op alongside `Op_drop_view` there too — `Op_drop_view` is the correct template in every case.

- [ ] **Step 8: Add the hook ref and the dispatch arm**

In `lib/db/db.ml`, immediately after the `rv_flush_hook` definition (~line 168):

```ocaml
(* #469: forward hook for [DROP REACTIVE VIEW]; populated at the bottom of the
   file alongside the other reactive-view hooks. *)
let rv_drop_hook
  : (t -> name:string -> if_exists:bool -> (unit, error) result Lwt.t) ref
  =
  ref (fun _ ~name:_ ~if_exists:_ -> Lwt.return (Ok ()))
;;
```

In `execute_control_op`, immediately after the `Op_create_reactive_view` arm (~line 1592):

```ocaml
  | Sql.Plan.Op_drop_reactive_view { name; if_exists } ->
    (* #469: deregister, drop [_rv_<name>], and forget the persisted
       definition.  Like CREATE, this is immediate rather than staged. *)
    Some (!rv_drop_hook top ~name ~if_exists)
```

- [ ] **Step 9: Run the test to verify it passes**

Run:
```sh
podman run --rm -v "/home/tej/projects/sqlite_ocaml_port/.worktrees/469-drop-rv:/workspace:z" \
  -w /workspace granary-dev dune test test/test_reactive_view_427.exe
```
Expected: PASS, including the new `drop statement parses` case.

Then confirm no grammar conflicts were introduced:
```sh
podman run --rm -v "/home/tej/projects/sqlite_ocaml_port/.worktrees/469-drop-rv:/workspace:z" \
  -w /workspace granary-dev dune build 2>&1 | grep -i "conflict" || echo "no conflicts"
```
Expected: `no conflicts`.

- [ ] **Step 10: Commit**

```bash
git add lib/sql/ast.ml lib/sql/ast.mli lib/sql/parser.mly lib/sql/sema.ml lib/sql/sema.mli \
        lib/sql/plan.ml lib/sql/plan.mli lib/sql/planner.ml lib/sql/exec.ml lib/db/db.ml \
        test/test_reactive_view_427.ml
git commit -m "feat(#469): parse and plan DROP REACTIVE VIEW [IF EXISTS]"
```

---

### Task 2: `rv_drop` — the teardown

**Files:**
- Modify: `lib/db/db.ml` (new `rv_drop` in the reactive-view driver section, after `rv_create` ~line 3190; hook wiring in the trailing `let () =` ~line 3290)
- Test: `test/test_reactive_view_427.ml`

**Interfaces:**
- Consumes: `Db.rv_drop_hook` and `Plan.Op_drop_reactive_view` from Task 1.
- Produces: `rv_drop : t -> name:string -> if_exists:bool -> (unit, error) result Lwt.t` (module-internal), installed into `rv_drop_hook`.

- [ ] **Step 1: Write the failing tests**

Replace the Task 1 placeholder test `test_drop_statement_parses` with the following block (keep the identifier-usable assertions by folding them into the first test):

```ocaml
(* #469: DROP REACTIVE VIEW is the supported removal path. *)
let test_drop_removes_view () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check bool) "live before the drop" true (Db.is_reactive_view db "cnt");
    exec db "DROP REACTIVE VIEW cnt";
    Alcotest.(check bool) "not live after" false (Db.is_reactive_view db "cnt");
    Alcotest.(check (list string)) "not listed after" [] (Db.reactive_view_names db);
    (* the materialisation is gone: selecting from it is now an error *)
    Alcotest.(check bool)
      "_rv_cnt no longer exists"
      true
      (Option.is_some (exec_err db "SELECT * FROM _rv_cnt"));
    (* a full-refresh view (MIN is not delta-maintainable) drops the same way *)
    exec db "CREATE REACTIVE VIEW lo AS SELECT grp, MIN(amt) FROM t GROUP BY grp";
    Alcotest.(check (list string)) "the full-refresh view is live" [ "lo" ] (Db.reactive_view_names db);
    exec db "DROP REACTIVE VIEW lo";
    Alcotest.(check (list string)) "and drops too" [] (Db.reactive_view_names db);
    (* REACTIVE remains usable as an ordinary identifier *)
    exec db "CREATE TABLE reactive (reactive TEXT)";
    exec db "DROP TABLE reactive")
;;

let test_drop_stops_callbacks () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    let fired = ref 0 in
    (match
       Db.register_view_callback db ~view_name:"cnt" (fun _ ->
         incr fired;
         Lwt.return_unit)
     with
     | Ok () -> ()
     | Error (`Unknown_view n) -> Alcotest.failf "expected %S to be a live view" n);
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check int) "callback fires while live" 1 !fired;
    exec db "DROP REACTIVE VIEW cnt";
    exec db "INSERT INTO t VALUES (2, 'b', 20)";
    Alcotest.(check int) "callback is silent after the drop" 1 !fired;
    Alcotest.check
      register_result
      "re-registering reports the view as unknown"
      (Error (`Unknown_view "cnt"))
      (Db.register_view_callback db ~view_name:"cnt" (fun _ -> Lwt.return_unit)))
;;

let test_drop_if_exists () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    (match exec_err db "DROP REACTIVE VIEW nosuch" with
     | None -> Alcotest.fail "dropping a missing reactive view must error"
     | Some msg ->
       Alcotest.(check bool) "error names the view" true (contains ~needle:"nosuch" msg));
    exec db "DROP REACTIVE VIEW IF EXISTS nosuch";
    (* IF EXISTS on a live view still drops it *)
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    exec db "DROP REACTIVE VIEW IF EXISTS cnt";
    Alcotest.(check (list string)) "dropped" [] (Db.reactive_view_names db))
;;

(* A dropped view must not come back on reopen, and must not leave the delta
   engine or the old materialisation behind for a same-named successor. *)
let test_drop_is_durable_and_recreatable () =
  let path = tmp_path () in
  let cleanup () =
    try Sys.remove path with
    | _ -> ()
  in
  cleanup ();
  Fun.protect ~finally:cleanup (fun () ->
    let db = unwrap (run (Granary_unix.open_file ~path ())) in
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW v AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    exec db "INSERT INTO t VALUES (2, 'a', 5)";
    exec db "DROP REACTIVE VIEW v";
    run (Db.close db);
    let db = unwrap (run (Granary_unix.open_file ~path ())) in
    Fun.protect
      ~finally:(fun () ->
        try run (Db.close db) with
        | _ -> ())
      (fun () ->
         Alcotest.(check (list string))
           "the drop survived the reopen"
           []
           (Db.reactive_view_names db);
         (* re-create the same name with a *different* aggregate: no stale
            registry, catalog row, or _rv_ table may leak through *)
         exec db "CREATE REACTIVE VIEW v AS SELECT grp, SUM(amt) FROM t GROUP BY grp";
         Alcotest.(check (list (pair string int)))
           "re-created view materialises the new query"
           [ "a", 15 ]
           (mv db "v");
         exec db "INSERT INTO t VALUES (3, 'a', 1)";
         Alcotest.(check (list (pair string int)))
           "and is maintained"
           [ "a", 16 ]
           (mv db "v")))
;;
```

Register all four in the suite. Add a new group after `"enumeration"`:

```ocaml
    ; ( "drop"
      , [ Alcotest.test_case "drop removes the view" `Quick test_drop_removes_view
        ; Alcotest.test_case "drop stops callbacks" `Quick test_drop_stops_callbacks
        ; Alcotest.test_case "if exists" `Quick test_drop_if_exists
        ; Alcotest.test_case
            "drop is durable and re-creatable"
            `Quick
            test_drop_is_durable_and_recreatable
        ] )
```

and remove the `drop statement parses` entry added in Task 1.

These tests use a `contains` helper. Define it once, next to the existing `exec_err`
helper near the top of the file (Tasks 3 and 4 use it too):

```ocaml
(* #469: substring search — error strings are part of the contract. *)
let contains ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  let rec go i = i + nl <= hl && (String.sub haystack i nl = needle || go (i + 1)) in
  go 0
;;
```

- [ ] **Step 2: Run the tests to verify they fail**

Run:
```sh
podman run --rm -v "/home/tej/projects/sqlite_ocaml_port/.worktrees/469-drop-rv:/workspace:z" \
  -w /workspace granary-dev dune test test/test_reactive_view_427.exe
```
Expected: all four new cases FAIL — the stub hook returns `Ok ()` without doing anything, so `is_reactive_view` still answers `true` and `DROP REACTIVE VIEW nosuch` wrongly succeeds.

- [ ] **Step 3: Implement `rv_drop`**

In `lib/db/db.ml`, insert after `rv_create` ends (before the `rv_load` comment block, ~line 3190):

```ocaml
(* #469: retire a reactive view.  Registry first, so a failure part-way leaves
   the view *gone* rather than registered-but-unmaintainable; [rv_load] on the
   next open then finds no catalog row and does not resurrect it.  Runs with
   [rv_refreshing] set so the internal [DROP TABLE] is not rejected by the
   internal-table guard in [execute_control_op]. *)
let rv_drop top ~name ~if_exists =
  if not (Hashtbl.mem top.reactive_views name)
  then
    if if_exists
    then Lwt.return (Ok ())
    else Lwt.return (Error (Runtime (Printf.sprintf "no such reactive view: %s" name)))
  else (
    Hashtbl.remove top.reactive_views name;
    (* Forget pending base-table deltas no remaining view depends on. *)
    let stale =
      Hashtbl.fold
        (fun tbl _ acc -> if rv_is_base_table top tbl then acc else tbl :: acc)
        top.rv_pending
        []
    in
    List.iter (Hashtbl.remove top.rv_pending) stale;
    top.rv_refreshing <- true;
    Lwt.finalize
      (fun () ->
         let* dr =
           execute
             top
             (Printf.sprintf
                "DROP TABLE IF EXISTS %s"
                (rv_quote (rv_table_name name)))
         in
         match dr with
         | Error _ as e -> Lwt.return e
         | Ok () ->
           let* () = Cat.remove_reactive_view top.store ~name in
           Lwt.return (Ok ()))
      (fun () ->
         top.rv_refreshing <- false;
         Lwt.return_unit))
;;
```

`DROP TABLE IF EXISTS` (not a bare `DROP TABLE`) is deliberate: a view whose stored SQL never produced a materialisation, or whose `_rv_` table a previous partial teardown already removed, must still drop cleanly.

- [ ] **Step 4: Wire the hook**

In the trailing `let () =` block at the bottom of `lib/db/db.ml`:

```ocaml
let () =
  rv_create_hook := rv_create;
  rv_drop_hook := rv_drop;
  (rv_flush_hook := fun top -> rv_flush top);
  rv_load_hook := rv_load
;;
```

- [ ] **Step 5: Run the tests to verify they pass**

Run:
```sh
podman run --rm -v "/home/tej/projects/sqlite_ocaml_port/.worktrees/469-drop-rv:/workspace:z" \
  -w /workspace granary-dev dune test test/test_reactive_view_427.exe
```
Expected: PASS, all cases including the pre-existing #427/#437 ones.

- [ ] **Step 6: Commit**

```bash
git add lib/db/db.ml test/test_reactive_view_427.ml
git commit -m "feat(#469): implement rv_drop — deregister, drop _rv_ table, forget definition"
```

---

### Task 3: Reject `DROP TABLE _rv_<name>` for a live reactive view

**Files:**
- Modify: `lib/db/db.ml` (helper before `execute_control_op` ~line 1498; new guard arm in `execute_control_op`)
- Test: `test/test_reactive_view_427.ml`

**Interfaces:**
- Consumes: `rv_drop` (Task 2) — the guard's error message points users at `DROP REACTIVE VIEW`.
- Produces: `rv_owned_table : t -> string -> bool` (module-internal).

- [ ] **Step 1: Write the failing test**

Add after `test_drop_is_durable_and_recreatable`:

```ocaml
(* #469: the materialisation is internal — dropping it directly would leave the
   registry claiming a view with no _rv_ table.  A user table that merely starts
   with [_rv_] and has no registry entry stays droppable. *)
let test_rv_table_drop_rejected () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    (match exec_err db "DROP TABLE _rv_cnt" with
     | None -> Alcotest.fail "dropping an internal _rv_ table must be rejected"
     | Some msg ->
       Alcotest.(check bool)
         "error points at DROP REACTIVE VIEW"
         true
         (contains ~needle:"DROP REACTIVE VIEW cnt" msg));
    Alcotest.(check bool) "the view is untouched" true (Db.is_reactive_view db "cnt");
    (* the view still works *)
    exec db "INSERT INTO t VALUES (1, 'a', 10)";
    Alcotest.(check (list (pair string int))) "still maintained" [ "a", 1 ] (mv db "cnt");
    (* a plain user table with an _rv_ prefix and no registry entry still drops *)
    exec db "CREATE TABLE _rv_ghost (a TEXT)";
    exec db "DROP TABLE _rv_ghost";
    (* and after a proper drop, the name is droppable as an ordinary table *)
    exec db "DROP REACTIVE VIEW cnt";
    exec db "CREATE TABLE _rv_cnt (a TEXT)";
    exec db "DROP TABLE _rv_cnt")
;;
```

Add to the `"drop"` group:

```ocaml
        ; Alcotest.test_case
            "_rv_ table drop rejected"
            `Quick
            test_rv_table_drop_rejected
```

(If Task 2 used `Astring` rather than the local `contains`, use the same helper here.)

- [ ] **Step 2: Run the test to verify it fails**

Run:
```sh
podman run --rm -v "/home/tej/projects/sqlite_ocaml_port/.worktrees/469-drop-rv:/workspace:z" \
  -w /workspace granary-dev dune test test/test_reactive_view_427.exe
```
Expected: FAIL at the first assertion — `DROP TABLE _rv_cnt` currently succeeds.

- [ ] **Step 3: Add the helper**

In `lib/db/db.ml`, immediately before the `execute_control_op` comment block (~line 1498):

```ocaml
(* #469: is [tbl] the materialisation [_rv_<name>] of a *registered* reactive
   view?  A user table that merely starts with [_rv_] is not — the registry, not
   the naming convention, is the authority (same rule as {!is_reactive_view}). *)
let rv_owned_table top tbl =
  String.length tbl > 4
  && String.sub tbl 0 4 = "_rv_"
  && Hashtbl.mem top.reactive_views (String.sub tbl 4 (String.length tbl - 4))
;;
```

- [ ] **Step 4: Add the guard arm**

In `execute_control_op`, immediately before the `Op_create_view` arm (~line 1573):

```ocaml
  | Sql.Plan.Op_drop_table { table_meta; _ }
    when (not top.rv_refreshing) && rv_owned_table top table_meta.Cat.name ->
    (* #469: the driver's own teardown/re-type drops run with [rv_refreshing]
       set and pass straight through. *)
    let tbl = table_meta.Cat.name in
    let view = String.sub tbl 4 (String.length tbl - 4) in
    Some
      (Lwt.return
         (Error
            (Runtime
               (Printf.sprintf
                  "table '%s' is an internal reactive-view materialisation; use DROP \
                   REACTIVE VIEW %s"
                  tbl
                  view))))
```

The `when` guard is what keeps the fallthrough correct: when it does not hold, matching continues to the catch-all `| _ -> None` and the drop is routed to `Exec` unchanged.

- [ ] **Step 5: Run the test to verify it passes**

Run:
```sh
podman run --rm -v "/home/tej/projects/sqlite_ocaml_port/.worktrees/469-drop-rv:/workspace:z" \
  -w /workspace granary-dev dune test test/test_reactive_view_427.exe
```
Expected: PASS. In particular the pre-existing `full refresh` cases must still pass — `rv_refresh_one` drops and re-creates `_rv_<name>` when re-typing a provisional schema, and that path relies on the `rv_refreshing` exemption.

- [ ] **Step 6: Commit**

```bash
git add lib/db/db.ml test/test_reactive_view_427.ml
git commit -m "fix(#469): reject DROP TABLE on a live reactive view's _rv_ materialisation"
```

---

### Task 4: `DROP VIEW` on a reactive view errors with a hint

**Files:**
- Modify: `lib/db/db.ml` (`execute_control_op`, new arm before the existing `Op_drop_view` arm ~line 1593)
- Test: `test/test_reactive_view_427.ml`

**Interfaces:**
- Consumes: `top.reactive_views` (existing), the `contains` helper from Task 2.
- Produces: nothing new.

- [ ] **Step 1: Write the failing test**

Add after `test_rv_table_drop_rejected`:

```ocaml
(* #469: DROP VIEW used to silently succeed as a no-op on a reactive view —
   reactive views live in their own registry, not in [t.views]. *)
let test_drop_view_on_reactive_view_errors () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    (match exec_err db "DROP VIEW cnt" with
     | None -> Alcotest.fail "DROP VIEW on a reactive view must not silently succeed"
     | Some msg ->
       Alcotest.(check bool)
         "error points at DROP REACTIVE VIEW"
         true
         (contains ~needle:"DROP REACTIVE VIEW cnt" msg));
    Alcotest.(check bool) "the view is untouched" true (Db.is_reactive_view db "cnt");
    (* IF EXISTS does not excuse it: the object exists, the statement is wrong *)
    Alcotest.(check bool)
      "IF EXISTS still errors"
      true
      (Option.is_some (exec_err db "DROP VIEW IF EXISTS cnt"));
    (* plain views are unaffected *)
    exec db "CREATE VIEW plain AS SELECT grp FROM t";
    exec db "DROP VIEW plain")
;;
```

Add to the `"drop"` group:

```ocaml
        ; Alcotest.test_case
            "DROP VIEW on a reactive view errors"
            `Quick
            test_drop_view_on_reactive_view_errors
```

- [ ] **Step 2: Run the test to verify it fails**

Run:
```sh
podman run --rm -v "/home/tej/projects/sqlite_ocaml_port/.worktrees/469-drop-rv:/workspace:z" \
  -w /workspace granary-dev dune test test/test_reactive_view_427.exe
```
Expected: FAIL — `DROP VIEW cnt` returns `Ok ()` today.

- [ ] **Step 3: Add the guard arm**

In `execute_control_op`, immediately *before* the existing `| Sql.Plan.Op_drop_view { name } ->` arm:

```ocaml
  | Sql.Plan.Op_drop_view { name } when Hashtbl.mem top.reactive_views name ->
    (* #469: DROP VIEW only knows about [t.views]; on a reactive view it would
       succeed while removing nothing.  Fires even under IF EXISTS — the object
       exists, the statement is the wrong one. *)
    Some
      (Lwt.return
         (Error
            (Runtime
               (Printf.sprintf
                  "'%s' is a reactive view; use DROP REACTIVE VIEW %s"
                  name
                  name))))
```

- [ ] **Step 4: Run the test to verify it passes**

Run:
```sh
podman run --rm -v "/home/tej/projects/sqlite_ocaml_port/.worktrees/469-drop-rv:/workspace:z" \
  -w /workspace granary-dev dune test test/test_reactive_view_427.exe
```
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add lib/db/db.ml test/test_reactive_view_427.ml
git commit -m "fix(#469): DROP VIEW on a reactive view errors instead of silently no-opping"
```

---

### Task 5: Document the semantics, then verify the whole tree

**Files:**
- Modify: `lib/db/db.mli` (doc comments on `reactive_view_names` ~line 341 and `register_view_callback` ~line 330)
- Test: `test/test_reactive_view_427.ml` (one test pinning the documented rollback behaviour)

**Interfaces:**
- Consumes: everything from Tasks 1–4.
- Produces: no new API — `DROP REACTIVE VIEW` is SQL-level only.

- [ ] **Step 1: Write the failing test**

Add after `test_drop_view_on_reactive_view_errors`:

```ocaml
(* #469: reactive-view DDL is immediate, not staged (symmetric with CREATE
   REACTIVE VIEW).  Pinned here so the asymmetry with DROP VIEW is deliberate
   rather than accidental. *)
let test_drop_is_not_rolled_back () =
  with_db (fun db ->
    exec db "CREATE TABLE t (id INTEGER PRIMARY KEY, grp TEXT, amt INTEGER)";
    exec db "CREATE REACTIVE VIEW cnt AS SELECT grp, COUNT(*) FROM t GROUP BY grp";
    exec db "BEGIN";
    exec db "DROP REACTIVE VIEW cnt";
    exec db "ROLLBACK";
    Alcotest.(check (list string))
      "ROLLBACK does not resurrect a dropped reactive view"
      []
      (Db.reactive_view_names db))
;;
```

Add to the `"drop"` group:

```ocaml
        ; Alcotest.test_case
            "drop is not rolled back"
            `Quick
            test_drop_is_not_rolled_back
```

- [ ] **Step 2: Run the test**

Run:
```sh
podman run --rm -v "/home/tej/projects/sqlite_ocaml_port/.worktrees/469-drop-rv:/workspace:z" \
  -w /workspace granary-dev dune test test/test_reactive_view_427.exe
```
Expected: PASS on the registry assertion. If instead it fails because `DROP TABLE` inside an explicit transaction deadlocks (#269), drop the `BEGIN`/`ROLLBACK` framing and replace the test body with a comment recording that reactive-view DDL is unsupported inside an explicit transaction, plus an `exec_err` assertion of whatever error is actually produced. Record which of the two outcomes you observed in the commit message — it is the answer to a real open question, not a formality.

- [ ] **Step 3: Document the semantics in `db.mli`**

Append to the doc comment on `reactive_view_names`:

```
    #469: a view leaves this list when [DROP REACTIVE VIEW name] retires it —
    the only supported removal path.  [DROP TABLE _rv_<name>] and
    [DROP VIEW <name>] are both rejected, so the registry cannot drift from the
    catalog.  Reactive-view DDL is immediate rather than transactional: a
    [ROLLBACK] after a drop does not bring the view back (symmetric with
    [CREATE REACTIVE VIEW]).
```

Append to the doc comment on `register_view_callback`:

```
    Callbacks are held by the registry entry, so [DROP REACTIVE VIEW] discards
    them: a callback registered against a dropped view stops firing, and
    re-registering reports [`Unknown_view].
```

- [ ] **Step 4: Run the full suite**

Run:
```sh
podman run --rm -v "/home/tej/projects/sqlite_ocaml_port/.worktrees/469-drop-rv:/workspace:z" \
  -w /workspace granary-dev dune test
```
Expected: PASS. Note the pre-existing failure tracked as #468 may still appear — compare against `main` before blaming this branch.

- [ ] **Step 5: Format and lint**

Run:
```sh
cd /home/tej/projects/sqlite_ocaml_port/.worktrees/469-drop-rv
sh scripts/check-fmt.sh --fix
sh scripts/check-fmt.sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace granary-dev merlint
```
Expected: `check-fmt.sh` ends with `✓ … parity with CI's @fmt gate` (no `dune`/`dune-project` file is touched by this plan, so `◐` should not appear — if it does, run the real gate in the main checkout as CLAUDE.md describes). `merlint`: 0 issues for the touched files.

- [ ] **Step 6: Commit**

```bash
git add lib/db/db.mli test/test_reactive_view_427.ml
git commit -m "docs(#469): document the single removal path and non-transactional drop semantics"
```

- [ ] **Step 7: Push and open the PR**

```bash
git push origin feat/469-drop-reactive-view
~/.local/bin/forgejo pr create tej/granary \
  --title="feat(#469): DROP REACTIVE VIEW + single removal path for reactive views" \
  --head=feat/469-drop-reactive-view \
  --base=main \
  --body="$(cat <<'EOF'
## Summary
- `DROP REACTIVE VIEW [IF EXISTS] <name>`: deregisters the view, drops its `_rv_<name>` materialisation, discards its delta-engine state and callbacks, and removes the persisted definition — the first caller of `Catalog.remove_reactive_view`.
- `DROP TABLE _rv_<name>` on a live reactive view is rejected as an internal table, so the registry can no longer drift from the catalog.
- `DROP VIEW <name>` on a reactive view now errors with a hint instead of silently succeeding as a no-op.
- Reactive-view DDL stays immediate rather than staged, symmetric with `CREATE REACTIVE VIEW`; documented in `db.mli` and pinned by a test.

## Test plan
- [ ] dune test passes
- [ ] new tests: drop removes the view; callbacks stop firing; `IF EXISTS`; durable across reopen and re-creatable with a different query; `_rv_` drop rejected while a plain `_rv_`-prefixed user table still drops; `DROP VIEW` errors; drop is not rolled back

Closes #469
EOF
)"
```

---

## Self-Review

**Spec coverage:**
- §1 grammar/plumbing → Task 1
- §2 `rv_drop` teardown (all five steps, registry-first ordering) → Task 2
- §3 internal-table guard, `rv_refreshing` exemption, user `_rv_` table still droppable → Task 3
- §4 `DROP VIEW` hint including under `IF EXISTS` → Task 4
- §5 transactionality + `db.mli` documentation → Task 5
- Testing items 1–9 → Task 2 (1–5, 8), Task 3 (6), Task 4 (7), Task 5 (9)

**Type consistency:** `if_exists : bool` is carried unchanged from `Ast.S_drop_reactive_view` through `Sema.BS_drop_reactive_view` and `Plan.Op_drop_reactive_view` into `rv_drop ~name ~if_exists`; the hook ref's type in Task 1 Step 8 matches `rv_drop`'s definition in Task 2 Step 3. `rv_owned_table` is used only in Task 3, where it is defined.
