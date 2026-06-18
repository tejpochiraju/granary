# #412 — as-of against ATTACHed databases — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let `Db.query_as_of` time-travel against an ATTACHed database by opening the historical snapshot on the routed handle's store, with attached stores inheriting the top handle's history setting and a per-schema retention API.

**Architecture:** The executor already runs against the routed handle (`t.store`/`t.catalog`/`t.clock`); only the as-of snapshot was wrongly taken from `top.store`. Reorder `query_as_of` to route first and open the snapshot on `t.store`, drop the attached-DB guard, thread `?as_of_history` through the file provider so ATTACH inherits the top's setting, and add `?schema` to the retention API. One store per query — cross-schema joins-at-a-timestamp stay out of scope.

**Tech Stack:** OCaml 5.x, Lwt, dune, Alcotest + QCheck, podman dev container (`sqlocaml-dev`). Spec: `docs/superpowers/specs/2026-06-18-412-as-of-attached-design.md`.

**Conventions for every build/test/format command below** (run from the worktree root `/home/tej/projects/sqlite_ocaml_port/.worktrees/412-as-of-attached`, abbreviated `$W`):

```sh
# build
podman run --rm -v "$W:/workspace:z" -w /workspace sqlocaml-dev dune build
# run one test exe
podman run --rm -v "$W:/workspace:z" -w /workspace sqlocaml-dev dune test test/test_foo.exe
# whole suite
podman run --rm -v "$W:/workspace:z" -w /workspace sqlocaml-dev dune test
```

All commits use the worktree; never push to `main`. End commit messages with:
`Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>`

---

## File Structure

- **Modify** `lib/store/store.mli` / `lib/store/store.ml` — add `history_enabled : t -> bool`.
- **Modify** `lib/db/db.ml` — provider type `?as_of_history`; ATTACH inheritance; `store_for_schema` helper; `?schema` on the four `history_*` functions; reorder `query_as_of`.
- **Modify** `lib/db/db.mli` — `?schema` signatures + doc comments; rewrite `query_as_of` doc.
- **Modify** `lib/unix/sqlocaml_unix.ml` — provider passes `?as_of_history` to `Store.open_file`.
- **Create** `test/test_attach_as_of.ml` — integration + QCheck tests.
- **Modify** `test/dune` — register the new test exe.
- **Modify** `test/test_store_as_of.ml` — unit test for `history_enabled`.

---

## Task 1: `S.history_enabled` store accessor

**Files:**
- Modify: `lib/store/store.mli` (after the `history_log` val, ~line 191)
- Modify: `lib/store/store.ml` (after `history_log`, ~line 1330)
- Test: `test/test_store_as_of.ml`

- [ ] **Step 1: Write the failing test**

Append to `test/test_store_as_of.ml` (add the case to its Alcotest list too — find the `Alcotest.run`/test-list near the bottom and add `Alcotest.test_case "history_enabled" \`Quick test_history_enabled;`). Use the file's existing helpers for opening a store; mirror the pattern already used by the other tests in that file for `open_file ~as_of_history`. Test body:

```ocaml
let test_history_enabled () =
  let path = fresh_path () in
  cleanup path;
  Lwt_main.run
    (Lwt.finalize
       (fun () ->
          let* on = S.open_file ~as_of_history:true ~path () in
          let on = ok on in
          let* off = S.open_file ~path:(path ^ ".off") () in
          let off = ok off in
          Alcotest.(check bool) "history on" true (S.history_enabled on);
          Alcotest.(check bool) "history off" false (S.history_enabled off);
          let* () = S.close on in
          S.close off)
       (fun () ->
          cleanup path;
          cleanup (path ^ ".off");
          Lwt.return_unit))
;;
```

> Adapt `fresh_path`, `cleanup`, `ok`, `S`, and `open_file` to whatever this file already defines (read the top of `test/test_store_as_of.ml` first; it already opens as-of stores, so the helpers exist). If `open_file` there is `Sqlocaml_unix.open_file`, keep that.

- [ ] **Step 2: Run the test, verify it fails to compile**

Run: `podman run --rm -v "$W:/workspace:z" -w /workspace sqlocaml-dev dune test test/test_store_as_of.exe`
Expected: FAIL — `Unbound value S.history_enabled`.

- [ ] **Step 3: Add the implementation**

In `lib/store/store.ml`, immediately after the `history_log` function (ends ~line 1330):

```ocaml
let history_enabled t =
  match bt_of t with
  | Some { history = Some _; _ } -> true
  | _ -> false
;;
```

In `lib/store/store.mli`, after the `history_log` val (~line 191):

```ocaml
(** [history_enabled t] (#266/#412) reports whether the store was opened with an
    as-of commit-log sink ([~as_of_history:true] with a sink supplied).  [false]
    on the in-memory backend or when as-of history is disabled.  Used by ATTACH
    to inherit the top handle's as-of setting. *)
val history_enabled : t -> bool
```

- [ ] **Step 4: Run the test, verify it passes**

Run: `podman run --rm -v "$W:/workspace:z" -w /workspace sqlocaml-dev dune test test/test_store_as_of.exe`
Expected: PASS.

- [ ] **Step 5: Commit**

```sh
cd "$W" && git add lib/store/store.ml lib/store/store.mli test/test_store_as_of.ml
git commit -m "$(printf 'feat(#412): add Store.history_enabled accessor\n\nReports whether a store was opened with an as-of commit-log sink, so\nATTACH can inherit the top handle.\n\nCo-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>')"
```

---

## Task 2: Per-schema retention API (`?schema` on `history_*`)

**Files:**
- Modify: `lib/db/db.ml:1722-1725` (the four `history_*` lets)
- Modify: `lib/db/db.mli:190-205` (signatures + docs)
- Create: `test/test_attach_as_of.ml`
- Modify: `test/dune` (after the `test_attach` stanza, ~line 543)

- [ ] **Step 1: Write the failing test**

Create `test/test_attach_as_of.ml` with the harness below plus the first two cases (`history_pin` default-schema round-trip; unknown-schema raises). This file grows in later tasks.

```ocaml
(** #412 — as-of time travel against ATTACHed databases.  The top handle is a
    file db opened with [~as_of_history:true]; attached dbs inherit that setting
    and get their own [<path>.aslog].  Retention is per-schema. *)

module D = struct
  include Sqlocaml.Db

  let open_file = Sqlocaml_unix.open_file
end

module H = Sqlocaml_store.History
open Lwt.Syntax

let () = Sqlocaml_unix.install ()
let run = Lwt_main.run
let counter = ref 0

let fresh_path () =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/sqlocaml_test_attach_as_of_%04d.db" n
;;

let cleanup path =
  List.iter
    (fun p ->
       try Unix.unlink p with
       | _ -> ())
    [ path; path ^ "-wal"; path ^ ".aslog" ]
;;

let ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "unexpected error: %a" D.pp_error e
;;

let exec db sql =
  match run (D.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec failed: %s — %a" sql D.pp_error e
;;

(* head txn id of [schema]'s commit log after the latest commit. *)
let head_txn db ~schema =
  match List.rev (run (D.history_log ~schema db)) with
  | last :: _ -> last.H.txn_id
  | [] -> Alcotest.failf "history log for %s is empty" schema
;;

let test_pin_default_schema () =
  let main_path = fresh_path () in
  cleanup main_path;
  Lwt.finalize
    (fun () ->
       let* db = D.open_file ~as_of_history:true ~path:main_path () in
       let db = ok db in
       let* _ = D.execute db "CREATE TABLE t(id INTEGER)" in
       let t1 = head_txn db ~schema:"main" in
       D.history_pin db ~txn_id:t1;
       Alcotest.(check (option int64)) "floor pinned" (Some t1) (D.history_floor db);
       D.history_release db;
       Alcotest.(check (option int64)) "floor cleared" None (D.history_floor db);
       D.close db)
    (fun () ->
       cleanup main_path;
       Lwt.return_unit)
  |> run
;;

let test_unknown_schema_raises () =
  let main_path = fresh_path () in
  cleanup main_path;
  Lwt.finalize
    (fun () ->
       let* db = D.open_file ~as_of_history:true ~path:main_path () in
       let db = ok db in
       let raised f =
         match f () with
         | exception Invalid_argument _ -> true
         | _ -> false
       in
       Alcotest.(check bool)
         "pin unknown raises"
         true
         (raised (fun () -> D.history_pin ~schema:"nope" db ~txn_id:1L));
       Alcotest.(check bool)
         "floor unknown raises"
         true
         (raised (fun () -> ignore (D.history_floor ~schema:"nope" db)));
       Alcotest.(check bool)
         "release unknown raises"
         true
         (raised (fun () -> D.history_release ~schema:"nope" db));
       D.close db)
    (fun () ->
       cleanup main_path;
       Lwt.return_unit)
  |> run
;;

let () =
  Alcotest.run
    "attach_as_of"
    [ ( "retention"
      , [ Alcotest.test_case "pin default schema" `Quick test_pin_default_schema
        ; Alcotest.test_case "unknown schema raises" `Quick test_unknown_schema_raises
        ] )
    ]
;;
```

Register the exe in `test/dune` after the `test_attach` stanza (~line 543):

```
(test
 (name test_attach_as_of)
 (modules test_attach_as_of)
 (libraries sqlocaml sqlocaml.unix alcotest lwt.unix unix))
```

- [ ] **Step 2: Run the test, verify it fails to compile**

Run: `podman run --rm -v "$W:/workspace:z" -w /workspace sqlocaml-dev dune test test/test_attach_as_of.exe`
Expected: FAIL — `history_pin`/`history_floor`/`history_release`/`history_log` do not accept `~schema` (signature mismatch / unknown label).

- [ ] **Step 3: Add `?schema` and the resolver**

In `lib/db/db.ml`, replace lines 1722-1725:

```ocaml
let history_pin t ~txn_id = S.history_pin t.store ~txn_id
let history_floor t = S.history_floor t.store
let history_release t = S.history_release t.store
let history_log t = S.history_log t.store
```

with:

```ocaml
(* #412: resolve a schema name to the store whose as-of history it owns.
   "main" is the top handle's own store; any other name must currently be an
   ATTACHed schema.  Raises [Invalid_argument] on an unknown schema. *)
let store_for_schema (top : t) schema =
  if String.equal schema "main"
  then top.store
  else (
    match Hashtbl.find_opt top.attached schema with
    | Some sub -> sub.store
    | None -> invalid_arg (Printf.sprintf "history: unknown schema '%s'" schema))
;;

let history_pin ?(schema = "main") t ~txn_id =
  S.history_pin (store_for_schema t schema) ~txn_id
;;

let history_floor ?(schema = "main") t = S.history_floor (store_for_schema t schema)
let history_release ?(schema = "main") t = S.history_release (store_for_schema t schema)
let history_log ?(schema = "main") t = S.history_log (store_for_schema t schema)
```

In `lib/db/db.mli`, replace lines 190-205 with:

```ocaml
(** [history_pin ?schema t ~txn_id] (#266/#412) sets the retention floor at
    [txn_id] for [schema] (default ["main"]; otherwise a currently ATTACHed
    schema): the committed root at or before [txn_id] is retained so
    {!query_as_of} can reach it.  No-op when as-of history is not enabled.
    @raise Invalid_argument if [schema] is neither ["main"] nor attached. *)
val history_pin : ?schema:string -> t -> txn_id:int64 -> unit

(** [history_floor ?schema t] (#266/#412) is [schema]'s current retention floor
    txn id, or [None] when no floor is pinned (or as-of history is not enabled).
    @raise Invalid_argument if [schema] is neither ["main"] nor attached. *)
val history_floor : ?schema:string -> t -> int64 option

(** [history_release ?schema t] (#266/#412) clears [schema]'s retention floor,
    allowing pruning of previously pinned historical roots.
    @raise Invalid_argument if [schema] is neither ["main"] nor attached. *)
val history_release : ?schema:string -> t -> unit

(** [history_log ?schema t] (#266/#412) is [schema]'s recorded commit history
    (the [<path>.aslog] sidecar), oldest first.  Empty when as-of history is not
    enabled.
    @raise Invalid_argument if [schema] is neither ["main"] nor attached. *)
val history_log : ?schema:string -> t -> Sqlocaml_store.History.record list Lwt.t
```

- [ ] **Step 4: Run the test, verify it passes**

Run: `podman run --rm -v "$W:/workspace:z" -w /workspace sqlocaml-dev dune test test/test_attach_as_of.exe`
Expected: PASS (both cases).

- [ ] **Step 5: Commit**

```sh
cd "$W" && git add lib/db/db.ml lib/db/db.mli test/test_attach_as_of.ml test/dune
git commit -m "$(printf 'feat(#412): per-schema retention API (?schema on history_*)\n\nhistory_pin/floor/release/log gain ?schema (default \"main\"); unknown\nschema raises Invalid_argument.\n\nCo-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>')"
```

---

## Task 3: ATTACH inherits the top handle's as-of setting

**Files:**
- Modify: `lib/db/db.ml:90-94` (the `file_provider` type), `lib/db/db.ml:1379` (ATTACH call)
- Modify: `lib/db/db.mli:92-95` (provider doc — if `open_store` field is documented there)
- Modify: `lib/unix/sqlocaml_unix.ml:15-26` (provider implementation)
- Test: `test/test_attach_as_of.ml`

- [ ] **Step 1: Write the failing test**

Add to `test/test_attach_as_of.ml` (and register both cases in the Alcotest list):

```ocaml
(* Attaching under a history-enabled top inherits as-of: writes to the attached
   db land in its own commit log. *)
let test_attach_inherits_history () =
  let main_path = fresh_path () in
  let aux_path = main_path ^ ".aux" in
  cleanup main_path;
  cleanup aux_path;
  Lwt.finalize
    (fun () ->
       let* db = D.open_file ~as_of_history:true ~path:main_path () in
       let db = ok db in
       exec db (Printf.sprintf "ATTACH DATABASE '%s' AS aux" aux_path);
       exec db "CREATE TABLE aux.t(id INTEGER)";
       exec db "INSERT INTO aux.t VALUES (1)";
       let log = run (D.history_log ~schema:"aux" db) in
       Alcotest.(check bool) "aux log non-empty" true (log <> []);
       D.close db)
    (fun () ->
       cleanup main_path;
       cleanup aux_path;
       Lwt.return_unit)
  |> run
;;

(* A history-OFF top yields a history-OFF attached db: query_as_of on it is
   History_unavailable. *)
let test_attach_history_off () =
  let main_path = fresh_path () in
  let aux_path = main_path ^ ".aux" in
  cleanup main_path;
  cleanup aux_path;
  let result =
    Lwt.finalize
      (fun () ->
         let* db = D.open_file ~path:main_path () in
         let db = ok db in
         exec db (Printf.sprintf "ATTACH DATABASE '%s' AS aux" aux_path);
         exec db "CREATE TABLE aux.t(id INTEGER)";
         exec db "PRAGMA active_database = aux";
         let* r = D.query_as_of db (`Txn 1L) "SELECT id FROM t" in
         let* () = D.close db in
         Lwt.return r)
      (fun () ->
         cleanup main_path;
         cleanup aux_path;
         Lwt.return_unit)
    |> run
  in
  match result with
  | Error D.History_unavailable -> ()
  | Ok _ -> Alcotest.fail "expected History_unavailable, got Ok"
  | Error e -> Alcotest.failf "expected History_unavailable, got %a" D.pp_error e
;;
```

> Note: `test_attach_history_off` will still hit the OLD guard until Task 4 removes it. With history off on the attached store, routing reaches the attached handle; the current guard returns a `Runtime` error, not `History_unavailable`. To keep this task's test honest about *inheritance only*, assert on the log here and DEFER the `query_as_of` assertion: include `test_attach_inherits_history` in this task's list now, and add `test_attach_history_off` to the list in Task 4 (where the guard is gone and the routed store actually raises `History_unavailable`). Add only `test_attach_inherits_history` to the Alcotest list in this task.

Add to the Alcotest `retention` (or a new `"attach"`) group:

```ocaml
; Alcotest.test_case "attach inherits history" `Quick test_attach_inherits_history
```

- [ ] **Step 2: Run the test, verify it fails**

Run: `podman run --rm -v "$W:/workspace:z" -w /workspace sqlocaml-dev dune test test/test_attach_as_of.exe`
Expected: FAIL — `aux log non-empty` is `false` (attached store opened without history, so `<aux>.aslog` is empty / `history_log` returns `[]`).

- [ ] **Step 3: Thread `?as_of_history` through the provider and ATTACH**

In `lib/db/db.ml`, change the `file_provider` type (lines 90-94):

```ocaml
type file_provider =
  { open_store :
      ?geom:S.Geometry.t
      -> ?as_of_history:bool
      -> path:string
      -> unit
      -> (S.t, S.error) result Lwt.t
  ; remove_file : string -> unit
  ; rename_file : string -> string -> unit
  }
```

In `lib/db/db.ml`, the ATTACH open call (line 1379) — inherit from the top handle:

```ocaml
let* result =
  prov.open_store ~as_of_history:(S.history_enabled top.store) ~path ()
in
```

In `lib/unix/sqlocaml_unix.ml`, update the provider's `open_store` (lines 16-26):

```ocaml
  { Sqlocaml.Db.open_store =
      (fun ?geom ?as_of_history ~path () ->
        match geom with
        | None -> Store.open_file ?as_of_history ~path ()
        | Some g ->
          Store.open_file
            ?as_of_history
            ~page_size:g.Sqlocaml_storage.Geometry.page_size
            ~reserved_bytes_per_page:g.Sqlocaml_storage.Geometry.reserved_bytes_per_page
            ~explicit_geometry:true
            ~path
            ())
```

If `lib/db/db.mli` documents the `open_store` field (around lines 92-95), add a sentence: `[as_of_history] (default [false]) enables the attached store's as-of commit log.` Keep the `(** … *)` style.

> The two VACUUM call sites (`db.ml:326`, `db.ml:339`) keep calling `prov.open_store` without `~as_of_history` — the new optional defaults to `false`, so they compile unchanged. VACUUM history preservation is intentionally out of scope.

- [ ] **Step 4: Run the test, verify it passes**

Run: `podman run --rm -v "$W:/workspace:z" -w /workspace sqlocaml-dev dune build && podman run --rm -v "$W:/workspace:z" -w /workspace sqlocaml-dev dune test test/test_attach_as_of.exe`
Expected: PASS — `aux log non-empty` is `true`.

- [ ] **Step 5: Commit**

```sh
cd "$W" && git add lib/db/db.ml lib/db/db.mli lib/unix/sqlocaml_unix.ml test/test_attach_as_of.ml
git commit -m "$(printf 'feat(#412): ATTACH inherits top handle as-of setting\n\nThread ?as_of_history through file_provider.open_store; ATTACH passes\nStore.history_enabled top.store so attached dbs get their own .aslog\nwhen the top has history on.\n\nCo-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>')"
```

---

## Task 4: `query_as_of` opens the snapshot on the routed store

**Files:**
- Modify: `lib/db/db.ml:1742-1818` (`query_as_of`)
- Modify: `lib/db/db.mli:173-183` (doc comment)
- Test: `test/test_attach_as_of.ml`

- [ ] **Step 1: Write the failing test**

Add to `test/test_attach_as_of.ml`. The happy path: write to `aux`, capture its head txn, write more, pin `aux`'s floor, query as-of and see only the old row. Also wire in `test_attach_history_off` (from Task 3) and a `History_pruned` case. Register all three in the Alcotest list.

```ocaml
let texts rows =
  List.map
    (fun (row : D.row) ->
       match row.(0) with
       | D.V_int n -> Int64.to_int n
       | _ -> Alcotest.fail "expected INT in column 0")
    rows
;;

let test_as_of_on_attached () =
  let main_path = fresh_path () in
  let aux_path = main_path ^ ".aux" in
  cleanup main_path;
  cleanup aux_path;
  let only_1, both =
    Lwt.finalize
      (fun () ->
         let* db = D.open_file ~as_of_history:true ~path:main_path () in
         let db = ok db in
         exec db (Printf.sprintf "ATTACH DATABASE '%s' AS aux" aux_path);
         exec db "CREATE TABLE aux.t(id INTEGER)";
         exec db "INSERT INTO aux.t VALUES (1)";
         let t1 = head_txn db ~schema:"aux" in
         exec db "INSERT INTO aux.t VALUES (2)";
         D.history_pin ~schema:"aux" db ~txn_id:t1;
         exec db "PRAGMA active_database = aux";
         let* hist = D.query_as_of db (`Txn t1) "SELECT id FROM t" in
         let* hist_rows = Lwt_stream.to_list (ok hist) in
         let* live = D.query db "SELECT id FROM t" in
         let* live_rows = Lwt_stream.to_list (ok live) in
         let* () = D.close db in
         Lwt.return (texts hist_rows, List.sort compare (texts live_rows)))
      (fun () ->
         cleanup main_path;
         cleanup aux_path;
         Lwt.return_unit)
    |> run
  in
  Alcotest.(check (list int)) "as-of t1 yields only 1" [ 1 ] only_1;
  Alcotest.(check (list int)) "live yields 1 and 2" [ 1; 2 ] both
;;

let test_as_of_attached_pruned () =
  let main_path = fresh_path () in
  let aux_path = main_path ^ ".aux" in
  cleanup main_path;
  cleanup aux_path;
  let result =
    Lwt.finalize
      (fun () ->
         let* db = D.open_file ~as_of_history:true ~path:main_path () in
         let db = ok db in
         exec db (Printf.sprintf "ATTACH DATABASE '%s' AS aux" aux_path);
         exec db "CREATE TABLE aux.t(id INTEGER)";
         exec db "INSERT INTO aux.t VALUES (1)";
         let t1 = head_txn db ~schema:"aux" in
         exec db "PRAGMA active_database = aux";
         (* No floor pinned on aux ⇒ History_pruned. *)
         let* r = D.query_as_of db (`Txn t1) "SELECT id FROM t" in
         let* () = D.close db in
         Lwt.return r)
      (fun () ->
         cleanup main_path;
         cleanup aux_path;
         Lwt.return_unit)
    |> run
  in
  match result with
  | Error D.History_pruned -> ()
  | Ok _ -> Alcotest.fail "expected History_pruned, got Ok"
  | Error e -> Alcotest.failf "expected History_pruned, got %a" D.pp_error e
;;
```

Add to the Alcotest list:

```ocaml
; Alcotest.test_case "as-of on attached" `Quick test_as_of_on_attached
; Alcotest.test_case "as-of attached pruned" `Quick test_as_of_attached_pruned
; Alcotest.test_case "attach history off" `Quick test_attach_history_off
```

- [ ] **Step 2: Run the test, verify it fails**

Run: `podman run --rm -v "$W:/workspace:z" -w /workspace sqlocaml-dev dune test test/test_attach_as_of.exe`
Expected: FAIL — `test_as_of_on_attached` gets a `Runtime "as-of queries are not supported against attached databases"` error (the current guard), so `ok hist` raises. `test_attach_history_off` likewise gets `Runtime`, not `History_unavailable`.

- [ ] **Step 3: Reorder `query_as_of` and drop the guard**

In `lib/db/db.ml`, replace the whole `query_as_of` function (lines 1742-1818) with:

```ocaml
let query_as_of top (target : Sqlocaml_store.History.target) sql =
  Lwt.catch
    (fun () ->
       (* #412: route FIRST, then open the historical snapshot on whichever store
          the statement resolves to (MAIN, or the active ATTACHed schema).  The
          executor already runs against the routed handle's store/catalog/clock;
          only the snapshot needs to follow routing.  As-of resolves per store —
          a single query cannot span two databases at one target (their commit
          orders are independent). *)
       let op_promise, t = compile_routed top sql in
       let* op = op_promise in
       match op with
       | Error e -> Lwt.return (Error e)
       | Ok op ->
         let* ro = S.ro_begin_as_of t.store target in
         (* Idempotent ender shared by the pre-stream guard and the drain path. *)
         let ended = ref false in
         let end_ro () =
           if !ended
           then Lwt.return_unit
           else (
             ended := true;
             S.ro_end ro)
         in
         Lwt.catch
           (fun () ->
              match
                Sql.Exec.query
                  ~mode:(Sql.Exec.In_ro_txn ro)
                  ~clock:t.clock
                  t.store
                  t.catalog
                  op
              with
              | exception Failure msg ->
                let* () = end_ro () in
                Lwt.return (Error (Runtime msg))
              | lwt_stream ->
                let* stream = lwt_stream in
                let wrapped =
                  Lwt_stream.from (fun () ->
                    Lwt.catch
                      (fun () ->
                         let* next = Lwt_stream.get stream in
                         match next with
                         | None ->
                           let* () = end_ro () in
                           Lwt.return_none
                         | Some row -> Lwt.return_some row)
                      (fun exn ->
                         let* () = end_ro () in
                         Lwt.fail exn))
                in
                Lwt.return (Ok wrapped))
           (fun exn ->
              let* () = end_ro () in
              Lwt.fail exn))
    (function
      | S.History_error S.History_unavailable -> Lwt.return (Error History_unavailable)
      | S.History_error S.History_pruned -> Lwt.return (Error History_pruned)
      | exn -> Lwt.fail exn)
;;
```

> Why this is leak-safe: `ro` is now opened only *after* a successful compile, so a planner failure during `op_promise` happens before any reader exists (the outer `Lwt.catch` re-raises). `ro_begin_as_of` releases its own read lock on `History_error` (the #266 fix). Once `ro` is open, every exit — `Failure`, mid-stream exception, drain-to-`None` — runs `end_ro` exactly once via the `ended` flag.

In `lib/db/db.mli`, replace the last two sentences of the `query_as_of` doc (lines 180-183, the "Schema is read at HEAD…" sentence stays; the "As-of applies to the MAIN database only…" sentence is replaced):

```ocaml
    [target] may misinterpret older rows (schema-as-of is out of scope, #266).
    As-of resolves against whichever database the statement routes to — MAIN, or
    the active ATTACHed schema (#412).  A single query cannot span MAIN and an
    attached db at one [target]: their commit orders are independent, so routing
    consults exactly one store's history. *)
```

- [ ] **Step 4: Run the test, verify it passes**

Run: `podman run --rm -v "$W:/workspace:z" -w /workspace sqlocaml-dev dune test test/test_attach_as_of.exe`
Expected: PASS — all attach-as-of cases green; also run the existing `test/test_db_as_of.exe` to confirm main-path as-of still works:
`podman run --rm -v "$W:/workspace:z" -w /workspace sqlocaml-dev dune test test/test_db_as_of.exe` → PASS.

- [ ] **Step 5: Commit**

```sh
cd "$W" && git add lib/db/db.ml lib/db/db.mli test/test_attach_as_of.ml
git commit -m "$(printf 'feat(#412): query_as_of opens snapshot on the routed store\n\nRoute first, then ro_begin_as_of on t.store (was top.store); drop the\nattached-db guard. As-of now works against the active attached schema;\nresolution is per store.\n\nCloses #412\n\nCo-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>')"
```

---

## Task 5: QCheck floor-independence + detach edge + final gates

**Files:**
- Modify: `test/test_attach_as_of.ml`

- [ ] **Step 1: Write the property + detach test**

Add to `test/test_attach_as_of.ml`. The property: for two independent pin targets, pinning `main` and `aux` floors leaves each store's `history_floor` exactly what was set for it (independence — setting one never changes the other). Also a detach edge: after `DETACH aux`, `history_pin ~schema:"aux"` raises `Invalid_argument`.

```ocaml
let test_floor_independence =
  QCheck.Test.make
    ~name:"main and aux floors are independent"
    ~count:50
    QCheck.(pair (int_range 1 1_000_000) (int_range 1 1_000_000))
    (fun (a, b) ->
       let main_path = fresh_path () in
       let aux_path = main_path ^ ".aux" in
       cleanup main_path;
       cleanup aux_path;
       let result =
         Lwt.finalize
           (fun () ->
              let* db = D.open_file ~as_of_history:true ~path:main_path () in
              let db = ok db in
              exec db (Printf.sprintf "ATTACH DATABASE '%s' AS aux" aux_path);
              let fa = Int64.of_int a
              and fb = Int64.of_int b in
              D.history_pin db ~txn_id:fa;
              D.history_pin ~schema:"aux" db ~txn_id:fb;
              let mf = D.history_floor db in
              let af = D.history_floor ~schema:"aux" db in
              let* () = D.close db in
              Lwt.return (mf, af))
           (fun () ->
              cleanup main_path;
              cleanup aux_path;
              Lwt.return_unit)
         |> run
       in
       result = (Some (Int64.of_int a), Some (Int64.of_int b)))
;;

let test_detach_then_pin_raises () =
  let main_path = fresh_path () in
  let aux_path = main_path ^ ".aux" in
  cleanup main_path;
  cleanup aux_path;
  Lwt.finalize
    (fun () ->
       let* db = D.open_file ~as_of_history:true ~path:main_path () in
       let db = ok db in
       exec db (Printf.sprintf "ATTACH DATABASE '%s' AS aux" aux_path);
       exec db "DETACH DATABASE aux";
       let raised =
         match D.history_pin ~schema:"aux" db ~txn_id:1L with
         | exception Invalid_argument _ -> true
         | _ -> false
       in
       Alcotest.(check bool) "pin after detach raises" true raised;
       D.close db)
    (fun () ->
       cleanup main_path;
       cleanup aux_path;
       Lwt.return_unit)
  |> run
;;
```

Add the QCheck test via the alcotest-qcheck bridge and the unit case to the list. Match how other test files in this repo register QCheck tests (e.g. `QCheck_alcotest.to_alcotest test_floor_independence`). Update the Alcotest list, e.g.:

```ocaml
; ( "properties"
  , [ QCheck_alcotest.to_alcotest test_floor_independence
    ; Alcotest.test_case "detach then pin raises" `Quick test_detach_then_pin_raises
    ] )
```

> If `QCheck_alcotest` is not already a dependency of this exe, add `qcheck-alcotest` to the `test_attach_as_of` libraries in `test/dune` (check a file like `test/test_db_as_of`'s dune neighbours for the exact lib name used in this repo).

- [ ] **Step 2: Run the test, verify it passes**

Run: `podman run --rm -v "$W:/workspace:z" -w /workspace sqlocaml-dev dune test test/test_attach_as_of.exe`
Expected: PASS (all groups).

- [ ] **Step 3: Full suite + format + lint gates**

```sh
# whole suite
podman run --rm -v "$W:/workspace:z" -w /workspace sqlocaml-dev dune build
podman run --rm -v "$W:/workspace:z" -w /workspace sqlocaml-dev dune test
# ocamlformat each touched .ml/.mli (write back to host)
for f in lib/store/store.ml lib/store/store.mli lib/db/db.ml lib/db/db.mli \
         lib/unix/sqlocaml_unix.ml test/test_attach_as_of.ml test/test_store_as_of.ml; do
  tmp=$(mktemp) && podman run --rm -v "$W:/workspace:z" -w /workspace sqlocaml-dev \
    ocamlformat "$f" > "$tmp" && mv "$tmp" "$W/$f" && chmod 644 "$W/$f"
done
# dune-file formatting for test/dune
tmp=$(mktemp) && podman run --rm -v "$W:/workspace:z" -w /workspace sqlocaml-dev \
  dune format-dune-file test/dune > "$tmp" && mv "$tmp" "$W/test/dune" && chmod 644 "$W/test/dune"
# merlint — expect 0 issues for touched files
podman run --rm -v "$W:/workspace:z" -w /workspace sqlocaml-dev merlint
```

Expected: build clean, whole suite PASS, merlint 0 issues on touched files (the pre-existing `sqlite3 not found` warning is ignored).

- [ ] **Step 4: Commit**

```sh
cd "$W" && git add -A
git commit -m "$(printf 'test(#412): floor-independence property + detach edge; fmt/lint\n\nCo-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>')"
```

- [ ] **Step 5: Coverage check (optional, target 100% on touched lines)**

```sh
podman run --rm -v "$W:/workspace:z" -w /workspace sqlocaml-dev \
  bisect-ppx-report html --output _coverage_report/ _build/default/test/*.coverage
```

Inspect `lib/db/db.ml` (query_as_of, `store_for_schema`, history_* ) and `lib/store/store.ml` (`history_enabled`) for uncovered lines; add cases if any branch is missed.

---

## Open the PR (after all tasks)

```sh
cd "$W" && git push origin feat/412-as-of-attached
~/.local/bin/forgejo pr create tej/sqlite_ocaml_port \
  --title="feat(#412): as-of time travel against ATTACHed databases" \
  --head=feat/412-as-of-attached --base=main \
  --body="$(cat <<'EOF'
## Summary
- `query_as_of` routes first, then opens the historical snapshot on the routed handle's store (was top store only); attached-db guard removed.
- ATTACH inherits the top handle's `~as_of_history` (via `Store.history_enabled` + `?as_of_history` on the file provider); attached dbs get their own `<path>.aslog`.
- Per-schema retention: `history_pin`/`history_floor`/`history_release`/`history_log` gain `?schema` (default `main`); unknown schema raises `Invalid_argument`.
- As-of resolves per store; cross-schema joins-at-a-timestamp remain out of scope (documented).

## Test plan
- [ ] dune test passes
- [ ] test_attach_as_of.ml: inherit, as-of happy path, pruned, history-off, per-schema pin, unknown-schema raise, detach edge, floor-independence property

Closes #412
EOF
)"
```

---

## Self-Review (completed by plan author)

- **Spec coverage:** query_as_of routed snapshot (Task 4) ✓; inherited enablement + `history_enabled` (Tasks 1, 3) ✓; per-schema retention `?schema` (Task 2) ✓; doc rewrite (Task 4) ✓; tests incl. QCheck independence (Tasks 2-5) ✓; VACUUM-out-of-scope honored (default `false`, Task 3 note) ✓; cross-store limitation documented (Task 4 doc) ✓.
- **Placeholders:** none — every code step has full code; test-harness adaptation notes point at concrete existing files to mirror.
- **Type consistency:** `history_enabled : t -> bool` defined Task 1, used Task 3 ATTACH; `store_for_schema` defined Task 2, used by all four `history_*`; provider field `?as_of_history` defined Task 3 type, implemented same task in unix provider; `query_as_of` uses `t.store`/`t.catalog`/`t.clock`/`compile_routed` consistent with existing executor call.
