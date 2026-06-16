# #385 — Monitor table-name filter + event-log export: Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add table-name filtering and an on-demand text export to the sqlocaml internals monitor (#385), building on the #382 ring buffer + txn-id filter.

**Architecture:** Page events gain a `tree` field stamped from a new `current_tree` context tracked in the store's `bt_get_tree`/`bt_get_tree_ro` chokepoint. `Db.tree_of_table` resolves a typed name → tree id (store stays catalog-free). The REPL `Event_log` filter becomes a `No_filter | By_txn | By_table` variant; a new `t` key opens a table-filter prompt; a `.dump [path]` dot command writes the visible buffer to a text file.

**Tech Stack:** OCaml 5.x, Lwt, Alcotest + QCheck, nottui/lwd (REPL). Build/test inside the `sqlocaml-dev` podman container.

## Conventions for every build/test command

All OCaml commands run in the container, from the **worktree root**
(`.worktrees/385-monitor-filter-export`). The `$(pwd)` below is the worktree
root because you have `cd`'d into it.

```sh
# build everything
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune build
# run one test executable
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_store_event.exe
```

Commit after each task. Do **not** push until the whole plan is green + merlint/fmt pass (final task).

## File structure

- `lib/store/store_event.mli` / `.ml` — `tree` field on page variants, `tree_id_of`, `pp`.
- `lib/store/store.ml` — `current_tree` in `bt_state`; set in `bt_get_tree` + `bt_get_tree_ro`; stamp in `translate_pager_event`.
- `lib/db/db.mli` / `.ml` — `tree_of_table`.
- `bin/repl/event_log.mli` / `.ml` — filter variant, `dump`.
- `bin/repl/repl_command.ml` — `prompt`-aware `classify`, `Dump` dot, `filter_input`.
- `bin/repl/monitor_view.ml` — `t` key, header for the variant.
- `bin/repl/sqlocaml_repl.ml` — `prompt` state, table-filter resolution, `.dump` dispatch.
- `test/test_store_event.ml`, `test/test_db_event_385.ml`, `test/test_repl_components.ml` — extend (all already wired in `test/dune`).

---

## Task 1: `Store_event` — `tree` field + `tree_id_of` + `pp`

**Files:**
- Modify: `lib/store/store_event.mli`
- Modify: `lib/store/store_event.ml`
- Test: `test/test_store_event.ml` (update `test_pp_all_constructors`, `test_label_and_txn_id`; add `test_tree_id_of`)

- [ ] **Step 1: Update the failing tests first**

In `test/test_store_event.ml`, the page-event constructors now require a `tree`
field and `pp` includes `tree=`. Replace the page-event lines in
`test_label_and_txn_id`:

```ocaml
  Alcotest.(check string)
    "page_read label"
    "PAGE_READ"
    (Ev.label (Ev.Page_read { txn_id = 5L; tree = 16; page = 1L }));
  Alcotest.(check (option int64))
    "page_read txn"
    (Some 5L)
    (Ev.txn_id (Ev.Page_read { txn_id = 5L; tree = 16; page = 1L }))
```

Replace the page-event checks in `test_pp_all_constructors`:

```ocaml
  check "PAGE_READ txn=1 tree=16 page=7" (Ev.Page_read { txn_id = 1L; tree = 16; page = 7L });
  check "PAGE_WRITE txn=2 tree=16 page=8" (Ev.Page_write { txn_id = 2L; tree = 16; page = 8L });
  check
    "PAGE_ALLOC txn=3 tree=16 page=9 reused=true"
    (Ev.Page_alloc { txn_id = 3L; tree = 16; page = 9L; reused = true });
  check
    "PAGE_ALLOC txn=3 tree=16 page=9 reused=false"
    (Ev.Page_alloc { txn_id = 3L; tree = 16; page = 9L; reused = false });
  check "PAGE_FREE txn=4 tree=16 page=10" (Ev.Page_free { txn_id = 4L; tree = 16; page = 10L })
```

Add a new unit test (place it after `test_pp_all_constructors`):

```ocaml
let test_tree_id_of () =
  Alcotest.(check (option int))
    "page_read tree" (Some 16)
    (Ev.tree_id_of (Ev.Page_read { txn_id = 1L; tree = 16; page = 7L }));
  Alcotest.(check (option int))
    "page_alloc tree" (Some 32)
    (Ev.tree_id_of (Ev.Page_alloc { txn_id = 1L; tree = 32; page = 7L; reused = false }));
  Alcotest.(check (option int))
    "page_free tree" (Some 9)
    (Ev.tree_id_of (Ev.Page_free { txn_id = 1L; tree = 9; page = 7L }));
  Alcotest.(check (option int))
    "page_write tree" (Some 9)
    (Ev.tree_id_of (Ev.Page_write { txn_id = 1L; tree = 9; page = 7L }));
  Alcotest.(check (option int))
    "txn has no tree" None
    (Ev.tree_id_of (Ev.Txn_commit { txn_id = 1L; frames = 0 }))
;;
```

Register it in the `"type"` group list (in the final `Alcotest.run`):

```ocaml
        ; Alcotest.test_case "tree_id_of" `Quick test_tree_id_of
```

- [ ] **Step 2: Run the test to verify it fails to compile**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_store_event.exe`
Expected: compile error — `Page_read` has no field `tree` / `tree_id_of` unbound.

- [ ] **Step 3: Update `store_event.mli`**

Change the four page variants to add `tree : int` (after `txn_id`), and add the
accessor doc. Replace the variants:

```ocaml
  | Page_read of { txn_id : int64; tree : int; page : int64 }
  | Page_write of { txn_id : int64; tree : int; page : int64 }
  | Page_alloc of { txn_id : int64; tree : int; page : int64; reused : bool }
  | Page_free of { txn_id : int64; tree : int; page : int64 }
```

In each variant's doc comment, append a note like:
`[tree] is the store tree id whose operation triggered the I/O; [-1] when no
op context was active. Best-effort for [Page_write] (stamped at WAL-flush time
with the most recently active tree).`

Add the accessor val (next to `txn_id`):

```ocaml
(** [tree_id_of ev] is the store tree id for the four page events, or [None]
    for every non-page event. [-1] means "no active tree context". *)
val tree_id_of : t -> int option
```

- [ ] **Step 4: Update `store_event.ml`**

Add `tree` to the four page variants in the `type t` definition (mirroring the
`.mli`). Update `pp` for the four page cases to print `tree=`:

```ocaml
  | Page_read { txn_id; tree; page } ->
    Format.fprintf fmt "PAGE_READ txn=%Ld tree=%d page=%Ld" txn_id tree page
  | Page_write { txn_id; tree; page } ->
    Format.fprintf fmt "PAGE_WRITE txn=%Ld tree=%d page=%Ld" txn_id tree page
  | Page_alloc { txn_id; tree; page; reused } ->
    Format.fprintf fmt "PAGE_ALLOC txn=%Ld tree=%d page=%Ld reused=%b" txn_id tree page reused
  | Page_free { txn_id; tree; page } ->
    Format.fprintf fmt "PAGE_FREE txn=%Ld tree=%d page=%Ld" txn_id tree page
```

`label` for the page variants is unchanged (it ignores fields). Add `tree_id_of`
(after `txn_id`):

```ocaml
let tree_id_of = function
  | Page_read { tree; _ } | Page_write { tree; _ } | Page_alloc { tree; _ }
  | Page_free { tree; _ } -> Some tree
  | _ -> None
;;
```

For `txn_id`'s page cases, the added field doesn't change them — but if `txn_id`
pattern-matches the page variants with explicit fields, update them to include
`tree = _` or use `{ txn_id; _ }`. Verify `txn_id` still compiles.

- [ ] **Step 5: Run the test to verify it passes**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_store_event.exe`
Expected: PASS (note: store.ml won't compile yet — that's Task 2; this command
builds the test's deps, so if store.ml breaks, jump to Task 2 and run the build
across both. If the page-event constructor sites in `store.ml`'s
`translate_pager_event` now fail to compile, that's expected and fixed in Task 2.)

> NOTE: Because `store_event` and `store` are in the same library, adding the
> `tree` field breaks `translate_pager_event` in `store.ml` immediately. It is
> fine to do Step 4 of Task 1 and Task 2's store edits together before the build
> passes. Commit only once the build is green (end of Task 2). If you prefer a
> green commit here, temporarily stamp `tree = (-1)` in `translate_pager_event`'s
> four cases, build, commit, then refine in Task 2.

- [ ] **Step 6: Commit (after Task 2 build is green, or with the `-1` stub)**

```bash
git add lib/store/store_event.ml lib/store/store_event.mli test/test_store_event.ml
git commit -m "feat(#385): add tree field + tree_id_of to Store_event page events"
```

---

## Task 2: Store active-tree stamping

**Files:**
- Modify: `lib/store/store.ml` (`bt_state` record ~line 195; `bt_get_tree` ~line 441; `bt_get_tree_ro` ~line 523; `translate_pager_event` ~line 3262)
- Test: `test/test_store_event.ml` (add `test_page_read_carries_tree`)

- [ ] **Step 1: Write the failing test**

Add to `test/test_store_event.ml` (after `test_checkpoint_then_read_emits_page_read`,
reusing the same cold-reopen pattern so reads hit the main file):

```ocaml
(* #385: page reads are stamped with the tree id of the op that triggered them.
   Cold-reopen (non-WAL) so reads come from the main file, then read ONLY tree
   16 — every captured Page_read must carry tree=16. *)
let test_page_read_carries_tree () =
  let path = fresh_path () in
  cleanup path;
  let trees =
    Lwt.finalize
      (fun () ->
         let open Lwt.Syntax in
         let* st = S.open_file_wal ~path () in
         let st = Result.get_ok st in
         let* txn = S.rw_begin st in
         let* () = S.put txn 16 (bs "k") (bs "v") in
         let* () = S.commit txn in
         let* () = S.checkpoint st in
         let* () = S.close st in
         let seen = ref [] in
         let* st2 = Sqlocaml_unix.Store.open_file ~path () in
         let st2 = Result.get_ok st2 in
         S.set_event_callback st2 (Some (fun ev ->
           match Ev.tree_id_of ev with
           | Some t when (match ev with Ev.Page_read _ -> true | _ -> false) ->
             seen := t :: !seen
           | _ -> ()));
         let* txn2 = S.rw_begin st2 in
         let* _ = S.get txn2 16 (bs "k") in
         let* () = S.commit txn2 in
         let* () = S.close st2 in
         Lwt.return (List.rev !seen))
      (fun () -> cleanup path; Lwt.return_unit)
    |> run
  in
  Alcotest.(check bool) "at least one page read" true (trees <> []);
  Alcotest.(check bool)
    "every page read tagged tree 16" true
    (List.for_all (fun t -> t = 16) trees)
;;
```

Register it in the `"seam"` group:

```ocaml
        ; Alcotest.test_case "page read carries tree" `Quick test_page_read_carries_tree
```

- [ ] **Step 2: Run to verify it fails**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_store_event.exe`
Expected: FAIL — reads tagged `-1` (no stamping yet) so `t = 16` is false.

- [ ] **Step 3: Add `current_tree` to `bt_state`**

In `lib/store/store.ml`, in the `bt_state` record (near the `on_event` field
~line 195), add:

```ocaml
  ; mutable current_tree : tree_id option
    (* #385: the tree id of the in-flight read/write/cursor op, set at the
       bt_get_tree(_ro) chokepoint and stamped onto page events by
       translate_pager_event. Best-effort (single mutable shared across fibers),
       same spirit as the pager's txn_id. [None] => stamp tree = -1. *)
```

Initialise it where `bt_state` is constructed (the record literal that sets
`tree_tags = Hashtbl.create 16` ~line 639): add `; current_tree = None`.

- [ ] **Step 4: Set it in both tree-resolution chokepoints**

At the **top** of `bt_get_tree` (~line 441) — first line of the function body,
before any I/O:

```ocaml
let bt_get_tree st (tid : tree_id) : (Btree.t, error) result Lwt.t =
  st.current_tree <- Some tid;
  ...
```

Do the same at the top of `bt_get_tree_ro` (~line 523):

```ocaml
let bt_get_tree_ro (snap : ro_snapshot) (st : bt_state) (tid : tree_id)
  : ... =
  st.current_tree <- Some tid;
  ...
```

- [ ] **Step 5: Stamp it in `translate_pager_event`**

Replace `translate_pager_event` (~line 3262):

```ocaml
let translate_pager_event (st : bt_state) (pev : Pager_event.t) : Store_event.t =
  let txn_id = Pager.get_txn_id st.pager in
  let tree = Option.value st.current_tree ~default:(-1) in
  match pev with
  | Pager_event.Page_read { page_id } ->
    Store_event.Page_read { txn_id; tree; page = page_id }
  | Pager_event.Page_write { page_id } ->
    Store_event.Page_write { txn_id; tree; page = page_id }
  | Pager_event.Page_alloc { page_id; reused } ->
    Store_event.Page_alloc { txn_id; tree; page = page_id; reused }
  | Pager_event.Page_free { page_id } ->
    Store_event.Page_free { txn_id; tree; page = page_id }
;;
```

Update the doc comment above it to note the `tree` stamping + best-effort caveat
for `Page_write`.

- [ ] **Step 6: Build + run the test**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune build && podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_store_event.exe`
Expected: PASS.

- [ ] **Step 7: Full test sweep (nothing else broke)**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test`
Expected: PASS (any other test constructing page events would have failed to
compile — grep `Page_read {`/`Page_alloc {` etc. across `test/` and `lib/` if so,
and add `tree = ...`).

- [ ] **Step 8: Commit**

```bash
git add lib/store/store.ml lib/store/store_event.ml lib/store/store_event.mli test/test_store_event.ml
git commit -m "feat(#385): stamp active tree id onto page events via bt_get_tree chokepoint"
```

---

## Task 3: `Db.tree_of_table`

**Files:**
- Modify: `lib/db/db.mli`
- Modify: `lib/db/db.ml`
- Test: `test/test_db_event_385.ml` (add `test_tree_of_table`)

- [ ] **Step 1: Write the failing test**

Add to `test/test_db_event_385.ml` (the existing file uses module `D` with
`open_file_wal`). Add:

```ocaml
let test_tree_of_table () =
  let path = fresh_path () in
  cleanup path;
  let open Lwt.Syntax in
  let (found, missing) =
    Lwt.finalize
      (fun () ->
         let* db = D.open_file_wal ~path () in
         let db = Result.get_ok db in
         let* _ = D.execute db "CREATE TABLE t(x INTEGER)" in
         let found = D.tree_of_table db "t" in
         let missing = D.tree_of_table db "nope" in
         let* () = D.close db in
         Lwt.return (found, missing))
      (fun () -> cleanup path; Lwt.return_unit)
    |> run
  in
  Alcotest.(check bool) "known table resolves" true (found <> None);
  Alcotest.(check (option int)) "unknown table is None" None missing
;;
```

Register it in the `Alcotest.run` list:

```ocaml
    [ "passthrough", [ Alcotest.test_case "fires" `Quick test_passthrough_fires ]
    ; "tree_of_table", [ Alcotest.test_case "resolves" `Quick test_tree_of_table ]
    ]
```

- [ ] **Step 2: Run to verify it fails**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_db_event_385.exe`
Expected: compile error — `tree_of_table` unbound in module `D`.

- [ ] **Step 3: Add to `db.mli`**

Near `set_event_callback`'s declaration, add:

```ocaml
(** [tree_of_table t name] is the storage tree id backing table [name] in the
    active schema, or [None] if no such table exists. Used by the internals
    monitor to filter page events by table (#385). *)
val tree_of_table : t -> string -> int option
```

- [ ] **Step 4: Implement in `db.ml`**

`db.ml` already has `module Cat = Sqlocaml_catalog.Catalog` and a `t.catalog`
field. Add (near other catalog-backed helpers):

```ocaml
let tree_of_table t name =
  match Cat.find_table_cached t.catalog ~name with
  | None -> None
  | Some meta -> Some (Cat.tid_of_storage meta.Cat.storage)
;;
```

If `meta.Cat.storage` field-access fails to type, use the existing accessor the
codebase prefers (check how other call sites read storage, e.g.
`Cat.row_storage`); `Cat.tid_of_storage` takes the `storage` value. If
`table_meta`'s `storage` field is not exposed, expose a helper or use
`Cat.tid_of_storage (… meta)` per the catalog API. Confirm with:
`grep -n "tid_of_storage\|val storage\|type table_meta" lib/catalog/catalog.mli`.

- [ ] **Step 5: Run to verify it passes**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_db_event_385.exe`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add lib/db/db.ml lib/db/db.mli test/test_db_event_385.ml
git commit -m "feat(#385): Db.tree_of_table — resolve table name to storage tree id"
```

---

## Task 4: `Event_log` filter variant + `dump`

**Files:**
- Modify: `bin/repl/event_log.mli`
- Modify: `bin/repl/event_log.ml`
- Test: `test/test_repl_components.ml` (rewrite `test_filter`; add `test_table_filter`, `test_dump`)

- [ ] **Step 1: Update + add failing tests**

In `test/test_repl_components.ml`, replace `test_filter` (it uses the old
`set_filter l (Some 2L)` API) with:

```ocaml
let mk_page ~txn ~tree =
  Ev.Page_read { txn_id = Int64.of_int txn; tree; page = 1L }
;;

let test_filter () =
  let l = L.create ~capacity:10 in
  List.iter (fun i -> L.push l (mk_commit (Int64.of_int i))) [ 1; 2; 3 ];
  L.set_filter l (L.By_txn 2L);
  Alcotest.(check (list int64))
    "only txn 2" [ 2L ]
    (List.filter_map Ev.txn_id (L.visible l));
  L.set_filter l L.No_filter;
  Alcotest.(check int) "filter cleared" 3 (List.length (L.visible l))
;;

let test_table_filter () =
  let l = L.create ~capacity:10 in
  L.push l (mk_page ~txn:1 ~tree:16);
  L.push l (mk_page ~txn:1 ~tree:32);
  L.push l (mk_commit 1L);
  L.set_filter l (L.By_table { name = "t"; tree = 16 });
  let trees = List.filter_map Ev.tree_id_of (L.visible l) in
  Alcotest.(check (list int)) "only tree 16 page events" [ 16 ] trees;
  Alcotest.(check int) "global commit hidden under table filter" 1
    (List.length (L.visible l))
;;

let test_dump () =
  let l = L.create ~capacity:10 in
  L.push l (mk_commit 1L);
  L.push l (mk_page ~txn:1 ~tree:16);
  let path = Filename.temp_file "sqlocaml_dump" ".log" in
  (match L.dump l path with
   | Ok n -> Alcotest.(check int) "dumped 2 events" 2 n
   | Error e -> Alcotest.failf "dump failed: %s" e);
  let ic = open_in path in
  let lines = ref [] in
  (try while true do lines := input_line ic :: !lines done with End_of_file -> ());
  close_in ic;
  Sys.remove path;
  Alcotest.(check int) "file has 2 lines" 2 (List.length !lines);
  (* dump respects the active filter *)
  L.set_filter l (L.By_table { name = "t"; tree = 16 });
  (match L.dump l path with
   | Ok n -> Alcotest.(check int) "filtered dump = 1" 1 n
   | Error e -> Alcotest.failf "dump failed: %s" e);
  Sys.remove path
;;
```

Register in the `"event_log"` group:

```ocaml
        ; Alcotest.test_case "table filter" `Quick test_table_filter
        ; Alcotest.test_case "dump" `Quick test_dump
```

- [ ] **Step 2: Run to verify it fails**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_repl_components.exe`
Expected: compile error — `L.By_txn` / `L.By_table` / `L.dump` unbound.

- [ ] **Step 3: Update `event_log.mli`**

Replace the txn-filter section. Change the filter type + the `set_filter` /
`filter` signatures, and add `dump`:

```ocaml
(** Active filter over the event stream (#385). *)
type filter =
  | No_filter
  | By_txn of int64
  | By_table of
      { name : string  (** display name; for the header only *)
      ; tree : int  (** the table's storage tree id; matched against events *)
      }

(** [set_filter t f] sets the active filter ([No_filter] = show all). *)
val set_filter : t -> filter -> unit

(** The active filter. *)
val filter : t -> filter

(** [dump t path] writes the currently {!visible} events (respecting the active
    filter), oldest-first, one per line via [Store_event.pp], to [path]
    (truncating). Returns the number of events written, or an error message on
    an I/O failure. *)
val dump : t -> string -> (int, string) result
```

Update `visible`'s doc comment to say "after applying the active filter"
(generic, not just txn).

- [ ] **Step 4: Update `event_log.ml`**

Change the record field type and the functions:

```ocaml
type filter =
  | No_filter
  | By_txn of int64
  | By_table of { name : string; tree : int }

type t =
  { capacity : int
  ; q : Event.t Queue.t
  ; mutable filter : filter
  ; mutable paused : bool
  ; state : unit Lwd.var
  }

let create ~capacity =
  { capacity; q = Queue.create (); filter = No_filter; paused = false; state = Lwd.var () }
;;
```

Replace `visible`:

```ocaml
let visible t =
  let all = List.of_seq (Queue.to_seq t.q) in
  match t.filter with
  | No_filter -> all
  | By_txn id -> List.filter (fun ev -> Event.txn_id ev = Some id) all
  | By_table { tree; _ } -> List.filter (fun ev -> Event.tree_id_of ev = Some tree) all
;;
```

Replace `set_filter` / `filter`:

```ocaml
let set_filter t f =
  t.filter <- f;
  bump t
;;

let filter t = t.filter
```

Add `dump` (after `clear`):

```ocaml
let dump t path =
  let evs = visible t in
  try
    let oc = open_out path in
    Fun.protect
      ~finally:(fun () -> close_out oc)
      (fun () ->
        List.iter (fun ev -> Printf.fprintf oc "%s\n" (Format.asprintf "%a" Event.pp ev)) evs);
    Ok (List.length evs)
  with
  | Sys_error msg -> Error msg
;;
```

Update `pp` (the `Event_log` pretty-printer at the bottom) so its `filter=`
branch matches the new variant:

```ocaml
    (match t.filter with
     | No_filter -> "-"
     | By_txn id -> Printf.sprintf "txn=%Ld" id
     | By_table { name; _ } -> Printf.sprintf "tbl=%s" name)
```

- [ ] **Step 5: Run to verify it passes**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_repl_components.exe`
Expected: compile errors remain in `monitor_view.ml` / `sqlocaml_repl.ml` (they
call the old `set_filter`/`filter`). That is expected — they are fixed in Tasks
5–6. To get THIS test green in isolation is not possible until the library
compiles. Proceed to Tasks 5–6, then run the full suite. (If you want an
intermediate green build, temporarily adjust the two call sites in
`monitor_view.ml`/`sqlocaml_repl.ml` to `No_filter`, then refine in Tasks 5–6.)

- [ ] **Step 6: Commit (with Tasks 5–6, once `repl_lib` builds)**

```bash
git add bin/repl/event_log.ml bin/repl/event_log.mli test/test_repl_components.ml
git commit -m "feat(#385): Event_log filter variant (txn/table) + dump-to-file"
```

---

## Task 5: `repl_command` — prompt-aware `classify` + `.dump`

**Files:**
- Modify: `bin/repl/repl_command.ml`
- Test: `test/test_repl_components.ml` (rewrite `test_classify`; extend `test_parse_dot`)

- [ ] **Step 1: Update failing tests**

In `test/test_repl_components.ml`, replace `test_classify` (old API used
`~filter_mode:bool` and `C.Filter (Some 42L)`):

```ocaml
let test_classify () =
  Alcotest.(check bool) "empty" true (C.classify ~prompt:C.No_prompt "  " = C.Empty);
  Alcotest.(check bool)
    "sql" true
    (C.classify ~prompt:C.No_prompt "SELECT 1;" = C.Sql [ "SELECT 1" ]);
  Alcotest.(check bool)
    "dot" true (C.classify ~prompt:C.No_prompt ".tables" = C.Dot C.Tables);
  Alcotest.(check bool)
    "txn filter ok" true
    (C.classify ~prompt:C.Txn_prompt "42" = C.Filter (C.Filter_txn (Some 42L)));
  Alcotest.(check bool)
    "txn filter bad" true
    (C.classify ~prompt:C.Txn_prompt "xx" = C.Filter (C.Filter_txn None));
  Alcotest.(check bool)
    "table filter" true
    (C.classify ~prompt:C.Table_prompt " users " = C.Filter (C.Filter_table "users"))
;;
```

Extend `test_parse_dot` with `.dump` cases:

```ocaml
  Alcotest.(check bool) "dump none" true (C.parse_dot ".dump" = C.Dump None);
  Alcotest.(check bool) "dump path" true (C.parse_dot ".dump /t/x.log" = C.Dump (Some "/t/x.log"));
```

- [ ] **Step 2: Run to verify it fails**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_repl_components.exe`
Expected: compile error — `C.No_prompt` / `C.Filter_txn` / `C.Dump` unbound.

- [ ] **Step 3: Update `repl_command.ml`**

Add `Dump` to `dot`, add `prompt` and `filter_input` types, change `action`'s
`Filter`, extend `parse_dot`, and make `classify` prompt-aware:

```ocaml
type dot =
  | Help
  | Quit
  | Tables
  | Schema of string option
  | Databases
  | Open of string
  | Dump of string option
  | Unknown of string

type prompt =
  | No_prompt
  | Txn_prompt
  | Table_prompt

type filter_input =
  | Filter_txn of int64 option
  | Filter_table of string

type action =
  | Empty
  | Filter of filter_input
  | Dot of dot
  | Sql of string list
```

In `parse_dot`, add before the `| _ -> Unknown` arm:

```ocaml
  | [ ".dump" ] -> Dump None
  | [ ".dump"; path ] -> Dump (Some path)
```

Replace `classify`:

```ocaml
let classify ~prompt input =
  match prompt with
  | Txn_prompt -> Filter (Filter_txn (Int64.of_string_opt (String.trim input)))
  | Table_prompt -> Filter (Filter_table (String.trim input))
  | No_prompt ->
    let t = String.trim input in
    if t = ""
    then Empty
    else if t.[0] = '.'
    then Dot (parse_dot t)
    else Sql (Repl_engine.split_stmts input)
;;
```

- [ ] **Step 4: Run the component test (still needs Task 6 for the app)**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune build bin/repl/repl_lib.cma 2>&1 | head` — `sqlocaml_repl.ml` is the executable, not in `repl_lib`, so `repl_lib` (with `repl_command`) should now compile. The test exe still won't link until Task 6 (it depends on `sqlocaml_repl.exe`? No — `test_repl_components` depends on `repl_lib`, not the exe). So:

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_repl_components.exe`
Expected: still fails to build IF `monitor_view.ml` (in `repl_lib`) hasn't been
updated for the new `Event_log` API. Do Task 6 next; `monitor_view` is part of
`repl_lib`. After Task 6, this test passes.

- [ ] **Step 5: Commit (with Task 6)**

```bash
git add bin/repl/repl_command.ml test/test_repl_components.ml
git commit -m "feat(#385): prompt-aware classify (txn/table) + .dump dot command"
```

---

## Task 6: `monitor_view` `t` key + header; `sqlocaml_repl` wiring

**Files:**
- Modify: `bin/repl/monitor_view.ml`
- Modify: `bin/repl/sqlocaml_repl.ml`
- Test: `test/test_repl_components.ml` already exercises `monitor_view.render`;
  add a key-handling smoke test.

- [ ] **Step 1: Add a failing smoke test for the `t` key**

In `test/test_repl_components.ml`, after `test_monitor_renders`, add a test that
the `t` key invokes the table-filter callback:

```ocaml
let test_monitor_table_key () =
  let l = Event_log.create ~capacity:10 in
  let txn_called = ref false in
  let table_called = ref false in
  let r =
    Monitor_view.handle_key l
      ~set_filter_prompt:(fun () -> txn_called := true)
      ~set_table_filter_prompt:(fun () -> table_called := true)
      (`ASCII 't', [])
  in
  Alcotest.(check bool) "t handled" true (r = `Handled);
  Alcotest.(check bool) "table prompt opened" true !table_called;
  Alcotest.(check bool) "txn prompt not opened" false !txn_called
;;
```

Register it in the `"monitor_view"` group:

```ocaml
    ; ( "monitor_view"
      , [ Alcotest.test_case "renders" `Quick test_monitor_renders
        ; Alcotest.test_case "table key" `Quick test_monitor_table_key
        ] )
```

- [ ] **Step 2: Run to verify it fails**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_repl_components.exe`
Expected: compile error — `handle_key` has no `~set_table_filter_prompt`.

- [ ] **Step 3: Update `monitor_view.ml`**

Update `header` to render the new filter variant:

```ocaml
let header log =
  let filt =
    match Event_log.filter log with
    | Event_log.No_filter -> "all"
    | Event_log.By_txn id -> Printf.sprintf "txn=%Ld" id
    | Event_log.By_table { name; _ } -> Printf.sprintf "tbl=%s" name
  in
  let pause = if Event_log.paused log then "PAUSED" else "live" in
  W.string
    ~attr:Notty.A.(st bold)
    (Printf.sprintf
       "-- internals monitor [%s] [%s]  (space=pause /=txn t=table c=clear x=clear-log) --"
       pause
       filt)
;;
```

Update `handle_key` to take `~set_table_filter_prompt`, add the `t` key, and make
`c` clear via the variant:

```ocaml
let handle_key log ~set_filter_prompt ~set_table_filter_prompt (key : Ui.key)
  : Ui.may_handle
  =
  match key with
  | `ASCII ' ', _ -> Event_log.toggle_pause log; `Handled
  | `ASCII 'c', _ -> Event_log.set_filter log Event_log.No_filter; `Handled
  | `ASCII 'x', _ -> Event_log.clear log; `Handled
  | `ASCII '/', _ -> set_filter_prompt (); `Handled
  | `ASCII 't', _ -> set_table_filter_prompt (); `Handled
  | _ -> `Unhandled
;;
```

- [ ] **Step 4: Update `sqlocaml_repl.ml`**

Replace the `filter_mode` flag with a `prompt` state and wire the new flows.

Change the declaration (~line 29):

```ocaml
let prompt = ref Repl_command.No_prompt
```

Add `module C = Repl_command` near the top module aliases if not present (the
file currently references `Repl_command` fully-qualified; either is fine —
examples below use `Repl_command.`).

Replace `submit` (~line 145) to handle the new `Filter` payloads and reset the
prompt:

```ocaml
let submit input =
  match Repl_command.classify ~prompt:!prompt input with
  | Empty -> Lwt.return_unit
  | Filter (Repl_command.Filter_txn idopt) ->
    prompt := Repl_command.No_prompt;
    (match idopt with
     | Some id ->
       Event_log.set_filter log (Event_log.By_txn id);
       Shell_view.set_status shell (Printf.sprintf "filter: txn=%Ld" id)
     | None ->
       Event_log.set_filter log Event_log.No_filter;
       Shell_view.set_status shell "filter: not a txn id");
    Lwt.return_unit
  | Filter (Repl_command.Filter_table name) ->
    prompt := Repl_command.No_prompt;
    (match !db_ref with
     | None -> Shell_view.set_status shell "filter: no database open"
     | Some db ->
       (match Db.tree_of_table db name with
        | Some tree ->
          Event_log.set_filter log (Event_log.By_table { name; tree });
          Shell_view.set_status shell (Printf.sprintf "filter: tbl=%s" name)
        | None ->
          Shell_view.set_status shell (Printf.sprintf "filter: no such table '%s'" name)));
    Lwt.return_unit
  | Dot d -> dispatch_dot d
  | Sql stmts -> Lwt_list.iter_s run_sql stmts
;;
```

Add the `.dump` case to `dispatch_dot` (~line 124):

```ocaml
  | Dump path_opt ->
    let path = Option.value path_opt ~default:"sqlocaml-events.log" in
    (match Event_log.dump log path with
     | Ok n -> Shell_view.set_status shell (Printf.sprintf "dumped %d event(s) to %s" n path)
     | Error e -> Shell_view.set_status shell (Printf.sprintf "dump failed: %s" e));
    Lwt.return_unit
```

Replace `set_filter_prompt` and add `set_table_filter_prompt` (~line 169):

```ocaml
let set_filter_prompt () =
  prompt := Repl_command.Txn_prompt;
  Lwd.set input_var "";
  Focus.request shell_focus;
  Shell_view.set_status shell "filter: enter a txn id, then Enter (empty=clear)"
;;

let set_table_filter_prompt () =
  prompt := Repl_command.Table_prompt;
  Lwd.set input_var "";
  Focus.request shell_focus;
  Shell_view.set_status shell "filter: enter a table name, then Enter"
;;
```

In `shell_handle`, replace the two `filter_mode := false` / `!filter_mode`
references (~lines 181–189):

```ocaml
  | `Escape, _ ->
    if !prompt <> Repl_command.No_prompt
    then (
      prompt := Repl_command.No_prompt;
      Shell_view.set_status shell "filter: cancelled")
    else do_quit ();
    `Handled
  | `Tab, _ ->
    prompt := Repl_command.No_prompt;
    Focus.request monitor_focus;
    `Handled
```

In `monitor_handle` (~line 236), pass the new callback:

```ocaml
  | _ -> Monitor_view.handle_key log ~set_filter_prompt ~set_table_filter_prompt key
```

- [ ] **Step 5: Build the executable + run the component test**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune build`
Expected: PASS (whole tree compiles, incl. `sqlocaml_repl.exe`).

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test test/test_repl_components.exe`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add bin/repl/monitor_view.ml bin/repl/sqlocaml_repl.ml test/test_repl_components.ml
git commit -m "feat(#385): monitor table-filter key + .dump wiring in REPL"
```

---

## Task 7: Full verification + lint/format (pre-push gate)

**Files:** none (verification only), plus any fmt fixes.

- [ ] **Step 1: Full test suite**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev dune test`
Expected: all green. If a non-REPL test constructed a page event, fix its
constructor to include `tree = …`.

- [ ] **Step 2: ocamlformat check on every touched `.ml`/`.mli`**

Run for each file (example):

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev \
  ocamlformat --check lib/store/store_event.ml lib/store/store_event.mli \
  lib/db/db.ml lib/db/db.mli bin/repl/event_log.ml bin/repl/event_log.mli \
  bin/repl/repl_command.ml bin/repl/monitor_view.ml bin/repl/sqlocaml_repl.ml \
  test/test_store_event.ml test/test_db_event_385.ml test/test_repl_components.ml
```

Fix any file that fails via the stdout→host redirect (per CLAUDE.md):

```sh
tmp=$(mktemp) && podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev \
  ocamlformat <file> > "$tmp" && mv "$tmp" <file> && chmod 644 <file>
```

- [ ] **Step 3: merlint (mli docs, pp, nesting ≤4, etc.)**

Run: `podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev merlint`
Expected: 0 issues for the touched files. (Every new `val` in an `.mli` has a
`(** … *)` doc — verify `tree_of_table`, `tree_id_of`, `dump`, the `filter` type
all carry doc comments. The `filter` *type* has no abstract `t` so no `pp`
required, but `Event_log.t` already has `pp`.)

- [ ] **Step 4: Manual smoke (optional but recommended)**

Run the REPL against a file db and exercise the new features by hand:

```sh
podman run --rm -it -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev \
  dune exec bin/repl/sqlocaml_repl.exe -- /tmp/smoke.db
```

In the shell: `CREATE TABLE a(x);` then `INSERT INTO a VALUES (1);`, Tab to the
monitor, press `t`, Tab back, type `a`, Enter → header shows `tbl=a` and only
`a`'s page events. Then `.dump /tmp/ev.log` → status confirms the path. Esc to
quit.

- [ ] **Step 5: Commit any fmt fixes**

```bash
git add -A && git commit -m "style(#385): ocamlformat + merlint fixes"
```

---

## Self-review notes (already reconciled)

- **Spec coverage:** A1/A2/A3 → Tasks 1–2; B1 → Task 3; C1 → Task 4; C2 → Tasks
  5–6; C3 (`.dump`) → Tasks 4 (dump fn) + 5 (parse) + 6 (dispatch). Testing
  section → tests embedded per task + Task 7.
- **Type consistency:** `tree_id_of` (Store_event), `By_txn`/`By_table`/`No_filter`
  (Event_log.filter), `Filter_txn`/`Filter_table` + `No_prompt`/`Txn_prompt`/
  `Table_prompt` (Repl_command), `Dump of string option`, `tree_of_table` — names
  used identically across Tasks 1–6.
- **Cross-task build ordering:** adding the `tree` field breaks the whole
  `store` library + `repl_lib` at once. Tasks 1–2 must land together (store), and
  Tasks 4–6 together (repl_lib + exe). The per-task "intermediate green" notes
  give a stub path if you want a compiling commit mid-task; otherwise commit at
  the noted boundaries.
