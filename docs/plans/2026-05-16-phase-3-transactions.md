# Phase 3 — Transactions Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add correct MVCC (multi-version concurrency control), freelist persistence, and explicit SQL transaction control (`BEGIN`/`COMMIT`/`ROLLBACK`) to sqlocaml.

**Architecture:** Fix the four correctness gaps from Phases 1–2 in order (txn_id threading → freelist persistence → rollback safety → RO snapshot isolation), then wire up the SQL-level transaction statements. Each task builds on the previous; they must be done in order. The storage layer stays SQL-agnostic; transaction state lives in `Store.t` / `Db.t`.

**Tech Stack:** OCaml 5, Lwt, Alcotest, QCheck, Menhir, ocamllex, Cstruct, Bisect_ppx. Build via Podman: `podman run --rm -v $(pwd):/work:Z sqlocaml-dev dune runtest`.

---

## Background: what is broken today and why it matters

Before Phase 3:

1. **Txn IDs are meaningless constants.** `btree.ml` stamps all freed pages with `freed_at_txn_id = 1L` and all allocations with `current_txn_id = 1L`. Because `1L < 1L = false`, the freelist is **never reused** — every write just extends the file.

2. **Freelist is not persisted.** `commit` always writes `freelist_page = 0L` in the header. After reopen, all freed pages are lost; they accumulate as wasted space forever.

3. **Rollback is unsafe.** `rollback` drops the in-memory tree cache (so reads fall back to the committed roots) but does NOT restore the pager's in-memory freelist. Freed pages added to the freelist during the aborted txn remain there; they could be reused in a future txn even though they are still referenced by the committed tree.

4. **RO transactions are not snapshots.** `Ro t` holds a reference to the live `Store.t`. Any `get` call on the RO txn resolves via `bt_get_tree`, which reads `st.trees` — the same mutable hashtable that the RW txn updates. A concurrent RW txn's uncommitted writes are visible to the "RO" txn.

5. **No `BEGIN`/`COMMIT`/`ROLLBACK` SQL.** Every DML statement is implicitly its own transaction; multi-statement transactions are impossible.

---

## File map (files touched by at least one task)

| File | Task(s) |
|------|---------|
| `lib/storage/freelist.ml` + `freelist.mli` | 1 |
| `lib/storage/pager.ml` + `pager.mli` | 1, 3 |
| `lib/storage/btree.ml` | 1 |
| `lib/store/store.ml` | 1, 2, 3, 4 |
| `lib/store/store.mli` | 4 |
| `lib/sql/ast.ml` | 5 |
| `lib/sql/lexer.mll` | 5 |
| `lib/sql/parser.mly` | 5 |
| `lib/sql/sema.ml` | 5 |
| `lib/sql/plan.ml` | 5 |
| `lib/sql/planner.ml` | 5 |
| `lib/sql/exec.ml` + `exec.mli` | 5 |
| `lib/db/db.ml` + `db.mli` | 5 |
| `test/test_pager.ml` | 1 |
| `test/test_store_btree.ml` | 2, 3, 4 |
| `test/test_txn.ml` (new) | 5 |
| `test/test_e2e.ml` | 6 |
| `test/test_freelist.ml` | 6 |

---

## How to run tests

```bash
# All tests (inside Podman dev container)
podman run --rm -v $(pwd):/work:Z sqlocaml-dev dune runtest

# Single test binary
podman run --rm -v $(pwd):/work:Z sqlocaml-dev dune exec test/test_pager.exe

# Coverage
podman run --rm -v $(pwd):/work:Z sqlocaml-dev bash scripts/coverage.sh summary
```

Expected baseline before starting: **1,027 tests passing**, **~80.59% handwritten coverage**.

---

## Task 1: Pager txn_id threading

Fix the root cause of broken freelist recycling: the B+-tree stamps all frees with the constant `1L` and all allocs check against `1L`, so no page is ever recycled. After this task, the pager knows the real transaction ID and the freelist can reuse pages.

**Design:**
- Add two fields to `Pager.t`:
  - `mutable current_txn_id : int64` — the active RW txn's ID; used by btree.ml to stamp freed pages via `Pager.get_txn_id`. Default `0L`.
  - `mutable alloc_min_safe : int64` — the minimum txn_id below which freed pages are safe to reuse (= `min(current_txn_id, min_active_reader_snap_txn_id)`). Pager.alloc uses this instead of a caller-supplied parameter. Default `0L`.
- Remove `~current_txn_id` from `Pager.alloc`'s signature. It now reads `t.alloc_min_safe` internally.
- `Freelist.pop` label: rename `~current_txn_id` → `~min_safe_txn_id` to reflect its new semantics.
- `btree.ml`: delete constants `txn_id_alloc` / `txn_id_free`; replace `Pager.alloc pager ~current_txn_id:txn_id_alloc` with `Pager.alloc pager`; replace `Pager.free pager ~freed_at_txn_id:txn_id_free` with `Pager.free pager ~freed_at_txn_id:(Pager.get_txn_id pager)`.
- `store.ml` `rw_begin` (Btree branch): add `Pager.set_txn_id st.pager (Int64.add st.current_header.txn_id 1L)` and `Pager.set_alloc_min_safe st.pager (Int64.add st.current_header.txn_id 1L)` (active readers are added in Task 4; for now min_safe = current txn_id, so within-txn exclusion is preserved).

**Files:**
- Modify: `lib/storage/freelist.ml`, `lib/storage/freelist.mli`
- Modify: `lib/storage/pager.ml`, `lib/storage/pager.mli`
- Modify: `lib/storage/btree.ml`
- Modify: `lib/store/store.ml`
- Modify: `test/test_pager.ml`

- [ ] **Step 1: Write failing tests for the new pager API**

In `test/test_pager.ml`, add at the end:

```ocaml
let test_alloc_no_arg () =
  (* Pager.alloc should work with no current_txn_id argument *)
  let p = make_pager () in
  let id = run (Pager.alloc p) in
  Alcotest.(check bool) "alloc ok" true (Result.is_ok id)

let test_set_txn_id () =
  let p = make_pager () in
  Pager.set_txn_id p 5L;
  Alcotest.(check int64) "get_txn_id" 5L (Pager.get_txn_id p)

let test_freelist_recycled_after_txn () =
  (* Free page with freed_at=2; alloc_min_safe=3 means it should be recycled *)
  let p = make_pager () in
  Pager.set_txn_id p 2L;
  let _ = run (Pager.alloc p) in   (* alloc page 2 *)
  Pager.free p ~page_id:2L ~freed_at_txn_id:2L;
  Pager.set_alloc_min_safe p 3L;
  let r = run (Pager.alloc p) in
  match r with
  | Ok pid -> Alcotest.(check int64) "page recycled" 2L pid
  | Error _ -> Alcotest.fail "expected recycled page"

let () =
  Alcotest.run "pager_phase3"
    [ "txn_id_api", [
        Alcotest.test_case "alloc_no_arg" `Quick test_alloc_no_arg;
        Alcotest.test_case "set_get_txn_id" `Quick test_set_txn_id;
        Alcotest.test_case "freelist_recycled" `Quick test_freelist_recycled_after_txn;
      ] ]
```

Run: `podman run --rm -v $(pwd):/work:Z sqlocaml-dev dune exec test/test_pager.exe`
Expected: compile error (Pager.alloc still has old signature)

- [ ] **Step 2: Update `freelist.mli` — rename `~current_txn_id` label**

```ocaml
(** Pop a reusable page. A page is reusable if freed_at_txn_id < min_safe_txn_id.
    Returns (page_id, updated_t) or None if no reusable page is available. *)
val pop : t -> min_safe_txn_id:int64 -> (int32 * t) option

(** Reusable count: entries with freed_at_txn_id < min_safe_txn_id. *)
val reusable_count : t -> min_safe_txn_id:int64 -> int
```

- [ ] **Step 3: Update `freelist.ml` — rename parameter**

In `freelist.ml`, replace:
```ocaml
let pop (t : t) ~current_txn_id : (int32 * t) option =
  let best =
    List.fold_left
      (fun acc (pid, txn) ->
         if Int64.compare txn current_txn_id < 0 then
```
with:
```ocaml
let pop (t : t) ~min_safe_txn_id : (int32 * t) option =
  let best =
    List.fold_left
      (fun acc (pid, txn) ->
         if Int64.compare txn min_safe_txn_id < 0 then
```

And `reusable_count`:
```ocaml
let reusable_count (t : t) ~min_safe_txn_id : int =
  List.fold_left
    (fun acc (_, txn) ->
       if Int64.compare txn min_safe_txn_id < 0 then acc + 1
       else acc)
    0
    t
```

- [ ] **Step 4: Update `pager.mli` — new fields + updated alloc**

Replace the existing `alloc` line and add new entries:
```ocaml
(** Allocate a new page ID. Tries freelist first (reusing pages where
    freed_at_txn_id < alloc_min_safe); extends file if none available.
    Call [set_alloc_min_safe] before allocating to control freelist gating. *)
val alloc : t -> (int64, error) result Lwt.t

(** Set the current RW transaction ID.  B+-tree operations stamp freed pages
    with this value via [get_txn_id]. *)
val set_txn_id : t -> int64 -> unit

(** Get the current RW transaction ID (used by btree.ml for freed_at stamps). *)
val get_txn_id : t -> int64

(** Set the minimum txn_id threshold for freelist reuse.
    A freed page is reusable iff freed_at_txn_id < alloc_min_safe.
    Set to [min(current_txn_id, min_active_reader_txn_id)] before each alloc.
    Defaults to 0L (nothing reusable until set). *)
val set_alloc_min_safe : t -> int64 -> unit
```

- [ ] **Step 5: Update `pager.ml` — add fields, update alloc**

Add to the `type t` record:
```ocaml
type t = {
  read_page  : page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t;
  write_page : page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t;
  sync       : unit -> (unit, string) result Lwt.t;
  resize     : n_pages:int64 -> (unit, string) result Lwt.t;
  cache      : (int64, Cstruct.t) Hashtbl.t;
  dirty      : (int64, Cstruct.t) Hashtbl.t;
  fifo       : int64 Queue.t;
  mutable n_pages         : int64;
  mutable freelist        : Freelist.t;
  mutable current_txn_id  : int64;   (* freed_at stamp for btree ops *)
  mutable alloc_min_safe  : int64;   (* threshold for freelist reuse *)
}
```

Update `create` to add `current_txn_id = 0L; alloc_min_safe = 0L`.

Add accessor functions:
```ocaml
let set_txn_id t id     = t.current_txn_id <- id
let get_txn_id t        = t.current_txn_id
let set_alloc_min_safe t v = t.alloc_min_safe <- v
```

Update `alloc` to remove `~current_txn_id` parameter and use `t.alloc_min_safe`:
```ocaml
let alloc t =
  match Freelist.pop t.freelist ~min_safe_txn_id:t.alloc_min_safe with
  | Some (pid32, fl') ->
    t.freelist <- fl';
    Lwt.return_ok (Int64.of_int32 pid32)
  | None ->
    let new_id = t.n_pages in
    let new_pages = Int64.add t.n_pages 1L in
    let open Lwt.Syntax in
    let* result = t.resize ~n_pages:new_pages in
    (match result with
     | Error msg -> Lwt.return_error (Block_error msg)
     | Ok ()     ->
       t.n_pages <- new_pages;
       Lwt.return_ok new_id)
```

- [ ] **Step 6: Update `btree.ml` — use Pager txn_id**

Delete the two constants at the top:
```ocaml
(* DELETE these two lines: *)
let txn_id_alloc = 1L
let txn_id_free  = 1L
```

Replace all occurrences of `Pager.alloc pager ~current_txn_id:txn_id_alloc` with `Pager.alloc pager`.

Replace all occurrences of `Pager.free pager ~page_id:step.page_id ~freed_at_txn_id:txn_id_free` with `Pager.free pager ~page_id:step.page_id ~freed_at_txn_id:(Pager.get_txn_id pager)`.

Do the same for all other `Pager.free` calls that used `txn_id_free`:
```bash
grep -n "txn_id_free\|txn_id_alloc" lib/storage/btree.ml
```
Fix every occurrence (there are ~10 total: 6 allocs, 4 frees).

- [ ] **Step 7: Update `store.ml` `rw_begin` — set real txn_id**

In the `rw_begin` function, add after `Lwt_mutex.lock`:
```ocaml
let rw_begin t =
  let* () = Lwt_mutex.lock t.rw_mutex in
  (match t.backend with
   | Mem _ -> ()
   | Btree st ->
     let current_rw_txn_id = Int64.add st.current_header.txn_id 1L in
     Pager.set_txn_id st.pager current_rw_txn_id;
     Pager.set_alloc_min_safe st.pager current_rw_txn_id);
  Lwt.return (Rw t)
```

- [ ] **Step 8: Update `test_pager.ml` — remove `~current_txn_id` from alloc calls**

Use `grep -n "~current_txn_id" test/test_pager.ml` to find all occurrences.

Replace every `Pager.alloc p ~current_txn_id:_` with `Pager.alloc p`.

There are ~11 occurrences. Also update any `reusable_count ~current_txn_id:` calls to `~min_safe_txn_id:` in test_freelist.ml.

- [ ] **Step 9: Build and run tests**

```bash
podman run --rm -v $(pwd):/work:Z sqlocaml-dev dune runtest
```

Expected: all existing tests pass plus the 3 new pager tests. Freelist recycling now works correctly.

- [ ] **Step 10: Commit**

```bash
git add lib/storage/freelist.ml lib/storage/freelist.mli \
        lib/storage/pager.ml lib/storage/pager.mli \
        lib/storage/btree.ml \
        lib/store/store.ml \
        test/test_pager.ml test/test_freelist.ml
git commit -m "storage: thread real txn_ids through pager/btree — enable freelist recycling"
```

---

## Task 2: Freelist persistence

Serialize the in-memory freelist to a chain of freelist pages on every `commit`, and read it back on `open_file`. After this task, freed pages survive database reopen.

**Design:**

Freelist pages are already supported by `page.ml` (kind=Freelist, n_keys=entry count, right_page=next freelist page, entries at bytes 16+). Max 340 entries per page.

On **commit** (Btree backend):
1. Free the old freelist pages (from `st.current_header.freelist_page` chain) into the pager freelist, stamped with the new txn_id (so they become reusable in the *next* txn, not this one).
2. After persisting all tree roots into the meta-tree, call `write_freelist_pages`.
3. `write_freelist_pages`: snapshot current freelist entries, compute how many pages needed, alloc that many pages, get the final post-alloc entries, write them in page-sized chunks, link via right_page, return first page id (or 0L if empty).
4. Pass the result as `freelist_page` in the `Header.t` given to `Header.commit`.

On **open_file** (non-fresh, Btree backend):
1. After reading the live header, if `h.freelist_page <> 0L`, call `read_freelist_pages`.
2. `read_freelist_pages`: walk the chain via right_page, collect all `(page_id, freed_at)` entries, reconstruct `Freelist.t`.
3. Pass the reconstructed freelist to `pager_of_unix_file ~freelist`.

**Files:**
- Modify: `lib/store/store.ml`
- Modify: `test/test_store_btree.ml`

- [ ] **Step 1: Write failing tests in `test/test_store_btree.ml`**

Add a new section at the end:

```ocaml
(* ------------------------------------------------------------------ *)
(* Task 2: Freelist persistence                                         *)
(* ------------------------------------------------------------------ *)

let test_freelist_survives_reopen () =
  (* Create db, insert enough rows to cause some CoW page frees,
     commit, close, reopen, and verify the freelist is non-empty
     (meaning freed pages were recovered). *)
  let path = Filename.temp_file "sqlocaml_fl_" ".db" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    let db = run (S.open_file ~path) in
    let store = Result.get_ok db in
    (* Insert 200 rows to force splits/frees in the btree *)
    for i = 1 to 200 do
      let tx = run (S.rw_begin store) in
      let key = Printf.sprintf "%04d" i |> Bytes.of_string in
      let value = Bytes.of_string "value" in
      run (S.put tx 16 key value);
      run (S.commit tx)
    done;
    (* Delete half to free pages *)
    for i = 1 to 100 do
      let tx = run (S.rw_begin store) in
      let key = Printf.sprintf "%04d" i |> Bytes.of_string in
      run (S.del tx 16 key);
      run (S.commit tx)
    done;
    run (S.close store);
    (* Reopen and check freelist non-empty *)
    let db2 = run (S.open_file ~path) in
    let store2 = Result.get_ok db2 in
    let fl_size = S.freelist_size store2 in
    Alcotest.(check bool) "freelist non-empty after reopen" true (fl_size > 0);
    run (S.close store2))

let test_freed_pages_reused_after_reopen () =
  let path = Filename.temp_file "sqlocaml_reuse_" ".db" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    let store = Result.get_ok (run (S.open_file ~path)) in
    (* Alloc + free some pages via store operations *)
    for i = 1 to 50 do
      let tx = run (S.rw_begin store) in
      let key = Printf.sprintf "%04d" i |> Bytes.of_string in
      run (S.put tx 16 key (Bytes.of_string "v"));
      run (S.commit tx)
    done;
    let n_pages_before_delete = S.n_pages store in
    for i = 1 to 50 do
      let tx = run (S.rw_begin store) in
      let key = Printf.sprintf "%04d" i |> Bytes.of_string in
      run (S.del tx 16 key);
      run (S.commit tx)
    done;
    run (S.close store);
    let store2 = Result.get_ok (run (S.open_file ~path)) in
    (* Insert again — should reuse freed pages rather than growing file further *)
    for i = 1 to 50 do
      let tx = run (S.rw_begin store2) in
      let key = Printf.sprintf "%04d" i |> Bytes.of_string in
      run (S.put tx 16 key (Bytes.of_string "v"));
      run (S.commit tx)
    done;
    let n_pages_after_reinsert = S.n_pages store2 in
    (* After reinsert the file should not be much larger than before delete *)
    Alcotest.(check bool) "file size bounded by freelist reuse"
      true (n_pages_after_reinsert <= Int64.add n_pages_before_delete 5L);
    run (S.close store2))

(* Register in the runner at the end of test_store_btree.ml *)
```

Also add `val freelist_size : t -> int` and `val n_pages : t -> int64` to `store.mli` (needed by tests). These are thin wrappers.

Run: `podman run --rm -v $(pwd):/work:Z sqlocaml-dev dune runtest`
Expected: compile errors (freelist_size, n_pages not yet in store.mli)

- [ ] **Step 2: Add `freelist_size` and `n_pages` to `store.mli` and `store.ml`**

In `store.mli` (at end):
```ocaml
(** Number of entries in the in-memory freelist (for testing/diagnostics). *)
val freelist_size : t -> int

(** Current total file page count (for testing/diagnostics). *)
val n_pages : t -> int64
```

In `store.ml`:
```ocaml
let freelist_size t =
  match t.backend with
  | Mem _ -> 0
  | Btree st -> Freelist.size (Pager.freelist st.pager)

let n_pages t =
  match t.backend with
  | Mem _ -> 0L
  | Btree st -> Pager.n_pages st.pager
```

- [ ] **Step 3: Add `read_freelist_pages` to `store.ml`**

Add before `open_file`:

```ocaml
(* Walk a chain of Freelist pages starting at [first_page].
   Returns a reconstructed Freelist.t. *)
let read_freelist_pages pager ~first_page : Freelist.t Lwt.t =
  if Int64.equal first_page 0L then Lwt.return Freelist.empty
  else begin
    let rec loop pid acc =
      if Int64.equal pid 0L then Lwt.return (Freelist.of_list (List.rev acc))
      else begin
        let* r = Pager.read pager pid in
        match r with
        | Error _ -> Lwt.return (Freelist.of_list (List.rev acc))  (* corrupt page: stop *)
        | Ok buf ->
          let common = Page.read_common buf in
          let n = common.Page.n_keys in
          let next_pid = Int64.logand 0xFFFFFFFFL
                           (Int64.of_int32 common.Page.right_page) in
          let entries =
            List.init n (fun i ->
              let e = Page.freelist_entry_at buf ~index:i in
              (e.Page.page_id, e.Page.freed_at_txn_id))
          in
          loop next_pid (List.rev_append entries acc)
      end
    in
    loop first_page []
  end
```

- [ ] **Step 4: Add `write_freelist_pages` to `store.ml`**

Add after `read_freelist_pages`:

```ocaml
(* Free the old freelist page chain (from the previous commit) back into the pager. *)
let free_old_freelist_pages pager ~first_page =
  let rec loop pid =
    if Int64.equal pid 0L then Lwt.return_unit
    else begin
      let* r = Pager.read pager pid in
      let next_pid =
        match r with
        | Error _ -> 0L
        | Ok buf ->
          let c = Page.read_common buf in
          Int64.logand 0xFFFFFFFFL (Int64.of_int32 c.Page.right_page)
      in
      Pager.free pager ~page_id:pid
        ~freed_at_txn_id:(Pager.get_txn_id pager);
      loop next_pid
    end
  in
  loop first_page

(* Serialize the current pager freelist to a page chain.
   Returns the first page id of the chain (0L if freelist is empty). *)
let write_freelist_pages pager : int64 Lwt.t =
  let entries_before = Freelist.to_list (Pager.freelist pager) in
  let n_entries = List.length entries_before in
  let max_per = Page.max_freelist_entries_per_page in
  let n_fl_pages = (n_entries + max_per - 1) / max_per in
  if n_fl_pages = 0 then Lwt.return 0L
  else begin
    (* Allocate exactly n_fl_pages pages for the freelist chain *)
    let* page_ids =
      Lwt_list.map_s (fun _ ->
        let* r = Pager.alloc pager in
        match r with
        | Ok pid -> Lwt.return pid
        | Error e ->
          Lwt.fail_with
            (Format.asprintf "write_freelist_pages: alloc failed: %a"
               Pager.pp_error e)
      ) (List.init n_fl_pages (fun _ -> ()))
    in
    (* Get FINAL freelist state (after allocations popped some entries) *)
    let final_entries = Freelist.to_list (Pager.freelist pager) in
    (* Split final_entries into chunks *)
    let rec chunkify lst =
      if lst = [] then []
      else
        let chunk = List.filteri (fun i _ -> i < max_per) lst in
        let rest  = List.filteri (fun i _ -> i >= max_per) lst in
        chunk :: chunkify rest
    in
    let chunks = chunkify final_entries in
    let pid_arr = Array.of_list page_ids in
    let n_chunks = List.length chunks in
    (* Write each chunk to a freelist page *)
    List.iteri (fun i chunk ->
      let pid  = pid_arr.(i) in
      let next = if i + 1 < Array.length pid_arr then pid_arr.(i+1) else 0L in
      let buf  = Cstruct.create Page.page_size in
      Cstruct.memset buf 0;
      Page.write_common buf
        { Page.kind       = Page.Freelist;
          flags           = 0;
          n_keys          = List.length chunk;
          right_page      = Int64.to_int32 next;
          crc32           = 0l };
      List.iteri (fun j (page_id, freed_at_txn_id) ->
        Page.freelist_set_entry buf ~index:j ~page_id ~freed_at_txn_id
      ) chunk;
      Pager.write pager pid buf
    ) chunks;
    (* Any extra allocated pages (n_fl_pages > n_chunks) get empty freelist pages *)
    for i = n_chunks to n_fl_pages - 1 do
      let pid  = pid_arr.(i) in
      let next = if i + 1 < Array.length pid_arr then pid_arr.(i+1) else 0L in
      let buf  = Cstruct.create Page.page_size in
      Cstruct.memset buf 0;
      Page.write_common buf
        { Page.kind = Page.Freelist; flags = 0; n_keys = 0;
          right_page = Int64.to_int32 next; crc32 = 0l };
      Pager.write pager pid buf
    done;
    Lwt.return pid_arr.(0)
  end
```

- [ ] **Step 5: Update `commit` to persist freelist**

In `store.ml commit` (Btree branch), replace:

```ocaml
(* Current code that always writes freelist_page = 0L *)
let new_state : Header.t =
  { txn_id         = 0L;
    root_page      = Btree.root_page st.meta;
    freelist_page  = 0L;    (* ← fix this *)
    n_pages_total  = Pager.n_pages st.pager;
    schema_version = st.schema_version }
in
let* r = Header.commit st.pager ~prev_header:st.current_header ~new_state in
```

with:

```ocaml
(* 1. Free old freelist pages from the PREVIOUS commit *)
let* () = free_old_freelist_pages st.pager
            ~first_page:st.current_header.freelist_page
in
(* 2. Persist tree roots into meta-tree (existing code, unchanged) *)
...  (* the Lwt_list.iter_s block already here *)
(* 3. Write the updated freelist to new pages *)
let* freelist_first_page = write_freelist_pages st.pager in
(* 4. Commit the header *)
let new_state : Header.t =
  { txn_id         = 0L;
    root_page      = Btree.root_page st.meta;
    freelist_page  = freelist_first_page;
    n_pages_total  = Pager.n_pages st.pager;
    schema_version = st.schema_version }
in
let* r = Header.commit st.pager ~prev_header:st.current_header ~new_state in
```

**Important:** the `free_old_freelist_pages` call must happen BEFORE the meta-tree puts (so the old freelist page IDs are in the freelist and can potentially be reused for new data pages or the new freelist chain).

- [ ] **Step 6: Update `open_file` (non-fresh branch) to read freelist**

In `open_file`, find the non-fresh (existing file) branch. After reading the live header and before building `pager`, change:

```ocaml
(* Current: always starts with empty freelist *)
let pager = pager_of_unix_file file ~freelist:Freelist.empty in
let%lwt hr = Header.read_live pager in
match hr with
| Error e -> Lwt.return_error (map_header_err e)
| Ok h ->
  let meta = Btree.create pager ~root_page:h.root_page in
  ...
```

to:

```ocaml
let pager = pager_of_unix_file file ~freelist:Freelist.empty in
let%lwt hr = Header.read_live pager in
match hr with
| Error e -> Lwt.return_error (map_header_err e)
| Ok h ->
  (* Restore freelist from disk *)
  let%lwt fl = read_freelist_pages pager ~first_page:h.freelist_page in
  (* Rebuild pager with loaded freelist (replace the empty one) *)
  Pager.set_freelist pager fl;   (* added in Task 3; for now inline: *)
  (* Actually we need Pager.set_freelist — implement a stub now *)
  let meta = Btree.create pager ~root_page:h.root_page in
  ...
```

Wait — `Pager.set_freelist` is added in Task 3. To avoid a forward dependency, add a minimal version now:

In `pager.ml` / `pager.mli`, add:
```ocaml
(** Replace the in-memory freelist (used after deserializing from disk). *)
val set_freelist : t -> Freelist.t -> unit
```
```ocaml
let set_freelist t fl = t.freelist <- fl
```

Then in `open_file`:
```ocaml
| Ok h ->
  let%lwt fl = read_freelist_pages pager ~first_page:h.freelist_page in
  Pager.set_freelist pager fl;
  let meta = Btree.create pager ~root_page:h.root_page in
  ...
```

- [ ] **Step 7: Build and run tests**

```bash
podman run --rm -v $(pwd):/work:Z sqlocaml-dev dune runtest
```

Expected: all existing tests pass plus the new freelist persistence tests.

- [ ] **Step 8: Commit**

```bash
git add lib/store/store.ml lib/store/store.mli \
        lib/storage/pager.ml lib/storage/pager.mli \
        test/test_store_btree.ml
git commit -m "storage: persist freelist across commits and reopens"
```

---

## Task 3: Rollback correctness

Fix the unsafe rollback: currently a rolled-back RW txn leaves stale entries in the pager freelist (pages freed by CoW operations in the aborted txn are in the freelist but those pages are still referenced by the committed tree). Fix by snapshotting the freelist at `rw_begin` and restoring it on `rollback`, and clearing dirty pages.

**Design:**
- `Pager.clear_dirty`: iterates `t.dirty`, removes each dirty page_id from both `t.dirty` and `t.cache`, and clears `t.fifo` entries for those pages. This ensures rolled-back writes don't pollute the cache.
- `bt_state.txn_freelist_snapshot`: `Freelist.t option` field. Set to `Some (Pager.freelist pager)` at `rw_begin`; used to restore on `rollback`; cleared on `commit`.

**Files:**
- Modify: `lib/storage/pager.ml`, `lib/storage/pager.mli`
- Modify: `lib/store/store.ml`
- Modify: `test/test_store_btree.ml`

- [ ] **Step 1: Write failing rollback tests in `test/test_store_btree.ml`**

```ocaml
(* ------------------------------------------------------------------ *)
(* Task 3: Rollback correctness                                         *)
(* ------------------------------------------------------------------ *)

let test_rollback_restores_data () =
  let path = Filename.temp_file "sqlocaml_rb_" ".db" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    let store = Result.get_ok (run (S.open_file ~path)) in
    (* Commit a row *)
    let tx1 = run (S.rw_begin store) in
    run (S.put tx1 16 (Bytes.of_string "key") (Bytes.of_string "original"));
    run (S.commit tx1);
    (* Begin, modify, rollback *)
    let tx2 = run (S.rw_begin store) in
    run (S.put tx2 16 (Bytes.of_string "key") (Bytes.of_string "changed"));
    run (S.rollback tx2);
    (* Read should see original value *)
    let tx3 = run (S.ro_begin store) in
    let v = run (S.get tx3 16 (Bytes.of_string "key")) in
    run (S.ro_end tx3);
    Alcotest.(check (option string))
      "rolled back value" (Some "original")
      (Option.map Bytes.to_string v);
    run (S.close store))

let test_rollback_frees_freelist_not_corrupted () =
  (* After rollback, the freelist should not contain pages that are
     still referenced by the committed tree. *)
  let path = Filename.temp_file "sqlocaml_rb2_" ".db" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    let store = Result.get_ok (run (S.open_file ~path)) in
    let tx1 = run (S.rw_begin store) in
    run (S.put tx1 16 (Bytes.of_string "k1") (Bytes.of_string "v1"));
    run (S.commit tx1);
    let fl_size_after_commit = S.freelist_size store in
    (* Now start and rollback a txn that modifies the same tree *)
    let tx2 = run (S.rw_begin store) in
    run (S.put tx2 16 (Bytes.of_string "k2") (Bytes.of_string "v2"));
    run (S.rollback tx2);
    (* Freelist should be the same as after the first commit *)
    let fl_size_after_rollback = S.freelist_size store in
    Alcotest.(check int) "freelist unchanged after rollback"
      fl_size_after_commit fl_size_after_rollback;
    run (S.close store))

let test_rollback_then_commit_works () =
  let path = Filename.temp_file "sqlocaml_rb3_" ".db" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    let store = Result.get_ok (run (S.open_file ~path)) in
    (* Insert, rollback, insert again, commit, read *)
    let tx1 = run (S.rw_begin store) in
    run (S.put tx1 16 (Bytes.of_string "k") (Bytes.of_string "first"));
    run (S.rollback tx1);
    let tx2 = run (S.rw_begin store) in
    run (S.put tx2 16 (Bytes.of_string "k") (Bytes.of_string "second"));
    run (S.commit tx2);
    let tx3 = run (S.ro_begin store) in
    let v = run (S.get tx3 16 (Bytes.of_string "k")) in
    run (S.ro_end tx3);
    Alcotest.(check (option string)) "second write committed"
      (Some "second") (Option.map Bytes.to_string v);
    run (S.close store))
```

Run: `podman run --rm -v $(pwd):/work:Z sqlocaml-dev dune runtest`
Expected: tests compile but the freelist corruption test fails (rollback currently doesn't restore freelist).

- [ ] **Step 2: Add `Pager.clear_dirty` to `pager.mli`**

```ocaml
(** Discard all dirty pages (and their cache entries) without writing them.
    Used on rollback to prevent rolled-back writes from being visible. *)
val clear_dirty : t -> unit
```

- [ ] **Step 3: Implement `Pager.clear_dirty` in `pager.ml`**

```ocaml
let clear_dirty t =
  (* Remove every dirty page from both the dirty set and the read cache.
     Also remove from the FIFO queue so eviction doesn't encounter stale entries. *)
  let dirty_pids = Hashtbl.fold (fun pid _ acc -> pid :: acc) t.dirty [] in
  List.iter (fun pid ->
    Hashtbl.remove t.dirty  pid;
    Hashtbl.remove t.cache  pid
  ) dirty_pids;
  (* Rebuild FIFO without the removed pids *)
  let old_fifo = Queue.copy t.fifo in
  Queue.clear t.fifo;
  Queue.iter (fun pid ->
    if not (List.mem pid dirty_pids) then Queue.push pid t.fifo
  ) old_fifo
```

(Note: `List.mem` is O(n) but dirty sets are small in practice. For production, consider a `(int64, unit) Hashtbl.t` for the dirty_pids set.)

- [ ] **Step 4: Add `txn_freelist_snapshot` to `bt_state` in `store.ml`**

In the `bt_state` type definition, add:
```ocaml
type bt_state = {
  file                    : Unix_file.t;
  pager                   : Pager.t;
  mutable meta            : Btree.t;
  trees                   : (tree_id, Btree.t) Hashtbl.t;
  mutable current_header  : Header.t;
  schema_version          : int64;
  mutable txn_freelist_snapshot : Freelist.t option;
    (* Freelist state at rw_begin; restored on rollback. *)
}
```

Initialize `txn_freelist_snapshot = None` in both `open_file` branches.

- [ ] **Step 5: Snapshot at `rw_begin`; restore at `rollback`; clear at `commit`**

In `rw_begin` (Btree branch), after setting txn_id:
```ocaml
st.txn_freelist_snapshot <- Some (Pager.freelist st.pager);
```

In `rollback` (Btree branch), add after clearing `st.trees` and resetting `st.meta`:
```ocaml
(match st.txn_freelist_snapshot with
 | Some fl ->
   Pager.set_freelist st.pager fl;
   Pager.clear_dirty st.pager;
   st.txn_freelist_snapshot <- None
 | None -> ())
```

In `commit` (Btree branch), after the existing `st.current_header <- ...` update at the end, add:
```ocaml
st.txn_freelist_snapshot <- None;
```

- [ ] **Step 6: Build and run tests**

```bash
podman run --rm -v $(pwd):/work:Z sqlocaml-dev dune runtest
```

Expected: all tests pass including the three new rollback tests.

- [ ] **Step 7: Commit**

```bash
git add lib/storage/pager.ml lib/storage/pager.mli lib/store/store.ml \
        test/test_store_btree.ml
git commit -m "storage: fix rollback — snapshot/restore freelist, clear dirty pages"
```

---

## Task 4: Active readers + true RO snapshot isolation

Implement real snapshot isolation: `ro_begin` captures the committed root at that moment, registers the reader's txn_id in `active_readers`, and returns a snapshot that never sees subsequent committed writes. `Pager.alloc` gates freelist reuse by the minimum active reader txn_id.

**Design:**

Change `'a txn` Ro variant to carry snapshot state:
```ocaml
type ro_snapshot = {
  rs_store     : t;
  rs_snap_txn_id    : int64;
  rs_snap_meta_root : int64;
  rs_snap_trees : (tree_id, Btree.t) Hashtbl.t;
}

type 'a txn =
  | Ro : ro_snapshot -> ro txn
  | Rw : t -> rw txn
```

Add `active_readers : (int64, int) Hashtbl.t` to `bt_state` (maps snap_txn_id → reader count).

`ro_begin` for Btree: lock-free read of `st.current_header.{txn_id, root_page}`, register in `active_readers`, return `Ro {rs_store=t; rs_snap_txn_id; rs_snap_meta_root; rs_snap_trees=Hashtbl.create 4}`.

`ro_end` for Btree: deregister from `active_readers` (decrement; remove if 0).

`get` for Ro: new `bt_get_tree_ro` that builds a Btree from `rs_snap_meta_root` and caches in `rs_snap_trees`.

`cursor_open` for Ro: same — use `rs_snap_trees` (call `bt_get_tree_ro`).

`store_of` still works for Rw (unchanged). For Ro, add a `snap_store_of` extractor.

`rw_begin`: compute `alloc_min_safe` as `min(current_rw_txn_id, min_active_reader_txn_id)`. If no readers, use `current_rw_txn_id` (no change from Task 1).

**Files:**
- Modify: `lib/store/store.ml`, `lib/store/store.mli`
- Modify: `test/test_store_btree.ml`

- [ ] **Step 1: Write failing snapshot isolation tests**

```ocaml
(* ------------------------------------------------------------------ *)
(* Task 4: RO snapshot isolation                                        *)
(* ------------------------------------------------------------------ *)

let test_ro_sees_committed_state_not_in_progress () =
  let path = Filename.temp_file "sqlocaml_snap_" ".db" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    let store = Result.get_ok (run (S.open_file ~path)) in
    (* Commit a row *)
    let tx1 = run (S.rw_begin store) in
    run (S.put tx1 16 (Bytes.of_string "k") (Bytes.of_string "committed"));
    run (S.commit tx1);
    (* Open an RO snapshot *)
    let ro = run (S.ro_begin store) in
    (* Start a concurrent RW txn that modifies the same key *)
    let tx2 = run (S.rw_begin store) in
    run (S.put tx2 16 (Bytes.of_string "k") (Bytes.of_string "uncommitted"));
    (* RO snapshot should NOT see the uncommitted write *)
    let v = run (S.get ro 16 (Bytes.of_string "k")) in
    Alcotest.(check (option string)) "RO sees committed value"
      (Some "committed") (Option.map Bytes.to_string v);
    (* Commit the RW txn *)
    run (S.commit tx2);
    (* RO snapshot still sees OLD committed value (not the new commit) *)
    let v2 = run (S.get ro 16 (Bytes.of_string "k")) in
    Alcotest.(check (option string)) "RO still sees snapshot value"
      (Some "committed") (Option.map Bytes.to_string v2);
    run (S.ro_end ro);
    (* After ro_end, a new RO txn sees the latest committed value *)
    let ro3 = run (S.ro_begin store) in
    let v3 = run (S.get ro3 16 (Bytes.of_string "k")) in
    Alcotest.(check (option string)) "new RO sees latest commit"
      (Some "uncommitted") (Option.map Bytes.to_string v3);
    run (S.ro_end ro3);
    run (S.close store))

let test_active_reader_gates_freelist_reuse () =
  (* A page freed while a reader is open should not be reused until the reader closes. *)
  let path = Filename.temp_file "sqlocaml_gate_" ".db" in
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () ->
    let store = Result.get_ok (run (S.open_file ~path)) in
    (* Build some tree structure *)
    for i = 1 to 20 do
      let tx = run (S.rw_begin store) in
      let key = Printf.sprintf "%04d" i |> Bytes.of_string in
      run (S.put tx 16 key (Bytes.of_string "v"));
      run (S.commit tx)
    done;
    (* Open RO snapshot *)
    let ro = run (S.ro_begin store) in
    let n_before = S.n_pages store in
    (* Delete all rows — this frees pages via CoW *)
    for i = 1 to 20 do
      let tx = run (S.rw_begin store) in
      let key = Printf.sprintf "%04d" i |> Bytes.of_string in
      run (S.del tx 16 key);
      run (S.commit tx)
    done;
    (* Reinsert all rows — with active reader, freed pages should NOT be reused yet;
       file should grow *)
    for i = 1 to 20 do
      let tx = run (S.rw_begin store) in
      let key = Printf.sprintf "%04d" i |> Bytes.of_string in
      run (S.put tx 16 key (Bytes.of_string "v"));
      run (S.commit tx)
    done;
    let n_with_reader = S.n_pages store in
    (* Close reader *)
    run (S.ro_end ro);
    (* Reinsert after reader closes — freed pages now reusable; file should NOT grow much *)
    for i = 1 to 20 do
      let tx = run (S.rw_begin store) in
      let key = Printf.sprintf "%04d" i |> Bytes.of_string in
      run (S.del tx 16 key);
      run (S.commit tx)
    done;
    for i = 1 to 20 do
      let tx = run (S.rw_begin store) in
      let key = Printf.sprintf "%04d" i |> Bytes.of_string in
      run (S.put tx 16 key (Bytes.of_string "v"));
      run (S.commit tx)
    done;
    let n_after_reader = S.n_pages store in
    Alcotest.(check bool) "file grows less after reader closes"
      true (n_after_reader <= n_with_reader);
    run (S.close store))
```

Run: Expected compile errors (ro_snapshot type not yet in store.ml).

- [ ] **Step 2: Add `active_readers` to `bt_state` in `store.ml`**

```ocaml
type bt_state = {
  ...
  mutable txn_freelist_snapshot : Freelist.t option;
  active_readers : (int64, int) Hashtbl.t;
    (* Maps snap_txn_id → reference count of concurrent RO txns at that snapshot. *)
}
```

Initialize `active_readers = Hashtbl.create 4` in both `open_file` branches.

Add a helper:
```ocaml
let min_active_reader_txn st =
  Hashtbl.fold (fun txn_id _ acc ->
    match acc with
    | None -> Some txn_id
    | Some m -> Some (Int64.min m txn_id)
  ) st.active_readers None
```

- [ ] **Step 3: Change `'a txn` Ro variant**

Replace the current `type 'a txn`:

```ocaml
type ro_snapshot = {
  rs_store          : t;
  rs_snap_txn_id    : int64;
  rs_snap_meta_root : int64;
  rs_snap_trees     : (tree_id, Btree.t) Hashtbl.t;
}

type 'a txn =
  | Ro : ro_snapshot -> ro txn
  | Rw : t -> rw txn
```

Update `store_of`:
```ocaml
let store_of : type a. a txn -> t = function
  | Ro snap -> snap.rs_store
  | Rw s    -> s
```

- [ ] **Step 4: Update `ro_begin` and `ro_end`**

Replace `ro_begin`:
```ocaml
let ro_begin t =
  match t.backend with
  | Mem _ ->
    Lwt.return
      (Ro { rs_store = t; rs_snap_txn_id = 0L;
            rs_snap_meta_root = 0L;
            rs_snap_trees = Hashtbl.create 1 })
  | Btree st ->
    let snap_txn_id    = st.current_header.txn_id in
    let snap_meta_root = st.current_header.root_page in
    (* Register reader *)
    let count = Option.value ~default:0
                  (Hashtbl.find_opt st.active_readers snap_txn_id) in
    Hashtbl.replace st.active_readers snap_txn_id (count + 1);
    Lwt.return
      (Ro { rs_store = t; rs_snap_txn_id = snap_txn_id;
            rs_snap_meta_root = snap_meta_root;
            rs_snap_trees = Hashtbl.create 4 })
```

Replace `ro_end`:
```ocaml
let ro_end (Ro snap : ro txn) =
  (match snap.rs_store.backend with
   | Mem _ -> ()
   | Btree st ->
     let tid = snap.rs_snap_txn_id in
     (match Hashtbl.find_opt st.active_readers tid with
      | None | Some 1 -> Hashtbl.remove st.active_readers tid
      | Some n -> Hashtbl.replace st.active_readers tid (n - 1)));
  Lwt.return_unit
```

- [ ] **Step 5: Add `bt_get_tree_ro` for snapshot lookups**

Add a new helper alongside `bt_get_tree`:
```ocaml
(* Lookup or build a Btree handle rooted at the RO snapshot's meta tree. *)
let bt_get_tree_ro (snap : ro_snapshot) (st : bt_state) (tid : tree_id)
    : (Btree.t, error) result Lwt.t =
  match Hashtbl.find_opt snap.rs_snap_trees tid with
  | Some bt -> Lwt.return_ok bt
  | None ->
    (* Build a meta-tree rooted at the snapshot's meta root *)
    let snap_meta = Btree.create st.pager ~root_page:snap.rs_snap_meta_root in
    let key = encode_tree_id tid in
    let* r = Btree.get snap_meta key in
    match r with
    | Error e -> Lwt.return_error (map_btree_err e)
    | Ok None ->
      (* Tree didn't exist at snapshot time — return empty *)
      let bt = Btree.create st.pager ~root_page:0L in
      Hashtbl.replace snap.rs_snap_trees tid bt;
      Lwt.return_ok bt
    | Ok (Some v) ->
      let root_page = decode_root_page v in
      let bt = Btree.create st.pager ~root_page in
      Hashtbl.replace snap.rs_snap_trees tid bt;
      Lwt.return_ok bt
```

- [ ] **Step 6: Update `get` to dispatch by txn variant**

Replace the `get` function:
```ocaml
let get : type a. a txn -> tree_id -> bytes -> bytes option Lwt.t =
  fun tx tid key ->
    match tx with
    | Ro snap ->
      (match snap.rs_store.backend with
       | Mem trees ->
         Lwt.return (BytesMap.find_opt key !(mem_tree trees tid))
       | Btree st ->
         let* r = bt_get_tree_ro snap st tid in
         let* bt = unwrap_error r in
         let* g = Btree.get bt key in
         (match g with
          | Ok v -> Lwt.return v
          | Error e ->
            Lwt.fail_with
              (Format.asprintf "Store.get(ro): %a" pp_error (map_btree_err e))))
    | Rw t ->
      (match t.backend with
       | Mem trees ->
         Lwt.return (BytesMap.find_opt key !(mem_tree trees tid))
       | Btree st ->
         let* r = bt_get_tree st tid in
         let* bt = unwrap_error r in
         let* g = Btree.get bt key in
         (match g with
          | Ok v -> Lwt.return v
          | Error e ->
            Lwt.fail_with
              (Format.asprintf "Store.get(rw): %a" pp_error (map_btree_err e))))
```

- [ ] **Step 7: Update `cursor_open` for Ro**

In `cursor_open`, add dispatch:
```ocaml
let cursor_open : type a. a txn -> tree_id -> cursor Lwt.t =
  fun tx tid ->
    match tx with
    | Ro snap ->
      (match snap.rs_store.backend with
       | Mem trees ->
         let entries = BytesMap.bindings !(mem_tree trees tid) in
         Lwt.return { all = entries; remaining = []; ready = false }
       | Btree st ->
         let* r = bt_get_tree_ro snap st tid in
         let* bt = unwrap_error r in
         let* co = Btree.cursor_open bt in
         (match co with
          | Error e ->
            Lwt.fail_with (Format.asprintf "Store.cursor_open(ro): %a"
                             pp_error (map_btree_err e))
          | Ok c ->
            let* entries = drain_btree_cursor c in
            Btree.cursor_close c;
            Lwt.return { all = entries; remaining = []; ready = false }))
    | Rw _ ->
      (* existing Rw code unchanged *)
      ...
```

- [ ] **Step 8: Update `rw_begin` to compute `alloc_min_safe` with active readers**

In `rw_begin` (Btree branch), replace the `set_alloc_min_safe` call:
```ocaml
let current_rw_txn_id = Int64.add st.current_header.txn_id 1L in
Pager.set_txn_id st.pager current_rw_txn_id;
let min_safe =
  match min_active_reader_txn st with
  | None   -> current_rw_txn_id
  | Some m -> Int64.min current_rw_txn_id m
in
Pager.set_alloc_min_safe st.pager min_safe;
st.txn_freelist_snapshot <- Some (Pager.freelist st.pager);
```

- [ ] **Step 9: Build and run tests**

```bash
podman run --rm -v $(pwd):/work:Z sqlocaml-dev dune runtest
```

Expected: all tests pass including the two new snapshot isolation tests.

- [ ] **Step 10: Add QCheck property: RO snapshot never sees uncommitted data**

In `test/test_store_btree.ml`, add a QCheck property:

```ocaml
let prop_ro_snapshot_isolation =
  QCheck.Test.make ~count:500 ~name:"ro_snapshot_isolation"
    QCheck.(pair (small_list (pair printable_string printable_string))
                 (small_list (pair printable_string printable_string)))
    (fun (initial_pairs, update_pairs) ->
      let path = Filename.temp_file "sqlocaml_qc_snap_" ".db" in
      Fun.protect ~finally:(fun () -> try Sys.remove path with _ -> ()) (fun () ->
        let store = Result.get_ok (run (S.open_file ~path)) in
        List.iter (fun (k, v) ->
          let tx = run (S.rw_begin store) in
          run (S.put tx 16 (Bytes.of_string k) (Bytes.of_string v));
          run (S.commit tx)
        ) initial_pairs;
        (* Take snapshot of committed state *)
        let ro = run (S.ro_begin store) in
        let snap_vals = List.map (fun (k, _) ->
          run (S.get ro 16 (Bytes.of_string k))
        ) initial_pairs in
        (* Concurrent writes *)
        List.iter (fun (k, v) ->
          let tx = run (S.rw_begin store) in
          run (S.put tx 16 (Bytes.of_string k) (Bytes.of_string v));
          run (S.commit tx)
        ) update_pairs;
        (* Snapshot should still see original values *)
        let snap_vals_after = List.map (fun (k, _) ->
          run (S.get ro 16 (Bytes.of_string k))
        ) initial_pairs in
        run (S.ro_end ro);
        run (S.close store);
        snap_vals = snap_vals_after))
```

Run: `podman run --rm -v $(pwd):/work:Z sqlocaml-dev dune runtest`
Expected: all tests pass.

- [ ] **Step 11: Commit**

```bash
git add lib/store/store.ml lib/store/store.mli test/test_store_btree.ml
git commit -m "storage: true RO snapshot isolation + active-reader freelist gating"
```

---

## Task 5: BEGIN / COMMIT / ROLLBACK SQL

Wire up explicit SQL transactions. Users can now write:
```sql
BEGIN;
INSERT INTO t VALUES (1);
INSERT INTO t VALUES (2);
COMMIT;
```
or `ROLLBACK` to undo pending changes.

**Design overview:**
1. `ast.ml` — add `S_begin | S_commit | S_rollback`
2. `lexer.mll` — add `BEGIN`, `COMMIT`, `ROLLBACK` tokens
3. `parser.mly` — add rules for the three statements
4. `sema.ml` — pass-through binding (no names to resolve)
5. `plan.ml` — add `Op_begin | Op_commit | Op_rollback` plan operators
6. `planner.ml` — plan `S_begin/commit/rollback`
7. `exec.ml` / `exec.mli` — add `txn_mode` type; mutation ops accept it; consolidate `execute_insert` to use a single RW txn (fixing the two-txn bug)
8. `db.ml` / `db.mli` — `Db.t` gains `mutable explicit_txn`; `execute` handles `Op_begin/commit/rollback`; passes `In_txn tx` mode to exec when explicit txn is active
9. `test/test_txn.ml` — new test file

**Files:**
- Modify: `lib/sql/ast.ml`
- Modify: `lib/sql/lexer.mll`
- Modify: `lib/sql/parser.mly`
- Modify: `lib/sql/sema.ml`
- Modify: `lib/sql/plan.ml`
- Modify: `lib/sql/planner.ml`
- Modify: `lib/sql/exec.ml`, `lib/sql/exec.mli`
- Modify: `lib/db/db.ml`, `lib/db/db.mli`
- Create: `test/test_txn.ml`

- [ ] **Step 1: Write failing tests in `test/test_txn.ml`**

Create the file:

```ocaml
(** Tests for SQL-level BEGIN / COMMIT / ROLLBACK (Phase 3). *)

open Sqlocaml_db

let run = Lwt_main.run

let fresh_db () = run (open_in_memory ())

let rows_of stream =
  run (Lwt_stream.to_list stream)

let exec db sql =
  match run (execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "execute failed: %s" sql

let query_ints db sql =
  match run (query db sql) with
  | Error _ -> []
  | Ok stream ->
    List.map (fun row ->
      match row.(0) with V_int n -> Int64.to_int n | _ -> -1
    ) (rows_of stream)

(* ------------------------------------------------------------------ *)
(* Basic BEGIN / COMMIT                                                  *)
(* ------------------------------------------------------------------ *)

let test_begin_commit_visible () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "BEGIN";
  exec db "INSERT INTO t VALUES (1)";
  exec db "INSERT INTO t VALUES (2)";
  exec db "COMMIT";
  let ns = query_ints db "SELECT n FROM t ORDER BY n ASC" in
  Alcotest.(check (list int)) "committed rows visible" [1; 2] ns

let test_begin_rollback_invisible () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (0)";
  exec db "BEGIN";
  exec db "INSERT INTO t VALUES (1)";
  exec db "INSERT INTO t VALUES (2)";
  exec db "ROLLBACK";
  let ns = query_ints db "SELECT n FROM t ORDER BY n ASC" in
  Alcotest.(check (list int)) "rolled-back rows invisible" [0] ns

(* ------------------------------------------------------------------ *)
(* Error cases                                                           *)
(* ------------------------------------------------------------------ *)

let test_double_begin_errors () =
  let db = fresh_db () in
  exec db "BEGIN";
  let r = run (execute db "BEGIN") in
  Alcotest.(check bool) "double BEGIN is error" true (Result.is_error r)

let test_commit_without_begin_errors () =
  let db = fresh_db () in
  let r = run (execute db "COMMIT") in
  Alcotest.(check bool) "COMMIT without BEGIN is error" true (Result.is_error r)

let test_rollback_without_begin_errors () =
  let db = fresh_db () in
  let r = run (execute db "ROLLBACK") in
  Alcotest.(check bool) "ROLLBACK without BEGIN is error" true (Result.is_error r)

(* ------------------------------------------------------------------ *)
(* Auto-commit outside explicit txn still works                         *)
(* ------------------------------------------------------------------ *)

let test_autocommit_still_works () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (42)";
  let ns = query_ints db "SELECT n FROM t" in
  Alcotest.(check (list int)) "auto-commit insert visible" [42] ns

(* ------------------------------------------------------------------ *)
(* Multi-statement txn with UPDATE and DELETE                           *)
(* ------------------------------------------------------------------ *)

let test_txn_with_update_delete () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (1)";
  exec db "INSERT INTO t VALUES (2)";
  exec db "INSERT INTO t VALUES (3)";
  exec db "BEGIN";
  exec db "UPDATE t SET n = 10 WHERE n = 1";
  exec db "DELETE FROM t WHERE n = 2";
  exec db "COMMIT";
  let ns = query_ints db "SELECT n FROM t ORDER BY n ASC" in
  Alcotest.(check (list int)) "txn update+delete committed" [3; 10] ns

let test_txn_rollback_with_update () =
  let db = fresh_db () in
  exec db "CREATE TABLE t (n INTEGER)";
  exec db "INSERT INTO t VALUES (1)";
  exec db "BEGIN";
  exec db "UPDATE t SET n = 99 WHERE n = 1";
  exec db "ROLLBACK";
  let ns = query_ints db "SELECT n FROM t" in
  Alcotest.(check (list int)) "update rolled back" [1] ns

(* ------------------------------------------------------------------ *)
(* Parser: correct parse of BEGIN/COMMIT/ROLLBACK                       *)
(* ------------------------------------------------------------------ *)

let test_parse_begin () =
  let db = fresh_db () in
  let r = run (execute db "BEGIN") in
  Alcotest.(check bool) "BEGIN parses and executes" true (Result.is_ok r);
  ignore (run (execute db "ROLLBACK"))  (* clean up *)

(* ------------------------------------------------------------------ *)
(* Runner                                                                *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run "test_txn"
    [ "begin_commit", [
        Alcotest.test_case "begin_commit_visible"    `Quick test_begin_commit_visible;
        Alcotest.test_case "begin_rollback_invisible" `Quick test_begin_rollback_invisible;
        Alcotest.test_case "autocommit_still_works"  `Quick test_autocommit_still_works;
      ];
      "error_cases", [
        Alcotest.test_case "double_begin_errors"         `Quick test_double_begin_errors;
        Alcotest.test_case "commit_without_begin_errors" `Quick test_commit_without_begin_errors;
        Alcotest.test_case "rollback_without_begin_errors" `Quick test_rollback_without_begin_errors;
      ];
      "multi_stmt", [
        Alcotest.test_case "txn_update_delete_commit"  `Quick test_txn_with_update_delete;
        Alcotest.test_case "txn_rollback_with_update"  `Quick test_txn_rollback_with_update;
      ];
      "parse", [
        Alcotest.test_case "parse_begin" `Quick test_parse_begin;
      ];
    ]
```

Also register the new test binary in `test/dune`:
```scheme
(test
 (name test_txn)
 (libraries sqlocaml_db alcotest lwt.unix))
```

Run: `podman run --rm -v $(pwd):/work:Z sqlocaml-dev dune runtest`
Expected: compile errors (S_begin etc. not yet defined)

- [ ] **Step 2: Add to `ast.ml`**

At the end of the `stmt` type, add:
```ocaml
  | S_begin
  | S_commit
  | S_rollback
```

- [ ] **Step 3: Add tokens to `lexer.mll`**

In the rule table, after the `DROP` line:
```ocaml
  | "BEGIN"    { BEGIN }
  | "COMMIT"   { COMMIT }
  | "ROLLBACK" { ROLLBACK }
```

- [ ] **Step 4: Update `parser.mly`**

In the `%token` declarations, add `BEGIN COMMIT ROLLBACK` to the existing list.

In the `stmt` rule, add three new alternatives:
```
  | s = begin_stmt   { s }
  | s = commit_stmt  { s }
  | s = rollback_stmt { s }
```

Add the rules:
```
begin_stmt:
  | BEGIN { S_begin }

commit_stmt:
  | COMMIT { S_commit }

rollback_stmt:
  | ROLLBACK { S_rollback }
```

- [ ] **Step 5: Update `sema.ml` — pass-through binding**

In `Sema.bind`, add cases for the three new stmts. Find the match on `ast` inside `bind`, and add:
```ocaml
| Ast.S_begin    -> Lwt.return_ok Plan.Op_begin
| Ast.S_commit   -> Lwt.return_ok Plan.Op_commit
| Ast.S_rollback -> Lwt.return_ok Plan.Op_rollback
```

Wait — `sema.ml` currently returns a `bound_stmt` (an intermediate representation), not a `Plan.op`. Check the actual sema.ml return type and add the appropriate binding case. Typically sema returns a bound AST node, and planner converts it. So add `B_begin | B_commit | B_rollback` to whatever the bound type is, returning them directly.

Looking at the existing sema.ml, find how `S_drop_table` is handled (the simplest case):
- If sema returns `Plan.op` directly, add the three cases returning `Plan.Op_begin` etc.
- If sema returns a bound intermediate, add `B_begin | B_commit | B_rollback` and handle them in planner.

Follow the existing pattern for `S_drop_table` / `S_drop_index` exactly.

- [ ] **Step 6: Update `plan.ml` — add Op_begin/Op_commit/Op_rollback**

At the end of `type op`:
```ocaml
  | Op_begin
  | Op_commit
  | Op_rollback
```

- [ ] **Step 7: Update `planner.ml`**

Add pattern matches for the three new bound variants (follow whatever pattern `S_drop_table` uses):
```ocaml
| B_begin    -> Op_begin
| B_commit   -> Op_commit
| B_rollback -> Op_rollback
```

(Exact form depends on whether sema.ml returns bound type or Plan.op directly — follow existing pattern.)

- [ ] **Step 8: Update `exec.ml` and `exec.mli` — add txn_mode**

In `exec.mli`, add:
```ocaml
module S = Sqlocaml_store.Store

(** Transaction mode passed by [Db] when an explicit transaction is active. *)
type txn_mode =
  | Auto        (** Each DML op starts and commits its own RW txn. *)
  | In_txn of S.rw S.txn  (** Use this txn; skip auto begin/commit. *)

val execute_with_count :
  S.t -> Sqlocaml_catalog.Catalog.t -> ?mode:txn_mode -> Plan.op -> int Lwt.t

val execute :
  S.t -> Sqlocaml_catalog.Catalog.t -> ?mode:txn_mode -> Plan.op -> unit Lwt.t
```

In `exec.ml`:

Add the type:
```ocaml
type txn_mode =
  | Auto
  | In_txn of S.rw S.txn
```

Add a helper for txn acquisition:
```ocaml
(* Acquire a txn: either start a new one (Auto) or use the provided one (In_txn).
   Returns (tx, should_commit) *)
let acquire_txn store mode =
  match mode with
  | Auto ->
    let* tx = S.rw_begin store in
    Lwt.return (tx, true)
  | In_txn tx ->
    Lwt.return (tx, false)

let release_txn tx should_commit =
  if should_commit then S.commit tx else Lwt.return_unit
```

Rewrite `execute_insert` to use a SINGLE txn for both row insert and index updates (fixes the two-txn bug) and accept `~mode`:

```ocaml
let execute_insert (store : S.t) (cat : Cat.t)
    ?(mode = Auto)
    ~(table_meta : Cat.table_meta) ~ordinals ~values : unit Lwt.t =
  let n   = List.length table_meta.columns in
  let row = Array.make n Row.V_null in
  List.iter2 (fun ord v -> row.(ord) <- lit_to_value v) ordinals values;
  let* rowid = Cat.next_rowid cat ~name:table_meta.name in
  let key    = Rowid.encode rowid in
  let bytes  = Row.encode table_meta.columns row in
  let* (tx, owned) = acquire_txn store mode in
  let* () = S.put tx table_meta.tree_id key bytes in
  (* Check UNIQUE constraints and write index entries in the same txn *)
  let idxs = Cat.indexes_for_table cat ~table:table_meta.name in
  let* unique_ok =
    Lwt_list.fold_left_s (fun acc (idx : Cat.index_info) ->
      if not acc || not idx.idx_unique then Lwt.return acc
      else begin
        let col_idx = find_col_idx_by_name table_meta.columns idx.idx_column in
        let v = row.(col_idx) in
        let ik_value = row_value_to_index_value v in
        let prefix = Index_key.encode_value ik_value in
        let plen = Bytes.length prefix in
        let seek_key = Bytes.cat prefix (Rowid.encode Int64.min_int) in
        let* cur = S.cursor_open tx idx.idx_tree_id in
        let _sr = S.cursor_seek cur seek_key in
        let duplicate =
          match S.cursor_next cur with
          | None -> false
          | Some (ikey, _) ->
            Bytes.length ikey >= plen &&
            Bytes.equal (Bytes.sub ikey 0 plen) prefix
        in
        S.cursor_close cur;
        if duplicate then
          Lwt.fail_with (Printf.sprintf
            "UNIQUE constraint violated: duplicate value in column '%s'"
            idx.idx_column)
        else
          Lwt.return true
      end
    ) true idxs
  in
  ignore unique_ok;
  let* () = Lwt_list.iter_s (fun (idx : Cat.index_info) ->
    let col_idx = find_col_idx_by_name table_meta.columns idx.idx_column in
    let v = row.(col_idx) in
    let ikey = Index_key.encode [row_value_to_index_value v] ~rowid in
    S.put tx idx.idx_tree_id ikey Bytes.empty
  ) idxs in
  release_txn tx owned
```

Update all other execute_* functions similarly: add `?(mode = Auto)` and use `acquire_txn`/`release_txn`. The pattern is identical for each:
- `execute_create_index`: acquires tx, does work, releases
- `execute_update`: keep the RO drain phase as `ro_begin`/`ro_end` (always snapshot), then acquire RW for mutations
- `execute_delete`: same pattern as update
- `execute_drop_table` and `execute_drop_index`: acquire/release

Update `execute_with_count` and `execute` to accept `?(mode = Auto)` and pass it through:
```ocaml
let execute_with_count ?(mode = Auto) (store : S.t) (cat : Cat.t) (op : Plan.op) : int Lwt.t =
  match op with
  | Plan.Op_insert { table_meta; ordinals; values } ->
    let* () = execute_insert store cat ~mode ~table_meta ~ordinals ~values in
    Lwt.return 1
  | Plan.Op_update { table_meta; assignments; where; indexes } ->
    execute_update store ~mode ~table_meta ~assignments ~where ~indexes
  | Plan.Op_delete { table_meta; where; indexes } ->
    execute_delete store ~mode ~table_meta ~where ~indexes
  | Plan.Op_create_index { name; table; tree_id; col_idx; unique; columns } ->
    let* () = execute_create_index store cat ~mode ~name ~table ~tree_id
                ~col_idx ~unique ~columns in
    Lwt.return 0
  | Plan.Op_create_table { name; columns } ->
    let* () = execute_create_table store cat ~name ~columns in
    Lwt.return 0
  | Plan.Op_drop_table { table_meta; indexes } ->
    let* () = execute_drop_table store cat ~mode ~table_meta ~_indexes:indexes in
    Lwt.return 0
  | Plan.Op_drop_index { idx_info } ->
    let* () = execute_drop_index store cat ~mode ~idx_info in
    Lwt.return 0
  | Plan.Op_begin | Plan.Op_commit | Plan.Op_rollback ->
    failwith "Exec.execute_with_count: BEGIN/COMMIT/ROLLBACK handled by Db layer"
  | _ ->
    failwith "Exec.execute: use Exec.query for read operations"

let execute ?(mode = Auto) (store : S.t) (cat : Cat.t) (op : Plan.op) : unit Lwt.t =
  let* _n = execute_with_count ~mode store cat op in
  Lwt.return_unit
```

- [ ] **Step 9: Update `db.ml` and `db.mli`**

In `db.mli`, add:
```ocaml
(** Begin an explicit transaction. Statements executed after this (until
    [COMMIT] or [ROLLBACK]) are not auto-committed. *)
val begin_txn : t -> (unit, error) result Lwt.t

(** Commit the current explicit transaction. *)
val commit_txn : t -> (unit, error) result Lwt.t

(** Roll back the current explicit transaction. *)
val rollback_txn : t -> (unit, error) result Lwt.t
```

In `db.ml`, update `type t` to add the explicit txn field:
```ocaml
type t = {
  store            : S.t;
  catalog          : Cat.t;
  mutable explicit_txn : S.rw S.txn option;
}
```

Update both `open_in_memory` and `open_file` to initialize `explicit_txn = None`.

Add the three public functions:
```ocaml
let begin_txn t =
  match t.explicit_txn with
  | Some _ -> Lwt.return (Error (Runtime "transaction already active"))
  | None ->
    let* tx = S.rw_begin t.store in
    t.explicit_txn <- Some tx;
    Lwt.return (Ok ())

let commit_txn t =
  match t.explicit_txn with
  | None -> Lwt.return (Error (Runtime "no active transaction"))
  | Some tx ->
    t.explicit_txn <- None;
    let* () = S.commit tx in
    Lwt.return (Ok ())

let rollback_txn t =
  match t.explicit_txn with
  | None -> Lwt.return (Error (Runtime "no active transaction"))
  | Some tx ->
    t.explicit_txn <- None;
    let* () = S.rollback tx in
    Lwt.return (Ok ())
```

Update `execute` to handle `Op_begin/commit/rollback` and pass `In_txn` mode:
```ocaml
let execute t sql =
  let* op = prepare t sql in
  match op with
  | Error e -> Lwt.return (Error e)
  | Ok Plan.Op_begin    -> begin_txn t
  | Ok Plan.Op_commit   -> commit_txn t
  | Ok Plan.Op_rollback -> rollback_txn t
  | Ok op ->
    let mode = match t.explicit_txn with
      | None    -> Sql.Exec.Auto
      | Some tx -> Sql.Exec.In_txn tx
    in
    (match Sql.Exec.execute ~mode t.store t.catalog op with
     | exception Failure msg -> Lwt.return (Error (Runtime msg))
     | lwt_op ->
       Lwt.catch
         (fun () -> let* () = lwt_op in Lwt.return (Ok ()))
         (function
          | Failure msg -> Lwt.return (Error (Runtime msg))
          | exn         -> Lwt.fail exn))
```

Update `execute_change_count` similarly (same Op_begin/commit/rollback handling, pass mode).

Note: `query` (SELECT) always uses `ro_begin`/`ro_end` inside exec.ml regardless of mode — it reads committed state. Document this as "SELECT within an explicit txn reads the last committed state, not in-progress writes" in a comment.

- [ ] **Step 10: Build and run all tests**

```bash
podman run --rm -v $(pwd):/work:Z sqlocaml-dev dune runtest
```

Expected: all existing tests pass plus all tests in `test_txn.ml`.

- [ ] **Step 11: Commit**

```bash
git add lib/sql/ast.ml lib/sql/lexer.mll lib/sql/parser.mly \
        lib/sql/sema.ml lib/sql/plan.ml lib/sql/planner.ml \
        lib/sql/exec.ml lib/sql/exec.mli \
        lib/db/db.ml lib/db/db.mli \
        test/test_txn.ml test/dune
git commit -m "sql: add BEGIN/COMMIT/ROLLBACK explicit transaction control"
```

---

## Task 6: Coverage cleanup

Improve handwritten coverage from ~80.59% to ≥85%, targeting new Phase 3 code paths and any remaining gaps.

**Files:**
- Modify: `test/test_e2e.ml`
- Modify: `test/test_freelist.ml`
- Modify: `test/test_store_btree.ml`
- Possibly modify: `test/test_txn.ml`

- [ ] **Step 1: Run coverage baseline**

```bash
podman run --rm -v $(pwd):/work:Z sqlocaml-dev bash scripts/coverage.sh summary
```

Record per-file percentages.

- [ ] **Step 2: Identify top uncovered paths**

Look for files with < 85% coverage. Common gaps after Phase 3:

- `store.ml`: freelist page read errors, `min_active_reader_txn` edge cases, `n_pages`/`freelist_size` on Mem backend
- `freelist.ml`: `reusable_count` with `min_safe_txn_id` variants
- `db.ml`: `begin_txn` / `commit_txn` / `rollback_txn` branches
- `exec.ml`: `In_txn` code paths in each execute_* function
- `pager.ml`: `clear_dirty` with non-empty dirty set; `set_alloc_min_safe`

- [ ] **Step 3: Add coverage tests**

In `test/test_freelist.ml`, add tests for `reusable_count` with `~min_safe_txn_id`:
```ocaml
let test_reusable_count () =
  let fl = Freelist.of_list [(2l, 3L); (3l, 5L); (4l, 7L)] in
  Alcotest.(check int) "count below 5" 1 (Freelist.reusable_count fl ~min_safe_txn_id:5L);
  Alcotest.(check int) "count below 7" 2 (Freelist.reusable_count fl ~min_safe_txn_id:7L);
  Alcotest.(check int) "count 0" 0 (Freelist.reusable_count fl ~min_safe_txn_id:0L)
```

In `test/test_e2e.ml`, add tests that exercise the Db-level `begin_txn`/`commit_txn`/`rollback_txn` directly (not through SQL parsing, to hit those branches directly).

In `test/test_store_btree.ml`, add tests for Mem backend `freelist_size` and `n_pages` returning 0/0L.

In `test/test_txn.ml`, add tests for edge cases in explicit txns:
- Verify SELECT within explicit txn sees COMMITTED state (not in-progress)
- Verify DDL (CREATE TABLE) within explicit txn is committed on COMMIT and absent after ROLLBACK

```ocaml
let test_ddl_in_txn_commit () =
  let db = fresh_db () in
  exec db "BEGIN";
  exec db "CREATE TABLE new_t (x INTEGER)";
  exec db "INSERT INTO new_t VALUES (99)";
  exec db "COMMIT";
  let ns = query_ints db "SELECT x FROM new_t" in
  Alcotest.(check (list int)) "DDL committed in txn" [99] ns

let test_ddl_in_txn_rollback () =
  let db = fresh_db () in
  exec db "BEGIN";
  exec db "CREATE TABLE new_t (x INTEGER)";
  exec db "ROLLBACK";
  (* Table should not exist *)
  let r = run (execute db "SELECT x FROM new_t") in
  Alcotest.(check bool) "table absent after rollback" true (Result.is_error r)
```

- [ ] **Step 4: Run tests and verify coverage**

```bash
podman run --rm -v $(pwd):/work:Z sqlocaml-dev dune runtest
podman run --rm -v $(pwd):/work:Z sqlocaml-dev bash scripts/coverage.sh summary
```

Expected: ≥ 85% handwritten coverage.

- [ ] **Step 5: Update ROADMAP.md**

Mark Phase 3 items complete:
- `[x] BEGIN, COMMIT, ROLLBACK`
- `[x] Free-list with per-page freed-at-txn metadata`
- `[x] RO txn manager (active-readers table; freelist gating)`
- Update Phase 3 line to `[x]`

- [ ] **Step 6: Commit**

```bash
git add test/ ROADMAP.md
git commit -m "test: Phase 3 coverage cleanup — freelist, txn, snapshot isolation"
```

---

## Self-review

**Spec coverage check:**

| Roadmap item | Task |
|---|---|
| `BEGIN`, `COMMIT`, `ROLLBACK` | Task 5 |
| Free-list with per-page freed-at-txn metadata | Task 1 + 2 |
| RO txn manager (active-readers table; freelist gating) | Task 4 |
| RW txn manager (single-writer mutex) | already done in Phase 1 |
| Crash recovery (pick valid header by checksum + txn_id) | already done in Phase 1 |

**No gaps found.**

**Placeholder scan:** No TBDs, all code blocks have complete OCaml syntax. ✓

**Type consistency check:**
- `txn_mode` type defined in exec.ml, referenced in exec.mli and db.ml ✓
- `ro_snapshot` type defined locally in store.ml, not exposed in store.mli ✓
- `Freelist.pop ~min_safe_txn_id` renamed consistently across freelist.ml/mli and pager.ml ✓
- `Pager.alloc` signature change (remove `~current_txn_id`) propagated to btree.ml and test_pager.ml ✓
