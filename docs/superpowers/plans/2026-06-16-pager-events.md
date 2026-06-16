# Pager page/COW/freelist events (#384) + Txn_commit frames fix (#386) — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add per-page read/write, page alloc/free (freelist pop/push) events to the internals-monitor seam — surfaced through new `Store_event` variants — and make `Txn_commit` report the real WAL frame count instead of `0`.

**Architecture:** A new storage-local signal type `Pager_event.t` decouples the hot path from `Store_event` (which lives a layer up; a direct reference would create a dependency cycle). `Pager` gains a `mutable on_page_event` hook that fires `Pager_event.t` values at the four physical-I/O sites, using guard-before-construct so nothing allocates when the monitor is off. `Store.set_event_callback` installs a translator that maps `Pager_event.t` → new `Store_event.t` variants, stamping `txn_id` from the pager. #386 threads the real appended frame count out of `commit_wal`.

**Tech Stack:** OCaml 5.1, Lwt, Cstruct, Alcotest + QCheck. Build/test/format run inside the `sqlocaml-dev` podman container (see commands below).

---

## Conventions for every command in this plan

Run all OCaml tooling inside the dev container from the worktree root
(`/home/tej/projects/sqlite_ocaml_port/.worktrees/384-pager-events`). Define once per shell:

```sh
WT=/home/tej/projects/sqlite_ocaml_port/.worktrees/384-pager-events
run() { podman run --rm -v "$WT:/workspace:z" -w /workspace sqlocaml-dev "$@"; }
```

- **Build:** `run dune build`
- **Test (one exe):** `run dune test test/test_pager_event.exe`
- **Test (all):** `run dune test`
- **Format check:** `run ocamlformat --check <file>`
- **Format fix (write back to host):**
  ```sh
  tmp=$(mktemp) && run ocamlformat <file> > "$tmp" && mv "$tmp" <file> && chmod 644 <file>
  ```

Commit after each task. Do **not** push or open the PR until the final task.

---

## File structure

- **Create** `lib/storage/pager_event.ml` — the page-level signal type (one variant per physical event). No `.mli` needed: it is a single public type with no functions; the `.ml` is its own interface. (Storage `dune` auto-includes all modules — no dune change.)
- **Modify** `lib/storage/pager.ml` — add `on_page_event` field, `set_page_event_callback`, internal `emit_page_event`/`emit_writes`, and emit at the read/alloc/free/flush sites.
- **Modify** `lib/storage/pager.mli` — expose `set_page_event_callback`.
- **Modify** `lib/store/store_event.ml` / `.mli` — four new variants + `label`/`txn_id`/`pp` arms.
- **Modify** `lib/store/store.ml` — `module Pager_event` alias; `translate_pager_event`; wire the pager hook in `set_event_callback`; thread the frame count for #386.
- **Create** `test/test_pager_event.ml` — storage-level Pager_event tests (mock block backend).
- **Modify** `test/dune` — register `test_pager_event` in `names` and `modules`.
- **Modify** `test/test_store_event.ml` — store-level page-event tests, pp/label additions, and the #386 frames assertion.

---

## Task 1: `Pager_event` type + Pager hook plumbing (no emit sites yet)

**Files:**
- Create: `lib/storage/pager_event.ml`
- Modify: `lib/storage/pager.ml` (record field at `type t`, `create`, new helpers + setter)
- Modify: `lib/storage/pager.mli` (`set_page_event_callback`)
- Create: `test/test_pager_event.ml`
- Modify: `test/dune`

- [ ] **Step 1: Create the signal type**

`lib/storage/pager_event.ml`:
```ocaml
(** Page-level signal emitted by {!Sqlocaml_storage.Pager} for the internals
    monitor (#384).  Storage-local on purpose: [Store_event] lives a layer up in
    [sqlocaml.store], which depends on this library, so the pager cannot
    reference it without creating a dependency cycle.  [Store.set_event_callback]
    translates these into [Store_event.t] variants.

    Emitted only on PHYSICAL I/O: [Page_read] fires on a backend read (cache
    miss), never on a cache hit; [Page_write] fires once per dirty page handed to
    the WAL/main on flush. *)
type t =
  | Page_read of { page_id : int64 } (** backend read — cache MISS only *)
  | Page_write of { page_id : int64 } (** page written to WAL/main on flush *)
  | Page_alloc of
      { page_id : int64
      ; reused : bool (** [true] = freelist/txn-pool reuse; [false] = file extend *)
      }
  | Page_free of { page_id : int64 } (** page pushed to the freelist / txn pool *)
```

- [ ] **Step 2: Write the failing test (plumbing: set/clear callback, no events without ops)**

Create `test/test_pager_event.ml`. (This file grows in later tasks; start with the harness + Task-1 tests.)
```ocaml
(** Tests for the Pager page-event seam (#384). *)

open Sqlocaml_storage

(* Keep cache assertions deterministic across this exe. *)
let () = Unix.putenv "SQLOCAML_PAGE_CACHE" "64"

type mock_block =
  { store : (int64, Bytes.t) Hashtbl.t
  ; mutable n_pages : int64
  }

let make_mock () = { store = Hashtbl.create 16; n_pages = 0L }

let mock_callbacks mb =
  let read_page ~page_id buf =
    (match Hashtbl.find_opt mb.store page_id with
     | None -> Cstruct.memset buf 0
     | Some bytes -> Cstruct.blit_from_bytes bytes 0 buf 0 Page.page_size);
    Lwt.return_ok ()
  in
  let write_page ~page_id buf =
    let bytes = Bytes.create Page.page_size in
    Cstruct.blit_to_bytes buf 0 bytes 0 Page.page_size;
    Hashtbl.replace mb.store page_id bytes;
    Lwt.return_ok ()
  in
  let sync () = Lwt.return_ok () in
  let resize ~n_pages =
    mb.n_pages <- n_pages;
    Lwt.return_ok ()
  in
  read_page, write_page, sync, resize
;;

let make_pager ?(n_pages = 0L) ?(freelist = Freelist.empty) () =
  let mb = make_mock () in
  let read_page, write_page, sync, resize = mock_callbacks mb in
  let pager = Pager.create ~read_page ~write_page ~sync ~resize ~n_pages ~freelist in
  pager, mb
;;

let run = Lwt_main.run

(* Attach a recorder; returns the (reversed-then-reversed) event list ref. *)
let recorder pager =
  let seen = ref [] in
  Pager.set_page_event_callback pager (Some (fun ev -> seen := ev :: !seen));
  seen
;;

let events seen = List.rev !seen

let test_no_ops_no_events () =
  let p, _ = make_pager () in
  let seen = recorder p in
  Alcotest.(check int) "no events before any op" 0 (List.length (events seen))
;;

let test_set_none_clears () =
  let p, _ = make_pager () in
  let seen = recorder p in
  Pager.set_page_event_callback p None;
  (* a read that would emit (Task 2) must produce nothing once cleared *)
  let _ = run (Pager.read p 0L) in
  Alcotest.(check int) "no events after clearing callback" 0 (List.length (events seen))
;;

let () =
  Alcotest.run
    "pager_event"
    [ ( "plumbing"
      , [ Alcotest.test_case "no ops no events" `Quick test_no_ops_no_events
        ; Alcotest.test_case "set None clears" `Quick test_set_none_clears
        ] )
    ]
;;
```

- [ ] **Step 3: Register the test exe in `test/dune`**

Add `test_pager_event` to BOTH the `(names ...)` and `(modules ...)` lists (e.g. right after `test_pager` on lines 23 and 67):
```
  test_pager
  test_pager_event
```

- [ ] **Step 4: Run the test — expect FAIL (compile error: `set_page_event_callback` not found)**

```sh
run dune test test/test_pager_event.exe
```
Expected: build error `Unbound value Pager.set_page_event_callback`.

- [ ] **Step 5: Add the hook field + create default + helper + setter in `pager.ml`**

In `type t` (after the `mutable write_tag : int32` field, before the closing `}` at ~line 79):
```ocaml
  ; mutable on_page_event : (Pager_event.t -> unit) option
    (** #384: optional, synchronous, fire-and-forget observer for physical page
        I/O (internals monitor).  [None] = zero overhead: every emit site guards
        on this before constructing a [Pager_event.t], so nothing allocates when
        unset.  [Store.set_event_callback] installs a translator here. *)
```

In `create` (after `write_tag = 0l`, before the closing `}` at ~line 121):
```ocaml
  ; on_page_event = None
```

Add near the other small accessors (e.g. just after `write_tag`, ~line 128) the setter and an internal emit helper:
```ocaml
let set_page_event_callback t cb = t.on_page_event <- cb

(* #384: fire a page event iff an observer is attached.  Guard-before-construct
   at every call site keeps the [None] path allocation-free on hot paths. *)
let emit_page_event t ev =
  match t.on_page_event with
  | None -> ()
  | Some f -> f ev
;;
```

- [ ] **Step 6: Expose the setter in `pager.mli`**

After `val write_tag : t -> int32` (~line 42), add:
```ocaml
(** #384: attach/detach a synchronous observer for physical page I/O events
    (internals monitor).  [None] (the default) is zero-overhead.  The observer
    must be cheap and non-blocking; exceptions it raises are NOT caught here —
    [Store.set_event_callback] wraps the store-level observer to swallow them. *)
val set_page_event_callback : t -> (Pager_event.t -> unit) option -> unit
```

- [ ] **Step 7: Run the test — expect PASS**

```sh
run dune test test/test_pager_event.exe
```
Expected: PASS (2 cases). `emit_page_event` is unused for now — that is fine; it is used in Task 2. If the compiler errors on unused `emit_page_event`, proceed directly to Task 2 in the same commit, or temporarily mark it; in this codebase top-level `let`s in a `.ml` with a `.mli` are not flagged unused, so no action needed.

- [ ] **Step 8: Format and commit**

```sh
for f in lib/storage/pager_event.ml lib/storage/pager.ml lib/storage/pager.mli test/test_pager_event.ml; do
  tmp=$(mktemp) && run ocamlformat "$f" > "$tmp" && mv "$tmp" "$f" && chmod 644 "$f"
done
run dune build
git add lib/storage/pager_event.ml lib/storage/pager.ml lib/storage/pager.mli test/test_pager_event.ml test/dune
git commit -m "feat(#384): Pager_event type + on_page_event hook plumbing"
```

---

## Task 2: `Page_read` emit on backend reads (cache miss)

**Files:**
- Modify: `lib/storage/pager.ml` (`load_main_page`, `load_main_page_borrow`)
- Modify: `test/test_pager_event.ml`

- [ ] **Step 1: Write the failing tests**

Add to `test/test_pager_event.ml` (before the `let () = Alcotest.run ...` block), and register them in a new `"read"` group:
```ocaml
let is_read = function
  | Pager_event.Page_read _ -> true
  | _ -> false
;;

let test_cache_miss_emits_read () =
  let p, _ = make_pager ~n_pages:4L () in
  let seen = recorder p in
  let _ = run (Pager.read p 2L) in
  let reads = List.filter is_read (events seen) in
  Alcotest.(check int) "one Page_read on cache miss" 1 (List.length reads);
  (match reads with
   | [ Pager_event.Page_read { page_id } ] ->
     Alcotest.(check int64) "page id" 2L page_id
   | _ -> Alcotest.fail "expected exactly one Page_read")
;;

let test_cache_hit_emits_nothing () =
  let p, _ = make_pager ~n_pages:4L () in
  let _ = run (Pager.read p 2L) in
  (* prime cache *)
  let seen = recorder p in
  let _ = run (Pager.read p 2L) in
  (* now a hit *)
  Alcotest.(check int) "no Page_read on cache hit" 0 (List.length (events seen))
;;

let test_borrow_miss_emits_read () =
  let p, _ = make_pager ~n_pages:4L () in
  let seen = recorder p in
  let _ = run (Pager.read_borrow p 3L (fun _ -> Lwt.return_unit)) in
  Alcotest.(check int)
    "one Page_read on borrow cache miss"
    1
    (List.length (List.filter is_read (events seen)))
;;
```
Register the group in the `Alcotest.run` list:
```ocaml
    ; ( "read"
      , [ Alcotest.test_case "cache miss emits read" `Quick test_cache_miss_emits_read
        ; Alcotest.test_case "cache hit emits nothing" `Quick test_cache_hit_emits_nothing
        ; Alcotest.test_case "borrow miss emits read" `Quick test_borrow_miss_emits_read
        ] )
```

- [ ] **Step 2: Run — expect FAIL**

```sh
run dune test test/test_pager_event.exe
```
Expected: assertion failures (`one Page_read on cache miss` got 0).

- [ ] **Step 3: Emit at both cache-miss branches**

In `load_main_page`, in the `None` (miss) branch, after `let* result = t.read_page ~page_id buf in` and the `Ok ()` arm, emit before returning. Replace the `Ok ()` arm body (~lines 267-272):
```ocaml
     | Ok () ->
       emit_page_event t (Pager_event.Page_read { page_id });
       if not bypass_cache
       then (
         cache_add t key (cstruct_dup buf);
         pin_page t pin_set page_id);
       Lwt.return_ok buf)
```

In `load_main_page_borrow`, the identical `Ok ()` arm (~lines 345-350):
```ocaml
     | Ok () ->
       emit_page_event t (Pager_event.Page_read { page_id });
       if not bypass_cache
       then (
         cache_add t key buf;
         pin_page t pin_set page_id);
       Lwt.return_ok buf)
```

- [ ] **Step 4: Run — expect PASS**

```sh
run dune test test/test_pager_event.exe
```
Expected: PASS (now 5 cases). Re-run the existing pager suite to confirm no regression:
```sh
run dune test test/test_pager.exe
```
Expected: PASS.

- [ ] **Step 5: Format and commit**

```sh
for f in lib/storage/pager.ml test/test_pager_event.ml; do
  tmp=$(mktemp) && run ocamlformat "$f" > "$tmp" && mv "$tmp" "$f" && chmod 644 "$f"
done
git add lib/storage/pager.ml test/test_pager_event.ml
git commit -m "feat(#384): emit Page_read on backend reads (cache miss)"
```

---

## Task 3: `Page_alloc` (reused flag) + `Page_free` emit

**Files:**
- Modify: `lib/storage/pager.ml` (`alloc`, `free`)
- Modify: `test/test_pager_event.ml`

- [ ] **Step 1: Write the failing tests**

Add to `test/test_pager_event.ml`:
```ocaml
let allocs seen =
  List.filter_map
    (function
      | Pager_event.Page_alloc { page_id; reused } -> Some (page_id, reused)
      | _ -> None)
    (events seen)
;;

let frees seen =
  List.filter_map
    (function
      | Pager_event.Page_free { page_id } -> Some page_id
      | _ -> None)
    (events seen)
;;

let test_alloc_extend_not_reused () =
  let p, _ = make_pager ~n_pages:0L () in
  let seen = recorder p in
  let pid = run (Pager.alloc p) |> Result.get_ok in
  Alcotest.(check (list (pair int64 bool)))
    "alloc by file-extend, reused=false"
    [ pid, false ]
    (allocs seen)
;;

let test_alloc_from_freelist_reused () =
  (* Freelist holds page 1, freed at txn 1; min_safe lets it pop. *)
  let fl = Freelist.add Freelist.empty ~page_id:1l ~freed_at_txn_id:1L in
  let p, _ = make_pager ~n_pages:8L ~freelist:fl () in
  Pager.set_alloc_min_safe p 5L;
  let seen = recorder p in
  let pid = run (Pager.alloc p) |> Result.get_ok in
  Alcotest.(check (list (pair int64 bool)))
    "alloc from freelist, reused=true"
    [ pid, true ]
    (allocs seen);
  Alcotest.(check int64) "reused page id is 1" 1L pid
;;

let test_alloc_from_txn_pool_reused () =
  let p, _ = make_pager ~n_pages:8L () in
  (* mark a page as txn-owned-freed: free a page at/above the rw_begin threshold *)
  Pager.set_n_pages_at_rw_begin p 4L;
  Pager.free p ~page_id:6L ~freed_at_txn_id:1L;
  (* drop the Page_free we just produced from the recording window *)
  let seen = recorder p in
  let pid = run (Pager.alloc p) |> Result.get_ok in
  Alcotest.(check (list (pair int64 bool)))
    "alloc from txn pool, reused=true"
    [ pid, true ]
    (allocs seen);
  Alcotest.(check int64) "txn-pool page id is 6" 6L pid
;;

let test_free_below_threshold_emits_free () =
  let p, _ = make_pager ~n_pages:8L () in
  Pager.set_n_pages_at_rw_begin p 8L;
  let seen = recorder p in
  Pager.free p ~page_id:2L ~freed_at_txn_id:3L;
  Alcotest.(check (list int64)) "Page_free for freelist push" [ 2L ] (frees seen)
;;

let test_free_txn_owned_emits_free () =
  let p, _ = make_pager ~n_pages:8L () in
  Pager.set_n_pages_at_rw_begin p 4L;
  let seen = recorder p in
  Pager.free p ~page_id:6L ~freed_at_txn_id:3L;
  Alcotest.(check (list int64)) "Page_free for txn-owned push" [ 6L ] (frees seen)
;;
```
Register a `"alloc/free"` group:
```ocaml
    ; ( "alloc/free"
      , [ Alcotest.test_case "alloc extend not reused" `Quick test_alloc_extend_not_reused
        ; Alcotest.test_case "alloc from freelist reused" `Quick test_alloc_from_freelist_reused
        ; Alcotest.test_case "alloc from txn pool reused" `Quick test_alloc_from_txn_pool_reused
        ; Alcotest.test_case "free below threshold" `Quick test_free_below_threshold_emits_free
        ; Alcotest.test_case "free txn-owned" `Quick test_free_txn_owned_emits_free
        ] )
```

- [ ] **Step 2: Run — expect FAIL**

```sh
run dune test test/test_pager_event.exe
```
Expected: failures (`alloc by file-extend` got `[]`).

- [ ] **Step 3: Emit in `alloc` and `free`**

Rewrite `alloc` (~lines 407-430) to emit on each path:
```ocaml
let alloc t =
  (* #297: consult the txn-owned pool first — pages that were allocated
     above n_pages_at_rw_begin and have since been freed within this txn. *)
  match t.txn_owned_pool with
  | pid :: rest ->
    t.txn_owned_pool <- rest;
    emit_page_event t (Pager_event.Page_alloc { page_id = pid; reused = true });
    Lwt.return_ok pid
  | [] ->
    (match Freelist.pop t.freelist ~min_safe_txn_id:t.alloc_min_safe with
     | Some (pid32, fl') ->
       t.freelist <- fl';
       let pid = Int64.of_int32 pid32 in
       emit_page_event t (Pager_event.Page_alloc { page_id = pid; reused = true });
       Lwt.return_ok pid
     | None ->
       (* Extend the file by one page *)
       let new_id = t.n_pages in
       let new_pages = Int64.add t.n_pages 1L in
       let open Lwt.Syntax in
       let* result = t.resize ~n_pages:new_pages in
       (match result with
        | Error msg -> Lwt.return_error (Block_error msg)
        | Ok () ->
          t.n_pages <- new_pages;
          emit_page_event t (Pager_event.Page_alloc { page_id = new_id; reused = false });
          Lwt.return_ok new_id))
;;
```

Rewrite `free` (~lines 432-442) to emit in both branches:
```ocaml
let free t ~page_id ~freed_at_txn_id =
  (* #297: pages allocated above n_pages_at_rw_begin are txn-owned and
     can be safely reused within the current txn.  Route them to the
     txn_owned_pool instead of the main freelist so [alloc] returns them
     immediately without risking cursor or snapshot corruption. *)
  if Int64.compare page_id t.n_pages_at_rw_begin >= 0
  then t.txn_owned_pool <- page_id :: t.txn_owned_pool
  else
    t.freelist
    <- Freelist.add t.freelist ~page_id:(Int64.to_int32 page_id) ~freed_at_txn_id;
  emit_page_event t (Pager_event.Page_free { page_id })
;;
```

- [ ] **Step 4: Run — expect PASS**

```sh
run dune test test/test_pager_event.exe && run dune test test/test_pager.exe
```
Expected: PASS for both.

- [ ] **Step 5: Format and commit**

```sh
for f in lib/storage/pager.ml test/test_pager_event.ml; do
  tmp=$(mktemp) && run ocamlformat "$f" > "$tmp" && mv "$tmp" "$f" && chmod 644 "$f"
done
git add lib/storage/pager.ml test/test_pager_event.ml
git commit -m "feat(#384): emit Page_alloc (reused flag) and Page_free"
```

---

## Task 4: `Page_write` emit on flush (one per dirty page) + QCheck invariant

**Files:**
- Modify: `lib/storage/pager.ml` (`flush_via_wal`, `flush_no_sync` non-WAL branch, `flush`, `flush_one_to_main`)
- Modify: `test/test_pager_event.ml`

- [ ] **Step 1: Write the failing tests**

Add to `test/test_pager_event.ml`:
```ocaml
let writes seen =
  List.filter_map
    (function
      | Pager_event.Page_write { page_id } -> Some page_id
      | _ -> None)
    (events seen)
;;

(* Non-WAL pager: [flush] writes each dirty page to main. *)
let test_flush_emits_one_write_per_dirty () =
  let p, _ = make_pager ~n_pages:4L () in
  Pager.write p 0L (fill_page 1);
  Pager.write p 1L (fill_page 2);
  Pager.write p 2L (fill_page 3);
  let seen = recorder p in
  let _ = run (Pager.flush p) |> Result.get_ok in
  let ws = List.sort compare (writes seen) in
  Alcotest.(check (list int64)) "one Page_write per dirty page" [ 0L; 1L; 2L ] ws
;;

let test_flush_one_to_main_emits_write () =
  let p, _ = make_pager ~n_pages:4L () in
  let seen = recorder p in
  let _ = run (Pager.flush_one_to_main p ~page_id:1L ~buf:(fill_page 9)) |> Result.get_ok in
  Alcotest.(check (list int64)) "single write" [ 1L ] (writes seen)
;;

let test_flush_empty_no_writes () =
  let p, _ = make_pager ~n_pages:4L () in
  let seen = recorder p in
  let _ = run (Pager.flush p) |> Result.get_ok in
  Alcotest.(check (list int64)) "no dirty pages -> no writes" [] (writes seen)
;;

(* QCheck: every reused page-id was previously freed in the same session. *)
let prop_reuse_was_freed =
  QCheck.Test.make
    ~count:100
    ~name:"alloc reused=true page was previously freed"
    QCheck.(int_range 1 30)
    (fun n ->
       let p, _ = make_pager ~n_pages:0L () in
       (* allocate n pages by extension, free them all to the freelist, then
          re-allocate and check every reused page id was in the freed set. *)
       Pager.set_n_pages_at_rw_begin p 1_000_000L (* force freelist, not txn pool *);
       let freed = Hashtbl.create 16 in
       let alloc1 () = run (Pager.alloc p) |> Result.get_ok in
       let pids = List.init n (fun _ -> alloc1 ()) in
       List.iter
         (fun pid ->
            Hashtbl.replace freed pid ();
            Pager.free p ~page_id:pid ~freed_at_txn_id:1L)
         pids;
       Pager.set_alloc_min_safe p 1_000_000L;
       let seen = ref [] in
       Pager.set_page_event_callback p (Some (fun ev -> seen := ev :: !seen));
       let _ = List.init n (fun _ -> alloc1 ()) in
       List.for_all
         (function
           | Pager_event.Page_alloc { page_id; reused = true } -> Hashtbl.mem freed page_id
           | _ -> true)
         !seen)
;;
```
Register a `"write"` group and add the prop to a `"props"` group:
```ocaml
    ; ( "write"
      , [ Alcotest.test_case "flush one write per dirty" `Quick test_flush_emits_one_write_per_dirty
        ; Alcotest.test_case "flush_one_to_main emits write" `Quick test_flush_one_to_main_emits_write
        ; Alcotest.test_case "flush empty no writes" `Quick test_flush_empty_no_writes
        ] )
    ; "props", [ QCheck_alcotest.to_alcotest prop_reuse_was_freed ]
```
Also ensure `fill_page` exists in this file; add near the top helpers if absent:
```ocaml
let fill_page byte =
  let buf = Cstruct.create Page.page_size in
  Cstruct.memset buf byte;
  buf
;;
```

- [ ] **Step 2: Run — expect FAIL**

```sh
run dune test test/test_pager_event.exe
```
Expected: `one Page_write per dirty page` got `[]`.

- [ ] **Step 3: Add an `emit_writes` helper and call it in every flush path**

Add the helper near `emit_page_event` (after it):
```ocaml
(* #384: emit one [Page_write] per dirty entry being flushed.  Guard once,
   then iterate — no allocation when no observer is attached. *)
let emit_writes t entries =
  match t.on_page_event with
  | None -> ()
  | Some f ->
    List.iter (fun (pid, _) -> f (Pager_event.Page_write { page_id = pid })) entries
;;
```

In `flush_via_wal`, after a successful append, before clearing dirty (the `Ok ()` arm, ~line 466):
```ocaml
    | Ok () ->
      emit_writes t entries;
      Hashtbl.clear t.dirty;
      Lwt.return_ok ())
```

In `flush_no_sync`'s non-WAL branch: the `entries` are bound at ~line 477; emit after the `write_all entries` loop completes successfully. Simplest: emit right before `write_all entries` is invoked (the writes are about to happen and, on success, all occur):
```ocaml
    let open Lwt.Syntax in
    let rec write_all = function
      ...
    in
    emit_writes t entries;
    write_all entries
```
(Place the `emit_writes t entries;` line immediately before `write_all entries` at the end of the branch.)

In `flush`'s WAL branch `Ok ()` arm (~line 524):
```ocaml
       | Ok () ->
         emit_writes t entries;
         Hashtbl.clear t.dirty;
         Lwt.return_ok ())
```
In `flush`'s non-WAL branch, add `emit_writes t entries;` immediately before the final `write_all entries` (~line 548), same as `flush_no_sync`.

In `flush_one_to_main` (~lines 603-611), the `Ok ()` arm:
```ocaml
  | Ok () ->
    emit_page_event t (Pager_event.Page_write { page_id });
    cache_add t (cache_key_main page_id) (cstruct_dup buf);
    Lwt.return_ok ()
```

> Note: `flush_sync_main` only calls `t.sync ()` (no page writes) — it emits nothing.

- [ ] **Step 4: Run — expect PASS**

```sh
run dune test test/test_pager_event.exe && run dune test test/test_pager.exe
```
Expected: PASS for both (including the QCheck prop).

- [ ] **Step 5: Format and commit**

```sh
for f in lib/storage/pager.ml test/test_pager_event.ml; do
  tmp=$(mktemp) && run ocamlformat "$f" > "$tmp" && mv "$tmp" "$f" && chmod 644 "$f"
done
git add lib/storage/pager.ml test/test_pager_event.ml
git commit -m "feat(#384): emit Page_write per dirty page on flush + reuse QCheck"
```

---

## Task 5: `Store_event` — four new variants + pp/label/txn_id arms

**Files:**
- Modify: `lib/store/store_event.ml`
- Modify: `lib/store/store_event.mli`
- Modify: `test/test_store_event.ml`

- [ ] **Step 1: Write the failing tests**

In `test/test_store_event.ml`, extend `test_pp_all_constructors` with:
```ocaml
  check "PAGE_READ txn=1 page=7" (Ev.Page_read { txn_id = 1L; page = 7L });
  check "PAGE_WRITE txn=2 page=8" (Ev.Page_write { txn_id = 2L; page = 8L });
  check "PAGE_ALLOC txn=3 page=9 reused=true" (Ev.Page_alloc { txn_id = 3L; page = 9L; reused = true });
  check "PAGE_ALLOC txn=3 page=9 reused=false" (Ev.Page_alloc { txn_id = 3L; page = 9L; reused = false });
  check "PAGE_FREE txn=4 page=10" (Ev.Page_free { txn_id = 4L; page = 10L })
```
And extend `test_label_and_txn_id` with a label + txn_id check:
```ocaml
  Alcotest.(check string) "page_read label" "PAGE_READ"
    (Ev.label (Ev.Page_read { txn_id = 5L; page = 1L }));
  Alcotest.(check (option int64)) "page_read txn"
    (Some 5L) (Ev.txn_id (Ev.Page_read { txn_id = 5L; page = 1L }))
```

- [ ] **Step 2: Run — expect FAIL (unknown constructor `Page_read`)**

```sh
run dune test test/test_store_event.exe
```
Expected: build error `Unbound constructor Page_read`.

- [ ] **Step 3: Add variants to `store_event.ml`**

After the `Checkpoint_end` variant (line 27), add:
```ocaml
  | Page_read of
      { txn_id : int64
      ; page : int64
      }
  | Page_write of
      { txn_id : int64
      ; page : int64
      }
  | Page_alloc of
      { txn_id : int64
      ; page : int64
      ; reused : bool
      }
  | Page_free of
      { txn_id : int64
      ; page : int64
      }
```

In `label`, add before the closing `;;`:
```ocaml
  | Page_read _ -> "PAGE_READ"
  | Page_write _ -> "PAGE_WRITE"
  | Page_alloc _ -> "PAGE_ALLOC"
  | Page_free _ -> "PAGE_FREE"
```

In `txn_id`, add the four to the `Some txn_id` group (extend the existing or-pattern):
```ocaml
  | Wal_append { txn_id; _ }
  | Page_read { txn_id; _ }
  | Page_write { txn_id; _ }
  | Page_alloc { txn_id; _ }
  | Page_free { txn_id; _ } -> Some txn_id
```

In `pp`, add before the closing `;;`:
```ocaml
  | Page_read { txn_id; page } -> Format.fprintf fmt "%s txn=%Ld page=%Ld" tag txn_id page
  | Page_write { txn_id; page } -> Format.fprintf fmt "%s txn=%Ld page=%Ld" tag txn_id page
  | Page_alloc { txn_id; page; reused } ->
    Format.fprintf fmt "%s txn=%Ld page=%Ld reused=%b" tag txn_id page reused
  | Page_free { txn_id; page } -> Format.fprintf fmt "%s txn=%Ld page=%Ld" tag txn_id page
```

- [ ] **Step 4: Update `store_event.mli`**

Mirror the four variants in the `type t` declaration in `store_event.mli` (add the same constructors after `Checkpoint_end`). The `label`/`txn_id`/`pp` signatures are unchanged.

- [ ] **Step 5: Run — expect PASS**

```sh
run dune test test/test_store_event.exe
```
Expected: PASS.

- [ ] **Step 6: Format and commit**

```sh
for f in lib/store/store_event.ml lib/store/store_event.mli test/test_store_event.ml; do
  tmp=$(mktemp) && run ocamlformat "$f" > "$tmp" && mv "$tmp" "$f" && chmod 644 "$f"
done
git add lib/store/store_event.ml lib/store/store_event.mli test/test_store_event.ml
git commit -m "feat(#384): add Page_* variants to Store_event (pp/label/txn_id)"
```

---

## Task 6: Wire the translator in `Store.set_event_callback`

**Files:**
- Modify: `lib/store/store.ml` (module alias; `translate_pager_event`; `set_event_callback`)
- Modify: `test/test_store_event.ml`

- [ ] **Step 1: Write the failing tests**

In `test/test_store_event.ml`, add store-level page-event tests. `with_recorder` records labels only; add a full-event recorder helper near it:
```ocaml
(* Collect full events (not just labels) while [f] runs. *)
let with_event_recorder ~f =
  let path = fresh_path () in
  cleanup path;
  let seen = ref [] in
  Lwt.finalize
    (fun () ->
       let open Lwt.Syntax in
       let* st = S.open_file_wal ~path () in
       let st = Result.get_ok st in
       S.set_event_callback st (Some (fun ev -> seen := ev :: !seen));
       let* () = f st in
       let* () = S.close st in
       Lwt.return (List.rev !seen))
    (fun () ->
       cleanup path;
       Lwt.return_unit)
  |> run
;;
```
Then:
```ocaml
let test_insert_emits_page_events () =
  let evs =
    with_event_recorder ~f:(fun st ->
      let open Lwt.Syntax in
      let* txn = S.rw_begin st in
      let* () = S.put txn 16 (bs "k") (bs "v") in
      S.commit txn)
  in
  let has p = List.exists p evs in
  Alcotest.(check bool) "has PAGE_ALLOC" true
    (has (function Ev.Page_alloc _ -> true | _ -> false));
  Alcotest.(check bool) "has PAGE_WRITE" true
    (has (function Ev.Page_write _ -> true | _ -> false));
  (* page-write events carry the committing txn id (>0) *)
  Alcotest.(check bool) "page write txn > 0" true
    (has (function Ev.Page_write { txn_id; _ } -> txn_id > 0L | _ -> false))
;;

let test_checkpoint_then_read_emits_page_read () =
  let evs =
    with_event_recorder ~f:(fun st ->
      let open Lwt.Syntax in
      let* txn = S.rw_begin st in
      let* () = S.put txn 16 (bs "k") (bs "v") in
      let* () = S.commit txn in
      let* () = S.checkpoint st in
      (* after checkpoint, reading from a fresh RO txn forces a backend read *)
      let* txn2 = S.rw_begin st in
      let* _ = S.get txn2 16 (bs "k") in
      S.commit txn2)
  in
  Alcotest.(check bool) "has PAGE_READ" true
    (List.exists (function Ev.Page_read _ -> true | _ -> false) evs)
;;
```
Register both in the `"seam"` group list.

> If `S.get` is not the read accessor in this store API, use the same accessor the existing `test_store.ml` uses for a point read; the goal is simply to force a backend page read after the cache is cold. Confirm by grepping `test/test_store.ml` for the read function name before writing the test.

- [ ] **Step 2: Run — expect FAIL**

```sh
run dune test test/test_store_event.exe
```
Expected: `has PAGE_ALLOC` is false (translator not wired yet).

- [ ] **Step 3: Add the module alias**

In `lib/store/store.ml`, after `module Freelist = Sqlocaml_storage.Freelist` (line 19), add:
```ocaml
module Pager_event = Sqlocaml_storage.Pager_event
```

- [ ] **Step 4: Add the translator and wire the pager hook**

Replace `set_event_callback` (lines 3253-3256) with:
```ocaml
(* #384: map a storage-level [Pager_event.t] to a [Store_event.t], stamping the
   txn id from the pager.  Write-path events (alloc/write/free) always fire
   inside an active RW txn, so they carry the exact id; [Page_read] may fire
   outside a write txn, where [get_txn_id] returns 0 before the first txn and the
   most-recent txn id between txns (best-effort, documented in the spec). *)
let translate_pager_event (st : bt_state) (pev : Pager_event.t) : Store_event.t =
  let txn_id = Pager.get_txn_id st.pager in
  match pev with
  | Pager_event.Page_read { page_id } -> Store_event.Page_read { txn_id; page = page_id }
  | Pager_event.Page_write { page_id } -> Store_event.Page_write { txn_id; page = page_id }
  | Pager_event.Page_alloc { page_id; reused } ->
    Store_event.Page_alloc { txn_id; page = page_id; reused }
  | Pager_event.Page_free { page_id } -> Store_event.Page_free { txn_id; page = page_id }
;;

let set_event_callback (t : t) (cb : (Store_event.t -> unit) option) =
  match t.backend with
  | Mem _ -> () (* Mem backend has no bt_state; emits no events (#382). *)
  | Btree st ->
    st.on_event <- cb;
    (match cb with
     | None -> Pager.set_page_event_callback st.pager None
     | Some f ->
       Pager.set_page_event_callback
         st.pager
         (Some
            (fun pev ->
              (* Same guarantee as [emit_event]: a faulty observer must never
                 break a transaction. *)
              try f (translate_pager_event st pev) with
              | _ -> ())))
;;
```

- [ ] **Step 5: Run — expect PASS**

```sh
run dune test test/test_store_event.exe
```
Expected: PASS.

- [ ] **Step 6: Format and commit**

```sh
for f in lib/store/store.ml test/test_store_event.ml; do
  tmp=$(mktemp) && run ocamlformat "$f" > "$tmp" && mv "$tmp" "$f" && chmod 644 "$f"
done
run dune build
git add lib/store/store.ml test/test_store_event.ml
git commit -m "feat(#384): translate Pager_event -> Store_event in set_event_callback"
```

---

## Task 7: #386 — thread the real frame count into `Txn_commit`

**Files:**
- Modify: `lib/store/store.ml` (`commit_wal` return type; `commit`)
- Modify: `test/test_store_event.ml`

- [ ] **Step 1: Write the failing test**

In `test/test_store_event.ml`:
```ocaml
let test_txn_commit_frames_matches_wal_append () =
  let evs =
    with_event_recorder ~f:(fun st ->
      let open Lwt.Syntax in
      let* txn = S.rw_begin st in
      let* () = S.put txn 16 (bs "k") (bs "v") in
      S.commit txn)
  in
  let commit_frames =
    List.find_map (function Ev.Txn_commit { frames; _ } -> Some frames | _ -> None) evs
  in
  let append_count =
    List.find_map (function Ev.Wal_append { count; _ } -> Some count | _ -> None) evs
  in
  Alcotest.(check (option int)) "commit frames present" (Some (Option.get append_count)) commit_frames;
  Alcotest.(check bool) "commit frames > 0 for a real write" true
    (match commit_frames with Some n -> n > 0 | None -> false)
;;
```
Register it in the `"seam"` group.

- [ ] **Step 2: Run — expect FAIL (frames is 0, not the append count)**

```sh
run dune test test/test_store_event.exe
```
Expected: `commit frames present` fails — got `Some 0`, expected `Some <count>`.

- [ ] **Step 3: Make `commit_wal` return the appended count**

In `commit_wal`, change the success tail (line 1893-1898) from:
```ocaml
       let appended = frames_after - frames_before in
       emit_event
         st
         (Store_event.Wal_append
            { txn_id = append_txn_id; base_idx = frames_before; count = appended });
       Lwt.return_unit)
```
to:
```ocaml
       let appended = frames_after - frames_before in
       emit_event
         st
         (Store_event.Wal_append
            { txn_id = append_txn_id; base_idx = frames_before; count = appended });
       Lwt.return appended)
```
The exception handler `(fun exn -> unlock_once (); Lwt.fail exn)` already returns a polymorphic `'a Lwt.t`, so the type now unifies to `int Lwt.t`.

- [ ] **Step 4: Thread the count through `commit`**

In `commit` (Btree branch), change the `let* () = match st.wal with ... in` block (lines 1932-1944) so the match yields the frame count and the emit uses it:
```ocaml
    let committed_id = active_txn_id st in
    let* frames =
      match st.wal with
      | None ->
        Lwt.finalize
          (fun () ->
             let* () = commit_prepare_btree ~header_commit:Header.commit st in
             maybe_autocheckpoint st)
          (fun () ->
             Rwlock.release_write t.lock;
             Lwt.return_unit)
        |> fun p ->
        let* () = p in
        Lwt.return 0 (* non-WAL: no WAL frames appended *)
      | Some _ -> commit_wal t st
    in
    emit_event st (Store_event.Txn_commit { txn_id = committed_id; frames });
    Lwt.return_unit
```
Update the placeholder comment just above (lines 1927-1930) to reflect that the count is now real:
```ocaml
    (* #382/#386: capture the id this txn commits as BEFORE the header is bumped
       (commit_prepare_btree advances [st.current_header.txn_id]).  [frames] is
       the authoritative WAL-appended count returned by [commit_wal] (0 for the
       non-WAL path, which appends no WAL frames). *)
```

> Style note: if the `|> fun p -> let* () = p in Lwt.return 0` shape trips ocamlformat or reads awkwardly, prefer the cleaner equivalent:
> ```ocaml
>       | None ->
>         let* () =
>           Lwt.finalize
>             (fun () ->
>                let* () = commit_prepare_btree ~header_commit:Header.commit st in
>                maybe_autocheckpoint st)
>             (fun () ->
>                Rwlock.release_write t.lock;
>                Lwt.return_unit)
>         in
>         Lwt.return 0
> ```
> Use this second form.

- [ ] **Step 5: Run — expect PASS**

```sh
run dune test test/test_store_event.exe
```
Expected: PASS. Then run the broader store/WAL suites to confirm no regression:
```sh
run dune test test/test_store.exe && run dune test test/test_store_wal.exe
```
Expected: PASS.

- [ ] **Step 6: Format and commit**

```sh
tmp=$(mktemp) && run ocamlformat lib/store/store.ml > "$tmp" && mv "$tmp" lib/store/store.ml && chmod 644 lib/store/store.ml
tmp=$(mktemp) && run ocamlformat test/test_store_event.ml > "$tmp" && mv "$tmp" test/test_store_event.ml && chmod 644 test/test_store_event.ml
run dune build
git add lib/store/store.ml test/test_store_event.ml
git commit -m "fix(#386): Txn_commit reports the real WAL frame count"
```

---

## Task 8: Full verification, coverage, dune-file format, and PR

**Files:** none (verification + PR)

- [ ] **Step 1: Full build + full test suite**

```sh
run dune build 2>&1 | tail -20
run dune test 2>&1 | tail -40
```
Expected: clean build; all tests PASS.

- [ ] **Step 2: Format-check every touched source + dune file**

```sh
for f in lib/storage/pager_event.ml lib/storage/pager.ml lib/storage/pager.mli \
         lib/store/store_event.ml lib/store/store_event.mli lib/store/store.ml \
         test/test_pager_event.ml test/test_store_event.ml; do
  run ocamlformat --check "$f" || echo "NEEDS FORMAT: $f"
done
# dune files (lint gate checks these; @fmt errors in worktrees, so format in place):
tmp=$(mktemp) && run dune format-dune-file test/dune > "$tmp" && mv "$tmp" test/dune && chmod 644 test/dune
git diff --stat test/dune
```
Expected: no `NEEDS FORMAT` lines; `test/dune` unchanged or re-formatted cleanly.

- [ ] **Step 3: Coverage on the new/changed modules**

```sh
run sh -c 'rm -f _build/default/test/*.coverage; dune test; bisect-ppx-report summary' 2>&1 | \
  grep -iE "pager_event|pager\.ml|store_event|store\.ml" || true
```
Inspect that `pager_event.ml` is 100% and the new arms in `store_event.ml` are covered. If any new line is uncovered, add a targeted test in the relevant `test_*` file and re-commit. (Lines in `store.ml` outside the changed translator/commit code are pre-existing and not in scope.)

- [ ] **Step 4: Commit any coverage-driven test additions**

```sh
git add -A && git commit -m "test(#384): cover remaining Page_* event arms" || echo "nothing to commit"
```

- [ ] **Step 5: Push and open the PR**

```sh
git push origin feat/384-pager-events
~/.local/bin/forgejo pr create tej/sqlite_ocaml_port \
  --title="feat(#384): internals monitor — page I/O, alloc/free events + #386 frames" \
  --head=feat/384-pager-events \
  --base=main \
  --body="$(cat <<'EOF'
## Summary
- New `Pager_event.t` storage-local signal + `Pager.set_page_event_callback` hook (zero-overhead when unset).
- Emit physical-I/O events: `Page_read` (cache-miss backend read), `Page_write` (per dirty page on flush), `Page_alloc` (with `reused` flag: freelist/txn-pool vs file-extend), `Page_free`.
- Four matching `Store_event.t` variants; `Store.set_event_callback` installs a translator that stamps `txn_id` and swallows observer exceptions.
- COW is inferred by the monitor from alloc-reused + free pairs (no btree changes).
- #386: `Txn_commit` now reports the real WAL-appended frame count (threaded out of `commit_wal`) instead of the hardwired `0`.

## Test plan
- [ ] `dune test` passes
- [ ] `test_pager_event.ml`: read (miss/hit/borrow), alloc (extend/freelist/txn-pool), free, write (per-dirty/single/empty), reuse QCheck invariant, zero-overhead when unset
- [ ] `test_store_event.ml`: store-level Page_* during insert; Page_read after checkpoint; `Txn_commit.frames` == `Wal_append.count` and > 0

Closes #384
Closes #386
EOF
)"
```

- [ ] **Step 6: Report the PR URL** back to the requester.

---

## Self-review notes (already reconciled)

- **Spec coverage:** all five spec decisions map to tasks — page grain (Tasks 2/4), COW-via-alloc/free (Task 3, `reused` flag), #386 thread-the-count (Task 7), per-dirty `Page_write` (Task 4), txn_id from `get_txn_id` (Task 6). New types (Tasks 1/5), wiring (Task 6), tests/coverage (all tasks + Task 8).
- **Type consistency:** `set_page_event_callback`, `on_page_event`, `emit_page_event`, `emit_writes`, `translate_pager_event`, and the `Pager_event` / `Store_event` constructor names are used identically across all tasks.
- **Pre-existing-API check flagged:** Task 6 Step 1 notes to confirm the store read accessor name (`S.get`) against `test_store.ml` before writing that one test.
