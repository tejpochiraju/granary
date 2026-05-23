# Phase 4 — Mirage_block Backend Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a `Mirage_block.S` adapter so sqlocaml's B+-tree storage runs on any MirageOS block device, with a multi-backend conformance test proving Mem + Unix_file + Mirage_block produce identical SQL results.

**Architecture:** A `Mirage_backend.Make(B: Mirage_block.S)` functor adapts any Mirage block device to our `Block.S` callback signature. `Store.open_block` accepts generic I/O callbacks, replacing the `Unix_file.t` field in `bt_state` with a `close_fn` closure. A multi-backend test instantiates all three backends and runs the same SQL suite against each.

**Tech Stack:** OCaml 5.1, dune 3.x, lwt, cstruct, mirage-block 3.0.2, mirage-block-unix 2.14.2 (file-backed Mirage_block.S for testing), alcotest. Builds run via `podman run --rm -v $(pwd):/workspace:Z -w /workspace sqlocaml-dev`.

---

## Background / Key design decisions

### Why `Pager.set_n_pages` is needed

Mirage block devices have **fixed physical size** (pre-allocated sectors) but variable *logical* page usage. When `Store.open_block` initialises a fresh device it must tell the Pager "pages 0 and 1 are now reserved" without physically resizing anything. Adding `Pager.set_n_pages` (a 1-line setter) solves this cleanly.

### Mirage adapter bounds-checking

The adapter's `read_page`/`write_page` callbacks bound-check against `t.capacity` (total device pages), **not** against `t.n_pages` (logical DB page count). This lets `Header.read_live` probe pages 0 and 1 even before the store has "claimed" them.

### `open_block` vs `open_file` initialization

`open_file` detects a fresh file by checking `Unix_file.n_pages = 0`. `open_block` always probes for valid headers first; if both are corrupt (`Both_headers_corrupt`) the device is treated as fresh. This is safer and works for pre-sized Mirage devices.

### Adapter handles resize as no-op

`resize ~n_pages` in the Mirage adapter just validates `n_pages ≤ capacity` and updates the internal counter — no physical I/O. The device is pre-allocated.

---

## File map

| Path | Action | Notes |
|------|--------|-------|
| `lib/block/mirage_backend.ml` | **CREATE** | `Make(B: Mirage_block.S)` functor |
| `lib/block/mirage_backend.mli` | **CREATE** | Public interface for functor |
| `lib/block/dune` | **MODIFY** | Add `sqlocaml.mirage_block` library stanza |
| `lib/storage/pager.ml` | **MODIFY** | Add `set_n_pages` |
| `lib/storage/pager.mli` | **MODIFY** | Expose `set_n_pages` |
| `lib/store/store.ml` | **MODIFY** | Replace `file: Unix_file.t` → `close_fn`, add `open_block` |
| `lib/store/store.mli` | **MODIFY** | Expose `open_block` |
| `lib/db/db.ml` | **MODIFY** | Add `open_block` |
| `lib/db/db.mli` | **MODIFY** | Expose `open_block` |
| `test/test_mirage_backend.ml` | **CREATE** | Unit tests for Mirage adapter |
| `test/test_all_backends.ml` | **CREATE** | Multi-backend SQL conformance |
| `test/dune` | **MODIFY** | Add two new test stanzas |
| `Containerfile` | **MODIFY** | Add mirage-block packages |
| `.forgejo/workflows/ci.yml` | **MODIFY** | Add mirage-block packages |
| `dune-project` | **MODIFY** | Add mirage-block dep |
| `ROADMAP.md` | **MODIFY** | Mark Phase 4 complete |

---

## Task 1: Mirage_block adapter library

**Files:**
- Create: `lib/block/mirage_backend.ml`
- Create: `lib/block/mirage_backend.mli`
- Modify: `lib/block/dune`
- Modify: `Containerfile`
- Modify: `dune-project`
- Create: `test/test_mirage_backend.ml`
- Modify: `test/dune`

- [ ] **Step 1: Update Containerfile to add mirage-block packages**

```
FROM docker.io/ocaml/opam:ubuntu-24.04-ocaml-5.1
USER root
RUN apt-get update && apt-get install -y pkg-config libgmp-dev
USER opam
RUN opam install -y lwt cstruct menhir alcotest qcheck-alcotest lwt_ppx bisect_ppx mirage-block mirage-block-unix
WORKDIR /workspace
ENTRYPOINT ["opam", "exec", "--"]
```

- [ ] **Step 2: Rebuild sqlocaml-dev container**

```bash
podman build -t sqlocaml-dev /home/tej/projects/sqlite_ocaml_port
```

Expected: build completes, "sqlocaml-dev" image updated.

- [ ] **Step 3: Update dune-project to declare mirage-block dependency**

In `dune-project`, add to the `(package sqlocaml ...)` `(depends ...)` list:

```
(mirage-block (>= "3.0.0"))
(mirage-block-unix (>= "2.14.0"))
```

Full updated depends block:

```
(depends
  (ocaml (>= "5.1"))
  (dune (>= "3.16"))
  (lwt (>= "5.7"))
  (cstruct (>= "6.2"))
  (menhir (>= "20240715"))
  (mirage-block (>= "3.0.0"))
  (mirage-block-unix (>= "2.14.0"))
  (alcotest :with-test)
  (qcheck-alcotest :with-test)
  (odoc :with-doc))
```

- [ ] **Step 4: Create `lib/block/mirage_backend.mli`**

```ocaml
(** Mirage_block.S adapter.  Wraps any [Mirage_block.S] implementation
    so it can be used as a sqlocaml block backend via [Store.open_block].

    Usage:
      module MB = Mirage_backend.Make(Block)   (* Block = mirage-block-unix *)
      let* dev = Block.connect path in
      let* adapter = MB.connect dev in
      let* store = Store.open_block
        ~read_page:(MB.read_page adapter)
        ~write_page:(MB.write_page adapter)
        ~sync:(MB.sync adapter)
        ~resize:(MB.resize adapter)
        ~n_pages:(MB.n_pages adapter)
        ~close:(fun () -> MB.close adapter) in
      ...
*)

module Make (B : Mirage_block.S) : sig
  type t
  (** Adapter state: wraps [B.t] with page-granularity access. *)

  val connect : B.t -> t Lwt.t
  (** [connect dev] reads [get_info] from [dev] to determine sector size
      and capacity, then creates an adapter with logical [n_pages = 0].
      Raises [Failure] if [page_size (4096)] is not a multiple of the
      device's [sector_size]. *)

  val n_pages  : t -> int64
  (** Logical page count (updated by [resize]). Starts at 0 after [connect]. *)

  val read_page  : t -> page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t
  val write_page : t -> page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t
  val sync       : t -> unit -> (unit, string) result Lwt.t
  val resize     : t -> n_pages:int64 -> (unit, string) result Lwt.t
  (** [resize t ~n_pages] validates [n_pages ≤ capacity] and updates the
      logical count.  The physical device is not modified. *)

  val close : t -> unit Lwt.t
  (** Calls [B.disconnect]. *)
end
```

- [ ] **Step 5: Create `lib/block/mirage_backend.ml`**

```ocaml
open Lwt.Syntax

let page_size = 4096

module Make (B : Mirage_block.S) = struct

  type t = {
    dev              : B.t;
    sectors_per_page : int;
    capacity         : int64;
    mutable n_pages  : int64;
  }

  let connect dev =
    let* info = B.get_info dev in
    let sector_size = info.Mirage_block.sector_size in
    if page_size mod sector_size <> 0 then
      Lwt.fail_with (Printf.sprintf
        "mirage_backend: page_size %d not divisible by sector_size %d"
        page_size sector_size)
    else
      let sectors_per_page = page_size / sector_size in
      let capacity =
        Int64.div info.Mirage_block.size_sectors (Int64.of_int sectors_per_page)
      in
      Lwt.return { dev; sectors_per_page; capacity; n_pages = 0L }

  let n_pages t = t.n_pages

  let in_capacity t page_id =
    Int64.compare page_id 0L >= 0 && Int64.compare page_id t.capacity < 0

  let read_page t ~page_id buf =
    if not (in_capacity t page_id) then
      Lwt.return (Error (Printf.sprintf
        "read_page: page_id=%Ld out of capacity=%Ld" page_id t.capacity))
    else
      let sector = Int64.mul page_id (Int64.of_int t.sectors_per_page) in
      let* r = B.read t.dev sector [buf] in
      Lwt.return (Result.map_error (Format.asprintf "%a" B.pp_error) r)

  let write_page t ~page_id buf =
    if not (in_capacity t page_id) then
      Lwt.return (Error (Printf.sprintf
        "write_page: page_id=%Ld out of capacity=%Ld" page_id t.capacity))
    else
      let sector = Int64.mul page_id (Int64.of_int t.sectors_per_page) in
      let* r = B.write t.dev sector [buf] in
      Lwt.return (Result.map_error (Format.asprintf "%a" B.pp_write_error) r)

  let sync _t () = Lwt.return (Ok ())

  let resize t ~n_pages =
    if Int64.compare n_pages t.capacity > 0 then
      Lwt.return (Error (Printf.sprintf
        "resize: %Ld pages exceeds device capacity %Ld" n_pages t.capacity))
    else begin
      t.n_pages <- n_pages;
      Lwt.return (Ok ())
    end

  let close t = B.disconnect t.dev
end
```

- [ ] **Step 6: Update `lib/block/dune` to add the new library**

Current `lib/block/dune`:
```
(library
 (name sqlocaml_block)
 (public_name sqlocaml.block)
 (libraries lwt lwt.unix unix cstruct)
 (instrumentation (backend bisect_ppx)))
```

New `lib/block/dune`:
```
(library
 (name sqlocaml_block)
 (public_name sqlocaml.block)
 (libraries lwt lwt.unix unix cstruct)
 (instrumentation (backend bisect_ppx)))

(library
 (name sqlocaml_mirage_block)
 (public_name sqlocaml.mirage_block)
 (libraries mirage-block lwt cstruct)
 (instrumentation (backend bisect_ppx)))
```

- [ ] **Step 7: Write `test/test_mirage_backend.ml`**

The test creates a 1 MB temp file, connects via `Block.connect` (mirage-block-unix), wraps with the adapter, then reads/writes a page and checks round-trip correctness.

```ocaml
open Lwt.Syntax
module MB = Sqlocaml_mirage_block.Mirage_backend.Make(Block)

let tmp_file () =
  let path = Filename.temp_file "sqlocaml_mb_test" ".raw" in
  (* Pre-allocate 1 MB so Block.connect sees a valid file *)
  let fd = Unix.openfile path [Unix.O_RDWR; Unix.O_CREAT] 0o644 in
  Unix.ftruncate fd (1024 * 1024);
  Unix.close fd;
  path

let test_connect_fresh () =
  let path = tmp_file () in
  Fun.protect ~finally:(fun () -> Unix.unlink path) (fun () ->
    Lwt_main.run (
      let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
      let* adapter = MB.connect dev in
      let n = MB.n_pages adapter in
      (* Fresh adapter always starts at 0 regardless of physical file size *)
      Alcotest.(check int64) "n_pages starts at 0" 0L n;
      let* () = MB.close adapter in
      Lwt.return_unit
    )
  )

let test_read_write_roundtrip () =
  let path = tmp_file () in
  Fun.protect ~finally:(fun () -> Unix.unlink path) (fun () ->
    Lwt_main.run (
      let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
      let* adapter = MB.connect dev in
      (* Write a page with a known pattern *)
      let buf_w = Cstruct.create 4096 in
      Cstruct.set_uint8 buf_w 0 0xAB;
      Cstruct.set_uint8 buf_w 4095 0xCD;
      let* wr = MB.write_page adapter ~page_id:0L buf_w in
      Alcotest.(check (result unit string)) "write ok" (Ok ()) wr;
      (* Read it back *)
      let buf_r = Cstruct.create 4096 in
      let* rr = MB.read_page adapter ~page_id:0L buf_r in
      Alcotest.(check (result unit string)) "read ok" (Ok ()) rr;
      Alcotest.(check int) "byte 0" 0xAB (Cstruct.get_uint8 buf_r 0);
      Alcotest.(check int) "byte 4095" 0xCD (Cstruct.get_uint8 buf_r 4095);
      let* () = MB.close adapter in
      Lwt.return_unit
    )
  )

let test_out_of_capacity () =
  let path = tmp_file () in
  Fun.protect ~finally:(fun () -> Unix.unlink path) (fun () ->
    Lwt_main.run (
      let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
      let* adapter = MB.connect dev in
      (* 1 MB / 4096 = 256 pages; page 256 is beyond capacity *)
      let buf = Cstruct.create 4096 in
      let* rr = MB.read_page adapter ~page_id:256L buf in
      Alcotest.(check bool) "read OOB is error" true (Result.is_error rr);
      let* wr = MB.write_page adapter ~page_id:256L buf in
      Alcotest.(check bool) "write OOB is error" true (Result.is_error wr);
      let* () = MB.close adapter in
      Lwt.return_unit
    )
  )

let test_resize_within_capacity () =
  let path = tmp_file () in
  Fun.protect ~finally:(fun () -> Unix.unlink path) (fun () ->
    Lwt_main.run (
      let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
      let* adapter = MB.connect dev in
      let* rr = MB.resize adapter ~n_pages:10L in
      Alcotest.(check (result unit string)) "resize within cap ok" (Ok ()) rr;
      Alcotest.(check int64) "n_pages updated" 10L (MB.n_pages adapter);
      let* () = MB.close adapter in
      Lwt.return_unit
    )
  )

let test_resize_beyond_capacity () =
  let path = tmp_file () in
  Fun.protect ~finally:(fun () -> Unix.unlink path) (fun () ->
    Lwt_main.run (
      let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
      let* adapter = MB.connect dev in
      (* 1 MB = 256 pages; requesting 300 pages fails *)
      let* rr = MB.resize adapter ~n_pages:300L in
      Alcotest.(check bool) "resize beyond cap is error" true (Result.is_error rr);
      let* () = MB.close adapter in
      Lwt.return_unit
    )
  )

let test_sync_always_ok () =
  let path = tmp_file () in
  Fun.protect ~finally:(fun () -> Unix.unlink path) (fun () ->
    Lwt_main.run (
      let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
      let* adapter = MB.connect dev in
      let* sr = MB.sync adapter () in
      Alcotest.(check (result unit string)) "sync ok" (Ok ()) sr;
      let* () = MB.close adapter in
      Lwt.return_unit
    )
  )

let () =
  let open Alcotest in
  run "mirage_backend" [
    "adapter", [
      test_case "connect_fresh"             `Quick test_connect_fresh;
      test_case "read_write_roundtrip"      `Quick test_read_write_roundtrip;
      test_case "out_of_capacity"           `Quick test_out_of_capacity;
      test_case "resize_within_capacity"    `Quick test_resize_within_capacity;
      test_case "resize_beyond_capacity"    `Quick test_resize_beyond_capacity;
      test_case "sync_always_ok"            `Quick test_sync_always_ok;
    ]
  ]
```

- [ ] **Step 8: Add test stanza to `test/dune`**

Add at the end of `test/dune`:

```
(test
 (name test_mirage_backend)
 (libraries sqlocaml.mirage_block mirage-block-unix alcotest lwt.unix unix))
```

- [ ] **Step 9: Build and run**

```bash
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_mirage_backend.exe 2>&1
```

Expected: 6 tests pass with no errors.

- [ ] **Step 10: Commit**

```bash
git add lib/block/mirage_backend.ml lib/block/mirage_backend.mli lib/block/dune \
        test/test_mirage_backend.ml test/dune \
        Containerfile dune-project sqlocaml.opam
git commit -m "feat(block): add Mirage_block.S adapter library (sqlocaml.mirage_block)"
```

---

## Task 2: Pager.set_n_pages + Store.open_block + bt_state refactor

**Files:**
- Modify: `lib/storage/pager.ml`
- Modify: `lib/storage/pager.mli`
- Modify: `lib/store/store.ml`
- Modify: `lib/store/store.mli`

### 2a — Add `Pager.set_n_pages`

- [ ] **Step 1: Add `set_n_pages` to `lib/storage/pager.mli`**

After the existing `set_alloc_min_safe` line, add:

```ocaml
val set_n_pages : t -> int64 -> unit
(** Override the pager's current page count.  Used by [Store.open_block]
    after probing headers to set the authoritative logical page count
    without going through the resize callback. *)
```

- [ ] **Step 2: Add implementation to `lib/storage/pager.ml`**

After the existing `set_alloc_min_safe` implementation, add:

```ocaml
let set_n_pages t n = t.n_pages <- n
```

### 2b — Refactor `bt_state` to remove `Unix_file` coupling

- [ ] **Step 3: Replace `file : Unix_file.t` with `close_fn` in `store.ml`**

In `lib/store/store.ml`, find:

```ocaml
type bt_state = {
  file                 : Unix_file.t;
  pager                : Pager.t;
```

Replace with:

```ocaml
type bt_state = {
  close_fn             : unit -> unit Lwt.t;
  pager                : Pager.t;
```

- [ ] **Step 4: Update `open_file` to use `close_fn`**

In `open_file`, the fresh branch currently builds `bt_state` as:
```ocaml
let st =
  { file; pager; meta; trees = Hashtbl.create 16; current_header = h;
    schema_version = h.schema_version; txn_freelist_snapshot = None;
    active_readers = Hashtbl.create 4 }
```

Change both occurrences (fresh and existing branch) to:
```ocaml
let close_fn () = let%lwt _ = Unix_file.close file in Lwt.return_unit in
let st =
  { close_fn; pager; meta; trees = Hashtbl.create 16; current_header = h;
    schema_version = h.schema_version; txn_freelist_snapshot = None;
    active_readers = Hashtbl.create 4 }
```

- [ ] **Step 5: Update `close` to use `close_fn`**

Find:
```ocaml
let close (t : t) : unit Lwt.t =
  match t.backend with
  | Mem _ -> Lwt.return_unit
  | Btree st ->
    let%lwt _ = Unix_file.close st.file in
    Lwt.return_unit
```

Replace with:
```ocaml
let close (t : t) : unit Lwt.t =
  match t.backend with
  | Mem _ -> Lwt.return_unit
  | Btree st -> st.close_fn ()
```

### 2c — Add `open_block`

- [ ] **Step 6: Add `open_block` to `lib/store/store.ml`**

Add after the `close` function. This function reuses the same freelist-reading and header helpers that `open_file` uses, so forward-declare them (or place after `read_freelist_pages`). The function must be placed AFTER `read_freelist_pages` in the file.

```ocaml
(* Generic block-device open: accepts I/O callbacks from any backend.
   Probes headers to detect fresh vs. existing DB.
   For fresh devices (Both_headers_corrupt): initialises two header pages.
   For existing devices: restores freelist and tree roots from the header.
   [n_pages] is the INITIAL logical page count; 0 means the caller's
   resize callback handles physical sizing (correct for Mirage adapters). *)
let open_block
    ~(read_page  : page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
    ~(write_page : page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
    ~(sync       : unit -> (unit, string) result Lwt.t)
    ~(resize     : n_pages:int64 -> (unit, string) result Lwt.t)
    ~(n_pages    : int64)
    ~(close      : unit -> unit Lwt.t)
    : (t, error) result Lwt.t =
  let pager =
    Pager.create ~read_page ~write_page ~sync ~resize ~n_pages ~freelist:Freelist.empty
  in
  let%lwt hr = Header.read_live pager in
  match hr with
  | Error Header.Both_headers_corrupt ->
    (* Fresh device — initialise two header pages *)
    let%lwt ir = Header.init pager in
    (match ir with
    | Error e -> Lwt.return_error (map_header_err e)
    | Ok () ->
      (* pages 0 and 1 are now reserved; tell the pager *)
      Pager.set_n_pages pager 2L;
      let%lwt hr2 = Header.read_live pager in
      (match hr2 with
      | Error e -> Lwt.return_error (map_header_err e)
      | Ok h ->
        let meta = Btree.create pager ~root_page:0L in
        let st =
          { close_fn = close; pager; meta;
            trees = Hashtbl.create 16;
            current_header = h;
            schema_version = h.schema_version;
            txn_freelist_snapshot = None;
            active_readers = Hashtbl.create 4 }
        in
        Lwt.return_ok
          { backend = Btree st; rw_mutex = Lwt_mutex.create ();
            mem_rw_snapshot = None }))
  | Error (Header.Io s) -> Lwt.return_error (Header_error s)
  | Ok h ->
    (* Existing DB *)
    Pager.set_n_pages pager h.n_pages_total;
    let%lwt fl = read_freelist_pages pager ~first_page:h.freelist_page in
    Pager.set_freelist pager fl;
    let meta = Btree.create pager ~root_page:h.root_page in
    let st =
      { close_fn = close; pager; meta;
        trees = Hashtbl.create 16;
        current_header = h;
        schema_version = h.schema_version;
        txn_freelist_snapshot = None;
        active_readers = Hashtbl.create 4 }
    in
    Lwt.return_ok
      { backend = Btree st; rw_mutex = Lwt_mutex.create ();
        mem_rw_snapshot = None }
```

- [ ] **Step 7: Add `open_block` to `lib/store/store.mli`**

After the `open_file` comment block, add:

```ocaml
(** Open a B+-tree backed store from any block device, given as I/O callbacks.
    This is the generic entry point for non-Unix backends (e.g. Mirage_block).

    [n_pages] is the initial logical page count (pass [0L] for Mirage adapters
    that bound-check internally against device capacity).
    [close] is called by [Store.close] to release the device.

    The function probes pages 0 and 1 for valid headers:
    - Both corrupt → fresh device: writes initial headers, starts with 2 pages.
    - One valid → existing database: restores from that header. *)
val open_block :
  read_page  : (page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t) ->
  write_page : (page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t) ->
  sync       : (unit -> (unit, string) result Lwt.t) ->
  resize     : (n_pages:int64 -> (unit, string) result Lwt.t) ->
  n_pages    : int64 ->
  close      : (unit -> unit Lwt.t) ->
  (t, error) result Lwt.t
```

- [ ] **Step 8: Add tests to `test/test_store_btree.ml`**

Add a new `"open_block"` test suite. These tests use mirage-block-unix's `Block` module to exercise `Store.open_block` with a real Mirage adapter.

```ocaml
(* At top of file, add: *)
module MB = Sqlocaml_mirage_block.Mirage_backend.Make(Block)

let tmp_block_file size_mb =
  let path = Filename.temp_file "sqlocaml_ob_test" ".raw" in
  let fd = Unix.openfile path [Unix.O_RDWR; Unix.O_CREAT] 0o644 in
  Unix.ftruncate fd (size_mb * 1024 * 1024);
  Unix.close fd;
  path

let with_block_store path f =
  Lwt_main.run (
    let open Lwt.Syntax in
    let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
    let* adapter = MB.connect dev in
    let* result = S.open_block
      ~read_page:(MB.read_page adapter)
      ~write_page:(MB.write_page adapter)
      ~sync:(MB.sync adapter)
      ~resize:(MB.resize adapter)
      ~n_pages:(MB.n_pages adapter)
      ~close:(fun () -> MB.close adapter)
    in
    match result with
    | Error e ->
      Alcotest.failf "open_block failed: %a" S.pp_error e
    | Ok store ->
      let* () = f store in
      S.close store
  )

let test_open_block_fresh () =
  let path = tmp_block_file 4 in
  Fun.protect ~finally:(fun () -> Unix.unlink path) (fun () ->
    with_block_store path (fun store ->
      let open Lwt.Syntax in
      let tx = Lwt_main.run (S.rw_begin store) in
      let* tid = S.get_or_create_tree store tx 100 in
      let* () = S.put store tx tid (Bytes.of_string "hello") (Bytes.of_string "world") in
      S.commit tx
    )
  )

let test_open_block_reopen_persists () =
  let path = tmp_block_file 4 in
  Fun.protect ~finally:(fun () -> Unix.unlink path) (fun () ->
    (* Write *)
    with_block_store path (fun store ->
      let open Lwt.Syntax in
      let* tx = S.rw_begin store in
      let* tid = S.get_or_create_tree store tx 200 in
      let* () = S.put store tx tid (Bytes.of_string "k1") (Bytes.of_string "v1") in
      S.commit tx
    );
    (* Reopen and read *)
    with_block_store path (fun store ->
      let open Lwt.Syntax in
      let* tx = S.ro_begin store in
      let* tid_opt = S.get_tree store tx 200 in
      match tid_opt with
      | None -> Alcotest.fail "tree missing after reopen"
      | Some tid ->
        let* v = S.get store tx tid (Bytes.of_string "k1") in
        Alcotest.(check (option bytes)) "value persisted"
          (Some (Bytes.of_string "v1")) v;
        S.ro_end tx;
        Lwt.return_unit
    )
  )

(* Add to the test runner: *)
let () =
  (* ... existing suites ... *)
  Alcotest.run "store_btree" [
    (* ... existing suites ... *)
    "open_block", [
      Alcotest.test_case "fresh_init"        `Quick test_open_block_fresh;
      Alcotest.test_case "reopen_persists"   `Quick test_open_block_reopen_persists;
    ];
  ]
```

**Note:** Check the actual test runner structure in `test/test_store_btree.ml` before adding — merge the `"open_block"` suite into the existing `Alcotest.run` call.

Also add `mirage-block-unix sqlocaml.mirage_block` to the libraries in the `(tests ...)` stanza for `test_store_btree` in `test/dune`, or add it as a separate test stanza.

The cleanest approach: change the `test_store_btree` entry to use its own `(test ...)` stanza with additional libraries:

In `test/dune`, remove `test_store_btree` from the shared `(tests ...)` stanza and add:

```
(test
 (name test_store_btree)
 (libraries alcotest qcheck-alcotest sqlocaml sqlocaml.block sqlocaml.encoding
            sqlocaml.store sqlocaml.catalog sqlocaml.sql sqlocaml.storage
            sqlocaml.mirage_block mirage-block-unix lwt.unix unix))
```

- [ ] **Step 9: Build and run**

```bash
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_store_btree.exe 2>&1 | tail -20
```

Expected: all existing tests pass, plus 2 new `open_block` tests.

- [ ] **Step 10: Commit**

```bash
git add lib/storage/pager.ml lib/storage/pager.mli \
        lib/store/store.ml lib/store/store.mli \
        test/test_store_btree.ml test/dune
git commit -m "feat(store): add open_block generic entry point; replace Unix_file coupling with close_fn"
```

---

## Task 3: Db.open_block

**Files:**
- Modify: `lib/db/db.ml`
- Modify: `lib/db/db.mli`

- [ ] **Step 1: Add `open_block` to `lib/db/db.ml`**

After `open_file`, add:

```ocaml
let open_block
    ~read_page ~write_page ~sync ~resize ~n_pages ~close
    : (t, error) result Lwt.t =
  let* result = S.open_block ~read_page ~write_page ~sync ~resize ~n_pages ~close in
  match result with
  | Error e ->
    let msg = Format.asprintf "%a" S.pp_error e in
    Lwt.return (Error (Runtime msg))
  | Ok store ->
    let* catalog = Cat.open_ store in
    Lwt.return (Ok { store; catalog; explicit_txn = None })
```

- [ ] **Step 2: Add `open_block` to `lib/db/db.mli`**

After `val open_file`, add:

```ocaml
(** Open a SQL engine on any block device given as I/O callbacks.
    Use with [Sqlocaml_mirage_block.Mirage_backend.Make(B)] to build
    the callbacks from a [Mirage_block.S] device.  Pass [~n_pages:0L]
    for Mirage adapters; the adapter handles device-capacity bounds
    internally.  [~close] is called by [Db.close]. *)
val open_block :
  read_page  : (page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t) ->
  write_page : (page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t) ->
  sync       : (unit -> (unit, string) result Lwt.t) ->
  resize     : (n_pages:int64 -> (unit, string) result Lwt.t) ->
  n_pages    : int64 ->
  close      : (unit -> unit Lwt.t) ->
  (t, error) result Lwt.t
```

- [ ] **Step 3: Build**

```bash
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
```

Expected: clean build.

- [ ] **Step 4: Commit**

```bash
git add lib/db/db.ml lib/db/db.mli
git commit -m "feat(db): expose open_block for Mirage_block-backed databases"
```

---

## Task 4: Multi-backend SQL conformance test

**Files:**
- Create: `test/test_all_backends.ml`
- Modify: `test/dune`

This test runs a shared SQL scenario (CREATE TABLE, INSERT, SELECT, UPDATE, DELETE, transactions) on all three backends and checks identical results.

- [ ] **Step 1: Create `test/test_all_backends.ml`**

```ocaml
(** Multi-backend SQL conformance test.
    Runs the same SQL scenario on Mem, Unix_file, and Mirage_block backends.
    Identical results are required. *)

open Lwt.Syntax
module DB = Sqlocaml
module MB = Sqlocaml_mirage_block.Mirage_backend.Make(Block)

(* ---------- helpers -------------------------------------------------- *)

let tmp_file () =
  let path = Filename.temp_file "sqlocaml_backend_test" ".raw" in
  let fd = Unix.openfile path [Unix.O_RDWR; Unix.O_CREAT] 0o644 in
  Unix.ftruncate fd (4 * 1024 * 1024);  (* 4 MB = 1024 pages *)
  Unix.close fd;
  path

let rows_of_stream s =
  Lwt_stream.to_list s

let execute_ok db sql =
  let* r = DB.execute db sql in
  match r with
  | Ok () -> Lwt.return_unit
  | Error e -> Alcotest.failf "execute failed: %s" (match e with DB.Parse s | DB.Runtime s -> s | DB.Sema e -> Format.asprintf "%a" Sqlocaml_sql.Sema.pp_error e)

let query_rows db sql =
  let* r = DB.query db sql in
  match r with
  | Error e -> Alcotest.failf "query failed: %s" (match e with DB.Parse s | DB.Runtime s -> s | DB.Sema e -> Format.asprintf "%a" Sqlocaml_sql.Sema.pp_error e)
  | Ok stream -> rows_of_stream stream

(* ---------- shared scenario ------------------------------------------ *)

(* Returns a list of result rows from a standard SQL scenario.
   The scenario: CREATE TABLE, INSERT 3 rows, UPDATE one, DELETE one,
   check txn rollback, then SELECT remaining rows ORDER BY id. *)
let run_scenario db =
  let* () = execute_ok db "CREATE TABLE t (id INTEGER PRIMARY KEY, v TEXT NOT NULL)" in
  let* () = execute_ok db "INSERT INTO t VALUES (1, 'one')" in
  let* () = execute_ok db "INSERT INTO t VALUES (2, 'two')" in
  let* () = execute_ok db "INSERT INTO t VALUES (3, 'three')" in
  let* () = execute_ok db "UPDATE t SET v = 'TWO' WHERE id = 2" in
  let* () = execute_ok db "DELETE FROM t WHERE id = 3" in
  (* Txn rollback: BEGIN, insert 99, ROLLBACK — 99 must not appear *)
  let* () = execute_ok db "BEGIN" in
  let* () = execute_ok db "INSERT INTO t VALUES (99, 'ghost')" in
  let* () = execute_ok db "ROLLBACK" in
  query_rows db "SELECT id, v FROM t ORDER BY id"

let rows_to_strings rows =
  List.map (fun row ->
    Array.to_list row
    |> List.map (function
      | DB.V_int n  -> Int64.to_string n
      | DB.V_text s -> s
      | DB.V_null   -> "NULL"
      | DB.V_real f -> string_of_float f
      | DB.V_blob b -> Printf.sprintf "blob(%d)" (Bytes.length b))
    |> String.concat ","
  ) rows

(* Expected result: rows (1,"one") and (2,"TWO"); row 99 absent. *)
let expected = ["1,one"; "2,TWO"]

(* ---------- per-backend wrappers ------------------------------------ *)

let with_mem_db f =
  let* db = DB.open_in_memory () in
  let* result = f db in
  let* () = DB.close db in
  Lwt.return result

let with_file_db path f =
  let* r = DB.open_file ~path in
  match r with
  | Error e -> Alcotest.failf "open_file failed: %s" (match e with DB.Runtime s -> s | _ -> "?")
  | Ok db ->
    let* result = f db in
    let* () = DB.close db in
    Lwt.return result

let with_mirage_db path f =
  let* dev = Block.connect ~prefered_sector_size:(Some 4096) path in
  let* adapter = MB.connect dev in
  let* r = DB.open_block
    ~read_page:(MB.read_page adapter)
    ~write_page:(MB.write_page adapter)
    ~sync:(MB.sync adapter)
    ~resize:(MB.resize adapter)
    ~n_pages:(MB.n_pages adapter)
    ~close:(fun () -> MB.close adapter)
  in
  match r with
  | Error e -> Alcotest.failf "open_block failed: %s" (match e with DB.Runtime s -> s | _ -> "?")
  | Ok db ->
    let* result = f db in
    let* () = DB.close db in
    Lwt.return result

(* ---------- tests --------------------------------------------------- *)

let test_mem_backend () =
  let rows = Lwt_main.run (with_mem_db run_scenario) in
  Alcotest.(check (list string)) "mem results" expected (rows_to_strings rows)

let test_unix_file_backend () =
  let path = Filename.temp_file "sqlocaml_cf_unix" ".db" in
  Fun.protect ~finally:(fun () -> try Unix.unlink path with _ -> ()) (fun () ->
    let rows = Lwt_main.run (with_file_db path run_scenario) in
    Alcotest.(check (list string)) "unix_file results" expected (rows_to_strings rows)
  )

let test_mirage_backend () =
  let path = tmp_file () in
  Fun.protect ~finally:(fun () -> try Unix.unlink path with _ -> ()) (fun () ->
    let rows = Lwt_main.run (with_mirage_db path run_scenario) in
    Alcotest.(check (list string)) "mirage_block results" expected (rows_to_strings rows)
  )

let test_all_identical () =
  (* Run all three and compare *)
  let mem_rows = Lwt_main.run (with_mem_db run_scenario) in
  let file_path = Filename.temp_file "sqlocaml_all_unix" ".db" in
  let mb_path = tmp_file () in
  Fun.protect
    ~finally:(fun () ->
      (try Unix.unlink file_path with _ -> ());
      (try Unix.unlink mb_path with _ -> ()))
    (fun () ->
      let file_rows = Lwt_main.run (with_file_db file_path run_scenario) in
      let mb_rows   = Lwt_main.run (with_mirage_db mb_path run_scenario) in
      let mem_s  = rows_to_strings mem_rows in
      let file_s = rows_to_strings file_rows in
      let mb_s   = rows_to_strings mb_rows in
      Alcotest.(check (list string)) "mem = expected"   expected mem_s;
      Alcotest.(check (list string)) "file = expected"  expected file_s;
      Alcotest.(check (list string)) "mirage = expected" expected mb_s;
      Alcotest.(check (list string)) "mem = file"   mem_s file_s;
      Alcotest.(check (list string)) "mem = mirage" mem_s mb_s
    )

let test_mirage_file_reopen_persists () =
  let path = tmp_file () in
  Fun.protect ~finally:(fun () -> try Unix.unlink path with _ -> ()) (fun () ->
    (* Write via Mirage backend *)
    Lwt_main.run (with_mirage_db path (fun db ->
      execute_ok db "CREATE TABLE persist (x INTEGER PRIMARY KEY)"
      >>= fun () -> execute_ok db "INSERT INTO persist VALUES (42)"
    ));
    (* Reopen and read back *)
    let rows = Lwt_main.run (with_mirage_db path (fun db ->
      query_rows db "SELECT x FROM persist"
    )) in
    Alcotest.(check (list string)) "persisted across reopen"
      ["42"] (rows_to_strings rows)
  )

let () =
  let open Alcotest in
  run "all_backends" [
    "conformance", [
      test_case "mem_backend"            `Quick test_mem_backend;
      test_case "unix_file_backend"      `Quick test_unix_file_backend;
      test_case "mirage_backend"         `Quick test_mirage_backend;
      test_case "all_identical"          `Quick test_all_identical;
      test_case "mirage_reopen_persists" `Quick test_mirage_file_reopen_persists;
    ]
  ]
```

- [ ] **Step 2: Add test stanza to `test/dune`**

Add at end of `test/dune`:

```
(test
 (name test_all_backends)
 (libraries sqlocaml sqlocaml.block sqlocaml.encoding sqlocaml.store
            sqlocaml.catalog sqlocaml.sql sqlocaml.storage
            sqlocaml.mirage_block mirage-block-unix
            alcotest lwt.unix unix))
```

- [ ] **Step 3: Build and run**

```bash
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z -w /workspace sqlocaml-dev dune build 2>&1
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z -w /workspace sqlocaml-dev dune exec test/test_all_backends.exe 2>&1
```

Expected: 5 tests pass. `all_identical` verifies Mem = Unix_file = Mirage_block.

- [ ] **Step 4: Commit**

```bash
git add test/test_all_backends.ml test/dune
git commit -m "test: multi-backend SQL conformance — Mem + Unix_file + Mirage_block all produce identical results"
```

---

## Task 5: CI update, ROADMAP, and Forgejo issues

**Files:**
- Modify: `.forgejo/workflows/ci.yml`
- Modify: `ROADMAP.md`
- Forgejo: close issue #6

- [ ] **Step 1: Update `.forgejo/workflows/ci.yml` to install mirage packages**

Find the "Install opam packages" step:
```yaml
- name: Install opam packages
  run: |
    opam install -y lwt cstruct menhir alcotest qcheck-alcotest lwt_ppx
```

Replace with:
```yaml
- name: Install opam packages
  run: |
    opam install -y lwt cstruct menhir alcotest qcheck-alcotest lwt_ppx \
      mirage-block mirage-block-unix
```

Similarly update the coverage workflow (`.forgejo/workflows/coverage.yml`):

```yaml
- name: Install opam packages (including bisect_ppx)
  run: |
    opam install -y lwt cstruct menhir alcotest qcheck-alcotest lwt_ppx bisect_ppx \
      mirage-block mirage-block-unix
```

- [ ] **Step 2: Run full test suite to confirm everything passes**

```bash
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z -w /workspace sqlocaml-dev dune runtest 2>&1 | tail -20
```

Expected: all tests pass (no failures).

- [ ] **Step 3: Check test counts**

```bash
podman run --rm -v /home/tej/projects/sqlite_ocaml_port:/workspace:Z -w /workspace sqlocaml-dev bash -c "
for exe in _build/default/test/test_*.exe; do
  name=\$(basename \$exe .exe)
  result=\$(\$exe 2>&1 | tail -1)
  echo \"\$name: \$result\"
done" 2>&1
```

- [ ] **Step 4: Update ROADMAP.md**

Change:
```
- [ ] **Phase 4 — Mirage_block (~4 wks):** unikernel deployment.
```
to:
```
- [x] **Phase 4 — Mirage_block (~4 wks):** unikernel deployment.
```

Also update the storage section — change:
```
- [x] `BLOCK` signature + Mem/Unix_file/Mirage_block backends
```
(this was already marked done in Phase 1; confirm it now reads as fully implemented)

And mark the tooling item done:
```
- [x] OCaml library API (`Db.open_`, `execute`, `query`, prepared stmts)
```

- [ ] **Step 5: Commit all Phase 4 wrap-up changes**

```bash
git add .forgejo/workflows/ci.yml .forgejo/workflows/coverage.yml ROADMAP.md
git commit -m "chore: Phase 4 complete — CI installs mirage-block packages; ROADMAP updated"
```

- [ ] **Step 6: Close Forgejo issue #6**

```bash
~/.local/bin/forgejo issue comment tej/sqlite_ocaml_port 6 --body "## Phase 4 complete — $(date +%Y-%m-%d)

**Deliverables:**

| Component | Details |
|---|---|
| \`Mirage_backend.Make(B: Mirage_block.S)\` | ~50-line functor in \`lib/block/mirage_backend.ml\`; separate \`sqlocaml.mirage_block\` library |
| \`Store.open_block\` | Generic I/O callback entry point; replaces \`Unix_file.t\` coupling in \`bt_state\` |
| \`Db.open_block\` | Public API; Mirage adapter plugs in here |
| \`Pager.set_n_pages\` | Lets \`open_block\` set authoritative page count after probing headers |
| Multi-backend conformance | \`test/test_all_backends.ml\` — 5 tests, all three backends produce identical SQL results |
| Mirage adapter unit tests | \`test/test_mirage_backend.ml\` — 6 tests covering read/write/resize/sync/OOB |

**Key design decisions:**
- Adapter bounds-checks read/write against device \`capacity\` (not logical \`n_pages\`), allowing header probing before DB claims any pages
- \`open_block\` detects fresh vs. existing DB by reading headers (Both_headers_corrupt → fresh); works for pre-sized fixed-size block devices
- \`resize\` callback in Mirage adapter is a no-op on physical storage (device already allocated); just updates logical page count
- \`sync\` is a no-op (Mirage block writes are synchronous)

**Acceptance:** multi-backend conformance test verifies Mem + Unix_file + Mirage_block produce identical SQL results including transactions, rollback, and persistence across reopen."

~/.local/bin/forgejo issue edit tej/sqlite_ocaml_port 6 --state=closed
```

---

## Self-review

### Spec coverage

| Spec requirement | Covered by |
|---|---|
| Mirage_block.S adapter | Task 1: `Mirage_backend.Make` |
| Store functor over BLOCK | Task 2: `open_block` + `bt_state` refactor |
| Db.open_block public API | Task 3 |
| Same test suite on all backends | Task 4: `test_all_backends.ml` |
| CI installs mirage-block | Task 5 |
| ROADMAP updated | Task 5 |

### Placeholder scan

- No "TBD" or "TODO" remaining in tasks — all code is shown
- All type signatures match between tasks
- `Header.Both_headers_corrupt` is the correct constructor from `header.mli` (`type error = Io of string | Both_headers_corrupt`)
- `S.get_or_create_tree` / `S.get_tree` — verify actual function names in `store.mli` before writing test_store_btree additions; adjust if different

### Type consistency

- `MB.read_page adapter` returns a curried function `page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t` — matches `Store.open_block ~read_page` parameter type ✓
- `MB.sync adapter` returns `unit -> (unit, string) result Lwt.t` — matches `Store.open_block ~sync` ✓
- `MB.resize adapter` returns `n_pages:int64 -> (unit, string) result Lwt.t` — matches `Store.open_block ~resize` ✓
- `MB.close adapter` returns `unit Lwt.t` — wrapped in `fun () -> MB.close adapter` to match `close: unit -> unit Lwt.t` ✓

### Known adaptation needed

`test/test_store_btree.ml` currently has a specific `Alcotest.run` call structure. Before adding the `"open_block"` suite, read the actual end of that file to insert the suite correctly. The store API (`get_or_create_tree`, `get_tree`, `ro_end`) — verify exact names from `store.mli` before writing tests.
