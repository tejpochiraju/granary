# Whole-DB `as-of` Time Travel — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let an opted-in database be queried as it existed at any past commit/timestamp — a non-destructive, concurrent-with-writes read against a retained copy-on-write root.

**Architecture:** A new append-only commit log (`txn_id -> timestamp, root_page`) is fed by an injected sink at each commit. Retention is achieved by pinning the existing reclamation watermark (`min_safe_txn_id`) to a floor; no new GC. A historical read opens a normal RO snapshot against an old root via a factored `ro_begin_at`. The whole feature is default-off behind an open-time `?as_of_history` flag.

**Tech Stack:** OCaml 5.x, Lwt, Cstruct, Dune. Build/test run inside the `sqlocaml-dev` podman image.

---

## Conventions

All build/test/format commands run inside the dev container **from the worktree root** (`.worktrees/266-as-of`). Throughout this plan, `DEV` means:

```sh
podman run --rm -v "$(pwd):/workspace:z" -w /workspace sqlocaml-dev
```

So "build" = `$DEV dune build`, "test all" = `$DEV dune test`. Set it once per shell:

```sh
DEV='podman run --rm -v '"$(pwd)"':/workspace:z -w /workspace sqlocaml-dev'
```

Commit after every green step. Never commit to `main` (we are on `feat/266-whole-db-as-of`).

## File Structure

- **Create** `lib/store/history.ml` + `history.mli` — pure record type, 28-byte codec with CRC32, torn-tail-tolerant buffer parser, and `<= target` resolver. No I/O, no Lwt. One responsibility: the on-disk log format + lookup.
- **Modify** `lib/store/store.ml` + `store.mli` — three `bt_state` fields (sink, clock, floor); fold the floor into the `min_safe` computation; new errors; factor `ro_begin_at`; add `ro_begin_as_of`, `history_pin`, `history_floor`, `history_release`, `history_log`; append to the sink in the commit hook; plumb `?as_of_history`/`?history`/`?now` through `create`/`open_block`/`open_block_wal`.
- **Modify** `lib/unix/store.ml` + `store.mli` — a file-backed `History.sink` over `<path>.aslog`; plumb `?as_of_history` through `open_file`/`open_file_wal`.
- **Modify** `lib/db/db.ml` + `db.mli` — thin `query_as_of` reusing `Sql.Exec.query ~mode:(In_txn …)`; plumb `?as_of_history` through `Db.open_block`.
- **Create** tests: `test/test_history.ml`, `test/test_store_as_of.ml`, `test/test_unix_as_of.ml`, `test/test_db_as_of.ml` (wire each into the relevant `test/dune`).

Each task ends green and committed.

---

## Task 1: `History` module — codec + resolver (pure)

**Files:**
- Create: `lib/store/history.ml`, `lib/store/history.mli`
- Test: `test/test_history.ml` (+ add stanza to `test/dune`)

- [ ] **Step 1: Write the `.mli`**

`lib/store/history.mli`:

```ocaml
(** Append-only commit-log records for whole-DB as-of time travel (#266).

    Each committed root is recorded as a fixed-width 28-byte record:
    [txn_id(8) ++ timestamp(8) ++ root_page(8) ++ crc32(4)], big-endian,
    with the CRC32 (IEEE polynomial) computed over the first 24 bytes.  A
    torn or corrupt tail record is dropped on load — the database header
    remains the source of truth for the live head. *)

(** One committed whole-DB snapshot. *)
type record =
  { txn_id : int64 (** monotonic commit id *)
  ; timestamp : int64 (** wall-clock ms since epoch at commit *)
  ; root_page : int64 (** meta-tree root committed at [txn_id] *)
  }

(** A target to resolve against the log. *)
type target =
  [ `Txn of int64
  | `Ts of int64
  ]

(** An injected append-only log backend (file, block device, or in-memory). *)
type sink =
  { append : record -> unit Lwt.t (** append one record; best-effort *)
  ; load : unit -> record list Lwt.t (** all records in ascending txn order *)
  }

(** Fixed serialized size of one record, in bytes. *)
val record_size : int

(** Serialize a record to a fresh {!record_size}-byte buffer. *)
val encode : record -> Cstruct.t

(** Decode a single {!record_size}-byte record.  [None] if the buffer is too
    short or the CRC does not match. *)
val decode : Cstruct.t -> record option

(** Parse a buffer of concatenated records.  Stops at the first short or
    CRC-failing record (a torn tail), returning every valid record before it. *)
val decode_all : Cstruct.t -> record list

(** [resolve records target] returns the record with the largest [txn_id]
    (for [`Txn]) or [timestamp] (for [`Ts]) that is [<=] the target, or
    [None] if every record is newer (or the list is empty).  [records] must
    be in ascending order. *)
val resolve : record list -> target -> record option
```

- [ ] **Step 2: Write the failing test**

`test/test_history.ml`:

```ocaml
module H = Sqlocaml_store.History

let r txn_id timestamp root_page = { H.txn_id; timestamp; root_page }

let test_roundtrip () =
  let rec_ = r 7L 1700000000000L 42L in
  match H.decode (H.encode rec_) with
  | Some got -> assert (got = rec_)
  | None -> assert false

let test_crc_rejects_corruption () =
  let buf = H.encode (r 1L 2L 3L) in
  Cstruct.set_uint8 buf 0 (Cstruct.get_uint8 buf 0 lxor 0xff);
  assert (H.decode buf = None)

let test_decode_all_drops_torn_tail () =
  let a = H.encode (r 1L 10L 100L) in
  let b = H.encode (r 2L 20L 200L) in
  let torn = Cstruct.sub b 0 (H.record_size - 3) in
  let buf = Cstruct.concat [ a; torn ] in
  assert (H.decode_all buf = [ r 1L 10L 100L ])

let test_resolve_txn_le () =
  let recs = [ r 1L 10L 100L; r 3L 30L 300L; r 5L 50L 500L ] in
  assert (H.resolve recs (`Txn 4L) = Some (r 3L 30L 300L));
  assert (H.resolve recs (`Txn 5L) = Some (r 5L 50L 500L));
  assert (H.resolve recs (`Txn 0L) = None)

let test_resolve_ts_le () =
  let recs = [ r 1L 10L 100L; r 3L 30L 300L ] in
  assert (H.resolve recs (`Ts 25L) = Some (r 1L 10L 100L));
  assert (H.resolve recs (`Ts 30L) = Some (r 3L 30L 300L))

let () =
  test_roundtrip ();
  test_crc_rejects_corruption ();
  test_decode_all_drops_torn_tail ();
  test_resolve_txn_le ();
  test_resolve_ts_le ();
  print_endline "test_history: OK"
```

Add to `test/dune` (mirror the form of an existing single-exe test stanza already in that file):

```
(test
 (name test_history)
 (libraries sqlocaml_store))
```

- [ ] **Step 3: Run the test, verify it fails**

Run: `$DEV dune test test/test_history.exe`
Expected: FAIL — `Unbound module Sqlocaml_store.History`.

- [ ] **Step 4: Implement `lib/store/history.ml`**

```ocaml
type record =
  { txn_id : int64
  ; timestamp : int64
  ; root_page : int64
  }

type target =
  [ `Txn of int64
  | `Ts of int64
  ]

type sink =
  { append : record -> unit Lwt.t
  ; load : unit -> record list Lwt.t
  }

let record_size = 28
let payload_size = 24

(* IEEE CRC32, computed over the whole given slice (nothing zeroed). Kept
   self-contained so the log codec owns its format end-to-end. *)
let crc32_table =
  Array.init 256 (fun i ->
    let crc = ref i in
    for _ = 0 to 7 do
      crc := if !crc land 1 = 1 then 0xEDB88320 lxor (!crc lsr 1) else !crc lsr 1
    done;
    !crc)

let crc32 (buf : Cstruct.t) : int32 =
  let crc = ref 0xFFFFFFFF in
  for i = 0 to Cstruct.length buf - 1 do
    let byte = Char.code (Cstruct.get_char buf i) in
    crc := crc32_table.(!crc lxor byte land 0xFF) lxor (!crc lsr 8)
  done;
  Int32.of_int (!crc lxor 0xFFFFFFFF)

let encode (r : record) : Cstruct.t =
  let buf = Cstruct.create record_size in
  Cstruct.BE.set_uint64 buf 0 r.txn_id;
  Cstruct.BE.set_uint64 buf 8 r.timestamp;
  Cstruct.BE.set_uint64 buf 16 r.root_page;
  Cstruct.BE.set_uint32 buf payload_size (crc32 (Cstruct.sub buf 0 payload_size));
  buf

let decode (buf : Cstruct.t) : record option =
  if Cstruct.length buf < record_size
  then None
  else (
    let stored = Cstruct.BE.get_uint32 buf payload_size in
    if Int32.equal stored (crc32 (Cstruct.sub buf 0 payload_size))
    then
      Some
        { txn_id = Cstruct.BE.get_uint64 buf 0
        ; timestamp = Cstruct.BE.get_uint64 buf 8
        ; root_page = Cstruct.BE.get_uint64 buf 16
        }
    else None)

let decode_all (buf : Cstruct.t) : record list =
  let n = Cstruct.length buf in
  let rec loop off acc =
    if off + record_size > n
    then List.rev acc
    else (
      match decode (Cstruct.sub buf off record_size) with
      | Some r -> loop (off + record_size) (r :: acc)
      | None -> List.rev acc)
  in
  loop 0 []

let resolve (records : record list) (target : target) : record option =
  let key r =
    match target with
    | `Txn _ -> r.txn_id
    | `Ts _ -> r.timestamp
  in
  let bound =
    match target with
    | `Txn t | `Ts t -> t
  in
  List.fold_left
    (fun acc r -> if Int64.compare (key r) bound <= 0 then Some r else acc)
    None
    records
```

- [ ] **Step 5: Run the test, verify it passes**

Run: `$DEV dune test test/test_history.exe`
Expected: PASS — prints `test_history: OK`.

- [ ] **Step 6: Add a QCheck round-trip property**

Append to `test/test_history.ml` before `let ()`:

```ocaml
let test_qcheck_roundtrip () =
  let gen = QCheck.(triple int64 int64 int64) in
  let prop =
    QCheck.Test.make ~count:1000 ~name:"history encode/decode roundtrip" gen
      (fun (a, b, c) ->
         let rec_ = r a b c in
         H.decode (H.encode rec_) = Some rec_)
  in
  QCheck_runner.run_tests_main [ prop ] |> ignore
```

Change the QCheck libraries line in the `test/dune` stanza to `(libraries sqlocaml_store qcheck qcheck-core)` (match how existing QCheck tests in `test/dune` declare their libs) and call `test_qcheck_roundtrip ()` from `let ()`.

- [ ] **Step 7: Format, then run the test again**

Run (format the new files via the stdout→host redirect from CLAUDE.md):
```sh
for f in lib/store/history.ml lib/store/history.mli test/test_history.ml; do
  tmp=$(mktemp) && $DEV ocamlformat "$f" > "$tmp" && mv "$tmp" "$f" && chmod 644 "$f"
done
$DEV dune test test/test_history.exe
```
Expected: PASS.

- [ ] **Step 8: Commit**

```sh
git add lib/store/history.ml lib/store/history.mli test/test_history.ml test/dune
git commit -m "feat(#266): History commit-log codec + resolver"
```

---

## Task 2: `bt_state` fields, watermark floor, errors, open-flag plumbing

This task wires the feature's state into `Store` and makes the floor pin retention, **without** the read API yet. End state: builds clean, all existing tests pass, feature still inert by default.

**Files:**
- Modify: `lib/store/store.ml` (record `bt_state` ~98-217; `create` ~624-680; `open_block` ~911; `open_block_wal` ~1044; `min_safe` ~1252; `error` ~31; `pp_error`)
- Modify: `lib/store/store.mli` (error doc; `create`/`open_block`/`open_block_wal` signatures)

- [ ] **Step 1: Add the three fields to `bt_state`**

In `lib/store/store.ml`, inside the `bt_state` record (after `mutable on_event …`, around line 199), add:

```ocaml
  ; mutable history : History.sink option
    (* #266: append-only as-of commit log.  [Some] iff the store was opened
       with [~as_of_history:true] AND a sink was supplied; [None] disables
       the whole feature (zero commit-path overhead). *)
  ; mutable history_now : unit -> int64
    (* #266: wall-clock (ms since epoch) stamped onto each commit-log record.
       Injected at open; defaults to a constant 0 when history is disabled. *)
  ; mutable history_floor : int64 option
    (* #266: retention floor.  When [Some t], [min_safe] is capped at [t+1]
       so pages reachable from roots >= t are never reused. *)
```

- [ ] **Step 2: Initialise the fields wherever a `bt_state` is constructed**

`create` (~664) and both `open_block`/`open_block_wal` build a `bt_state`. Add to each record literal:

```ocaml
    ; history = None
    ; history_now = (fun () -> 0L)
    ; history_floor = None
```

(For `open_block`/`open_block_wal`, the actual sink/clock are assigned from the new params in Step 4; initialise to these defaults in the literal and override below.)

- [ ] **Step 3: Add errors + `pp_error` arms**

In the `error` variant (~31) add:

```ocaml
  | History_unavailable (** as-of API used on a store opened without the feature *)
  | History_pruned (** as-of target is older than the retained floor *)
  | History_misconfigured (** [as_of_history:true] but no history sink supplied *)
```

In `pp_error` add matching arms:

```ocaml
  | History_unavailable -> Format.fprintf fmt "as-of time travel is not enabled on this database"
  | History_pruned -> Format.fprintf fmt "as-of target is older than the retained history horizon"
  | History_misconfigured -> Format.fprintf fmt "as_of_history was requested but no history log was supplied"
```

Mirror the three constructors (with `(** … *)` doc comments — merlint requires doc comments on exposed constructors) into the `error` type in `lib/store/store.mli`.

- [ ] **Step 4: Add `?as_of_history` / `?history` / `?now` to `create`, `open_block`, `open_block_wal`**

For each, add optional params and, after building `st`, wire them:

```ocaml
let create ?(as_of_history = false) ?history ?now () : t =
  ...
  (match as_of_history, history with
   | true, Some sink -> st.history <- Some sink
   | true, None -> () (* misconfig surfaced by the caller-facing open; create has no result type *)
   | false, _ -> ());
  (match now with Some f -> st.history_now <- f | None -> ());
  ...
```

For `open_block`/`open_block_wal` (which return `(t, error) result Lwt.t`), enforce the misconfig rule:

```ocaml
  if as_of_history && Option.is_none history
  then Lwt.return_error History_misconfigured
  else (
    ... existing body, then before returning Ok:
    (match as_of_history, history with true, Some s -> st.history <- Some s | _ -> ());
    (match now with Some f -> st.history_now <- f | None -> ());
    ...)
```

Update `store.mli` signatures to add (with doc comments):

```ocaml
  ?as_of_history:bool ->
  ?history:History.sink ->
  ?now:(unit -> int64) ->
```

placed before the existing `?key`/`~init_if_corrupt` params on `create`, `open_block`, `open_block_wal`. Document: "*(#266) When [as_of_history] is [true], a [history] sink MUST be supplied (else [History_misconfigured]); each commit is recorded for as-of reads. Default [false] — feature off, zero overhead.*"

- [ ] **Step 5: Fold the floor into `min_safe`**

In `rw_begin` at the `min_safe` computation (~1252-1256), after the existing `let min_safe = … in`, replace `Pager.set_alloc_min_safe st.pager min_safe;` with:

```ocaml
       let min_safe =
         match st.history_floor with
         | None -> min_safe
         | Some f -> Int64.min min_safe (Int64.add f 1L)
       in
       Pager.set_alloc_min_safe st.pager min_safe;
```

(Reuse only happens via `Pager.alloc` → `Freelist.pop ~min_safe_txn_id`, so capping `alloc_min_safe` is the single sufficient gate.)

- [ ] **Step 6: Build + run the full suite**

Run: `$DEV dune build && $DEV dune test`
Expected: clean build; all existing tests PASS (feature inert — `history = None`, `history_floor = None` everywhere).

- [ ] **Step 7: Format changed files + commit**

```sh
for f in lib/store/store.ml lib/store/store.mli; do
  tmp=$(mktemp) && $DEV ocamlformat "$f" > "$tmp" && mv "$tmp" "$f" && chmod 644 "$f"
done
git add lib/store/store.ml lib/store/store.mli
git commit -m "feat(#266): Store as-of state, errors, watermark floor, open flag"
```

---

## Task 3: as-of read path, retention API, commit hook

**Files:**
- Modify: `lib/store/store.ml` (`ro_begin` ~1122; `commit_prepare_btree` ~1719-1731)
- Modify: `lib/store/store.mli` (new public vals)
- Test: `test/test_store_as_of.ml` (+ `test/dune` stanza)

- [ ] **Step 1: Factor `ro_begin_at` out of `ro_begin`**

In `lib/store/store.ml`, extract the Btree-snapshot tail of `ro_begin` (lines ~1158-1199) into a helper that takes explicit snapshot coordinates, and have `ro_begin` call it with the live header. New helper (place just above `ro_begin`):

```ocaml
(* Build an RO snapshot against an explicit (txn_id, meta_root).  [ro_begin]
   passes the live header; [ro_begin_as_of] passes a retained historical root.
   Registers the snapshot in [active_readers]/[active_reader_frames] so
   reclamation respects it, exactly as the live path does.  Uses the CURRENT
   committed-frames horizon: CoW never rewrites a retained page-id, so each
   retained page has exactly one WAL frame and "latest up to head" == the
   historical content (the floor/reader pin prevents reuse). *)
let ro_begin_at st ~snap_txn_id ~snap_meta_root =
  let committed_frames =
    match st.wal with
    | None -> 0
    | Some w -> Wal.committed_frames w
  in
  let snap_frames =
    if st.follower
    then (
      match st.follower_ack_position with
      | Some n -> min committed_frames n
      | None -> committed_frames)
    else committed_frames
  in
  let count = Option.value ~default:0 (Hashtbl.find_opt st.active_readers snap_txn_id) in
  Hashtbl.replace st.active_readers snap_txn_id (count + 1);
  let frame_count =
    Option.value ~default:0 (Hashtbl.find_opt st.active_reader_frames snap_frames)
  in
  Hashtbl.replace st.active_reader_frames snap_frames (frame_count + 1);
  Ro
    { rs_store = (* the wrapping [t] *) assert false (* see Step 1b *)
    ; rs_snap_txn_id = snap_txn_id
    ; rs_snap_meta_root = snap_meta_root
    ; rs_snap_trees = Hashtbl.create 4
    ; rs_snap_frames = snap_frames
    ; rs_pinned = Hashtbl.create 64
    ; rs_mem_snap = None
    }
```

- [ ] **Step 1b: Thread `t` so `rs_store` is set correctly**

`rs_store` must be the outer `t`. Give the helper the `t` param: `let ro_begin_at t st ~snap_txn_id ~snap_meta_root =` and set `rs_store = t`. In `ro_begin`'s `Btree st ->` branch, replace the inlined record construction with:

```ocaml
    | Btree st -> Lwt.return (ro_begin_at t st ~snap_txn_id:st.current_header.txn_id
                                ~snap_meta_root:st.current_header.root_page)
```

Run `$DEV dune build` and confirm the existing `test/` RO tests still pass (`$DEV dune test`). Commit this refactor on its own:

```sh
git add lib/store/store.ml && git commit -m "refactor(#266): factor ro_begin_at from ro_begin"
```

- [ ] **Step 2: Write the failing Store-level test**

`test/test_store_as_of.ml` (uses the in-memory file backend helper if one exists in `test/`; otherwise drives a `Unix.Store` temp file — match the pattern an existing `test/test_store_*.ml` uses to open a Btree store). Skeleton:

```ocaml
open Lwt.Syntax
module S = Sqlocaml_store.Store
module H = Sqlocaml_store.History

(* In-memory sink for tests. *)
let mem_sink () =
  let buf = ref [] in
  let sink =
    { H.append = (fun r -> buf := r :: !buf; Lwt.return_unit)
    ; load = (fun () -> Lwt.return (List.rev !buf))
    }
  in
  sink, buf

let test_pin_floor () =
  let s = S.create ~as_of_history:true ~history:(fst (mem_sink ()))
            ~now:(fun () -> 0L) () in
  assert (S.history_floor s = None);
  S.history_pin s ~txn_id:5L;
  assert (S.history_floor s = Some 5L);
  S.history_release s;
  assert (S.history_floor s = None)

let test_unavailable_without_flag () =
  let s = S.create () in
  Lwt_main.run
    (Lwt.catch
       (fun () -> let* _ = S.ro_begin_as_of s (`Txn 1L) in Lwt.return false)
       (function S.History_error History_unavailable -> Lwt.return true
               | _ -> Lwt.return false))
  |> fun ok -> assert ok

let () =
  test_pin_floor ();
  print_endline "test_store_as_of: OK"
```

> Note: how `ro_begin_as_of` signals errors must match the rest of `Store`. If `Store` raises rather than returns `result` for RO begins (it does — `ro_begin` is `t -> ro txn Lwt.t`), `ro_begin_as_of` should `Lwt.fail` with an exception carrying the `error`. Define `exception History_error of error` in `store.ml`/`.mli` and raise it. Update the test's pattern (`S.History_error …`) to match the actual exception name you expose.

Add to `test/dune`:

```
(test
 (name test_store_as_of)
 (libraries sqlocaml_store lwt lwt.unix))
```

- [ ] **Step 3: Run, verify it fails**

Run: `$DEV dune test test/test_store_as_of.exe`
Expected: FAIL — `Unbound value S.history_pin` / `S.ro_begin_as_of`.

- [ ] **Step 4: Implement the retention API + `ro_begin_as_of`**

In `lib/store/store.ml` add (near `ro_begin`):

```ocaml
exception History_error of error

let bt st = match st.backend with Btree s -> Some s | Mem _ -> None

let history_pin t ~txn_id =
  match bt t with Some st -> st.history_floor <- Some txn_id | None -> ()

let history_floor t =
  match bt t with Some st -> st.history_floor | None -> None

let history_release t =
  match bt t with Some st -> st.history_floor <- None | None -> ()

let history_log t =
  match bt t with
  | Some { history = Some sink; _ } -> sink.History.load ()
  | _ -> Lwt.return []

let ro_begin_as_of t (target : History.target) =
  let* () = Rwlock.acquire_read t.lock in
  match bt t with
  | None ->
    Rwlock.release_read t.lock;
    Lwt.fail (History_error History_unavailable)
  | Some ({ history = None; _ }) ->
    Rwlock.release_read t.lock;
    Lwt.fail (History_error History_unavailable)
  | Some ({ history = Some sink; _ } as st) ->
    if st.closing
    then (Rwlock.release_read t.lock;
          Lwt.fail_with "Store.ro_begin_as_of: store is closing")
    else
      let* records = sink.History.load () in
      (match History.resolve records target with
       | None ->
         Rwlock.release_read t.lock;
         Lwt.fail (History_error History_pruned)
       | Some r ->
         let pruned =
           match st.history_floor with
           | Some f -> Int64.compare r.History.txn_id f < 0
           | None -> false
         in
         if pruned
         then (Rwlock.release_read t.lock; Lwt.fail (History_error History_pruned))
         else
           Lwt.return
             (ro_begin_at t st ~snap_txn_id:r.History.txn_id
                ~snap_meta_root:r.History.root_page))
```

> Match the exact field/accessor names used by `ro_begin` for `t.lock`, `st.closing`, and `t.backend`. `ro_begin` acquires `Rwlock.acquire_read t.lock` and checks `st.closing`; mirror precisely so `ro_end` (unchanged) releases correctly via `active_readers`.

Add the vals to `store.mli` with doc comments:

```ocaml
(** [history_pin t ~txn_id] (#266) sets the retention floor: pages reachable
    from commits [>= txn_id] are never reused, so they stay queryable via
    {!ro_begin_as_of}.  No-op on the in-memory backend or when as-of is off. *)
val history_pin : t -> txn_id:int64 -> unit

(** The current retention floor, or [None] when unset. *)
val history_floor : t -> int64 option

(** Clear the retention floor; superseded pages become reclaimable again
    through the normal freelist. *)
val history_release : t -> unit

(** All retained commit-log records in ascending txn order ([] when as-of is
    off). *)
val history_log : t -> History.record list Lwt.t

(** [ro_begin_as_of t target] opens a read-only snapshot against the retained
    root with the largest txn/timestamp [<=] [target].  Raises
    {!History_error} [History_unavailable] when as-of is off and
    [History_pruned] when the target predates the retained floor.  End it with
    {!ro_end} like any RO txn. *)
val ro_begin_as_of : t -> History.target -> ro txn Lwt.t

(** Raised by {!ro_begin_as_of} to carry an as-of {!error}. *)
exception History_error of error
```

- [ ] **Step 5: Add the commit hook**

In `commit_prepare_btree`, in the `Ok ()` branch after `st.current_header <- …` (line 1724) and before `Lwt.return_unit` (replace the trailing `Lwt.return_unit`):

```ocaml
    Pager.txn_owned_pool_set st.pager [];
    (match st.history with
     | None -> Lwt.return_unit
     | Some sink ->
       let r =
         { History.txn_id = st.current_header.txn_id
         ; timestamp = st.history_now ()
         ; root_page = st.current_header.root_page
         }
       in
       (* Best-effort: a failed append must never fail a durable commit. *)
       Lwt.catch (fun () -> sink.History.append r) (fun _ -> Lwt.return_unit))
```

- [ ] **Step 6: Add the as-of read test that proves time travel**

Append to `test/test_store_as_of.ml` a test that: opens a Btree store with `as_of_history:true` + the mem sink + a monotonic `now`; inserts row A and commits (capture txn T1 via `history_log`); inserts row B and commits; pins the floor at T1; opens `ro_begin_as_of (`Txn T1)`, reads the tree, asserts it sees A but NOT B; `ro_end`; then a live `ro_begin` sees both. Use the same low-level put/get the existing `test/test_store_*.ml` files use. Drive with `Lwt_main.run`.

- [ ] **Step 7: Run the test, verify it passes**

Run: `$DEV dune test test/test_store_as_of.exe`
Expected: PASS — `test_store_as_of: OK` and the time-travel assertions hold.

- [ ] **Step 8: Full suite + format + commit**

```sh
$DEV dune test
for f in lib/store/store.ml lib/store/store.mli test/test_store_as_of.ml; do
  tmp=$(mktemp) && $DEV ocamlformat "$f" > "$tmp" && mv "$tmp" "$f" && chmod 644 "$f"
done
git add lib/store/store.ml lib/store/store.mli test/test_store_as_of.ml test/dune
git commit -m "feat(#266): as-of read path, retention API, commit hook"
```

---

## Task 4: Unix file-backed sink + open-flag plumbing

**Files:**
- Modify: `lib/unix/store.ml` (`open_file` ~88; `open_file_wal` ~185), `lib/unix/store.mli`
- Test: `test/test_unix_as_of.ml` (+ `test/dune` stanza)

- [ ] **Step 1: Write the failing test for the file sink**

`test/test_unix_as_of.ml`: build a file sink over a temp `.aslog`, append three records via the sink, reload via a fresh sink, assert `decode_all` order; then truncate the file mid-record and assert the torn tail is dropped. Use the `file_sink` function created in Step 3 (expose it from `lib/unix/store.mli` as `Sqlocaml_unix.Store.file_history_sink : path:string -> History.sink`).

```ocaml
module U = Sqlocaml_unix.Store
module H = Sqlocaml_store.History
open Lwt.Syntax

let test_file_roundtrip () =
  let path = Filename.temp_file "aslog" ".log" in
  Lwt_main.run
    (let sink = U.file_history_sink ~path in
     let* () = sink.H.append { txn_id = 1L; timestamp = 10L; root_page = 100L } in
     let* () = sink.H.append { txn_id = 2L; timestamp = 20L; root_page = 200L } in
     let sink2 = U.file_history_sink ~path in
     let* recs = sink2.H.load () in
     assert (recs = [ { H.txn_id = 1L; timestamp = 10L; root_page = 100L }
                    ; { H.txn_id = 2L; timestamp = 20L; root_page = 200L } ]);
     Lwt.return_unit);
  Sys.remove path

let () = test_file_roundtrip (); print_endline "test_unix_as_of: OK"
```

Add to `test/dune`:

```
(test
 (name test_unix_as_of)
 (libraries sqlocaml_unix sqlocaml_store lwt lwt.unix))
```

- [ ] **Step 2: Run, verify it fails**

Run: `$DEV dune test test/test_unix_as_of.exe`
Expected: FAIL — `Unbound value U.file_history_sink`.

- [ ] **Step 3: Implement the file sink in `lib/unix/store.ml`**

Add (use the same `Lwt_unix` style the rest of `lib/unix/store.ml` uses):

```ocaml
let file_history_sink ~path : History.sink =
  let append (r : History.record) =
    Lwt_unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND ] 0o644
    >>= fun fd ->
    Lwt.finalize
      (fun () ->
        let buf = History.encode r in
        let bytes = Cstruct.to_bytes buf in
        let* _ = Lwt_unix.write fd bytes 0 (Bytes.length bytes) in
        Lwt_unix.fsync fd)
      (fun () -> Lwt_unix.close fd)
  in
  let load () =
    if not (Sys.file_exists path)
    then Lwt.return []
    else
      Lwt_unix.openfile path [ Unix.O_RDONLY ] 0 >>= fun fd ->
      Lwt.finalize
        (fun () ->
          let len = (Unix.stat path).st_size in
          let bytes = Bytes.create len in
          let rec read_all off =
            if off >= len then Lwt.return_unit
            else
              let* n = Lwt_unix.read fd bytes off (len - off) in
              if n = 0 then Lwt.return_unit else read_all (off + n)
          in
          let* () = read_all 0 in
          Lwt.return (History.decode_all (Cstruct.of_bytes bytes)))
        (fun () -> Lwt_unix.close fd)
  in
  { History.append; load }
```

Expose in `lib/unix/store.mli`:

```ocaml
(** [file_history_sink ~path] (#266) is an append-only, fsync-on-append
    {!Sqlocaml_store.History.sink} backed by the file at [path] (the
    [<db>.aslog] sidecar).  A torn tail record is dropped on load. *)
val file_history_sink : path:string -> Sqlocaml_store.History.sink
```

- [ ] **Step 4: Plumb `?as_of_history` through `open_file` / `open_file_wal`**

Add `?(as_of_history = false)` to both. When true, build `let history = file_history_sink ~path:(path ^ ".aslog")` and a wall clock `let now () = Int64.of_float (Unix.gettimeofday () *. 1000.)`, and pass `~as_of_history ~history ~now` into the underlying `Core.open_block` / `Core.open_block_wal` call. When false, pass nothing (defaults). Add `?as_of_history:bool` to the `open_file`/`open_file_wal` signatures in `lib/unix/store.mli` with a doc comment.

- [ ] **Step 5: Run the file-sink test + full suite**

Run: `$DEV dune test test/test_unix_as_of.exe && $DEV dune test`
Expected: PASS.

- [ ] **Step 6: Format + commit**

```sh
for f in lib/unix/store.ml lib/unix/store.mli test/test_unix_as_of.ml; do
  tmp=$(mktemp) && $DEV ocamlformat "$f" > "$tmp" && mv "$tmp" "$f" && chmod 644 "$f"
done
git add lib/unix/store.ml lib/unix/store.mli test/test_unix_as_of.ml test/dune
git commit -m "feat(#266): Unix file-backed history sink + open flag"
```

---

## Task 5: `Db.query_as_of` SQL-level read + open-flag plumbing

**Files:**
- Modify: `lib/db/db.ml` (the `query` path ~1653-1712; `open_block` ~54), `lib/db/db.mli`
- Test: `test/test_db_as_of.ml` (+ `test/dune` stanza)

- [ ] **Step 1: Write the failing SQL-level test**

`test/test_db_as_of.ml`: open a Db over a temp file with `as_of_history:true`; `CREATE TABLE t(id INTEGER, v TEXT)`; `INSERT … (1,'a')`; commit; record the current head txn via `Db.history_log`/`Store.history_log`; `INSERT … (2,'b')`; pin floor at the first txn; `Db.query_as_of (`Txn first) "SELECT v FROM t"` returns only `'a'`; live `Db.query` returns both. Drive with `Lwt_main.run`. Match the exact `Db.open_block`/temp-file pattern an existing `test/test_db_*.ml` uses.

Add to `test/dune`:

```
(test
 (name test_db_as_of)
 (libraries sqlocaml_db lwt lwt.unix))
```

- [ ] **Step 2: Run, verify it fails**

Run: `$DEV dune test test/test_db_as_of.exe`
Expected: FAIL — `Unbound value Db.query_as_of`.

- [ ] **Step 3: Implement `query_as_of`**

In `lib/db/db.ml`, mirror the existing `query` body (the `Sql.Exec.query ~mode ~clock:t.clock ?stats t.store t.catalog op` path around line 1709), but open a historical RO txn and run in `In_txn` mode:

```ocaml
let query_as_of t (target : Sqlocaml_store.History.target) sql =
  let* ro = S.ro_begin_as_of t.store target in
  Lwt.catch
    (fun () ->
       (* parse/plan [sql] exactly as [query] does to get [op] *)
       match (* build op from sql, as in `query` *) with
       | Error e -> let* () = S.ro_end ro in Lwt.return (Error e)
       | Ok op ->
         let mode = Sql.Exec.In_txn ro in
         (match Sql.Exec.query ~mode ~clock:t.clock t.store t.catalog op with
          | Error e -> let* () = S.ro_end ro in Lwt.return (Error e)
          | Ok stream ->
            (* end the snapshot when the stream is exhausted, mirroring how
               `query` manages its RO txn lifetime — wrap the stream so
               `ro_end ro` runs on close. *)
            Lwt.return (Ok (wrap_stream_with_cleanup stream (fun () -> S.ro_end ro)))))
    (fun exn -> let* () = S.ro_end ro in Lwt.fail exn)
```

> Match `query`'s exact parsing/op-construction and its stream lifetime handling (how it ends its own `ro_begin` txn — see `db.ml:515-527` for the `ro_begin`/`ro_end` bracket pattern). Reuse that bracket so the historical snapshot is released after the stream drains. If `query` factors op-construction into a helper, call it; otherwise lift the shared parse/plan into a small local function and use it from both. Surface `History_error` as a `Db.error` arm (add `History_pruned`/`History_unavailable` to `Db.error` + `pp_error`, converting from `Store.History_error`).

Add to `lib/db/db.mli`:

```ocaml
(** [query_as_of t target sql] (#266) runs a read-only [sql] query against the
    database as it existed at [target] (a past txn id or timestamp).  Requires
    the db to have been opened with [~as_of_history:true]; otherwise the result
    carries [History_unavailable].  [History_pruned] when [target] predates the
    retained floor.  Schema is read at HEAD: a query whose table had DDL after
    [target] may misinterpret older rows (schema-as-of is out of scope, #266). *)
val query_as_of
  :  t
  -> Sqlocaml_store.History.target
  -> string
  -> (row Lwt_stream.t, error) result Lwt.t
```

- [ ] **Step 4: Plumb `?as_of_history` through `Db.open_block`**

Add `?(as_of_history = false)` to `Db.open_block` (and its `.mli`), threading it into the `Sqlocaml_unix.Store.open_file`/`open_file_wal` call it makes. Document default-off.

- [ ] **Step 5: Run the SQL test + full suite**

Run: `$DEV dune test test/test_db_as_of.exe && $DEV dune test`
Expected: PASS — historical query returns only the pre-floor rows.

- [ ] **Step 6: Format + commit**

```sh
for f in lib/db/db.ml lib/db/db.mli test/test_db_as_of.ml; do
  tmp=$(mktemp) && $DEV ocamlformat "$f" > "$tmp" && mv "$tmp" "$f" && chmod 644 "$f"
done
git add lib/db/db.ml lib/db/db.mli test/test_db_as_of.ml test/dune
git commit -m "feat(#266): Db.query_as_of SQL-level time travel + open flag"
```

---

## Task 6: Pre-push gates, docs, coverage, issue hygiene

- [ ] **Step 1: dune-file formatting**

For every `dune` file touched (`test/dune`, any lib `dune` if deps changed):
```sh
diff <($DEV dune format-dune-file test/dune) test/dune || {
  tmp=$(mktemp) && $DEV dune format-dune-file test/dune > "$tmp" && mv "$tmp" test/dune && chmod 644 test/dune; }
```

- [ ] **Step 2: merlint clean for new/changed files**

Run: `$DEV merlint`
Expected: 0 issues for `lib/store/history.*`, `lib/store/store.*`, `lib/unix/store.*`, `lib/db/db.*` (mli present, `(** … *)` docs on every new public val/constructor, nesting ≤ 4; abstract types have `pp`). Fix any findings, re-run.

- [ ] **Step 3: Coverage of the new module**

Run the manual bisect loop (per CLAUDE.md), confirm `History` and the new `Store` functions are exercised. Add targeted cases for any uncovered branch (e.g. `decode` short-buffer, `resolve` empty list, `History_misconfigured`). Re-run `$DEV dune test`.

- [ ] **Step 4: Docs**

Add a short "As-of time travel (#266)" section to `README` (or the docs index used by the repo): default-off `?as_of_history`, that a `.aslog` sidecar is written, the `history_pin`/`query_as_of` usage, and the documented limits (unencrypted only, schema read at HEAD, dense whole-DB retention grows with churn). Commit.

- [ ] **Step 5: Full green + push**

```sh
$DEV dune build && $DEV dune test
git push origin feat/266-whole-db-as-of
```

- [ ] **Step 6: Open the PR (keep #266 open)**

```sh
~/.local/bin/forgejo pr create tej/sqlite_ocaml_port \
  --title="feat(#266): whole-DB as-of time travel (Tier-1)" \
  --head=feat/266-whole-db-as-of --base=main \
  --body="$(cat <<'EOF'
## Summary
- Default-off `?as_of_history` open flag; injected append-only commit log (txn_id -> ts, root)
- Retention via the existing min_safe watermark floor (no new GC); historical reads via ro_begin_at against a retained CoW root
- Unix file sink (`<db>.aslog`); `Db.query_as_of` SQL-level reads
- Spec: docs/superpowers/specs/2026-06-18-whole-db-as-of-design.md

## Test plan
- [ ] dune test passes (history codec, store as-of, unix sink, db SQL-level)
- [ ] feature inert by default — existing suites unchanged

Refs #266 (keep open: per-table retention, sparse retention + mark-sweep GC, Tier-2 remain).
EOF
)"
```

Do **not** close #266 — comment that Tier-1 whole-DB landed and the per-table / sparse / Tier-2 scope stays open.

---

## Self-review notes (addressed)

- **Spec coverage:** log+codec (Task 1), timestamp via injected clock (Task 2/4), floor watermark (Task 2), as-of read + factored `ro_begin_at` (Task 3), commit hook (Task 3), file sink (Task 4), SQL `query_as_of` (Task 5), default-off flag + misconfig (Task 2/4/5), checkpoint-safety + torn-tail + pruned/unavailable errors all have tasks/tests. #266 stays open (Task 6).
- **Uncertain seams flagged explicitly** (rather than faked): the exact `Db.query` op-construction/stream-lifetime to reuse (Task 5 Step 3), the `Store` RO error-signalling convention (`History_error` exception, Task 3 Step 2/4), and the existing temp-file/test-store open helpers (Tasks 3–5). Each step says which existing function to mirror and where it lives.
- **Type consistency:** `History.record`/`target`/`sink` names are used identically across Tasks 1–5; `as_of_history`/`history`/`now` param names match across `create`/`open_block`/`open_block_wal`/`open_file`/`Db.open_block`.
