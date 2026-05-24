(* Phase 1 store.  Two backends share the same interface:

   - [Mem] — pure in-memory [Bytes_map]-per-tree (the Phase 0 backend).
     Used by [create ()].  No I/O, no size limits, no errors.

   - [Btree] — CoW B+-tree over a Pager over a BLOCK device (Unix file).
     Used by [open_file ~path].  Persists across reopen.  Inherits the
     B+-tree leaf-cell size limits (512-byte keys, 1024-byte values).

   The two are wrapped in a sum type so callers see one [Store.t]. *)

open Lwt.Syntax

module Btree    = Sqlocaml_storage.Btree
module Pager    = Sqlocaml_storage.Pager
module Header   = Sqlocaml_storage.Header
module Freelist = Sqlocaml_storage.Freelist
module Page     = Sqlocaml_storage.Page
module Unix_file = Sqlocaml_block.Unix_file
module Varint   = Sqlocaml_encoding.Varint

module Bytes_map = Map.Make (Bytes)

type ro
type rw

type tree_id = int

type error =
  | Block_error of string
  | Corruption of string
  | Key_too_large of int
  | Value_too_large of int
  | Header_error of string

let pp_error fmt = function
  | Block_error s     -> Format.fprintf fmt "Block_error(%s)" s
  | Corruption s      -> Format.fprintf fmt "Corruption(%s)" s
  | Key_too_large n   -> Format.fprintf fmt "Key_too_large(%d)" n
  | Value_too_large n -> Format.fprintf fmt "Value_too_large(%d)" n
  | Header_error s    -> Format.fprintf fmt "Header_error(%s)" s

(* ------------------------------------------------------------------ *)
(* Btree-backend internal state                                         *)
(* ------------------------------------------------------------------ *)

(* The meta-tree is stored separately from user trees (it doesn't live
   in the [trees] hashtable).  It tracks the root_page of every tree_id
   created via [get/put/del]; its OWN root_page is what we commit into
   the header. *)

type bt_savepoint = {
  sp_name        : string;
  sp_meta_root   : int64;
  sp_tree_roots  : (tree_id * int64) list;
  sp_freelist    : Freelist.t;
  sp_n_pages     : int64;
  sp_dirty       : Pager.dirty_snapshot;
}

(* Per-store commit queue for WAL-mode group commit (#77, #151).  After
   a writer has staged its WAL frames it releases [lock] and joins
   this queue to await one shared fsync.  [drainer] is set to [true] by
   the first arriving writer (acting as coordinator); subsequent writers
   register a resolver on [waiters] and block until the drainer wakes
   them with the sync result.  [pending] tracks the number of waiters so
   the drainer can yield additional ticks while new arrivals keep
   registering, widening the batch.  Cooperative Lwt scheduling makes
   the [drainer]/[pending]/[waiters] transitions atomic (no implicit
   yield between read and write).

   The per-batch resolvers carry [(unit, exn) result] so an fsync
   failure in the drainer propagates to every joiner in the same batch
   instead of being silently dropped by a unit-broadcast (#151). *)
type commit_queue = {
  mutable drainer : bool;
  mutable pending : int;
  mutable waiters : (unit, exn) result Lwt.u list;
}

let create_commit_queue () =
  { drainer = false;
    pending = 0;
    waiters = [] }

type bt_state = {
  close_fn             : unit -> unit Lwt.t;
  pager                : Pager.t;
  mutable meta         : Btree.t;
  trees                : (tree_id, Btree.t) Hashtbl.t;
  mutable current_header : Header.t;
  schema_version : int64;
  mutable txn_freelist_snapshot : Freelist.t option;
  (* Snapshot of freelist taken at rw_begin; restored on rollback. None when no RW txn is active. *)
  active_readers : (int64, int) Hashtbl.t;
  (* Maps snap_txn_id -> reference count of active RO txns at that snapshot *)
  mutable bt_savepoints : bt_savepoint list;
  (* Stack of named savepoints; newest at front. Cleared on commit/rollback. *)
  wal : Sqlocaml_storage.Wal.t option;
  (* When set, commits append to this WAL instead of writing to the main
     DB; reads route through it via the Pager hook. *)
  wal_close : (unit -> unit Lwt.t) option;
  mutable wal_autocheckpoint_threshold : int;
  (* When > 0 and committed WAL frames reach this number, the next
     commit triggers an inline checkpoint (still under [lock]) so
     the WAL stays bounded. 0 disables auto-checkpoint. Per-connection,
     not persisted. *)
  commit_queue : commit_queue;
  (* WAL-mode group commit (#77).  Used only when [wal] is [Some];
     allocated unconditionally to keep [bt_state] uniform. *)
  active_reader_frames : (int, int) Hashtbl.t;
  (* WAL committed_frames snapshot value -> refcount of RO snapshots
     captured at that value.  Lets [min_active_reader_frames] compute
     the lowest snapshot bound currently in flight in O(distinct
     snapshots) which is bounded by the number of concurrent readers. *)
  reader_done_cond : unit Lwt_condition.t;
  (* Broadcast on every [ro_end] so a waiting checkpoint can re-check
     [min_active_reader_frames] without busy-waiting. *)
  mutable autockpt_in_flight : bool;
  (* True iff a background autocheckpoint fiber is currently running.
     Used to coalesce: if a commit crosses the threshold while a
     checkpoint is already running, we skip rescheduling. *)
}

let default_wal_autocheckpoint_threshold = 1000

type backend =
  | Mem  of (tree_id, Bytes.t Bytes_map.t ref) Hashtbl.t
  | Btree of bt_state

type t = {
  backend  : backend;
  lock     : Rwlock.t;
  (* Snapshot of Mem backend tree contents taken at rw_begin.
     Used to implement rollback for the in-memory backend.
     None when no RW transaction is active. *)
  mutable mem_rw_snapshot : (tree_id * Bytes.t Bytes_map.t) list option;
  (* Savepoint stack for the Mem backend; newest entry at front.
     Each entry is (savepoint_name, snapshot_of_all_trees). *)
  mutable mem_savepoints  : (string * (tree_id * Bytes.t Bytes_map.t) list) list;
}

let pp fmt t =
  Format.fprintf fmt "Store.t { backend = %s }"
    (match t.backend with Mem _ -> "Mem" | Btree _ -> "Btree")

type ro_snapshot = {
  rs_store          : t;
  rs_snap_txn_id    : int64;
  rs_snap_meta_root : int64;
  rs_snap_trees     : (tree_id, Btree.t) Hashtbl.t;
  rs_snap_frames    : int;
  (* WAL committed_frames at ro_begin; 0 when no WAL is in effect. *)
  rs_pinned         : (int64, unit) Hashtbl.t;
  (* Page ids this snapshot has pinned in the Pager cache (#159).  Every
     snapshot read records the pages it materialises here; [ro_end]
     releases them via [Pager.unpin_all].  Unused for the Mem backend. *)
}

type 'a txn =
  | Ro : ro_snapshot -> ro txn
  | Rw : t -> rw txn

type seek_result =
  | Found of bytes
  | Not_found of [`Greater of bytes | `End]

(* Cursor over either backend.
   For the in-memory backend, the cursor holds an immutable snapshot of
   the bindings as a list (matches Phase 0 semantics).

   For the B+-tree backend we similarly materialise a snapshot (list of
   (k,v) pairs) at cursor_open time.  This is acceptable for Phase 1 and
   makes seek/next semantics identical to the in-memory implementation;
   true streaming cursors come later.

   In both cases [ready] and [remaining] together implement the
   "pre-positioned" semantics from [store.mli]: the first [cursor_next]
   after positioning returns the positioned entry without advancing. *)
type cursor = {
  all : (bytes * bytes) list;
  mutable remaining : (bytes * bytes) list;
  mutable ready : bool;
}

(* ------------------------------------------------------------------ *)
(* Backend helpers — Mem                                                *)
(* ------------------------------------------------------------------ *)

let mem_tree trees tid =
  match Hashtbl.find_opt trees tid with
  | Some r -> r
  | None ->
    let r = ref Bytes_map.empty in
    Hashtbl.add trees tid r;
    r

(* ------------------------------------------------------------------ *)
(* Backend helpers — Btree                                              *)
(* ------------------------------------------------------------------ *)

(* tree_id <-> bytes encoding via varint (zigzag, since negative ids are
   reserved for internal use; we don't actually persist negative ids but
   using signed encoding lets us round-trip safely). *)
let encode_tree_id (tid : tree_id) : bytes =
  let buf = Buffer.create 8 in
  Varint.encode_int64 buf (Int64.of_int tid);
  Buffer.to_bytes buf

let encode_root_page (pid : int64) : bytes =
  let buf = Buffer.create 8 in
  Varint.encode_uint64 buf pid;
  Buffer.to_bytes buf

let decode_root_page (b : bytes) : int64 =
  let v, _ = Varint.decode_uint64 b 0 in
  v

let map_btree_err : Btree.error -> error = function
  | Btree.Pager_error (Pager.Block_error s)  -> Block_error s
  | Btree.Pager_error (Pager.Corruption s)   -> Corruption s
  | Btree.Key_too_large n                    -> Key_too_large n
  | Btree.Value_too_large n                  -> Value_too_large n
  | Btree.Tree_corrupt s                     -> Corruption s

(* The B+-tree treats Bytes by [Bytes.compare]; cursor_seek consumes the
   raw bytes; everything is byte-clean. *)

(* Lookup-or-build the Btree handle for a tree_id.  Looks up the
   tree_id's root page in the meta-tree; if absent (new tree), creates a
   fresh empty Btree (root_page = 0L). *)
let bt_get_tree st (tid : tree_id) : (Btree.t, error) result Lwt.t =
  match Hashtbl.find_opt st.trees tid with
  | Some bt -> Lwt.return_ok bt
  | None ->
    let key = encode_tree_id tid in
    let* r = Btree.get st.meta key in
    match r with
    | Error e -> Lwt.return_error (map_btree_err e)
    | Ok None ->
      let bt = Btree.create st.pager ~root_page:0L in
      Hashtbl.replace st.trees tid bt;
      Lwt.return_ok bt
    | Ok (Some v) ->
      let root_page = decode_root_page v in
      let bt = Btree.create st.pager ~root_page in
      Hashtbl.replace st.trees tid bt;
      Lwt.return_ok bt

(* Convert a result with [error] payload to an Lwt-failing version.  The
   public [get/put/del/cursor_open] signatures don't return [result], so
   B+-tree errors are surfaced as Lwt exceptions. *)
let unwrap_error r =
  match r with
  | Ok v -> Lwt.return v
  | Error e -> Lwt.fail_with (Format.asprintf "Store: %a" pp_error e)

let min_active_reader_txn st =
  Hashtbl.fold (fun txn_id _ acc ->
    match acc with
    | None -> Some txn_id
    | Some m -> Some (Int64.min m txn_id)
  ) st.active_readers None

let min_active_reader_frames (st : bt_state) : int option =
  Hashtbl.fold (fun k _ acc ->
    match acc with
    | None -> Some k
    | Some m -> Some (min m k)
  ) st.active_reader_frames None

(* Lookup-or-build the Btree handle for a tree_id using a snapshot's
   pinned meta root page rather than the live meta tree. *)
let bt_get_tree_ro (snap : ro_snapshot) (st : bt_state) (tid : tree_id)
    : (Btree.t, error) result Lwt.t =
  match Hashtbl.find_opt snap.rs_snap_trees tid with
  | Some bt -> Lwt.return_ok bt
  | None ->
    let snap_frames = if snap.rs_snap_frames = 0 then None
                      else Some snap.rs_snap_frames in
    let snap_meta =
      Btree.create ?snapshot_frames:snap_frames ~pin_set:snap.rs_pinned
        st.pager ~root_page:snap.rs_snap_meta_root
    in
    let key = encode_tree_id tid in
    let* r = Btree.get snap_meta key in
    match r with
    | Error e -> Lwt.return_error (map_btree_err e)
    | Ok None ->
      let bt =
        Btree.create ?snapshot_frames:snap_frames ~pin_set:snap.rs_pinned
          st.pager ~root_page:0L
      in
      Hashtbl.replace snap.rs_snap_trees tid bt;
      Lwt.return_ok bt
    | Ok (Some v) ->
      let root_page = decode_root_page v in
      let bt =
        Btree.create ?snapshot_frames:snap_frames ~pin_set:snap.rs_pinned
          st.pager ~root_page
      in
      Hashtbl.replace snap.rs_snap_trees tid bt;
      Lwt.return_ok bt

(* ------------------------------------------------------------------ *)
(* Freelist page I/O helpers (forward-declared here; used by open_file  *)
(* and commit below)                                                    *)
(* ------------------------------------------------------------------ *)

(* Walk the freelist page chain starting at [first_page], collect all
   entries, and return a reconstructed [Freelist.t]. *)
let read_freelist_pages pager ~first_page : Freelist.t Lwt.t =
  if Int64.equal first_page 0L then Lwt.return Freelist.empty
  else begin
    let rec loop pid acc =
      if Int64.equal pid 0L then Lwt.return (Freelist.of_list (List.rev acc))
      else begin
        let* r = Pager.read pager pid in
        match r with
        | Error _ -> Lwt.return (Freelist.of_list (List.rev acc))
        | Ok buf ->
          let common = Page.read_common buf in
          let n = min common.Page.n_keys Page.max_freelist_entries_per_page in
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

(* ------------------------------------------------------------------ *)
(* create / open_file / close                                           *)
(* ------------------------------------------------------------------ *)

let create () : t =
  { backend = Mem (Hashtbl.create 16); lock = Rwlock.create ();
    mem_rw_snapshot = None; mem_savepoints = [] }

let map_unix_err (e : Unix_file.error) : error =
  match e with
  | Unix_file.Io s -> Block_error s
  | Unix_file.Out_of_bounds { page_id; n_pages } ->
    Block_error
      (Format.asprintf "out of bounds page_id=%Ld n_pages=%Ld" page_id n_pages)

let map_header_err (e : Header.error) : error =
  match e with
  | Header.Io s -> Header_error s
  | Header.Both_headers_corrupt ->
    Header_error "both header pages corrupt"

(* Build a Pager that delegates to a Unix_file. *)
let pager_of_unix_file (f : Unix_file.t) ~freelist : Pager.t =
  let read_page ~page_id buf =
    let%lwt r = Unix_file.read_page f ~page_id buf in
    match r with
    | Ok () -> Lwt.return_ok ()
    | Error e -> Lwt.return_error (Format.asprintf "%a" Unix_file.pp_error e)
  in
  let write_page ~page_id buf =
    let%lwt r = Unix_file.write_page f ~page_id buf in
    match r with
    | Ok () -> Lwt.return_ok ()
    | Error e -> Lwt.return_error (Format.asprintf "%a" Unix_file.pp_error e)
  in
  let sync () =
    let%lwt r = Unix_file.sync f in
    match r with
    | Ok () -> Lwt.return_ok ()
    | Error e -> Lwt.return_error (Format.asprintf "%a" Unix_file.pp_error e)
  in
  let resize ~n_pages =
    let%lwt r = Unix_file.resize f ~n_pages in
    match r with
    | Ok () -> Lwt.return_ok ()
    | Error e -> Lwt.return_error (Format.asprintf "%a" Unix_file.pp_error e)
  in
  let n_pages = Unix_file.n_pages f in
  Pager.create ~read_page ~write_page ~sync ~resize ~n_pages ~freelist

(* Build a fully-initialised [t] wrapping a B-tree-backed [bt_state] from the
   given pager/meta/header.  [wal]/[wal_close] default to None (plain opens);
   WAL opens pass [Some _]. *)
let make_btree_store ?(wal = None) ?(wal_close = None)
    ~close_fn ~pager ~meta ~(h : Header.t) () =
  let st =
    { close_fn; pager; meta;
      trees = Hashtbl.create 16;
      current_header = h;
      schema_version = h.schema_version;
      txn_freelist_snapshot = None;
      active_readers = Hashtbl.create 4;
      bt_savepoints = []; wal; wal_close;
      wal_autocheckpoint_threshold = default_wal_autocheckpoint_threshold;
      commit_queue = create_commit_queue ();
      active_reader_frames = Hashtbl.create 4;
      reader_done_cond = Lwt_condition.create ();
      autockpt_in_flight = false }
  in
  { backend = Btree st; lock = Rwlock.create ();
    mem_rw_snapshot = None; mem_savepoints = [] }

let open_file ~path : (t, error) result Lwt.t =
  let%lwt fr = Unix_file.open_ ~path in
  match fr with
  | Error e -> Lwt.return_error (map_unix_err e)
  | Ok file ->
    let n_pages = Unix_file.n_pages file in
    if Int64.compare n_pages 0L = 0 then begin
      (* Fresh file — pre-resize to 2 pages so Header.init can write the
         two alternating header pages, then initialise them.  We MUST create
         the pager AFTER the resize so it knows n_pages=2; otherwise
         [Pager.alloc] would re-allocate page 0. *)
      let%lwt rr = Unix_file.resize file ~n_pages:2L in
      match rr with
      | Error e ->
        let%lwt _ = Unix_file.close file in
        Lwt.return_error (map_unix_err e)
      | Ok () ->
      let pager = pager_of_unix_file file ~freelist:Freelist.empty in
      let%lwt ir = Header.init pager in
      match ir with
      | Error e -> Lwt.return_error (map_header_err e)
      | Ok () ->
        let%lwt hr = Header.read_live pager in
        match hr with
        | Error e -> Lwt.return_error (map_header_err e)
        | Ok h ->
          let meta = Btree.create pager ~root_page:0L in
          let close_fn () = let%lwt _ = Unix_file.close file in Lwt.return_unit in
          Lwt.return_ok (make_btree_store ~close_fn ~pager ~meta ~h ())
    end else begin
      let pager = pager_of_unix_file file ~freelist:Freelist.empty in
      let%lwt hr = Header.read_live pager in
      match hr with
      | Error e -> Lwt.return_error (map_header_err e)
      | Ok h ->
        let%lwt fl = read_freelist_pages pager ~first_page:h.freelist_page in
        Pager.set_freelist pager fl;
        let meta = Btree.create pager ~root_page:h.root_page in
        let close_fn () = let%lwt _ = Unix_file.close file in Lwt.return_unit in
        Lwt.return_ok (make_btree_store ~close_fn ~pager ~meta ~h ())
    end

let close (t : t) : unit Lwt.t =
  match t.backend with
  | Mem _ -> Lwt.return_unit
  | Btree st ->
    let* () =
      match st.wal_close with
      | None -> Lwt.return_unit
      | Some f -> f ()
    in
    st.close_fn ()

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
    (* Fresh device — initialise headers *)
    let%lwt ir = Header.init pager in
    (match ir with
    | Error e -> Lwt.return_error (map_header_err e)
    | Ok () ->
      Pager.set_n_pages pager 2L;
      let%lwt hr2 = Header.read_live pager in
      (match hr2 with
      | Error e -> Lwt.return_error (map_header_err e)
      | Ok h ->
        let meta = Btree.create pager ~root_page:0L in
        Lwt.return_ok (make_btree_store ~close_fn:close ~pager ~meta ~h ())))
  | Error e -> Lwt.return_error (map_header_err e)
  | Ok h ->
    Pager.set_n_pages pager h.n_pages_total;
    let%lwt fl = read_freelist_pages pager ~first_page:h.freelist_page in
    Pager.set_freelist pager fl;
    let meta = Btree.create pager ~root_page:h.root_page in
    Lwt.return_ok (make_btree_store ~close_fn:close ~pager ~meta ~h ())

(* ------------------------------------------------------------------ *)
(* WAL-mode opens                                                       *)
(* ------------------------------------------------------------------ *)

module Wal = Sqlocaml_storage.Wal

let install_wal_hook (pager : Pager.t) (wal : Wal.t) =
  let cb : Pager.wal_callbacks = {
    wal_find_page = (fun pid -> Wal.find_page wal pid);
    wal_find_page_at = (fun pid ~max_frame ->
      Wal.find_page_at wal pid ~max_frame);
    wal_read_frame = (fun idx ->
      let* r = Wal.read_frame wal idx in
      match r with
      | Ok page -> Lwt.return_ok page
      | Error e -> Lwt.return_error (Format.asprintf "%a" Wal.pp_error e));
    wal_append_commit = (fun pages ->
      let* r = Wal.append_commit wal pages in
      match r with
      | Ok () -> Lwt.return_ok ()
      | Error e -> Lwt.return_error (Format.asprintf "%a" Wal.pp_error e));
    wal_append_commit_no_sync = (fun pages ->
      let* r = Wal.append_commit_no_sync wal pages in
      match r with
      | Ok () -> Lwt.return_ok ()
      | Error e -> Lwt.return_error (Format.asprintf "%a" Wal.pp_error e));
    wal_sync = (fun () ->
      let* r = Wal.flush_sync wal in
      match r with
      | Ok () -> Lwt.return_ok ()
      | Error e -> Lwt.return_error (Format.asprintf "%a" Wal.pp_error e));
  } in
  Pager.set_wal pager (Some cb)

(* After the WAL hook is installed, re-read the (now WAL-aware) header,
   reconcile [n_pages] for a freshly-initialised DB, load the freelist, and
   build the WAL-backed store. *)
let finish_wal_open ~close ~wal_close ~pager ~wal ~was_fresh =
  let%lwt hr2 = Header.read_live pager in
  match hr2 with
  | Error e -> Lwt.return_error (map_header_err e)
  | Ok h ->
    (* If the header n_pages_total is below the pager's current allocation,
       prefer the pager's value (freshly-init'd headers carry
       n_pages_total = 0). *)
    let chosen_n_pages =
      if was_fresh
      then Int64.max h.n_pages_total (Pager.n_pages pager)
      else h.n_pages_total
    in
    Pager.set_n_pages pager chosen_n_pages;
    let%lwt fl = read_freelist_pages pager ~first_page:h.freelist_page in
    Pager.set_freelist pager fl;
    let meta = Btree.create pager ~root_page:h.root_page in
    Lwt.return_ok
      (make_btree_store ~wal:(Some wal) ~wal_close:(Some wal_close)
         ~close_fn:close ~pager ~meta ~h ())

let open_block_wal
    ~(read_page  : page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
    ~(write_page : page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
    ~(sync       : unit -> (unit, string) result Lwt.t)
    ~(resize     : n_pages:int64 -> (unit, string) result Lwt.t)
    ~(n_pages    : int64)
    ~(wal_read_at  : offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
    ~(wal_write_at : offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
    ~(wal_sync     : unit -> (unit, string) result Lwt.t)
    ~(wal_size_bytes : int64)
    ~(close      : unit -> unit Lwt.t)
    ~(wal_close  : unit -> unit Lwt.t)
    : (t, error) result Lwt.t =
  let pager =
    Pager.create ~read_page ~write_page ~sync ~resize ~n_pages
      ~freelist:Freelist.empty
  in
  (* Step 1: read the main-DB header (or initialise if fresh). The WAL
     hook is NOT installed yet, so writes go directly to the main DB.
     [was_fresh] flag preserves the post-init n_pages override below. *)
  let%lwt hr = Header.read_live pager in
  let%lwt init_result =
    match hr with
    | Error Header.Both_headers_corrupt ->
      let%lwt ir = Header.init pager in
      (match ir with
       | Error e -> Lwt.return_error (map_header_err e)
       | Ok () ->
         Pager.set_n_pages pager 2L;
         Lwt.return_ok true)
    | Error e -> Lwt.return_error (map_header_err e)
    | Ok _ -> Lwt.return_ok false
  in
  match init_result with
  | Error e -> Lwt.return_error e
  | Ok was_fresh ->
    (* Step 2: open the WAL and recover its index. *)
    let%lwt wr =
      Wal.open_ ~read_at:wal_read_at ~write_at:wal_write_at
        ~sync:wal_sync ~size_bytes:wal_size_bytes
    in
    (match wr with
     | Error e ->
       Lwt.return_error
         (Block_error (Format.asprintf "wal open: %a" Wal.pp_error e))
     | Ok wal ->
       (* Step 3: install the hook so subsequent reads consult the WAL. *)
       install_wal_hook pager wal;
       (* Step 4: re-read the header (now WAL-aware) and build the store. *)
       finish_wal_open ~close ~wal_close ~pager ~wal ~was_fresh)

(* ------------------------------------------------------------------ *)
(* WAL convenience: open a main DB + WAL on the same path prefix.       *)
(* The main DB lives at [path] and the WAL at [path ^ "-wal"].          *)
(* ------------------------------------------------------------------ *)

let unix_file_read_at file ~offset (out : Cstruct.t) =
  let len = Cstruct.length out in
  try
    let _ = Unix.lseek file (Int64.to_int offset) Unix.SEEK_SET in
    let tmp = Bytes.create len in
    let rec loop o r =
      if r = 0 then ()
      else
        let n = Unix.read file tmp o r in
        if n = 0 then begin
          (* read past EOF — return zeros for the remainder *)
          Bytes.fill tmp o r '\x00';
        end else
          loop (o + n) (r - n)
    in
    loop 0 len;
    Cstruct.blit_from_bytes tmp 0 out 0 len;
    Lwt.return (Ok ())
  with Unix.Unix_error (e, _, _) -> Lwt.return (Error (Unix.error_message e))

let unix_file_write_at file ~offset (src : Cstruct.t) =
  let len = Cstruct.length src in
  try
    let _ = Unix.lseek file (Int64.to_int offset) Unix.SEEK_SET in
    let tmp = Bytes.create len in
    Cstruct.blit_to_bytes src 0 tmp 0 len;
    let rec loop o r =
      if r = 0 then ()
      else
        let n = Unix.write file tmp o r in
        if n = 0 then failwith "short write"
        else loop (o + n) (r - n)
    in
    loop 0 len;
    Lwt.return (Ok ())
  with Unix.Unix_error (e, _, _) -> Lwt.return (Error (Unix.error_message e))

(* The four pager block-IO callbacks backed by a [Unix_file.t], each mapping
   the file's typed error to the [string] error the pager expects. *)
let unix_file_pager_ops file =
  let wrap = function
    | Ok () -> Lwt.return_ok ()
    | Error e -> Lwt.return_error (Format.asprintf "%a" Unix_file.pp_error e)
  in
  let read_page ~page_id buf =
    let%lwt r = Unix_file.read_page file ~page_id buf in wrap r
  in
  let write_page ~page_id buf =
    let%lwt r = Unix_file.write_page file ~page_id buf in wrap r
  in
  let sync () = let%lwt r = Unix_file.sync file in wrap r in
  let resize ~n_pages = let%lwt r = Unix_file.resize file ~n_pages in wrap r in
  (read_page, write_page, sync, resize)

let open_file_wal ~path : (t, error) result Lwt.t =
  let%lwt fr = Unix_file.open_ ~path in
  match fr with
  | Error e -> Lwt.return_error (map_unix_err e)
  | Ok file ->
    let wal_path = path ^ "-wal" in
    let wal_fd =
      try Unix.openfile wal_path [Unix.O_RDWR; Unix.O_CREAT] 0o644
      with Unix.Unix_error _ ->
        Unix.openfile wal_path [Unix.O_RDWR; Unix.O_CREAT] 0o644
    in
    let wal_size_bytes =
      Int64.of_int (Unix.lseek wal_fd 0 Unix.SEEK_END)
    in
    let (read_page, write_page, sync, resize) = unix_file_pager_ops file in
    let n_pages = Unix_file.n_pages file in
    (* Fresh main DB: pre-resize to 2 pages for the alternating headers. *)
    let%lwt () =
      if Int64.equal n_pages 0L then begin
        let%lwt _ = Unix_file.resize file ~n_pages:2L in
        Lwt.return_unit
      end else Lwt.return_unit
    in
    let n_pages = Unix_file.n_pages file in
    let wal_read_at  = unix_file_read_at  wal_fd in
    let wal_write_at = unix_file_write_at wal_fd in
    let wal_sync () =
      try Unix.fsync wal_fd; Lwt.return_ok ()
      with Unix.Unix_error (e, _, _) ->
        Lwt.return_error (Unix.error_message e)
    in
    let close () =
      let%lwt _ = Unix_file.close file in Lwt.return_unit
    in
    let wal_close () =
      (try Unix.close wal_fd with Unix.Unix_error _ -> ());
      Lwt.return_unit
    in
    open_block_wal
      ~read_page ~write_page ~sync ~resize ~n_pages
      ~wal_read_at ~wal_write_at ~wal_sync ~wal_size_bytes
      ~close ~wal_close

(* ------------------------------------------------------------------ *)
(* Transactions                                                         *)
(* ------------------------------------------------------------------ *)

let ro_begin t =
  (* [Rwlock.acquire_read] is a counter bump, not an exclusion: under
     snapshot isolation readers and writers don't conflict, so the call
     never blocks regardless of writer state.  It only matters for the
     checkpoint coordinator that wants to know "are any RO snapshots
     still in flight?" *)
  let* () = Rwlock.acquire_read t.lock in
  match t.backend with
  | Mem _ ->
    Lwt.return
      (Ro { rs_store = t; rs_snap_txn_id = 0L;
            rs_snap_meta_root = 0L;
            rs_snap_trees = Hashtbl.create 1;
            rs_snap_frames = 0;
            rs_pinned = Hashtbl.create 1 })
  | Btree st ->
    let snap_txn_id    = st.current_header.txn_id in
    let snap_meta_root = st.current_header.root_page in
    let snap_frames =
      match st.wal with
      | None   -> 0
      | Some w -> Wal.committed_frames w
    in
    let count = Option.value ~default:0
                  (Hashtbl.find_opt st.active_readers snap_txn_id) in
    Hashtbl.replace st.active_readers snap_txn_id (count + 1);
    let frame_count = Option.value ~default:0
                        (Hashtbl.find_opt st.active_reader_frames snap_frames) in
    Hashtbl.replace st.active_reader_frames snap_frames (frame_count + 1);
    Lwt.return
      (Ro { rs_store = t; rs_snap_txn_id = snap_txn_id;
            rs_snap_meta_root = snap_meta_root;
            rs_snap_trees = Hashtbl.create 4;
            rs_snap_frames = snap_frames;
            rs_pinned = Hashtbl.create 64 })

let rw_begin t =
  let* () = Rwlock.acquire_write t.lock in
  (match t.backend with
   | Mem trees ->
     (* Snapshot all currently-existing trees so rollback can restore them. *)
     let snap = Hashtbl.fold (fun tid r acc -> (tid, !r) :: acc) trees [] in
     t.mem_rw_snapshot <- Some snap;
     t.mem_savepoints <- []
   | Btree st ->
     let current_rw_txn_id = Int64.add st.current_header.txn_id 1L in
     Pager.set_txn_id st.pager current_rw_txn_id;
     (* Safety: readers registered via ro_begin AFTER this rw_begin are visible at the
        NEXT rw_begin (active_readers is checked at every rw_begin). Pages freed in
        the current txn (freed_at = current_rw_txn_id) cannot be reused within this
        txn because freed_at < alloc_min_safe = current_rw_txn_id is false. *)
     let min_safe =
       match min_active_reader_txn st with
       | None   -> current_rw_txn_id
       | Some m -> Int64.min current_rw_txn_id m
     in
     Pager.set_alloc_min_safe st.pager min_safe;
     st.txn_freelist_snapshot <- Some (Pager.freelist st.pager));
  Lwt.return (Rw t)

let ro_end (Ro snap : ro txn) =
  (match snap.rs_store.backend with
   | Mem _ -> ()
   | Btree st ->
     let tid = snap.rs_snap_txn_id in
     (match Hashtbl.find_opt st.active_readers tid with
      | None | Some 1 -> Hashtbl.remove st.active_readers tid
      | Some n -> Hashtbl.replace st.active_readers tid (n - 1));
     (match Hashtbl.find_opt st.active_reader_frames snap.rs_snap_frames with
      | None | Some 1 ->
        Hashtbl.remove st.active_reader_frames snap.rs_snap_frames
      | Some n ->
        Hashtbl.replace st.active_reader_frames snap.rs_snap_frames (n - 1));
     (* Release the pages this snapshot pinned (#159) so they become
        evictable again. *)
     Pager.unpin_all st.pager snap.rs_pinned;
     Lwt_condition.broadcast st.reader_done_cond ());
  Rwlock.release_read snap.rs_store.lock;
  Lwt.return_unit

let with_ro t f =
  let* tx = ro_begin t in
  Lwt.finalize (fun () -> f tx) (fun () -> ro_end tx)

(* Free the previous freelist page chain back into the pager's in-memory
   freelist (stamped with the current txn_id). *)
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
      (* Note: if read fails mid-chain, remaining pages beyond this point are
         orphaned (leaked). This is acceptable only because a corrupt freelist
         page implies a deeper storage invariant violation. *)
      Pager.free pager ~page_id:pid
        ~freed_at_txn_id:(Pager.get_txn_id pager);
      loop next_pid
    end
  in
  loop first_page

(* Serialize the current pager freelist to a new page chain.
   Returns the first page id (0L if the freelist is empty). *)
(* Build and write a single freelist page holding [chunk] (possibly empty),
   chaining to [next]. *)
let write_one_freelist_page pager ~pid ~next ~chunk =
  let buf = Cstruct.create Page.page_size in
  Cstruct.memset buf 0;
  Page.write_common buf
    { Page.kind = Page.Freelist; flags = 0;
      n_keys = List.length chunk;
      right_page = Int64.to_int32 next; crc32 = 0l };
  List.iteri (fun j (page_id, freed_at_txn_id) ->
    Page.freelist_set_entry buf ~index:j ~page_id ~freed_at_txn_id
  ) chunk;
  Pager.write pager pid buf

let write_freelist_pages pager : int64 Lwt.t =
  let entries_before = Freelist.to_list (Pager.freelist pager) in
  let n_entries = List.length entries_before in
  let max_per = Page.max_freelist_entries_per_page in
  let n_fl_pages = (n_entries + max_per - 1) / max_per in
  if n_fl_pages = 0 then Lwt.return 0L
  else begin
    (* Allocate all needed pages *)
    let* page_ids =
      Lwt_list.map_s (fun () ->
        let* r = Pager.alloc pager in
        match r with
        | Ok pid -> Lwt.return pid
        | Error e ->
          Lwt.fail_with
            (Format.asprintf "write_freelist_pages: %a" Pager.pp_error e)
      ) (List.init n_fl_pages (fun _ -> ()))
    in
    (* Get FINAL freelist state after allocations *)
    let final_entries = Freelist.to_list (Pager.freelist pager) in
    (* Split into chunks of max_per *)
    let rec chunkify = function
      | [] -> []
      | lst ->
        let chunk = List.filteri (fun i _ -> i < max_per) lst in
        let rest  = List.filteri (fun i _ -> i >= max_per) lst in
        chunk :: chunkify rest
    in
    let chunks = chunkify final_entries in
    let n_chunks = List.length chunks in
    let pid_arr = Array.of_list page_ids in
    let next_of i =
      if i + 1 < Array.length pid_arr then pid_arr.(i+1) else 0L
    in
    (* Write each chunk to a freelist page *)
    List.iteri (fun i chunk ->
      write_one_freelist_page pager ~pid:pid_arr.(i) ~next:(next_of i) ~chunk
    ) chunks;
    (* Any extra allocated pages (n_fl_pages > n_chunks) get empty freelist pages *)
    for i = n_chunks to n_fl_pages - 1 do
      write_one_freelist_page pager ~pid:pid_arr.(i) ~next:(next_of i) ~chunk:[]
    done;
    Lwt.return pid_arr.(0)
  end

(** Body of [checkpoint] without mutex management. Caller MUST already
    hold [t.lock] (e.g. during [commit]). Defined here so [commit]
    can invoke it via [maybe_autocheckpoint] below. *)
(* Block until every active RO snapshot's [committed_frames] bound is at
   least [target].  Used by [checkpoint_unlocked] before [Wal.reset]
   truncates the index — otherwise an in-flight reader's [find_page_at]
   would resolve to a recycled frame index after the next writer's append.

   No [~mutex] is passed to [Lwt_condition.wait]: under cooperative Lwt
   the multiset check + wait register atomically (no yield between
   [min_active_reader_frames] and the wait), so the standard POSIX
   condvar mutex pairing isn't needed.  Would need revisiting under a
   preemptive or effect-based multicore runtime. *)
let rec wait_for_readers_past (st : bt_state) ~target =
  match min_active_reader_frames st with
  | Some m when m < target ->
    let* () = Lwt_condition.wait st.reader_done_cond in
    wait_for_readers_past st ~target
  | _ -> Lwt.return_unit

let checkpoint_unlocked (st : bt_state) (wal : Wal.t) : unit Lwt.t =
  let target = Wal.committed_frames wal in
  let* () = wait_for_readers_past st ~target in
  let pairs = ref [] in
  Wal.iter_index wal (fun pid idx -> pairs := (pid, idx) :: !pairs);
  let rec write_each = function
    | [] -> Lwt.return_unit
    | (pid, idx) :: rest ->
      let* r = Wal.read_frame wal idx in
      (match r with
       | Error e ->
         Lwt.fail_with (Format.asprintf "checkpoint read: %a"
                          Wal.pp_error e)
       | Ok page ->
         let* wr =
           Pager.flush_one_to_main st.pager ~page_id:pid ~buf:page
         in
         (match wr with
          | Error e ->
            Lwt.fail_with
              (Format.asprintf "checkpoint write: %a"
                 Pager.pp_error e)
          | Ok () -> write_each rest))
  in
  let* () = write_each !pairs in
  let* sr = Pager.flush_sync_main st.pager in
  (match sr with
   | Error e ->
     Lwt.fail_with
       (Format.asprintf "checkpoint sync: %a" Pager.pp_error e)
   | Ok () ->
     Wal.reset wal;
     Lwt.return_unit)

(** Called from [commit] while [lock] is still held (exclusive). If the WAL has
    grown past the per-connection threshold, migrate it inline so
    subsequent commits start fresh. Best-effort: a checkpoint failure
    is swallowed (the commit itself already succeeded). *)
let maybe_autocheckpoint (st : bt_state) : unit Lwt.t =
  match st.wal with
  | None -> Lwt.return_unit
  | Some wal ->
    let thr = st.wal_autocheckpoint_threshold in
    if thr <= 0 then Lwt.return_unit
    else if Wal.committed_frames wal < thr then Lwt.return_unit
    else
      Lwt.catch
        (fun () -> checkpoint_unlocked st wal)
        (fun _ -> Lwt.return_unit)

(* Group-commit coordinator (#77, #151).  One fiber per [commit_queue]
   runs the actual fsync via [sync_fn]; concurrent writers register a
   per-batch resolver and block until the drainer wakes them with the
   sync result.  Returns the role this fiber played so the caller can
   attach drainer-only side work (e.g. autocheckpoint).

   Each joiner pushes a [(unit, exn) result Lwt.u] resolver onto
   [waiters] and increments [pending] so the drainer can detect
   concurrent arrivals.  The drainer yields via [Lwt.pause] once to let
   any ready-to-write fibers reach the queue, then loops pausing while
   [pending] keeps growing.  This widens the batch from "2 commits per
   fsync" (one Lwt.pause yields one continuation) to "N concurrent
   writers per fsync" while adding only one tick of latency to a lone
   writer.

   Failure semantics (#151): if [sync_fn] raises, the drainer wakes
   every joiner's resolver with [Error exn] (so each joiner re-raises
   the same exception via [Lwt.fail]) and then re-raises to its own
   caller.  All N writers in the current batch observe the failure;
   none see a spurious [Ok].

   Late joiners that arrive while [sync_fn] is in flight register on
   the same [waiters] list (since [q.drainer] is still [true]) and so
   ride along with the current sync's result — matching the pre-#151
   broadcast behaviour.  Whether those late frames are physically
   flushed by the in-flight fsync is timing-dependent at the kernel
   level (POSIX only guarantees flushing of writes queued before the
   fsync syscall); this is a pre-existing concern, not introduced by
   the error-channel rework. *)
let group_commit_sync (q : commit_queue) (sync_fn : unit -> unit Lwt.t)
    : [`Drainer | `Joiner] Lwt.t =
  if q.drainer then begin
    let (p, u) = Lwt.wait () in
    q.waiters <- u :: q.waiters;
    q.pending <- q.pending + 1;
    let* r = p in
    match r with
    | Ok () -> Lwt.return `Joiner
    | Error exn -> Lwt.fail exn
  end else begin
    q.drainer <- true;
    (* Gather: one initial pause to let the next-in-line writer reach
       the queue; then keep pausing while [pending] keeps growing.
       Stops as soon as a pause completes without seeing any new
       arrival — keeping per-commit overhead bounded for solo
       writers. *)
    let* () = Lwt.pause () in
    let rec gather last_seen =
      let now_seen = q.pending in
      if now_seen > last_seen then
        let* () = Lwt.pause () in
        gather now_seen
      else Lwt.return_unit
    in
    let* () = gather 0 in
    (* Run sync first, capturing success or failure; then atomically
       snapshot the (possibly grown) waiter list, clear queue state for
       the next batch, and wake each joiner with the same result.
       Cooperative scheduling guarantees no yield between try_bind's
       handler and the iter, so no joiner can register after the
       snapshot. *)
    Lwt.try_bind
      sync_fn
      (fun () ->
        let waiters = q.waiters in
        q.waiters <- [];
        q.pending <- 0;
        q.drainer <- false;
        List.iter (fun u -> Lwt.wakeup_later u (Ok ())) waiters;
        Lwt.return `Drainer)
      (fun exn ->
        let waiters = q.waiters in
        q.waiters <- [];
        q.pending <- 0;
        q.drainer <- false;
        List.iter (fun u -> Lwt.wakeup_later u (Error exn)) waiters;
        Lwt.fail exn)
  end

(* Prepare phase of [commit] for the Btree backend.  Pushes every tree's
   latest root_page through the meta-tree, writes the freelist pages,
   and invokes [~header_commit] (either {!Header.commit} for the inline-
   sync path or {!Header.commit_no_sync} for group commit).  On success
   advances [st.current_header] and resets per-txn state.  On failure
   raises via [Lwt.fail_with] without touching the mutex. *)
let commit_prepare_btree
    ~(header_commit :
        Pager.t ->
        prev_header:Header.t ->
        new_state:Header.t ->
        (unit, Header.error) result Lwt.t)
    (st : bt_state) : unit Lwt.t =
  let* () = free_old_freelist_pages st.pager
              ~first_page:st.current_header.freelist_page
  in
  let bindings =
    Hashtbl.fold (fun tid bt acc -> (tid, bt) :: acc) st.trees []
  in
  let* () =
    Lwt_list.iter_s (fun (tid, bt) ->
      let key = encode_tree_id tid in
      let v   = encode_root_page (Btree.root_page bt) in
      let* r = Btree.put st.meta key v in
      match r with
      | Ok meta' -> st.meta <- meta'; Lwt.return_unit
      | Error e -> Lwt.fail_with
        (Format.asprintf "Store.commit: %a" pp_error (map_btree_err e))
    ) bindings
  in
  let* freelist_first_page = write_freelist_pages st.pager in
  let new_state : Header.t =
    { txn_id         = 0L;  (* overwritten by header_commit *)
      root_page      = Btree.root_page st.meta;
      freelist_page  = freelist_first_page;
      n_pages_total  = Pager.n_pages st.pager;
      schema_version = st.schema_version }
  in
  let* r = header_commit st.pager
             ~prev_header:st.current_header ~new_state
  in
  match r with
  | Error e ->
    Lwt.fail_with
      (Format.asprintf "Store.commit: %a" pp_error (map_header_err e))
  | Ok () ->
    st.current_header <-
      { new_state with
        txn_id = Int64.add st.current_header.txn_id 1L };
    st.txn_freelist_snapshot <- None;
    st.bt_savepoints <- [];
    Lwt.return_unit

(* commit:
   - Mem backend: no I/O, just release the writer lock.
   - Btree backend without WAL: flush all currently-open trees'
     root_pages into the meta-tree, then write a new header pointing at
     the new meta root, sync inline, autocheckpoint, release.
   - Btree backend with WAL: same prepare phase but using
     [Header.commit_no_sync] so the writer can release [lock]
     before the fsync.  Writers then converge on a per-store
     [commit_queue]; one drainer fsyncs and resolves all waiters.  Only
     the drainer attempts the autocheckpoint (single check per fsync
     covers the whole batch).

   Note: the Btree.create/put/del API returns a NEW Btree.t after every
   mutation (root_page may have changed).  We update [st.trees] each
   time; here we additionally persist the latest root_page for each
   touched tree into the meta-tree (whose own root we then commit via
   the header alternating-pages protocol). *)
(* After a WAL group-commit, the drainer kicks off an async autocheckpoint if
   the WAL has grown past the threshold and none is already in flight. *)
let maybe_autockpt_after_commit t st =
  if st.wal_autocheckpoint_threshold <= 0
     || Wal.committed_frames
          (match st.wal with Some w -> w | None -> assert false)
        < st.wal_autocheckpoint_threshold
     || st.autockpt_in_flight
  then Lwt.return_unit
  else begin
    st.autockpt_in_flight <- true;
    Lwt.async (fun () ->
      Lwt.finalize
        (fun () ->
          Lwt.catch
            (fun () ->
              let* () = Rwlock.acquire_write t.lock in
              Lwt.finalize
                (fun () ->
                  match st.wal with
                  | None -> Lwt.return_unit
                  | Some wal -> checkpoint_unlocked st wal)
                (fun () ->
                  Rwlock.release_write t.lock;
                  Lwt.return_unit))
            (fun _ -> Lwt.return_unit))
        (fun () ->
          st.autockpt_in_flight <- false;
          Lwt.return_unit));
    Lwt.return_unit
  end

(* WAL-mode commit: prepare the btree (no sync), release the write lock early,
   then group-commit-sync the WAL.  The elected drainer may autocheckpoint. *)
let commit_wal t st =
  let unlocked = ref false in
  let unlock_once () =
    if not !unlocked then begin
      unlocked := true;
      Rwlock.release_write t.lock
    end
  in
  Lwt.catch
    (fun () ->
      let* () =
        commit_prepare_btree ~header_commit:Header.commit_no_sync st
      in
      unlock_once ();
      let* role =
        group_commit_sync st.commit_queue (fun () ->
          let* r = Pager.wal_sync st.pager in
          match r with
          | Ok () -> Lwt.return_unit
          | Error e ->
            Lwt.fail_with
              (Format.asprintf "Store.commit: wal_sync: %a"
                 Pager.pp_error e))
      in
      (match role with
       | `Joiner -> Lwt.return_unit
       | `Drainer -> maybe_autockpt_after_commit t st))
    (fun exn ->
      unlock_once ();
      Lwt.fail exn)

let commit (Rw t : rw txn) : unit Lwt.t =
  match t.backend with
  | Mem _ ->
    t.mem_rw_snapshot <- None;
    t.mem_savepoints <- [];
    Rwlock.release_write t.lock;
    Lwt.return_unit
  | Btree st ->
    match st.wal with
    | None ->
      Lwt.finalize
        (fun () ->
          let* () =
            commit_prepare_btree ~header_commit:Header.commit st
          in
          maybe_autocheckpoint st)
        (fun () -> Rwlock.release_write t.lock; Lwt.return_unit)
    | Some _ -> commit_wal t st

(* rollback:
   - Mem: restore the snapshot of tree contents taken at rw_begin, so that
     mutations made during this txn are undone.
   - Btree: drop cached tree handles so subsequent reads pick up
     last-committed roots from the meta-tree, then restore the freelist
     snapshot taken at rw_begin and clear dirty pages.
     Discard dirty pages from the aborted txn: clear_dirty removes them from
     both the dirty set and the read cache, so subsequent reads see committed
     data from disk. The freelist snapshot ensures no aborted CoW frees
     corrupt future allocations. *)
let rollback (Rw t : rw txn) : unit Lwt.t =
  (match t.backend with
   | Mem trees ->
     (* Restore tree contents to the snapshot taken at rw_begin. *)
     (match t.mem_rw_snapshot with
      | None -> ()   (* no snapshot (shouldn't happen) *)
      | Some snap ->
        (* Restore each tree that existed at snapshot time. *)
        List.iter (fun (tid, map) ->
          match Hashtbl.find_opt trees tid with
          | None -> ()   (* tree was added after snapshot; skip *)
          | Some r -> r := map
        ) snap;
        (* Remove trees that were created during this txn (tid not in snap). *)
        let snap_tids = List.map fst snap in
        Hashtbl.iter (fun tid _ ->
          if not (List.mem tid snap_tids) then
            Hashtbl.remove trees tid
        ) (Hashtbl.copy trees);
        t.mem_rw_snapshot <- None;
        t.mem_savepoints <- [])
   | Btree st ->
     (* Drop the per-tree cache so subsequent reads pick up the
        last-committed roots from the meta-tree.  Note: the meta-tree
        itself may have been mutated during this txn (uncommitted puts
        to it); we revert it to the last-committed root from the
        header. *)
     Hashtbl.clear st.trees;
     st.meta <- Btree.create st.pager ~root_page:st.current_header.root_page;
     (match st.txn_freelist_snapshot with
      | Some fl ->
        Pager.set_freelist st.pager fl;
        Pager.clear_dirty st.pager;
        st.txn_freelist_snapshot <- None
      | None -> ());
     st.bt_savepoints <- []);
  Rwlock.release_write t.lock;
  Lwt.return_unit

(* ------------------------------------------------------------------ *)
(* WAL checkpoint                                                       *)
(* ------------------------------------------------------------------ *)

(** Migrate every page currently in the WAL index to the main DB, sync
    the main DB, then reset the WAL. Holds the RW mutex so no
    concurrent commit can append fresh frames while we read the index.
    On Mem stores or non-WAL Btree stores this is a no-op. *)
let checkpoint (t : t) : unit Lwt.t =
  match t.backend with
  | Mem _ -> Lwt.return_unit
  | Btree st ->
    (match st.wal with
     | None -> Lwt.return_unit
     | Some wal ->
       let* () = Rwlock.acquire_write t.lock in
       Lwt.finalize
         (fun () -> checkpoint_unlocked st wal)
         (fun () -> Rwlock.release_write t.lock; Lwt.return_unit))

let wal_autocheckpoint (t : t) : int =
  match t.backend with
  | Mem _ -> 0
  | Btree st -> st.wal_autocheckpoint_threshold

let set_wal_autocheckpoint (t : t) (n : int) : unit =
  match t.backend with
  | Mem _ -> ()
  | Btree st -> st.wal_autocheckpoint_threshold <- max 0 n

(* Number of fsyncs the WAL has performed since open.  Exposed for #77
   group-commit testing: lets the test assert that N concurrent
   autocommit fibers issue ≪ N fsyncs (proof of coalescing). *)
let wal_sync_count (t : t) : int =
  match t.backend with
  | Mem _ -> 0
  | Btree st ->
    (match st.wal with
     | None -> 0
     | Some w -> Sqlocaml_storage.Wal.sync_count w)

(* Diagnostic/testing accessors for #164: observe that ending a snapshot
   releases its bookkeeping even when its reader closure raised. *)
let active_reader_count (t : t) : int =
  match t.backend with
  | Mem _ -> 0
  | Btree st -> Hashtbl.fold (fun _ c acc -> acc + c) st.active_readers 0

let pinned_page_count (t : t) : int =
  match t.backend with
  | Mem _ -> 0
  | Btree st -> Pager.pinned_count st.pager

let live_read_locks (t : t) : int = Rwlock.readers t.lock

(* ------------------------------------------------------------------ *)
(* Savepoints (Mem backend only; B-tree deferred)                      *)
(* ------------------------------------------------------------------ *)

(** Push a named savepoint: snapshot current state. *)
let savepoint_begin (Rw t : rw txn) name =
  match t.backend with
  | Mem trees ->
    let snap = Hashtbl.fold (fun tid r acc -> (tid, !r) :: acc) trees [] in
    t.mem_savepoints <- (name, snap) :: t.mem_savepoints;
    Lwt.return_unit
  | Btree st ->
    let tree_roots =
      Hashtbl.fold (fun tid bt acc -> (tid, Btree.root_page bt) :: acc)
        st.trees []
    in
    let sp = {
      sp_name       = name;
      sp_meta_root  = Btree.root_page st.meta;
      sp_tree_roots = tree_roots;
      sp_freelist   = Pager.freelist st.pager;
      sp_n_pages    = Pager.n_pages st.pager;
      sp_dirty      = Pager.dirty_clone st.pager;
    } in
    st.bt_savepoints <- sp :: st.bt_savepoints;
    Lwt.return_unit

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
  | Btree st ->
    let rec drop = function
      | [] -> []
      | sp :: rest when String.equal sp.sp_name name -> rest
      | _ :: rest -> drop rest
    in
    st.bt_savepoints <- drop st.bt_savepoints;
    Lwt.return_unit

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
  | Btree st ->
    let rec find = function
      | [] -> ()
      | sp :: rest when String.equal sp.sp_name name ->
        (* Restore meta-tree root *)
        st.meta <- Btree.create st.pager ~root_page:sp.sp_meta_root;
        (* Restore per-tree roots: drop the cache, re-populate from snapshot. *)
        Hashtbl.clear st.trees;
        List.iter (fun (tid, root) ->
          let bt = Btree.create st.pager ~root_page:root in
          Hashtbl.replace st.trees tid bt
        ) sp.sp_tree_roots;
        (* Restore freelist + n_pages + dirty set. Pages above sp.sp_n_pages
           that were freshly allocated in the rolled-back range become
           orphans in the file but are not in any tree, freelist, or
           dirty set — harmless storage leak. *)
        Pager.set_freelist st.pager sp.sp_freelist;
        Pager.set_n_pages  st.pager sp.sp_n_pages;
        Pager.dirty_restore st.pager sp.sp_dirty;
        (* Keep the named savepoint at the top so it can be re-used. *)
        st.bt_savepoints <- sp :: rest
      | _ :: rest -> find rest
    in
    find st.bt_savepoints;
    Lwt.return_unit

(* ------------------------------------------------------------------ *)
(* get / put / del                                                      *)
(* ------------------------------------------------------------------ *)

let store_of : type a. a txn -> t = function
  | Ro snap -> snap.rs_store
  | Rw s    -> s

let get : type a. a txn -> tree_id -> bytes -> bytes option Lwt.t =
  fun tx tid key ->
    match tx with
    | Ro snap ->
      (match snap.rs_store.backend with
       | Mem trees ->
         Lwt.return (Bytes_map.find_opt key !(mem_tree trees tid))
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
         Lwt.return (Bytes_map.find_opt key !(mem_tree trees tid))
       | Btree st ->
         let* r = bt_get_tree st tid in
         let* bt = unwrap_error r in
         let* g = Btree.get bt key in
         (match g with
          | Ok v -> Lwt.return v
          | Error e ->
            Lwt.fail_with
              (Format.asprintf "Store.get(rw): %a" pp_error (map_btree_err e))))

let put (Rw t : rw txn) tid key value : unit Lwt.t =
  match t.backend with
  | Mem trees ->
    let r = mem_tree trees tid in
    r := Bytes_map.add key value !r;
    Lwt.return_unit
  | Btree st ->
    let* r = bt_get_tree st tid in
    let* bt = unwrap_error r in
    let* p = Btree.put bt key value in
    (match p with
     | Ok bt' ->
       Hashtbl.replace st.trees tid bt';
       Lwt.return_unit
     | Error e ->
       Lwt.fail_with
         (Format.asprintf "Store.put: %a" pp_error (map_btree_err e)))

let del (Rw t : rw txn) tid key : unit Lwt.t =
  match t.backend with
  | Mem trees ->
    let r = mem_tree trees tid in
    r := Bytes_map.remove key !r;
    Lwt.return_unit
  | Btree st ->
    let* r = bt_get_tree st tid in
    let* bt = unwrap_error r in
    let* d = Btree.del bt key in
    (match d with
     | Ok bt' ->
       Hashtbl.replace st.trees tid bt';
       Lwt.return_unit
     | Error e ->
       Lwt.fail_with
         (Format.asprintf "Store.del: %a" pp_error (map_btree_err e)))

(* ------------------------------------------------------------------ *)
(* Cursors                                                              *)
(* ------------------------------------------------------------------ *)

(* Drain a B+-tree cursor into an in-memory snapshot list.  Phase 1
   cursors are materialised; streaming cursors arrive later. *)
let drain_btree_cursor (c : Btree.cursor) : (bytes * bytes) list Lwt.t =
  let rec loop acc =
    let* r = Btree.cursor_next c in
    match r with
    | Error e ->
      Lwt.fail_with
        (Format.asprintf "Store.cursor: %a" pp_error (map_btree_err e))
    | Ok None -> Lwt.return (List.rev acc)
    | Ok (Some kv) -> loop (kv :: acc)
  in
  loop []

let cursor_open : type a. a txn -> tree_id -> cursor Lwt.t =
  fun tx tid ->
    match tx with
    | Ro snap ->
      (match snap.rs_store.backend with
       | Mem trees ->
         let entries = Bytes_map.bindings !(mem_tree trees tid) in
         Lwt.return { all = entries; remaining = []; ready = false }
       | Btree st ->
         let* r = bt_get_tree_ro snap st tid in
         let* bt = unwrap_error r in
         let* co = Btree.cursor_open bt in
         (match co with
          | Error e ->
            Lwt.fail_with
              (Format.asprintf "Store.cursor_open(ro): %a"
                 pp_error (map_btree_err e))
          | Ok c ->
            let* entries = drain_btree_cursor c in
            Btree.cursor_close c;
            Lwt.return { all = entries; remaining = []; ready = false }))
    | Rw _ ->
      let t = store_of tx in
      (match t.backend with
       | Mem trees ->
         let entries = Bytes_map.bindings !(mem_tree trees tid) in
         Lwt.return { all = entries; remaining = []; ready = false }
       | Btree st ->
         let* r = bt_get_tree st tid in
         let* bt = unwrap_error r in
         let* co = Btree.cursor_open bt in
         (match co with
          | Error e ->
            Lwt.fail_with
              (Format.asprintf "Store.cursor_open: %a" pp_error (map_btree_err e))
          | Ok c ->
            let* entries = drain_btree_cursor c in
            Btree.cursor_close c;
            Lwt.return { all = entries; remaining = []; ready = false }))

let cursor_close _ = ()

let cursor_first c =
  c.remaining <- c.all;
  match c.all with
  | [] ->
    c.ready <- false;
    Not_found `End
  | (k, _) :: _ ->
    c.ready <- true;
    Found k

let cursor_seek c key =
  let rec find = function
    | [] ->
      c.remaining <- [];
      c.ready <- false;
      Not_found `End
    | ((k, _) :: _ as cur) ->
      let cmp = Bytes.compare k key in
      if cmp >= 0 then begin
        c.remaining <- cur;
        c.ready <- true;
        if cmp = 0 then Found k else Not_found (`Greater k)
      end else
        find (List.tl cur)
  in
  find c.all

let cursor_next c =
  match c.remaining with
  | [] -> None
  | entry :: rest ->
    if c.ready then begin
      c.ready <- false;
      Some entry
    end else begin
      c.remaining <- rest;
      match rest with
      | [] -> None
      | next :: _ -> Some next
    end

let cursor_value c =
  match c.remaining with
  | (_, v) :: _ when c.ready -> Some v
  | _ -> None

let wal_mode t =
  match t.backend with
  | Mem _ -> false
  | Btree st -> st.wal <> None

let freelist_size t =
  match t.backend with
  | Mem _ -> 0
  | Btree st -> Freelist.size (Pager.freelist st.pager)

let freelist_entries t =
  match t.backend with
  | Mem _ -> []
  | Btree st -> Freelist.to_list (Pager.freelist st.pager)

let n_pages t =
  match t.backend with
  | Mem _ -> 0L
  | Btree st -> Pager.n_pages st.pager

(* Enumerate all tree_ids known to the meta tree.  For VACUUM. *)
let list_tree_ids t : tree_id list Lwt.t =
  match t.backend with
  | Mem trees ->
    Lwt.return (Hashtbl.fold (fun tid _ acc -> tid :: acc) trees [])
  | Btree st ->
    let* r = Btree.cursor_open st.meta in
    match r with
    | Error e ->
      Lwt.fail_with
        (Format.asprintf "Store.list_tree_ids: %a" pp_error (map_btree_err e))
    | Ok cur ->
      let rec loop acc =
        let* r = Btree.cursor_next cur in
        match r with
        | Error e ->
          Lwt.fail_with
            (Format.asprintf "Store.list_tree_ids: %a" pp_error (map_btree_err e))
        | Ok None -> Lwt.return (List.rev acc)
        | Ok (Some (k, _v)) ->
          let tid, _ = Varint.decode_int64 k 0 in
          loop (Int64.to_int tid :: acc)
      in
      let* result = loop [] in
      Btree.cursor_close cur;
      Lwt.return result

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
