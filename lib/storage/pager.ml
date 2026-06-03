(** Pager: page cache + allocator over a BLOCK backend.

    Maintains:
    - A bounded FIFO cache of pages (read from BLOCK).  Capacity defaults to
      [default_cache_capacity] and is overridable via [SQLOCAML_PAGE_CACHE].
    - A dirty table of pages modified since the last flush.
    - A pin table (#159): pages referenced by a live RO snapshot are pinned
      so the writer's CoW churn can't FIFO out a reader's working set.
    - An in-memory freelist for page allocation.

    Dirty and pinned pages are never evicted from the cache; dirty pages are
    written to BLOCK only on [flush]. *)

(* Default if [SQLOCAML_PAGE_CACHE] is unset/invalid.  Bumped from the
   original 64 (#159): a bigger cache lets a reader's working set and a
   writer's CoW churn coexist without immediate eviction pressure. *)
let default_cache_capacity = 1024

let cache_capacity_from_env () =
  match Sys.getenv_opt "SQLOCAML_PAGE_CACHE" with
  | Some s ->
    (match int_of_string_opt s with
     | Some n when n > 0 -> n
     | _ -> default_cache_capacity)
  | None -> default_cache_capacity
;;

type cache_key = int64 * int (* (page_id, version);  -1 = main DB *)

let cache_key_main pid : cache_key = pid, -1

type wal_callbacks =
  { wal_find_page : int64 -> int option
  ; wal_find_page_at : int64 -> max_frame:int -> int option
  ; wal_read_frame : int -> (Cstruct.t, string) result Lwt.t
  ; wal_append_commit : (int64 * Cstruct.t) list -> (unit, string) result Lwt.t
  ; wal_append_commit_no_sync : (int64 * Cstruct.t) list -> (unit, string) result Lwt.t
  ; wal_sync : unit -> (unit, string) result Lwt.t
  }

type t =
  { read_page : page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t
  ; write_page : page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t
  ; sync : unit -> (unit, string) result Lwt.t
  ; resize : n_pages:int64 -> (unit, string) result Lwt.t
  ; mutable geom : Geometry.t
    (** Page geometry for this file (#95): buffers are allocated at
        [geom.page_size]; [btree]/[store] read [max_data_bytes] etc. from here.
        Defaults to {!Geometry.default}; the open path calls {!set_geom} once
        with the file's real geometry before any page op, after which it is
        effectively immutable. *)
  ; cache : (cache_key, Cstruct.t) Hashtbl.t
  ; dirty : (int64, Cstruct.t) Hashtbl.t
  ; fifo : cache_key Queue.t (* insertion order for FIFO eviction *)
  ; cache_capacity : int
  ; pinned : (cache_key, int) Hashtbl.t
  ; (* Refcount per cache key of live RO snapshots that have materialised it
     (#159).  [maybe_evict] never drops a key with refcount > 0.  Multiple
     concurrent snapshots referencing the same page share the count. *)
    mutable n_pages : int64
  ; mutable freelist : Freelist.t
  ; mutable current_txn_id : int64
  ; mutable alloc_min_safe : int64
  ; mutable wal : wal_callbacks option
  ; mutable write_tag : int32
    (** #174: schema-fingerprint stamp to write into the reserved header bytes
        of the next Branch/Leaf page built.  Set per tree-operation by the
        store; 0 for system/untagged trees.  Safe as shared state because
        writes are serialised under the single RW transaction. *)
  }

type error =
  | Block_error of string
  | Corruption of string

let pp_error fmt = function
  | Block_error msg -> Format.fprintf fmt "Block_error: %s" msg
  | Corruption msg -> Format.fprintf fmt "Corruption: %s" msg
;;

let pp fmt t =
  Format.fprintf
    fmt
    "@[<hv>Pager.t { n_pages = %Ld;@ cached = %d;@ dirty = %d;@ txn_id = %Ld }@]"
    t.n_pages
    (Hashtbl.length t.cache)
    (Hashtbl.length t.dirty)
    t.current_txn_id
;;

let create ~read_page ~write_page ~sync ~resize ~n_pages ~freelist =
  { read_page
  ; write_page
  ; sync
  ; resize
  ; geom = Geometry.default
  ; cache = Hashtbl.create 64
  ; dirty = Hashtbl.create 16
  ; fifo = Queue.create ()
  ; cache_capacity = cache_capacity_from_env ()
  ; pinned = Hashtbl.create 16
  ; n_pages
  ; freelist
  ; current_txn_id = 0L
  ; alloc_min_safe = 0L
  ; wal = None
  ; write_tag = 0l
  }
;;

(* #174: set the schema-fingerprint stamp for subsequently-built Branch/Leaf
   pages.  Reset to 0 before writing system-tree (e.g. meta) pages. *)
let set_write_tag t (tag : int32) = t.write_tag <- tag
let write_tag t = t.write_tag

(* #95: set the file's page geometry.  Called once by the open path before any
   page read/write, after peeking/deciding the geometry. *)
let set_geom t geom = t.geom <- geom

(* #95: page geometry accessors (cheap field reads on the hot path). *)
let geom t = t.geom
let page_size t = t.geom.page_size
let reserved_bytes t = t.geom.reserved_bytes_per_page
let max_data_bytes t = Geometry.max_data_bytes t.geom
let max_overflow_payload_bytes t = Geometry.max_overflow_payload_bytes t.geom
let max_freelist_entries_per_page t = Geometry.max_freelist_entries_per_page t.geom

let set_wal t cb =
  (* Any cache entries built before the WAL hook was attached came from
     the main DB only. If a WAL frame exists for those pages it is more
     recent — so clear the cache when transitioning into WAL mode so a
     subsequent [read] re-resolves through the WAL index. *)
  (match cb, t.wal with
   | Some _, None ->
     Hashtbl.reset t.cache;
     Queue.clear t.fifo
   | _ -> ());
  t.wal <- cb
;;

let wal_mode t = t.wal <> None

(** Evict the oldest cache entry if the cache is at capacity.
    Never evicts dirty or pinned (#159) pages. *)
let maybe_evict t =
  (* Keep trying to evict until we find a clean page or the cache is small enough *)
  let cache_size = Hashtbl.length t.cache in
  if cache_size < t.cache_capacity
  then ()
  else (
    (* Scan the FIFO queue front-to-back looking for an evictable page
       (neither dirty nor pinned). *)
    let evicted = ref false in
    let temp = Queue.create () in
    while (not !evicted) && not (Queue.is_empty t.fifo) do
      let key = Queue.pop t.fifo in
      if Hashtbl.mem t.dirty (fst key) || Hashtbl.mem t.pinned key
      then
        (* dirty or pinned — put back at end so we don't lose track of it *)
        Queue.push key temp
      else (
        Hashtbl.remove t.cache key;
        evicted := true;
        (* push anything we moved to temp back into the real queue *)
        Queue.iter (fun k -> Queue.push k t.fifo) temp;
        Queue.clear temp)
    done;
    (* If we couldn't evict (all cached pages are dirty/pinned), keep them. *)
    if not !evicted then Queue.iter (fun k -> Queue.push k t.fifo) temp)
;;

(* Largest number of distinct pages a set of live snapshots may pin.  We
   always keep a reserve of evictable slots so [maybe_evict] can make
   progress and the cache stays bounded even under a giant scan. *)
let max_pinned t = t.cache_capacity - max 8 (t.cache_capacity / 8)

(* Pin [page_id] for the snapshot whose pin set is [s], if budget allows.
   Idempotent per snapshot: a page already in [s] is not double-counted.
   When the pin budget is exhausted the page is simply left unpinned (it is
   still cached normally and may be evicted). *)
let pin_page t pin_set page_id =
  match pin_set with
  | None -> ()
  | Some s ->
    if (not (Hashtbl.mem s page_id)) && Hashtbl.length t.pinned < max_pinned t
    then (
      Hashtbl.replace s page_id ();
      let key = cache_key_main page_id in
      let c = Option.value ~default:0 (Hashtbl.find_opt t.pinned key) in
      Hashtbl.replace t.pinned key (c + 1))
;;

(** Release every pin held by a snapshot (called from [Store.ro_end]).
    Decrements the shared refcount for each page the snapshot pinned. *)
let unpin_all t pin_set =
  Hashtbl.iter
    (fun page_id () ->
       let key = cache_key_main page_id in
       match Hashtbl.find_opt t.pinned key with
       | None | Some 1 -> Hashtbl.remove t.pinned key
       | Some n -> Hashtbl.replace t.pinned key (n - 1))
    pin_set
;;

(** Add a page to the cache, evicting if necessary. *)
let cache_add t key buf =
  let already_cached = Hashtbl.mem t.cache key in
  maybe_evict t;
  Hashtbl.replace t.cache key buf;
  if not already_cached then Queue.push key t.fifo
;;

(** Make a deep copy of a Cstruct. *)
let cstruct_dup src =
  let len = Cstruct.length src in
  let dst = Cstruct.create len in
  Cstruct.blit src 0 dst 0 len;
  dst
;;

(* Resolve [page_id] from the WAL, if any.  [finder] picks the relevant frame
   (latest, or latest <= a snapshot bound).  WAL frames are NOT cached: frame
   indices are recycled after a WAL reset (checkpoint), so a cached
   (page_id, frame_idx) entry could be served stale.  Returns a fresh Cstruct. *)
let resolve_wal_page t finder =
  let open Lwt.Syntax in
  match t.wal with
  | None -> Lwt.return_ok None
  | Some cb ->
    (match finder cb with
     | None -> Lwt.return_ok None
     | Some frame_idx ->
       let* r = cb.wal_read_frame frame_idx in
       (match r with
        | Error s -> Lwt.return_error (Block_error s)
        | Ok page -> Lwt.return_ok (Some (cstruct_dup page))))
;;

(* Load [page_id] from the shared cache, or from the block device on a miss
   (caching the result).  The returned Cstruct is fresh. *)
let load_main_page t pin_set page_id =
  let open Lwt.Syntax in
  let key = cache_key_main page_id in
  match Hashtbl.find_opt t.cache key with
  | Some buf ->
    pin_page t pin_set page_id;
    Lwt.return_ok (cstruct_dup buf)
  | None ->
    let buf = Cstruct.create t.geom.page_size in
    let* result = t.read_page ~page_id buf in
    (match result with
     | Error msg -> Lwt.return_error (Block_error msg)
     | Ok () ->
       cache_add t key (cstruct_dup buf);
       pin_page t pin_set page_id;
       Lwt.return_ok buf)
;;

let read ?snapshot_frames ?pin_set t page_id =
  let open Lwt.Syntax in
  let load_after_wal finder =
    let* wal_r = resolve_wal_page t finder in
    match wal_r with
    | Error e -> Lwt.return_error e
    | Ok (Some page) -> Lwt.return_ok page
    | Ok None -> load_main_page t pin_set page_id
  in
  match snapshot_frames with
  | None ->
    (* Writer / no-snapshot path: dirty wins. *)
    (match Hashtbl.find_opt t.dirty page_id with
     | Some buf -> Lwt.return_ok (cstruct_dup buf)
     | None -> load_after_wal (fun cb -> cb.wal_find_page page_id))
  | Some max_frame ->
    (* Snapshot reader path: never consult [dirty]. *)
    load_after_wal (fun cb -> cb.wal_find_page_at page_id ~max_frame)
;;

let write t page_id buf =
  let copy = cstruct_dup buf in
  Hashtbl.replace t.dirty page_id copy
;;

(* #231: like [write], but takes OWNERSHIP of [buf] — no defensive copy.  The
   caller must never mutate [buf] after this call.  Used by the B+-tree
   build-and-write helpers, which create a fresh page buffer per write and drop
   it immediately; the [write]-path [cstruct_dup] was a pure ~4KB alloc+memcpy
   per page written (one per tree level per insert).  Reads still hand out
   copies of dirty pages, so stored buffers are never aliased to readers. *)
let write_owned t page_id buf = Hashtbl.replace t.dirty page_id buf

(* Previously also injected into the shared cache here for
     read-after-write inside the same txn.  Removed (#149): a concurrent
     reader at an older snapshot would see uncommitted bytes.  The
     [dirty] table already covers writer read-after-write — [read]
     consults [dirty] first on the no-snapshot path. *)

let alloc t =
  match Freelist.pop t.freelist ~min_safe_txn_id:t.alloc_min_safe with
  | Some (pid32, fl') ->
    t.freelist <- fl';
    Lwt.return_ok (Int64.of_int32 pid32)
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
       Lwt.return_ok new_id)
;;

let free t ~page_id ~freed_at_txn_id =
  t.freelist <- Freelist.add t.freelist ~page_id:(Int64.to_int32 page_id) ~freed_at_txn_id
;;

(* Internal: drive the WAL append callback [append] with the dirty
   entries; on success clear the dirty set.  Used by both [flush] (sync)
   and [flush_no_sync] (group commit) so the dirty-set management is
   identical. *)
let flush_via_wal t ~append =
  let open Lwt.Syntax in
  let entries = Hashtbl.fold (fun pid buf acc -> (pid, buf) :: acc) t.dirty [] in
  if entries = []
  then Lwt.return_ok ()
  else
    let* r = append entries in
    match r with
    | Error msg -> Lwt.return_error (Block_error msg)
    | Ok () ->
      Hashtbl.clear t.dirty;
      Lwt.return_ok ()
;;

let flush_no_sync t =
  match t.wal with
  | Some cb -> flush_via_wal t ~append:cb.wal_append_commit_no_sync
  | None ->
    (* Non-WAL backends have no notion of deferred sync — fall through to
       the regular [flush] which writes pages + syncs. *)
    let entries = Hashtbl.fold (fun pid buf acc -> (pid, buf) :: acc) t.dirty [] in
    let open Lwt.Syntax in
    let rec write_all = function
      | [] ->
        let* sync_result = t.sync () in
        (match sync_result with
         | Error msg -> Lwt.return_error (Block_error msg)
         | Ok () ->
           Hashtbl.clear t.dirty;
           Lwt.return_ok ())
      | (pid, buf) :: rest ->
        let* result = t.write_page ~page_id:pid buf in
        (match result with
         | Error msg -> Lwt.return_error (Block_error msg)
         | Ok () ->
           (* Update main-key cache so post-flush reads don't serve stale data.
              [write] no longer injects into the shared cache (#149), so we must
              update here after the block write is committed. *)
           cache_add t (cache_key_main pid) (cstruct_dup buf);
           write_all rest)
    in
    write_all entries
;;

let wal_sync t =
  match t.wal with
  | Some cb ->
    let open Lwt.Syntax in
    let* r = cb.wal_sync () in
    (match r with
     | Error msg -> Lwt.return_error (Block_error msg)
     | Ok () -> Lwt.return_ok ())
  | None -> Lwt.return_ok ()
;;

let flush t =
  let open Lwt.Syntax in
  let entries = Hashtbl.fold (fun pid buf acc -> (pid, buf) :: acc) t.dirty [] in
  match t.wal with
  | Some cb ->
    if entries = []
    then Lwt.return_ok ()
    else
      let* r = cb.wal_append_commit entries in
      (match r with
       | Error msg -> Lwt.return_error (Block_error msg)
       | Ok () ->
         Hashtbl.clear t.dirty;
         Lwt.return_ok ())
  | None ->
    (* Legacy path: write every dirty page to the main DB and sync. *)
    let rec write_all = function
      | [] ->
        let* sync_result = t.sync () in
        (match sync_result with
         | Error msg -> Lwt.return_error (Block_error msg)
         | Ok () ->
           Hashtbl.clear t.dirty;
           Lwt.return_ok ())
      | (pid, buf) :: rest ->
        let* result = t.write_page ~page_id:pid buf in
        (match result with
         | Error msg -> Lwt.return_error (Block_error msg)
         | Ok () ->
           (* Update main-key cache so post-flush reads don't serve stale data.
              [write] no longer injects into the shared cache (#149), so we must
              update here after the block write is committed. *)
           cache_add t (cache_key_main pid) (cstruct_dup buf);
           write_all rest)
    in
    write_all entries
;;

let n_pages t = t.n_pages
let freelist t = t.freelist
let set_txn_id t id = t.current_txn_id <- id
let get_txn_id t = t.current_txn_id
let set_alloc_min_safe t v = t.alloc_min_safe <- v

(* Number of distinct pages currently pinned by live RO snapshots (#159).
   Exposed for #164 testing: lets a test assert pins return to 0 after a
   snapshot ends — including when its reader closure raised. *)
let pinned_count t = Hashtbl.length t.pinned
let set_freelist t fl = t.freelist <- fl
let set_n_pages t n = t.n_pages <- n

let clear_dirty t =
  let dirty_pids = Hashtbl.fold (fun pid _ acc -> pid :: acc) t.dirty [] in
  List.iter
    (fun pid ->
       Hashtbl.remove t.dirty pid;
       Hashtbl.remove t.cache (cache_key_main pid))
    dirty_pids;
  let old_fifo = Queue.copy t.fifo in
  Queue.clear t.fifo;
  Queue.iter (fun key -> if Hashtbl.mem t.cache key then Queue.push key t.fifo) old_fifo
;;

type dirty_snapshot = (int64, Cstruct.t) Hashtbl.t

let dirty_clone t = Hashtbl.copy t.dirty

let dirty_restore t snap =
  Hashtbl.reset t.dirty;
  Hashtbl.iter (fun k v -> Hashtbl.replace t.dirty k v) snap
;;

let flush_one_to_main t ~page_id ~buf =
  let open Lwt.Syntax in
  let* r = t.write_page ~page_id buf in
  match r with
  | Ok () ->
    cache_add t (cache_key_main page_id) (cstruct_dup buf);
    Lwt.return_ok ()
  | Error s -> Lwt.return_error (Block_error s)
;;

let flush_sync_main t =
  let open Lwt.Syntax in
  let* r = t.sync () in
  match r with
  | Ok () -> Lwt.return_ok ()
  | Error s -> Lwt.return_error (Block_error s)
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
