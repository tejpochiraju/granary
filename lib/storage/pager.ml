(** Pager: page cache + allocator over a BLOCK backend.

    Maintains:
    - A bounded FIFO cache of pages (read from BLOCK, or — since #611 —
      resolved from a WAL frame).  Capacity defaults to
      [default_cache_capacity] and is overridable via [GRANARY_PAGE_CACHE].
      Entries are keyed by [cache_key = (page_id, version)]: [-1] for the main
      file, the WAL frame index otherwise.  See the comment on [main_version]
      for how the WAL-resolved entries are invalidated.
    - A dirty table of pages modified since the last flush.
    - A pin table (#159): pages referenced by a live RO snapshot are pinned
      so the writer's CoW churn can't FIFO out a reader's working set.
    - An in-memory freelist for page allocation.

    Dirty and pinned pages are never evicted from the cache; dirty pages are
    written to BLOCK only on [flush]. *)

(* Default if [GRANARY_PAGE_CACHE] is unset/invalid.  Bumped from the
   original 64 (#159): a bigger cache lets a reader's working set and a
   writer's CoW churn coexist without immediate eviction pressure. *)
let default_cache_capacity = 1024

let cache_capacity_from_env () =
  match Sys.getenv_opt "GRANARY_PAGE_CACHE" with
  | Some s ->
    (match int_of_string_opt s with
     | Some n when n > 0 -> n
     | _ -> default_cache_capacity)
  | None -> default_cache_capacity
;;

type cache_key = int64 * int (* (page_id, version);  -1 = main DB *)

(* #611: the version slot of a [cache_key].  [main_version] tags a page whose
   bytes came from the main file; any value >= 0 is the WAL FRAME INDEX the
   page was resolved from.  Keying WAL-resolved pages by frame index — rather
   than by page id alone — is what makes the three invalidation triggers fall
   out of the key instead of needing a notification:

   (a) a NEWER frame for the same page lands at a different index, so the next
       resolution builds a different key and cannot hit the older entry;
   (b) an OLDER-snapshot reader ([wal_find_page_at], the #266 as-of path)
       resolves to the frame its snapshot bound allows and looks that frame up
       by index, so it can never be served the newest one;
   (c) a checkpoint ([Wal.reset]) RECYCLES frame indices, which the key alone
       cannot distinguish — that one is handled by [sync_wal_epoch] below. *)
let main_version = -1
let cache_key_main pid : cache_key = pid, main_version
let cache_key_wal pid frame_idx : cache_key = pid, frame_idx
let is_wal_key ((_, v) : cache_key) = v >= 0

type wal_callbacks =
  { wal_find_page : int64 -> int option
  ; wal_find_page_at : int64 -> max_frame:int -> int option
  ; wal_read_frame : int -> (Cstruct.t, string) result Lwt.t
  ; wal_append_commit : (int64 * Cstruct.t) list -> (unit, string) result Lwt.t
  ; wal_append_commit_no_sync : (int64 * Cstruct.t) list -> (unit, string) result Lwt.t
  ; wal_sync : unit -> (unit, string) result Lwt.t
  ; wal_epoch : unit -> int64
    (** #611: the WAL's generation counter ([Wal.epoch]).  It is bumped by
        exactly the operation that recycles frame indices — [Wal.reset] bumps
        it on both of its arms and on neither of its failure paths, and nothing
        else in [Wal] ever clears the index — so comparing it on every WAL
        resolution is a complete guard against serving a frame from a dead
        generation.  Deliberately a callback rather than a notification from
        the checkpoint sites: the pager then cannot be left stale by a
        [Wal.reset] call site nobody remembered to hook up (there are three —
        [Store.checkpoint], [Replication], [Standby.promote]). *)
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
  ; mutable n_pages_at_rw_begin : int64
    (** #297: page count captured at rw_begin; pages with id >= this
        threshold were allocated by file extension in the current txn
        and can be safely freed+reused within the same txn without
        affecting snapshot readers or in-flight cursors. *)
  ; mutable txn_owned_pool : int64 list
    (** #297: pool of page-ids that were allocated above
        [n_pages_at_rw_begin] during the current txn and have since
        been freed.  [alloc] consults this pool before the main
        freelist. *)
  ; mutable wal : wal_callbacks option
  ; mutable wal_epoch : int64
    (** #611: the WAL generation every WAL-keyed entry currently in [cache] was
        built under.  [sync_wal_epoch] compares it against the live
        [wal_epoch ()] on every WAL resolution and purges the WAL-keyed
        entries wholesale when they differ. *)
  ; mutable write_tag : int32
    (** #174: schema-fingerprint stamp to write into the reserved header bytes
        of the next Branch/Leaf page built.  Set per tree-operation by the
        store; 0 for system/untagged trees.  Safe as shared state because
        writes are serialised under the single RW transaction. *)
  ; mutable on_page_event : (Pager_event.t -> unit) option
    (** #384: optional, synchronous, fire-and-forget observer for physical page
        I/O (internals monitor).  [None] = zero overhead: the per-kind [emit_*]
        helpers construct the [Pager_event.t] only inside the [Some] branch, so
        the [None] path neither allocates nor invokes anything.
        [Store.set_event_callback] installs a translator here. *)
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
    "@[<hv>Pager.t { n_pages = %Ld;@ cached = %d;@ dirty = %d;@ txn_id = %Ld;@ txn_pool \
     = %d }@]"
    t.n_pages
    (Hashtbl.length t.cache)
    (Hashtbl.length t.dirty)
    t.current_txn_id
    (List.length t.txn_owned_pool)
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
  ; n_pages_at_rw_begin = 0L
  ; txn_owned_pool = []
  ; wal = None
  ; wal_epoch = 0L
  ; write_tag = 0l
  ; on_page_event = None
  }
;;

(* #174: set the schema-fingerprint stamp for subsequently-built Branch/Leaf
   pages.  Reset to 0 before writing system-tree (e.g. meta) pages. *)
let set_write_tag t (tag : int32) = t.write_tag <- tag
let write_tag t = t.write_tag
let set_page_event_callback t cb = t.on_page_event <- cb

(* #384: emit a [Page_read] iff an observer is attached.  The event record is
   constructed only inside the [Some] branch, so the [None] path allocates
   nothing — keeping physical-read instrumentation truly zero-overhead when the
   internals monitor is off. *)
let emit_read t page_id =
  match t.on_page_event with
  | None -> ()
  | Some f -> f (Pager_event.Page_read { page_id })
;;

(* #392: emit a [Wal_read] iff an observer is attached — the WAL-overlay
   counterpart of [emit_read].  Fires on a pager cache miss resolved from a WAL
   frame (a backend resolution from the pager's viewpoint), with the same
   zero-alloc guard as the other per-kind emit helpers. *)
let emit_wal_read t page_id =
  match t.on_page_event with
  | None -> ()
  | Some f -> f (Pager_event.Wal_read { page_id })
;;

(* #384: emit a [Page_alloc]/[Page_free] iff an observer is attached; the record
   is built only inside the [Some] branch (zero-alloc when the monitor is off). *)
let emit_alloc t page_id reused =
  match t.on_page_event with
  | None -> ()
  | Some f -> f (Pager_event.Page_alloc { page_id; reused })
;;

(* #384: same zero-alloc guard as emit_alloc. *)
let emit_free t page_id =
  match t.on_page_event with
  | None -> ()
  | Some f -> f (Pager_event.Page_free { page_id })
;;

(* #384: emit one [Page_write] per dirty entry being flushed.  Guard once, then
   iterate — no allocation when no observer is attached (same zero-alloc
   discipline as the per-kind emit helpers). *)
let emit_writes t entries =
  match t.on_page_event with
  | None -> ()
  | Some f ->
    List.iter (fun (pid, _) -> f (Pager_event.Page_write { page_id = pid })) entries
;;

(* #384: emit a single [Page_write] iff an observer is attached (zero-alloc on
   the [None] path, like the other per-kind emit helpers). *)
let emit_write t page_id =
  match t.on_page_event with
  | None -> ()
  | Some f -> f (Pager_event.Page_write { page_id })
;;

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

(* #611: drop every WAL-keyed cache entry, keeping the main-file ones.  The
   FIFO is rebuilt rather than filtered in place so it never accumulates keys
   that are no longer in [cache] (a stale FIFO entry would let [cache_add] push
   the same key twice on re-insert, and [maybe_evict] would waste a pass on
   it).  Main-file entries survive: their bytes came from the main DB, which a
   checkpoint only ever brings FORWARD to what the WAL already said. *)
let purge_wal_cache t =
  let stale =
    Hashtbl.fold (fun k _ acc -> if is_wal_key k then k :: acc else acc) t.cache []
  in
  if stale <> []
  then (
    List.iter (fun k -> Hashtbl.remove t.cache k) stale;
    let old_fifo = Queue.copy t.fifo in
    Queue.clear t.fifo;
    Queue.iter (fun k -> if Hashtbl.mem t.cache k then Queue.push k t.fifo) old_fifo)
;;

(* #611: invalidation trigger (b) — a checkpoint recycled the frame indices, so
   every cached (page_id, frame_idx) entry now names a slot the new generation
   will overwrite with unrelated bytes.  Detected by comparing the WAL's own
   generation counter, which is bumped by exactly that operation.  This runs
   BEFORE the cache is consulted on every WAL resolution, so a stale entry
   cannot be served even once.  Cost on the hot path: one closure call and an
   [Int64] compare. *)
let sync_wal_epoch t cb =
  let e = cb.wal_epoch () in
  if not (Int64.equal e t.wal_epoch)
  then (
    purge_wal_cache t;
    t.wal_epoch <- e)
;;

let set_wal t cb =
  (* Any cache entries built before the WAL hook was attached came from
     the main DB only. If a WAL frame exists for those pages it is more
     recent — so clear the cache when transitioning into WAL mode so a
     subsequent [read] re-resolves through the WAL index.
     #611: detaching the overlay (or swapping in a different one) must drop the
     WAL-keyed entries too, or a page would keep being served from a frame
     index that no longer means anything. *)
  (match cb, t.wal with
   | Some _, (None | Some _) ->
     Hashtbl.reset t.cache;
     Queue.clear t.fifo
   | None, Some _ -> purge_wal_cache t
   | None, None -> ());
  t.wal <- cb;
  t.wal_epoch
  <- (match cb with
      | None -> 0L
      | Some c -> c.wal_epoch ())
;;

let wal_mode t = t.wal <> None

(* #611: number of WAL-resolved frames currently held in the page cache.
   Exposed for the invalidation tests, which must be able to assert that a
   checkpoint actually PURGED the entries rather than merely failing to hit
   them (a key-mismatch miss and a purge are indistinguishable from the
   answers alone). *)
let wal_cached_count t =
  Hashtbl.fold (fun k _ n -> if is_wal_key k then n + 1 else n) t.cache 0
;;

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
      (* #611: the dirty guard is MAIN-key only.  A WAL-keyed entry holds
         committed frame bytes; that the same page also happens to be dirty in
         the current write txn says nothing about it, and treating it as
         un-evictable would let a hot dirty page pin an unbounded number of
         its own superseded frames in the cache. *)
      if
        ((not (is_wal_key key)) && Hashtbl.mem t.dirty (fst key))
        || Hashtbl.mem t.pinned key
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
   (latest, or latest <= a snapshot bound).  Returns a fresh Cstruct.
   #392: emits [Wal_read page_id] on the frame-served path (the WAL-overlay
   counterpart of [emit_read] in [load_main_page]) — on a MISS only, matching
   [load_main_page]'s [emit_read], so the counter keeps meaning "a resolution
   that went to the backend".

   #611: WAL frames ARE cached now, keyed by (page_id, frame_idx).  Frame
   indices are recycled after a WAL reset (checkpoint), which used to be the
   reason not to cache at all; [sync_wal_epoch] closes that hole by purging on
   the generation bump that recycling implies.  The buffer stored is the one
   [wal_read_frame] returned, without a defensive copy: [Wal.read_frame]'s
   contract (#246) is that its result is immutable for the life of the
   generation and dropped wholesale on [reset], never mutated in place — the
   same invariant the main page cache relies on — and callers of THIS function
   are handed a [cstruct_dup], never the stored buffer.

   The cache-miss half lives in [read_and_cache_frame] just below, shared with
   [resolve_wal_page_borrow] so the caching rule exists in exactly one place. *)

(* #611: read frame [frame_idx], publish it under [key] (unless the caller
   asked to bypass the cache), and hand back the very buffer [wal_read_frame]
   returned.

   [expected_epoch] is the whole subtlety, and it is the SAME guard the layer
   below already carries — see [Wal.cache_frame]'s [~expected_epoch] and
   [read_committed_frame]'s deliberate refusal to prime the frame cache
   (reviews #209/#210).  [sync_wal_epoch] runs BEFORE the yield in
   [wal_read_frame]; a checkpoint that lands DURING that yield would otherwise
   let this [cache_add] install a dead generation's bytes AFTER a later fiber
   had already purged and repopulated the same [key], and — because
   [t.wal_epoch] would by then already be the new epoch — no later
   [sync_wal_epoch] would ever purge it again.  The result is a permanently
   poisoned entry and a silent wrong answer for every subsequent reader.  The
   window is real: [Store.checkpoint_unlocked]'s reader gate is
   [ro_readers_below] ([m < target]), so a reader at the WAL head is not gated
   and runs concurrently with [Wal.reset].

   Both conjuncts are checked deliberately.  [t.wal_epoch] cannot currently
   differ from [cb.wal_epoch ()] here without the first conjunct also failing
   (epochs are monotone and [t.wal_epoch] is only ever assigned from
   [cb.wal_epoch ()]), so the second is redundant today — but it is what keeps
   this correct if the pager's own epoch bookkeeping ever gains another writer,
   and it costs an [Int64] compare on a path that has just done device I/O.

   The frame's BYTES are still returned to the caller when the epoch moved.
   That is unchanged pre-#611 behaviour and matches [Wal.read_frame], which
   also returns what it read and merely declines to cache it: the resolving
   reader picked [frame_idx] under its own snapshot before the checkpoint, and
   deciding what a reader at the head owes a concurrent checkpoint is #555/#585
   territory, not this cache's. *)
let read_and_cache_frame ~bypass_cache t cb ~page_id ~frame_idx ~key =
  let open Lwt.Syntax in
  let expected_epoch = cb.wal_epoch () in
  let* r = cb.wal_read_frame frame_idx in
  match r with
  | Error s -> Lwt.return_error (Block_error s)
  | Ok page ->
    emit_wal_read t page_id;
    if
      (not bypass_cache)
      && Int64.equal (cb.wal_epoch ()) expected_epoch
      && Int64.equal t.wal_epoch expected_epoch
    then cache_add t key page;
    Lwt.return_ok page
;;

let resolve_wal_page ?(bypass_cache = false) t ~page_id finder =
  match t.wal with
  | None -> Lwt.return_ok None
  | Some cb ->
    sync_wal_epoch t cb;
    (match finder cb with
     | None -> Lwt.return_ok None
     | Some frame_idx ->
       let key = cache_key_wal page_id frame_idx in
       (match Hashtbl.find_opt t.cache key with
        | Some buf -> Lwt.return_ok (Some (cstruct_dup buf))
        | None ->
          Lwt.map
            (Result.map (fun page -> Some (cstruct_dup page)))
            (read_and_cache_frame ~bypass_cache t cb ~page_id ~frame_idx ~key)))
;;

(* Load [page_id] from the shared cache, or from the block device on a miss
   (caching the result).  The returned Cstruct is fresh. *)
let load_main_page ?(bypass_cache = false) t pin_set page_id =
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
       emit_read t page_id;
       if not bypass_cache
       then (
         cache_add t key (cstruct_dup buf);
         pin_page t pin_set page_id);
       Lwt.return_ok buf)
;;

let read ?snapshot_frames ?pin_set ?(bypass_cache = false) t page_id =
  let open Lwt.Syntax in
  let load_after_wal finder =
    let* wal_r = resolve_wal_page ~bypass_cache t ~page_id finder in
    match wal_r with
    | Error e -> Lwt.return_error e
    | Ok (Some page) -> Lwt.return_ok page
    | Ok None -> load_main_page ~bypass_cache t pin_set page_id
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

(* ---------------------------------------------------------------------- *)
(* #244: scoped zero-copy borrow read path                                *)
(* ---------------------------------------------------------------------- *)

(* Like [resolve_wal_page] but hands back the frame buffer WITHOUT a defensive
   copy.  Memory-safe under the borrow contract: the page is decoded-and-
   discarded inside the callback and never mutated or retained.  NOTE (#246):
   on a cache hit [Wal.read_frame] now returns a buffer the WAL RETAINS in its
   decrypted-frame cache, shared across readers — so this no longer rests on the
   old "fresh, unshared buffer per call" property.  It is sound because that
   cached buffer is immutable for the life of the WAL generation (frames are
   append-only; the cache is dropped wholesale on [Wal.reset], never mutated in
   place), exactly like the main page cache's borrow invariant above.  A future
   in-place mutation of a [Wal.read_frame] result would corrupt the cache and
   every concurrent borrower — see [Wal.read_frame]'s contract. *)
(* #611: on a hit this borrows the PAGER cache's entry for that frame, which is
   the very buffer [Wal.read_frame] returned, so the contract above is
   unchanged.  Eviction and [purge_wal_cache] only drop the hashtable entry —
   the buffer itself stays live for as long as the borrower holds it, exactly
   as in [load_main_page_borrow]. *)
let resolve_wal_page_borrow ?(bypass_cache = false) t ~page_id finder =
  match t.wal with
  | None -> Lwt.return_ok None
  | Some cb ->
    sync_wal_epoch t cb;
    (match finder cb with
     | None -> Lwt.return_ok None
     | Some frame_idx ->
       let key = cache_key_wal page_id frame_idx in
       (match Hashtbl.find_opt t.cache key with
        | Some buf -> Lwt.return_ok (Some buf)
        | None ->
          Lwt.map
            (Result.map Option.some)
            (read_and_cache_frame ~bypass_cache t cb ~page_id ~frame_idx ~key)))
;;

(* Like [load_main_page] but returns the cache's own buffer WITHOUT a defensive
   copy, and on a miss caches (and returns) the very buffer it read into rather
   than caching a separate copy.  Sound ONLY under the borrow contract: the
   buffer is decoded-and-discarded inside the callback and never mutated or
   retained.  Cache/dirty buffers are immutable once stored (only ever replaced,
   never written in place — see [write]/[write_owned]/[cache_add]), so a
   concurrent writer dirtying the same page during a yielding callback installs
   a NEW buffer and leaves this borrowed one untouched; eviction merely drops
   the hashtbl entry, the buffer itself stays live while the callback holds it. *)
let load_main_page_borrow ?(bypass_cache = false) t pin_set page_id =
  let open Lwt.Syntax in
  let key = cache_key_main page_id in
  match Hashtbl.find_opt t.cache key with
  | Some buf ->
    pin_page t pin_set page_id;
    Lwt.return_ok buf
  | None ->
    let buf = Cstruct.create t.geom.page_size in
    let* result = t.read_page ~page_id buf in
    (match result with
     | Error msg -> Lwt.return_error (Block_error msg)
     | Ok () ->
       emit_read t page_id;
       if not bypass_cache
       then (
         cache_add t key buf;
         pin_page t pin_set page_id);
       Lwt.return_ok buf)
;;

let read_borrow ?snapshot_frames ?pin_set ?(bypass_cache = false) t page_id f =
  let open Lwt.Syntax in
  let borrow buf =
    let* v = f buf in
    Lwt.return_ok v
  in
  let load_after_wal finder =
    let* wal_r = resolve_wal_page_borrow ~bypass_cache t ~page_id finder in
    match wal_r with
    | Error e -> Lwt.return_error e
    | Ok (Some page) -> borrow page
    | Ok None ->
      let* r = load_main_page_borrow ~bypass_cache t pin_set page_id in
      (match r with
       | Error e -> Lwt.return_error e
       | Ok buf -> borrow buf)
  in
  match snapshot_frames with
  | None ->
    (match Hashtbl.find_opt t.dirty page_id with
     | Some buf -> borrow buf
     | None -> load_after_wal (fun cb -> cb.wal_find_page page_id))
  | Some max_frame -> load_after_wal (fun cb -> cb.wal_find_page_at page_id ~max_frame)
;;

(* #481: [read] without the defensive [cstruct_dup], for callers that RETAIN
   the buffer past a single scope but only ever read it.  [read_borrow] already
   hands out the pager's own buffer, but only for the extent of a callback; a
   B+-tree cursor holds its leaf page across the K [cursor_next] calls that
   consume it, which is not a scope [read_borrow] can express.  Before this,
   that cursor used [read], paying a full ~4 KB page copy per leaf advance —
   even on a cache hit — which lands in the major heap and dominated the
   allocation of a "SELECT COUNT(*)" scan (#481, cause 3).

   CONTRACT — the caller MUST treat the result as read-only and MUST NOT
   mutate it.  Soundness rests on the same invariant as [read_borrow]: cache
   and WAL-frame buffers are immutable once stored (replaced wholesale, never
   written in place — see [write]/[write_owned]/[cache_add]/[Wal.read_frame]),
   and eviction only drops the hashtbl entry, so a retained buffer stays live
   and stays correct for as long as the caller holds it.  Retention is
   therefore safe for an unbounded time, at the cost of keeping one page alive.

   DIRTY PAGES ARE STILL COPIED, deliberately.  A dirty page is the one buffer
   in the pager that IS mutated in place — [dirty_buffer] grants the writer
   exactly that right (#356), and the B+-tree insert path uses it.  Handing a
   retaining reader the live dirty buffer would let a writer shift entries
   under a positioned cursor.  Copying keeps this function's observable
   behaviour identical to [read] on the writer path.

   WHAT THIS ADDS TO THE INVARIANT ANY FUTURE WAL CHANGE MUST KEEP.  Retention
   is unbounded in time, so "immutable once stored" now has to hold for the
   whole life of a scan, not just the extent of a [read_borrow] callback.  Two
   changes in flight touch exactly that: #611 (PR #649) caches WAL-resolved
   frames in the pager, and #612 (PR #644) truncates the WAL at checkpoint.
   Both are fine as long as they keep dropping-and-rebuilding rather than
   recycling: a retained borrower keeps its buffer alive through the GC, so
   evicting a cache entry, resetting a hashtable or truncating the WAL file
   costs it nothing.  What would break it is REUSING a frame or page buffer for
   different content — reading a new frame into a buffer some cache still
   hands out, or writing a checkpointed page back into the buffer a cursor is
   mid-leaf on.  That was already forbidden by [read_borrow]'s contract; this
   function widens the window in which violating it is observable from
   microseconds to the length of a table scan. *)
let read_shared ?snapshot_frames ?pin_set ?(bypass_cache = false) t page_id =
  let open Lwt.Syntax in
  let load_after_wal finder =
    let* wal_r = resolve_wal_page_borrow t ~page_id finder in
    match wal_r with
    | Error e -> Lwt.return_error e
    | Ok (Some page) -> Lwt.return_ok page
    | Ok None -> load_main_page_borrow ~bypass_cache t pin_set page_id
  in
  match snapshot_frames with
  | None ->
    (match Hashtbl.find_opt t.dirty page_id with
     | Some buf -> Lwt.return_ok (cstruct_dup buf)
     | None -> load_after_wal (fun cb -> cb.wal_find_page page_id))
  | Some max_frame -> load_after_wal (fun cb -> cb.wal_find_page_at page_id ~max_frame)
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

(* #356: return the LIVE dirty buffer for [page_id] (NOT a copy), or [None] if
   the page is not dirty in the current write txn.  The caller MAY mutate the
   returned buffer in place: a page in [dirty] was allocated or CoW-copied by
   THIS txn, so no committed snapshot and no concurrent reader references it
   (snapshot readers resolve through WAL frames, never [dirty]; the writer's own
   read-your-own-writes via [read] returns a fresh [cstruct_dup]).  The single
   RW lock guarantees no other writer.  Mutations must keep the page well-formed;
   the CRC is resealed for every dirty page at flush time (see [seal_dirty]). *)
let dirty_buffer t page_id : Cstruct.t option = Hashtbl.find_opt t.dirty page_id

(* Previously also injected into the shared cache here for
     read-after-write inside the same txn.  Removed (#149): a concurrent
     reader at an older snapshot would see uncommitted bytes.  The
     [dirty] table already covers writer read-after-write — [read]
     consults [dirty] first on the no-snapshot path. *)

let alloc t =
  (* #297: consult the txn-owned pool first — pages that were allocated
     above n_pages_at_rw_begin and have since been freed within this txn. *)
  match t.txn_owned_pool with
  | pid :: rest ->
    t.txn_owned_pool <- rest;
    emit_alloc t pid true;
    Lwt.return_ok pid
  | [] ->
    (match Freelist.pop t.freelist ~min_safe_txn_id:t.alloc_min_safe with
     | Some (pid32, fl') ->
       t.freelist <- fl';
       let pid = Int64.of_int32 pid32 in
       emit_alloc t pid true;
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
          emit_alloc t new_id false;
          Lwt.return_ok new_id))
;;

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
  emit_free t page_id
;;

(* Seal all dirty pages' CRCs just before flushing to disk (#356).
   B+-tree build helpers skip Page.seal per-page to avoid sealing pages
   that will be immediately overwritten by the next insert (the
   txn_owned_pool recycles page ids, so at commit only ~O(tree_size) pages
   survive, not O(n_inserts) × pages_per_insert).  Sealing at flush time
   amortises the cost across the entire batch. *)
let seal_dirty t = Hashtbl.iter (fun _ buf -> Page.seal buf) t.dirty

(* Internal: drive the WAL append callback [append] with the dirty
   entries; on success clear the dirty set.  Used by both [flush] (sync)
   and [flush_no_sync] (group commit) so the dirty-set management is
   identical. *)
let flush_via_wal t ~append =
  let open Lwt.Syntax in
  let entries = Hashtbl.fold (fun pid buf acc -> (pid, buf) :: acc) t.dirty [] in
  if entries = []
  then Lwt.return_ok ()
  else (
    seal_dirty t;
    let* r = append entries in
    match r with
    | Error msg -> Lwt.return_error (Block_error msg)
    | Ok () ->
      emit_writes t entries;
      Hashtbl.clear t.dirty;
      Lwt.return_ok ())
;;

let flush_no_sync t =
  match t.wal with
  | Some cb -> flush_via_wal t ~append:cb.wal_append_commit_no_sync
  | None ->
    (* Non-WAL backends have no notion of deferred sync — fall through to
       the regular [flush] which writes pages + syncs. *)
    let entries = Hashtbl.fold (fun pid buf acc -> (pid, buf) :: acc) t.dirty [] in
    seal_dirty t;
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
           emit_write t pid;
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
         emit_writes t entries;
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
           emit_write t pid;
           write_all rest)
    in
    write_all entries
;;

let n_pages t = t.n_pages
let freelist t = t.freelist
let set_txn_id t id = t.current_txn_id <- id
let get_txn_id t = t.current_txn_id
let set_alloc_min_safe t v = t.alloc_min_safe <- v
let set_n_pages_at_rw_begin t v = t.n_pages_at_rw_begin <- v
let txn_owned_pool_get t = t.txn_owned_pool
let txn_owned_pool_set t v = t.txn_owned_pool <- v

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
  Queue.iter (fun key -> if Hashtbl.mem t.cache key then Queue.push key t.fifo) old_fifo;
  (* #297: txn-owned pages are discarded on rollback (they were never
     part of a committed tree). *)
  txn_owned_pool_set t []
;;

type dirty_snapshot = (int64, Cstruct.t) Hashtbl.t

(* #356: DEEP-copy each page buffer when snapshotting/restoring the dirty set
   for savepoints.  In-place insert mutation (see [dirty_buffer]) mutates dirty
   buffers in place, so a shallow [Hashtbl.copy] (which aliases the Cstructs)
   would let a post-savepoint mutation corrupt the snapshot — ROLLBACK TO could
   then not undo it.  Deep copies make the snapshot immune; [dirty_restore]
   likewise installs fresh copies so the snapshot stays pristine for a repeated
   ROLLBACK TO the same savepoint.  Savepoints are only taken on explicit
   SAVEPOINT statements (never per-insert), so this copy is off the hot path. *)
let dirty_clone t =
  let snap = Hashtbl.create (Hashtbl.length t.dirty) in
  Hashtbl.iter (fun k v -> Hashtbl.replace snap k (cstruct_dup v)) t.dirty;
  snap
;;

let dirty_restore t snap =
  Hashtbl.reset t.dirty;
  Hashtbl.iter (fun k v -> Hashtbl.replace t.dirty k (cstruct_dup v)) snap
;;

let flush_one_to_main t ~page_id ~buf =
  let open Lwt.Syntax in
  let* r = t.write_page ~page_id buf in
  match r with
  | Ok () ->
    emit_write t page_id;
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
