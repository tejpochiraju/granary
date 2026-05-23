(** Pager: page cache + allocator over a BLOCK backend.

    Maintains:
    - A bounded FIFO cache of up to 64 pages (read from BLOCK).
    - A dirty table of pages modified since the last flush.
    - An in-memory freelist for page allocation.

    Dirty pages are never evicted from the cache; they are written to BLOCK only
    on [flush]. *)

let cache_capacity = 64

type wal_callbacks = {
  wal_find_page    : int64 -> int option;
  wal_read_frame   : int -> (Cstruct.t, string) result Lwt.t;
  wal_append_commit: (int64 * Cstruct.t) list -> (unit, string) result Lwt.t;
}

type t = {
  read_page  : page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t;
  write_page : page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t;
  sync       : unit -> (unit, string) result Lwt.t;
  resize     : n_pages:int64 -> (unit, string) result Lwt.t;
  cache      : (int64, Cstruct.t) Hashtbl.t;
  dirty      : (int64, Cstruct.t) Hashtbl.t;
  fifo       : int64 Queue.t;   (* insertion order for FIFO eviction *)
  mutable n_pages       : int64;
  mutable freelist      : Freelist.t;
  mutable current_txn_id  : int64;
  mutable alloc_min_safe  : int64;
  mutable wal            : wal_callbacks option;
}

type error = Block_error of string | Corruption of string

let pp_error fmt = function
  | Block_error msg -> Format.fprintf fmt "Block_error: %s" msg
  | Corruption msg  -> Format.fprintf fmt "Corruption: %s" msg

let create ~read_page ~write_page ~sync ~resize ~n_pages ~freelist =
  { read_page;
    write_page;
    sync;
    resize;
    cache           = Hashtbl.create 64;
    dirty           = Hashtbl.create 16;
    fifo            = Queue.create ();
    n_pages;
    freelist;
    current_txn_id  = 0L;
    alloc_min_safe  = 0L;
    wal             = None;
  }

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
let wal_mode t = t.wal <> None

(** Evict the oldest cache entry if the cache is at capacity.
    Never evicts dirty pages. *)
let maybe_evict t =
  (* Keep trying to evict until we find a clean page or the cache is small enough *)
  let cache_size = Hashtbl.length t.cache in
  if cache_size < cache_capacity then ()
  else begin
    (* Scan the FIFO queue front-to-back looking for a non-dirty page *)
    let evicted = ref false in
    let temp = Queue.create () in
    while not !evicted && not (Queue.is_empty t.fifo) do
      let pid = Queue.pop t.fifo in
      if Hashtbl.mem t.dirty pid then
        (* dirty — put back at end so we don't lose track of it *)
        Queue.push pid temp
      else begin
        Hashtbl.remove t.cache pid;
        evicted := true;
        (* push anything we moved to temp back into the real queue *)
        Queue.iter (fun p -> Queue.push p t.fifo) temp;
        Queue.clear temp
      end
    done;
    (* If we couldn't evict (all cached pages are dirty), just keep them *)
    if not !evicted then
      Queue.iter (fun p -> Queue.push p t.fifo) temp
  end

(** Add a page to the cache, evicting if necessary. *)
let cache_add t page_id buf =
  let already_cached = Hashtbl.mem t.cache page_id in
  maybe_evict t;
  Hashtbl.replace t.cache page_id buf;
  if not already_cached then
    Queue.push page_id t.fifo

(** Make a deep copy of a Cstruct. *)
let cstruct_dup src =
  let len = Cstruct.length src in
  let dst = Cstruct.create len in
  Cstruct.blit src 0 dst 0 len;
  dst

let read t page_id =
  (* Dirty (uncommitted current-txn writes) takes priority — it is
     always more recent than anything elsewhere. *)
  match Hashtbl.find_opt t.dirty page_id with
  | Some buf ->
    Lwt.return_ok (cstruct_dup buf)
  | None ->
    let open Lwt.Syntax in
    (* The cache holds the latest committed-or-written version of any
       page we've touched: [write] adds the new data, [from_wal] adds
       the WAL frame contents on first miss, [flush_one_to_main] keeps
       it fresh after checkpoint, [clear_dirty] purges entries backing
       rolled-back writes, and [set_wal] resets it when WAL mode is
       enabled (so pre-WAL-aware reads from main don't shadow newer
       WAL frames). So a cache hit is authoritative and we don't need
       to re-resolve through the WAL device on every probe — this
       eliminates the per-read frame I/O + allocation that dominated
       the WAL hot path. *)
    (match Hashtbl.find_opt t.cache page_id with
     | Some buf -> Lwt.return_ok (cstruct_dup buf)
     | None ->
       let from_wal () =
         match t.wal with
         | None -> Lwt.return_ok None
         | Some cb ->
           (match cb.wal_find_page page_id with
            | None -> Lwt.return_ok None
            | Some frame_idx ->
              let* r = cb.wal_read_frame frame_idx in
              (match r with
               | Error s -> Lwt.return_error (Block_error s)
               | Ok page ->
                 let copy = cstruct_dup page in
                 cache_add t page_id (cstruct_dup copy);
                 Lwt.return_ok (Some copy)))
       in
       let* wal_r = from_wal () in
       (match wal_r with
        | Error e -> Lwt.return_error e
        | Ok (Some page) -> Lwt.return_ok page
        | Ok None ->
          let buf = Cstruct.create Page.page_size in
          let* result = t.read_page ~page_id buf in
          match result with
          | Error msg -> Lwt.return_error (Block_error msg)
          | Ok () ->
            let copy = cstruct_dup buf in
            cache_add t page_id copy;
            Lwt.return_ok (cstruct_dup copy)))

let write t page_id buf =
  let copy = cstruct_dup buf in
  Hashtbl.replace t.dirty page_id copy;
  (* Also update / add to the read cache so subsequent reads see the new data *)
  let cache_copy = cstruct_dup buf in
  cache_add t page_id cache_copy

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
     | Ok ()     ->
       t.n_pages <- new_pages;
       Lwt.return_ok new_id)

let free t ~page_id ~freed_at_txn_id =
  t.freelist <-
    Freelist.add t.freelist
      ~page_id:(Int64.to_int32 page_id)
      ~freed_at_txn_id

let flush t =
  let open Lwt.Syntax in
  let entries = Hashtbl.fold (fun pid buf acc -> (pid, buf) :: acc) t.dirty [] in
  match t.wal with
  | Some cb ->
    if entries = [] then Lwt.return_ok ()
    else begin
      let* r = cb.wal_append_commit entries in
      match r with
      | Error msg -> Lwt.return_error (Block_error msg)
      | Ok () ->
        Hashtbl.clear t.dirty;
        Lwt.return_ok ()
    end
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
         | Ok ()     -> write_all rest)
    in
    write_all entries

let n_pages t = t.n_pages

let freelist t = t.freelist

let set_txn_id t id = t.current_txn_id <- id

let get_txn_id t = t.current_txn_id

let set_alloc_min_safe t v = t.alloc_min_safe <- v

let set_freelist t fl = t.freelist <- fl

let set_n_pages t n = t.n_pages <- n

let clear_dirty t =
  let dirty_pids = Hashtbl.fold (fun pid _ acc -> pid :: acc) t.dirty [] in
  List.iter (fun pid ->
    Hashtbl.remove t.dirty pid;
    Hashtbl.remove t.cache pid
  ) dirty_pids;
  (* Rebuild FIFO queue without the removed page ids *)
  let pids_set = Hashtbl.create (List.length dirty_pids) in
  List.iter (fun pid -> Hashtbl.replace pids_set pid ()) dirty_pids;
  let old_fifo = Queue.copy t.fifo in
  Queue.clear t.fifo;
  Queue.iter (fun pid ->
    if not (Hashtbl.mem pids_set pid) then Queue.push pid t.fifo
  ) old_fifo

type dirty_snapshot = (int64, Cstruct.t) Hashtbl.t

let dirty_clone t = Hashtbl.copy t.dirty

let dirty_restore t snap =
  Hashtbl.reset t.dirty;
  Hashtbl.iter (fun k v -> Hashtbl.replace t.dirty k v) snap

let flush_one_to_main t ~page_id ~buf =
  let open Lwt.Syntax in
  let* r = t.write_page ~page_id buf in
  match r with
  | Ok () ->
    Hashtbl.replace t.cache page_id (cstruct_dup buf);
    Lwt.return_ok ()
  | Error s -> Lwt.return_error (Block_error s)

let flush_sync_main t =
  let open Lwt.Syntax in
  let* r = t.sync () in
  match r with
  | Ok () -> Lwt.return_ok ()
  | Error s -> Lwt.return_error (Block_error s)
