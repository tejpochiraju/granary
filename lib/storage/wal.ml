(* Write-ahead log; see wal.mli for the layout description. *)

open Lwt.Syntax

let header_size_bytes = 24
let frame_meta_bytes = 24

(* Default-geometry frame size (4096-byte page + 24-byte meta = 4120).  A WAL's
   actual frame size follows its page size — see [t.frame_size] (#95).  This
   module-level constant is the 4096 default, exposed for callers/tests that
   build default-geometry WALs. *)
let frame_size_bytes = frame_meta_bytes + Geometry.default.page_size
let wal_magic = 0x57414C35_00000000L (* "WAL5\0\0\0\0" *)

type frame =
  { frame_idx : int
  ; page_id : int64
  ; is_commit : bool
  ; page : Cstruct.t
  }

type error =
  | Block_error of string
  | Corrupt_frame of int

let pp_error fmt = function
  | Block_error s -> Format.fprintf fmt "Block_error(%s)" s
  | Corrupt_frame i -> Format.fprintf fmt "Corrupt_frame(idx=%d)" i
;;

type t =
  { read_at : offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t
  ; write_at : offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t
  ; sync : unit -> (unit, string) result Lwt.t
  ; page_size : int (** page bytes per frame (#95); matches the main DB geometry *)
  ; frame_size : int (** [frame_meta_bytes + page_size + cipher_overhead] *)
  ; cipher : Crypto.t option
    (** When [Some c], frame payloads are AES-256-GCM encrypted. *)
  ; cipher_overhead : int (** 0 or [Crypto.overhead] depending on [cipher]. *)
  ; mutable size_bytes : int64
  ; (* Tracks the high-water mark of the WAL device — initialised to the
     size at open, grows as we append frames so subsequent reads know
     which frames are addressable. *)
    salt : int64
  ; seed : int64
  ; mutable committed_frames : int
  ; index : (int64, int list) Hashtbl.t
  ; (* page_id -> frame indexes (newest-first). Each commit's new frames are
     prepended; [find_page_at] walks the list to find the newest idx that is
     strictly less than the reader's [committed_frames] snapshot. *)
    mutable sync_count : int
  ; (* Number of successful device syncs since open. Exposed for #77
         group-commit testing so test_group_commit can prove the fsync
         coalescing behaviour. *)
    mutable epoch : int64
    (* Bumped every time [reset] is called (i.e. after checkpoint).  Starts
         at 0 on open.  Used by replication to detect whether a checkpoint
         expired its snapshot. *)
  ; frame_cache : (int, Cstruct.t) Hashtbl.t
  ; (* #246: frame_idx -> decrypted plaintext page.  A committed frame's bytes
       are immutable within a WAL generation (the WAL is append-only and
       [read_frame] only ever serves idx < committed_frames), so caching the
       decrypted result lets repeated reads of a WAL-resident page skip BOTH the
       device re-read and the AES-GCM re-decrypt.  Authentication (the GCM tag
       check) still runs on the first, filling read of each frame — exactly like
       the pager's main page cache authenticates once on fill.  This is RAM-only
       and does not weaken the at-rest guarantee (the WAL on disk stays
       encrypted).  MUST be cleared in [reset]: a checkpoint recycles frame
       indices, so a stale (idx -> bytes) entry from the previous generation
       would otherwise be served for a different page. *)
    frame_cache_fifo : int Queue.t (* insertion order for bounded FIFO eviction *)
  ; frame_cache_capacity : int
    (* max cached frames; 0 disables.  See [default_frame_cache_capacity]. *)
  }

(* #246: default bound on the decrypted-frame cache.  One full WAL generation's
   worth of frames (the default auto-checkpoint threshold is 1000), so a hot
   working set that fits the un-checkpointed window never re-decrypts.
   Configurable down via [SQLOCAML_WAL_FRAME_CACHE] for memory-tight unikernels
   (0 disables the cache entirely, restoring decrypt-on-every-read).  This is
   per-WAL RAM (~4 KB/frame plaintext) and stacks ON TOP of the pager's main
   page cache — size both together when budgeting a unikernel. *)
let default_frame_cache_capacity =
  match Sys.getenv_opt "SQLOCAML_WAL_FRAME_CACHE" with
  | Some s ->
    (match int_of_string_opt s with
     | Some n when n >= 0 -> n
     | _ -> 1024)
  | None -> 1024
;;

let committed_frames t = t.committed_frames
let sync_count t = t.sync_count
let epoch t = t.epoch
let salt t = t.salt
let seed t = t.seed

let pp fmt t =
  Format.fprintf
    fmt
    "@[<hv>Wal.t { committed_frames = %d;@ size_bytes = %Ld }@]"
    t.committed_frames
    t.size_bytes
;;

let find_page t pid =
  match Hashtbl.find_opt t.index pid with
  | None | Some [] -> None
  | Some (idx :: _) -> Some idx
;;

(* Snapshot-aware lookup: return the newest frame_idx for [pid] such
   that [idx < max_frame]. Note the strict "<": [max_frame] is the
   reader's [committed_frames] snapshot, which counts how many frames
   are visible — frame indices 0..max_frame-1. *)
let find_page_at t pid ~max_frame =
  match Hashtbl.find_opt t.index pid with
  | None -> None
  | Some lst ->
    let rec scan = function
      | [] -> None
      | idx :: rest -> if idx < max_frame then Some idx else scan rest
    in
    scan lst
;;

let iter_index t f =
  Hashtbl.iter
    (fun pid lst ->
       match lst with
       | [] -> ()
       | idx :: _ -> f pid idx)
    t.index
;;

(* ----------------------------------------------------------------- *)
(* FNV-1a 64-bit checksum                                              *)
(* ----------------------------------------------------------------- *)

let fnv64_offset = 0xCBF29CE484222325L
let fnv64_prime = 0x00000100000001B3L

let fnv64_update_byte h b =
  let h' = Int64.logxor h (Int64.of_int (b land 0xff)) in
  Int64.mul h' fnv64_prime
;;

let fnv64_update_int64 h x =
  let h = ref h in
  for i = 7 downto 0 do
    h := fnv64_update_byte !h (Int64.to_int (Int64.shift_right_logical x (i * 8)))
  done;
  !h
;;

let fnv64_update_cstruct h c =
  let len = Cstruct.length c in
  let h = ref h in
  for i = 0 to len - 1 do
    h := fnv64_update_byte !h (Char.code (Cstruct.get_char c i))
  done;
  !h
;;

let frame_checksum ~salt ~seed ~page_id ~flags ~page =
  let h = fnv64_offset in
  let h = fnv64_update_int64 h salt in
  let h = fnv64_update_int64 h seed in
  let h = fnv64_update_int64 h page_id in
  let h = fnv64_update_int64 h flags in
  fnv64_update_cstruct h page
;;

(* ----------------------------------------------------------------- *)
(* Header read/write                                                   *)
(* ----------------------------------------------------------------- *)

let init_header ~write_at ~sync =
  let salt = Random.int64 Int64.max_int in
  let seed = Random.int64 Int64.max_int in
  let hdr = Cstruct.create header_size_bytes in
  Cstruct.BE.set_uint64 hdr 0 wal_magic;
  Cstruct.BE.set_uint64 hdr 8 salt;
  Cstruct.BE.set_uint64 hdr 16 seed;
  let* r = write_at ~offset:0L hdr in
  match r with
  | Error s -> Lwt.return_error (Block_error s)
  | Ok () ->
    let* s = sync () in
    (match s with
     | Error s -> Lwt.return_error (Block_error s)
     | Ok () -> Lwt.return_ok (salt, seed))
;;

let read_header ~read_at =
  let hdr = Cstruct.create header_size_bytes in
  let* r = read_at ~offset:0L hdr in
  match r with
  | Error s -> Lwt.return_error (Block_error s)
  | Ok () ->
    let magic = Cstruct.BE.get_uint64 hdr 0 in
    if Int64.equal magic wal_magic
    then (
      let salt = Cstruct.BE.get_uint64 hdr 8 in
      let seed = Cstruct.BE.get_uint64 hdr 16 in
      Lwt.return_ok (Some (salt, seed)))
    else Lwt.return_ok None
;;

(* ----------------------------------------------------------------- *)
(* Frame read at offset                                                *)
(* ----------------------------------------------------------------- *)

let frame_offset t idx =
  Int64.add
    (Int64.of_int header_size_bytes)
    (Int64.mul (Int64.of_int idx) (Int64.of_int t.frame_size))
;;

(* [verify=true] computes and checks the frame checksum (used during
   recovery while we're still discovering what's durable). [verify=false]
   skips the FNV loop and trusts the frame — safe for any frame inside
   [0, committed_frames) because recovery already validated it and the
   WAL is append-only after that. The byte-by-byte FNV over 4 KB is the
   single biggest hot-path cost on WAL reads, so skipping it when sound
   is the main win. *)
let read_frame_raw ?(verify = true) t idx =
  let off = frame_offset t idx in
  let last_byte = Int64.add off (Int64.of_int t.frame_size) in
  if Int64.compare last_byte t.size_bytes > 0
  then Lwt.return_ok None
  else (
    let buf = Cstruct.create t.frame_size in
    let* r = t.read_at ~offset:off buf in
    match r with
    | Error s -> Lwt.return_error (Block_error s)
    | Ok () ->
      let page_id = Cstruct.BE.get_uint64 buf 0 in
      let flags = Cstruct.BE.get_uint64 buf 8 in
      let payload_len = t.page_size + t.cipher_overhead in
      let payload = Cstruct.sub buf frame_meta_bytes payload_len in
      let ok =
        if verify
        then (
          let ck_have = Cstruct.BE.get_uint64 buf 16 in
          let ck_want =
            frame_checksum ~salt:t.salt ~seed:t.seed ~page_id ~flags ~page:payload
          in
          Int64.equal ck_have ck_want)
        else true
      in
      if ok
      then (
        let is_commit = Int64.logand flags 1L <> 0L in
        match t.cipher with
        | None ->
          let page_copy = Cstruct.create t.page_size in
          Cstruct.blit payload 0 page_copy 0 t.page_size;
          Lwt.return_ok (Some { frame_idx = idx; page_id; is_commit; page = page_copy })
        | Some c ->
          (match Crypto.decrypt_frame c ~page_id payload with
           | Error `Tag_mismatch ->
             (* The FNV checksum (over ciphertext) already passed, so this is a
                structurally-intact frame whose GCM tag fails: not a torn/short
                tail but an authenticated-frame integrity failure — i.e. genuine
                tampering (the checksum salt/seed are not key-derived, so an
                attacker editing ciphertext can repair the checksum; GCM is what
                catches it; wrong-key is already caught at open by the header
                canary).  Surface it as a hard error rather than silently
                treating it as end-of-WAL, which would drop later committed
                frames and mask the attack as benign truncation (#219). *)
             Lwt.return_error (Corrupt_frame idx)
           | Ok page_copy ->
             Lwt.return_ok
               (Some { frame_idx = idx; page_id; is_commit; page = page_copy })))
      else Lwt.return_ok None)
;;

(* ----------------------------------------------------------------- *)
(* Open + recovery                                                     *)
(* ----------------------------------------------------------------- *)

let recover_index t =
  (* Walk forward; keep "pending" updates in a local table; on commit frame
     flush pending into the persistent index and bump committed_frames.
     Stop on first frame whose checksum/EOF fails. *)
  let pending : (int64, int) Hashtbl.t = Hashtbl.create 16 in
  let last_commit_idx = ref (-1) in
  let stop = ref false in
  let idx = ref 0 in
  let rec loop () =
    if !stop
    then Lwt.return_ok ()
    else
      let* r = read_frame_raw t !idx in
      match r with
      | Error e -> Lwt.return_error e
      | Ok None ->
        stop := true;
        Lwt.return_ok () (* EOF or bad checksum *)
      | Ok (Some f) ->
        Hashtbl.replace pending f.page_id !idx;
        if f.is_commit
        then (
          (* commit: flush pending into the persistent index *)
          Hashtbl.iter
            (fun k v ->
               let prev = Option.value ~default:[] (Hashtbl.find_opt t.index k) in
               Hashtbl.replace t.index k (v :: prev))
            pending;
          Hashtbl.reset pending;
          last_commit_idx := !idx);
        incr idx;
        loop ()
  in
  let* r = loop () in
  match r with
  | Error e -> Lwt.return_error e
  | Ok () ->
    t.committed_frames <- !last_commit_idx + 1;
    Lwt.return_ok ()
;;

let open_
      ?(cipher = None)
      ?(page_size = Geometry.default.page_size)
      ?(frame_cache_capacity = default_frame_cache_capacity)
      ~read_at
      ~write_at
      ~sync
      ~size_bytes
      ()
  =
  let cipher_overhead =
    match cipher with
    | Some _ -> Crypto.overhead
    | None -> 0
  in
  let frame_size = frame_meta_bytes + page_size + cipher_overhead in
  if Int64.compare size_bytes (Int64.of_int header_size_bytes) < 0
  then
    (* Device too small for even a header; treat as fresh and init. *)
    let* r = init_header ~write_at ~sync in
    match r with
    | Error e -> Lwt.return_error e
    | Ok (salt, seed) ->
      Lwt.return_ok
        { read_at
        ; write_at
        ; sync
        ; page_size
        ; frame_size
        ; cipher
        ; cipher_overhead
        ; size_bytes
        ; salt
        ; seed
        ; committed_frames = 0
        ; sync_count = 0
        ; epoch = 0L
        ; index = Hashtbl.create 64
        ; frame_cache = Hashtbl.create 64
        ; frame_cache_fifo = Queue.create ()
        ; frame_cache_capacity
        }
  else
    let* hr = read_header ~read_at in
    match hr with
    | Error e -> Lwt.return_error e
    | Ok None ->
      (* Magic missing — initialise. *)
      let* r = init_header ~write_at ~sync in
      (match r with
       | Error e -> Lwt.return_error e
       | Ok (salt, seed) ->
         Lwt.return_ok
           { read_at
           ; write_at
           ; sync
           ; page_size
           ; frame_size
           ; cipher
           ; cipher_overhead
           ; size_bytes
           ; salt
           ; seed
           ; committed_frames = 0
           ; sync_count = 0
           ; epoch = 0L
           ; index = Hashtbl.create 64
           ; frame_cache = Hashtbl.create 64
           ; frame_cache_fifo = Queue.create ()
           ; frame_cache_capacity
           })
    | Ok (Some (salt, seed)) ->
      let t =
        { read_at
        ; write_at
        ; sync
        ; page_size
        ; frame_size
        ; cipher
        ; cipher_overhead
        ; size_bytes
        ; salt
        ; seed
        ; committed_frames = 0
        ; sync_count = 0
        ; epoch = 0L
        ; index = Hashtbl.create 64
        ; frame_cache = Hashtbl.create 64
        ; frame_cache_fifo = Queue.create ()
        ; frame_cache_capacity
        }
      in
      let* r = recover_index t in
      (match r with
       | Error e -> Lwt.return_error e
       | Ok () -> Lwt.return_ok t)
;;

(* ----------------------------------------------------------------- *)
(* read_frame                                                          *)
(* ----------------------------------------------------------------- *)

(* #246: install a decrypted frame into the bounded cache.  The buffer is the
   freshly-decrypted page from [read_frame_raw]; it is never mutated in place
   afterwards (callers either [cstruct_dup] it or borrow it read-only under the
   pager's borrow contract), so sharing it across repeated reads is sound — the
   same immutability invariant the main page cache relies on.

   [expected_epoch] guards against cache-poisoning: a concurrent checkpoint
   can call [Wal.reset] (clearing [frame_cache] and bumping epoch) during
   the I/O yield in [read_frame_raw]; the caller captures the epoch before
   the yield and passes it here so we no-op if the epoch has changed
   (review #210). *)
let cache_frame t ~expected_epoch idx page =
  if
    Int64.equal t.epoch expected_epoch
    && t.frame_cache_capacity > 0
    && not (Hashtbl.mem t.frame_cache idx)
  then (
    while
      Hashtbl.length t.frame_cache >= t.frame_cache_capacity
      && not (Queue.is_empty t.frame_cache_fifo)
    do
      Hashtbl.remove t.frame_cache (Queue.pop t.frame_cache_fifo)
    done;
    Hashtbl.replace t.frame_cache idx page;
    Queue.push idx t.frame_cache_fifo)
;;

let read_frame t idx =
  if idx < 0 || idx >= t.committed_frames
  then Lwt.return_error (Corrupt_frame idx)
  else (
    match Hashtbl.find_opt t.frame_cache idx with
    | Some page ->
      (* Cache hit: skip the device re-read and the AES-GCM re-decrypt.  Auth
         already ran on the filling read below. *)
      Lwt.return_ok page
    | None ->
      (* Skip checksum: frames < committed_frames were validated at recovery
         and the WAL is append-only thereafter. *)
      let epoch_before = t.epoch in
      let* r = read_frame_raw ~verify:false t idx in
      (match r with
       | Error e -> Lwt.return_error e
       | Ok None -> Lwt.return_error (Corrupt_frame idx)
       | Ok (Some f) ->
         cache_frame t ~expected_epoch:epoch_before idx f.page;
         Lwt.return_ok f.page))
;;

let read_committed_frame t idx =
  if idx < 0 || idx >= t.committed_frames
  then Lwt.return_error (Corrupt_frame idx)
  else
    let* r = read_frame_raw ~verify:false t idx in
    match r with
    | Error e -> Lwt.return_error e
    | Ok None -> Lwt.return_error (Corrupt_frame idx)
    | Ok (Some f) ->
      (* Do NOT call [cache_frame] here: a concurrent checkpoint could
         have reset the cache during the I/O yield, and inserting a
         stale-epoch page would poison the cache for the new epoch
         (review #209).  The normal [read_frame] path populates the
         cache; backup reads don't need to prime it. *)
      Lwt.return_ok f
;;

(* ----------------------------------------------------------------- *)
(* append_commit                                                       *)
(* ----------------------------------------------------------------- *)

let write_frame t ~idx ~page_id ~is_commit ~page =
  let buf = Cstruct.create t.frame_size in
  Cstruct.BE.set_uint64 buf 0 page_id;
  let flags = if is_commit then 1L else 0L in
  Cstruct.BE.set_uint64 buf 8 flags;
  let payload_len = t.page_size + t.cipher_overhead in
  (match t.cipher with
   | None -> Cstruct.blit page 0 buf frame_meta_bytes t.page_size
   | Some c ->
     let enc = Crypto.encrypt_frame c ~page_id ~plaintext:page in
     Cstruct.blit enc 0 buf frame_meta_bytes payload_len);
  let ck =
    frame_checksum
      ~salt:t.salt
      ~seed:t.seed
      ~page_id
      ~flags
      ~page:(Cstruct.sub buf frame_meta_bytes payload_len)
  in
  Cstruct.BE.set_uint64 buf 16 ck;
  let off = frame_offset t idx in
  t.write_at ~offset:off buf
;;

(* Internal: write [pages] starting at [base], without syncing. Returns
   [n] (the number of pages written) on success.  Frames are emitted with
   the last one carrying the commit marker. *)
let write_pages_at t ~base pages =
  let n = List.length pages in
  let last = n - 1 in
  let rec write_all i = function
    | [] -> Lwt.return_ok n
    | (page_id, page) :: rest ->
      let is_commit = i = last in
      let* r = write_frame t ~idx:(base + i) ~page_id ~is_commit ~page in
      (match r with
       | Error s -> Lwt.return_error (Block_error s)
       | Ok () -> write_all (i + 1) rest)
  in
  write_all 0 pages
;;

(* Internal: publish (index update, committed_frames bump, size_bytes
   high-water mark advance) for [pages] just written starting at [base]. *)
let publish_pages t ~base pages =
  let n = List.length pages in
  List.iteri
    (fun i (page_id, _) ->
       let prev = Option.value ~default:[] (Hashtbl.find_opt t.index page_id) in
       Hashtbl.replace t.index page_id ((base + i) :: prev))
    pages;
  t.committed_frames <- base + n;
  let new_end =
    Int64.add
      (Int64.of_int header_size_bytes)
      (Int64.mul (Int64.of_int (base + n)) (Int64.of_int t.frame_size))
  in
  if Int64.compare new_end t.size_bytes > 0 then t.size_bytes <- new_end
;;

let flush_sync t =
  let* r = t.sync () in
  match r with
  | Error s -> Lwt.return_error (Block_error s)
  | Ok () ->
    t.sync_count <- t.sync_count + 1;
    Lwt.return_ok ()
;;

let append_commit_no_sync t pages =
  match pages with
  | [] -> Lwt.return_ok ()
  | _ ->
    let base = t.committed_frames in
    let* r = write_pages_at t ~base pages in
    (match r with
     | Error e -> Lwt.return_error e
     | Ok _ ->
       (* Publish so subsequent writers (still under [rw_mutex]) and any
         in-flight reads can locate the new frames.  Durability is
         deferred to a later [flush_sync] by the group-commit coordinator;
         a sync failure is treated as fatal by callers. *)
       publish_pages t ~base pages;
       Lwt.return_ok ())
;;

let append_commit t pages =
  match pages with
  | [] -> Lwt.return_ok ()
  | _ ->
    let base = t.committed_frames in
    let* r = write_pages_at t ~base pages in
    (match r with
     | Error e -> Lwt.return_error e
     | Ok _ ->
       let* sr = flush_sync t in
       (match sr with
        | Error e -> Lwt.return_error e
        | Ok () ->
          publish_pages t ~base pages;
          Lwt.return_ok ()))
;;

let reset t =
  Hashtbl.reset t.index;
  (* #246: a checkpoint recycles frame indices, so every cached (idx -> bytes)
     entry now refers to a frame slot that will be overwritten by the next
     generation.  Drop them all; failing to do so would serve a stale page. *)
  Hashtbl.reset t.frame_cache;
  Queue.clear t.frame_cache_fifo;
  t.committed_frames <- 0;
  t.epoch <- Int64.succ t.epoch
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
