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
  ; resize : (int64 -> (unit, string) result Lwt.t) option
    (** #612: physically shrink the WAL device to a byte length.  Optional: a
        device that cannot be resized (the in-memory stubs, a fixed-extent
        block device) simply keeps the old high-water behaviour.  Called ONLY
        from [reset], only after the generation-marker rotation is durable, and
        never with a target below [header_size_bytes]. *)
  ; page_size : int (** page bytes per frame (#95); matches the main DB geometry *)
  ; frame_size : int (** [frame_meta_bytes + page_size + cipher_overhead] *)
  ; cipher : Crypto.t option
    (** When [Some c], frame payloads are AES-256-GCM encrypted. *)
  ; cipher_overhead : int (** 0 or [Crypto.overhead] depending on [cipher]. *)
  ; mutable size_bytes : int64
  ; (* Tracks the high-water mark of the WAL device — initialised to the
     size at open, grows as we append frames so subsequent reads know
     which frames are addressable. *)
    mutable salt : int64
  ; mutable seed : int64
    (* #562: the (salt, seed) pair in the WAL header is the file's GENERATION
       MARKER, not just checksum entropy.  Every frame's checksum is computed
       over it, so rotating the pair at [reset] makes every frame written by the
       previous generation fail recovery's checksum — which is exactly what a
       checkpoint means on disk.  Mutable for that reason alone; nothing else
       changes them after open. *)
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
  ; mutable wrote_since_rotation : bool
    (** #562/#636 (B2): has any frame byte been ISSUED to the device under the
        current generation marker?  Set before the write in [write_pages_at],
        not after it — a write that failed part-way may still have landed, and
        the whole point of the flag is to record uncertainty.  Cleared only by
        a successful rotation in [reset], which is what makes every such frame
        unverifiable again.

        [committed_frames = 0] is NOT a substitute.  [append_commit] writes
        every frame — including the one carrying the commit flag — before
        [flush_sync], and bumps [committed_frames] only after; so a batch whose
        frames reached the device but whose sync failed leaves valid,
        commit-flagged frames on disk with [committed_frames] still 0.  Skipping
        the rotation there would let recovery resurrect them over a shorter
        successor generation — exactly the bug #636 is about, reached through
        the fast path meant to be free. *)
  ; mutable poisoned : string option
    (** #562/#636 (B1): set when [reset]'s header write or fsync failed, so it
        is UNKNOWN whether the device holds the old marker or the new one.
        Neither answer is safe to append under: if the new marker is durable,
        every frame written under the in-memory (old) one fails recovery, so an
        acked, fsynced commit is silently lost on reopen.  While poisoned, every
        append is refused with an error the caller must surface; reads are
        unaffected (they never re-verify checksums).  Terminal for this handle
        — reopening the file re-reads whichever header actually landed and
        recovers from it. *)
  }

(* #246: default bound on the decrypted-frame cache.  One full WAL generation's
   worth of frames (the default auto-checkpoint threshold is 1000), so a hot
   working set that fits the un-checkpointed window never re-decrypts.
   Configurable down via [GRANARY_WAL_FRAME_CACHE] for memory-tight unikernels
   (0 disables the cache entirely, restoring decrypt-on-every-read).  This is
   per-WAL RAM (~4 KB/frame plaintext) and stacks ON TOP of the pager's main
   page cache — size both together when budgeting a unikernel. *)
let default_frame_cache_capacity =
  match Sys.getenv_opt "GRANARY_WAL_FRAME_CACHE" with
  | Some s ->
    (match int_of_string_opt s with
     | Some n when n >= 0 -> n
     | _ -> 1024)
  | None -> 1024
;;

let committed_frames t = t.committed_frames
let size_bytes t = t.size_bytes
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

(* #562: write the 24-byte WAL header and make it durable.  Shared by
   [init_header] (file creation) and [reset] (checkpoint), which differ only in
   where the (salt, seed) pair comes from. *)
let write_header ~write_at ~sync ~salt ~seed =
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
     | Ok () -> Lwt.return_ok ())
;;

(* #613: where a FRESH WAL's [(salt, seed)] generation marker comes from.

   It used to be two [Random.int64] draws.  Nothing in this tree calls
   [Random.self_init] — and [self_init] is not available to a MirageOS
   unikernel anyway, since it reads the clock and the pid — so OCaml's default
   PRNG state is identical in every process: every WAL created by a freshly
   started process got the SAME pair.  Since #636 that pair is not merely
   checksum entropy, it is the generation marker recovery trusts to decide
   which frames are this file's own, so a constant marker means a [-wal] file
   restored next to the WRONG main database verifies as that database's log by
   construction rather than by chance.

   [Mirage_crypto_rng] is the entropy source this library already depends on
   (see [Crypto]), and it is the one that works on both platforms: a unikernel
   seeds it from the Mirage runtime, a Unix application from
   [Mirage_crypto_rng_unix.use_default ()] — which {!Granary_unix.install} now
   does for every file-backed open.

   It can be UNSEEDED, though, and [lib/] deliberately never seeds it itself
   (see [Store.ensure_rng_seeded]): a plaintext store must keep opening with no
   RNG installed at all.  So the draw degrades to the old [Random] pair rather
   than failing the open — a fresh WAL is not worth refusing over a marker
   whose only job is to tell generations apart.  In that degraded mode #613's
   defect is still present, which is why the fix is not the source alone but
   the source plus the Unix driver seeding.

   [set_initial_marker_source] is the seam for a caller with its own entropy,
   and for the fault-injection tests, which need the marker to be reproducible
   rather than fresh. *)
let default_initial_marker () =
  match Mirage_crypto_rng.generate 16 with
  | b -> Some (String.get_int64_be b 0, String.get_int64_be b 8)
  | exception
      (Mirage_crypto_rng.Unseeded_generator | Mirage_crypto_rng.No_default_generator) ->
    None
;;

let initial_marker_source : (unit -> (int64 * int64) option) ref =
  ref default_initial_marker
;;

let set_initial_marker_source f = initial_marker_source := f
let reset_initial_marker_source () = initial_marker_source := default_initial_marker

let draw_initial_marker () =
  match !initial_marker_source () with
  | Some (salt, seed) -> salt, seed
  | None -> Random.int64 Int64.max_int, Random.int64 Int64.max_int
;;

let init_header ~write_at ~sync =
  let salt, seed = draw_initial_marker () in
  let* r = write_header ~write_at ~sync ~salt ~seed in
  match r with
  | Error e -> Lwt.return_error e
  | Ok () -> Lwt.return_ok (salt, seed)
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
      ?resize
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
        ; resize
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
        ; (* The device is too small to hold even a header, so it cannot hold
             a frame either: this is the one branch that is PROVABLY fresh, and
             the only one that may start with the flag clear. *)
          wrote_since_rotation = false
        ; poisoned = None
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
           ; resize
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
           ; (* #636: NOT provably fresh, despite having just written a header.
                [Ok None] means there WERE bytes on the device and the magic did
                not match — a torn 24-byte header write that left the salt/seed
                words intact is exactly this, and the frames it was protecting
                are still there.  [init_header] then re-draws from an
                un-self-init'd [Random] (#613), so a freshly-started process
                draws the SAME (salt, seed) the original creator drew and those
                old frames verify under the supposedly-new marker.  The first
                [reset] would take the fast path, skip the rotation, and let
                recovery resurrect them over a shorter successor generation.

                Same epistemic position as the recovered branch below: we cannot
                enumerate what is on the device, so assume the worst.  Costs one
                header write + fsync on the first checkpoint after opening a
                header-damaged or pre-allocated zero-filled WAL. *)
             wrote_since_rotation = true
           ; poisoned = None
           })
    | Ok (Some (salt, seed)) ->
      let t =
        { read_at
        ; write_at
        ; sync
        ; resize
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
        ; (* #636 (B2): an existing file may hold frames written under this
             marker that recovery does not count — a batch whose frames landed
             but whose sync failed leaves [committed_frames = 0] with valid
             frames on disk.  Assume the worst so the first [reset] rotates. *)
          wrote_since_rotation = true
        ; poisoned = None
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
  (* #636 (B2): record BEFORE issuing any byte.  A write that fails part-way
     may still have reached the device, and [committed_frames] will not move,
     so this flag is the only thing that remembers the frames are there. *)
  t.wrote_since_rotation <- true;
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

(* #636 (B1): refuse to append while the generation marker's durability is
   unknown.  Returning an error here is what turns a silent loss (an acked
   commit that vanishes on reopen) into a failure the caller can surface. *)
let is_poisoned t = t.poisoned <> None

let poison_error t =
  match t.poisoned with
  | None -> None
  | Some why ->
    Some
      (Block_error
         (Printf.sprintf
            "WAL is poisoned: the generation marker's durability is unknown (%s); reopen \
             the database"
            why))
;;

let append_commit_no_sync t pages =
  match poison_error t with
  | Some e -> Lwt.return_error e
  | None ->
    (match pages with
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
          Lwt.return_ok ()))
;;

let append_commit t pages =
  match poison_error t with
  | Some e -> Lwt.return_error e
  | None ->
    (match pages with
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
             Lwt.return_ok ())))
;;

(* #562: derive the next generation's [(salt, seed)] by hashing the current
   one — a chain, not a fresh draw.

   [Random] is the obvious choice and is wrong here: nothing in this tree calls
   [Random.self_init], so every process walks the SAME sequence (#613).  A
   database created by one process and checkpointed by a freshly-started one
   would draw exactly the pair [init_header] drew, i.e. rotate the marker to
   the value it already had — leaving the previous generation's frames
   verifying, which is the whole bug.  Chaining off the current marker cannot do that: the output
   depends on the input, and a repeat would require an FNV collision rather
   than a PRNG restart.

   Mixing in [epoch] and [committed_frames] also separates two resets that
   happen to start from the same marker. *)
let next_generation_marker t =
  let mix tag =
    let h = fnv64_update_int64 fnv64_offset tag in
    let h = fnv64_update_int64 h t.salt in
    let h = fnv64_update_int64 h t.seed in
    let h = fnv64_update_int64 h t.epoch in
    fnv64_update_int64 h (Int64.of_int t.committed_frames)
  in
  (* [frame_checksum] is FNV over [salt] then [seed], so keep the two derived
     words independent by tagging them differently. *)
  let salt =
    mix 0x5741_4C5F_5341_4C54L
    (* "WAL_SALT" *)
  in
  let seed =
    mix 0x5741_4C5F_5345_4544L
    (* "WAL_SEED" *)
  in
  if Int64.equal salt t.salt && Int64.equal seed t.seed
  then (* Astronomically improbable; perturb rather than loop forever. *)
    Int64.succ salt, seed
  else salt, seed
;;

(* #612: physically reclaim the WAL file after a successful generation
   rotation.  Everything below the new tail is dead by construction — the
   rotation already made every byte past [header_size_bytes] fail recovery's
   checksum — so shrinking the file to the bare header removes bytes that no
   longer mean anything to anybody.

   Why it is safe to crash anywhere in here.  The rotation is ALREADY durable
   when this runs (it is fsynced above), which pins the two survivable states:

   - the truncation never reached the device: the old frames are still there
     and still fail the new marker, so recovery reads the WAL as empty — the
     pre-#612 outcome, unchanged;
   - it reached the device wholly or in part: the file is [header_size_bytes]
     long, or some intermediate length whose trailing bytes are old-generation
     frames that fail the new marker just the same.  Recovery reads it as empty
     either way.

   The one thing that would NOT be recoverable is losing the header, so the
   floor is [header_size_bytes] and never 0: a 0-length WAL re-inits a fresh
   marker at the next open, breaking the generation chain [next_generation_marker]
   depends on.

   Doing it in the other order — truncate, then rotate — is what is unsafe: a
   crash in between leaves a short file under the OLD marker, and the next
   generation's frames would be indistinguishable from the survivors of the old
   one.

   {b Failure is not an error.} A device that refuses [ftruncate] costs disk
   space, not correctness, and turning that into a failed [reset] would turn a
   benign EPERM into a failed checkpoint ([Store.ckpt_install] raises on
   a reset error).  So the result is deliberately dropped — but [size_bytes] is
   lowered only on success, because it and the file length have to keep
   describing the same device.  The two errors are not symmetric: leaving
   [size_bytes] high over a short file is harmless (a read past the real tail
   zero-fills or fails, and its checksum fails, so the frame reads as absent),
   while lowering it over a long file makes [read_frame_raw]'s bounds check
   reject frames that are really there.  That asymmetry is the load-bearing
   coupling #612 called out, and it is why the lowering follows the truncation
   rather than preceding it. *)
let truncate_after_rotation t =
  match t.resize with
  | None -> Lwt.return_unit
  | Some resize ->
    let floor = Int64.of_int header_size_bytes in
    if Int64.compare t.size_bytes floor <= 0
    then Lwt.return_unit
    else
      let* r = resize floor in
      (match r with
       | Ok () ->
         t.size_bytes <- floor;
         Lwt.return_unit
       | Error _ -> Lwt.return_unit)
;;

let reset t =
  (* #562: nothing to invalidate when nothing has been written under the
     current marker.  The invariant the rotation maintains is "no frame on the
     device verifies under the current marker except the ones this generation
     committed" — so the rotation is only needed once a frame has been ISSUED
     since the last one.

     #636 (B2): the condition is [wrote_since_rotation], NOT
     [committed_frames = 0].  [append_commit] writes every frame — the
     commit-flagged one included — before [flush_sync] and only then bumps
     [committed_frames], so a batch whose bytes landed and whose sync failed
     leaves valid, commit-flagged frames on disk with [committed_frames] still
     0.  Skipping the rotation there let recovery resurrect them over a shorter
     successor generation: the very bug this code exists to prevent, reached
     through the fast path.  The flag is set before the write, so it is true in
     exactly that case.

     What remains free is the genuinely untouched WAL — a fresh file, or one
     that was already rotated and not appended to since (a no-op
     autocheckpoint, a [Standby.promote] over an empty WAL). *)
  if not t.wrote_since_rotation
  then (
    Hashtbl.reset t.index;
    Hashtbl.reset t.frame_cache;
    Queue.clear t.frame_cache_fifo;
    t.committed_frames <- 0;
    t.epoch <- Int64.succ t.epoch;
    Lwt.return_ok ())
  else (
    let salt, seed = next_generation_marker t in
    (* #562: rotate the on-disk generation marker BEFORE dropping the in-memory
       state, and make it durable.  Crash-safe in this direction only:

       - crash before the header is durable: the old [(salt, seed)] survives,
         the old frames still verify, and recovery replays them.  Harmless —
         the caller has already migrated and fsynced exactly those pages to the
         main DB, so the replay reinstates identical bytes.
       - crash after: the old frames fail recovery's checksum, so the WAL reads
         as empty and the main DB (already fsynced) is the truth.

       Clearing the in-memory state first is what is NOT safe: an append could
       then land at frame 0 under the OLD marker while the caller believes the
       generation has rotated.  Callers must hold the writer lock across this
       call so no append can interleave with the fsync's yield. *)
    let* r = write_header ~write_at:t.write_at ~sync:t.sync ~salt ~seed in
    match r with
    | Error e ->
      (* #636 (B1): the write or the fsync failed, so which marker is on the
         device is UNKNOWN.  Keeping the old one in memory is not the
         conservative choice it looks like: if the new one did land, every
         subsequent commit is written and fsynced under a marker recovery will
         reject, and the application is told those commits are durable.  Adopting
         the new one is no better — if it did NOT land, this generation's frames
         become unreadable.  The only safe move is to stop appending.  Reads are
         left alone: they never re-verify a checksum. *)
      t.poisoned <- Some (Format.asprintf "%a" pp_error e);
      Lwt.return_error e
    | Ok () ->
      t.salt <- salt;
      t.seed <- seed;
      t.wrote_since_rotation <- false;
      Hashtbl.reset t.index;
      (* #246: a checkpoint recycles frame indices, so every cached
         (idx -> bytes) entry now refers to a frame slot that the next
         generation will overwrite.  Drop them all; keeping them would serve a
         stale page. *)
      Hashtbl.reset t.frame_cache;
      Queue.clear t.frame_cache_fifo;
      t.committed_frames <- 0;
      (* #562/#612: the previous generation's frames are physically present past
         the new generation's tail and no longer verify, so recovery stops at
         the first of them.  [size_bytes] must keep covering whatever is
         actually on the device — [read_frame_raw]'s bounds check would
         otherwise reject a frame the next generation legitimately reuses — so
         the two move TOGETHER, here, under the caller's writer lock, or not at
         all.

         The epoch is bumped BEFORE the truncation, not after: the truncation
         yields, and [cache_frame] decides whether to install a decrypted frame
         by comparing the epoch it captured before ITS yield against the current
         one.  With the bump after, a read that started before this reset and
         landed during the truncation's yield would still see the old epoch,
         re-populate the cache we just cleared, and have that entry served for a
         different page once the next generation reuses the index — the exact
         stale-cache hazard [reset] clears the cache to prevent (review #210). *)
      t.epoch <- Int64.succ t.epoch;
      let* () = truncate_after_rotation t in
      Lwt.return_ok ())
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
