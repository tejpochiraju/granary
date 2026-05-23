(* Write-ahead log; see wal.mli for the layout description. *)

open Lwt.Syntax

let page_size = 4096
let header_size_bytes = 24
let frame_meta_bytes = 24
let frame_size_bytes = frame_meta_bytes + page_size  (* = 4120 *)

let wal_magic = 0x57414C35_00000000L  (* "WAL5\0\0\0\0" *)

type frame = {
  frame_idx : int;
  page_id   : int64;
  is_commit : bool;
  page      : Cstruct.t;
}

type error =
  | Block_error of string
  | Corrupt_frame of int

let pp_error fmt = function
  | Block_error s   -> Format.fprintf fmt "Block_error(%s)" s
  | Corrupt_frame i -> Format.fprintf fmt "Corrupt_frame(idx=%d)" i

type t = {
  read_at  : offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t;
  write_at : offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t;
  sync     : unit -> (unit, string) result Lwt.t;
  mutable size_bytes : int64;
  (* Tracks the high-water mark of the WAL device — initialised to the
     size at open, grows as we append frames so subsequent reads know
     which frames are addressable. *)
  salt : int64;
  seed : int64;
  mutable committed_frames : int;
  index : (int64, int list) Hashtbl.t;
  (* page_id -> frame indexes (newest-first). Each commit's new frames are
     prepended; lookup walks the list to find the newest idx <= snapshot. *)
  mutable sync_count : int;
  (* Number of successful device syncs since open. Exposed for #77
     group-commit testing so test_group_commit can prove the fsync
     coalescing behaviour. *)
}

let committed_frames t = t.committed_frames
let sync_count t = t.sync_count

let find_page t pid =
  match Hashtbl.find_opt t.index pid with
  | None | Some [] -> None
  | Some (idx :: _) -> Some idx

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

let iter_index t f =
  Hashtbl.iter (fun pid lst ->
    match lst with
    | [] -> ()
    | idx :: _ -> f pid idx) t.index

(* ----------------------------------------------------------------- *)
(* FNV-1a 64-bit checksum                                              *)
(* ----------------------------------------------------------------- *)

let fnv64_offset = 0xCBF29CE484222325L
let fnv64_prime  = 0x00000100000001B3L

let fnv64_update_byte h b =
  let h' = Int64.logxor h (Int64.of_int (b land 0xff)) in
  Int64.mul h' fnv64_prime

let fnv64_update_int64 h x =
  let h = ref h in
  for i = 7 downto 0 do
    h := fnv64_update_byte !h (Int64.to_int (Int64.shift_right_logical x (i*8)))
  done;
  !h

let fnv64_update_cstruct h c =
  let len = Cstruct.length c in
  let h = ref h in
  for i = 0 to len - 1 do
    h := fnv64_update_byte !h (Char.code (Cstruct.get_char c i))
  done;
  !h

let frame_checksum ~salt ~seed ~page_id ~flags ~page =
  let h = fnv64_offset in
  let h = fnv64_update_int64 h salt in
  let h = fnv64_update_int64 h seed in
  let h = fnv64_update_int64 h page_id in
  let h = fnv64_update_int64 h flags in
  fnv64_update_cstruct h page

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

let read_header ~read_at =
  let hdr = Cstruct.create header_size_bytes in
  let* r = read_at ~offset:0L hdr in
  match r with
  | Error s -> Lwt.return_error (Block_error s)
  | Ok () ->
    let magic = Cstruct.BE.get_uint64 hdr 0 in
    if Int64.equal magic wal_magic then
      let salt = Cstruct.BE.get_uint64 hdr 8 in
      let seed = Cstruct.BE.get_uint64 hdr 16 in
      Lwt.return_ok (Some (salt, seed))
    else
      Lwt.return_ok None

(* ----------------------------------------------------------------- *)
(* Frame read at offset                                                *)
(* ----------------------------------------------------------------- *)

let frame_offset idx =
  Int64.add (Int64.of_int header_size_bytes)
    (Int64.mul (Int64.of_int idx) (Int64.of_int frame_size_bytes))

(* [verify=true] computes and checks the frame checksum (used during
   recovery while we're still discovering what's durable). [verify=false]
   skips the FNV loop and trusts the frame — safe for any frame inside
   [0, committed_frames) because recovery already validated it and the
   WAL is append-only after that. The byte-by-byte FNV over 4 KB is the
   single biggest hot-path cost on WAL reads, so skipping it when sound
   is the main win. *)
let read_frame_raw ?(verify = true) t idx =
  let off = frame_offset idx in
  let last_byte = Int64.add off (Int64.of_int frame_size_bytes) in
  if Int64.compare last_byte t.size_bytes > 0 then
    Lwt.return_ok None
  else
    let buf = Cstruct.create frame_size_bytes in
    let* r = t.read_at ~offset:off buf in
    match r with
    | Error s -> Lwt.return_error (Block_error s)
    | Ok () ->
      let page_id = Cstruct.BE.get_uint64 buf 0 in
      let flags   = Cstruct.BE.get_uint64 buf 8 in
      let page = Cstruct.sub buf frame_meta_bytes page_size in
      let ok =
        if verify then
          let ck_have = Cstruct.BE.get_uint64 buf 16 in
          let ck_want = frame_checksum ~salt:t.salt ~seed:t.seed
                          ~page_id ~flags ~page in
          Int64.equal ck_have ck_want
        else true
      in
      if ok then
        let is_commit = Int64.logand flags 1L <> 0L in
        let page_copy = Cstruct.create page_size in
        Cstruct.blit page 0 page_copy 0 page_size;
        Lwt.return_ok (Some { frame_idx = idx; page_id; is_commit;
                              page = page_copy })
      else
        Lwt.return_ok None

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
    if !stop then Lwt.return_ok ()
    else
      let* r = read_frame_raw t !idx in
      match r with
      | Error e -> Lwt.return_error e
      | Ok None ->
        stop := true; Lwt.return_ok ()  (* EOF or bad checksum *)
      | Ok (Some f) ->
        Hashtbl.replace pending f.page_id !idx;
        if f.is_commit then begin
          (* commit: flush pending into the persistent index *)
          Hashtbl.iter (fun k v ->
            let prev = Option.value ~default:[] (Hashtbl.find_opt t.index k) in
            Hashtbl.replace t.index k (v :: prev)) pending;
          Hashtbl.reset pending;
          last_commit_idx := !idx;
        end;
        incr idx;
        loop ()
  in
  let* r = loop () in
  match r with
  | Error e -> Lwt.return_error e
  | Ok () ->
    t.committed_frames <- !last_commit_idx + 1;
    Lwt.return_ok ()

let open_ ~read_at ~write_at ~sync ~size_bytes =
  if Int64.compare size_bytes (Int64.of_int header_size_bytes) < 0 then begin
    (* Device too small for even a header; treat as fresh and init. *)
    let* r = init_header ~write_at ~sync in
    match r with
    | Error e -> Lwt.return_error e
    | Ok (salt, seed) ->
      Lwt.return_ok {
        read_at; write_at; sync; size_bytes;
        salt; seed;
        committed_frames = 0;
        sync_count = 0;
        index = Hashtbl.create 64;
      }
  end else begin
    let* hr = read_header ~read_at in
    match hr with
    | Error e -> Lwt.return_error e
    | Ok None ->
      (* Magic missing — initialise. *)
      let* r = init_header ~write_at ~sync in
      (match r with
       | Error e -> Lwt.return_error e
       | Ok (salt, seed) ->
         Lwt.return_ok {
           read_at; write_at; sync; size_bytes;
           salt; seed;
           committed_frames = 0;
           sync_count = 0;
           index = Hashtbl.create 64;
         })
    | Ok (Some (salt, seed)) ->
      let t = {
        read_at; write_at; sync; size_bytes;
        salt; seed;
        committed_frames = 0;
        sync_count = 0;
        index = Hashtbl.create 64;
      } in
      let* r = recover_index t in
      (match r with
       | Error e -> Lwt.return_error e
       | Ok () -> Lwt.return_ok t)
  end

(* ----------------------------------------------------------------- *)
(* read_frame                                                          *)
(* ----------------------------------------------------------------- *)

let read_frame t idx =
  if idx < 0 || idx >= t.committed_frames then
    Lwt.return_error (Corrupt_frame idx)
  else
    (* Skip checksum: frames < committed_frames were validated at recovery
       and the WAL is append-only thereafter. *)
    let* r = read_frame_raw ~verify:false t idx in
    match r with
    | Error e -> Lwt.return_error e
    | Ok None -> Lwt.return_error (Corrupt_frame idx)
    | Ok (Some f) -> Lwt.return_ok f.page

(* ----------------------------------------------------------------- *)
(* append_commit                                                       *)
(* ----------------------------------------------------------------- *)

let write_frame t ~idx ~page_id ~is_commit ~page =
  let buf = Cstruct.create frame_size_bytes in
  Cstruct.BE.set_uint64 buf 0 page_id;
  let flags = if is_commit then 1L else 0L in
  Cstruct.BE.set_uint64 buf 8 flags;
  Cstruct.blit page 0 buf frame_meta_bytes page_size;
  let ck = frame_checksum ~salt:t.salt ~seed:t.seed ~page_id ~flags
             ~page:(Cstruct.sub buf frame_meta_bytes page_size) in
  Cstruct.BE.set_uint64 buf 16 ck;
  let off = frame_offset idx in
  t.write_at ~offset:off buf

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
      let* r =
        write_frame t ~idx:(base + i) ~page_id ~is_commit ~page
      in
      (match r with
       | Error s -> Lwt.return_error (Block_error s)
       | Ok () -> write_all (i + 1) rest)
  in
  write_all 0 pages

(* Internal: publish (index update, committed_frames bump, size_bytes
   high-water mark advance) for [pages] just written starting at [base]. *)
let publish_pages t ~base pages =
  let n = List.length pages in
  List.iteri (fun i (page_id, _) ->
    let prev = Option.value ~default:[] (Hashtbl.find_opt t.index page_id) in
    Hashtbl.replace t.index page_id ((base + i) :: prev)) pages;
  t.committed_frames <- base + n;
  let new_end =
    Int64.add (Int64.of_int header_size_bytes)
      (Int64.mul (Int64.of_int (base + n))
         (Int64.of_int frame_size_bytes))
  in
  if Int64.compare new_end t.size_bytes > 0 then
    t.size_bytes <- new_end

let flush_sync t =
  let* r = t.sync () in
  match r with
  | Error s -> Lwt.return_error (Block_error s)
  | Ok () -> t.sync_count <- t.sync_count + 1; Lwt.return_ok ()

let append_commit_no_sync t pages =
  match pages with
  | [] -> Lwt.return_ok ()
  | _ ->
    let base = t.committed_frames in
    let* r = write_pages_at t ~base pages in
    match r with
    | Error e -> Lwt.return_error e
    | Ok _ ->
      (* Publish so subsequent writers (still under [rw_mutex]) and any
         in-flight reads can locate the new frames.  Durability is
         deferred to a later [flush_sync] by the group-commit coordinator;
         a sync failure is treated as fatal by callers. *)
      publish_pages t ~base pages;
      Lwt.return_ok ()

let append_commit t pages =
  match pages with
  | [] -> Lwt.return_ok ()
  | _ ->
    let base = t.committed_frames in
    let* r = write_pages_at t ~base pages in
    match r with
    | Error e -> Lwt.return_error e
    | Ok _ ->
      let* sr = flush_sync t in
      match sr with
      | Error e -> Lwt.return_error e
      | Ok () ->
        publish_pages t ~base pages;
        Lwt.return_ok ()

let reset t =
  Hashtbl.reset t.index;
  t.committed_frames <- 0
