(** WAL-based physical replication to object store (Litestream-style).

    Implementation of the apply primitive and cold restore. *)

open Lwt.Syntax
module Wal = Sqlocaml_storage.Wal
module Pager = Sqlocaml_storage.Pager

type replicated_frame =
  { epoch : int64
  ; frame_idx : int
  ; page_id : int64
  ; is_commit : bool
  ; page : Cstruct.t
  ; checksum : int64
  ; source_salt : int64
  ; source_seed : int64
  }

type frame_sink = replicated_frame list -> unit Lwt.t

(* ------------------------------------------------------------------ *)
(* Helpers                                                             *)
(* ------------------------------------------------------------------ *)

(** Group frames into commit batches, splitting at each [is_commit]
    boundary.  Trailing non-commit frames are silently dropped. *)
let group_into_batches frames =
  let rec go acc batch = function
    | [] ->
      (* Only include batches that end with a commit frame.
         Trailing non-commit frames are silently ignored. *)
      if batch = []
      then List.rev acc
      else (
        let last_is_commit = (List.hd (List.rev batch)).is_commit in
        if last_is_commit then List.rev (List.rev batch :: acc) else List.rev acc)
    | f :: rest ->
      if f.is_commit
      then go (List.rev (f :: batch) :: acc) [] rest
      else go acc (f :: batch) rest
  in
  go [] [] frames
;;

let verify_checksum f =
  let flags = if f.is_commit then 1L else 0L in
  let computed =
    Wal.frame_checksum
      ~salt:f.source_salt
      ~seed:f.source_seed
      ~page_id:f.page_id
      ~flags
      ~page:f.page
  in
  Int64.equal computed f.checksum
;;

(* ------------------------------------------------------------------ *)
(* Apply primitive                                                      *)
(* ------------------------------------------------------------------ *)

let apply_frames ~wal ~pager frames =
  (* 1. Verify transport checksums before any side effects *)
  let all_valid = List.for_all verify_checksum frames in
  if not all_valid
  then Lwt.return_error (`Apply_error "transport checksum verification failed")
  else (
    (* 2. Grow the device if any page_id exceeds current capacity *)
    let max_pid = List.fold_left (fun acc f -> Int64.max acc f.page_id) 0L frames in
    let current_pages = Pager.n_pages pager in
    (* Page ids are 0-indexed and n_pages is a count: to hold page
       [max_pid] we need at least [max_pid + 1] pages. *)
    let needed = Int64.succ max_pid in
    if needed > current_pages then Pager.set_n_pages pager needed;
    (* 3. Group frames into commit batches *)
    let batches = group_into_batches frames in
    (* 4. Apply each batch via append_commit *)
    let rec apply = function
      | [] -> Lwt.return_ok ()
      | batch :: rest ->
        let entries = List.map (fun f -> f.page_id, f.page) batch in
        let* r = Wal.append_commit wal entries in
        (match r with
         | Error e ->
           Lwt.return_error
             (`Apply_error (Format.asprintf "append_commit: %a" Wal.pp_error e))
         | Ok () -> apply rest)
    in
    apply batches)
;;

(* ------------------------------------------------------------------ *)
(* Cold restore                                                         *)
(* ------------------------------------------------------------------ *)

let cold_restore
      ~read_at
      ~write_at
      ~sync
      ~wal_size_bytes
      ~pager
      ~base_snapshot_path:_
      ~wal_frames
      ()
  =
  (* Open a WAL over the provided callbacks *)
  let* wal_r = Wal.open_ ~read_at ~write_at ~sync ~size_bytes:wal_size_bytes () in
  match wal_r with
  | Error e ->
    Lwt.return_error (`Restore_error (Format.asprintf "Wal.open_: %a" Wal.pp_error e))
  | Ok wal ->
    (* Accumulate frames into commit batches, apply each batch *)
    let rec loop acc =
      let* next = Lwt_stream.get wal_frames in
      match next with
      | None ->
        (* End of stream: apply any trailing batch that ends with commit *)
        if acc <> [] && (List.hd (List.rev acc)).is_commit
        then
          let* r = apply_frames ~wal ~pager acc in
          match r with
          | Ok () -> Lwt.return_ok ()
          | Error (`Apply_error msg) -> Lwt.return_error (`Restore_error msg)
        else Lwt.return_ok ()
      | Some frames ->
        (* Flush at the first commit boundary; carry remainder forward.
           Note: apply_frames internally calls group_into_batches which
           re-splits multi-commit batches, so we don't need to split
           further here — we just hand the batch off at each commit. *)
        let rec take_until_commit buf = function
          | [] -> List.rev buf, []
          | f :: rest ->
            if f.is_commit
            then List.rev (f :: buf), rest
            else take_until_commit (f :: buf) rest
        in
        let committed, remaining = take_until_commit [] frames in
        if committed = []
        then loop (acc @ frames)
        else (
          let batch = acc @ committed in
          let* r = apply_frames ~wal ~pager batch in
          match r with
          | Error (`Apply_error msg) -> Lwt.return_error (`Restore_error msg)
          | Ok () -> loop remaining)
    in
    loop []
;;

(* ------------------------------------------------------------------ *)
(* Epoch-aware apply for standby (#172)                                *)
(* ------------------------------------------------------------------ *)

(** Migrate the latest version of every page in the WAL index to the main
    DB, sync, then reset the WAL (bumping its epoch).  Mirrors the engine's
    [Store.checkpoint_unlocked] sequence; no reader-gating is needed because
    a following standby serves no readers.  Unlike [checkpoint_unlocked] it
    does not re-pin a downstream replication floor (no [replication_shipped_frames]
    equivalent) — correct for a leaf standby; revisit for cascading
    replication (see #208). *)
let checkpoint_wal_to_main ~wal ~pager =
  let pairs = ref [] in
  Wal.iter_index wal (fun pid idx -> pairs := (pid, idx) :: !pairs);
  let rec write_each = function
    | [] -> Lwt.return_ok ()
    | (pid, idx) :: rest ->
      let* r = Wal.read_frame wal idx in
      (match r with
       | Error e ->
         Lwt.return_error
           (`Apply_error (Format.asprintf "checkpoint read: %a" Wal.pp_error e))
       | Ok page ->
         let* wr = Pager.flush_one_to_main pager ~page_id:pid ~buf:page in
         (match wr with
          | Error e ->
            Lwt.return_error
              (`Apply_error (Format.asprintf "checkpoint write: %a" Pager.pp_error e))
          | Ok () -> write_each rest))
  in
  let* r = write_each !pairs in
  match r with
  | Error (`Apply_error _) as e -> Lwt.return e
  | Ok () ->
    let* sr = Pager.flush_sync_main pager in
    (match sr with
     | Error e ->
       Lwt.return_error
         (`Apply_error (Format.asprintf "checkpoint sync: %a" Pager.pp_error e))
     | Ok () ->
       Wal.reset wal;
       Lwt.return_ok ())
;;

let apply_frames_epoch_aware ~wal ~pager ~last_epoch ~last_idx frames =
  match frames with
  | [] -> Lwt.return_ok (last_epoch, last_idx)
  | _ ->
    let first_epoch = (List.hd frames).epoch in
    let* apply_result =
      if first_epoch <> last_epoch
      then
        let* r = checkpoint_wal_to_main ~wal ~pager in
        match r with
        | Error (`Apply_error _) as e -> Lwt.return e
        | Ok () -> apply_frames ~wal ~pager frames
      else apply_frames ~wal ~pager frames
    in
    (match apply_result with
     | Error _ as e -> Lwt.return e
     | Ok () ->
       (* Report the position of the {e last committed} frame actually
          applied — not just the tail frame.  [apply_frames] drops trailing
          non-commit frames, so a batch ending in non-commit frames still
          durably applied the commit frames before them; keying on the tail
          would under-report the acked position. *)
       let last_committed =
         List.fold_left
           (fun acc f -> if f.is_commit then f.epoch, f.frame_idx else acc)
           (last_epoch, last_idx)
           frames
       in
       Lwt.return_ok last_committed)
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
