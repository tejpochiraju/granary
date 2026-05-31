(** Standby follower driver (#172) implementation. *)

open Lwt.Syntax
module Store = Sqlocaml_store.Store
module Wal = Sqlocaml_storage.Wal
module Pager = Sqlocaml_storage.Pager

type follower_mode =
  | Following
  | Promoted

type acked_position =
  { epoch : int64
  ; frame_idx : int
  }

type t =
  { store : Store.t
  ; pager : Pager.t
  ; wal : Wal.t
  ; mutable mode : follower_mode
  ; mutable last_epoch : int64
  ; mutable last_frame_idx : int
  ; apply_mutex : Lwt_mutex.t
  }

let create ~store ~pager ~wal =
  { store
  ; pager
  ; wal
  ; mode = Following
  ; last_epoch = 0L
  ; last_frame_idx = -1
  ; apply_mutex = Lwt_mutex.create ()
  }
;;

let mode t = t.mode
let acked_position t = { epoch = t.last_epoch; frame_idx = t.last_frame_idx }

let pp fmt t =
  Format.fprintf
    fmt
    "Standby.t { mode = %s; epoch = %Ld; frame_idx = %d }"
    (match t.mode with
     | Following -> "Following"
     | Promoted -> "Promoted")
    t.last_epoch
    t.last_frame_idx
;;

let promote t =
  match t.mode with
  | Promoted -> Lwt.return_unit
  | Following ->
    (* Take the apply mutex so any in-flight [apply_frames_epoch_aware] from
       the follower loop completes before we drain and reset.  Re-check the
       mode under the lock: a concurrent [promote] may have won the race. *)
    Lwt_mutex.with_lock t.apply_mutex (fun () ->
      match t.mode with
      | Promoted -> Lwt.return_unit
      | Following ->
        (* Drain buffered committed frames to the main DB before recycling
           the WAL — otherwise the fresh engine view opened on promotion
           (which recovers from main) would lose every frame applied since
           the last epoch-change checkpoint.  [checkpoint_wal_to_main] also
           bumps the local WAL epoch via [Wal.reset], so locally-generated
           writes start with a fresh epoch distinct from the master's. *)
        let* r = Replication.checkpoint_wal_to_main ~wal:t.wal ~pager:t.pager in
        (match r with
         | Error (`Apply_error msg) ->
           Lwt.fail_with ("Standby.promote: drain failed: " ^ msg)
         | Ok () ->
           t.mode <- Promoted;
           Store.set_follower t.store false;
           t.last_epoch <- Wal.epoch t.wal;
           t.last_frame_idx <- 0;
           Lwt.return_unit))
;;

let start_following t stream =
  Store.set_follower t.store true;
  let* result =
    Lwt.catch
      (fun () ->
         let rec loop () =
           let* next = Lwt_stream.get stream in
           match next with
           | None ->
             if t.mode = Following then Store.set_follower t.store false;
             Lwt.return (Ok ())
           | Some frames ->
             let* () = Lwt_mutex.lock t.apply_mutex in
             let* r =
               Replication.apply_frames_epoch_aware
                 ~wal:t.wal
                 ~pager:t.pager
                 ~last_epoch:t.last_epoch
                 ~last_idx:t.last_frame_idx
                 frames
             in
             (match r with
              | Error _ as e ->
                Lwt_mutex.unlock t.apply_mutex;
                Lwt.return e
              | Ok (epoch, idx) ->
                t.last_epoch <- epoch;
                t.last_frame_idx <- idx;
                Lwt_mutex.unlock t.apply_mutex;
                loop ())
         in
         loop ())
      (fun exn ->
         if t.mode = Following then Store.set_follower t.store false;
         Lwt.return (Error (`Apply_error (Printexc.to_string exn))))
  in
  (* Ensure follower mode is cleared on any exit path (unless promoted) *)
  if t.mode = Following then Store.set_follower t.store false;
  Lwt.return result
;;
