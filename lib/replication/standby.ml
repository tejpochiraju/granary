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
           (* Nothing applied in the new (local) epoch yet — same sentinel
              as [create]. *)
           t.last_frame_idx <- -1;
           Lwt.return_unit))
;;

let start_following t stream =
  match t.mode with
  | Promoted ->
    (* Never re-enter follower mode on a promoted node: doing so would leave
       the store rejecting writes (the exit clause below only clears follower
       mode when still [Following]).  Promotion is terminal. *)
    Lwt.return (Ok ())
  | Following ->
    Store.set_follower t.store true;
    let* result =
      Lwt.catch
        (fun () ->
           let rec loop () =
             let* next = Lwt_stream.get stream in
             match next with
             | None -> Lwt.return (Ok ())
             | Some frames ->
               (* [with_lock] releases the mutex even if [apply_frames_epoch_aware]
                raises, so a faulting batch can never wedge a later [promote].
                Re-check the mode under the lock: a [promote] may have won the
                race while we were blocked, in which case the WAL has been
                recycled and we must not apply this batch into the promoted
                node. *)
               let* outcome =
                 Lwt_mutex.with_lock t.apply_mutex (fun () ->
                   match t.mode with
                   | Promoted -> Lwt.return (`Stop (Ok ()))
                   | Following ->
                     let* r =
                       Replication.apply_frames_epoch_aware
                         ~wal:t.wal
                         ~pager:t.pager
                         ~last_epoch:t.last_epoch
                         ~last_idx:t.last_frame_idx
                         frames
                     in
                     (match r with
                      | Error _ as e -> Lwt.return (`Stop e)
                      | Ok (epoch, idx) ->
                        t.last_epoch <- epoch;
                        t.last_frame_idx <- idx;
                        Store.set_follower_ack_position t.store;
                        Lwt.return `Continue))
               in
               (match outcome with
                | `Stop result -> Lwt.return result
                | `Continue -> loop ())
           in
           loop ())
        (fun exn -> Lwt.return (Error (`Apply_error (Printexc.to_string exn))))
    in
    (* Clear follower mode on any exit path unless we have been promoted. *)
    if t.mode = Following then Store.set_follower t.store false;
    Lwt.return result
;;

let rebase t segments =
  match t.mode with
  | Promoted ->
    Lwt.return (Error (`Apply_error "Standby.rebase: cannot rebase a promoted standby"))
  | Following ->
    (* Take the apply mutex so a rebase never races an in-flight apply from a
       (stopped, but possibly still draining) follower loop. *)
    Lwt_mutex.with_lock t.apply_mutex (fun () ->
      match t.mode with
      | Promoted ->
        Lwt.return
          (Error (`Apply_error "Standby.rebase: cannot rebase a promoted standby"))
      | Following ->
        (* The application has overwritten the main DB (the pager passed to
           [create]) with a fresh, consistent base snapshot from the object
           store.  The standby's existing WAL index therefore describes a
           superseded state and must be DISCARDED — not drained, which would
           corrupt the fresh base.  [Wal.reset] drops the index and bumps the
           local WAL epoch. *)
        Wal.reset t.wal;
        t.last_epoch <- Wal.epoch t.wal;
        t.last_frame_idx <- -1;
        (* Replay the object-store WAL segments in order through the
           epoch-aware apply primitive, threading the position forward.  The
           segments may span several master epochs; [apply_frames_epoch_aware]
           checkpoints the local WAL at each transition (#209), so on
           completion the main DB holds the base plus all-but-the-last epoch
           and the WAL holds the live tail's epoch — exactly the steady-state
           a following standby maintains. *)
        let rec loop () =
          let* next = Lwt_stream.get segments in
          match next with
          | None -> Lwt.return (Ok { epoch = t.last_epoch; frame_idx = t.last_frame_idx })
          | Some frames ->
            let* r =
              Replication.apply_frames_epoch_aware
                ~wal:t.wal
                ~pager:t.pager
                ~last_epoch:t.last_epoch
                ~last_idx:t.last_frame_idx
                frames
            in
            (match r with
             | Error _ as e -> Lwt.return e
             | Ok (epoch, idx) ->
               t.last_epoch <- epoch;
               t.last_frame_idx <- idx;
               Store.set_follower_ack_position t.store;
               loop ())
        in
        loop ())
;;
