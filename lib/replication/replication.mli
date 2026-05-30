(** WAL-based physical replication to object store (Litestream-style).

    This module defines the frame types carried on the replication stream,
    the asynchronous frame-sink interface that applications implement to
    ship frames to an object store, and the apply primitive that replays
    frames into local DB state for cold restore or a standby replica. *)

open Lwt.Syntax

type replicated_frame = {
  epoch: int64;
  frame_idx: int;
  page_id: int64;
  is_commit: bool;
  page: Cstruct.t;
}
(** A single frame as it travels on the replication stream.  The [epoch]
    identifies the WAL generation this frame belongs to; the consumer uses
    it to detect checkpoint/truncation boundaries and re-anchor. *)

type frame_sink = replicated_frame list -> unit Lwt.t
(** Application-implemented callback.  Receives a committed batch of
    frames in order.  The callback MUST be asynchronous and non-blocking
    so the commit path is never stalled. *)

val apply_frames
  :  wal:Sqlocaml_storage.Wal.t
  -> pager:Sqlocaml_storage.Pager.t
  -> replicated_frame list
  -> (unit, [> `Apply_error of string ]) result Lwt.t
(** Replay a batch of replicated frames into local DB state.

    Each frame's page is written via [Wal.append_commit], which
    recomputes the per-frame checksum using the local WAL's salt.
    Frame content integrity across the transport is verified
    independently by the caller (transport checksums).

    - Respects commit boundaries: frames up to and including the
      last [is_commit] frame form one append_commit batch.
      Trailing non-commit frames are silently ignored (the caller
      should ensure the input list ends at a commit boundary).
    - Updates the pager's logical page count via [Pager.set_n_pages]
      if any frame's [page_id] exceeds the current device capacity.
    - Returns [Ok ()] when all committed batches are applied. *)

val cold_restore
  :  read_at:(offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> write_at:(offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> sync:(unit -> (unit, string) result Lwt.t)
  -> wal_size_bytes:int64
  -> pager:Sqlocaml_storage.Pager.t
  -> base_snapshot_path:string
  -> wal_frames:(replicated_frame list) Lwt_stream.t
  -> unit
  -> (unit, [> `Restore_error of string ]) result Lwt.t
(** One-shot cold restore: open a WAL over [read_at]/[write_at]/[sync],
    then consume the [wal_frames] stream, accumulating frames into
    commit batches (up to each [is_commit] frame) and replaying them
    via {!apply_frames}.  The [base_snapshot_path] identifies the
    pre-downloaded base snapshot (application-specific; this module
    does not download snapshots itself). *)
