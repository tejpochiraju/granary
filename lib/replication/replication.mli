(** WAL-based physical replication to object store (Litestream-style).

    This module defines the frame types carried on the replication stream,
    the asynchronous frame-sink interface that applications implement to
    ship frames to an object store, and the apply primitive that replays
    frames into local DB state for cold restore or a standby replica. *)

(** A single frame as it travels on the replication stream.  The [epoch]
    identifies the WAL generation this frame belongs to; the consumer uses
    it to detect checkpoint/truncation boundaries and re-anchor.

    [checksum], [source_salt], and [source_seed] store the transport-level
    checksum computed at the replication source.  Callers can verify
    transport integrity with {!verify_checksum}. *)
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

(** Application-implemented callback.  Receives a committed batch of
    frames in order.  The callback MUST be asynchronous and non-blocking
    so the commit path is never stalled. *)
type frame_sink = replicated_frame list -> unit Lwt.t

(** Recompute the frame checksum using [source_salt] and [source_seed] and
    compare against the stored [checksum].  Returns [true] if the frame was
    not corrupted in transit. *)
val verify_checksum : replicated_frame -> bool

(** Replay a batch of replicated frames into local DB state.

    Each frame's page is written via [Wal.append_commit], which
    recomputes the per-frame checksum using the local WAL's salt.
    Frame content integrity across the transport is verified
    independently by the caller (transport checksums).

    - Respects commit boundaries: frames up to and including the
      last [is_commit] frame form one append_commit batch.
      Trailing non-commit frames are silently ignored (the caller
      should ensure the input list ends at a commit boundary).
    - Grows the device via [Pager.set_n_pages] to accommodate the
      highest [page_id] in the batch (page ids are 0-indexed, so
      a frame for page N needs at least N+1 pages).
    - Returns [Ok ()] when all committed batches are applied.

    Note: this initial implementation does not inspect [epoch] for
    re-anchoring; it assumes all frames belong to a single WAL
    generation.  Epoch-aware apply (with re-anchor from a fresh
    base snapshot) is deferred to the standby-apply driver in #172. *)
val apply_frames
  :  wal:Sqlocaml_storage.Wal.t
  -> pager:Sqlocaml_storage.Pager.t
  -> replicated_frame list
  -> (unit, [> `Apply_error of string ]) result Lwt.t

(** One-shot cold restore: open a WAL over [read_at]/[write_at]/[sync],
    then consume the [wal_frames] stream, accumulating frames into
    commit batches (up to each [is_commit] frame) and replaying them
    via {!apply_frames}.

    [base_snapshot_path] is informational only — the caller must
    have already loaded the base snapshot into the [pager] before
    calling this function.  This module does not download or apply
    snapshots itself; it only replays WAL frames on top of the
    already-prepared pager state. *)
val cold_restore
  :  read_at:(offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> write_at:(offset:int64 -> Cstruct.t -> (unit, string) result Lwt.t)
  -> sync:(unit -> (unit, string) result Lwt.t)
  -> wal_size_bytes:int64
  -> pager:Sqlocaml_storage.Pager.t
  -> base_snapshot_path:string
  -> wal_frames:replicated_frame list Lwt_stream.t
  -> unit
  -> (unit, [> `Restore_error of string ]) result Lwt.t
