(** Standby follower driver (#172).

    Consumes a stream of replicated frames from the master and applies
    them continuously to a local store.  The standby is in follower mode
    (write transactions are rejected) while following and can be promoted
    to accept local writes after master failure.

    The caller (the application/orchestrator) owns transport and the
    decision to promote; this module provides the storage-level apply
    loop, promotion handshake, and lag tracking. *)

type follower_mode =
  | Following
  | Promoted

type acked_position =
  { epoch : int64
  ; frame_idx : int
  }

type t

(** Create a new standby follower wrapping the given store, pager, and
    WAL.  [start_following] enables follower mode on the store before
    entering the apply loop; [promote] disables it.

    [~on_standby_ack] is an optional callback invoked after each
    successful apply batch with the standby's local
    [Wal.committed_frames] count.  The application typically uses it to
    advance the master's replication floor via
    {!Sqlocaml_store.Store.update_replication_position}, so the master's
    checkpoint gate reflects the standby's true applied position.  No-op
    when omitted. *)
val create
  :  store:Sqlocaml_store.Store.t
  -> pager:Sqlocaml_storage.Pager.t
  -> wal:Sqlocaml_storage.Wal.t
  -> ?on_standby_ack:(int -> unit)
  -> unit
  -> t

(** Enter the follower loop: consume frames from [stream] and apply them
    via the epoch-aware apply primitive.  Returns only on stream end
    (Ok) or on error (Apply_error).  Enables follower mode on the store
    before entering the loop and disables it on exit (unless explicitly
    promoted). *)
val start_following
  :  t
  -> Replication.replicated_frame list Lwt_stream.t
  -> (unit, [> `Apply_error of string ]) result Lwt.t

(** Stop following and promote to accept local writes.  Waits for any
    in-flight apply to finish, then drains buffered committed frames to
    the main DB (checkpoint), which also bumps the local WAL epoch via
    [Wal.reset] so locally-generated writes start with a fresh epoch
    distinct from the master's; finally disables follower mode and marks
    the standby as promoted.

    Draining is required for correctness: [apply_frames] writes only to
    the WAL, so without it the fresh engine view opened on promotion
    (which recovers from the main DB) would lose every frame applied
    since the last epoch-change checkpoint.

    Idempotent: a second call after promotion is a no-op.  Fails the
    promise (without promoting) if the drain checkpoint errors. *)
val promote : t -> unit Lwt.t

(** Re-base a fallen-behind standby from the #92 object store (#208).

    When the master checkpoints past a slow or dead standby (see the
    checkpoint-gate timeout, #207), the frames the standby still needs have
    been recycled out of the master's live WAL window.  The standby must
    rebuild from the object store: the application loads a consistent base
    snapshot into the standby's main DB (the pager passed to {!create}) —
    exactly {!Replication.cold_restore}'s [base_snapshot_path] contract —
    then hands this driver [segments], the object-store WAL frame batches to
    replay on top, in order.

    [rebase] is the standby-side equivalent of {!Replication.cold_restore},
    reusing the same epoch-aware apply path as the live loop.  It first
    discards the standby's stale WAL ([Wal.reset]) — the loaded base
    supersedes it — then replays [segments], which may span several master
    epochs.  It returns the resulting {!acked_position} (the last replayed
    committed frame) so the caller can resume the live tail via
    {!start_following} from exactly that point, applying only frames beyond
    it and thereby never double-applying what the re-base already
    materialized.

    Must be called while in [Following] mode and not concurrently with an
    active follower loop (the typical flow stops following, re-bases, then
    starts following a fresh live stream).  Returns [Apply_error] if the
    standby has been promoted or if replaying a segment fails.

    Detecting the gap and fetching the base snapshot and segments from the
    object store are the application's responsibility — transport and the
    object store are app-owned. *)
val rebase
  :  t
  -> Replication.replicated_frame list Lwt_stream.t
  -> (acked_position, [> `Apply_error of string ]) result Lwt.t

(** Current follower state. *)
val mode : t -> follower_mode

(** Last applied committed-frame position.  Used for lag/liveness
    tracking so the app/orchestrator can decide promotion readiness. *)
val acked_position : t -> acked_position

(** Pretty-print the standby's current mode and last acked position. *)
val pp : Format.formatter -> t -> unit
