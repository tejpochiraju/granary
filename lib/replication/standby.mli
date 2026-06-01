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
    entering the apply loop; [promote] disables it. *)
val create
  :  store:Sqlocaml_store.Store.t
  -> pager:Sqlocaml_storage.Pager.t
  -> wal:Sqlocaml_storage.Wal.t
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

(** Current follower state. *)
val mode : t -> follower_mode

(** Last applied committed-frame position.  Used for lag/liveness
    tracking so the app/orchestrator can decide promotion readiness. *)
val acked_position : t -> acked_position

(** Pretty-print the standby's current mode and last acked position. *)
val pp : Format.formatter -> t -> unit
