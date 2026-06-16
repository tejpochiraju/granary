(** Engine-internal events surfaced to an optional observer (the internals
    monitor; #382).  Pure — depends on nothing in {!Sqlocaml_store.Store}, so
    {!Store} can depend on it without a cycle.  Events are fire-and-forget; the
    library never blocks on an observer. *)

type t =
  | Txn_begin of { txn_id : int64 }
  | Txn_commit of
      { txn_id : int64
      ; frames : int
      }
  | Txn_rollback of { txn_id : int64 }
  | Savepoint_begin of
      { txn_id : int64
      ; name : string
      }
  | Savepoint_release of
      { txn_id : int64
      ; name : string
      }
  | Savepoint_rollback of
      { txn_id : int64
      ; name : string
      }
  | Wal_append of
      { txn_id : int64
      ; base_idx : int
      ; count : int
      }
  | Wal_reset of { epoch : int64 }
  | Checkpoint_begin of { target_frames : int }
  | Checkpoint_end of { pages_migrated : int }

(** Short uppercase tag for display, e.g. ["COMMIT"], ["WAL_APPEND"]. *)
val label : t -> string

(** The transaction id an event belongs to, or [None] for events with no
    single owning txn ([Wal_reset], [Checkpoint_*]).  Used by the monitor's
    txn-id filter. *)
val txn_id : t -> int64 option

(** Human-readable one-line rendering of an event. *)
val pp : Format.formatter -> t -> unit
