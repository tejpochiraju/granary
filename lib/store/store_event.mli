(** Engine-internal events surfaced to an optional observer (the internals
    monitor; #382).  Pure — depends on nothing in {!Granary_store.Store}, so
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
  (* #384: page-level physical-I/O events.  [txn_id] is stamped by
     [Store.set_event_callback] from [Pager.get_txn_id] (the underlying
     [Pager_event.t] carries only the page id).  It is exact for write-path
     events fired inside an active RW txn ([Page_alloc]/[Page_free], and
     [Page_write] on commit-flush).  It is best-effort otherwise: [Page_read]
     outside a write txn, and [Page_write] emitted during checkpoint /
     replication / recovery (no owning txn), carry the most-recent committed
     txn id rather than an owning one — and are NOT bracketed by
     [Txn_begin]/[Txn_commit]. *)
  | Page_read of
      { txn_id : int64
      ; tree : int
        (** [tree] is the store tree id whose operation triggered the
                       I/O; [-1] when no op context was active (best-effort for
                       [Page_write], which is stamped at WAL-flush time with the
                       most recently active tree). *)
      ; page : int64
      }
  | Wal_read of
      { txn_id : int64
      ; tree : int
        (** [tree] is the store tree id whose operation triggered the WAL-served
                       read (#392); [-1] when no op context was active.  Same
                       best-effort [txn_id]/[tree] stamping as [Page_read]. *)
      ; page : int64
      }
  | Page_write of
      { txn_id : int64
      ; tree : int
        (** [tree] is the store tree id whose operation triggered the
                       I/O; [-1] when no op context was active (best-effort for
                       [Page_write], which is stamped at WAL-flush time with the
                       most recently active tree). *)
      ; page : int64
      }
  | Page_alloc of
      { txn_id : int64
      ; tree : int
        (** [tree] is the store tree id whose operation triggered the
                       I/O; [-1] when no op context was active (best-effort for
                       [Page_write], which is stamped at WAL-flush time with the
                       most recently active tree). *)
      ; page : int64
      ; reused : bool
      }
  | Page_free of
      { txn_id : int64
      ; tree : int
        (** [tree] is the store tree id whose operation triggered the
                       I/O; [-1] when no op context was active (best-effort for
                       [Page_write], which is stamped at WAL-flush time with the
                       most recently active tree). *)
      ; page : int64
      }

(** Short uppercase tag for display, e.g. ["COMMIT"], ["WAL_APPEND"]. *)
val label : t -> string

(** The transaction id an event belongs to, or [None] for events with no
    single owning txn ([Wal_reset], [Checkpoint_*]).  Used by the monitor's
    txn-id filter. *)
val txn_id : t -> int64 option

(** [tree_id_of ev] is the store tree id for the page/WAL-read events
    ([Page_read]/[Wal_read]/[Page_write]/[Page_alloc]/[Page_free]), or [None]
    for every other event. [-1] means "no active tree context". *)
val tree_id_of : t -> int option

(** Human-readable one-line rendering of an event. *)
val pp : Format.formatter -> t -> unit
