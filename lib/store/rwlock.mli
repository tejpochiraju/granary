(** Writer-mutex + reader-counter for Lwt.

    Semantics (intentionally NOT a classic shared/exclusive RwLock):
    - Many readers run concurrently and never block.
    - Writers serialise against other writers (one at a time).
    - Readers do NOT block writers.  Writers do NOT block readers.

    Rationale: under snapshot isolation (see [Store.ro_snapshot] and
    [Pager.read ~snapshot_frames]) a reader's view at WAL frame N is
    invariant under a concurrent writer's appends, so reader/writer
    mutual exclusion is not needed for correctness.  This module only
    serialises writers and tracks active readers for the checkpoint
    coordinator.

    Re-entrancy is NOT supported — a fiber that holds the writer lock
    must not call [acquire_write] again. *)

type t

val create : unit -> t

(** [acquire_read] increments the reader counter and returns
    immediately.  Never blocks. *)
val acquire_read  : t -> unit Lwt.t

(** [release_read] decrements the counter; broadcasts when it reaches
    zero (used by the checkpoint coordinator). *)
val release_read  : t -> unit

(** [acquire_write] blocks while another writer is active.  Does NOT
    block on active readers. *)
val acquire_write : t -> unit Lwt.t

val release_write : t -> unit

(** Acquire shared (reader) access for the duration of [f]. *)
val with_read  : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t

(** Acquire exclusive (writer) access for the duration of [f]. *)
val with_write : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t

(** Number of currently-held read locks. *)
val readers : t -> int

(** True iff a writer is currently holding (or queued for) the lock.
    Diagnostic only — not used by [Store] for any control flow now
    that readers and writers don't mutually exclude. *)
val writer_pending : t -> bool

(** True iff a writer is currently holding the lock. *)
val writer_active : t -> bool
