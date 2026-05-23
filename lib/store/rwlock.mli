(** Lwt-friendly shared / exclusive lock.

    Many concurrent readers are allowed.  Writers are exclusive of
    other writers AND of readers.  Writer-priority: a pending writer
    prevents new readers from acquiring, preventing writer starvation
    under a steady reader stream.

    Re-entrancy is NOT supported — recursive acquire deadlocks. *)

type t

val create : unit -> t

val acquire_read  : t -> unit Lwt.t
val release_read  : t -> unit
val acquire_write : t -> unit Lwt.t
val release_write : t -> unit

(** Acquire shared (reader) access for the duration of [f]. *)
val with_read  : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t

(** Acquire exclusive (writer) access for the duration of [f]. *)
val with_write : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t

(** Number of currently-held read locks. *)
val readers : t -> int

(** True iff a writer is currently holding (or queued for) the lock. *)
val writer_pending : t -> bool

(** True iff a writer is currently holding the lock (exclusive lock is
    active).  Unlike [writer_pending], this is false when a writer is
    only waiting (queued) but has not yet acquired.  Used by
    [Store.ro_begin] to bypass [acquire_read] when a writer is already
    active: this covers both (a) intra-fiber re-entrancy (the writer's
    own code path calls [ro_begin], where [acquire_read] would deadlock)
    and (b) cross-fiber yield scenarios (another fiber's reader arriving
    while the writer is paused mid-txn).  Bypass is safe in both cases
    because RO snapshot reads consult [Wal.find_page_at ~max_frame] —
    the writer's in-flight dirty pages are invisible to a snapshot. *)
val writer_active : t -> bool
