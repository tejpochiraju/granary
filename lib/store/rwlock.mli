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
