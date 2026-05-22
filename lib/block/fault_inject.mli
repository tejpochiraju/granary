(** Fault-injecting wrapper around the [Unix_file] block backend.

    Wraps the standard block callbacks; the [fail_after_writes] counter
    causes the (N+1)-th [write_page] call to return [Error "injected
    crash"].  After failure all subsequent writes also fail (simulating
    a downed device).  Optionally, [fail_on_sync] forces [sync] to fail
    on first invocation.

    Recovery correctness (file consistency after a mid-write crash) is
    NOT in scope for this module — it merely exposes the injection
    mechanics so that test harnesses can verify the engine does not
    panic in the face of I/O errors. *)

type t

type config = {
  fail_after_writes : int option;
  fail_on_sync      : bool;
}

(** Default: no faults injected. *)
val default_config : config

(** Build a fault-injecting block backend wrapping a file at [path].
    The file is created if absent and resized to [size_bytes] bytes.
    [config] controls when faults trigger.  Returns the wrapper handle
    along with the callbacks expected by [Db.open_block].

    The [close] callback returns [unit Lwt.t] to match [Db.open_block]'s
    signature: any error from the underlying [Unix_file.close] is
    swallowed (logged-and-discarded) at the wrapper boundary. *)
val open_with_faults :
  path:string ->
  size_bytes:int ->
  config:config ->
  ( t *
    (page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t) *  (* read_page *)
    (page_id:int64 -> Cstruct.t -> (unit, string) result Lwt.t) *  (* write_page *)
    (unit -> (unit, string) result Lwt.t) *                        (* sync *)
    (n_pages:int64 -> (unit, string) result Lwt.t) *               (* resize *)
    int64 *                                                        (* n_pages *)
    (unit -> unit Lwt.t)                                           (* close *)
  ) Lwt.t

(** Number of successful underlying writes observed by this wrapper. *)
val writes_completed : t -> int

(** Whether the injected fault has tripped. *)
val faulted : t -> bool
