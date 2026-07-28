(** Compile-time proof that the granary core can be consumed by a library that
    declares no [unix] / [lwt.unix] dependency (acceptance check for the
    platform-agnostic core, #170). *)

(** Open an in-memory database through the core's platform-agnostic API. *)
val probe : unit -> Granary.Db.t Lwt.t
