val available : unit -> bool
(** [true] when the runtime recommends more than one domain — i.e. a
    multi-core Unix host. Always [false] on monocore Solo5/Mirage, where
    [Domain.spawn] cannot create a second domain. *)

val run : parallel:(unit -> 'a) -> sequential:(unit -> 'a) -> 'a
(** [run ~parallel ~sequential] evaluates [parallel ()] when {!available},
    otherwise [sequential ()]. On Solo5 it always takes [sequential], so a
    [Domain.spawn] placed inside [parallel] is never reached. The two thunks
    must be observably equivalent. *)
