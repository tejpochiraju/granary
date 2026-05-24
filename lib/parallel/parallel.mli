(** Portable multi-core gate around OCaml 5 domains.

    Lets call sites offer a parallel implementation that is only taken when
    the runtime actually has more than one domain available, falling back to
    a sequential equivalent on monocore targets (notably Solo5/Mirage, where
    a second domain cannot be spawned). *)

(** [true] when the runtime recommends more than one domain — i.e. a
    multi-core Unix host. Always [false] on monocore Solo5/Mirage, where
    [Domain.spawn] cannot create a second domain. *)
val available : unit -> bool

(** [run ~parallel ~sequential] evaluates [parallel ()] when {!available},
    otherwise [sequential ()]. On Solo5 it always takes [sequential], so a
    [Domain.spawn] placed inside [parallel] is never reached. The two thunks
    must be observably equivalent. *)
val run : parallel:(unit -> 'a) -> sequential:(unit -> 'a) -> 'a
