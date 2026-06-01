(** Block I/O abstraction. Backends implement [S]. *)

module type S = sig
  type t
  type error

  val pp_error : Format.formatter -> error -> unit

  (** This device's page size in bytes (#95): a per-file choice fixed at
      creation, no longer a compile-time constant. *)
  val page_size : t -> int

  val n_pages : t -> int64
  val read_page : t -> page_id:int64 -> Cstruct.t -> (unit, error) result Lwt.t
  val write_page : t -> page_id:int64 -> Cstruct.t -> (unit, error) result Lwt.t
  val sync : t -> (unit, error) result Lwt.t
  val resize : t -> n_pages:int64 -> (unit, error) result Lwt.t
end
