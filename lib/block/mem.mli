type t
type error =
  | Out_of_bounds of { page_id : int64; n_pages : int64 }

val pp_error : Format.formatter -> error -> unit
val page_size : int

val create : n_pages:int64 -> t

val n_pages    : t -> int64
val read_page  : t -> page_id:int64 -> Cstruct.t -> (unit, error) result Lwt.t
val write_page : t -> page_id:int64 -> Cstruct.t -> (unit, error) result Lwt.t
val sync       : t -> (unit, error) result Lwt.t
val resize     : t -> n_pages:int64 -> (unit, error) result Lwt.t
