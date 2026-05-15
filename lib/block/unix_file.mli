(** Unix file-backed BLOCK implementation.
    Opens or creates a file. Takes an OS-level flock(LOCK_EX) on open
    to prevent concurrent access from two processes. *)

type t
type error = Io of string | Out_of_bounds of { page_id: int64; n_pages: int64 }

val pp_error : Format.formatter -> error -> unit
val page_size : int  (* 4096 *)

val open_  : path:string -> (t, error) result Lwt.t
val close  : t -> (unit, error) result Lwt.t

val n_pages    : t -> int64
val read_page  : t -> page_id:int64 -> Cstruct.t -> (unit, error) result Lwt.t
val write_page : t -> page_id:int64 -> Cstruct.t -> (unit, error) result Lwt.t
val sync       : t -> (unit, error) result Lwt.t
val resize     : t -> n_pages:int64 -> (unit, error) result Lwt.t
