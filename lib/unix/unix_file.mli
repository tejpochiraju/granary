(** Unix file-backed BLOCK implementation.
    Opens or creates a file. Takes an OS-level flock(LOCK_EX) on open
    to prevent concurrent access from two processes. *)

type t

type error =
  | Io of string
  | Out_of_bounds of
      { page_id : int64
      ; n_pages : int64
      }

(** Pretty-print the file's page count. *)
val pp : Format.formatter -> t -> unit

(** Pretty-print an {!error}. *)
val pp_error : Format.formatter -> error -> unit

(** This file's page size in bytes (#95). *)
val page_size : t -> int

(** [set_page_size t ps] adopts page size [ps] for addressing and recomputes
    the logical page count from the file's byte size (#95).  Called by the open
    path after peeking the header's geometry on an existing file. *)
val set_page_size : t -> int -> unit

(** [open_ ?page_size ~path ()] opens (or creates) the file and takes an
    exclusive [flock].  [page_size] (default 4096, #95) sets the addressing
    granularity; for an existing file of a different geometry, open at the
    default then {!set_page_size} after peeking the header. *)
val open_ : ?page_size:int -> path:string -> unit -> (t, error) result Lwt.t

(** Release the lock and close the underlying file descriptor. *)
val close : t -> (unit, error) result Lwt.t

(** Current size of the file, in pages. *)
val n_pages : t -> int64

(** [read_page t ~page_id buf] reads page [page_id] into [buf]. *)
val read_page : t -> page_id:int64 -> Cstruct.t -> (unit, error) result Lwt.t

(** [write_page t ~page_id buf] writes [buf] to page [page_id]. *)
val write_page : t -> page_id:int64 -> Cstruct.t -> (unit, error) result Lwt.t

(** [fsync] the underlying file, durably persisting prior writes. *)
val sync : t -> (unit, error) result Lwt.t

(** [resize t ~n_pages] grows or shrinks the file to [n_pages] pages. *)
val resize : t -> n_pages:int64 -> (unit, error) result Lwt.t
