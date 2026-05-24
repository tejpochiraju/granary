(** In-memory BLOCK device: a fixed-size, page-addressed buffer.

    Implements the same page-read/write/sync/resize interface as the Unix-file
    backend ([Sqlocaml_unix.Unix_file]) but keeps all pages in memory, for
    tests and ephemeral databases. [sync] is a no-op. *)

type t

type error =
  | Out_of_bounds of
      { page_id : int64
      ; n_pages : int64
      }

(** Pretty-print the device's page count. *)
val pp : Format.formatter -> t -> unit

(** Pretty-print an {!error}. *)
val pp_error : Format.formatter -> error -> unit

(** Page size in bytes (4096). *)
val page_size : int

(** [create ~n_pages] allocates a zero-filled device of [n_pages] pages. *)
val create : n_pages:int64 -> t

(** Current size of the device, in pages. *)
val n_pages : t -> int64

(** [read_page t ~page_id buf] copies page [page_id] into [buf]. *)
val read_page : t -> page_id:int64 -> Cstruct.t -> (unit, error) result Lwt.t

(** [write_page t ~page_id buf] stores [buf] as page [page_id]. *)
val write_page : t -> page_id:int64 -> Cstruct.t -> (unit, error) result Lwt.t

(** No-op for the in-memory backend (nothing to flush). *)
val sync : t -> (unit, error) result Lwt.t

(** [resize t ~n_pages] grows or shrinks the device to [n_pages] pages. *)
val resize : t -> n_pages:int64 -> (unit, error) result Lwt.t
