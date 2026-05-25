(** Page geometry (#95): the per-file choice of [page_size] and
    [reserved_bytes_per_page], fixed at file creation and persisted in the
    header.  Addressing is [offset = page_id * page_size], so the size is
    uniform across a file and immutable once chosen.

    This is a cheap, pure value carried on the pager/store context; the derived
    sizes below are plain field arithmetic, safe to call on every page op. *)

type t =
  { page_size : int (** total bytes per page on disk; a multiple of 4096 *)
  ; reserved_bytes_per_page : int
    (** fixed bytes carved off the END of every page (default 0); subtractive
        overhead reserved for future fixed-size per-page metadata such as a
        per-page AEAD tag for at-rest encryption (#84).  Does not enable
        page-level compression. *)
  }

(** Pretty-print a geometry as [{ page_size = ...; reserved_bytes_per_page = ... }]. *)
val pp : Format.formatter -> t -> unit

(** 16 — the common page header, excluded from usable payload. *)
val header_size : int

(** 12 — bytes per freelist entry. *)
val freelist_entry_size : int

(** The historical fixed geometry: 4096-byte page, no reserved bytes.  Used as
    the default at creation and as the bootstrap geometry while peeking a
    header to learn a file's real page size. *)
val default : t

(** Usable data-area bytes: [page_size - header_size - reserved_bytes_per_page]. *)
val max_data_bytes : t -> int

(** Maximum payload an overflow page can hold: [max_data_bytes - 2] (2 bytes
    for the payload-length field). *)
val max_overflow_payload_bytes : t -> int

(** Freelist entries that fit on one freelist page: [max_data_bytes / 12]. *)
val max_freelist_entries_per_page : t -> int

(** A creation-time geometry-validation failure. *)
type error =
  | Bad_page_size of int (** not a multiple of 4096, or outside [4096, 65536] *)
  | Bad_reserved of int (** negative, or so large it leaves too little payload *)

(** Pretty-print a validation {!error}. *)
val pp_error : Format.formatter -> error -> unit

(** Validate and build a geometry.  [page_size] must be a multiple of 4096 in
    [4096, 65536]; [reserved_bytes_per_page] must be >= 0 and leave a usable
    payload of at least 480 bytes.  Returns [Error] otherwise. *)
val create : page_size:int -> reserved_bytes_per_page:int -> (t, error) result
