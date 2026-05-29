(** Offline, best-effort salvage of a damaged database file into a fresh,
    structurally-valid one — the corruption-recovery counterpart to the
    crash-recovery already performed on open.

    Read-only on the source; emits a clean new file rather than repairing
    in place.  Implementation phases:

    1. [scan_raw]: Raw-page scan verifying CRC32 on every page and
       classifying by kind byte.  Provides a trust map used later.
    2. [open_source]: Attempt to open the damaged file via Store.open_block
       (handles crash recovery / WAL replay automatically).  Load the catalog.
    3. [extract]: Walk each table's tree via cursor, skip corrupt pages
       (flagged by the scan trust map), decode rows.
    4. [rebuild]: Open a fresh Store and replay recovered rows through the
       normal put path, producing correct CRCs in a clean CoW B+-tree.

    When the source headers are intact (the common case), phases 2–3 use
    the existing Store/Catalog infrastructure.  When headers are corrupt,
    the tool falls back to raw-page catalog reconstruction from the
    redundant mirror (#174). *)

(** A single page from the raw scan, annotated with CRC status
    and decoded page kind. *)
type scanned_page = private
  { page_id : int64
  ; kind : Sqlocaml_storage.Page.kind
  ; crc_valid : bool
  ; tag : int32
  }

(** Pretty-print a one-line summary for a scanned page. *)
val pp_scanned_page : Format.formatter -> scanned_page -> unit

(** Result of the raw-page scan phase. *)
type scan_result =
  { n_pages : int64
  ; pages : scanned_page list
  ; trusted_count : int
  ; damaged_count : int
  }

(** [scan ~read_page ~n_pages] reads every page [0..n_pages-1] from the
    source, verifies its CRC32, decodes the kind byte, and returns the
    annotated list.  Pages that fail to read are silently skipped. *)
val scan
  :  read_page:(int64 -> Cstruct.t option Lwt.t)
  -> n_pages:int64
  -> scan_result Lwt.t

(** A row recovered and ready to write to the output store. *)
type recovered_row =
  { tree_id : int
  ; key : bytes
  ; value : bytes
  }

(** Final result of a complete recovery run. *)
type recover_result =
  { rows_recovered : int
  ; pages_scanned : int64
  ; pages_trusted : int
  ; pages_damaged : int
  ; pages_skipped : int
  ; tables_recovered : int
  ; tables_total : int
  }

(** Pretty-print the recovery summary. *)
val pp_result : Format.formatter -> recover_result -> unit

(** [recover ~read_page ~n_pages ~open_source ~write_to] runs the full
    recovery pipeline. *)
val recover
  :  read_page:(int64 -> Cstruct.t option Lwt.t)
  -> n_pages:int64
  -> open_source:(unit -> (Sqlocaml_store.Store.t * Sqlocaml_catalog.Catalog.t, string) result Lwt.t)
  -> write_to:(recovered_row list -> (unit, string) result Lwt.t)
  -> (recover_result, string) result Lwt.t
