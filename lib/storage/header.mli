(** Alternating two-header commit protocol for crash safety.
    Pages 0 and 1 hold the two alternating file headers.
    The header with the higher txn_id and valid CRC is the live state. *)

type t =
  { txn_id : int64
  ; root_page : int64
  ; freelist_page : int64
  ; n_pages_total : int64
  ; schema_version : int64
  ; format_version : int32
    (** On-disk format version (#174).  Preserved across commits; only a fresh
        [init] stamps [current_format_version]. *)
  ; geom : Geometry.t
    (** Page geometry persisted in the header (#95): page_size + reserved bytes,
        fixed at creation, preserved verbatim across commits. *)
  }

(** On-disk format version this build writes for freshly-initialised
    databases.  v1 = pre-#174 (no fingerprints); v2 = #174 (schema
    fingerprints, redundant catalog mirror, per-page fingerprint stamp). *)
val current_format_version : int32

(** Highest on-disk format version this build can open.  [read_live] fails
    with [Unsupported_format] for any database stamped above this. *)
val max_supported_format_version : int32

type error =
  | Io of string
  | Both_headers_corrupt
  | Unsupported_format of int32 (** on-disk format_version exceeds support *)

(** Pretty-print all header fields. *)
val pp : Format.formatter -> t -> unit

(** Pretty-print an {!error}. *)
val pp_error : Format.formatter -> error -> unit

(** #95 bootstrap: discover a file's page geometry from the leading bytes of
    page 0 (page_size at byte 56, reserved at byte 64) WITHOUT verifying the
    CRC — the CRC covers the whole real page, whose size is what we're trying
    to learn.  Pass at least the first 68 bytes of page 0 (a default 4096-byte
    read suffices).  Returns [None] for a fresh/zeroed page or garbage, in which
    case the caller uses a supplied/default geometry.  Once the geometry is
    known, {!read_live} re-reads and CRC-verifies the full header pages. *)
val peek_geometry : Cstruct.t -> Geometry.t option

(** Read both headers (pages 0 and 1) from the pager.
    Pick the live one: the valid header (passes CRC) with the higher txn_id.
    If one is corrupt and the other valid, use the valid one.
    Returns Error Both_headers_corrupt if neither passes CRC. *)
val read_live : Pager.t -> (t, error) result Lwt.t

(** Commit a new header state. Writes to the INACTIVE header page
    (the one NOT used by prev_header), computes and seals CRC, then
    calls Pager.flush (which writes all dirty pages + syncs).
    The txn_id of the new header is prev_header.txn_id + 1.
    [new_state] provides root_page, freelist_page, n_pages_total, schema_version. *)
val commit : Pager.t -> prev_header:t -> new_state:t -> (unit, error) result Lwt.t

(** Like {!commit} but defers the device sync to a later
    [Pager.wal_sync].  Stages the next header into its inactive slot and
    pushes all dirty pages through [Pager.flush_no_sync]; group-commit
    callers coalesce the actual fsync across multiple writers.  On
    non-WAL backends this falls through to the regular sync flush, so it
    is safe to call regardless of backend. *)
val commit_no_sync : Pager.t -> prev_header:t -> new_state:t -> (unit, error) result Lwt.t

(** Initialise a brand-new empty file: write both header pages with txn_id=0
    and zeroed root/freelist. Called once on new file creation.
    After init, a subsequent read_live returns the zeroed header. *)
val init : Pager.t -> (unit, error) result Lwt.t
