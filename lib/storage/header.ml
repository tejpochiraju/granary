(** Alternating two-header commit protocol.

    Pages 0 and 1 are the two header pages. On each commit, the "inactive"
    header page is overwritten with the new state and the pager is flushed
    (fsync). The header with the higher txn_id and valid CRC is live. *)

type t =
  { txn_id : int64
  ; root_page : int64
  ; freelist_page : int64
  ; n_pages_total : int64
  ; schema_version : int64
  ; format_version : int32
    (** On-disk format version (#174).  Preserved across commits; only a fresh
        [init] stamps [current_format_version].  Opening a header whose version
        exceeds [max_supported_format_version] fails with [Unsupported_format]. *)
  ; geom : Geometry.t
    (** Page geometry persisted in the header (#95): page_size at byte 56,
        reserved_bytes_per_page at byte 64.  Chosen at creation, immutable
        thereafter, preserved verbatim across commits. *)
  }

(* On-disk format versions:
   - v1: original layout, no schema fingerprints / mirror / page stamps.
   - v2: #174 — schema fingerprints, redundant catalog mirror, per-page
     fingerprint stamp in the reserved header bytes. *)
let current_format_version = 2l
let max_supported_format_version = 2l

type error =
  | Io of string
  | Both_headers_corrupt
  | Unsupported_format of int32

let pp_error fmt = function
  | Io msg -> Format.fprintf fmt "Io: %s" msg
  | Both_headers_corrupt -> Format.pp_print_string fmt "Both_headers_corrupt"
  | Unsupported_format v -> Format.fprintf fmt "Unsupported_format: %ld" v
;;

let pp fmt t =
  Format.fprintf
    fmt
    "@[<hv>{ txn_id = %Ld;@ root_page = %Ld;@ freelist_page = %Ld;@ n_pages_total = \
     %Ld;@ schema_version = %Ld }@]"
    t.txn_id
    t.root_page
    t.freelist_page
    t.n_pages_total
    t.schema_version
;;

(* Convert a Pager error to our error type. *)
let of_pager_err e = Io (Format.asprintf "%a" Pager.pp_error e)

(* Build a sealed header page buffer.  Sized to the file's geometry (#95). *)
let build_page (h : t) =
  let buf = Cstruct.create h.geom.page_size in
  Page.write_common
    buf
    { Page.kind = Page.Header; flags = 0; n_keys = 0; right_page = 0l; crc32 = 0l };
  Page.write_header_fields
    buf
    { Page.txn_id = h.txn_id
    ; root_page = h.root_page
    ; freelist_page = h.freelist_page
    ; n_pages_total = h.n_pages_total
    ; schema_version = h.schema_version
    ; page_size = Int32.of_int h.geom.page_size
    ; format_version = h.format_version
    ; reserved_bytes_per_page = Int32.of_int h.geom.reserved_bytes_per_page
    };
  Page.seal buf;
  buf
;;

(* Try to decode a header from a raw page buffer.
   Returns [Some t] when the page is a valid Header page with correct CRC,
   [None] otherwise. *)
let decode_page buf =
  if not (Page.verify_crc buf)
  then None
  else (
    match Page.read_common buf with
    | exception _ -> None
    | c ->
      if c.Page.kind <> Page.Header
      then None
      else (
        let f = Page.read_header_fields buf in
        (* Reconstruct the persisted geometry (#95).  A stored geometry that
           fails validation is treated as corruption — the header is rejected. *)
        match
          Geometry.create
            ~page_size:(Int32.to_int f.Page.page_size)
            ~reserved_bytes_per_page:(Int32.to_int f.Page.reserved_bytes_per_page)
        with
        | Error _ -> None
        | Ok geom ->
          Some
            { txn_id = f.Page.txn_id
            ; root_page = f.Page.root_page
            ; freelist_page = f.Page.freelist_page
            ; n_pages_total = f.Page.n_pages_total
            ; schema_version = f.Page.schema_version
            ; format_version = f.Page.format_version
            ; geom
            }))
;;

(* ------------------------------------------------------------------ *)
(* Public interface                                                     *)
(* ------------------------------------------------------------------ *)

let read_live pager =
  let%lwt r0 = Pager.read pager 0L in
  let%lwt r1 = Pager.read pager 1L in
  let h0 =
    match r0 with
    | Error _ -> None
    | Ok buf -> decode_page buf
  in
  let h1 =
    match r1 with
    | Error _ -> None
    | Ok buf -> decode_page buf
  in
  let chosen =
    match h0, h1 with
    | None, None -> Error Both_headers_corrupt
    | Some h, None -> Ok h
    | None, Some h -> Ok h
    | Some a, Some b ->
      (* Both valid — pick the one with the higher txn_id.
         In case of a tie (e.g. right after init) prefer page 0 (a). *)
      if Int64.compare b.txn_id a.txn_id > 0 then Ok b else Ok a
  in
  Lwt.return
    (match chosen with
     | (Error _ : (t, error) result) as e -> e
     | Ok h ->
       (* Forward-compatibility gate (#174): refuse a database written by a
          newer binary rather than misreading its layout. *)
       if Int32.compare h.format_version max_supported_format_version > 0
       then Error (Unsupported_format h.format_version)
       else Ok h)
;;

(* #95 bootstrap: learn a file's geometry from the leading bytes of page 0
   WITHOUT verifying the CRC — the CRC covers the whole real page, whose size
   we don't yet know.  The page_size/reserved fields live at bytes 56/64, always
   within the first 4096 bytes regardless of the true page size, so a single
   default-geometry read of page 0 is enough to discover them.  Returns [None]
   for a zeroed/fresh page (page_size 0 fails validation) or for garbage; the
   caller then falls back to a caller-supplied or default geometry, and the
   subsequent full {!read_live} CRC-verifies under the chosen geometry. *)
let peek_geometry (first_bytes : Cstruct.t) : Geometry.t option =
  if Cstruct.length first_bytes < 68
  then None
  else (
    let page_size = Int32.to_int (Cstruct.BE.get_uint32 first_bytes 56) in
    let reserved = Int32.to_int (Cstruct.BE.get_uint32 first_bytes 64) in
    match Geometry.create ~page_size ~reserved_bytes_per_page:reserved with
    | Ok g -> Some g
    | Error _ -> None)
;;

(* Internal: write the next header into the inactive page slot.  Returns
   the prepared header value (so callers can mirror it into in-memory
   state).  Does NOT flush — callers choose [Pager.flush] (full sync) or
   [Pager.flush_no_sync] (group commit). *)
let stage_next_header pager ~prev_header ~new_state =
  let inactive_page = Int64.to_int (Int64.rem (Int64.add prev_header.txn_id 1L) 2L) in
  let next_txn_id = Int64.add prev_header.txn_id 1L in
  let h = { new_state with txn_id = next_txn_id } in
  let buf = build_page h in
  Pager.write pager (Int64.of_int inactive_page) buf;
  h
;;

let commit pager ~prev_header ~new_state =
  let _ = stage_next_header pager ~prev_header ~new_state in
  match%lwt Pager.flush pager with
  | Error e -> Lwt.return (Error (of_pager_err e))
  | Ok () -> Lwt.return (Ok ())
;;

let commit_no_sync pager ~prev_header ~new_state =
  let _ = stage_next_header pager ~prev_header ~new_state in
  match%lwt Pager.flush_no_sync pager with
  | Error e -> Lwt.return (Error (of_pager_err e))
  | Ok () -> Lwt.return (Ok ())
;;

let init pager =
  let zero =
    { txn_id = 0L
    ; root_page = 0L
    ; freelist_page = 0L
    ; n_pages_total = 0L
    ; schema_version = 0L
    ; format_version = current_format_version
    ; geom = Pager.geom pager
    }
  in
  let buf0 = build_page zero in
  let buf1 = build_page zero in
  Pager.write pager 0L buf0;
  Pager.write pager 1L buf1;
  match%lwt Pager.flush pager with
  | Error e -> Lwt.return (Error (of_pager_err e))
  | Ok () -> Lwt.return (Ok ())
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
