(** Alternating two-header commit protocol.

    Pages 0 and 1 are the two header pages. On each commit, the "inactive"
    header page is overwritten with the new state and the pager is flushed
    (fsync). The header with the higher txn_id and valid CRC is live. *)

type encryption =
  { canary_nonce : string
  ; canary_tag : string
  }

let enc_magic_value = 0x53454E43l

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
  ; enc : encryption option
    (** Encryption marker and key-check canary (#84).  [None] for plaintext
        databases; [Some _] when enc_magic = 0x53454E43 is set on disk. *)
  }

(* On-disk format versions:
   - v1: original layout, no schema fingerprints / mirror / page stamps.
   - v2: #174 — schema fingerprints, redundant catalog mirror, per-page
     fingerprint stamp in the reserved header bytes.
   - v3: #578 (PR #690) — [Index_key.encode_value]'s type-tag bytes were
     renumbered so NaN gets its own tag [0x01], distinct from NULL's [0x00]
     (previously byte-identical, which spuriously conflicted a NaN insert
     against a pre-existing NULL under a UNIQUE index).  INTEGER/REAL/TEXT/BLOB
     shifted from [0x01]-[0x04] to [0x02]-[0x05].  Every index key ever written
     by a v2-or-older binary is therefore misread by a v3 decoder: the old
     INTEGER tag byte [0x01] is now read as the NaN arm, which (unlike every
     other arm) advances the decode offset past only the 1-byte tag rather than
     the 8 payload bytes the old encoding actually wrote there, desyncing every
     later field in that key.  There is no migration path, so v3 refuses to
     open anything older (see [min_supported_format_version]) rather than risk
     silently misdecoding an index.
   - v4: #754 — [Index_key.encode_value]'s [IK_real] arm now normalizes
     [-0.0]'s bit pattern to [+0.0]'s before the order-preserving transform, so
     the two encode IDENTICALLY rather than [-0.0] sorting strictly below
     [+0.0].  Unlike v3, this is not a tag-byte desync: the field WIDTH and
     every OTHER float's bytes are unchanged, so a v3 index over a REAL column
     that never stored a [-0.0] decodes perfectly well under a v4 reader.  The
     one row that decodes wrong is a stored [-0.0] itself: its v3 index key
     sits under the OLD bytes ([0x7FFF...], sign-flip-then-lognot), and a
     v4-built seek for [0.0] constructs the NEW bytes ([0x8000...],
     sign-flip-only) — the key simply is not found, silently, exactly the
     seek-misses-a-row bug #754 fixes for FRESH data reintroduced for a v3
     FILE opened by a v4 binary.  There is no per-row way to tell, on open,
     whether a v3 file's indexes hold a [-0.0] without a full index scan, and
     no migration/reindex-on-open mechanism exists to fall back to, so v4
     refuses to open a v3-or-older file outright rather than risk it — the
     same call v3 made for its own encoding change. *)
let current_format_version = 4l
let max_supported_format_version = 4l

(* Lowest on-disk format version this build can open.  Set equal to
   [current_format_version]: both v3's index-key tag renumbering and v4's
   [-0.0] key normalization (see above) are silent, garbage-producing-or-row-
   losing incompatibilities for a pre-existing index, and there is no
   reindex-on-open mechanism in this codebase to fall back to (verified:
   nothing between v1 and v2 ever migrated in place either — a v1 file just
   kept being read/written as v1).  Refusing outright is the "loud, not
   silent" pattern this project already uses for #634 (VACUUM staleness) and
   #598 (ATTACH routing). *)
let min_supported_format_version = 4l

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
    ; enc_magic =
        (match h.enc with
         | Some _ -> enc_magic_value
         | None -> 0l)
    ; canary_nonce =
        (match h.enc with
         | Some e -> e.canary_nonce
         | None -> String.make 16 '\000')
    ; canary_tag =
        (match h.enc with
         | Some e -> e.canary_tag
         | None -> String.make 16 '\000')
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
          let enc =
            if Int32.equal f.Page.enc_magic enc_magic_value
            then
              Some { canary_nonce = f.Page.canary_nonce; canary_tag = f.Page.canary_tag }
            else None
          in
          Some
            { txn_id = f.Page.txn_id
            ; root_page = f.Page.root_page
            ; freelist_page = f.Page.freelist_page
            ; n_pages_total = f.Page.n_pages_total
            ; schema_version = f.Page.schema_version
            ; format_version = f.Page.format_version
            ; geom
            ; enc
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
          newer binary rather than misreading its layout.  Backward gate
          (#578/#690): refuse a database written by an older binary whose
          index-key tag layout this decoder no longer agrees with, rather than
          silently desyncing every key past the first NaN/INTEGER/REAL/TEXT/BLOB
          tag it decodes (see [min_supported_format_version]). *)
       if
         Int32.compare h.format_version max_supported_format_version > 0
         || Int32.compare h.format_version min_supported_format_version < 0
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

let init ?(enc = None) pager =
  let zero =
    { txn_id = 0L
    ; root_page = 0L
    ; freelist_page = 0L
    ; n_pages_total = 0L
    ; schema_version = 0L
    ; format_version = current_format_version
    ; geom = Pager.geom pager
    ; enc
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
