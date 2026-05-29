module S = Sqlocaml_store.Store
module Row = Sqlocaml_encoding.Row
module Varint = Sqlocaml_encoding.Varint
module Schema_fingerprint = Sqlocaml_encoding.Schema_fingerprint
module Rowid = Sqlocaml_encoding.Rowid

type fk_action =
  | FA_no_action
  | FA_restrict
  | FA_cascade
  | FA_set_null
  | FA_set_default

(* System tree IDs *)
let sys_tables_tid : S.tree_id = 0
let sys_columns_tid : S.tree_id = 1
let sys_indexes_tid : S.tree_id = 2
let sys_meta_tid : S.tree_id = 3
let sys_fts_tid : S.tree_id = 4
let sys_views_tid : S.tree_id = 5
let sys_triggers_tid : S.tree_id = 6

(* #174: redundant catalog mirror.  A second, self-describing copy of every
   table's schema keyed by tree_id, so a single damaged primary-catalog page
   does not lose the schema for every table.  Also the canonical
   reference-fingerprint store used for drift detection on open. *)
let sys_mirror_tid : S.tree_id = 7

(* Rowid counter key suffix for FTS tables: name ++ "\x00rowid" *)
let sys_fts_rowid_suffix = Bytes.of_string "\x00rowid"
let next_user_tid_key = Bytes.of_string "next_user_tid"
let next_user_tid_init = 16

(* Counter for monotonically-increasing index IDs, stored in sys_meta. *)
let next_index_id_key = Bytes.of_string "next_index_id"
let user_version_key = Bytes.of_string "\x00user_version"

(** Read user_version from an already-open RW or RO transaction.
    Returns 0 if never set. *)
let read_user_version_tx tx : int64 Lwt.t =
  let%lwt v = S.get tx sys_meta_tid user_version_key in
  Lwt.return
    (match v with
     | None -> 0L
     | Some b -> Bytes.get_int64_be b 0)
;;

(** Write user_version inside an already-open RW transaction.
    Caller is responsible for commit. *)
let write_user_version_tx tx (v : int64) : unit Lwt.t =
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 v;
  S.put tx sys_meta_tid user_version_key b
;;

type fk_constraint =
  { fk_local_cols : string list
  ; fk_parent_table : string
  ; fk_parent_cols : string list
  ; fk_on_delete : fk_action
  ; fk_on_update : fk_action
  ; fk_deferrable : bool (** false = IMMEDIATE (default), true = INITIALLY DEFERRED *)
  }

type pending_fk_kind =
  [ `Insert
  | `Update
  | `Delete
  ]

type pending_fk_recheck = { recheck : 'm. 'm S.txn -> bool Lwt.t }

type pending_fk_check =
  { pfk_kind : pending_fk_kind
  ; pfk_table : string
  ; pfk_rowid : int64
  ; pfk_message : string
  ; pfk_recheck : pending_fk_recheck
  }

type table_meta =
  { name : string
  ; tree_id : S.tree_id
  ; columns : Row.column list
  ; next_rowid : int64
  ; fk_constraints : fk_constraint list
  ; without_rowid : bool (** WITHOUT ROWID — phase 37 #122. *)
  }

type index_info =
  { idx_name : string
  ; idx_table : string
  ; idx_columns : string list (* col names for plain; expr SQL for expression indexes *)
  ; idx_unique : bool
  ; idx_tree_id : S.tree_id
  ; idx_expr_flags : bool list (* true = expression index column, false = plain column *)
  ; idx_where_sql : string option
  }

type fts_table_meta =
  { fts_name : string
  ; fts_content_tree : S.tree_id
  ; fts_index_tree : S.tree_id
  ; fts_columns : string list
  }

type t =
  { store : S.t
  ; cache : (string, table_meta) Hashtbl.t
  ; (* index_name -> index_info *)
    indexes : (string, index_info) Hashtbl.t
  ; (* fts_name -> fts_table_meta *)
    fts : (string, fts_table_meta) Hashtbl.t
  ; mutable fk_enforcement : bool
  ; mutable recursive_triggers : bool
  ; mutable defer_fks_pragma : bool
    (** PRAGMA defer_foreign_keys — when ON, every FK enforcement site treats
        the violation as deferred regardless of constraint definition.
        Reset to false at every txn boundary by the db layer. *)
  ; mutable pending_fk_checks : pending_fk_check list
    (** Queued deferred FK violations; drained at commit. The list is in
        reverse insertion order; drain reverses again before returning. *)
  }

let pp fmt t =
  Format.fprintf
    fmt
    "@[<hv>Catalog.t { tables = %d;@ indexes = %d;@ fts = %d;@ fk_enforcement = %b }@]"
    (Hashtbl.length t.cache)
    (Hashtbl.length t.indexes)
    (Hashtbl.length t.fts)
    t.fk_enforcement
;;

(* ------------------------------------------------------------------ *)
(* Encoding helpers                                                     *)
(* ------------------------------------------------------------------ *)

let encode_table_value m =
  let buf = Buffer.create 16 in
  Varint.encode_uint64 buf (Int64.of_int m.tree_id);
  Varint.encode_int64 buf m.next_rowid;
  (* Trailing without_rowid flag (phase 37).  Old encodings have no trailing
     bytes; the decoder treats their absence as [false]. *)
  Varint.encode_uint64 buf (if m.without_rowid then 1L else 0L);
  Buffer.to_bytes buf
;;

let decode_table_value bytes =
  let tid, off = Varint.decode_uint64 bytes 0 in
  let next, off' = Varint.decode_int64 bytes off in
  let without_rowid =
    if off' >= Bytes.length bytes
    then false
    else (
      let v, _ = Varint.decode_uint64 bytes off' in
      Int64.to_int v <> 0)
  in
  Int64.to_int tid, next, without_rowid
;;

(* Column key: table_name ++ NUL ++ ordinal_be8 *)
let column_key table_name ordinal =
  let tn = Bytes.of_string table_name in
  let ord = Bytes.create 8 in
  for i = 0 to 7 do
    Bytes.set_uint8 ord i ((ordinal lsr ((7 - i) * 8)) land 0xFF)
  done;
  Bytes.cat (Bytes.cat tn (Bytes.of_string "\x00")) ord
;;

let column_prefix table_name =
  Bytes.cat (Bytes.of_string table_name) (Bytes.of_string "\x00")
;;

let type_tag = function
  | Row.Integer -> 1
  | Row.Text -> 2
  | Row.Real -> 3
  | Row.Blob -> 4
;;

let type_of_tag = function
  | 1 -> Row.Integer
  | 2 -> Row.Text
  | 3 -> Row.Real
  | 4 -> Row.Blob
  | n -> failwith (Printf.sprintf "unknown column type tag %d" n)
;;

let default_value_tag : Row.default_value -> int = function
  | Row.DV_null -> 0
  | Row.DV_int _ -> 1
  | Row.DV_real _ -> 2
  | Row.DV_text _ -> 3
  | Row.DV_blob _ -> 4
  | Row.DV_current_timestamp -> 5
  | Row.DV_current_date -> 6
  | Row.DV_current_time -> 7
;;

let encode_default_value buf (dv : Row.default_value) =
  Varint.encode_uint64 buf (Int64.of_int (default_value_tag dv));
  match dv with
  | Row.DV_null -> ()
  | Row.DV_int n ->
    (* 8-byte LE int64 *)
    let tmp = Bytes.create 8 in
    for k = 0 to 7 do
      Bytes.set_uint8
        tmp
        k
        (Int64.to_int (Int64.logand (Int64.shift_right_logical n (k * 8)) 0xFFL))
    done;
    Buffer.add_bytes buf tmp
  | Row.DV_real f ->
    let bits = Int64.bits_of_float f in
    let tmp = Bytes.create 8 in
    for k = 0 to 7 do
      Bytes.set_uint8
        tmp
        k
        (Int64.to_int (Int64.logand (Int64.shift_right_logical bits (k * 8)) 0xFFL))
    done;
    Buffer.add_bytes buf tmp
  | Row.DV_text s ->
    Varint.encode_uint64 buf (Int64.of_int (String.length s));
    Buffer.add_string buf s
  | Row.DV_blob b ->
    Varint.encode_uint64 buf (Int64.of_int (Bytes.length b));
    Buffer.add_bytes buf b
  | Row.DV_current_timestamp | Row.DV_current_date | Row.DV_current_time ->
    () (* tag alone is sufficient — no payload *)
;;

let decode_default_value bytes off =
  let tag, off = Varint.decode_uint64 bytes off in
  match Int64.to_int tag with
  | 0 -> Row.DV_null, off
  | 1 ->
    let n = ref Int64.zero in
    for k = 0 to 7 do
      let byte = Int64.of_int (Bytes.get_uint8 bytes (off + k)) in
      n := Int64.logor !n (Int64.shift_left byte (k * 8))
    done;
    Row.DV_int !n, off + 8
  | 2 ->
    let bits = ref Int64.zero in
    for k = 0 to 7 do
      let byte = Int64.of_int (Bytes.get_uint8 bytes (off + k)) in
      bits := Int64.logor !bits (Int64.shift_left byte (k * 8))
    done;
    Row.DV_real (Int64.float_of_bits !bits), off + 8
  | 3 ->
    let len, off = Varint.decode_uint64 bytes off in
    let len = Int64.to_int len in
    let s = Bytes.sub_string bytes off len in
    Row.DV_text s, off + len
  | 4 ->
    let len, off = Varint.decode_uint64 bytes off in
    let len = Int64.to_int len in
    let b = Bytes.sub bytes off len in
    Row.DV_blob b, off + len
  | 5 -> Row.DV_current_timestamp, off
  | 6 -> Row.DV_current_date, off
  | 7 -> Row.DV_current_time, off
  | n -> failwith (Printf.sprintf "unknown default value tag %d" n)
;;

let encode_column (col : Row.column) =
  let buf = Buffer.create 16 in
  Varint.encode_uint64 buf (Int64.of_int (type_tag col.ty));
  Varint.encode_uint64 buf (Int64.of_int (String.length col.name));
  Buffer.add_string buf col.name;
  Varint.encode_uint64 buf (if col.not_null then 1L else 0L);
  Varint.encode_uint64 buf (if col.primary_key then 1L else 0L);
  (match col.default with
   | None -> Varint.encode_uint64 buf 0L
   | Some dv ->
     Varint.encode_uint64 buf 1L;
     encode_default_value buf dv);
  (* Phase 9: check_sql field — appended at end for backward compat *)
  (match col.check_sql with
   | None -> Varint.encode_uint64 buf 0L
   | Some sql ->
     Varint.encode_uint64 buf 1L;
     Varint.encode_uint64 buf (Int64.of_int (String.length sql));
     Buffer.add_string buf sql);
  (* Phase 25: generated_as field — appended for backward compat *)
  (match col.generated_as with
   | None -> Varint.encode_uint64 buf 0L
   | Some (sql, is_stored) ->
     Varint.encode_uint64 buf 1L;
     Varint.encode_uint64 buf (if is_stored then 1L else 0L);
     Varint.encode_uint64 buf (Int64.of_int (String.length sql));
     Buffer.add_string buf sql);
  Buffer.to_bytes buf
;;

let decode_check_sql bytes off =
  if Bytes.length bytes - off <= 0
  then None, off
  else (
    let has_check, off2 = Varint.decode_uint64 bytes off in
    if Int64.to_int has_check = 0
    then None, off2
    else (
      let sql_len, off3 = Varint.decode_uint64 bytes off2 in
      let sql = Bytes.sub_string bytes off3 (Int64.to_int sql_len) in
      Some sql, off3 + Int64.to_int sql_len))
;;

let decode_generated_as bytes off =
  if Bytes.length bytes - off <= 0
  then None
  else (
    let has_gen, off2 = Varint.decode_uint64 bytes off in
    if Int64.to_int has_gen = 0
    then None
    else (
      let is_stored, off3 = Varint.decode_uint64 bytes off2 in
      let sql_len, off4 = Varint.decode_uint64 bytes off3 in
      let sql = Bytes.sub_string bytes off4 (Int64.to_int sql_len) in
      Some (sql, Int64.to_int is_stored = 1)))
;;

let decode_column bytes =
  let tag, off = Varint.decode_uint64 bytes 0 in
  let len, off = Varint.decode_uint64 bytes off in
  let name = Bytes.sub_string bytes off (Int64.to_int len) in
  let off = off + Int64.to_int len in
  (* not_null and primary_key — present only in the new format.
     If there are no more bytes, default to false (backward compat). *)
  let bytes_left = Bytes.length bytes - off in
  if bytes_left = 0
  then
    Row.
      { name
      ; ty = type_of_tag (Int64.to_int tag)
      ; not_null = false
      ; primary_key = false
      ; default = None
      ; check_sql = None
      ; generated_as = None
      }
  else (
    let nn, off = Varint.decode_uint64 bytes off in
    let pk, off = Varint.decode_uint64 bytes off in
    let has_def, off = Varint.decode_uint64 bytes off in
    let default, off =
      if Int64.to_int has_def = 0
      then None, off
      else (
        let dv, off' = decode_default_value bytes off in
        Some dv, off')
    in
    let check_sql, final_off = decode_check_sql bytes off in
    let generated_as = decode_generated_as bytes final_off in
    Row.
      { name
      ; ty = type_of_tag (Int64.to_int tag)
      ; not_null = Int64.to_int nn <> 0
      ; primary_key = Int64.to_int pk <> 0
      ; default
      ; check_sql
      ; generated_as
      })
;;

(* Index value encoding:
   varint(name_len) ++ name ++ varint(table_len) ++ table
   ++ varint(n_cols) ++ (varint(col_len) ++ col)*n_cols
   ++ [unique: 1 byte] ++ varint(tree_id) *)
let encode_index_value (idx : index_info) =
  let buf = Buffer.create 32 in
  Varint.encode_uint64 buf (Int64.of_int (String.length idx.idx_name));
  Buffer.add_string buf idx.idx_name;
  Varint.encode_uint64 buf (Int64.of_int (String.length idx.idx_table));
  Buffer.add_string buf idx.idx_table;
  Varint.encode_uint64 buf (Int64.of_int (List.length idx.idx_columns));
  List.iter
    (fun col ->
       Varint.encode_uint64 buf (Int64.of_int (String.length col));
       Buffer.add_string buf col)
    idx.idx_columns;
  Buffer.add_char buf (if idx.idx_unique then '\x01' else '\x00');
  Varint.encode_uint64 buf (Int64.of_int idx.idx_tree_id);
  (* Extended fields version 2: expr flags + optional WHERE *)
  Varint.encode_uint64 buf 2L;
  (* One varint per column: 0 = plain column, 1 = expression column *)
  List.iter
    (fun is_expr -> Varint.encode_uint64 buf (if is_expr then 1L else 0L))
    idx.idx_expr_flags;
  (* WHERE clause SQL *)
  (match idx.idx_where_sql with
   | None -> Varint.encode_uint64 buf 0L
   | Some sql ->
     Varint.encode_uint64 buf 1L;
     Varint.encode_uint64 buf (Int64.of_int (String.length sql));
     Buffer.add_string buf sql);
  Buffer.to_bytes buf
;;

let decode_index_ext_fields bytes off2 cols =
  if off2 >= Bytes.length bytes
  then List.map (fun _ -> false) cols, None (* old format: no extended fields *)
  else (
    let version, off3 = Varint.decode_uint64 bytes off2 in
    match Int64.to_int version with
    | 1 ->
      (* Version 1 (Task 1): only WHERE clause, no expr flags *)
      let has_where, off4 = Varint.decode_uint64 bytes off3 in
      let where_sql =
        if Int64.to_int has_where = 0
        then None
        else (
          let sql_len, off5 = Varint.decode_uint64 bytes off4 in
          Some (Bytes.sub_string bytes off5 (Int64.to_int sql_len)))
      in
      List.map (fun _ -> false) cols, where_sql
    | 2 ->
      (* Version 2 (Task 2): n_cols expr flags, then WHERE clause *)
      let off_ref = ref off3 in
      let expr_flags =
        List.map
          (fun _ ->
             let flag, next = Varint.decode_uint64 bytes !off_ref in
             off_ref := next;
             Int64.to_int flag = 1)
          cols
      in
      let has_where, off4 = Varint.decode_uint64 bytes !off_ref in
      let where_sql =
        if Int64.to_int has_where = 0
        then None
        else (
          let sql_len, off5 = Varint.decode_uint64 bytes off4 in
          Some (Bytes.sub_string bytes off5 (Int64.to_int sql_len)))
      in
      expr_flags, where_sql
    | _ -> List.map (fun _ -> false) cols, None)
;;

let decode_index_value bytes =
  let name_len, off = Varint.decode_uint64 bytes 0 in
  let name_len = Int64.to_int name_len in
  let name = Bytes.sub_string bytes off name_len in
  let off = off + name_len in
  let tbl_len, off = Varint.decode_uint64 bytes off in
  let tbl_len = Int64.to_int tbl_len in
  let tbl = Bytes.sub_string bytes off tbl_len in
  let off = off + tbl_len in
  let n_cols, off = Varint.decode_uint64 bytes off in
  let n_cols = Int64.to_int n_cols in
  let off = ref off in
  let cols =
    List.init n_cols (fun _ ->
      let col_len, next_off = Varint.decode_uint64 bytes !off in
      let col = Bytes.sub_string bytes next_off (Int64.to_int col_len) in
      off := next_off + Int64.to_int col_len;
      col)
  in
  let unique_byte = Bytes.get_uint8 bytes !off in
  let tree_id, off2 = Varint.decode_uint64 bytes (!off + 1) in
  let idx_expr_flags, idx_where_sql = decode_index_ext_fields bytes off2 cols in
  { idx_name = name
  ; idx_table = tbl
  ; idx_columns = cols
  ; idx_unique = unique_byte <> 0
  ; idx_tree_id = Int64.to_int tree_id
  ; idx_expr_flags
  ; idx_where_sql
  }
;;

(* FTS value encoding:
   varint(content_tree) ++ varint(index_tree) ++ varint(n_cols)
   ++ (varint(col_len) ++ col_bytes)* *)
let encode_fts_value (m : fts_table_meta) =
  let buf = Buffer.create 32 in
  Varint.encode_uint64 buf (Int64.of_int m.fts_content_tree);
  Varint.encode_uint64 buf (Int64.of_int m.fts_index_tree);
  Varint.encode_uint64 buf (Int64.of_int (List.length m.fts_columns));
  List.iter
    (fun col ->
       let b = Bytes.of_string col in
       Varint.encode_uint64 buf (Int64.of_int (Bytes.length b));
       Buffer.add_bytes buf b)
    m.fts_columns;
  Buffer.to_bytes buf
;;

let decode_fts_value fts_name bytes =
  let ct, off0 = Varint.decode_uint64 bytes 0 in
  let it, off1 = Varint.decode_uint64 bytes off0 in
  let nc, off2 = Varint.decode_uint64 bytes off1 in
  let n = Int64.to_int nc in
  let cols = ref [] in
  let pos = ref off2 in
  for _ = 1 to n do
    let len, off = Varint.decode_uint64 bytes !pos in
    let col = Bytes.sub_string bytes off (Int64.to_int len) in
    cols := col :: !cols;
    pos := off + Int64.to_int len
  done;
  { fts_name
  ; fts_content_tree = Int64.to_int ct
  ; fts_index_tree = Int64.to_int it
  ; fts_columns = List.rev !cols
  }
;;

(* ------------------------------------------------------------------ *)
(* next_user_tid / next_index_id management                              *)
(* ------------------------------------------------------------------ *)

let read_uint64_key store key default =
  S.with_ro store
  @@ fun tx ->
  let%lwt v = S.get tx sys_meta_tid key in
  match v with
  | Some b ->
    let n, _ = Varint.decode_uint64 b 0 in
    Lwt.return (Int64.to_int n)
  | None -> Lwt.return default
;;

let write_uint64_key store key n =
  let%lwt tx = S.rw_begin store in
  let buf = Buffer.create 8 in
  Varint.encode_uint64 buf (Int64.of_int n);
  let%lwt () = S.put tx sys_meta_tid key (Buffer.to_bytes buf) in
  S.commit tx
;;

let read_next_user_tid store = read_uint64_key store next_user_tid_key next_user_tid_init
let write_next_user_tid store tid = write_uint64_key store next_user_tid_key tid
let read_next_index_id store = read_uint64_key store next_index_id_key 0
let write_next_index_id store id = write_uint64_key store next_index_id_key id

(* Encode an int as a varint key for the _sys_indexes tree. *)
let index_key id =
  let buf = Buffer.create 8 in
  Varint.encode_uint64 buf (Int64.of_int id);
  Buffer.to_bytes buf
;;

(* ------------------------------------------------------------------ *)
(* Load all metadata from the store                                     *)
(* ------------------------------------------------------------------ *)

let load_columns tx table_name =
  let prefix = column_prefix table_name in
  let%lwt cur = S.cursor_open tx sys_columns_tid in
  let _sr = S.cursor_seek cur prefix in
  let cols = ref [] in
  let rec walk () =
    match S.cursor_next cur with
    | None -> ()
    | Some (ck, cv) ->
      let plen = Bytes.length prefix in
      if Bytes.length ck >= plen && Bytes.equal (Bytes.sub ck 0 plen) prefix
      then (
        cols := decode_column cv :: !cols;
        walk ())
  in
  walk ();
  S.cursor_close cur;
  Lwt.return (List.rev !cols)
;;

let load_all_tables store =
  let tbl = Hashtbl.create 16 in
  S.with_ro store
  @@ fun tx ->
  let%lwt cur = S.cursor_open tx sys_tables_tid in
  let _sr = S.cursor_first cur in
  let rec walk_tables () =
    match S.cursor_next cur with
    | None -> Lwt.return_unit
    | Some (k, v) ->
      let%lwt () =
        Lwt.catch
          (fun () ->
             let name = Bytes.to_string k in
             let tid, next_rowid, without_rowid = decode_table_value v in
             let%lwt cols = load_columns tx name in
             Hashtbl.replace
               tbl
               name
               { name
               ; tree_id = tid
               ; columns = cols
               ; next_rowid
               ; fk_constraints = []
               ; without_rowid
               };
             Lwt.return_unit)
          (fun _exn ->
             (* Corrupt primary catalog row/columns (#174): skip it here; the
                table is reconstructed from the redundant mirror in [open_]. *)
             Lwt.return_unit)
      in
      walk_tables ()
  in
  let%lwt () = walk_tables () in
  S.cursor_close cur;
  Lwt.return tbl
;;

let load_all_indexes store =
  let tbl = Hashtbl.create 8 in
  S.with_ro store
  @@ fun tx ->
  let%lwt cur = S.cursor_open tx sys_indexes_tid in
  let _sr = S.cursor_first cur in
  let rec walk () =
    match S.cursor_next cur with
    | None -> Lwt.return_unit
    | Some (_k, v) ->
      let info = decode_index_value v in
      Hashtbl.replace tbl info.idx_name info;
      walk ()
  in
  let%lwt () = walk () in
  S.cursor_close cur;
  Lwt.return tbl
;;

let is_fts_rowid_key k =
  let slen = Bytes.length sys_fts_rowid_suffix in
  Bytes.length k >= slen
  && Bytes.equal (Bytes.sub k (Bytes.length k - slen) slen) sys_fts_rowid_suffix
;;

let load_all_fts store =
  let tbl = Hashtbl.create 4 in
  S.with_ro store
  @@ fun tx ->
  let%lwt cur = S.cursor_open tx sys_fts_tid in
  let _sr = S.cursor_first cur in
  let rec walk () =
    match S.cursor_next cur with
    | None -> Lwt.return_unit
    | Some (k, v) ->
      (* Skip rowid counter keys: they end with "\x00rowid" *)
      if is_fts_rowid_key k
      then walk ()
      else (
        (try
           let name = Bytes.to_string k in
           let meta = decode_fts_value name v in
           Hashtbl.replace tbl name meta
         with
         | Invalid_argument msg ->
           (* Corrupt FTS catalog entry for key; skip and continue.
             A corrupt entry will simply be absent from the cache;
             queries against that table will fail with "table not found". *)
           Printf.eprintf "warning: skipping corrupt FTS catalog entry (%s)\n%!" msg);
        walk ())
  in
  let%lwt () = walk () in
  S.cursor_close cur;
  Lwt.return tbl
;;

(* ------------------------------------------------------------------ *)
(* View persistence                                                     *)
(* ------------------------------------------------------------------ *)

let load_all_views store =
  S.with_ro store
  @@ fun tx ->
  let%lwt cur = S.cursor_open tx sys_views_tid in
  let _sr = S.cursor_first cur in
  let pairs = ref [] in
  let rec walk () =
    match S.cursor_next cur with
    | None -> ()
    | Some (k, v) ->
      pairs := (Bytes.to_string k, Bytes.to_string v) :: !pairs;
      walk ()
  in
  walk ();
  S.cursor_close cur;
  Lwt.return (List.rev !pairs)
;;

let persist_view store ~name ~sql =
  let%lwt tx = S.rw_begin store in
  let%lwt () = S.put tx sys_views_tid (Bytes.of_string name) (Bytes.of_string sql) in
  S.commit tx
;;

let remove_view store ~name =
  let%lwt tx = S.rw_begin store in
  let%lwt () = S.del tx sys_views_tid (Bytes.of_string name) in
  S.commit tx
;;

(* ------------------------------------------------------------------ *)
(* Trigger persistence                                                  *)
(* ------------------------------------------------------------------ *)

let load_all_triggers store =
  S.with_ro store
  @@ fun tx ->
  let%lwt cur = S.cursor_open tx sys_triggers_tid in
  let _sr = S.cursor_first cur in
  let pairs = ref [] in
  let rec walk () =
    match S.cursor_next cur with
    | None -> ()
    | Some (k, v) ->
      pairs := (Bytes.to_string k, Bytes.to_string v) :: !pairs;
      walk ()
  in
  walk ();
  S.cursor_close cur;
  Lwt.return (List.rev !pairs)
;;

let persist_trigger store ~name ~sql =
  let%lwt tx = S.rw_begin store in
  let%lwt () = S.put tx sys_triggers_tid (Bytes.of_string name) (Bytes.of_string sql) in
  S.commit tx
;;

let remove_trigger store ~name =
  let%lwt tx = S.rw_begin store in
  let%lwt () = S.del tx sys_triggers_tid (Bytes.of_string name) in
  S.commit tx
;;

(* ------------------------------------------------------------------ *)
(* FK constraint persistence                                            *)
(* ------------------------------------------------------------------ *)

let fk_meta_key table_name = Bytes.of_string ("fk:" ^ table_name)

let fk_action_to_string = function
  | FA_no_action -> "no_action"
  | FA_restrict -> "restrict"
  | FA_cascade -> "cascade"
  | FA_set_null -> "set_null"
  | FA_set_default -> "set_default"
;;

let fk_action_of_string = function
  | "no_action" -> FA_no_action
  | "restrict" -> FA_restrict
  | "cascade" -> FA_cascade
  | "set_null" -> FA_set_null
  | "set_default" -> FA_set_default
  | s -> failwith ("catalog: unknown fk_action: " ^ s)
;;

let encode_fks fks =
  let lines =
    List.map
      (fun fk ->
         String.concat
           "\t"
           [ String.concat "," fk.fk_local_cols
           ; fk.fk_parent_table
           ; String.concat "," fk.fk_parent_cols
           ; fk_action_to_string fk.fk_on_delete
           ; fk_action_to_string fk.fk_on_update
           ; (if fk.fk_deferrable then "1" else "0")
           ])
      fks
  in
  Bytes.of_string (String.concat "\n" lines)
;;

let decode_fks bytes =
  let s = Bytes.to_string bytes in
  if s = ""
  then []
  else
    List.filter_map
      (fun line ->
         match String.split_on_char '\t' line with
         | [ lc; pt; pc ] ->
           (* Legacy 3-field form (very old). *)
           Some
             { fk_local_cols = String.split_on_char ',' lc
             ; fk_parent_table = pt
             ; fk_parent_cols = String.split_on_char ',' pc
             ; fk_on_delete = FA_restrict
             ; fk_on_update = FA_restrict
             ; fk_deferrable = false
             }
         | [ lc; pt; pc; od; ou ] ->
           (* Pre-phase-35 5-field form: deferrable defaults false. *)
           Some
             { fk_local_cols = String.split_on_char ',' lc
             ; fk_parent_table = pt
             ; fk_parent_cols = String.split_on_char ',' pc
             ; fk_on_delete = fk_action_of_string od
             ; fk_on_update = fk_action_of_string ou
             ; fk_deferrable = false
             }
         | [ lc; pt; pc; od; ou; def ] ->
           (* Phase 35 6-field form. *)
           Some
             { fk_local_cols = String.split_on_char ',' lc
             ; fk_parent_table = pt
             ; fk_parent_cols = String.split_on_char ',' pc
             ; fk_on_delete = fk_action_of_string od
             ; fk_on_update = fk_action_of_string ou
             ; fk_deferrable = def = "1"
             }
         | _ -> None)
      (String.split_on_char '\n' s)
;;

(* ------------------------------------------------------------------ *)
(* Schema fingerprint + redundant catalog mirror helpers (#174)         *)
(* Defined here, ahead of all DDL and [open_], which use them.          *)
(* ------------------------------------------------------------------ *)

(* Schema fingerprint: a stable hash of a table's shape (columns +
   without_rowid).  Computed from the in-memory cache, so it always reflects
   the current schema after any DDL. *)
let fingerprint_of_meta (m : table_meta) =
  Schema_fingerprint.compute ~columns:m.columns ~without_rowid:m.without_rowid
;;

(* #174: register the table's page-header stamp (low 32 bits of its
   fingerprint) with the store, so its B+-tree pages self-identify their
   schema.  Skips the ephemeral CTE sentinel (tree_id = -1). *)
let register_tag store (m : table_meta) =
  if m.tree_id >= 0
  then S.set_tree_tag store m.tree_id (Schema_fingerprint.low32 (fingerprint_of_meta m))
;;

(* The mirror is keyed by tree_id (fixed 8-byte BE) and stores a fully
   self-describing schema blob — name, tree_id, WITHOUT ROWID, fingerprint,
   every column (via the same [encode_column] used by the primary), and FK
   constraints.  It carries no volatile state (no [next_rowid]) so it only
   changes on DDL, not on every insert. *)
let mirror_version = 1

let mirror_key (tid : S.tree_id) =
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 (Int64.of_int tid);
  b
;;

let encode_mirror_entry (m : table_meta) =
  let buf = Buffer.create 128 in
  Varint.encode_uint64 buf (Int64.of_int mirror_version);
  Varint.encode_uint64 buf (Int64.of_int (String.length m.name));
  Buffer.add_string buf m.name;
  Varint.encode_uint64 buf (Int64.of_int m.tree_id);
  Buffer.add_uint8 buf (if m.without_rowid then 1 else 0);
  let fpb = Bytes.create 8 in
  Bytes.set_int64_be fpb 0 (fingerprint_of_meta m);
  Buffer.add_bytes buf fpb;
  Varint.encode_uint64 buf (Int64.of_int (List.length m.columns));
  List.iter
    (fun col ->
       let cb = encode_column col in
       Varint.encode_uint64 buf (Int64.of_int (Bytes.length cb));
       Buffer.add_bytes buf cb)
    m.columns;
  let fkb = encode_fks m.fk_constraints in
  Varint.encode_uint64 buf (Int64.of_int (Bytes.length fkb));
  Buffer.add_bytes buf fkb;
  Buffer.to_bytes buf
;;

(* Decode a mirror entry into a [table_meta] (with [next_rowid = 1L]; the
   mirror does not persist the rowid counter — recovery is schema-only) and
   the stored fingerprint. *)
let decode_mirror_entry bytes : table_meta * int64 =
  let _ver, off = Varint.decode_uint64 bytes 0 in
  let nlen, off = Varint.decode_uint64 bytes off in
  let nlen = Int64.to_int nlen in
  let name = Bytes.sub_string bytes off nlen in
  let off = off + nlen in
  let tid, off = Varint.decode_uint64 bytes off in
  let without_rowid = Bytes.get_uint8 bytes off <> 0 in
  let off = off + 1 in
  let fp = Bytes.get_int64_be bytes off in
  let off = ref (off + 8) in
  let ncols, o = Varint.decode_uint64 bytes !off in
  off := o;
  let columns =
    List.init (Int64.to_int ncols) (fun _ ->
      let clen, o = Varint.decode_uint64 bytes !off in
      let clen = Int64.to_int clen in
      let col = decode_column (Bytes.sub bytes o clen) in
      off := o + clen;
      col)
  in
  let fklen, o = Varint.decode_uint64 bytes !off in
  let fkb = Bytes.sub bytes o (Int64.to_int fklen) in
  let fk_constraints = decode_fks fkb in
  ( { name
    ; tree_id = Int64.to_int tid
    ; columns
    ; next_rowid = 1L
    ; fk_constraints
    ; without_rowid
    }
  , fp )
;;

(* Write/replace a table's mirror entry inside an already-open RW txn. *)
let put_mirror_tx tx (m : table_meta) =
  S.put tx sys_mirror_tid (mirror_key m.tree_id) (encode_mirror_entry m)
;;

(* Remove a table's mirror entry inside an already-open RW txn. *)
let del_mirror_tx tx (tid : S.tree_id) = S.del tx sys_mirror_tid (mirror_key tid)

(* Decode every mirror entry into a [table_meta]; skip corrupt entries. *)
let load_mirror_entries store =
  S.with_ro store
  @@ fun tx ->
  let%lwt cur = S.cursor_open tx sys_mirror_tid in
  let _sr = S.cursor_first cur in
  let acc = ref [] in
  let rec walk () =
    match S.cursor_next cur with
    | None -> ()
    | Some (_k, v) ->
      (try
         let m, _fp = decode_mirror_entry v in
         acc := m :: !acc
       with
       | Invalid_argument _ | Failure _ -> ());
      walk ()
  in
  walk ();
  S.cursor_close cur;
  Lwt.return (List.rev !acc)
;;

(* #175: recover next_rowid for tables reconstructed from the mirror.
   Scan the table's data tree for the maximum integer rowid key (the
   tree is keyed by [Rowid.encode], so the last key in byte-sorted order
   is the maximum rowid).  Return [next_rowid = max + 1], or [1L] for an
   empty or unreadable tree.  WITHOUT ROWID tables are skipped — they
   don't use rowid keys. *)
let recover_next_rowid store (m : table_meta) : table_meta Lwt.t =
  if m.without_rowid
  then Lwt.return m
  else (
    S.with_ro store
    @@ fun tx ->
    let%lwt cur = S.cursor_open tx m.tree_id in
    let _sr = S.cursor_first cur in
    let max_key = ref None in
    let rec walk () =
      match S.cursor_next cur with
      | None -> ()
      | Some (k, _) ->
        max_key := Some k;
        walk ()
    in
    walk ();
    S.cursor_close cur;
    let recovered =
      match !max_key with
      | None -> 1L
      | Some k -> Int64.add (Rowid.decode k) 1L
    in
    Lwt.return { m with next_rowid = recovered })

let load_fk_constraints_raw store table_name =
  let key = fk_meta_key table_name in
  S.with_ro store
  @@ fun tx ->
  let%lwt v = S.get tx sys_meta_tid key in
  Lwt.return
    (match v with
     | None -> []
     | Some b -> decode_fks b)
;;

let save_fk_constraints t ~table_name ~fks =
  let key = fk_meta_key table_name in
  let%lwt tx = S.rw_begin t.store in
  let%lwt () =
    if fks = []
    then S.del tx sys_meta_tid key
    else S.put tx sys_meta_tid key (encode_fks fks)
  in
  (* Keep the mirror's FK list current so a mirror reconstruction restores
     constraints, not just columns. *)
  let%lwt () =
    match Hashtbl.find_opt t.cache table_name with
    | Some m -> put_mirror_tx tx { m with fk_constraints = fks }
    | None -> Lwt.return_unit
  in
  S.commit tx
;;

let set_fk_constraints t ~table_name ~fks =
  match Hashtbl.find_opt t.cache table_name with
  | None -> ()
  | Some meta -> Hashtbl.replace t.cache table_name { meta with fk_constraints = fks }
;;

(* ------------------------------------------------------------------ *)
(* Public API                                                           *)
(* ------------------------------------------------------------------ *)

let open_ store =
  let%lwt cache = load_all_tables store in
  let%lwt indexes = load_all_indexes store in
  let%lwt fts = load_all_fts store in
  (* Load FK constraints for each table *)
  let names = Hashtbl.fold (fun k _ acc -> k :: acc) cache [] in
  let%lwt () =
    Lwt_list.iter_s
      (fun name ->
         let%lwt fks = load_fk_constraints_raw store name in
         (match Hashtbl.find_opt cache name with
          | Some meta -> Hashtbl.replace cache name { meta with fk_constraints = fks }
          | None -> ());
         Lwt.return_unit)
      names
  in
  (* #174: reconstruct any table missing from the primary catalog (its
     _sys_tables row or column entries were lost or failed to decode) from the
     redundant mirror.  Tables loaded fine from the primary are left untouched
     here; the mirror only fills gaps.  Reconstructed entries carry the
     mirror's own columns and FK constraints (the primary FK load above ran
     only over primary tables). *)
  let%lwt mirror = load_mirror_entries store in
  let present_tids =
    Hashtbl.fold (fun _ (m : table_meta) acc -> m.tree_id :: acc) cache []
  in
  let reconstructed =
    List.filter (fun (m : table_meta) -> not (List.mem m.tree_id present_tids)) mirror
  in
  List.iter (fun (m : table_meta) -> Hashtbl.replace cache m.name m) reconstructed;
  (* #175: for tables reconstructed from the mirror, recover next_rowid by
     scanning the data tree for the maximum integer rowid key.  WITHOUT ROWID
     tables are skipped.  Best-effort: defaults to 1L for empty trees. *)
  let%lwt () =
    Lwt_list.iter_s
      (fun (m : table_meta) ->
         let%lwt recovered = recover_next_rowid store m in
         Hashtbl.replace cache recovered.name recovered;
         Lwt.return_unit)
      reconstructed
  in
  (* #174: schema-drift check on open.  For tables present in BOTH the primary
     and the mirror, a fingerprint mismatch means one copy is corrupt or drifted
     — warn (but stay openable so recovery tooling can still run).  Reconstructed
     tables match the mirror by construction, so they never trip this. *)
  List.iter
    (fun (m : table_meta) ->
       match Hashtbl.find_opt cache m.name with
       | Some primary
         when primary.tree_id = m.tree_id
              && not (Int64.equal (fingerprint_of_meta primary) (fingerprint_of_meta m))
         ->
         Printf.eprintf
           "warning: schema fingerprint mismatch for table %s — primary and redundant \
            catalog disagree (possible corruption, #174)\n\
            %!"
           m.name
       | _ -> ())
    mirror;
  (* #174: register every table's page-header stamp so subsequent writes
     stamp the tree's schema fingerprint. *)
  Hashtbl.iter (fun _ (m : table_meta) -> register_tag store m) cache;
  Lwt.return
    { store
    ; cache
    ; indexes
    ; fts
    ; fk_enforcement = false
    ; recursive_triggers = true
    ; defer_fks_pragma = false
    ; pending_fk_checks = []
    }
;;

(** Allocate and return the next available user tree ID, atomically incrementing the counter. *)
let next_user_tid t =
  let%lwt tid = read_next_user_tid t.store in
  let%lwt () = write_next_user_tid t.store (tid + 1) in
  Lwt.return tid
;;

let create_table t ~name ~columns ~without_rowid =
  if Hashtbl.mem t.cache name
  then failwith (Printf.sprintf "table '%s' already exists" name);
  let%lwt tid = next_user_tid t in
  let m =
    { name; tree_id = tid; columns; next_rowid = 1L; fk_constraints = []; without_rowid }
  in
  let%lwt tx = S.rw_begin t.store in
  let%lwt () = S.put tx sys_tables_tid (Bytes.of_string name) (encode_table_value m) in
  let%lwt () =
    Lwt_list.iteri_s
      (fun i col -> S.put tx sys_columns_tid (column_key name i) (encode_column col))
      columns
  in
  let%lwt () = put_mirror_tx tx m in
  let%lwt () = S.commit tx in
  Hashtbl.replace t.cache name m;
  register_tag t.store m;
  Lwt.return tid
;;

let find_table t ~name = Lwt.return (Hashtbl.find_opt t.cache name)
let find_table_cached t ~name = Hashtbl.find_opt t.cache name

let table_fingerprint t ~name =
  Option.map fingerprint_of_meta (Hashtbl.find_opt t.cache name)
;;

let fingerprints_by_tree_id t =
  Hashtbl.fold
    (fun _ (m : table_meta) acc ->
       (* Skip the ephemeral CTE sentinel (tree_id = -1): no real on-disk tree. *)
       if m.tree_id >= 0 then (m.tree_id, fingerprint_of_meta m) :: acc else acc)
    t.cache
    []
;;

(* All [(tree_id, fingerprint)] pairs recorded in the mirror. *)
let mirror_fingerprints t =
  S.with_ro t.store
  @@ fun tx ->
  let%lwt cur = S.cursor_open tx sys_mirror_tid in
  let _sr = S.cursor_first cur in
  let acc = ref [] in
  let rec walk () =
    match S.cursor_next cur with
    | None -> ()
    | Some (_k, v) ->
      (try
         let m, fp = decode_mirror_entry v in
         acc := (m.tree_id, fp) :: !acc
       with
       | Invalid_argument _ | Failure _ -> ());
      walk ()
  in
  walk ();
  S.cursor_close cur;
  Lwt.return !acc
;;

type schema_discrepancy =
  | Fingerprint_mismatch of
      { tree_id : S.tree_id
      ; primary : int64
      ; mirror : int64
      }
  | Missing_in_mirror of S.tree_id
  | Missing_in_primary of S.tree_id

let verify_against_mirror t =
  let%lwt mirror = mirror_fingerprints t in
  let primary = fingerprints_by_tree_id t in
  let findings = ref [] in
  List.iter
    (fun (tid, pfp) ->
       match List.assoc_opt tid mirror with
       | None -> findings := Missing_in_mirror tid :: !findings
       | Some mfp ->
         if not (Int64.equal pfp mfp)
         then
           findings
           := Fingerprint_mismatch { tree_id = tid; primary = pfp; mirror = mfp }
              :: !findings)
    primary;
  List.iter
    (fun (tid, _) ->
       if not (List.mem_assoc tid primary)
       then findings := Missing_in_primary tid :: !findings)
    mirror;
  Lwt.return (List.rev !findings)
;;

let register_ephemeral t (meta : table_meta) = Hashtbl.replace t.cache meta.name meta
let unregister_ephemeral t ~name = Hashtbl.remove t.cache name
let list_tables t = Lwt.return (Hashtbl.fold (fun _ v acc -> v :: acc) t.cache [])

let next_rowid t ~name =
  match Hashtbl.find_opt t.cache name with
  | None -> failwith (Printf.sprintf "no table '%s'" name)
  | Some m ->
    let id = m.next_rowid in
    let m' = { m with next_rowid = Int64.add id 1L } in
    Hashtbl.replace t.cache name m';
    let%lwt tx = S.rw_begin t.store in
    let%lwt () = S.put tx sys_tables_tid (Bytes.of_string name) (encode_table_value m') in
    let%lwt () = S.commit tx in
    Lwt.return id
;;

(** Like [next_rowid] but uses an already-acquired RW transaction.
    The txn is NOT committed; the caller is responsible for the commit.
    Use this when an explicit transaction is already held to avoid
    deadlocking on the store's RW mutex. *)
let next_rowid_in_txn t ~name (tx : S.rw S.txn) =
  match Hashtbl.find_opt t.cache name with
  | None -> failwith (Printf.sprintf "no table '%s'" name)
  | Some m ->
    let id = m.next_rowid in
    let m' = { m with next_rowid = Int64.add id 1L } in
    Hashtbl.replace t.cache name m';
    let%lwt () = S.put tx sys_tables_tid (Bytes.of_string name) (encode_table_value m') in
    Lwt.return id
;;

let create_index t ~name ~table ~columns ~unique ~expr_flags ~where_sql =
  if Hashtbl.mem t.indexes name
  then Lwt.return (Error (Printf.sprintf "index '%s' already exists" name))
  else (
    match Hashtbl.find_opt t.cache table with
    | None -> Lwt.return (Error (Printf.sprintf "no table '%s'" table))
    | Some tm ->
      (* Validate: for plain columns, check they exist in the table; skip for expression columns *)
      let col_with_flags = List.combine columns expr_flags in
      let missing =
        List.find_opt
          (fun (col, is_expr) ->
             (not is_expr)
             && not (List.exists (fun (c : Row.column) -> c.name = col) tm.columns))
          col_with_flags
      in
      (match missing with
       | Some (col, _) ->
         Lwt.return (Error (Printf.sprintf "no column '%s' on table '%s'" col table))
       | None ->
         let%lwt tid = next_user_tid t in
         let%lwt id = read_next_index_id t.store in
         let%lwt () = write_next_index_id t.store (id + 1) in
         let info =
           { idx_name = name
           ; idx_table = table
           ; idx_columns = columns
           ; idx_unique = unique
           ; idx_tree_id = tid
           ; idx_expr_flags = expr_flags
           ; idx_where_sql = where_sql
           }
         in
         let%lwt tx = S.rw_begin t.store in
         let%lwt () = S.put tx sys_indexes_tid (index_key id) (encode_index_value info) in
         let%lwt () = S.commit tx in
         Hashtbl.replace t.indexes name info;
         Lwt.return (Ok info)))
;;

let add_column t ~table_name ~(column : Row.column) =
  match Hashtbl.find_opt t.cache table_name with
  | None -> Lwt.return (Error (Printf.sprintf "table not found: %s" table_name))
  | Some meta ->
    let exists =
      List.exists (fun c -> String.equal c.Row.name column.Row.name) meta.columns
    in
    if exists
    then Lwt.return (Error (Printf.sprintf "column already exists: %s" column.Row.name))
    else (
      let new_cols = meta.columns @ [ column ] in
      let new_meta = { meta with columns = new_cols } in
      let ordinal = List.length meta.columns in
      let col_k = column_key table_name ordinal in
      let col_v = encode_column column in
      let%lwt tx = S.rw_begin t.store in
      let%lwt () = S.put tx sys_columns_tid col_k col_v in
      let%lwt () = put_mirror_tx tx new_meta in
      let%lwt () = S.commit tx in
      Hashtbl.replace t.cache table_name new_meta;
      register_tag t.store new_meta;
      Lwt.return (Ok ()))
;;

let indexes_for_table t ~table =
  Hashtbl.fold
    (fun _ info acc -> if info.idx_table = table then info :: acc else acc)
    t.indexes
    []
;;

let find_index t ~name = Hashtbl.find_opt t.indexes name

let find_index_covering_cols t ~table_name ~col_idxs =
  match Hashtbl.find_opt t.cache table_name with
  | None -> None
  | Some meta ->
    let n_target = List.length col_idxs in
    if n_target = 0
    then None
    else (
      (* Resolve col_idxs to column names; bail out if any idx is out of range. *)
      let cols_arr = Array.of_list meta.columns in
      let n_cols = Array.length cols_arr in
      let target_names_opt =
        try
          Some
            (List.map
               (fun i ->
                  if i < 0 || i >= n_cols then raise Exit else cols_arr.(i).Row.name)
               col_idxs)
        with
        | Exit -> None
      in
      match target_names_opt with
      | None -> None
      | Some target_names ->
        let candidates = indexes_for_table t ~table:table_name in
        List.find_opt
          (fun (i : index_info) ->
             (* Skip partial indexes — a row absent from the index may still
             satisfy the FK predicate (the WHERE clause masks rows). *)
             if i.idx_where_sql <> None
             then false
             else (
               (* Skip indexes that contain any expression column in the leading
               prefix we'd be scanning — we cannot match a raw value list
               against an expression key. *)
               let n_idx = List.length i.idx_columns in
               if n_idx < n_target
               then false
               else (
                 let prefix_names =
                   List.filteri (fun k _ -> k < n_target) i.idx_columns
                 in
                 let prefix_flags =
                   let len_flags = List.length i.idx_expr_flags in
                   if len_flags = 0
                   then List.init n_target (fun _ -> false)
                   else List.filteri (fun k _ -> k < n_target) i.idx_expr_flags
                 in
                 let no_expr_in_prefix = not (List.exists Fun.id prefix_flags) in
                 no_expr_in_prefix
                 &&
                 try List.for_all2 String.equal prefix_names target_names with
                 | Invalid_argument _ -> false)))
          candidates)
;;

let table_exists t ~name = Hashtbl.mem t.cache name
let index_exists t ~name = Hashtbl.mem t.indexes name

(** Scan _sys_indexes (using the given txn) to find the key for [name].
    Returns [None] if not found.

    [cursor_first] positions at the first entry (id=0 when it exists).
    We inspect that entry immediately via [cursor_next] — which on a
    pre-positioned cursor returns the current entry without advancing —
    so no entry is ever skipped, including the very first one. *)
let find_index_key_in_txn tx name =
  let%lwt cur = S.cursor_open tx sys_indexes_tid in
  (* Position at the first entry; returns Not_found `End if the tree is
     empty, in which case cursor_next will immediately return None. *)
  let _sr = S.cursor_first cur in
  let result = ref None in
  (* cursor_next after cursor_first returns the positioned (first) entry on
     its initial call, then advances on each subsequent call. *)
  let rec walk () =
    match S.cursor_next cur with
    | None -> ()
    | Some (k, v) ->
      let info = decode_index_value v in
      if info.idx_name = name then result := Some k else walk ()
  in
  walk ();
  S.cursor_close cur;
  Lwt.return !result
;;

let drop_index t tx ~name =
  (* Remove from _sys_indexes on disk by scanning for the numeric key. *)
  let%lwt key_opt = find_index_key_in_txn tx name in
  let%lwt () =
    match key_opt with
    | None -> Lwt.return_unit
    | Some key -> S.del tx sys_indexes_tid key
  in
  (* Update in-memory cache. *)
  Hashtbl.remove t.indexes name;
  Lwt.return_unit
;;

let drop_table t tx ~name =
  (* 0. Remove the mirror entry (keyed by tree_id), if we know the tree_id. *)
  let%lwt () =
    match Hashtbl.find_opt t.cache name with
    | Some m -> del_mirror_tx tx m.tree_id
    | None -> Lwt.return_unit
  in
  (* 1. Remove table entry from _sys_tables. *)
  let%lwt () = S.del tx sys_tables_tid (Bytes.of_string name) in
  (* 2. Remove all column entries from _sys_columns. *)
  let n_cols =
    match Hashtbl.find_opt t.cache name with
    | None -> 0
    | Some m -> List.length m.columns
  in
  let%lwt () =
    Lwt_list.iter_s
      (fun i -> S.del tx sys_columns_tid (column_key name i))
      (List.init n_cols (fun i -> i))
  in
  (* 3. Remove all associated indexes. *)
  let idx_list = indexes_for_table t ~table:name in
  let%lwt () =
    Lwt_list.iter_s
      (fun (idx : index_info) -> drop_index t tx ~name:idx.idx_name)
      idx_list
  in
  (* 4. Update in-memory cache. *)
  Hashtbl.remove t.cache name;
  Lwt.return_unit
;;

let rekey_table_columns tx ~old_name ~new_name ~n_cols =
  let rec loop i =
    if i >= n_cols
    then Lwt.return (Ok ())
    else (
      let old_k = column_key old_name i in
      let new_k = column_key new_name i in
      let%lwt bytes_opt = S.get tx sys_columns_tid old_k in
      match bytes_opt with
      | None ->
        let%lwt () = S.rollback tx in
        Lwt.return
          (Error
             (Printf.sprintf "catalog corrupt: column %d missing for table %s" i old_name))
      | Some bytes ->
        let%lwt () = S.del tx sys_columns_tid old_k in
        let%lwt () = S.put tx sys_columns_tid new_k bytes in
        loop (i + 1))
  in
  loop 0
;;

let finish_rename t tx ~old_name ~new_name ~meta =
  (* Re-write sys_indexes entries that reference old_name *)
  let%lwt idx_updates =
    S.with_ro t.store
    @@ fun tx_ro_idx ->
    let%lwt cur = S.cursor_open tx_ro_idx sys_indexes_tid in
    let _sr = S.cursor_first cur in
    let idx_updates = ref [] in
    let rec scan_idxs () =
      match S.cursor_next cur with
      | None -> ()
      | Some (k, v) ->
        let info = decode_index_value v in
        if String.equal info.idx_table old_name
        then idx_updates := (k, info) :: !idx_updates;
        scan_idxs ()
    in
    scan_idxs ();
    S.cursor_close cur;
    Lwt.return !idx_updates
  in
  let%lwt () =
    Lwt_list.iter_s
      (fun (k, (info : index_info)) ->
         let new_info = { info with idx_table = new_name } in
         S.put tx sys_indexes_tid k (encode_index_value new_info))
      idx_updates
  in
  (* Refresh the mirror entry (keyed by the unchanged tree_id) with the new
     name; the schema shape — hence the fingerprint — is unchanged. *)
  let%lwt () = put_mirror_tx tx { meta with name = new_name } in
  let%lwt () = S.commit tx in
  (* Update in-memory cache *)
  Hashtbl.remove t.cache old_name;
  Hashtbl.replace t.cache new_name { meta with name = new_name };
  (* Update in-memory index entries that reference old table name *)
  let to_update =
    Hashtbl.fold
      (fun k v acc -> if String.equal v.idx_table old_name then (k, v) :: acc else acc)
      t.indexes
      []
  in
  List.iter
    (fun (k, v) -> Hashtbl.replace t.indexes k { v with idx_table = new_name })
    to_update;
  Lwt.return (Ok ())
;;

let rename_table t ~old_name ~new_name =
  match Hashtbl.find_opt t.cache old_name with
  | None -> Lwt.return (Error (Printf.sprintf "table not found: %s" old_name))
  | Some meta ->
    if Hashtbl.mem t.cache new_name
    then Lwt.return (Error (Printf.sprintf "table already exists: %s" new_name))
    else (
      let%lwt tx = S.rw_begin t.store in
      (* Remove old sys_tables entry *)
      let%lwt () = S.del tx sys_tables_tid (Bytes.of_string old_name) in
      (* Insert new sys_tables entry *)
      let%lwt () =
        S.put tx sys_tables_tid (Bytes.of_string new_name) (encode_table_value meta)
      in
      (* Re-key all column entries; return Error if any entry is missing *)
      let%lwt col_result =
        rekey_table_columns tx ~old_name ~new_name ~n_cols:(List.length meta.columns)
      in
      match col_result with
      | Error msg -> Lwt.return (Error msg)
      | Ok () -> finish_rename t tx ~old_name ~new_name ~meta)
;;

let rename_column t ~table_name ~old_col ~new_col =
  match Hashtbl.find_opt t.cache table_name with
  | None -> Lwt.return (Error (Printf.sprintf "table not found: %s" table_name))
  | Some meta ->
    (match List.find_index (fun c -> String.equal c.Row.name old_col) meta.columns with
     | None -> Lwt.return (Error (Printf.sprintf "column not found: %s" old_col))
     | Some i ->
       let%lwt tx = S.rw_begin t.store in
       let col_k = column_key table_name i in
       let%lwt bytes_opt = S.get tx sys_columns_tid col_k in
       (match bytes_opt with
        | None ->
          let%lwt () = S.rollback tx in
          Lwt.return (Error "column entry missing from catalog")
        | Some old_bytes ->
          let old_col_rec = decode_column old_bytes in
          let new_col_rec = { old_col_rec with Row.name = new_col } in
          let%lwt () = S.put tx sys_columns_tid col_k (encode_column new_col_rec) in
          let new_columns =
            List.mapi
              (fun j c -> if j = i then { c with Row.name = new_col } else c)
              meta.columns
          in
          let new_meta = { meta with columns = new_columns } in
          let%lwt () = put_mirror_tx tx new_meta in
          let%lwt () = S.commit tx in
          Hashtbl.replace t.cache table_name new_meta;
          register_tag t.store new_meta;
          Lwt.return (Ok ())))
;;

let drop_column t ~table_name ~col_name =
  match Hashtbl.find_opt t.cache table_name with
  | None -> Lwt.return (Error (Printf.sprintf "table not found: %s" table_name))
  | Some meta ->
    let rec find_idx i = function
      | [] -> None
      | (c : Row.column) :: _ when String.equal c.name col_name -> Some i
      | _ :: rest -> find_idx (i + 1) rest
    in
    (match find_idx 0 meta.columns with
     | None -> Lwt.return (Error (Printf.sprintf "column not found: %s" col_name))
     | Some drop_idx ->
       let n_cols = List.length meta.columns in
       let%lwt tx = S.rw_begin t.store in
       (* Delete the dropped column's entry *)
       let%lwt () = S.del tx sys_columns_tid (column_key table_name drop_idx) in
       (* Re-key all columns after drop_idx: shift ordinal down by 1 *)
       let%lwt () =
         let rec shift i =
           if i >= n_cols
           then Lwt.return_unit
           else (
             let old_k = column_key table_name i in
             let new_k = column_key table_name (i - 1) in
             let%lwt bytes_opt = S.get tx sys_columns_tid old_k in
             match bytes_opt with
             | None -> shift (i + 1)
             | Some bytes ->
               let%lwt () = S.del tx sys_columns_tid old_k in
               let%lwt () = S.put tx sys_columns_tid new_k bytes in
               shift (i + 1))
         in
         shift (drop_idx + 1)
       in
       let new_columns = List.filteri (fun i _ -> i <> drop_idx) meta.columns in
       let new_meta = { meta with columns = new_columns } in
       let%lwt () = put_mirror_tx tx new_meta in
       let%lwt () = S.commit tx in
       Hashtbl.replace t.cache table_name new_meta;
       register_tag t.store new_meta;
       Lwt.return (Ok ()))
;;

(* ------------------------------------------------------------------ *)
(* FTS public API                                                       *)
(* ------------------------------------------------------------------ *)

let find_fts (t : t) name = Hashtbl.find_opt t.fts name
let list_fts_tables (t : t) = Hashtbl.fold (fun _name meta acc -> meta :: acc) t.fts []

let create_fts_table (t : t) ~name ~columns : fts_table_meta Lwt.t =
  (* NOTE: tree-ID allocation and metadata write span multiple transactions.
     A crash between the two next_user_tid calls leaks a tree-ID slot (non-fatal;
     the next create will allocate the next available slot). A crash after both
     allocations but before the sys_fts_tid write leaves the name unregistered and
     the two tree IDs permanently unused. Same pattern as create_table. *)
  (* Allocate two new tree IDs: one for content, one for the inverted index *)
  let%lwt content_tree = next_user_tid t in
  let%lwt index_tree = next_user_tid t in
  let meta =
    { fts_name = name
    ; fts_content_tree = content_tree
    ; fts_index_tree = index_tree
    ; fts_columns = columns
    }
  in
  (* Write to sys_fts_tid *)
  let%lwt tx = S.rw_begin t.store in
  let key = Bytes.of_string name in
  let value = encode_fts_value meta in
  let%lwt () = S.put tx sys_fts_tid key value in
  let%lwt () = S.commit tx in
  Hashtbl.replace t.fts name meta;
  Lwt.return meta
;;

(** Rowid counter for FTS tables stored as a separate key in sys_fts_tid.
    Key format: name ++ "\x00rowid" (the \x00 prefix sorts before printable ASCII). *)
let next_fts_rowid_in_txn (_t : t) ~name (tx : S.rw S.txn) : int64 Lwt.t =
  let rowid_key = Bytes.cat (Bytes.of_string name) sys_fts_rowid_suffix in
  let%lwt cur_opt = S.get tx sys_fts_tid rowid_key in
  let cur =
    match cur_opt with
    | None -> 1L
    | Some b ->
      let n, _ = Varint.decode_int64 b 0 in
      n
  in
  let next = Int64.add cur 1L in
  let nbuf = Buffer.create 8 in
  Varint.encode_int64 nbuf next;
  let%lwt () = S.put tx sys_fts_tid rowid_key (Buffer.to_bytes nbuf) in
  Lwt.return cur
;;

let get_fk_enforcement t = t.fk_enforcement
let set_fk_enforcement t v = t.fk_enforcement <- v
let get_recursive_triggers t = t.recursive_triggers
let set_recursive_triggers t v = t.recursive_triggers <- v
let get_defer_fks_pragma t = t.defer_fks_pragma
let set_defer_fks_pragma t v = t.defer_fks_pragma <- v
let queue_pending_fk_check t check = t.pending_fk_checks <- check :: t.pending_fk_checks

let drain_pending_fk_checks t =
  let pending = List.rev t.pending_fk_checks in
  t.pending_fk_checks <- [];
  pending
;;

let clear_pending_fk_checks t = t.pending_fk_checks <- []
let pending_fk_check_count t = List.length t.pending_fk_checks
let store t = t.store

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
