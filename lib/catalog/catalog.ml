module S = Sqlocaml_store.Store
module Row = Sqlocaml_encoding.Row
module Varint = Sqlocaml_encoding.Varint

(* System tree IDs *)
let sys_tables_tid  : S.tree_id = 0
let sys_columns_tid : S.tree_id = 1
let sys_indexes_tid : S.tree_id = 2
let sys_meta_tid    : S.tree_id = 3

let next_user_tid_key = Bytes.of_string "next_user_tid"
let next_user_tid_init = 16

(* Counter for monotonically-increasing index IDs, stored in sys_meta. *)
let next_index_id_key = Bytes.of_string "next_index_id"

type table_meta = {
  name : string;
  tree_id : S.tree_id;
  columns : Row.column list;
  next_rowid : int64;
}

type index_info = {
  idx_name    : string;
  idx_table   : string;
  idx_column  : string;
  idx_unique  : bool;
  idx_tree_id : S.tree_id;
}

type t = {
  store : S.t;
  cache : (string, table_meta) Hashtbl.t;
  (* index_name -> index_info *)
  indexes : (string, index_info) Hashtbl.t;
}

(* ------------------------------------------------------------------ *)
(* Encoding helpers                                                     *)
(* ------------------------------------------------------------------ *)

let encode_table_value m =
  let buf = Buffer.create 16 in
  Varint.encode_uint64 buf (Int64.of_int m.tree_id);
  Varint.encode_int64 buf m.next_rowid;
  Buffer.to_bytes buf

let decode_table_value bytes =
  let tid, off = Varint.decode_uint64 bytes 0 in
  let next, _ = Varint.decode_int64 bytes off in
  (Int64.to_int tid, next)

(* Column key: table_name ++ NUL ++ ordinal_be8 *)
let column_key table_name ordinal =
  let tn = Bytes.of_string table_name in
  let ord = Bytes.create 8 in
  for i = 0 to 7 do
    Bytes.set_uint8 ord i ((ordinal lsr ((7 - i) * 8)) land 0xFF)
  done;
  Bytes.cat (Bytes.cat tn (Bytes.of_string "\x00")) ord

let column_prefix table_name =
  Bytes.cat (Bytes.of_string table_name) (Bytes.of_string "\x00")

let type_tag = function
  | Row.Integer -> 1
  | Row.Text    -> 2
  | Row.Real    -> 3
  | Row.Blob    -> 4

let type_of_tag = function
  | 1 -> Row.Integer
  | 2 -> Row.Text
  | 3 -> Row.Real
  | 4 -> Row.Blob
  | n -> failwith (Printf.sprintf "unknown column type tag %d" n)

let default_value_tag : Row.default_value -> int = function
  | Row.DV_null   -> 0
  | Row.DV_int  _ -> 1
  | Row.DV_real _ -> 2
  | Row.DV_text _ -> 3
  | Row.DV_blob _ -> 4

let encode_default_value buf (dv : Row.default_value) =
  Varint.encode_uint64 buf (Int64.of_int (default_value_tag dv));
  match dv with
  | Row.DV_null   -> ()
  | Row.DV_int  n ->
    (* 8-byte LE int64 *)
    let tmp = Bytes.create 8 in
    for k = 0 to 7 do
      Bytes.set_uint8 tmp k (Int64.to_int (Int64.logand
        (Int64.shift_right_logical n (k * 8)) 0xFFL))
    done;
    Buffer.add_bytes buf tmp
  | Row.DV_real f ->
    let bits = Int64.bits_of_float f in
    let tmp = Bytes.create 8 in
    for k = 0 to 7 do
      Bytes.set_uint8 tmp k (Int64.to_int (Int64.logand
        (Int64.shift_right_logical bits (k * 8)) 0xFFL))
    done;
    Buffer.add_bytes buf tmp
  | Row.DV_text s ->
    Varint.encode_uint64 buf (Int64.of_int (String.length s));
    Buffer.add_string buf s
  | Row.DV_blob b ->
    Varint.encode_uint64 buf (Int64.of_int (Bytes.length b));
    Buffer.add_bytes buf b

let decode_default_value bytes off =
  let tag, off = Varint.decode_uint64 bytes off in
  match Int64.to_int tag with
  | 0 -> (Row.DV_null, off)
  | 1 ->
    let n = ref Int64.zero in
    for k = 0 to 7 do
      let byte = Int64.of_int (Bytes.get_uint8 bytes (off + k)) in
      n := Int64.logor !n (Int64.shift_left byte (k * 8))
    done;
    (Row.DV_int !n, off + 8)
  | 2 ->
    let bits = ref Int64.zero in
    for k = 0 to 7 do
      let byte = Int64.of_int (Bytes.get_uint8 bytes (off + k)) in
      bits := Int64.logor !bits (Int64.shift_left byte (k * 8))
    done;
    (Row.DV_real (Int64.float_of_bits !bits), off + 8)
  | 3 ->
    let len, off = Varint.decode_uint64 bytes off in
    let len = Int64.to_int len in
    let s = Bytes.sub_string bytes off len in
    (Row.DV_text s, off + len)
  | 4 ->
    let len, off = Varint.decode_uint64 bytes off in
    let len = Int64.to_int len in
    let b = Bytes.sub bytes off len in
    (Row.DV_blob b, off + len)
  | n -> failwith (Printf.sprintf "unknown default value tag %d" n)

let encode_column (col : Row.column) =
  let buf = Buffer.create 16 in
  Varint.encode_uint64 buf (Int64.of_int (type_tag col.ty));
  Varint.encode_uint64 buf (Int64.of_int (String.length col.name));
  Buffer.add_string buf col.name;
  Varint.encode_uint64 buf (if col.not_null    then 1L else 0L);
  Varint.encode_uint64 buf (if col.primary_key then 1L else 0L);
  (match col.default with
   | None    -> Varint.encode_uint64 buf 0L
   | Some dv ->
     Varint.encode_uint64 buf 1L;
     encode_default_value buf dv);
  Buffer.to_bytes buf

let decode_column bytes =
  let tag, off = Varint.decode_uint64 bytes 0 in
  let len, off = Varint.decode_uint64 bytes off in
  let name = Bytes.sub_string bytes off (Int64.to_int len) in
  let off  = off + Int64.to_int len in
  (* not_null and primary_key — present only in the new format.
     If there are no more bytes, default to false (backward compat). *)
  let bytes_left = Bytes.length bytes - off in
  if bytes_left = 0 then
    Row.{ name; ty = type_of_tag (Int64.to_int tag);
          not_null = false; primary_key = false; default = None }
  else begin
    let nn, off  = Varint.decode_uint64 bytes off in
    let pk, off  = Varint.decode_uint64 bytes off in
    let has_def, off = Varint.decode_uint64 bytes off in
    let default =
      if Int64.to_int has_def = 0 then None
      else let dv, _ = decode_default_value bytes off in Some dv
    in
    Row.{ name; ty = type_of_tag (Int64.to_int tag);
          not_null    = (Int64.to_int nn <> 0);
          primary_key = (Int64.to_int pk <> 0);
          default }
  end

(* Index value encoding:
   varint(name_len) ++ name ++ varint(table_len) ++ table
   ++ varint(col_len) ++ col ++ [unique: 1 byte] ++ varint(tree_id) *)
let encode_index_value (idx : index_info) =
  let buf = Buffer.create 32 in
  Varint.encode_uint64 buf (Int64.of_int (String.length idx.idx_name));
  Buffer.add_string buf idx.idx_name;
  Varint.encode_uint64 buf (Int64.of_int (String.length idx.idx_table));
  Buffer.add_string buf idx.idx_table;
  Varint.encode_uint64 buf (Int64.of_int (String.length idx.idx_column));
  Buffer.add_string buf idx.idx_column;
  Buffer.add_char buf (if idx.idx_unique then '\x01' else '\x00');
  Varint.encode_uint64 buf (Int64.of_int idx.idx_tree_id);
  Buffer.to_bytes buf

let decode_index_value bytes =
  let name_len, off = Varint.decode_uint64 bytes 0 in
  let name_len = Int64.to_int name_len in
  let name = Bytes.sub_string bytes off name_len in
  let off = off + name_len in
  let tbl_len, off = Varint.decode_uint64 bytes off in
  let tbl_len = Int64.to_int tbl_len in
  let tbl = Bytes.sub_string bytes off tbl_len in
  let off = off + tbl_len in
  let col_len, off = Varint.decode_uint64 bytes off in
  let col_len = Int64.to_int col_len in
  let col = Bytes.sub_string bytes off col_len in
  let off = off + col_len in
  let unique_byte = Bytes.get_uint8 bytes off in
  let off = off + 1 in
  let tree_id, _ = Varint.decode_uint64 bytes off in
  {
    idx_name    = name;
    idx_table   = tbl;
    idx_column  = col;
    idx_unique  = (unique_byte <> 0);
    idx_tree_id = Int64.to_int tree_id;
  }

(* ------------------------------------------------------------------ *)
(* next_user_tid / next_index_id management                              *)
(* ------------------------------------------------------------------ *)

let read_uint64_key store key default =
  let%lwt tx = S.ro_begin store in
  let%lwt v = S.get tx sys_meta_tid key in
  let%lwt () = S.ro_end tx in
  match v with
  | Some b ->
    let n, _ = Varint.decode_uint64 b 0 in
    Lwt.return (Int64.to_int n)
  | None -> Lwt.return default

let write_uint64_key store key n =
  let%lwt tx = S.rw_begin store in
  let buf = Buffer.create 8 in
  Varint.encode_uint64 buf (Int64.of_int n);
  let%lwt () = S.put tx sys_meta_tid key (Buffer.to_bytes buf) in
  S.commit tx

let read_next_user_tid store =
  read_uint64_key store next_user_tid_key next_user_tid_init

let write_next_user_tid store tid =
  write_uint64_key store next_user_tid_key tid

let read_next_index_id store =
  read_uint64_key store next_index_id_key 0

let write_next_index_id store id =
  write_uint64_key store next_index_id_key id

(* Encode an int as a varint key for the _sys_indexes tree. *)
let index_key id =
  let buf = Buffer.create 8 in
  Varint.encode_uint64 buf (Int64.of_int id);
  Buffer.to_bytes buf

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
      if Bytes.length ck >= plen &&
         Bytes.equal (Bytes.sub ck 0 plen) prefix
      then begin
        cols := decode_column cv :: !cols;
        walk ()
      end
  in
  walk ();
  S.cursor_close cur;
  Lwt.return (List.rev !cols)

let load_all_tables store =
  let tbl = Hashtbl.create 16 in
  let%lwt tx = S.ro_begin store in
  let%lwt cur = S.cursor_open tx sys_tables_tid in
  let _sr = S.cursor_first cur in
  let rec walk_tables () =
    match S.cursor_next cur with
    | None -> Lwt.return_unit
    | Some (k, v) ->
      let name = Bytes.to_string k in
      let tid, next_rowid = decode_table_value v in
      let%lwt cols = load_columns tx name in
      Hashtbl.replace tbl name {
        name;
        tree_id = tid;
        columns = cols;
        next_rowid;
      };
      walk_tables ()
  in
  let%lwt () = walk_tables () in
  S.cursor_close cur;
  let%lwt () = S.ro_end tx in
  Lwt.return tbl

let load_all_indexes store =
  let tbl = Hashtbl.create 8 in
  let%lwt tx = S.ro_begin store in
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
  let%lwt () = S.ro_end tx in
  Lwt.return tbl

(* ------------------------------------------------------------------ *)
(* Public API                                                           *)
(* ------------------------------------------------------------------ *)

let open_ store =
  let%lwt cache = load_all_tables store in
  let%lwt indexes = load_all_indexes store in
  Lwt.return { store; cache; indexes }

let create_table t ~name ~columns =
  if Hashtbl.mem t.cache name then
    failwith (Printf.sprintf "table '%s' already exists" name);
  let%lwt tid = read_next_user_tid t.store in
  let%lwt () = write_next_user_tid t.store (tid + 1) in
  let m = { name; tree_id = tid; columns; next_rowid = 1L } in
  let%lwt tx = S.rw_begin t.store in
  let%lwt () = S.put tx sys_tables_tid (Bytes.of_string name) (encode_table_value m) in
  let%lwt () =
    Lwt_list.iteri_s (fun i col ->
      S.put tx sys_columns_tid (column_key name i) (encode_column col)
    ) columns
  in
  let%lwt () = S.commit tx in
  Hashtbl.replace t.cache name m;
  Lwt.return tid

let find_table t ~name =
  Lwt.return (Hashtbl.find_opt t.cache name)

let list_tables t =
  Lwt.return (Hashtbl.fold (fun _ v acc -> v :: acc) t.cache [])

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

let create_index t ~name ~table ~column ~unique =
  if Hashtbl.mem t.indexes name then
    Lwt.return (Error (Printf.sprintf "index '%s' already exists" name))
  else match Hashtbl.find_opt t.cache table with
    | None ->
      Lwt.return (Error (Printf.sprintf "no table '%s'" table))
    | Some tm ->
      let has_col =
        List.exists (fun (c : Row.column) -> c.name = column) tm.columns
      in
      if not has_col then
        Lwt.return (Error (Printf.sprintf
                             "no column '%s' on table '%s'" column table))
      else
        let%lwt tid = read_next_user_tid t.store in
        let%lwt () = write_next_user_tid t.store (tid + 1) in
        let%lwt id = read_next_index_id t.store in
        let%lwt () = write_next_index_id t.store (id + 1) in
        let info = {
          idx_name    = name;
          idx_table   = table;
          idx_column  = column;
          idx_unique  = unique;
          idx_tree_id = tid;
        } in
        let%lwt tx = S.rw_begin t.store in
        let%lwt () =
          S.put tx sys_indexes_tid (index_key id) (encode_index_value info)
        in
        let%lwt () = S.commit tx in
        Hashtbl.replace t.indexes name info;
        Lwt.return (Ok info)

let indexes_for_table t ~table =
  Hashtbl.fold (fun _ info acc ->
    if info.idx_table = table then info :: acc else acc
  ) t.indexes []

let find_index t ~name =
  Hashtbl.find_opt t.indexes name

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
      if info.idx_name = name then
        result := Some k
      else
        walk ()
  in
  walk ();
  S.cursor_close cur;
  Lwt.return !result

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

let drop_table t tx ~name =
  (* 1. Remove table entry from _sys_tables. *)
  let%lwt () = S.del tx sys_tables_tid (Bytes.of_string name) in
  (* 2. Remove all column entries from _sys_columns. *)
  let n_cols =
    match Hashtbl.find_opt t.cache name with
    | None -> 0
    | Some m -> List.length m.columns
  in
  let%lwt () = Lwt_list.iter_s (fun i ->
    S.del tx sys_columns_tid (column_key name i)
  ) (List.init n_cols (fun i -> i)) in
  (* 3. Remove all associated indexes. *)
  let idx_list = indexes_for_table t ~table:name in
  let%lwt () = Lwt_list.iter_s (fun (idx : index_info) ->
    drop_index t tx ~name:idx.idx_name
  ) idx_list in
  (* 4. Update in-memory cache. *)
  Hashtbl.remove t.cache name;
  Lwt.return_unit
