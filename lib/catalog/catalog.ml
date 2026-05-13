module S = Sqlocaml_store.Store
module Row = Sqlocaml_encoding.Row
module Varint = Sqlocaml_encoding.Varint

(* System tree IDs *)
let sys_tables_tid : S.tree_id = 0
let sys_columns_tid : S.tree_id = 1
let sys_meta_tid : S.tree_id = 3

let next_user_tid_key = Bytes.of_string "next_user_tid"
let next_user_tid_init = 16

type table_meta = {
  name : string;
  tree_id : S.tree_id;
  columns : Row.column list;
  next_rowid : int64;
}

type t = {
  store : S.t;
  cache : (string, table_meta) Hashtbl.t;
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

let type_tag = function Row.Integer -> 1 | Row.Text -> 2

let type_of_tag = function
  | 1 -> Row.Integer
  | 2 -> Row.Text
  | n -> failwith (Printf.sprintf "unknown column type tag %d" n)

let encode_column (col : Row.column) =
  let buf = Buffer.create 16 in
  Varint.encode_uint64 buf (Int64.of_int (type_tag col.ty));
  Varint.encode_uint64 buf (Int64.of_int (String.length col.name));
  Buffer.add_string buf col.name;
  Buffer.to_bytes buf

let decode_column bytes =
  let tag, off = Varint.decode_uint64 bytes 0 in
  let len, off = Varint.decode_uint64 bytes off in
  let name = Bytes.sub_string bytes off (Int64.to_int len) in
  Row.{ name; ty = type_of_tag (Int64.to_int tag) }

(* ------------------------------------------------------------------ *)
(* next_user_tid management                                             *)
(* ------------------------------------------------------------------ *)

let read_next_user_tid store =
  let%lwt tx = S.ro_begin store in
  let%lwt v = S.get tx sys_meta_tid next_user_tid_key in
  let%lwt () = S.ro_end tx in
  match v with
  | Some b ->
    let n, _ = Varint.decode_uint64 b 0 in
    Lwt.return (Int64.to_int n)
  | None -> Lwt.return next_user_tid_init

let write_next_user_tid store tid =
  let%lwt tx = S.rw_begin store in
  let buf = Buffer.create 8 in
  Varint.encode_uint64 buf (Int64.of_int tid);
  let%lwt () = S.put tx sys_meta_tid next_user_tid_key (Buffer.to_bytes buf) in
  S.commit tx

(* ------------------------------------------------------------------ *)
(* Load all table metadata from the store                               *)
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

let load_all store =
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

(* ------------------------------------------------------------------ *)
(* Public API                                                           *)
(* ------------------------------------------------------------------ *)

let open_ store =
  let%lwt cache = load_all store in
  Lwt.return { store; cache }

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
