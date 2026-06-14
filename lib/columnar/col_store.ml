module Row = Sqlocaml_encoding.Row
module Varint = Sqlocaml_encoding.Varint

type t =
  { schema : Row.column list
  ; mutable columns : Col.t array
  ; mutable total : int
  ; mutable dirty : bool
  }

let store_format_version = 0x01
let create schema = { schema; columns = [||]; total = 0; dirty = false }
let nrows t = t.total
let columns t = t.schema
let dirty t = t.dirty
let mark_clean t = t.dirty <- false

let insert_rows t batch =
  let n = Array.length batch in
  if n > 0
  then (
    let ncols = List.length t.schema in
    let new_total = t.total + n in
    let schema_arr = Array.of_list t.schema in
    let init_col i =
      let col_ty = schema_arr.(i).Row.ty in
      let col = Col.create col_ty (max n 16) in
      let col_rows = Array.init n (fun r -> batch.(r).(i)) in
      Col.append_batch col col_rows
    in
    let append_col i existing =
      let col_rows = Array.init n (fun r -> batch.(r).(i)) in
      Col.append_batch existing col_rows
    in
    let new_cols =
      if Array.length t.columns = 0
      then Array.init ncols init_col
      else Array.init ncols (fun i -> append_col i t.columns.(i))
    in
    t.columns <- new_cols;
    t.total <- new_total;
    t.dirty <- true)
;;

let to_row_seq t =
  let n = t.total in
  if n = 0
  then Seq.empty
  else (
    let ncols = List.length t.schema in
    let ri = ref 0 in
    let rec next () =
      if !ri >= n
      then Seq.Nil
      else (
        let row = Array.init ncols (fun i -> Col.get_value t.columns.(i) !ri) in
        incr ri;
        Seq.Cons (row, next))
    in
    next)
;;

let encode t =
  let buf = Buffer.create 64 in
  Buffer.add_char buf (Char.chr store_format_version);
  Varint.encode_uint64 buf (Int64.of_int (Array.length t.columns));
  Varint.encode_uint64 buf (Int64.of_int t.total);
  Array.iter (fun col -> Buffer.add_bytes buf (Col.encode col)) t.columns;
  Buffer.to_bytes buf
;;

let decode ?(off = 0) schema buf =
  let version = Char.code (Bytes.get buf off) in
  if version < store_format_version
  then
    failwith
      (Printf.sprintf
         "Col_store.decode: unsupported format version %d (expected >= %d)"
         version
         store_format_version);
  let ncols, off = Varint.decode_uint64 buf (off + 1) in
  let ncols = Int64.to_int ncols in
  let schema_len = List.length schema in
  if ncols <> schema_len
  then
    failwith
      (Printf.sprintf
         "Col_store.decode: encoded column count %d does not match schema length %d"
         ncols
         schema_len);
  let total, off = Varint.decode_uint64 buf off in
  let total = Int64.to_int total in
  let columns = Array.make ncols (Col.create Row.Integer 0) in
  let off = ref off in
  for i = 0 to ncols - 1 do
    let col, o2 = Col.decode buf !off in
    columns.(i) <- col;
    off := o2
  done;
  Array.iteri
    (fun i col ->
       let clen = Col.length col in
       if clen <> total
       then
         failwith
           (Printf.sprintf
              "Col_store.decode: column %d has length %d but store header reports total \
               %d"
              i
              clen
              total))
    columns;
  { schema; columns; total; dirty = false }
;;
