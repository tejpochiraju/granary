type ty =
  | Integer
  | Text

type column = { name : string; ty : ty }
type schema = column list

type value =
  | V_int of int64
  | V_text of string
  | V_null

type t = value array

let value_equal a b = match a, b with
  | V_int x,  V_int y  -> Int64.equal x y
  | V_text x, V_text y -> String.equal x y
  | V_null,   V_null   -> true
  | _                  -> false

let equal a b =
  Array.length a = Array.length b
  && Array.for_all2 value_equal a b

let encode schema row =
  let n = List.length schema in
  if Array.length row <> n then
    invalid_arg (Printf.sprintf
      "Row.encode: expected %d columns, got %d" n (Array.length row));
  let buf = Buffer.create 32 in
  (* 1. column count *)
  Varint.encode_uint64 buf (Int64.of_int n);
  (* 2. null bitmap: bit i set  => column i is NULL *)
  let bitmap_bytes = (n + 7) / 8 in
  let bitmap = Bytes.make bitmap_bytes '\x00' in
  Array.iteri (fun i v ->
    if v = V_null then begin
      let byte_idx = i / 8 and bit_idx = i mod 8 in
      let cur = Bytes.get_uint8 bitmap byte_idx in
      Bytes.set_uint8 bitmap byte_idx (cur lor (1 lsl bit_idx))
    end
  ) row;
  Buffer.add_bytes buf bitmap;
  (* 3. non-null values in column order *)
  List.iteri (fun i col ->
    match row.(i) with
    | V_null   -> ()
    | V_int n ->
      (match col.ty with
       | Integer -> Varint.encode_int64 buf n
       | Text    ->
         invalid_arg (Printf.sprintf
           "Row.encode: integer value in text column '%s'" col.name))
    | V_text s ->
      (match col.ty with
       | Text ->
         Varint.encode_uint64 buf (Int64.of_int (String.length s));
         Buffer.add_string buf s
       | Integer ->
         invalid_arg (Printf.sprintf
           "Row.encode: text value in integer column '%s'" col.name))
  ) schema;
  Buffer.to_bytes buf

let decode schema encoded =
  let n = List.length schema in
  (* 1. read column count *)
  let n', off = Varint.decode_uint64 encoded 0 in
  if Int64.to_int n' <> n then
    invalid_arg (Printf.sprintf
      "Row.decode: expected %d columns, got %Ld" n n');
  (* 2. read null bitmap *)
  let bitmap_bytes = (n + 7) / 8 in
  let bitmap = Bytes.sub encoded off bitmap_bytes in
  let off = ref (off + bitmap_bytes) in
  (* 3. decode each column *)
  let result = Array.make n V_null in
  List.iteri (fun i col ->
    let byte_idx = i / 8 and bit_idx = i mod 8 in
    let is_null = (Bytes.get_uint8 bitmap byte_idx lsr bit_idx) land 1 = 1 in
    if not is_null then
      match col.ty with
      | Integer ->
        let v, off' = Varint.decode_int64 encoded !off in
        result.(i) <- V_int v;
        off := off'
      | Text ->
        let len, off' = Varint.decode_uint64 encoded !off in
        let len = Int64.to_int len in
        let s = Bytes.sub_string encoded off' len in
        result.(i) <- V_text s;
        off := off' + len
  ) schema;
  result
