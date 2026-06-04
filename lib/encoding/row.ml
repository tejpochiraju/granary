type ty =
  | Integer
  | Text
  | Real
  | Blob

type default_value =
  | DV_int of int64
  | DV_text of string
  | DV_null
  | DV_real of float
  | DV_blob of bytes
  | DV_current_timestamp
  | DV_current_date
  | DV_current_time

type column =
  { name : string
  ; ty : ty
  ; not_null : bool
  ; primary_key : bool
  ; default : default_value option (* None = no DEFAULT *)
  ; check_sql : string option (* None = no CHECK constraint *)
  ; generated_as : (string * bool) option
    (** Some (expr_sql, is_stored): GENERATED ALWAYS AS expr.
      is_stored=true => STORED; false => VIRTUAL (both computed at write time). *)
  }

type schema = column list

type value =
  | V_int of int64
  | V_text of string
  | V_null
  | V_real of float
  | V_blob of bytes

type t = value array

let value_equal a b =
  match a, b with
  | V_int x, V_int y -> Int64.equal x y
  | V_text x, V_text y -> String.equal x y
  | V_null, V_null -> true
  | V_real x, V_real y -> Int64.equal (Int64.bits_of_float x) (Int64.bits_of_float y)
  | V_blob x, V_blob y -> Bytes.equal x y
  | _ -> false
;;

let equal a b = Array.length a = Array.length b && Array.for_all2 value_equal a b

(* Encode one column's value into [buf], enforcing that the value's runtime
   type matches the column's declared type. NULLs encode nothing (the null
   bitmap records them). *)
let encode_col_value buf (col : column) v =
  match v with
  | V_null -> ()
  | V_int n ->
    (match col.ty with
     | Integer -> Varint.encode_int64 buf n
     | _ ->
       invalid_arg
         (Printf.sprintf "Row.encode: integer value in non-integer column '%s'" col.name))
  | V_text s ->
    (match col.ty with
     | Text ->
       Varint.encode_uint64 buf (Int64.of_int (String.length s));
       Buffer.add_string buf s
     | _ ->
       invalid_arg
         (Printf.sprintf "Row.encode: text value in non-text column '%s'" col.name))
  | V_real f ->
    (match col.ty with
     | Real ->
       (* 8-byte little-endian IEEE-754 float64 *)
       let bits = Int64.bits_of_float f in
       let tmp = Bytes.create 8 in
       for k = 0 to 7 do
         Bytes.set_uint8
           tmp
           k
           (Int64.to_int (Int64.logand (Int64.shift_right_logical bits (k * 8)) 0xFFL))
       done;
       Buffer.add_bytes buf tmp
     | _ ->
       invalid_arg
         (Printf.sprintf "Row.encode: real value in non-real column '%s'" col.name))
  | V_blob b ->
    (match col.ty with
     | Blob ->
       Varint.encode_uint64 buf (Int64.of_int (Bytes.length b));
       Buffer.add_bytes buf b
     | _ ->
       invalid_arg
         (Printf.sprintf "Row.encode: blob value in non-blob column '%s'" col.name))
;;

let encode schema row =
  let n = List.length schema in
  if Array.length row <> n
  then
    invalid_arg
      (Printf.sprintf "Row.encode: expected %d columns, got %d" n (Array.length row));
  let buf = Buffer.create 32 in
  (* 1. column count *)
  Varint.encode_uint64 buf (Int64.of_int n);
  (* 2. null bitmap: bit i set  => column i is NULL.
        VIRTUAL generated columns are always encoded as NULL — their value
        is recomputed on read (see decode_with_virtual in lib/sql/exec.ml).
        STORED generated columns are persisted normally. *)
  let bitmap_bytes = (n + 7) / 8 in
  let bitmap = Bytes.make bitmap_bytes '\x00' in
  let is_virtual_col i =
    let col = List.nth schema i in
    match col.generated_as with
    | Some (_, false) -> true
    | _ -> false
  in
  Array.iteri
    (fun i v ->
       let null_in_bitmap =
         is_virtual_col i
         ||
         match v with
         | V_null -> true
         | _ -> false
       in
       if null_in_bitmap
       then (
         let byte_idx = i / 8
         and bit_idx = i mod 8 in
         let cur = Bytes.get_uint8 bitmap byte_idx in
         Bytes.set_uint8 bitmap byte_idx (cur lor (1 lsl bit_idx))))
    row;
  Buffer.add_bytes buf bitmap;
  (* 3. non-null values in column order — skip VIRTUAL generated cols *)
  List.iteri
    (fun i col -> if is_virtual_col i then () else encode_col_value buf col row.(i))
    schema;
  Buffer.to_bytes buf
;;

(* Decode one stored column value at [off]; returns (value, next_off). *)
let decode_col_value encoded off col =
  match col.ty with
  | Integer ->
    let v, off' = Varint.decode_int64 encoded off in
    V_int v, off'
  | Text ->
    let len, off' = Varint.decode_uint64 encoded off in
    let len = Int64.to_int len in
    V_text (Bytes.sub_string encoded off' len), off' + len
  | Real ->
    (* 8-byte little-endian IEEE-754 float64 *)
    let bits = ref Int64.zero in
    for k = 0 to 7 do
      let byte = Int64.of_int (Bytes.get_uint8 encoded (off + k)) in
      bits := Int64.logor !bits (Int64.shift_left byte (k * 8))
    done;
    V_real (Int64.float_of_bits !bits), off + 8
  | Blob ->
    let len, off' = Varint.decode_uint64 encoded off in
    let len = Int64.to_int len in
    V_blob (Bytes.sub encoded off' len), off' + len
;;

(* Value for a column absent from the encoding: its DEFAULT, else NULL. *)
let default_col_value col =
  match col.default with
  | Some (DV_int n) -> V_int n
  | Some (DV_text s) -> V_text s
  | Some (DV_real f) -> V_real f
  | Some (DV_blob b) -> V_blob b
  | Some DV_null | None -> V_null
  | Some DV_current_timestamp | Some DV_current_date | Some DV_current_time -> V_null
;;

let decode schema encoded =
  let n = List.length schema in
  (* 1. read column count *)
  let n', off = Varint.decode_uint64 encoded 0 in
  let n_encoded = Int64.to_int n' in
  if n_encoded > n
  then
    invalid_arg
      (Printf.sprintf "Row.decode: expected at most %d columns, got %d" n n_encoded);
  (* 2. read null bitmap (sized for the encoded columns only) *)
  let bitmap_bytes = (n_encoded + 7) / 8 in
  let bitmap = Bytes.sub encoded off bitmap_bytes in
  let off = ref (off + bitmap_bytes) in
  (* 3. decode each column *)
  let result = Array.make n V_null in
  List.iteri
    (fun i col ->
       if i < n_encoded
       then (
         let byte_idx = i / 8
         and bit_idx = i mod 8 in
         let is_null = (Bytes.get_uint8 bitmap byte_idx lsr bit_idx) land 1 = 1 in
         if not is_null
         then (
           let v, off' = decode_col_value encoded !off col in
           result.(i) <- v;
           off := off'))
       else
         (* Column i >= n_encoded: fill with schema default or NULL *)
         result.(i) <- default_col_value col)
    schema;
  result
;;

(* #247: decode only columns [0, upto] (inclusive); later columns are left
   [V_null].  Columns are variable-length and stored in order, so reaching column
   [upto] still requires walking (and decoding) every earlier column, but the
   trailing columns — typically a large TEXT/BLOB payload an aggregate never
   reads — are never decoded or allocated.  Columns in [0, upto] keep exact
   [decode] semantics; callers MUST guarantee no consumer reads a column index
   > [upto] (the aggregate fast path enforces this: it only reads up to the
   max referenced column, and only prunes when there is no row filter). *)
let decode_prefix schema encoded ~upto =
  let n = List.length schema in
  let n', off = Varint.decode_uint64 encoded 0 in
  let n_encoded = Int64.to_int n' in
  if n_encoded > n
  then
    invalid_arg
      (Printf.sprintf
         "Row.decode_prefix: expected at most %d columns, got %d"
         n
         n_encoded);
  let bitmap_bytes = (n_encoded + 7) / 8 in
  let bitmap = Bytes.sub encoded off bitmap_bytes in
  let off = ref (off + bitmap_bytes) in
  let result = Array.make n V_null in
  let limit = if upto >= n then n - 1 else upto in
  (try
     List.iteri
       (fun i col ->
          if i > limit
          then raise Exit (* nothing past [limit] is needed; stop walking *)
          else if i < n_encoded
          then (
            let byte_idx = i / 8
            and bit_idx = i mod 8 in
            let is_null = (Bytes.get_uint8 bitmap byte_idx lsr bit_idx) land 1 = 1 in
            if not is_null
            then (
              let v, off' = decode_col_value encoded !off col in
              result.(i) <- v;
              off := off'))
          else result.(i) <- default_col_value col)
       schema
   with
   | Exit -> ());
  result
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
