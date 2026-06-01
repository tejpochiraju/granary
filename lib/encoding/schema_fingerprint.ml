(* FNV-1a 64-bit. Self-contained copy (the storage-layer WAL has its own, but
   this module lives in [sqlocaml.encoding] which storage does not depend on,
   so we keep an independent, tested copy here rather than introduce a
   dependency edge into the crash-recovery-critical WAL code). *)
let fnv64_offset = 0xCBF29CE484222325L
let fnv64_prime = 0x00000100000001B3L
let fnv64_byte h b = Int64.mul (Int64.logxor h (Int64.of_int (b land 0xff))) fnv64_prime

let fnv64_bytes h (b : bytes) =
  let h = ref h in
  for i = 0 to Bytes.length b - 1 do
    h := fnv64_byte !h (Bytes.get_uint8 b i)
  done;
  !h
;;

(* Canonicalization format version.  Bump this if the byte layout below ever
   changes, so old and new fingerprints are never silently compared. *)
let canon_version = 1

let type_tag : Row.ty -> int = function
  | Row.Integer -> 1
  | Row.Text -> 2
  | Row.Real -> 3
  | Row.Blob -> 4
;;

let default_tag : Row.default_value -> int = function
  | Row.DV_null -> 0
  | Row.DV_int _ -> 1
  | Row.DV_real _ -> 2
  | Row.DV_text _ -> 3
  | Row.DV_blob _ -> 4
  | Row.DV_current_timestamp -> 5
  | Row.DV_current_date -> 6
  | Row.DV_current_time -> 7
;;

(* Length-delimited string: varint(len) ++ bytes.  The length prefix makes the
   serialization unambiguous, so two adjacent string fields cannot be re-split
   to produce the same byte stream. *)
let add_str buf s =
  Varint.encode_uint64 buf (Int64.of_int (String.length s));
  Buffer.add_string buf s
;;

let add_u8 buf n = Buffer.add_uint8 buf (n land 0xff)

let add_int64_le buf x =
  for k = 0 to 7 do
    add_u8 buf (Int64.to_int (Int64.logand (Int64.shift_right_logical x (k * 8)) 0xFFL))
  done
;;

let add_default buf (dv : Row.default_value) =
  add_u8 buf (default_tag dv);
  match dv with
  | Row.DV_null | Row.DV_current_timestamp | Row.DV_current_date | Row.DV_current_time ->
    ()
  | Row.DV_int n -> add_int64_le buf n
  | Row.DV_real f -> add_int64_le buf (Int64.bits_of_float f)
  | Row.DV_text s -> add_str buf s
  | Row.DV_blob b -> add_str buf (Bytes.to_string b)
;;

let add_column buf (c : Row.column) =
  add_str buf c.name;
  add_u8 buf (type_tag c.ty);
  add_u8 buf (if c.not_null then 1 else 0);
  add_u8 buf (if c.primary_key then 1 else 0);
  (match c.default with
   | None -> add_u8 buf 0
   | Some dv ->
     add_u8 buf 1;
     add_default buf dv);
  (match c.check_sql with
   | None -> add_u8 buf 0
   | Some sql ->
     add_u8 buf 1;
     add_str buf sql);
  match c.generated_as with
  | None -> add_u8 buf 0
  | Some (expr, is_stored) ->
    add_u8 buf 1;
    add_u8 buf (if is_stored then 1 else 0);
    add_str buf expr
;;

let canonical ~columns ~without_rowid =
  let buf = Buffer.create 64 in
  add_u8 buf canon_version;
  add_u8 buf (if without_rowid then 1 else 0);
  Varint.encode_uint64 buf (Int64.of_int (List.length columns));
  List.iter (add_column buf) columns;
  Buffer.to_bytes buf
;;

let compute ~columns ~without_rowid =
  fnv64_bytes fnv64_offset (canonical ~columns ~without_rowid)
;;

let low32 (x : int64) : int32 = Int64.to_int32 (Int64.logand x 0xFFFFFFFFL)

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
