(** Order-preserving encoding of SQL values for use as B+-tree index keys.
    Encoded bytes compare correctly under Bytes.compare for all supported types.
    NULL sorts before all non-null values.
    Type ordering: NULL < INTEGER < REAL < TEXT < BLOB

    A NaN [IK_real] is the one exception to "one value, one type region": since
    #578 it gets its own single-byte tag, sorting between NULL and INTEGER —
    matching {!Granary_sql.Exec.compare_values}'s [NULL < NaN < every number]
    exactly, rather than merely "both below every number" as it did before
    #578 (when NaN shared NULL's [0x00] byte and the two were indistinguishable
    to a byte-equality probe such as a UNIQUE conflict check). *)

type value =
  | IK_null
  | IK_int of int64
  | IK_real of float
  | IK_text of string
  | IK_blob of bytes

(** Encode a single value. Result is order-preserving under Bytes.compare. *)
val encode_value : value -> bytes

(** Encode a multi-column key with a rowid suffix.
    Result: concat of encode_value for each column + rowid suffix (8 bytes). *)
val encode : value list -> rowid:int64 -> bytes

(** Decode a key produced by [encode]. Returns (column_values, rowid). *)
val decode : bytes -> (value list * int64, string) result
