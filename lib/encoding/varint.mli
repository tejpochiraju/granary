(** Variable-length integer encoding.
    Uses LEB128 for unsigned, zigzag+LEB128 for signed. *)

val encode_uint64 : Buffer.t -> int64 -> unit
val decode_uint64 : Bytes.t -> int -> int64 * int
(** [decode_uint64 buf off] returns [(value, new_offset)]. *)

val encode_int64 : Buffer.t -> int64 -> unit
val decode_int64 : Bytes.t -> int -> int64 * int
(** [decode_int64 buf off] returns [(value, new_offset)]. *)
