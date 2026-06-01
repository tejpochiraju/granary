(** Variable-length integer encoding.
    Uses LEB128 for unsigned, zigzag+LEB128 for signed. *)

(** [encode_uint64 buf n] appends the LEB128 encoding of [n] to [buf]. *)
val encode_uint64 : Buffer.t -> int64 -> unit

(** [decode_uint64 buf off] returns [(value, new_offset)]. *)
val decode_uint64 : Bytes.t -> int -> int64 * int

(** [encode_int64 buf n] appends the zigzag+LEB128 encoding of [n] to [buf]. *)
val encode_int64 : Buffer.t -> int64 -> unit

(** [decode_int64 buf off] returns [(value, new_offset)]. *)
val decode_int64 : Bytes.t -> int -> int64 * int
