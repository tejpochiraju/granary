(** Order-preserving int64 codec.
    Byte-wise comparison of encoded values yields the same ordering
    as [Int64.compare] on the original int64 values. *)

val encode : int64 -> bytes
val decode : bytes -> int64
