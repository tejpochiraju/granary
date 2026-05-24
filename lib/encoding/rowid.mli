(** Order-preserving int64 codec.
    Byte-wise comparison of encoded values yields the same ordering
    as [Int64.compare] on the original int64 values. *)

(** Encode an int64 to its order-preserving byte representation. *)
val encode : int64 -> bytes

(** Decode an order-preserving byte representation back to its int64. *)
val decode : bytes -> int64
