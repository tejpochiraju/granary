(** Bitmap helpers for null-mask encoding.

    Used by {!Granary_columnar.Col} for columnar null bitmaps.  Intended to
    also replace the inline bitmap logic in {!Granary_encoding.Row} once the
    API is adapted (see issue #372). *)

module Bigarray = Bigarray

(** [pack_bits bits n] packs the first [n] elements of [bits] into a
    big-endian bitmask byte string.  Each byte encodes 8 entries: bit 0 of
    byte 0 = bits[0], bit 1 = bits[1], etc. *)
val pack_bits
  :  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  -> int
  -> bytes

(** [pack_bits_into buf bits n] writes a packed bitmask directly into [buf],
    one byte per 8 entries, without materialising an intermediate [Bytes.t]. *)
val pack_bits_into
  :  Buffer.t
  -> (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  -> int
  -> unit

(** [unpack_bits buf off n] reads [ceil(n/8)] bytes from [buf] starting at
    [off] and unpacks them into a fresh bigarray of length [n]. *)
val unpack_bits
  :  bytes
  -> int
  -> int
  -> (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(** [pack_bits_of_bools is_null n] packs the predicate [is_null] applied to
    indices [0, n-1] into a big-endian bitmask byte string.  Each byte encodes
    8 entries: bit 0 of byte 0 = is_null(0), bit 1 = is_null(1), etc. *)
val pack_bits_of_bools : (int -> bool) -> int -> bytes

(** [unpack_bits_to_bools buf off n] reads [ceil(n/8)] bytes from [buf] at
    [off] and returns a [bool array] of length [n] where element [i] is [true]
    iff bit [i] of the bitmask is set. *)
val unpack_bits_to_bools : bytes -> int -> int -> bool array
