open Bigarray

type int64_arr = (int64, int64_elt, c_layout) Array1.t
type float64_arr = (float, float64_elt, c_layout) Array1.t
type int32_arr = (int32, int32_elt, c_layout) Array1.t
type uint8_arr = (int, int8_unsigned_elt, c_layout) Array1.t

type t =
  | I64 of int64_arr
  | F64 of float64_arr
  | Bool of uint8_arr
  | Sym of
      { codes : int32_arr
      ; dict : string array
      }
  | Str of string array

val length : t -> int
val morsel_size : int
val create_for_type : Sqlocaml_encoding.Row.ty -> int -> t
val value_of_col : t -> int -> Sqlocaml_encoding.Row.value
