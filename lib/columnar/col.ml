open Bigarray
module Row = Sqlocaml_encoding.Row

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

let length = function
  | I64 a -> Array1.dim a
  | F64 a -> Array1.dim a
  | Bool a -> Array1.dim a
  | Sym s -> Array1.dim s.codes
  | Str a -> Array.length a
;;

let morsel_size = 1024

let create_for_type (ty : Row.ty) n =
  match ty with
  | Row.Integer -> I64 (Array1.create int64 c_layout n)
  | Row.Real -> F64 (Array1.create float64 c_layout n)
  | Row.Blob -> Str (Array.make n "")
  | Row.Text -> Str (Array.make n "")
;;

let value_of_col col i =
  match col with
  | I64 a -> Row.V_int a.{i}
  | F64 a -> Row.V_real a.{i}
  | Bool a -> Row.V_int (Int64.of_int a.{i})
  | Sym s ->
    let code = Int32.to_int s.codes.{i} in
    Row.V_text s.dict.(code)
  | Str a -> Row.V_text a.(i)
;;
