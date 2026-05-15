type ty =
  | Integer
  | Text
  | Real
  | Blob

type column = { name : string; ty : ty }
type schema = column list

type value =
  | V_int  of int64
  | V_text of string
  | V_null
  | V_real of float
  | V_blob of bytes

type t = value array

val equal : t -> t -> bool
val encode : schema -> t -> bytes
val decode : schema -> bytes -> t
