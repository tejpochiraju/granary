type ty =
  | Integer
  | Text

type column = { name : string; ty : ty }
type schema = column list

type value =
  | V_int of int64
  | V_text of string
  | V_null

type t = value array

val equal : t -> t -> bool
val encode : schema -> t -> bytes
val decode : schema -> bytes -> t
