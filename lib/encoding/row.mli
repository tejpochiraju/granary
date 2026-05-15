type ty =
  | Integer
  | Text
  | Real
  | Blob

type default_value =
  | DV_int  of int64
  | DV_text of string
  | DV_null
  | DV_real of float
  | DV_blob of bytes

type column = {
  name        : string;
  ty          : ty;
  not_null    : bool;
  primary_key : bool;
  default     : default_value option;  (* None = no DEFAULT *)
}
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
