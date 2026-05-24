(** Row schema and value (de)serialisation.

    Defines column types ({!ty}), DEFAULT/CHECK/GENERATED column metadata
    ({!column}, {!schema}) and the runtime cell {!value}s of a row ({!t}),
    plus the binary [encode]/[decode] of a row against its schema. *)

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
  | DV_current_timestamp
  | DV_current_date
  | DV_current_time

type column = {
  name         : string;
  ty           : ty;
  not_null     : bool;
  primary_key  : bool;
  default      : default_value option;  (* None = no DEFAULT *)
  check_sql    : string option;         (* None = no CHECK constraint *)
  generated_as : (string * bool) option;
  (** Some (expr_sql, is_stored): GENERATED ALWAYS AS expr.
      is_stored=true => STORED; false => VIRTUAL (both computed at write time). *)
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
