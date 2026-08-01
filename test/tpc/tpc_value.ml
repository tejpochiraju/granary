type t =
  | VInt of int
  | VReal of float
  | VText of string
  | VNull

let literal = function
  | VInt i -> string_of_int i
  | VNull -> "NULL"
  | VReal f ->
    (* A non-finite REAL has no SQL literal. [%.17g] renders nan/infinity as
       "nan"/"inf", neither of which contains '.', 'e' or 'E', so the
       whole-valued branch below would append ".0" and emit `nan.0` / `inf.0`
       — text granary parses as something else entirely, or rejects at a
       point far from the generator that produced the value. Unreachable from
       the current generators; guarded anyway because this module's standard
       is that a broken value raises rather than becoming a plausible one. *)
    if not (Float.is_finite f)
    then
      invalid_arg
        (Printf.sprintf "Tpc_value.literal: %s has no SQL literal" (Float.to_string f));
    let s = Printf.sprintf "%.17g" f in
    if String.exists (fun c -> c = '.' || c = 'e' || c = 'E') s then s else s ^ ".0"
  | VText s ->
    let buf = Buffer.create (String.length s + 2) in
    Buffer.add_char buf '\'';
    String.iter
      (fun c -> if c = '\'' then Buffer.add_string buf "''" else Buffer.add_char buf c)
      s;
    Buffer.add_char buf '\'';
    Buffer.contents buf
;;

let pp fmt = function
  | VInt i -> Format.fprintf fmt "VInt %d" i
  | VReal f -> Format.fprintf fmt "VReal %g" f
  | VText s -> Format.fprintf fmt "VText %S" s
  | VNull -> Format.fprintf fmt "VNull"
;;
