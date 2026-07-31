module type ENGINE = sig
  type t

  val name : string
  val open_db : dir:string -> t
  val exec : t -> string -> unit
  val query_rows : t -> string -> string list list
  val close : t -> unit
end

let cpu_now () =
  let t = Unix.times () in
  t.Unix.tms_utime +. t.Unix.tms_stime
;;

let time_it f =
  let w0 = Unix.gettimeofday () in
  let c0 = cpu_now () in
  let v = f () in
  let wall = Unix.gettimeofday () -. w0 in
  let cpu = cpu_now () -. c0 in
  v, wall, cpu
;;

let rel_epsilon = 1e-9
let abs_floor = 1e-6

let real_eq a b =
  if a = b
  then true (* identical infinities compare equal here; NaN <> NaN so falls through *)
  else if (not (Float.is_finite a)) || not (Float.is_finite b)
  then false (* NaN, or mismatched/one-sided infinities *)
  else (
    let d = Float.abs (a -. b) in
    if d <= abs_floor
    then true
    else (
      let scale = Float.max (Float.abs a) (Float.abs b) in
      d <= rel_epsilon *. scale))
;;

let env_int key default =
  match Sys.getenv_opt key with
  | Some s ->
    (try int_of_string s with
     | _ -> default)
  | None -> default
;;

let env_float key default =
  match Sys.getenv_opt key with
  | Some s ->
    (try float_of_string s with
     | _ -> default)
  | None -> default
;;

let env_str key default =
  match Sys.getenv_opt key with
  | Some s when s <> "" -> s
  | _ -> default
;;

let host_label () =
  env_str
    "GRANARY_TPC_HOST"
    (try Unix.gethostname () with
     | _ -> "unknown")
;;

module Csv = struct
  let needs_quoting s =
    String.exists
      (function
        | ',' | '"' | '\n' | '\r' -> true
        | _ -> false)
      s
  ;;

  let quote s =
    let buf = Buffer.create (String.length s + 2) in
    Buffer.add_char buf '"';
    String.iter
      (fun c -> if c = '"' then Buffer.add_string buf "\"\"" else Buffer.add_char buf c)
      s;
    Buffer.add_char buf '"';
    Buffer.contents buf
  ;;

  let field s = if needs_quoting s then quote s else s
  let row fields = String.concat "," (List.map field fields)
  let header names = row names
end
