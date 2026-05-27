(** EDN-format writer for Jepsen operation histories.

    Jepsen uses one EDN map per line (not a top-level vector).
    Each entry represents either a client operation
    ({:type :invoke/:ok/:fail}) or a nemesis event ({:type :info}).

    See https://github.com/jepsen-io/jepsen for the format spec. *)

type op_type = Invoke | Ok | Fail | Info

type value =
  | Txn of txn_op list       (* list-append / mixed workloads *)
  | Add of int * int         (* counter: [k, delta] *)
  | Read of int * int option (* counter: [k, expected?] *)
  | Transfer of int * int * int  (* bank: [from, to, amount] *)
  | Nemesis of string        (* nemesis event name *)

and txn_op =
  | Append of int * int      (* key, value *)
  | Read of int * int list   (* key, values seen *)

type entry = {
  typ : op_type;
  f : string;
  value : value;
  process : int;
  index : int;
  time_ns : int64;
}

let pp_op_type ppf = function
  | Invoke -> Format.fprintf ppf ":invoke"
  | Ok -> Format.fprintf ppf ":ok"
  | Fail -> Format.fprintf ppf ":fail"
  | Info -> Format.fprintf ppf ":info"

let rec pp_txn_op ppf = function
  | Append (k, v) -> Format.fprintf ppf "[:append %d %d]" k v
  | Read (k, vs) ->
    Format.fprintf ppf "[:r %d [" k;
    List.iteri (fun i v ->
      if i > 0 then Format.fprintf ppf " %d" v
      else Format.fprintf ppf "%d" v) vs;
    Format.fprintf ppf "]]"

and pp_txn_ops ppf ops =
  Format.fprintf ppf "[";
  List.iteri (fun i op ->
    if i > 0 then Format.fprintf ppf " ";
    pp_txn_op ppf op) ops;
  Format.fprintf ppf "]"

let pp_value ppf = function
  | Txn ops -> pp_txn_ops ppf ops
  | Add (k, v) -> Format.fprintf ppf "[%d %d]" k v
  | Read (k, v) ->
    (match v with
     | None -> Format.fprintf ppf "[%d nil]" k
     | Some n -> Format.fprintf ppf "[%d %d]" k n)
  | Transfer (f, t, a) -> Format.fprintf ppf "[%d %d %d]" f t a
  | Nemesis s -> Format.fprintf ppf "%S" s

let pp_entry ppf (e : entry) =
  Format.fprintf ppf
    "{:type %a :f %S :value %a :process %d :index %d :time %Ld}"
    pp_op_type e.typ
    e.f
    pp_value e.value
    e.process
    e.index
    e.time_ns

(** Write an entry to the output channel. *)
let write_entry ch (e : entry) =
  Format.fprintf (Format.formatter_of_out_channel ch) "%a\n%!" pp_entry e

(** Write a full history (list of entries) to a file. *)
let write_history path entries =
  let ch = open_out path in
  List.iter (write_entry ch) entries;
  close_out ch
