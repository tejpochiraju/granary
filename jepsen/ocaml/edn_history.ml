(** EDN-format writer for Jepsen operation histories.

    Jepsen uses one EDN map per line (not a top-level vector).
    Each entry represents either a client operation
    ({:type :invoke/:ok/:fail}) or a nemesis event ({:type :info}).

    See https://github.com/jepsen-io/jepsen for the format spec. *)

type op_type = Invoke | Ok | Fail | Info

type value =
  | Txn of txn_op list           (** list-append / mixed workloads *)
  | Add of int * int             (** counter: [k, delta] *)
  | Read of int * int option     (** counter: [k, expected?] *)
  | Transfer of int * int * int  (** bank: [from, to, amount] *)
  | BankRead of (int * int64) list  (** bank read: [(account_id, balance)] *)
  | SetAdd of int                (** set: [element] *)
  | SetRead of int list          (** set read: [elements] *)
  | Nemesis of string            (** nemesis event name *)

and txn_op =
  | Append of int * int          (** key, value *)
  | Read of int * int list       (** key, values seen *)

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
  | BankRead balances ->
    Format.fprintf ppf "[";
    List.iteri (fun i (id, bal) ->
      if i > 0 then Format.fprintf ppf " ";
      Format.fprintf ppf "[%d %Ld]" id bal) balances;
    Format.fprintf ppf "]"
  | SetAdd e -> Format.fprintf ppf "%d" e
  | SetRead es ->
    Format.fprintf ppf "[";
    List.iteri (fun i e ->
      if i > 0 then Format.fprintf ppf " ";
      Format.fprintf ppf "%d" e) es;
    Format.fprintf ppf "]"
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

(** Convenience: create an invoke entry with the current timestamp. *)
let make_invoke ~f ~value ~process ~index =
  let time = Int64.of_float (Unix.gettimeofday () *. 1e9) in
  { typ = Invoke; f; value; process; index; time_ns = time }

(** Convenience: create an ok/fail entry with timestamp. *)
let make_result ~typ ~f ~value ~process ~index =
  let time = Int64.of_float (Unix.gettimeofday () *. 1e9) in
  { typ; f; value; process; index; time_ns = time }

(** Convenience: create a nemesis info entry. *)
let make_nemesis ~nemesis_name ~process ~index =
  let time = Int64.of_float (Unix.gettimeofday () *. 1e9) in
  { typ = Info; f = nemesis_name; value = Nemesis nemesis_name;
    process; index; time_ns = time }
