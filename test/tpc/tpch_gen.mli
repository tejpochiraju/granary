(** Deterministic TPC-H-derived data generator (#482).

    Emits the eight TPC-H tables at a given scale factor, streaming rows to a
    callback so a large dataset never has to be held in memory. Output is a
    pure function of [seed] and [sf], which is what makes benchmark numbers
    comparable across runs and machines.

    Money columns are [VReal]: neither granary nor SQLite has DECIMAL, and
    using scaled integers on one side only would make the cross-engine answer
    comparison misleading. Dates are [VText] in ['YYYY-MM-DD'] form, which
    orders correctly under lexicographic comparison. *)

(** A generated column value. *)
type value =
  | VInt of int (** INTEGER column *)
  | VReal of float (** REAL column — money and rates *)
  | VText of string (** TEXT column — names, codes, dates, comments *)

(** Generator context: seed, scale factor, and the shared text pool. *)
type t

(** [pp fmt t] prints the context's seed and scale factor, for debugging
    only — the text pool is elided. *)
val pp : Format.formatter -> t -> unit

(** [create ~seed ~sf] builds a context at scale factor [sf]. Raises
    [Invalid_argument] if [sf <= 0.0]. *)
val create : seed:int -> sf:float -> t

(** The eight table names in load order — parents before children, so a
    foreign-key-respecting loader can follow this list directly. *)
val tables : string list

(** [row_count t ~table] is the number of rows [iter_rows] will emit. For
    ["lineitem"] this requires generating the order stream, so it is not
    free. Raises [Invalid_argument] on an unknown table name. *)
val row_count : t -> table:string -> int

(** [iter_rows t ~table ~f] calls [f] once per generated row, with the row's
    columns in schema declaration order. Raises [Invalid_argument] on an
    unknown table name. *)
val iter_rows : t -> table:string -> f:(value array -> unit) -> unit

(** [column_names ~table] is the schema declaration order for [table],
    matching the array layout [iter_rows] produces. Raises
    [Invalid_argument] on an unknown table name. *)
val column_names : table:string -> string list
