(* Column names and order here MUST match Tpch_gen.column_names exactly —
   verified by test/test_tpch_load.ml's "columns match Tpch_gen.column_names"
   case, which parses each CREATE TABLE statement below and diffs it against
   Tpch_gen.column_names ~table. *)
let ddl =
  [ {|CREATE TABLE region (
       r_regionkey INTEGER PRIMARY KEY,
       r_name      TEXT NOT NULL,
       r_comment   TEXT)|}
  ; {|CREATE TABLE nation (
       n_nationkey INTEGER PRIMARY KEY,
       n_name      TEXT NOT NULL,
       n_regionkey INTEGER NOT NULL,
       n_comment   TEXT)|}
  ; {|CREATE TABLE supplier (
       s_suppkey   INTEGER PRIMARY KEY,
       s_name      TEXT NOT NULL,
       s_address   TEXT NOT NULL,
       s_nationkey INTEGER NOT NULL,
       s_phone     TEXT NOT NULL,
       s_acctbal   REAL NOT NULL,
       s_comment   TEXT)|}
  ; {|CREATE TABLE part (
       p_partkey     INTEGER PRIMARY KEY,
       p_name        TEXT NOT NULL,
       p_mfgr        TEXT NOT NULL,
       p_brand       TEXT NOT NULL,
       p_type        TEXT NOT NULL,
       p_size        INTEGER NOT NULL,
       p_container   TEXT NOT NULL,
       p_retailprice REAL NOT NULL,
       p_comment     TEXT)|}
  ; {|CREATE TABLE partsupp (
       ps_partkey    INTEGER NOT NULL,
       ps_suppkey    INTEGER NOT NULL,
       ps_availqty   INTEGER NOT NULL,
       ps_supplycost REAL NOT NULL,
       ps_comment    TEXT)|}
  ; {|CREATE TABLE customer (
       c_custkey    INTEGER PRIMARY KEY,
       c_name       TEXT NOT NULL,
       c_address    TEXT NOT NULL,
       c_nationkey  INTEGER NOT NULL,
       c_phone      TEXT NOT NULL,
       c_acctbal    REAL NOT NULL,
       c_mktsegment TEXT NOT NULL,
       c_comment    TEXT)|}
  ; {|CREATE TABLE orders (
       o_orderkey      INTEGER PRIMARY KEY,
       o_custkey       INTEGER NOT NULL,
       o_orderstatus   TEXT NOT NULL,
       o_totalprice    REAL NOT NULL,
       o_orderdate     TEXT NOT NULL,
       o_orderpriority TEXT NOT NULL,
       o_clerk         TEXT NOT NULL,
       o_shippriority  INTEGER NOT NULL,
       o_comment       TEXT)|}
  ; {|CREATE TABLE lineitem (
       l_orderkey      INTEGER NOT NULL,
       l_partkey       INTEGER NOT NULL,
       l_suppkey       INTEGER NOT NULL,
       l_linenumber    INTEGER NOT NULL,
       l_quantity      REAL    NOT NULL,
       l_extendedprice REAL    NOT NULL,
       l_discount      REAL    NOT NULL,
       l_tax           REAL    NOT NULL,
       l_returnflag    TEXT    NOT NULL,
       l_linestatus    TEXT    NOT NULL,
       l_shipdate      TEXT    NOT NULL,
       l_commitdate    TEXT    NOT NULL,
       l_receiptdate   TEXT    NOT NULL,
       l_shipinstruct  TEXT    NOT NULL,
       l_shipmode      TEXT    NOT NULL,
       l_comment       TEXT)|}
  ]
;;

let indexes =
  [ "CREATE INDEX idx_lineitem_orderkey ON lineitem (l_orderkey)"
  ; "CREATE INDEX idx_lineitem_partkey ON lineitem (l_partkey)"
  ; "CREATE INDEX idx_lineitem_suppkey ON lineitem (l_suppkey)"
  ; "CREATE INDEX idx_lineitem_shipdate ON lineitem (l_shipdate)"
  ; "CREATE INDEX idx_orders_custkey ON orders (o_custkey)"
  ; "CREATE INDEX idx_orders_orderdate ON orders (o_orderdate)"
  ; "CREATE INDEX idx_partsupp_partkey ON partsupp (ps_partkey)"
  ; "CREATE INDEX idx_partsupp_suppkey ON partsupp (ps_suppkey)"
  ; "CREATE INDEX idx_customer_nationkey ON customer (c_nationkey)"
  ; "CREATE INDEX idx_supplier_nationkey ON supplier (s_nationkey)"
  ]
;;

let literal = function
  | Tpch_gen.VInt i -> string_of_int i
  | Tpch_gen.VReal f ->
    (* granary types a literal by its text: "100" is an INTEGER literal and a
       REAL column rejects it under strict column typing, so a whole-valued
       REAL has to keep a fractional part. *)
    let s = Printf.sprintf "%.17g" f in
    if String.exists (fun c -> c = '.' || c = 'e' || c = 'E') s then s else s ^ ".0"
  | Tpch_gen.VText s ->
    let buf = Buffer.create (String.length s + 2) in
    Buffer.add_char buf '\'';
    String.iter
      (fun c -> if c = '\'' then Buffer.add_string buf "''" else Buffer.add_char buf c)
      s;
    Buffer.add_char buf '\'';
    Buffer.contents buf
;;

module Load (E : Bench_report.ENGINE) = struct
  (* Clamped to at least 1: 0 would raise Division_by_zero below, and a
     negative value would silently disable batching instead of erroring. *)
  let batch_size = max 1 (Bench_report.env_int "GRANARY_TPCH_BATCH" 500)

  let flush engine ~table ~cols pending =
    if pending <> []
    then (
      let values =
        List.rev_map
          (fun row ->
             "(" ^ String.concat "," (List.map literal (Array.to_list row)) ^ ")")
          pending
      in
      E.exec
        engine
        (Printf.sprintf
           "INSERT INTO %s (%s) VALUES %s"
           table
           (String.concat "," cols)
           (String.concat "," values)))
  ;;

  let load_table engine gen ~table =
    let cols = Tpch_gen.column_names ~table in
    E.exec engine "BEGIN";
    let pending = ref []
    and n = ref 0 in
    Tpch_gen.iter_rows gen ~table ~f:(fun row ->
      pending := row :: !pending;
      incr n;
      if !n mod batch_size = 0
      then (
        flush engine ~table ~cols !pending;
        pending := []));
    flush engine ~table ~cols !pending;
    E.exec engine "COMMIT"
  ;;

  let run engine gen =
    List.iter (E.exec engine) ddl;
    List.iter (fun table -> load_table engine gen ~table) Tpch_gen.tables;
    List.iter (E.exec engine) indexes
  ;;
end
