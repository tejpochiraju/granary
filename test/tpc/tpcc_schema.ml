let ddl =
  [ {|CREATE TABLE warehouse (
       w_id       INTEGER PRIMARY KEY,
       w_name     TEXT NOT NULL,
       w_street_1 TEXT NOT NULL,
       w_street_2 TEXT NOT NULL,
       w_city     TEXT NOT NULL,
       w_state    TEXT NOT NULL,
       w_zip      TEXT NOT NULL,
       w_tax      REAL NOT NULL,
       w_ytd      REAL NOT NULL)|}
  ; {|CREATE TABLE district (
       d_id        INTEGER NOT NULL,
       d_w_id      INTEGER NOT NULL,
       d_name      TEXT NOT NULL,
       d_street_1  TEXT NOT NULL,
       d_street_2  TEXT NOT NULL,
       d_city      TEXT NOT NULL,
       d_state     TEXT NOT NULL,
       d_zip       TEXT NOT NULL,
       d_tax       REAL NOT NULL,
       d_ytd       REAL NOT NULL,
       d_next_o_id INTEGER NOT NULL,
       PRIMARY KEY (d_w_id, d_id))|}
  ; {|CREATE TABLE customer (
       c_id           INTEGER NOT NULL,
       c_d_id         INTEGER NOT NULL,
       c_w_id         INTEGER NOT NULL,
       c_first        TEXT NOT NULL,
       c_middle       TEXT NOT NULL,
       c_last         TEXT NOT NULL,
       c_street_1     TEXT NOT NULL,
       c_street_2     TEXT NOT NULL,
       c_city         TEXT NOT NULL,
       c_state        TEXT NOT NULL,
       c_zip          TEXT NOT NULL,
       c_phone        TEXT NOT NULL,
       c_since        TEXT NOT NULL,
       c_credit       TEXT NOT NULL,
       c_credit_lim   REAL NOT NULL,
       c_discount     REAL NOT NULL,
       c_balance      REAL NOT NULL,
       c_ytd_payment  REAL NOT NULL,
       c_payment_cnt  INTEGER NOT NULL,
       c_delivery_cnt INTEGER NOT NULL,
       c_data         TEXT NOT NULL,
       PRIMARY KEY (c_w_id, c_d_id, c_id))|}
  ; {|CREATE TABLE history (
       h_c_id   INTEGER NOT NULL,
       h_c_d_id INTEGER NOT NULL,
       h_c_w_id INTEGER NOT NULL,
       h_d_id   INTEGER NOT NULL,
       h_w_id   INTEGER NOT NULL,
       h_date   TEXT NOT NULL,
       h_amount REAL NOT NULL,
       h_data   TEXT NOT NULL)|}
  ; {|CREATE TABLE item (
       i_id    INTEGER PRIMARY KEY,
       i_im_id INTEGER NOT NULL,
       i_name  TEXT NOT NULL,
       i_price REAL NOT NULL,
       i_data  TEXT NOT NULL)|}
  ; {|CREATE TABLE stock (
       s_i_id       INTEGER NOT NULL,
       s_w_id       INTEGER NOT NULL,
       s_quantity   INTEGER NOT NULL,
       s_dist_01    TEXT NOT NULL,
       s_dist_02    TEXT NOT NULL,
       s_dist_03    TEXT NOT NULL,
       s_dist_04    TEXT NOT NULL,
       s_dist_05    TEXT NOT NULL,
       s_dist_06    TEXT NOT NULL,
       s_dist_07    TEXT NOT NULL,
       s_dist_08    TEXT NOT NULL,
       s_dist_09    TEXT NOT NULL,
       s_dist_10    TEXT NOT NULL,
       s_ytd        INTEGER NOT NULL,
       s_order_cnt  INTEGER NOT NULL,
       s_remote_cnt INTEGER NOT NULL,
       s_data       TEXT NOT NULL,
       PRIMARY KEY (s_w_id, s_i_id))|}
  ; {|CREATE TABLE orders (
       o_id         INTEGER NOT NULL,
       o_d_id       INTEGER NOT NULL,
       o_w_id       INTEGER NOT NULL,
       o_c_id       INTEGER NOT NULL,
       o_entry_d    TEXT NOT NULL,
       o_carrier_id INTEGER,
       o_ol_cnt     INTEGER NOT NULL,
       o_all_local  INTEGER NOT NULL,
       PRIMARY KEY (o_w_id, o_d_id, o_id))|}
  ; {|CREATE TABLE new_order (
       no_o_id INTEGER NOT NULL,
       no_d_id INTEGER NOT NULL,
       no_w_id INTEGER NOT NULL,
       PRIMARY KEY (no_w_id, no_d_id, no_o_id))|}
  ; {|CREATE TABLE order_line (
       ol_o_id        INTEGER NOT NULL,
       ol_d_id        INTEGER NOT NULL,
       ol_w_id        INTEGER NOT NULL,
       ol_number      INTEGER NOT NULL,
       ol_i_id        INTEGER NOT NULL,
       ol_supply_w_id INTEGER NOT NULL,
       ol_delivery_d  TEXT,
       ol_quantity    INTEGER NOT NULL,
       ol_amount      REAL NOT NULL,
       ol_dist_info   TEXT NOT NULL,
       PRIMARY KEY (ol_w_id, ol_d_id, ol_o_id, ol_number))|}
  ]
;;

(* The single source of truth for the table list is Tpcc_gen.tables; keep a
   local alias so callers can still say Tpcc_schema.tables. *)
let tables = Tpcc_gen.tables

(* Payment and OrderStatus look a customer up by last name within a district;
   OrderStatus takes that customer's most recent order.  Without these two the
   read-heavy half of the mix degrades into full scans of a 30,000-row table
   per transaction, which measures scan speed rather than the profile. *)
let indexes =
  [ "CREATE INDEX idx_customer_last ON customer (c_w_id, c_d_id, c_last)"
  ; "CREATE INDEX idx_orders_cust ON orders (o_w_id, o_d_id, o_c_id)"
  ]
;;

module Load (E : Bench_report.ENGINE) = struct
  (* Clamped to at least 1: 0 would raise Division_by_zero below, and a
     negative value would silently disable batching instead of erroring. *)
  let batch_size = max 1 (Bench_report.env_int "GRANARY_TPCC_BATCH" 500)

  let flush engine ~table ~cols pending =
    if pending <> []
    then (
      let values =
        List.rev_map
          (fun row ->
             "("
             ^ String.concat "," (List.map Tpc_value.literal (Array.to_list row))
             ^ ")")
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
    let cols = Tpcc_gen.column_names ~table in
    E.exec engine "BEGIN";
    let pending = ref []
    and n = ref 0 in
    Tpcc_gen.iter_rows gen ~table ~f:(fun row ->
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
    List.iter (fun table -> load_table engine gen ~table) Tpcc_gen.tables;
    List.iter (E.exec engine) indexes
  ;;
end
