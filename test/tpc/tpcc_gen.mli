(** Deterministic TPC-C-derived data generator (#500).

    Emits the nine TPC-C tables for a given warehouse count, streaming rows to
    a callback so a large population never has to be held in memory. Output is
    a pure function of [seed] and [warehouses], which is what makes benchmark
    numbers comparable across runs and machines.

    Every table scales with the warehouse count except ["item"], which the spec
    fixes at 100,000 rows regardless of scale.

    Money columns are {!Tpc_value.VReal}: neither granary nor SQLite has
    DECIMAL, and using scaled integers on one side only would make the
    cross-engine comparison misleading. Timestamps are {!Tpc_value.VText} — the
    whole initial population shares one load timestamp, since TPC-C gives the
    spread of load times no meaning.

    The generated state already satisfies the TPC-C consistency conditions:
    [w_ytd] is the sum of its districts' [d_ytd]; [d_next_o_id - 1] is the
    district's maximum [o_id] and its maximum [no_o_id]; the new_order rows of
    a district are a contiguous run of 900 ids; and the sum of [o_ol_cnt] over
    orders is the order_line row count. *)

(** Generator context: the seed and the warehouse count. *)
type t

(** The spec's cardinality constants, exported so that every consumer derives
    from them instead of re-spelling the literal.

    {!Tpcc_txn} draws its transaction inputs against exactly these numbers —
    a district id in [\[1, districts_per_warehouse\]], a customer id in
    [\[1, customers_per_district\]], an item id in [\[1, items\]] — and the
    load and the workload must agree, or the workload targets rows that were
    never generated and the benchmark silently measures misses instead of
    hits. They are exported for the same reason {!c_load} is: a change here
    must retarget the workload rather than let the two drift apart in
    silence. Consumers must not re-derive these as literals. *)

(** Districts per warehouse: the spec's 10. *)
val districts_per_warehouse : int

(** Customers per district: the spec's 3,000. *)
val customers_per_district : int

(** Orders per district: the spec's 3,000, one per customer. *)
val orders_per_district : int

(** Rows in ["item"]: the spec's 100,000, independent of the warehouse count.
    An id outside [\[1, items\]] therefore matches no row, which is what
    {!Tpcc_txn.invalid_item_id} is built from. *)
val items : int

(** The lowest [o_id] left undelivered by the initial load: orders from this
    id up carry no carrier id and own the district's [new_order] rows. With
    {!orders_per_district} this fixes the 900-row new-order queue per
    district that consistency condition 3 checks. *)
val first_undelivered_o_id : int

(** The nine TPC-C table names, in load order (parents before children). The
    single source of truth for the table list — {!Tpcc_schema.tables} and
    {!Tpcc_schema.Load} both derive from this rather than keeping their own
    copy, so the two cannot drift apart. *)
val tables : string list

(** [pp fmt t] prints the context's seed and warehouse count, for debugging
    only. *)
val pp : Format.formatter -> t -> unit

(** [create ~seed ~warehouses] builds a context for [warehouses] warehouses.
    Raises [Invalid_argument] if [warehouses < 1]. *)
val create : seed:int -> warehouses:int -> t

(** [warehouses t] is the warehouse count [t] was created with — the TPC-C
    scale factor. *)
val warehouses : t -> int

(** [row_count t ~table] is the number of rows [iter_rows] will emit. For
    ["order_line"] this replays the order stream to sum [o_ol_cnt], so it is
    not free. Raises [Invalid_argument] on an unknown table name. *)
val row_count : t -> table:string -> int

(** [iter_rows t ~table ~f] calls [f] once per generated row, with the row's
    columns in schema declaration order. Each table draws from its own stream,
    so generating one table alone yields the same rows as generating it inside
    a full load. Raises [Invalid_argument] on an unknown table name. *)
val iter_rows : t -> table:string -> f:(Tpc_value.t array -> unit) -> unit

(** [column_names ~table] is the schema declaration order for [table],
    matching the array layout [iter_rows] produces. Raises [Invalid_argument]
    on an unknown table name. *)
val column_names : table:string -> string list

(** The spec's C_LOAD: the load-time NURand constant this generator uses to
    draw [c_last] for customers past the first 1,000 of a district (see
    [customer_last_name] in the implementation). Fixed at the spec's own
    load-time value rather than threaded through as a parameter, so callers
    cannot accidentally drift from it.

    A transaction driver must NOT reuse this value as its [c_last] {e run}
    constant: TPC-C 2.1.6.1 requires the two to differ by a delta in
    [\[65,119\]] excluding 96 and 112, precisely so the run's hot surnames do
    not coincide with the load's. {!Tpcc_txn.default_run_constants} derives a
    conforming run constant from this one. Nothing breaks if they differ,
    either: the first 1,000 customers of every district take
    [last_name (c_id - 1)], so all 1,000 surnames exist in every district and
    any run constant resolves to real rows. *)
val c_load : int
