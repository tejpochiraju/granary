(** A granary connection the TPC-C driver can drive asynchronously (#500).

    {!Granary_engine} cannot serve the driver: every one of its operations
    calls [Lwt_main.run] internally, and {!Tpcc_driver.run} is itself an Lwt
    computation, so using it inside a terminal would nest [Lwt_main.run] in
    [Lwt_main.run]. This module wraps the same [Db.t] the other way round —
    Lwt-native operations, with the synchronous {!Bench_report.ENGINE} view
    kept only for the load phase, which runs before any terminal exists.

    One connection is one [Db.t] and therefore one explicit-transaction slot.
    Handing the same connection to two of {!Tpcc_driver.run}'s workers would
    reintroduce exactly the hazard that driver's pool exists to prevent — see
    its module header. *)

(** The synchronous view, used by [Tpcc_schema.Load] during the untimed load
    phase. Its [open_db ~dir] creates [tpcc.db] beneath [dir], removing any
    previous database and WAL there. *)
include Bench_report.ENGINE

(** [pp fmt t] prints the connection's database path, for debugging and test
    failure messages. *)
val pp : Format.formatter -> t -> unit

(** [ops t] is the Lwt-native operation record the transaction profiles run
    against — no [Lwt_main.run] anywhere beneath it, so it is safe inside a
    driver terminal. *)
val ops : t -> Tpcc_txn.ops
