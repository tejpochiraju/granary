(** Shared sqlocaml demo workload (#403).

    Exercised by both the sample MirageOS unikernel ([mirage/unikernel.ml]) and
    the host smoke test ([test/test_mirage_unikernel_smoke.ml]) so the two
    package the {b same} engine workload over a {!Sqlocaml_store.Store} opened in
    WAL mode. *)

(** [run_demo db] runs a small, deterministic workload against [db]: creates a
    table, inserts three rows inside an explicit transaction, commits, and reads
    the rows back.  On success it returns the number of rows read back (always
    [3]).

    The explicit [BEGIN]/[COMMIT] drives the engine's WAL fsync / commit path,
    so the on-disk byte ordering of pages and WAL frames is exercised
    end-to-end — the arch-neutral concern behind #402/#403. *)
val run_demo : Sqlocaml.Db.t -> (int, Sqlocaml.Db.error) result Lwt.t
