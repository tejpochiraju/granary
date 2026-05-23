(** Execute write operations against the store and catalog. *)

(** Transaction mode for DML operations.
    [Auto] wraps each DML operation in its own RW transaction (autocommit).
    [In_txn tx] reuses an externally managed transaction; the caller is
    responsible for committing or rolling back. *)
type txn_mode =
  | Auto
  | In_txn of Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn

val execute :
  ?mode:txn_mode ->
  ?clock:(unit -> float) option ->
  ?params:Sqlocaml_encoding.Row.value array ->
  ?before_hook:(tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn ->
                new_row:Sqlocaml_encoding.Row.t option ->
                old_row:Sqlocaml_encoding.Row.t option ->
                unit Lwt.t) option ->
  ?after_hook:(tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn ->
               new_row:Sqlocaml_encoding.Row.t option ->
               old_row:Sqlocaml_encoding.Row.t option ->
               unit Lwt.t) option ->
  ?on_replace_delete_before:(tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn ->
                             old_row:Sqlocaml_encoding.Row.t -> unit Lwt.t) option ->
  ?on_replace_delete:(tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn ->
                      old_row:Sqlocaml_encoding.Row.t -> unit Lwt.t) option ->
  ?on_upsert_update_before:(tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn ->
                            old_row:Sqlocaml_encoding.Row.t ->
                            new_row:Sqlocaml_encoding.Row.t ->
                            unit Lwt.t) option ->
  ?on_upsert_update:(tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn ->
                     old_row:Sqlocaml_encoding.Row.t ->
                     new_row:Sqlocaml_encoding.Row.t ->
                     unit Lwt.t) option ->
  Sqlocaml_store.Store.t ->
  Sqlocaml_catalog.Catalog.t ->
  Plan.op ->
  unit Lwt.t

(** Like [execute], but returns the rows-affected count.  For DDL ops
    (CREATE TABLE, CREATE INDEX) this is [0]; for INSERT it is [1]; for
    UPDATE it is the number of rows whose contents were modified.
    [Op_begin], [Op_commit], and [Op_rollback] are not handled here —
    they raise [Failure] if passed; the [Db] layer intercepts them. *)
val execute_with_count :
  ?mode:txn_mode ->
  ?clock:(unit -> float) option ->
  ?params:Sqlocaml_encoding.Row.value array ->
  ?before_hook:(tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn ->
                new_row:Sqlocaml_encoding.Row.t option ->
                old_row:Sqlocaml_encoding.Row.t option ->
                unit Lwt.t) option ->
  ?after_hook:(tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn ->
               new_row:Sqlocaml_encoding.Row.t option ->
               old_row:Sqlocaml_encoding.Row.t option ->
               unit Lwt.t) option ->
  ?on_replace_delete_before:(tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn ->
                             old_row:Sqlocaml_encoding.Row.t -> unit Lwt.t) option ->
  ?on_replace_delete:(tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn ->
                      old_row:Sqlocaml_encoding.Row.t -> unit Lwt.t) option ->
  ?on_upsert_update_before:(tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn ->
                            old_row:Sqlocaml_encoding.Row.t ->
                            new_row:Sqlocaml_encoding.Row.t ->
                            unit Lwt.t) option ->
  ?on_upsert_update:(tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn ->
                     old_row:Sqlocaml_encoding.Row.t ->
                     new_row:Sqlocaml_encoding.Row.t ->
                     unit Lwt.t) option ->
  Sqlocaml_store.Store.t ->
  Sqlocaml_catalog.Catalog.t ->
  Plan.op ->
  int Lwt.t

(** Execute read operations; returns a lazy stream of result rows.
    [params] are the positional parameter values for [?] placeholders.
    [clock] supplies the current Unix timestamp for SQL date/time
    functions invoked with the literal ['now']. *)
val query :
  ?mode:txn_mode ->
  ?clock:(unit -> float) option ->
  ?params:Sqlocaml_encoding.Row.value array ->
  Sqlocaml_store.Store.t ->
  Sqlocaml_catalog.Catalog.t ->
  Plan.op ->
  Sqlocaml_encoding.Row.t Lwt_stream.t Lwt.t

(** Evaluate a [Plan.expr] against a row.  Exposed primarily for testing
    the rich expression language directly without round-tripping through
    SQL.  Failures (e.g. division by zero, type-mismatched arithmetic)
    raise [Failure]. *)
val eval_expr :
  (unit -> float) option ->
  Sqlocaml_encoding.Row.value array ->
  Sqlocaml_encoding.Row.t ->
  Plan.expr ->
  Sqlocaml_encoding.Row.value
