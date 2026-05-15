(** Execute write operations against the store and catalog. *)
val execute :
  Sqlocaml_store.Store.t ->
  Sqlocaml_catalog.Catalog.t ->
  Plan.op ->
  unit Lwt.t

(** Like [execute], but returns the rows-affected count.  For DDL ops
    (CREATE TABLE, CREATE INDEX) this is [0]; for INSERT it is [1]; for
    UPDATE it is the number of rows whose contents were modified. *)
val execute_with_count :
  Sqlocaml_store.Store.t ->
  Sqlocaml_catalog.Catalog.t ->
  Plan.op ->
  int Lwt.t

(** Execute read operations; returns a lazy stream of result rows. *)
val query :
  Sqlocaml_store.Store.t ->
  Sqlocaml_catalog.Catalog.t ->
  Plan.op ->
  Sqlocaml_encoding.Row.t Lwt_stream.t Lwt.t

(** Evaluate a [Plan.expr] against a row.  Exposed primarily for testing
    the rich expression language directly without round-tripping through
    SQL.  Failures (e.g. division by zero, type-mismatched arithmetic)
    raise [Failure]. *)
val eval_expr :
  Sqlocaml_encoding.Row.t ->
  Plan.expr ->
  Sqlocaml_encoding.Row.value
