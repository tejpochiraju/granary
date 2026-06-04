(** Execute write operations against the store and catalog. *)

(** Transaction mode for DML operations.
    [Auto] wraps each DML operation in its own RW transaction (autocommit).
    [In_txn tx] reuses an externally managed transaction; the caller is
    responsible for committing or rolling back. *)
type txn_mode =
  | Auto
  | In_txn of Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn

(** Execute a write operation ([Plan.op]) against the store and catalog.
    Runs in its own autocommit RW transaction unless [?mode] supplies an
    enclosing one; the optional hooks observe row changes for triggers and
    REPLACE/UPSERT bookkeeping. *)
val execute
  :  ?mode:txn_mode
  -> ?clock:(unit -> float) option
  -> ?params:Sqlocaml_encoding.Row.value array
  -> ?before_hook:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> new_row:Sqlocaml_encoding.Row.t option
        -> old_row:Sqlocaml_encoding.Row.t option
        -> unit Lwt.t)
         option
  -> ?after_hook:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> new_row:Sqlocaml_encoding.Row.t option
        -> old_row:Sqlocaml_encoding.Row.t option
        -> unit Lwt.t)
         option
  -> ?on_replace_delete_before:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> old_row:Sqlocaml_encoding.Row.t
        -> unit Lwt.t)
         option
  -> ?on_replace_delete:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> old_row:Sqlocaml_encoding.Row.t
        -> unit Lwt.t)
         option
  -> ?on_upsert_update_before:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> old_row:Sqlocaml_encoding.Row.t
        -> new_row:Sqlocaml_encoding.Row.t
        -> unit Lwt.t)
         option
  -> ?on_upsert_update:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> old_row:Sqlocaml_encoding.Row.t
        -> new_row:Sqlocaml_encoding.Row.t
        -> unit Lwt.t)
         option
  -> Sqlocaml_store.Store.t
  -> Sqlocaml_catalog.Catalog.t
  -> Plan.op
  -> unit Lwt.t

(** Like [execute], but returns the rows-affected count.  For DDL ops
    (CREATE TABLE, CREATE INDEX) this is [0]; for INSERT it is [1]; for
    UPDATE it is the number of rows whose contents were modified.
    [Op_begin], [Op_commit], and [Op_rollback] are not handled here —
    they raise [Failure] if passed; the [Db] layer intercepts them. *)
val execute_with_count
  :  ?mode:txn_mode
  -> ?clock:(unit -> float) option
  -> ?params:Sqlocaml_encoding.Row.value array
  -> ?before_hook:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> new_row:Sqlocaml_encoding.Row.t option
        -> old_row:Sqlocaml_encoding.Row.t option
        -> unit Lwt.t)
         option
  -> ?after_hook:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> new_row:Sqlocaml_encoding.Row.t option
        -> old_row:Sqlocaml_encoding.Row.t option
        -> unit Lwt.t)
         option
  -> ?on_replace_delete_before:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> old_row:Sqlocaml_encoding.Row.t
        -> unit Lwt.t)
         option
  -> ?on_replace_delete:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> old_row:Sqlocaml_encoding.Row.t
        -> unit Lwt.t)
         option
  -> ?on_upsert_update_before:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> old_row:Sqlocaml_encoding.Row.t
        -> new_row:Sqlocaml_encoding.Row.t
        -> unit Lwt.t)
         option
  -> ?on_upsert_update:
       (tx:Sqlocaml_store.Store.rw Sqlocaml_store.Store.txn
        -> old_row:Sqlocaml_encoding.Row.t
        -> new_row:Sqlocaml_encoding.Row.t
        -> unit Lwt.t)
         option
  -> Sqlocaml_store.Store.t
  -> Sqlocaml_catalog.Catalog.t
  -> Plan.op
  -> int Lwt.t

(** #239: per-query cost/stats signal for an external cost-based cache.
    [rows_examined] is the number of rows pulled from a base table/index scan —
    the true work signal (a query that scans a million rows to return one reads
    [rows_examined = 1_000_000], [rows_returned = 1]); [rows_returned] is the
    result size once the stream is drained; [used_index] is the plan-time fact
    that the base access is an index/rowid/FTS seek rather than a full scan.

    Mutable counters updated as the stream is consumed: pass a fresh record to
    {!query} via [?stats], drain the stream, then read the fields.  Mirage-pure
    (no clock/Unix dependency).  [rows_examined] covers seq scans, index/rowid
    lookups (incl. nested-loop-join right-side probes), the count fast path, and
    both inputs of a hash join; FTS internal seeks and rows read inside scalar/
    correlated subqueries are not yet counted (#257). *)
type query_stats =
  { mutable rows_examined : int
  ; mutable rows_returned : int
  ; mutable used_index : bool
  }

(** A zeroed {!query_stats} ([used_index = false]). *)
val make_query_stats : unit -> query_stats

(** Execute read operations; returns a lazy stream of result rows.
    [params] are the positional parameter values for [?] placeholders.
    [clock] supplies the current Unix timestamp for SQL date/time
    functions invoked with the literal ['now'].
    [stats], when supplied, is populated as the returned stream is consumed
    (#239); omit it and the read path is entirely unaffected. *)
val query
  :  ?mode:txn_mode
  -> ?clock:(unit -> float) option
  -> ?params:Sqlocaml_encoding.Row.value array
  -> ?stats:query_stats
  -> Sqlocaml_store.Store.t
  -> Sqlocaml_catalog.Catalog.t
  -> Plan.op
  -> Sqlocaml_encoding.Row.t Lwt_stream.t Lwt.t

(** Evaluate a [Plan.expr] against a row.  Exposed primarily for testing
    the rich expression language directly without round-tripping through
    SQL.  Failures (e.g. division by zero, type-mismatched arithmetic)
    raise [Failure]. *)
val eval_expr
  :  (unit -> float) option
  -> Sqlocaml_encoding.Row.value array
  -> Sqlocaml_encoding.Row.t
  -> Plan.expr
  -> Sqlocaml_encoding.Row.value
