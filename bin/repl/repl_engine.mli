(** Pure REPL helpers, independent of any UI.  Extracted from the original
    blocking shell so both the terminal logic and the nottui views can share
    them (#382). *)

module Db = Sqlocaml.Db

(** Render one engine value as a display string (NULL/int/real/text/blob). *)
val value_to_string : Db.value -> string

(** True iff the statement is a row-returning query (SELECT/WITH/EXPLAIN/
    VALUES/PRAGMA). *)
val is_query_stmt : string -> bool

(** True iff [buf] contains a [;] terminator outside any string literal
    (['…']), quoted identifier (["…"], [`…`], [\[…\]]), [--] line comment or
    [/* … */] block comment (#389). *)
val has_terminator : Buffer.t -> bool

(** Split a multi-statement string into trimmed statements.  A [;] only ends a
    statement when it sits in plain SQL: occurrences inside string literals
    (['…']), quoted identifiers (["…"], [`…`], [\[…\]]), [--] line comments and
    [/* … */] block comments are kept verbatim and never split (#389). *)
val split_stmts : string -> string list

(** [sqlite_dump_stmts dump] is the list of statements to replay from a SQLite
    [.dump] script: transaction control ([BEGIN]/[COMMIT]/[END]), [PRAGMA]s,
    [ANALYZE] and INSERT/DELETE maintenance of internal [sqlite_] tables
    ([sqlite_sequence], [sqlite_stat1/4]) are dropped; the rest is kept verbatim.
    The internal-table test anchors on the target table, so a user INSERT whose
    value merely mentions ["sqlite_…"] is preserved (#91). *)
val sqlite_dump_stmts : string -> string list

(** [import_sqlite_dump db dump] replays the statements of a SQLite [.dump]
    script into [db], best-effort and in autocommit so a single failing
    statement does not abort the rest.  Returns the number of statements
    applied and the list of [(statement, error message)] pairs that failed
    (#91). *)
val import_sqlite_dump : Db.t -> string -> (int * (string * string) list) Lwt.t

(** Open a db at [path] ([":memory:"] for in-memory). *)
val open_db : path:string -> (Db.t, Db.error) result Lwt.t

(** Column widths for a pipe-aligned table render of [rows]. *)
val column_widths : Db.row list -> int array
