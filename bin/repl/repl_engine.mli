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

(** Open a db at [path] ([":memory:"] for in-memory). *)
val open_db : path:string -> (Db.t, Db.error) result Lwt.t

(** Column widths for a pipe-aligned table render of [rows]. *)
val column_widths : Db.row list -> int array
