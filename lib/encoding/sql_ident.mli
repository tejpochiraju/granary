(** #577: when a SQL identifier may be written bare, and how to delimit it when
    it may not.

    One rule, shared by every emitter of SQL text in the engine:
    [Granary_sql.Ast.expr_to_sql] (stored CHECK / GENERATED / partial-index
    text), [Granary_sql.Exec]'s DDL renderer ([sqlite_master] and [Db.dump]),
    and [Granary_catalog.rewrite_ident_in_sql] (the substitution
    [ALTER TABLE ... RENAME COLUMN] performs on that stored text).

    It lives in [granary.encoding] because [granary.catalog] sits below
    [granary.sql] and so cannot reach the parser's keyword table; its
    keyword-blind local copy of the predicate was #577.

    {2 Do not prune {!sql_keywords}, {!is_sql_keyword} or
       {!ident_needs_quoting} (#619)}

    {!sql_keywords}, {!is_sql_keyword} and {!ident_needs_quoting} have no
    caller in [lib/] outside this module — {!quote_ident} is the only one the
    engine reaches for. [dead_code_analyzer] will therefore report all three as
    unused exports, and it is documented as the tool to run "before a release
    or when pruning" ([docs/DEAD_CODE.md]).

    They are NOT dead. They are the handle the [lexer.mll] drift guard in
    [test_ident_quoting_572.ml] holds: that guard re-derives the keyword table
    from [lexer.mll] and fails if it and {!sql_keywords} have drifted apart,
    which is the only thing binding this list to the lexer now that the two
    live in different libraries. Delete them and nothing fails — the guard just
    stops guarding, and the next reserved word added to the lexer corrupts
    stored SQL again (#577, #572).

    Before #619 the guard held this handle one indirection further out, through
    aliases in [Granary_sql.Ast]; it was moved here so the aliases could go and
    the "do not prune" note could sit on the values that actually matter. *)

(** Every word the lexer turns into a keyword token rather than an identifier,
    upper-cased. Mirrors the keyword table in [lib/sql/lexer.mll]; the drift
    guard in [test_ident_quoting_572.ml] re-derives it from that source.

    Not dead code — see the note above. *)
val sql_keywords : string list

(** [is_sql_keyword s] is whether [s] — compared case-insensitively, as the
    lexer does — is one of {!sql_keywords}.

    Not dead code — see the note above. *)
val is_sql_keyword : string -> bool

(** [ident_needs_quoting s] is whether [s] must be written delimited to be read
    back as the identifier [s]: it is not a plain [[A-Za-z_][A-Za-z0-9_]*]
    word, or it is one that the lexer would swallow as a keyword.

    Not dead code — see the note above. *)
val ident_needs_quoting : string -> bool

(** [quote_ident s] is [s] wrapped in double quotes (embedded quotes doubled)
    when {!ident_needs_quoting} says so, and [s] verbatim otherwise. *)
val quote_ident : string -> string
