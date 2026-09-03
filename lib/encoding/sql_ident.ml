(* #577: the one rule about when a SQL identifier may be written bare.

   It lives here, in the lowest library any emitter shares, because THREE
   emitters need it and they sit at two different levels of the dependency
   graph:

   - [Granary_sql.Ast.expr_to_sql], which renders stored CHECK / GENERATED /
     partial-index text (#572);
   - [Granary_sql.Exec]'s DDL renderer, which builds [sqlite_master] and
     [Db.dump] output;
   - [Granary_catalog]'s [rewrite_ident_in_sql], which substitutes a new column
     name into that stored text on [ALTER TABLE ... RENAME COLUMN] (#553).

   The third is why the rule moved. [granary.catalog] sits BELOW [granary.sql],
   so it could not reach the keyword table and shipped a keyword-blind copy of
   the predicate instead: renaming a column to a reserved word wrote [order]
   into a bare position and left the table permanently un-insertable (#577).
   Two emitters of the same SQL text must agree on when a name is bare, so
   there is one implementation, not three.

   [test_ident_quoting_572.ml] re-derives {!sql_keywords} from [lexer.mll] and
   fails if the two have drifted apart.

   #619: that guard is the ONLY consumer of [sql_keywords], [is_sql_keyword]
   and [ident_needs_quoting] outside this module, so [dead_code_analyzer] flags
   all three.  Do not prune them — see the note at the top of [sql_ident.mli].
   Deleting them does not fail anything; it silently unbinds the keyword list
   from the lexer, which is #577's failure mode reintroduced through the back
   door. *)

(* #572: every word the lexer maps to a keyword token instead of [IDENT].  A
   column or table with one of these names can only ever have been spelled
   delimited, so re-emitting it bare would produce text that no longer parses
   — exactly the failure this list exists to prevent.

   Mirrors the keyword table at the [ident] rule in [lexer.mll]; the lexer
   uppercases before matching, so the comparison here is case-insensitive too.
   [test_ident_quoting_572.ml] re-derives the list from [lexer.mll] and fails
   if the two have drifted apart. *)
let sql_keywords =
  [ (* generated from lexer.mll *)
    "ABORT"
  ; "ABS"
  ; "ACOS"
  ; "ADD"
  ; "AFTER"
  ; "ALL"
  ; "ALTER"
  ; "ANALYZE"
  ; "AND"
  ; "AS"
  ; "ASC"
  ; "ASIN"
  ; "ATAN"
  ; "ATAN2"
  ; "ATTACH"
  ; "AUTOINCREMENT"
  ; "AVG"
  ; "BEFORE"
  ; "BEGIN"
  ; "BETWEEN"
  ; "BLOB"
  ; "BY"
  ; "CASCADE"
  ; "CASE"
  ; "CAST"
  ; "CEIL"
  ; "CEILING"
  ; "CHANGES"
  ; "CHAR"
  ; "CHECK"
  ; "COALESCE"
  ; "COLLATE"
  ; "COLUMN"
  ; "COLUMNSTORE"
  ; "COMMIT"
  ; "CONFLICT"
  ; "COS"
  ; "COUNT"
  ; "CREATE"
  ; "CROSS"
  ; "DATABASE"
  ; "DATE"
  ; "DATETIME"
  ; "DEFAULT"
  ; "DEFERRABLE"
  ; "DEFERRED"
  ; "DEGREES"
  ; "DELETE"
  ; "DELTA"
  ; "DESC"
  ; "DETACH"
  ; "DISTINCT"
  ; "DO"
  ; "DOUBLE"
  ; "DROP"
  ; "EACH"
  ; "ELSE"
  ; "END"
  ; "EXCEPT"
  ; "EXISTS"
  ; "EXP"
  ; "EXPLAIN"
  ; "FAIL"
  ; "FLOAT"
  ; "FLOOR"
  ; "FOLLOWING"
  ; "FOR"
  ; "FOREIGN"
  ; "FORMAT"
  ; "FROM"
  ; "FTS5"
  ; "FULL"
  ; "GLOB"
  ; "GROUP"
  ; "GROUP_CONCAT"
  ; "HAVING"
  ; "HEX"
  ; "IF"
  ; "IFNULL"
  ; "IGNORE"
  ; "IIF"
  ; "IMMEDIATE"
  ; "IN"
  ; "INDEX"
  ; "INITIALLY"
  ; "INNER"
  ; "INSERT"
  ; "INSTEAD"
  ; "INSTR"
  ; "INT"
  ; "INTEGER"
  ; "INTERSECT"
  ; "INTO"
  ; "IS"
  ; "JOIN"
  ; "JSON_ARRAY"
  ; "JSON_EXTRACT"
  ; "JSON_INSERT"
  ; "JSON_OBJECT"
  ; "JSON_REMOVE"
  ; "JSON_REPLACE"
  ; "JSON_SET"
  ; "JSON_TYPE"
  ; "JSON_VALID"
  ; "JULIANDAY"
  ; "KEY"
  ; "LAST_INSERT_ROWID"
  ; "LEFT"
  ; "LENGTH"
  ; "LIKE"
  ; "LIMIT"
  ; "LN"
  ; "LOG"
  ; "LOG10"
  ; "LOG2"
  ; "LOWER"
  ; "LTRIM"
  ; "MATCH"
  ; "MAX"
  ; "MIN"
  ; "NOT"
  ; "NULL"
  ; "NULLIF"
  ; "NULLS"
  ; "OFFSET"
  ; "ON"
  ; "OR"
  ; "ORDER"
  ; "OUTER"
  ; "OVER"
  ; "PARTITION"
  ; "PI"
  ; "POW"
  ; "POWER"
  ; "PRAGMA"
  ; "PRECEDING"
  ; "PRIMARY"
  ; "PRINTF"
  ; "RADIANS"
  ; "RANDOM"
  ; "RANDOMBLOB"
  ; "REACTIVE"
  ; "REAL"
  ; "RECURSIVE"
  ; "REFERENCES"
  ; "REFRESH"
  ; "RELEASE"
  ; "RENAME"
  ; "REPLACE"
  ; "RESTRICT"
  ; "RETURNING"
  ; "ROLLBACK"
  ; "ROUND"
  ; "ROW"
  ; "ROWID"
  ; "RTRIM"
  ; "SAVEPOINT"
  ; "SELECT"
  ; "SET"
  ; "SIGN"
  ; "SIN"
  ; "SNIPPET"
  ; "SQLITE_VERSION"
  ; "SQRT"
  ; "STRFTIME"
  ; "STRING_AGG"
  ; "SUBSTR"
  ; "SUBSTRING"
  ; "SUM"
  ; "TABLE"
  ; "TAN"
  ; "TEXT"
  ; "THEN"
  ; "TIME"
  ; "TO"
  ; "TOTAL_CHANGES"
  ; "TRIGGER"
  ; "TRIM"
  ; "TRUNC"
  ; "TRUNCATE"
  ; "TYPEOF"
  ; "UNICODE"
  ; "UNION"
  ; "UNIQUE"
  ; "UNIXEPOCH"
  ; "UPDATE"
  ; "UPPER"
  ; "USING"
  ; "VACUUM"
  ; "VALUES"
  ; "VARCHAR"
  ; "VIEW"
  ; "VIRTUAL"
  ; "WHEN"
  ; "WHERE"
  ; "WITH"
  ; "WITHOUT"
  ; "ZEROBLOB"
  ]
;;

let keyword_table =
  lazy
    (let h = Hashtbl.create 512 in
     List.iter (fun k -> Hashtbl.replace h k ()) sql_keywords;
     h)
;;

let is_sql_keyword s = Hashtbl.mem (Lazy.force keyword_table) (String.uppercase_ascii s)

(* An identifier survives being written back bare only if it is a plain
   [[A-Za-z_][A-Za-z0-9_]*] word that the lexer would not swallow as a
   keyword.  Everything else — the empty name, a leading digit, a space, any
   punctuation, an embedded quote — has to be delimited. *)
let ident_needs_quoting s =
  String.length s = 0
  || (let c = s.[0] in
      not ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c = '_'))
  || String.exists
       (fun c ->
          not
            ((c >= 'A' && c <= 'Z')
             || (c >= 'a' && c <= 'z')
             || (c >= '0' && c <= '9')
             || c = '_'))
       s
  || is_sql_keyword s
;;

let quote_ident s =
  if ident_needs_quoting s
  then "\"" ^ String.concat "\"\"" (String.split_on_char '"' s) ^ "\""
  else s
;;
