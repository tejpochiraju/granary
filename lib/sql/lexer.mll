{
  open Parser
}

let digit  = ['0'-'9']
let alpha  = ['a'-'z' 'A'-'Z' '_']
let ident  = alpha (alpha | digit)*

rule token = parse
  | [' ' '\t' '\r' '\n']+  { token lexbuf }
  | "--" [^ '\n']* '\n'?   { token lexbuf }
  | "*"        { STAR }
  | "("        { LPAREN }
  | ")"        { RPAREN }
  | ","        { COMMA }
  | ";"        { SEMI }
  | "<>"       { NE }
  | "!="       { NE }
  | "<="       { LE }
  | ">="       { GE }
  | "<<"       { LSHIFT }
  | ">>"       { RSHIFT }
  | "<"        { LT }
  | ">"        { GT }
  | "="        { EQ }
  | "||"       { CONCAT }
  | "|"        { PIPE }
  | "+"        { PLUS }
  | "-"        { MINUS }
  | "/"        { SLASH }
  | "%"        { PERCENT }
  | "&"        { AMPERSAND }
  | "~"        { TILDE }
  | "."        { DOT }
  | (digit+ as i) '.' (digit* as f)
    { FLOAT_LIT (float_of_string (i ^ "." ^ f)) }
  | digit+ as n             { INT_LIT (Int64.of_string n) }
  | '\'' ([^ '\'']* as s) '\''  { STRING_LIT s }
  | '\'' [^ '\'']*          { failwith "unterminated string literal" }
  | '?' (digit+ as n) { IPARAM (int_of_string n) }
  | '?'               { QUESTION }
  | ':' (ident as id) { NAMED_PARAM id }
  | '@' (ident as id) { NAMED_PARAM id }
  | '$' (ident as id) { NAMED_PARAM id }
  | '"'  { read_double_quoted (Buffer.create 16) lexbuf }
  | '`'  { read_backtick_quoted (Buffer.create 16) lexbuf }
  | '['  { read_bracket_quoted (Buffer.create 16) lexbuf }
  | ident as id             {
      match String.uppercase_ascii id with
      | "CREATE"     -> CREATE
      | "TABLE"      -> TABLE
      | "INSERT"     -> INSERT
      | "INTO"       -> INTO
      | "VALUES"     -> VALUES
      | "SELECT"     -> SELECT
      | "FROM"       -> FROM
      | "WHERE"      -> WHERE
      | "INTEGER"    -> INTEGER_TY
      | "INT"        -> INTEGER_TY
      | "TEXT"       -> TEXT_TY
      | "VARCHAR"    -> TEXT_TY
      | "NOT"        -> NOT
      | "NULL"       -> NULL
      | "PRIMARY"    -> PRIMARY
      | "KEY"        -> KEY
      | "ALL"        -> ALL
      | "AND"        -> AND
      | "EXCEPT"     -> EXCEPT
      | "INTERSECT"  -> INTERSECT
      | "UNION"      -> UNION
      | "OR"         -> OR
      | "IS"         -> IS
      | "REAL"       -> REAL_TY
      | "FLOAT"      -> REAL_TY
      | "DOUBLE"     -> REAL_TY
      | "BLOB"       -> BLOB_TY
      | "ORDER"      -> ORDER
      | "BY"         -> BY
      | "ASC"        -> ASC
      | "DESC"       -> DESC
      | "LIMIT"      -> LIMIT
      | "OFFSET"     -> OFFSET
      | "INDEX"      -> INDEX
      | "ON"         -> ON
      | "UNIQUE"     -> UNIQUE
      | "UPDATE"     -> UPDATE
      | "SET"        -> SET
      | "DEFAULT"    -> DEFAULT
      | "DELETE"     -> DELETE
      | "DISTINCT"   -> DISTINCT
      | "DROP"       -> DROP
      | "BEGIN"      -> BEGIN
      | "COMMIT"     -> COMMIT
      | "ROLLBACK"   -> ROLLBACK
      | "ABORT"      -> ABORT
      | "IGNORE"     -> IGNORE
      | "FAIL"       -> FAIL
      | "JOIN"       -> JOIN
      | "INNER"      -> INNER
      | "LEFT"       -> LEFT
      | "OUTER"      -> OUTER
      | "GROUP"      -> GROUP
      | "HAVING"     -> HAVING
      | "COUNT"      -> COUNT
      | "SUM"        -> SUM
      | "AVG"        -> AVG
      | "MIN"        -> MIN
      | "MAX"        -> MAX
      | "LENGTH"     -> LENGTH
      | "LOWER"      -> LOWER
      | "UPPER"      -> UPPER
      | "ABS"        -> ABS
      | "COALESCE"   -> COALESCE
      | "IFNULL"     -> IFNULL
      | "IF"         -> IF
      | "SUBSTR"     -> SUBSTR
      | "SUBSTRING"  -> SUBSTR
      | "TRIM"       -> TRIM
      | "LTRIM"      -> LTRIM
      | "RTRIM"      -> RTRIM
      | "REPLACE"    -> REPLACE
      | "INSTR"      -> INSTR
      | "ROUND"      -> ROUND
      | "TYPEOF"     -> TYPEOF
      | "DATETIME"   -> DATETIME
      | "DATE"       -> DATE
      | "JULIANDAY"  -> JULIANDAY
      | "STRFTIME"   -> STRFTIME
      | "TIME"       -> TIME
      | "UNIXEPOCH"  -> UNIXEPOCH
      | "VIRTUAL"    -> VIRTUAL
      | "USING"      -> USING
      | "FTS5"       -> FTS5
      | "MATCH"      -> MATCH
      | "RETURNING"  -> RETURNING
      | "ALTER"      -> ALTER
      | "ADD"        -> ADD
      | "RENAME"     -> RENAME
      | "TO"         -> TO
      | "COLUMN"     -> COLUMN
      | "PRAGMA"     -> PRAGMA
      | "CHECK"      -> CHECK
      | "REFERENCES" -> REFERENCES
      | "FOREIGN"    -> FOREIGN
      | "LIKE"       -> LIKE
      | "GLOB"       -> GLOB
      | "BETWEEN"    -> BETWEEN
      | "IN"         -> IN
      | "EXISTS"     -> EXISTS
      | "CASE"       -> CASE
      | "WHEN"       -> WHEN
      | "THEN"       -> THEN
      | "ELSE"       -> ELSE
      | "END"        -> END
      | "AS"         -> AS
      | "CAST"       -> CAST
      | "NULLIF"     -> NULLIF
      | "IIF"        -> IIF
      | "WITH"       -> WITH
      | "CONFLICT"   -> CONFLICT
      | "DO"         -> DO
      | "VIEW"       -> VIEW
      | "OVER"       -> OVER
      | "PARTITION"  -> PARTITION
      | "RECURSIVE"  -> RECURSIVE
      | "COLLATE"    -> COLLATE
      | "PRECEDING"  -> PRECEDING
      | "FOLLOWING"  -> FOLLOWING
      | "CEIL" | "CEILING" -> CEIL
      | "FLOOR"      -> FLOOR
      | "SQRT"       -> SQRT
      | "POW" | "POWER" -> POW
      | "EXP"        -> EXP
      | "LN"         -> LN
      | "LOG"        -> LOG
      | "LOG2"       -> LOG2
      | "LOG10"      -> LOG10
      | "SIGN"       -> SIGN
      | "TRUNC" | "TRUNCATE" -> TRUNC
      | "PI"         -> PI
      | "SIN"        -> SIN
      | "COS"        -> COS
      | "TAN"        -> TAN
      | "ASIN"       -> ASIN
      | "ACOS"       -> ACOS
      | "ATAN"       -> ATAN
      | "ATAN2"      -> ATAN2
      | "DEGREES"    -> DEGREES
      | "RADIANS"    -> RADIANS
      | "NULLS"      -> NULLS
      | "JSON_EXTRACT" -> JSON_EXTRACT
      | "JSON_OBJECT"  -> JSON_OBJECT_FN
      | "JSON_ARRAY"   -> JSON_ARRAY_FN
      | "JSON_TYPE"    -> JSON_TYPE
      | "JSON_VALID"   -> JSON_VALID
      | "JSON_SET"     -> JSON_SET
      | "JSON_INSERT"  -> JSON_INSERT_FN
      | "JSON_REPLACE" -> JSON_REPLACE_FN
      | "JSON_REMOVE"  -> JSON_REMOVE
      | "SAVEPOINT"    -> SAVEPOINT
      | "RELEASE"      -> RELEASE
      | "TRIGGER"      -> TRIGGER
      | "BEFORE"       -> BEFORE
      | "AFTER"        -> AFTER
      | "CASCADE"      -> CASCADE
      | "RESTRICT"     -> RESTRICT
      | "GROUP_CONCAT" -> GROUP_CONCAT
      | "STRING_AGG"   -> STRING_AGG
      | "EXPLAIN"  -> EXPLAIN
      | "ANALYZE"  -> ANALYZE
      | "SNIPPET"  -> SNIPPET
      | _              -> IDENT id
    }
  | eof                     { EOF }
  | _ as c                  { failwith (Printf.sprintf "unexpected char: '%c'" c) }

and read_double_quoted buf = parse
  | '"' '"'    { Buffer.add_char buf '"'; read_double_quoted buf lexbuf }
  | '"'        { IDENT (Buffer.contents buf) }
  | [^ '"']+   { Buffer.add_string buf (Lexing.lexeme lexbuf);
                 read_double_quoted buf lexbuf }
  | eof        { failwith "unterminated quoted identifier" }

and read_backtick_quoted buf = parse
  | '`' '`'   { Buffer.add_char buf '`'; read_backtick_quoted buf lexbuf }
  | '`'       { IDENT (Buffer.contents buf) }
  | [^ '`']+  { Buffer.add_string buf (Lexing.lexeme lexbuf);
                read_backtick_quoted buf lexbuf }
  | eof       { failwith "unterminated quoted identifier" }

and read_bracket_quoted buf = parse
  | "]]"      { Buffer.add_char buf ']'; read_bracket_quoted buf lexbuf }
  | ']'       { IDENT (Buffer.contents buf) }
  | [^ ']']+  { Buffer.add_string buf (Lexing.lexeme lexbuf);
                read_bracket_quoted buf lexbuf }
  | eof       { failwith "unterminated quoted identifier" }
