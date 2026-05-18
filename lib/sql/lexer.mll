{
  open Parser
}

let digit  = ['0'-'9']
let alpha  = ['a'-'z' 'A'-'Z' '_']
let ident  = alpha (alpha | digit)*

rule token = parse
  | [' ' '\t' '\r' '\n']+  { token lexbuf }
  | "--" [^ '\n']* '\n'?   { token lexbuf }
  | "CREATE"   { CREATE }
  | "TABLE"    { TABLE }
  | "INSERT"   { INSERT }
  | "INTO"     { INTO }
  | "VALUES"   { VALUES }
  | "SELECT"   { SELECT }
  | "FROM"     { FROM }
  | "WHERE"    { WHERE }
  | "INTEGER"  { INTEGER_TY }
  | "TEXT"     { TEXT_TY }
  | "NOT"      { NOT }
  | "NULL"     { NULL }
  | "PRIMARY"  { PRIMARY }
  | "KEY"      { KEY }
  | "ALL"       { ALL }
  | "AND"      { AND }
  | "EXCEPT"    { EXCEPT }
  | "INTERSECT" { INTERSECT }
  | "UNION"     { UNION }
  | "OR"       { OR }
  | "IS"       { IS }
  | "REAL"     { REAL_TY }
  | "BLOB"     { BLOB_TY }
  | "ORDER"    { ORDER }
  | "BY"       { BY }
  | "ASC"      { ASC }
  | "DESC"     { DESC }
  | "LIMIT"    { LIMIT }
  | "OFFSET"   { OFFSET }
  | "INDEX"    { INDEX }
  | "ON"       { ON }
  | "UNIQUE"   { UNIQUE }
  | "UPDATE"   { UPDATE }
  | "SET"      { SET }
  | "DEFAULT"  { DEFAULT }
  | "DELETE"   { DELETE }
  | "DISTINCT" { DISTINCT }
  | "DROP"     { DROP }
  | "BEGIN"    { BEGIN }
  | "COMMIT"   { COMMIT }
  | "ROLLBACK" { ROLLBACK }
  | "ABORT"    { ABORT }
  | "IGNORE"   { IGNORE }
  | "FAIL"     { FAIL }
  | "JOIN"     { JOIN }
  | "INNER"    { INNER }
  | "LEFT"     { LEFT }
  | "OUTER"    { OUTER }
  | "GROUP"    { GROUP }
  | "HAVING"   { HAVING }
  | "COUNT"    { COUNT }
  | "SUM"      { SUM }
  | "AVG"      { AVG }
  | "MIN"      { MIN }
  | "MAX"      { MAX }
  | "LENGTH"   { LENGTH }
  | "LOWER"    { LOWER }
  | "UPPER"    { UPPER }
  | "ABS"      { ABS }
  | "COALESCE" { COALESCE }
  | "IFNULL"   { IFNULL }
  | "SUBSTR"   { SUBSTR }
  | "TRIM"     { TRIM }
  | "LTRIM"    { LTRIM }
  | "RTRIM"    { RTRIM }
  | "REPLACE"  { REPLACE }
  | "INSTR"    { INSTR }
  | "ROUND"    { ROUND }
  | "TYPEOF"   { TYPEOF }
  | "DATETIME"  { DATETIME }
  | "DATE"      { DATE }
  | "JULIANDAY" { JULIANDAY }
  | "STRFTIME"  { STRFTIME }
  | "TIME"      { TIME }
  | "UNIXEPOCH" { UNIXEPOCH }
  | "VIRTUAL"  { VIRTUAL }
  | "USING"    { USING }
  | "FTS5"     { FTS5 }
  | "MATCH"    { MATCH }
  | "RETURNING" { RETURNING }
  | "ALTER"    { ALTER }
  | "ADD"      { ADD }
  | "RENAME"   { RENAME }
  | "TO"       { TO }
  | "COLUMN"   { COLUMN }
  | "PRAGMA"   { PRAGMA }
  | "CHECK"    { CHECK }
  | "check"    { CHECK }
  | "REFERENCES" { REFERENCES }
  | "references" { REFERENCES }
  | "FOREIGN"  { FOREIGN }
  | "foreign"  { FOREIGN }
  | "LIKE"     { LIKE }
  | "GLOB"     { GLOB }
  | "BETWEEN"  { BETWEEN }
  | "IN"       { IN }
  | "EXISTS"   { EXISTS }
  | "exists"   { EXISTS }
  | "CASE"  | "case"  { CASE }
  | "WHEN"  | "when"  { WHEN }
  | "THEN"  | "then"  { THEN }
  | "ELSE"  | "else"  { ELSE }
  | "END"   | "end"   { END }
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
  | ident as id             {
      match String.uppercase_ascii id with
      | "AS"     -> AS
      | "CAST"   -> CAST
      | "NULLIF" -> NULLIF
      | "IIF"    -> IIF
      | "WITH"      -> WITH
      | "CONFLICT"  -> CONFLICT
      | "DO"        -> DO
      | "VIEW"      -> VIEW
      | "OVER"      -> OVER
      | "PARTITION" -> PARTITION
      | "RECURSIVE" -> RECURSIVE
      | "COLLATE"   -> COLLATE
      | "PRECEDING" -> PRECEDING
      | "FOLLOWING" -> FOLLOWING
      | _           -> IDENT id
    }
  | eof                     { EOF }
  | _ as c                  { failwith (Printf.sprintf "unexpected char: '%c'" c) }
