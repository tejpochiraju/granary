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
  | "AND"      { AND }
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
  | "DROP"     { DROP }
  | "BEGIN"    { BEGIN }
  | "COMMIT"   { COMMIT }
  | "ROLLBACK" { ROLLBACK }
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
  | "VIRTUAL"  { VIRTUAL }
  | "USING"    { USING }
  | "FTS5"     { FTS5 }
  | "MATCH"    { MATCH }
  | "*"        { STAR }
  | "("        { LPAREN }
  | ")"        { RPAREN }
  | ","        { COMMA }
  | ";"        { SEMI }
  | "<>"       { NE }
  | "!="       { NE }
  | "<="       { LE }
  | ">="       { GE }
  | "<"        { LT }
  | ">"        { GT }
  | "="        { EQ }
  | "+"        { PLUS }
  | "-"        { MINUS }
  | "/"        { SLASH }
  | "."        { DOT }
  | (digit+ as i) '.' (digit* as f)
    { FLOAT_LIT (float_of_string (i ^ "." ^ f)) }
  | digit+ as n             { INT_LIT (Int64.of_string n) }
  | '\'' ([^ '\'']* as s) '\''  { STRING_LIT s }
  | '\'' [^ '\'']*          { failwith "unterminated string literal" }
  | '?'                      { QUESTION }
  | ident as id             { IDENT id }
  | eof                     { EOF }
  | _ as c                  { failwith (Printf.sprintf "unexpected char: '%c'" c) }
