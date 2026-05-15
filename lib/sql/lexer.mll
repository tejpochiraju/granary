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
  | "*"        { STAR }
  | "("        { LPAREN }
  | ")"        { RPAREN }
  | ","        { COMMA }
  | ";"        { SEMI }
  | "="        { EQ }
  | digit+ as n             { INT_LIT (Int64.of_string n) }
  | "-" (digit+ as n)       { INT_LIT (Int64.neg (Int64.of_string n)) }
  | '\'' ([^ '\'']* as s) '\''  { STRING_LIT s }
  | '\'' [^ '\'']*          { failwith "unterminated string literal" }
  | ident as id             { IDENT id }
  | eof                     { EOF }
  | _ as c                  { failwith (Printf.sprintf "unexpected char: '%c'" c) }
