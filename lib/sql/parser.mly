%{
  (* open Ast -- omitted until grammar rules are added in Tasks 13-15 *)
  let _unused = ()  (* suppress unused-open warning *)
%}

%token <string> IDENT
%token <int64>  INT_LIT
%token <string> STRING_LIT
%token CREATE TABLE INSERT INTO VALUES SELECT FROM WHERE
%token INTEGER_TY TEXT_TY
%token NOT NULL PRIMARY KEY AND
%token STAR LPAREN RPAREN COMMA SEMI EQ
%token EOF

%start <unit> stmt_eof

%%

stmt_eof:
  | EOF { () }
