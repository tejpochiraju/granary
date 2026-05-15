%token EOF
%start <unit> dummy
%%
dummy: EOF { () }
