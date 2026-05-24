(** Parser fuzzing — Phase 36a Task 3 / Forgejo #94.

    Two QCheck properties guard against parser crashes:

    1. [no_crash_prop]    — feeds random ASCII-ish bytes through
                            {!Sqlocaml.Db.execute}; any [Ok ()] or
                            [Error _] is accepted; an uncaught exception
                            is a property failure.
    2. [keyword_no_crash_prop] — same, but the generator biases toward
                            SQL keywords so the fuzzer reaches deeper
                            parser paths.

    Per-iteration fresh in-memory databases are used; that costs a few
    milliseconds per iteration but avoids cross-iteration state leakage
    from prior [CREATE TABLE] statements.

    We deliberately do *not* implement Step 5 of the plan
    (round-trip-via-AST) — there is no AST printer exposed today, and
    Step 5 is explicitly documented as optional in the plan. The no-crash
    properties on their own satisfy the acceptance criterion of #94. *)

open Lwt.Syntax
module Db = Sqlocaml.Db

(* Counters: 1000 random + 500 keyword-biased = 1500 inputs per run.
   That stays well under the 10s wall-clock budget on this machine; see
   the run logged in the task report. *)
let n_random = 1000
let n_keyword = 500

(** Run one Lwt body and catch any escaping exception. We treat
    [Ok () | Error _] as success — the failure mode being hunted is an
    *uncaught exception*. *)
let safe_execute sql =
  Lwt_main.run
    (Lwt.catch
       (fun () ->
          let* db = Db.open_in_memory () in
          let* _result = Db.execute db sql in
          let* () = Db.close db in
          Lwt.return true)
       (fun _exn -> Lwt.return false))
;;

(* ------------------------------------------------------------------ *)
(* Generator 1: random ASCII-ish bytes 0-200 chars.                    *)
(* ------------------------------------------------------------------ *)

let random_sql_gen =
  QCheck.(
    string_size
      ~gen:
        (Gen.oneof_weighted
           [ 1, Gen.char_range 'a' 'z'
           ; 1, Gen.char_range 'A' 'Z'
           ; 1, Gen.char_range '0' '9'
           ; ( 1
             , Gen.oneof_list
                 [ ' '
                 ; ','
                 ; ';'
                 ; '('
                 ; ')'
                 ; '\''
                 ; '"'
                 ; '*'
                 ; '='
                 ; '<'
                 ; '>'
                 ; '+'
                 ; '-'
                 ; '/'
                 ; '.'
                 ; '_'
                 ; '%'
                 ] )
           ])
      Gen.(0 -- 200))
;;

(* [random_sql_gen] already carries [QCheck.Print.string] as the default
   printer for [string_size], so a [~print] override is not needed here. *)
let no_crash_prop =
  QCheck.Test.make
    ~count:n_random
    ~name:"parser_no_crash_random"
    random_sql_gen
    safe_execute
;;

(* ------------------------------------------------------------------ *)
(* Generator 2: keyword-biased token salad.                            *)
(* ------------------------------------------------------------------ *)

let keywords =
  [| "SELECT"
   ; "FROM"
   ; "WHERE"
   ; "ORDER"
   ; "BY"
   ; "LIMIT"
   ; "OFFSET"
   ; "GROUP"
   ; "HAVING"
   ; "INSERT"
   ; "INTO"
   ; "VALUES"
   ; "UPDATE"
   ; "SET"
   ; "DELETE"
   ; "CREATE"
   ; "TABLE"
   ; "INDEX"
   ; "VIEW"
   ; "JOIN"
   ; "ON"
   ; "AND"
   ; "OR"
   ; "NOT"
   ; "NULL"
   ; "AS"
   ; "BEGIN"
   ; "COMMIT"
   ; "ROLLBACK"
   ; "INTEGER"
   ; "TEXT"
   ; "REAL"
   ; "BLOB"
   ; "PRIMARY"
   ; "KEY"
   ; "UNIQUE"
   ; "DEFAULT"
   ; "CASE"
   ; "WHEN"
   ; "THEN"
   ; "ELSE"
   ; "END"
   ; "IS"
   ; "IN"
   ; "LIKE"
   ; "BETWEEN"
   ; "DISTINCT"
   ; "ALL"
   ; "EXISTS"
   ; "DROP"
   ; "ALTER"
   ; "ADD"
   ; "COLUMN"
   ; "IF"
   ; "INNER"
   ; "LEFT"
   ; "OUTER"
   ; "CROSS"
   ; "USING"
   ; "ASC"
   ; "DESC"
  |]
;;

let atom_gen : string QCheck.Gen.t =
  QCheck.Gen.oneof
    [ QCheck.Gen.map
        (fun i -> keywords.(i))
        (QCheck.Gen.int_range 0 (Array.length keywords - 1))
    ; QCheck.Gen.string_size
        ~gen:(QCheck.Gen.char_range 'a' 'z')
        (QCheck.Gen.int_range 1 6)
    ; QCheck.Gen.map string_of_int (QCheck.Gen.int_range 0 999)
    ; QCheck.Gen.oneof_list [ ","; ";"; "("; ")"; "*"; "="; "<"; ">"; "+"; "-" ]
    ; QCheck.Gen.return "'x'"
    ; QCheck.Gen.return "\"col\""
    ]
;;

let keyword_sql_gen =
  QCheck.make
    ~print:QCheck.Print.string
    QCheck.Gen.(map (String.concat " ") (list_size (int_range 1 15) atom_gen))
;;

let keyword_no_crash_prop =
  QCheck.Test.make
    ~count:n_keyword
    ~name:"parser_no_crash_keyword_biased"
    keyword_sql_gen
    safe_execute
;;

(* ------------------------------------------------------------------ *)
(* Entry point.                                                         *)
(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "parser_fuzz"
    [ ( "no_crash"
      , List.map QCheck_alcotest.to_alcotest [ no_crash_prop; keyword_no_crash_prop ] )
    ]
;;
