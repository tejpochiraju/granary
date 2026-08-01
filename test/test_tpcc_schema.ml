(* #500 — asserts granary accepts every TPC-C DDL statement: the nine
   CREATE TABLE statements (composite primary keys, nullable columns) and the
   two secondary indexes. A statement granary rejects is an engine finding,
   not something to work around here. *)

module Granary_engine = Granary_tpc.Granary_engine
module Schema = Granary_tpc.Tpcc_schema

(* [Filename.temp_dir] creates the directory itself, with no create-then-replace
   window of the [temp_file]/[unlink]/[mkdir] sequence it replaced. *)
let with_tmp_dir f =
  let dir = Filename.temp_dir "tpcc-schema" "" in
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir))))
    (fun () -> f dir)
;;

let test_ddl_accepted () =
  with_tmp_dir (fun dir ->
    let e = Granary_engine.open_db ~dir in
    List.iter
      (fun stmt ->
         try Granary_engine.exec e stmt with
         | Failure msg -> Alcotest.failf "granary rejected CREATE TABLE: %s" msg)
      Schema.ddl;
    Granary_engine.close e)
;;

let test_indexes_accepted () =
  with_tmp_dir (fun dir ->
    let e = Granary_engine.open_db ~dir in
    List.iter (Granary_engine.exec e) Schema.ddl;
    List.iter
      (fun stmt ->
         try Granary_engine.exec e stmt with
         | Failure msg -> Alcotest.failf "granary rejected CREATE INDEX: %s" msg)
      Schema.indexes;
    Granary_engine.close e)
;;

let test_tables_match_ddl_order () =
  Alcotest.(check int) "same length" (List.length Schema.ddl) (List.length Schema.tables)
;;

let () =
  Alcotest.run
    "tpcc_schema"
    [ ( "ddl"
      , [ Alcotest.test_case "every CREATE TABLE is accepted" `Quick test_ddl_accepted
        ; Alcotest.test_case "every CREATE INDEX is accepted" `Quick test_indexes_accepted
        ; Alcotest.test_case
            "tables list matches ddl in length"
            `Quick
            test_tables_match_ddl_order
        ] )
    ]
;;
