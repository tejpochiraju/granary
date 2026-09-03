(** #583: [Db.dump]'s #548 NOT NULL refusal points at [PRAGMA not_null_check].

    #548 made [Db.dump] refuse to emit a script whose schema line contradicts its
    own data lines, and hand-wrote the repairing [UPDATE] / [DELETE] for the one
    table and column it happened to trip on. #563 then landed the first-class
    pair — [PRAGMA not_null_check] (read-only, one row per offending
    (table, column, count), silent when clean) and [PRAGMA not_null_repair]
    (deletes those rows through the ordinary delete path, so indexes and
    ON DELETE cascades are honoured).

    {b Why the ordering matters.} The refusal is raised from inside the row
    stream, so it reports the violation the dump STOPPED ON, not the scope: it
    knows one (table, column) and nothing about the rest of the file. An operator
    repairing table by table off successive dump failures is doing precisely
    what report mode was built to prevent. So the message now leads with the
    survey and the first-class repair, and keeps the hand-written statements as
    the manual escape hatch — #548 refuses in order to stop information being
    destroyed silently, so the non-destructive [UPDATE] must stay visible.

    The fixture is #548's own: rows go in while the column is genuinely nullable,
    then the implicit PK index — the record [Catalog.open_] re-derives NOT NULL
    from (#533) — is registered in the catalog afterwards. *)

open Lwt.Syntax
module Db = Granary.Db
module Cat = Granary_catalog.Catalog
module Row = Granary_encoding.Row

let run = Lwt_main.run

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

let contains ~needle hay =
  let nl = String.length needle
  and hl = String.length hay in
  let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
  nl = 0 || go 0
;;

let open_db path =
  match run (Granary_unix.open_file ~path ()) with
  | Ok db -> db
  | Error e -> Alcotest.failf "open %s: %a" path Db.pp_error e
;;

let close_db db =
  try run (Db.close db) with
  | _ -> ()
;;

let show_value = function
  | Row.V_text s -> s
  | Row.V_null -> "<null>"
  | Row.V_int n -> Int64.to_string n
  | Row.V_real f -> string_of_float f
  | Row.V_blob _ -> "<blob>"
;;

let texts db sql =
  let rows =
    run
      (let* r = Db.query db sql in
       match r with
       | Error e -> Alcotest.failf "query %S: %a" sql Db.pp_error e
       | Ok stream -> Lwt_stream.to_list stream)
  in
  List.map
    (fun (r : Row.t) -> Array.to_list r |> List.map show_value |> String.concat "|")
    rows
;;

(* #548's recipe verbatim: register the implicit PRIMARY KEY index directly in
   the catalog of an already-populated file, so the next [Catalog.open_]
   re-derives NOT NULL onto columns whose stored rows hold NULL. *)
let add_implicit_pk_index path ~table ~cols =
  run
    (let* store =
       let* r = Granary_unix.Store.open_file ~path () in
       match r with
       | Ok s -> Lwt.return s
       | Error _ -> Alcotest.failf "cannot reopen store %s" path
     in
     let* cat = Cat.open_ store in
     let* r =
       Cat.create_index
         cat
         ~name:(Printf.sprintf "__pk_%s_%s_0" table (String.concat "_" cols))
         ~table
         ~columns:cols
         ~unique:true
         ~expr_flags:(List.map (fun _ -> false) cols)
         ~where_sql:None
         ~origin:`Implicit_pk
     in
     match r with
     | Ok _ -> Granary_store.Store.close store
     | Error m -> Alcotest.failf "create_index: %s" m)
;;

let with_legacy_null_key_db f =
  let path = Filename.temp_file "granary_583_" ".db" in
  Sys.remove path;
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | _ -> ())
    (fun () ->
       let db = open_db path in
       exec db "CREATE TABLE stock (sw INTEGER, si INTEGER, qty INTEGER)";
       exec db "INSERT INTO stock VALUES (1, 2, 50)";
       exec db "INSERT INTO stock VALUES (1, NULL, 60)";
       close_db db;
       add_implicit_pk_index path ~table:"stock" ~cols:[ "sw"; "si" ];
       let db = open_db path in
       Fun.protect ~finally:(fun () -> close_db db) (fun () -> f db))
;;

let refusal_message db =
  match run (Db.dump_to_string db ()) with
  | Ok script ->
    Alcotest.failf "expected a refusal, got a dump that cannot replay:\n%s" script
  | Error e -> Format.asprintf "%a" Db.pp_error e
;;

(* ------------------------------------------------------------------ *)

(* The headline: the survey and the first-class repair are both named. *)
let the_diagnostic_names_the_pragmas () =
  with_legacy_null_key_db (fun db ->
    let msg = refusal_message db in
    List.iter
      (fun needle ->
         Alcotest.(check bool)
           (Printf.sprintf "the diagnostic mentions %S (got %S)" needle msg)
           true
           (contains ~needle msg))
      [ "PRAGMA not_null_check"; "PRAGMA not_null_repair" ])
;;

(* The manual escape hatch survives the demotion.  #548 refuses in order to stop
   information being destroyed silently, so a message offering only the
   destructive repair works against its own reason for existing — and
   [~data_only:true] is still how the rows come out of an unrepaired file. *)
let the_manual_repairs_and_data_only_survive () =
  with_legacy_null_key_db (fun db ->
    let msg = refusal_message db in
    List.iter
      (fun needle ->
         Alcotest.(check bool)
           (Printf.sprintf "the diagnostic still mentions %S (got %S)" needle msg)
           true
           (contains ~needle msg))
      [ "stock"
      ; "si"
      ; "NOT NULL"
      ; "#548"
      ; "UPDATE stock SET si"
      ; "DELETE FROM stock"
      ; "~data_only:true"
      ])
;;

(* The refusal reports one (table, column); [PRAGMA not_null_check] reports the
   scope.  This is the substance of the redirection, not the wording: the
   message would be pointless advice if the command it names did not answer a
   question the message itself cannot. *)
let the_named_survey_reports_the_scope () =
  with_legacy_null_key_db (fun db ->
    Alcotest.(check (list string))
      "not_null_check enumerates the offending (table, column, count)"
      [ "stock|si|1" ]
      (texts db "PRAGMA not_null_check"))
;;

(* Following the message works end to end: survey, repair, dump. *)
let following_the_diagnostic_makes_the_dump_succeed () =
  with_legacy_null_key_db (fun db ->
    let _ : string = refusal_message db in
    Alcotest.(check (list string))
      "the repair the message names deletes the offending row"
      [ "stock|si|1" ]
      (texts db "PRAGMA not_null_repair");
    Alcotest.(check (list string))
      "and the survey is then silent"
      []
      (texts db "PRAGMA not_null_check");
    match run (Db.dump_to_string db ()) with
    | Error e ->
      Alcotest.failf "dump still refused after the named repair: %a" Db.pp_error e
    | Ok script ->
      Alcotest.(check bool)
        "the surviving row is dumped"
        true
        (contains ~needle:"INSERT INTO stock VALUES(1,2,50)" script);
      Alcotest.(check bool)
        "and the deleted one is not"
        false
        (contains ~needle:"INSERT INTO stock VALUES(1,NULL,60)" script))
;;

let () =
  Alcotest.run
    "dump_not_null_check_583"
    [ ( "diagnostic"
      , List.map
          (fun (n, f) -> Alcotest.test_case n `Quick f)
          [ ( "names PRAGMA not_null_check and not_null_repair"
            , the_diagnostic_names_the_pragmas )
          ; ( "keeps the manual repairs and ~data_only:true"
            , the_manual_repairs_and_data_only_survive )
          ; "the named survey reports the scope", the_named_survey_reports_the_scope
          ; ( "following it makes the dump succeed"
            , following_the_diagnostic_makes_the_dump_succeed )
          ] )
    ]
;;
