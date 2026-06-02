(** #233 regression: FTS term/prefix queries must be O(log n) in the index size,
    not O(n).

    Before the fix [fts_posting_list] / [fts_prefix_posting_list] opened a
    [Store] cursor that materialised the ENTIRE FTS index tree into a list per
    query (the same drain that made #228/#229 O(n)).  They now use the native
    streaming [Store.seek_ge].

    The assertion is machine-independent: it times the *same* rare-term query
    (one matching document) against two index sizes (1x and 3x) and requires the
    per-op cost to stay roughly flat.  A query that drains the whole index would
    grow ~linearly with size; an O(log n) seek does not. *)

module Db = Sqlocaml.Db

let run = Lwt_main.run

let unwrap = function
  | Ok v -> v
  | Error e -> Alcotest.failf "db error: %a" Db.pp_error e
;;

let now () = Unix.gettimeofday ()

let with_db f =
  let dir = Filename.temp_file "sqlocaml_fts_scaling" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let path = Filename.concat dir "fts.db" in
  let db = unwrap (run (Sqlocaml_unix.open_file_wal ~path ())) in
  Fun.protect
    ~finally:(fun () ->
      (try run (Db.close db) with
       | _ -> ());
      List.iter
        (fun s ->
           try Sys.remove (Filename.concat dir s) with
           | _ -> ())
        [ "fts.db"; "fts.db-wal" ];
      try Unix.rmdir dir with
      | _ -> ())
    (fun () -> f db)
;;

let exec_lwt db sql =
  let open Lwt.Syntax in
  let* r = Db.execute db sql in
  match r with
  | Ok () -> Lwt.return_unit
  | Error e -> Alcotest.failf "exec %S: %a" sql Db.pp_error e
;;

let query_rowids db sql =
  run
    (let open Lwt.Syntax in
     let* s = Lwt.map unwrap (Db.query db sql) in
     Lwt_stream.to_list s)
;;

(* Build an FTS index of [docs] documents; each body has a shared term [alpha]
   plus a unique term [unique<i>].  Returns the mean per-op wall-clock (seconds)
   of a rare-term query (one match), and asserts that query's correctness. *)
let build_and_time db ~docs =
  run (exec_lwt db "CREATE VIRTUAL TABLE docs USING FTS5(body)");
  run
    (let open Lwt.Syntax in
     let* () = exec_lwt db "BEGIN" in
     let rec loop i =
       if i >= docs
       then Lwt.return_unit
       else
         let* () =
           exec_lwt
             db
             (Printf.sprintf "INSERT INTO docs (body) VALUES ('alpha unique%d')" i)
         in
         loop (i + 1)
     in
     let* () = loop 0 in
     exec_lwt db "COMMIT");
  (* Correctness: a rare term matches exactly its one document; the shared term
     matches all of them. *)
  let one = query_rowids db "SELECT body FROM docs WHERE docs MATCH 'unique7'" in
  Alcotest.(check int) "rare term matches exactly 1 doc" 1 (List.length one);
  let all = query_rowids db "SELECT body FROM docs WHERE docs MATCH 'alpha'" in
  Alcotest.(check int) "shared term matches all docs" docs (List.length all);
  (* Speed: time M rare-term queries (each one match), varying the term. *)
  let m = 100 in
  let t0 = now () in
  for i = 0 to m - 1 do
    let k = i * 2654435761 mod docs in
    let rows =
      query_rowids
        db
        (Printf.sprintf "SELECT body FROM docs WHERE docs MATCH 'unique%d'" k)
    in
    if List.length rows <> 1
    then Alcotest.failf "rare term unique%d matched %d docs" k (List.length rows)
  done;
  (now () -. t0) /. float_of_int m
;;

let test_term_query_flat () =
  let small = with_db (fun db -> build_and_time db ~docs:1000) in
  let large = with_db (fun db -> build_and_time db ~docs:3000) in
  let ratio = large /. small in
  Printf.eprintf
    "FTS-SCALING: rare-term query 1k=%.3f ms/op  3k=%.3f ms/op  ratio=%.2f\n%!"
    (small *. 1000.)
    (large *. 1000.)
    ratio;
  (* 3x the index. A drain would cost ~3x; an O(log n) seek is ~flat.  Require
     < 2.0x (wide margin against GC/scheduler noise, but far below the ~3x a
     re-introduced full drain would produce). *)
  Alcotest.(check bool)
    (Printf.sprintf "3x-index rare-term query < 2x slower (got %.2fx)" ratio)
    true
    (ratio < 2.0)
;;

let () =
  Alcotest.run
    "fts_scaling"
    [ ( "scaling"
      , [ Alcotest.test_case "FTS term query is O(log n)" `Slow test_term_query_flat ] )
    ]
;;
