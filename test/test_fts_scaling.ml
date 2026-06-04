(** #233 regression: FTS term/prefix queries must be O(log n) in the index size,
    not O(n).

    Before the fix [fts_posting_list] / [fts_prefix_posting_list] opened a
    [Store] cursor that materialised the ENTIRE FTS index tree into a list per
    query (the same drain that made #228/#229 O(n)).  They now use the native
    streaming [Store.seek_ge].

    The assertions are machine-independent: each times the *same* query against
    two index sizes (1x and 3x) and requires the per-op cost to stay roughly
    flat.  A query that drains the whole index grows ~linearly with size; an
    O(log n) seek does not.  Two paths are covered:
      - exact term ([fts_posting_list]) via [MATCH 'unique<k>'] (one match);
      - prefix ([fts_prefix_posting_list]) via [MATCH 'zebra*'] over a FIXED
        small set of zebra docs present in both tables (so the match count is
        constant and only the surrounding index size varies).

    Validated: reverting either site to the old [cursor_open] drain makes the
    corresponding ratio ~3.0x (1x->3x index), tripping the gate.

    The ratio gate is a wall-clock measurement: on a shared, loaded CI runner a
    sub-millisecond 1k baseline is noise-dominated and the ratio flakes.  As with
    the [bench_*] suites, CI neutralizes it via [SQLOCAML_BENCH_MAX_RATIO] (set
    high) so the benches still run and print, without failing on load. *)

module Db = Sqlocaml.Db

let run = Lwt_main.run

let unwrap = function
  | Ok v -> v
  | Error e -> Alcotest.failf "db error: %a" Db.pp_error e
;;

let now () = Unix.gettimeofday ()

(* Timing-ratio ceiling for the O(log n) gate.  Defaults to 2.0 — a wide margin
   vs GC/scheduler noise, well below the ~3x a re-introduced O(n) drain produces.
   Raised via [SQLOCAML_BENCH_MAX_RATIO] to neutralize the gate on loaded CI. *)
let max_ratio =
  match Sys.getenv_opt "SQLOCAML_BENCH_MAX_RATIO" with
  | Some v ->
    (try float_of_string v with
     | _ -> 2.0)
  | None -> 2.0
;;

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

(* A fixed, size-independent set of docs matched by the prefix query, so
   [MATCH 'zebra*'] returns the same count regardless of the index size. *)
let n_zebra = 5

let mean_per_op f ~reps =
  let t0 = now () in
  for i = 0 to reps - 1 do
    f i
  done;
  (now () -. t0) /. float_of_int reps
;;

(* Build an FTS index: [n_zebra] fixed zebra docs (matched by the prefix query)
   plus [docs] padding docs each with a shared term [alpha] and a unique term
   [unique<i>].  Returns (term_query_per_op, prefix_query_per_op) in seconds,
   asserting correctness of both. *)
let build_and_time db ~docs =
  run (exec_lwt db "CREATE VIRTUAL TABLE docs USING FTS5(body)");
  run
    (let open Lwt.Syntax in
     let* () = exec_lwt db "BEGIN" in
     let* () =
       let rec loop j =
         if j >= n_zebra
         then Lwt.return_unit
         else
           let* () =
             exec_lwt db (Printf.sprintf "INSERT INTO docs (body) VALUES ('zebra%d')" j)
           in
           loop (j + 1)
       in
       loop 0
     in
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
  (* Correctness: a rare term matches its one doc; the shared term matches all
     padding docs; the prefix matches exactly the fixed zebra set. *)
  let one = query_rowids db "SELECT body FROM docs WHERE docs MATCH 'unique7'" in
  Alcotest.(check int) "rare term matches exactly 1 doc" 1 (List.length one);
  let all = query_rowids db "SELECT body FROM docs WHERE docs MATCH 'alpha'" in
  Alcotest.(check int) "shared term matches all padding docs" docs (List.length all);
  let zs = query_rowids db "SELECT body FROM docs WHERE docs MATCH 'zebra*'" in
  Alcotest.(check int) "prefix matches the fixed zebra set" n_zebra (List.length zs);
  (* Speed: exact-term path (one match, varying term) and prefix path (fixed
     match set). *)
  let term_per_op =
    mean_per_op ~reps:100 (fun i ->
      let k = i * 2654435761 mod docs in
      let rows =
        query_rowids
          db
          (Printf.sprintf "SELECT body FROM docs WHERE docs MATCH 'unique%d'" k)
      in
      if List.length rows <> 1
      then Alcotest.failf "rare term unique%d matched %d docs" k (List.length rows))
  in
  let prefix_per_op =
    mean_per_op ~reps:100 (fun _ ->
      let rows = query_rowids db "SELECT body FROM docs WHERE docs MATCH 'zebra*'" in
      if List.length rows <> n_zebra
      then Alcotest.failf "prefix zebra* matched %d docs" (List.length rows))
  in
  term_per_op, prefix_per_op
;;

let assert_flat label small large =
  let ratio = large /. small in
  Printf.eprintf
    "FTS-SCALING: %s 1k=%.3f ms/op  3k=%.3f ms/op  ratio=%.2f\n%!"
    label
    (small *. 1000.)
    (large *. 1000.)
    ratio;
  (* 3x the surrounding index.  A full drain costs ~3x (validated); an O(log n)
     seek is ~flat.  Gate at < [max_ratio] (default 2.0): wide margin vs
     GC/scheduler noise, well below the ~3x a re-introduced drain produces;
     neutralized on loaded CI via SQLOCAML_BENCH_MAX_RATIO. *)
  Alcotest.(check bool)
    (Printf.sprintf "%s: 3x index < %.1fx slower (got %.2fx)" label max_ratio ratio)
    true
    (ratio < max_ratio)
;;

let test_fts_queries_flat () =
  let term_s, prefix_s = with_db (fun db -> build_and_time db ~docs:1000) in
  let term_l, prefix_l = with_db (fun db -> build_and_time db ~docs:3000) in
  assert_flat "exact-term query" term_s term_l;
  assert_flat "prefix query" prefix_s prefix_l
;;

let () =
  Alcotest.run
    "fts_scaling"
    [ ( "scaling"
      , [ Alcotest.test_case
            "FTS term + prefix queries are O(log n)"
            `Slow
            test_fts_queries_flat
        ] )
    ]
;;
