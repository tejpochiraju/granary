(* #482 — TPC-H-derived benchmark runner.

   Loads a deterministic TPC-H dataset into granary and into reference C SQLite,
   times each query on both, cross-checks the answers, and writes one CSV row per
   (engine, query) to stdout.  Timing is reported, never gated: this is a
   measurement tool.  A wrong *answer*, by contrast, is a real failure and sets a
   non-zero exit code.

   Not audited TPC results — see docs/benchmarks/BENCHMARKS-TPCH.md. *)

module BR = Granary_tpc.Bench_report
module G = Granary_tpc.Tpch_gen
module Q = Granary_tpc.Tpch_queries
module Granary_engine = Granary_tpc.Granary_engine

(* ── reference C SQLite (in-process bindings) ─────────────────────────────────
   PRAGMA parity with test/bench_compare.ml's Ref_sqlite: same page size, same
   WAL journal mode, same fsync-per-commit durability as granary. *)

module Ref_sqlite : BR.ENGINE = struct
  type t = { db : Sqlite3.db }

  let name = "sqlite"

  let ok rc =
    match rc with
    | Sqlite3.Rc.OK | Sqlite3.Rc.DONE | Sqlite3.Rc.ROW -> ()
    | r -> failwith ("sqlite3: " ^ Sqlite3.Rc.to_string r)
  ;;

  let exec t sql = ok (Sqlite3.exec t.db sql)

  let open_db ~dir =
    let path = Filename.concat dir "ref.db" in
    List.iter
      (fun p ->
         try Unix.unlink p with
         | _ -> ())
      [ path; path ^ "-wal"; path ^ "-shm" ];
    let t = { db = Sqlite3.db_open path } in
    exec t "PRAGMA page_size=4096";
    exec t "PRAGMA journal_mode=WAL";
    exec t "PRAGMA synchronous=FULL";
    t
  ;;

  (* Rendered exactly as Granary_engine renders, so equal values compare equal as
     text and the float fallback in [field_eq] only has to absorb arithmetic
     noise. *)
  let render = function
    | Sqlite3.Data.INT i -> Int64.to_string i
    | Sqlite3.Data.FLOAT f -> Printf.sprintf "%.17g" f
    | Sqlite3.Data.TEXT s -> s
    | Sqlite3.Data.BLOB _ -> "<blob>"
    | Sqlite3.Data.NULL | Sqlite3.Data.NONE -> "NULL"
  ;;

  let query_rows t sql =
    let stmt = Sqlite3.prepare t.db sql in
    let acc = ref [] in
    let row () =
      List.init (Sqlite3.data_count stmt) (fun i -> render (Sqlite3.column stmt i))
    in
    let rec loop () =
      match Sqlite3.step stmt with
      | Sqlite3.Rc.ROW ->
        acc := row () :: !acc;
        loop ()
      | Sqlite3.Rc.DONE -> ()
      | r -> failwith ("sqlite3 step: " ^ Sqlite3.Rc.to_string r)
    in
    loop ();
    ok (Sqlite3.finalize stmt);
    List.rev !acc
  ;;

  let close t = if not (Sqlite3.db_close t.db) then failwith "sqlite3: db_close failed"
end

(* ── answer comparison ────────────────────────────────────────────────────── *)

let field_eq a b =
  if a = b
  then true
  else (
    match float_of_string_opt a, float_of_string_opt b with
    | Some fa, Some fb -> BR.real_eq fa fb
    | _ -> false)
;;

let row_eq ra rb = List.length ra = List.length rb && List.for_all2 field_eq ra rb

(* Q2, Q3, Q10, Q18 and Q21 truncate with LIMIT under an ORDER BY that is not a
   total order — Q10 ties on revenue alone, Q18 on (o_totalprice, o_orderdate).
   Two engines may legitimately order tied rows differently, so for these the
   comparison is on the multiset of rows rather than the sequence.  Every other
   query keeps sequence comparison, where a wrong row order IS a defect.

   Residual limit, stated rather than hidden: if the tie spans the LIMIT cut, the
   engines may return genuinely different rows and this still reports MISMATCH.
   That is the correct default — it is indistinguishable from a real disagreement
   without re-deriving the query's tie-break semantics. *)
let tie_prone = [ 2; 3; 10; 18; 21 ]

let rec take_matching ra acc = function
  | [] -> None
  | rb :: tl ->
    if row_eq ra rb
    then Some (List.rev_append acc tl)
    else take_matching ra (rb :: acc) tl
;;

(* Greedy multiset match; result is the rows of [a] with no partner in [b]. *)
let multiset_unmatched a b =
  let remaining = ref b in
  let step acc ra =
    match take_matching ra [] !remaining with
    | Some rest ->
      remaining := rest;
      acc
    | None -> ra :: acc
  in
  List.rev (List.fold_left step [] a)
;;

let rec first_seq_diff i a b =
  match a, b with
  | [], [] -> None
  | ra :: ta, rb :: tb ->
    if row_eq ra rb then first_seq_diff (i + 1) ta tb else Some (i, Some ra, Some rb)
  | ra :: _, [] -> Some (i, Some ra, None)
  | [], rb :: _ -> Some (i, None, Some rb)
;;

(* [None] when the answers agree; [Some report] describing the first
   disagreement otherwise. *)
let compare_rows ~unordered granary_rows sqlite_rows =
  let show r =
    match r with
    | None -> "<missing>"
    | Some row -> String.concat " | " row
  in
  let counts =
    Printf.sprintf
      "granary %d rows, sqlite %d rows"
      (List.length granary_rows)
      (List.length sqlite_rows)
  in
  if unordered
  then (
    let only_g = multiset_unmatched granary_rows sqlite_rows in
    let only_s = multiset_unmatched sqlite_rows granary_rows in
    match only_g, only_s with
    | [], [] when List.length granary_rows = List.length sqlite_rows -> None
    | g, s ->
      Some
        (Printf.sprintf
           "%s; unmatched (multiset compare): granary-only %s / sqlite-only %s"
           counts
           (show (List.nth_opt g 0))
           (show (List.nth_opt s 0))))
  else (
    match first_seq_diff 0 granary_rows sqlite_rows with
    | None -> None
    | Some (i, g, s) ->
      Some
        (Printf.sprintf "%s; row %d: granary %s / sqlite %s" counts i (show g) (show s)))
;;

(* ── setup views ──────────────────────────────────────────────────────────── *)

(* A query's [setup] creates views (Q15's revenue0) on a database that outlives
   the query — reloading the dataset per query is not affordable.  The spec's
   trailing DROP VIEW was omitted on the assumption that the harness discards the
   database between queries; that assumption does not hold, so the harness drops
   the view itself: before setup, so setup is re-runnable across repeats and
   engines, and after the query, so no view leaks into a later one. *)
let setup_views q = List.filter_map Q.view_name_of_setup q.Q.setup

(* ── disk-space guard ─────────────────────────────────────────────────────── *)

(* TPC-H's raw dataset is roughly 1 GB per unit of scale factor; loading it into
   two engines with indexes needs about three times that. *)
let check_disk_space ~dir ~sf =
  let needed_bytes = Int64.of_float (3.0 *. 1.1e9 *. sf) in
  let available =
    (* `df -k` is portable across the dev container and the CI runner; the Alpine
       CI image has no python3 or bc, so keep this to one shell call. *)
    let ic =
      Unix.open_process_in (Printf.sprintf "df -k %s | tail -1" (Filename.quote dir))
    in
    let line =
      try input_line ic with
      | End_of_file -> ""
    in
    ignore (Unix.close_process_in ic);
    match String.split_on_char ' ' line |> List.filter (fun s -> s <> "") with
    | _dev :: _size :: _used :: avail :: _ ->
      (try Int64.mul (Int64.of_string avail) 1024L with
       | _ -> Int64.max_int)
    | _ -> Int64.max_int
  in
  if Int64.compare available needed_bytes < 0
  then
    failwith
      (Printf.sprintf
         "GRANARY_TPCH_SF=%g needs about %Ld MB under %s but only %Ld MB is free"
         sf
         (Int64.div needed_bytes 1_048_576L)
         dir
         (Int64.div available 1_048_576L))
;;

(* ── per-engine execution ─────────────────────────────────────────────────── *)

type outcome =
  { number : int
  ; verdict : string
  ; wall : float
  ; cpu : float
  ; rows : string list list option (* [None] when the query failed or is skipped *)
  }

module Runner (E : BR.ENGINE) = struct
  module Load = Granary_tpc.Tpch_schema.Load (E)

  let best ~repeats f =
    let keep acc =
      let v, w, c = BR.time_it f in
      match acc with
      | Some (_, bw, _) when bw <= w -> acc
      | _ -> Some (v, w, c)
    in
    let rec loop i acc = if i > repeats then acc else loop (i + 1) (keep acc) in
    match loop 1 None with
    | Some r -> r
    | None -> invalid_arg "repeats must be >= 1"
  ;;

  let drop_views e q =
    List.iter
      (fun v -> E.exec e (Printf.sprintf "DROP VIEW IF EXISTS %s" v))
      (setup_views q)
  ;;

  let one e ~repeats q =
    match q.Q.verdict with
    | Q.Skipped _ ->
      { number = q.Q.number
      ; verdict = Q.verdict_label q.Q.verdict
      ; wall = 0.0
      ; cpu = 0.0
      ; rows = None
      }
    (* [Rewritten_pending] queries are still attempted: they are the ones whose
       failure class we want in the CSV, and reference SQLite runs most of them,
       which is the baseline Task 8b needs. *)
    | Q.Native | Q.Rewritten _ | Q.Rewritten_pending _ ->
      (try
         drop_views e q;
         List.iter (E.exec e) q.Q.setup;
         let rows, wall, cpu = best ~repeats (fun () -> E.query_rows e q.Q.sql) in
         drop_views e q;
         { number = q.Q.number
         ; verdict = Q.verdict_label q.Q.verdict
         ; wall
         ; cpu
         ; rows = Some rows
         }
       with
       | exn ->
         (try drop_views e q with
          | _ -> ());
         { number = q.Q.number
         ; verdict = "error: " ^ Printexc.to_string exn
         ; wall = 0.0
         ; cpu = 0.0
         ; rows = None
         })
  ;;

  let run ~dir ~gen ~queries ~repeats =
    let e = E.open_db ~dir in
    Printf.eprintf "[%s] loading…%!" E.name;
    let (), load_wall, _ = BR.time_it (fun () -> Load.run e gen) in
    Printf.eprintf " %.2fs\n%!" load_wall;
    let outcomes = List.map (one e ~repeats) queries in
    E.close e;
    outcomes
  ;;
end

module Granary_runner = Runner (Granary_engine)
module Sqlite_runner = Runner (Ref_sqlite)

(* ── CSV ──────────────────────────────────────────────────────────────────── *)

let columns =
  [ "host"
  ; "engine"
  ; "sf"
  ; "query"
  ; "verdict"
  ; "wall_s"
  ; "cpu_s"
  ; "cpu_wall_ratio"
  ; "rows_out"
  ; "cross_check"
  ]
;;

let emit ~host ~sf ~engine o ~cross_check =
  let ratio = if o.wall > 0.0 then o.cpu /. o.wall else 0.0 in
  print_endline
    (BR.Csv.row
       [ host
       ; engine
       ; Printf.sprintf "%g" sf
       ; string_of_int o.number
       ; o.verdict
       ; Printf.sprintf "%.6f" o.wall
       ; Printf.sprintf "%.6f" o.cpu
       ; Printf.sprintf "%.3f" ratio
       ; (match o.rows with
          | None -> ""
          | Some rows -> string_of_int (List.length rows))
       ; cross_check
       ])
;;

(* ── driver ───────────────────────────────────────────────────────────────── *)

let selected_numbers () =
  match BR.env_str "GRANARY_TPCH_QUERIES" "" with
  | "" -> None
  | s ->
    (* An unparseable token must fail loudly: silently dropping it (as
       [List.filter_map] would) could leave zero queries selected, which
       would run to completion, exit 0, and emit a header-only CSV — an
       error that looks like success. *)
    Some
      (String.split_on_char ',' s
       |> List.map (fun tok ->
         let trimmed = String.trim tok in
         match int_of_string_opt trimmed with
         | Some n -> n
         | None ->
           failwith
             (Printf.sprintf
                "GRANARY_TPCH_QUERIES: %S is not a query number (in %S)"
                trimmed
                s)))
;;

let make_tmp_dir () =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "bench-tpch-%d" (Unix.getpid ()))
  in
  (try Unix.mkdir dir 0o755 with
   | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  dir
;;

(* Two empty answers compare equal, and that agreement carries no information:
   at SF 0.001 Q2 and Q20 read "ok" for two rounds of review and were wrong at
   SF 0.01.  So an all-empty agreement gets its own token — the false-pass class
   is then visible in the artifact instead of depending on a reader noticing. *)
let cross_check_of q g s =
  match g.rows, s.rows with
  | Some gr, Some sr ->
    let unordered = List.mem q.Q.number tie_prone in
    (match compare_rows ~unordered gr sr with
     | None -> if gr = [] && sr = [] then "ok-both-empty" else "ok"
     | Some report ->
       Printf.eprintf "MISMATCH Q%d: %s\n%!" q.Q.number report;
       "MISMATCH")
  | _ -> "skipped"
;;

let () =
  let sf = BR.env_float "GRANARY_TPCH_SF" 0.01 in
  let repeats = max 1 (BR.env_int "GRANARY_TPCH_REPEATS" 3) in
  let seed = BR.env_int "GRANARY_TPC_SEED" 42 in
  let host = BR.host_label () in
  let queries =
    match selected_numbers () with
    | None -> Q.all
    | Some ns -> List.filter (fun q -> List.mem q.Q.number ns) Q.all
  in
  let dir = make_tmp_dir () in
  check_disk_space ~dir ~sf;
  Printf.eprintf
    "tpch: sf=%g seed=%d repeats=%d queries=%d dir=%s\n%!"
    sf
    seed
    repeats
    (List.length queries)
    dir;
  let gen = G.create ~seed ~sf in
  let g_out = Granary_runner.run ~dir ~gen ~queries ~repeats in
  let s_out = Sqlite_runner.run ~dir ~gen ~queries ~repeats in
  print_endline (BR.Csv.header columns);
  let mismatches = ref 0 in
  let report q g s =
    let cross_check = cross_check_of q g s in
    if cross_check = "MISMATCH" then incr mismatches;
    emit ~host ~sf ~engine:"granary" g ~cross_check;
    emit ~host ~sf ~engine:"sqlite" s ~cross_check
  in
  List.iteri (fun i g -> report (List.nth queries i) g (List.nth s_out i)) g_out;
  if !mismatches > 0
  then (
    Printf.eprintf
      "%d quer%s disagreed with SQLite\n%!"
      !mismatches
      (if !mismatches = 1 then "y" else "ies");
    exit 1)
;;
