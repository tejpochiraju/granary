(** #716: how long a ROLLBACK takes when it bumped a rowid counter on a large
    table.

    Prints; asserts nothing. This is NOT a timing gate — see CLAUDE.md, whose
    gate table is the complete set, and this adds no row to it.

    Skipped unless GRANARY_BENCH_ROLLBACK_716 is set, because populating the
    table takes far longer than any suite should. *)

module Db = struct
  include Granary.Db

  let open_file_wal = Granary_unix.open_file_wal
end

let () = Granary_unix.install ()
let run = Lwt_main.run
let enabled () = Sys.getenv_opt "GRANARY_BENCH_ROLLBACK_716" <> None

let rows =
  match Sys.getenv_opt "GRANARY_BENCH_ROLLBACK_716_ROWS" with
  | Some s -> int_of_string s
  | None -> 300_000
;;

(* Sibling worktrees run suites concurrently, so every path carries the pid —
   the convention in test_rowid_counter_ownership_632.ml. *)
let tmp_path () = Printf.sprintf "/tmp/granary_bench_rollback_716_%d.db" (Unix.getpid ())

let cleanup path =
  List.iter
    (fun p ->
       try Unix.unlink p with
       | _ -> ())
    [ path; path ^ "-wal"; path ^ ".aslog" ]
;;

let exec db sql =
  match run (Db.execute db sql) with
  | Ok () -> ()
  | Error e -> Alcotest.failf "error in %S: %a" sql Db.pp_error e
;;

let batch_size = 1_000

(* Insert [rows] rows in batches, each batch inside its own explicit
   transaction, so the population phase itself does not dominate the run. *)
let populate db ~rows =
  let inserted = ref 0 in
  while !inserted < rows do
    let n = min batch_size (rows - !inserted) in
    exec db "BEGIN";
    for i = 1 to n do
      ignore i;
      exec db "INSERT INTO t (v) VALUES ('x')"
    done;
    exec db "COMMIT";
    inserted := !inserted + n
  done
;;

let median (xs : float array) =
  let sorted = Array.copy xs in
  Array.sort compare sorted;
  let n = Array.length sorted in
  if n mod 2 = 1 then sorted.(n / 2) else (sorted.((n / 2) - 1) +. sorted.(n / 2)) /. 2.0
;;

let time_rollback db =
  let t0 = Unix.gettimeofday () in
  exec db "BEGIN";
  exec db "INSERT INTO t (v) VALUES ('doomed')";
  exec db "ROLLBACK";
  Unix.gettimeofday () -. t0
;;

let main () =
  if not (enabled ())
  then
    Printf.printf
      "bench_rollback_recompute_716: skipped (set GRANARY_BENCH_ROLLBACK_716 to run)\n%!"
  else (
    let path = tmp_path () in
    cleanup path;
    Fun.protect
      ~finally:(fun () -> cleanup path)
      (fun () ->
         let db =
           match run (Db.open_file_wal ~path ()) with
           | Ok d -> d
           | Error e -> Alcotest.failf "open_file_wal: %a" Db.pp_error e
         in
         exec db "CREATE TABLE t (v TEXT)";
         Printf.printf "bench_rollback_recompute_716: populating %d rows...\n%!" rows;
         let t_pop0 = Unix.gettimeofday () in
         populate db ~rows;
         let t_pop1 = Unix.gettimeofday () in
         Printf.printf
           "bench_rollback_recompute_716: populated %d rows in %.3f s\n%!"
           rows
           (t_pop1 -. t_pop0);
         let trials = 5 in
         let durations = Array.make trials 0.0 in
         for i = 0 to trials - 1 do
           let d = time_rollback db in
           durations.(i) <- d;
           Printf.printf
             "bench_rollback_recompute_716: trial %d/%d ROLLBACK = %.3f ms\n%!"
             (i + 1)
             trials
             (d *. 1000.0)
         done;
         let med = median durations in
         Printf.printf
           "bench_rollback_recompute_716: rows=%d median ROLLBACK = %.3f ms\n%!"
           rows
           (med *. 1000.0);
         run (Db.close db)))
;;

let () = main ()
