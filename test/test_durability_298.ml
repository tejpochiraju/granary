(** Tests for #298 — per-deployment durability knob (full/batched/off). *)

open Lwt.Syntax

module S = struct
  include Sqlocaml_store.Store

  let open_file_wal = Sqlocaml_unix.Store.open_file_wal
end

module D = struct
  include Sqlocaml.Db

  let open_file_wal = Sqlocaml_unix.open_file_wal [@@warning "-32"]
end

let run = Lwt_main.run
let counter = ref 0

let fresh_path () =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/sqlocaml_test_dura_298_%04d.db" n
;;

let cleanup path =
  (try Unix.unlink path with
   | _ -> ());
  try Unix.unlink (path ^ "-wal") with
  | _ -> ()
;;

let with_fresh ~f =
  let path = fresh_path () in
  cleanup path;
  Lwt.finalize
    (fun () -> f path)
    (fun () ->
       cleanup path;
       Lwt.return_unit)
;;

let bs = Bytes.of_string [@@warning "-32"]

let open_st path =
  let* sr = S.open_file_wal ~path () in
  match sr with
  | Ok t -> Lwt.return t
  | Error e -> Alcotest.failf "open_file_wal: %a" S.pp_error e
;;

(* --- accessors --- *)

let test_default_is_full () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    Alcotest.(check bool)
      "default Full"
      true
      (match S.durability st with
       | S.Full -> true
       | _ -> false);
    Alcotest.(check int) "default batch commits" 256 (S.sync_batch_commits st);
    Alcotest.(check int) "default batch interval" 100 (S.sync_batch_interval_ms st);
    let* () = S.close st in
    Lwt.return_unit)
;;

let test_set_get_round_trip () =
  run
  @@ with_fresh ~f:(fun path ->
    let* st = open_st path in
    S.set_durability st (S.Batched { commits = 7; interval_ms = 33 });
    Alcotest.(check bool)
      "now Batched"
      true
      (match S.durability st with
       | S.Batched _ -> true
       | _ -> false);
    Alcotest.(check int) "commits stored" 7 (S.sync_batch_commits st);
    Alcotest.(check int) "interval stored" 33 (S.sync_batch_interval_ms st);
    S.set_durability st S.Full;
    Alcotest.(check int) "commits survive mode switch" 7 (S.sync_batch_commits st);
    S.set_durability st (S.Batched { commits = 7; interval_ms = 33 });
    S.set_sync_batch_commits st 99;
    Alcotest.(check int) "granular N" 99 (S.sync_batch_commits st);
    Alcotest.(check bool)
      "still Batched"
      true
      (match S.durability st with
       | S.Batched _ -> true
       | _ -> false);
    let* () = S.close st in
    Lwt.return_unit)
;;

let () =
  Alcotest.run
    "durability_298"
    [ ( "accessors"
      , [ Alcotest.test_case "default is full" `Quick test_default_is_full
        ; Alcotest.test_case "set/get round-trip" `Quick test_set_get_round_trip
        ] )
    ]
;;
