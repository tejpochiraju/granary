(** QCheck property tests for snapshot isolation and crash recovery
    against the WAL-backed Store (#32). *)

open Lwt.Syntax

module S = struct
  include Sqlocaml_store.Store

  let open_file_wal = Sqlocaml_unix.Store.open_file_wal
end

module Wal = Sqlocaml_storage.Wal

let bs s = Bytes.of_string s
let run = Lwt_main.run
let counter = ref 0

let fresh_path () =
  let n = !counter in
  incr counter;
  Printf.sprintf "/tmp/sqlocaml_crash_prop_%04d.db" n
;;

let cleanup path =
  (try Unix.unlink path with
   | _ -> ());
  try Unix.unlink (path ^ "-wal") with
  | _ -> ()
;;

let ok_store = function
  | Ok t -> t
  | Error e -> failwith (Format.asprintf "open: %a" S.pp_error e)
;;

let bytes_eq a b =
  match a, b with
  | None, None -> true
  | Some a, Some b -> Bytes.equal a b
  | _ -> false
;;

(* ------------------------------------------------------------------ *)
(* prop_snapshot_iso                                                    *)
(* ------------------------------------------------------------------ *)

(* An RO snapshot taken before an RW commit must NEVER observe the
   writes from that commit. We pick K random insertions, take an RO
   snapshot, perform the commit, then read each pre-commit key — none
   should be visible through the snapshot. *)
let prop_snapshot_iso =
  QCheck.Test.make
    ~count:50
    ~name:"snapshot iso: RO never sees later writes"
    QCheck.(list_small (pair (string_size (Gen.return 6)) (string_size (Gen.return 8))))
    (fun kvs ->
       let path = fresh_path () in
       cleanup path;
       try
         run
           (let* sr = S.open_file_wal ~path in
            let st = ok_store sr in
            (* Take RO snapshot first, while DB is empty. *)
            let* ro = S.ro_begin st in
            (* Then commit a bunch of writes. *)
            let* tx = S.rw_begin st in
            let* () = Lwt_list.iter_s (fun (k, v) -> S.put tx 16 (bs k) (bs v)) kvs in
            let* () = S.commit tx in
            (* Every key must still be invisible through the old snapshot. *)
            let ok = ref true in
            let* () =
              Lwt_list.iter_s
                (fun (k, _) ->
                   let* v = S.get ro 16 (bs k) in
                   if not (bytes_eq v None) then ok := false;
                   Lwt.return_unit)
                kvs
            in
            let* () = S.ro_end ro in
            let* () = S.close st in
            Lwt.return !ok)
       with
       | _ ->
         cleanup path;
         false
         |> fun result ->
         cleanup path;
         result)
;;

(* ------------------------------------------------------------------ *)
(* prop_crash_then_open                                                 *)
(* ------------------------------------------------------------------ *)

(* For an arbitrary commit sequence, truncating the WAL at a random
   byte and reopening must reveal the state of SOME prefix of commits
   (possibly the empty one). Either way: every visible key is a key
   that was committed at or before the truncation point. *)

(* Build a sequence of commits and record the cumulative state after
   each. Returns the on-disk WAL byte content and the list of expected
   states. *)
let build_sequence_to_disk ~path ~commits =
  let* sr = S.open_file_wal ~path in
  let st = ok_store sr in
  let prefixes = ref [] in
  let cur = Hashtbl.create 32 in
  let* () =
    Lwt_list.iter_s
      (fun batch ->
         let* tx = S.rw_begin st in
         let* () =
           Lwt_list.iter_s
             (fun (k, v) ->
                Hashtbl.replace cur k v;
                S.put tx 16 (bs k) (bs v))
             batch
         in
         let* () = S.commit tx in
         prefixes := Hashtbl.copy cur :: !prefixes;
         Lwt.return_unit)
      commits
  in
  let* () = S.close st in
  Lwt.return (List.rev !prefixes)
;;

let truncate_wal path keep_bytes =
  let wal_path = path ^ "-wal" in
  let fd = Unix.openfile wal_path [ Unix.O_RDWR ] 0o644 in
  Unix.ftruncate fd keep_bytes;
  Unix.close fd
;;

let read_all_keys st keys =
  let* tx = S.ro_begin st in
  let* observed =
    Lwt_list.map_s
      (fun k ->
         let* v = S.get tx 16 (bs k) in
         Lwt.return (k, v))
      keys
  in
  let* () = S.ro_end tx in
  Lwt.return observed
;;

(* Check whether [observed] (k -> value option) matches the state of
   some prefix in [expected_prefixes]. The empty prefix (Hashtbl with no
   entries) is also valid: every observed value must then be None. *)
let matches_any_prefix observed expected_prefixes =
  let matches state =
    List.for_all
      (fun (k, v) ->
         match v with
         | None -> not (Hashtbl.mem state k)
         | Some bytes_v ->
           (match Hashtbl.find_opt state k with
            | Some s -> Bytes.equal bytes_v (bs s)
            | None -> false))
      observed
  in
  let empty = Hashtbl.create 0 in
  matches empty || List.exists matches expected_prefixes
;;

let prop_crash_then_open =
  QCheck.Test.make
    ~count:30
    ~name:"crash recovery: WAL truncation → prefix"
    QCheck.(
      triple
        (list_small
           (list_small (pair (string_size (Gen.return 4)) (string_size (Gen.return 6)))))
        (int_range 0 4096)
        (int_range 0 4))
    (fun (commits, truncate_offset_extra, _seed) ->
       QCheck.assume (commits <> []);
       let path = fresh_path () in
       cleanup path;
       try
         let all_keys =
           List.concat_map (List.map fst) commits |> List.sort_uniq compare
         in
         let result =
           run
             (let* prefixes = build_sequence_to_disk ~path ~commits in
              (* Get WAL size and truncate to a random byte within it. *)
              let wal_size =
                try Unix.((stat (path ^ "-wal")).st_size) with
                | _ -> 0
              in
              let keep =
                if wal_size = 0 then 0 else truncate_offset_extra mod (wal_size + 1)
              in
              truncate_wal path keep;
              let* sr = S.open_file_wal ~path in
              let st = ok_store sr in
              let* observed = read_all_keys st all_keys in
              let* () = S.close st in
              Lwt.return (matches_any_prefix observed prefixes))
         in
         cleanup path;
         result
       with
       | exn ->
         cleanup path;
         Printf.eprintf "crash_then_open exn: %s\n" (Printexc.to_string exn);
         false)
;;

(* ------------------------------------------------------------------ *)

let () =
  Alcotest.run
    "crash_property"
    [ ( "qcheck"
      , List.map QCheck_alcotest.to_alcotest [ prop_snapshot_iso; prop_crash_then_open ] )
    ]
;;
