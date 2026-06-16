(** Tests for the Pager page-event seam (#384). *)

open Sqlocaml_storage

(* Keep cache assertions deterministic across this exe. *)
let () = Unix.putenv "SQLOCAML_PAGE_CACHE" "64"

type mock_block =
  { store : (int64, Bytes.t) Hashtbl.t
  ; mutable n_pages : int64 [@warning "-69"]
  }

let make_mock () = { store = Hashtbl.create 16; n_pages = 0L }

let mock_callbacks mb =
  let read_page ~page_id buf =
    (match Hashtbl.find_opt mb.store page_id with
     | None -> Cstruct.memset buf 0
     | Some bytes -> Cstruct.blit_from_bytes bytes 0 buf 0 Page.page_size);
    Lwt.return_ok ()
  in
  let write_page ~page_id buf =
    let bytes = Bytes.create Page.page_size in
    Cstruct.blit_to_bytes buf 0 bytes 0 Page.page_size;
    Hashtbl.replace mb.store page_id bytes;
    Lwt.return_ok ()
  in
  let sync () = Lwt.return_ok () in
  let resize ~n_pages =
    mb.n_pages <- n_pages;
    Lwt.return_ok ()
  in
  read_page, write_page, sync, resize
;;

let make_pager ?(n_pages = 0L) ?(freelist = Freelist.empty) () =
  let mb = make_mock () in
  let read_page, write_page, sync, resize = mock_callbacks mb in
  let pager = Pager.create ~read_page ~write_page ~sync ~resize ~n_pages ~freelist in
  pager, mb
;;

let run = Lwt_main.run

(* Attach a recorder; returns the event-list ref (newest-first). *)
let recorder pager =
  let seen = ref [] in
  Pager.set_page_event_callback pager (Some (fun ev -> seen := ev :: !seen));
  seen
;;

let events seen = List.rev !seen

let test_no_ops_no_events () =
  let p, _ = make_pager () in
  let seen = recorder p in
  Alcotest.(check int) "no events before any op" 0 (List.length (events seen))
;;

let test_set_none_clears () =
  let p, _ = make_pager () in
  let seen = recorder p in
  Pager.set_page_event_callback p None;
  (* a read that would emit (Task 2) must produce nothing once cleared *)
  let _ = run (Pager.read p 0L) in
  Alcotest.(check int) "no events after clearing callback" 0 (List.length (events seen))
;;

let is_read = function
  | Pager_event.Page_read _ -> true
  | _ -> false
;;

let test_cache_miss_emits_read () =
  let p, _ = make_pager ~n_pages:4L () in
  let seen = recorder p in
  let _ = run (Pager.read p 2L) in
  let reads = List.filter is_read (events seen) in
  Alcotest.(check int) "one Page_read on cache miss" 1 (List.length reads);
  match reads with
  | [ Pager_event.Page_read { page_id } ] -> Alcotest.(check int64) "page id" 2L page_id
  | _ -> Alcotest.fail "expected exactly one Page_read"
;;

let test_cache_hit_emits_nothing () =
  let p, _ = make_pager ~n_pages:4L () in
  let _ = run (Pager.read p 2L) in
  (* prime cache *)
  let seen = recorder p in
  let _ = run (Pager.read p 2L) in
  (* now a hit *)
  Alcotest.(check int) "no Page_read on cache hit" 0 (List.length (events seen))
;;

let test_borrow_miss_emits_read () =
  let p, _ = make_pager ~n_pages:4L () in
  let seen = recorder p in
  let _ = run (Pager.read_borrow p 3L (fun _ -> Lwt.return_unit)) in
  Alcotest.(check int)
    "one Page_read on borrow cache miss"
    1
    (List.length (List.filter is_read (events seen)))
;;

let () =
  Alcotest.run
    "pager_event"
    [ ( "plumbing"
      , [ Alcotest.test_case "no ops no events" `Quick test_no_ops_no_events
        ; Alcotest.test_case "set None clears" `Quick test_set_none_clears
        ] )
    ; ( "read"
      , [ Alcotest.test_case "cache miss emits read" `Quick test_cache_miss_emits_read
        ; Alcotest.test_case "cache hit emits nothing" `Quick test_cache_hit_emits_nothing
        ; Alcotest.test_case "borrow miss emits read" `Quick test_borrow_miss_emits_read
        ] )
    ]
;;
