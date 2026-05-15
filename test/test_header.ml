(** Tests for Sqlocaml_storage.Header *)

open Sqlocaml_storage

(* ------------------------------------------------------------------ *)
(* Mock BLOCK backend (same style as test_pager.ml)                    *)
(* ------------------------------------------------------------------ *)

type mock_block = {
  store : (int64, Bytes.t) Hashtbl.t;
}

let make_mock () =
  { store = Hashtbl.create 4 }

let mock_callbacks mb =
  let read_page ~page_id buf =
    (match Hashtbl.find_opt mb.store page_id with
     | None ->
       Cstruct.memset buf 0;
       Lwt.return_ok ()
     | Some bytes ->
       Cstruct.blit_from_bytes bytes 0 buf 0 Page.page_size;
       Lwt.return_ok ())
  in
  let write_page ~page_id buf =
    let bytes = Bytes.create Page.page_size in
    Cstruct.blit_to_bytes buf 0 bytes 0 Page.page_size;
    Hashtbl.replace mb.store page_id bytes;
    Lwt.return_ok ()
  in
  let sync () = Lwt.return_ok () in
  let resize ~n_pages:_ = Lwt.return_ok () in
  (read_page, write_page, sync, resize)

(** Create a pager wired to a fresh mock block, pre-seeded with n_pages. *)
let make_pager ?(n_pages = 2L) () =
  let mb = make_mock () in
  let (read_page, write_page, sync, resize) = mock_callbacks mb in
  let pager =
    Pager.create ~read_page ~write_page ~sync ~resize
      ~n_pages ~freelist:Freelist.empty
  in
  (pager, mb)

(** Create a fresh pager (empty cache) wired to an existing mock block. *)
let fresh_pager_over mb ?(n_pages = 2L) () =
  let (read_page, write_page, sync, resize) = mock_callbacks mb in
  Pager.create ~read_page ~write_page ~sync ~resize
    ~n_pages ~freelist:Freelist.empty

let run = Lwt_main.run

(* ------------------------------------------------------------------ *)
(* Helpers                                                             *)
(* ------------------------------------------------------------------ *)

(** Read the raw bytes stored in the mock for a given page. *)
let raw_page mb page_id =
  match Hashtbl.find_opt mb.store (Int64.of_int page_id) with
  | None ->
    let buf = Cstruct.create Page.page_size in
    Cstruct.memset buf 0;
    buf
  | Some bytes ->
    let buf = Cstruct.create Page.page_size in
    Cstruct.blit_from_bytes bytes 0 buf 0 Page.page_size;
    buf

(** Decode the txn_id stored in a raw page buffer. *)
let txn_id_of_raw buf =
  let f = Page.read_header_fields buf in
  f.Page.txn_id

(** Corrupt a page in the mock store by writing junk bytes. *)
let corrupt_page mb page_id =
  let bytes = Bytes.make Page.page_size '\xFF' in
  Hashtbl.replace mb.store (Int64.of_int page_id) bytes

(** Build a zero header record. *)
let zero_header =
  Header.{ txn_id = 0L; root_page = 0L; freelist_page = 0L;
           n_pages_total = 0L; schema_version = 0L }

(** Build a state record with explicit fields. *)
let make_state ?(root_page=0L) ?(freelist_page=0L) ?(n_pages_total=0L) ?(schema_version=0L) () =
  Header.{ txn_id = 0L; root_page; freelist_page; n_pages_total; schema_version }

(* ------------------------------------------------------------------ *)
(* Unit tests                                                          *)
(* ------------------------------------------------------------------ *)

(* init succeeds; read_live returns txn_id=0, all fields zero *)
let test_init_then_read_live () =
  let (pager, _mb) = make_pager () in
  (match run (Header.init pager) with
   | Error e -> Alcotest.failf "init failed: %a" Header.pp_error e
   | Ok () ->
     match run (Header.read_live pager) with
     | Error e -> Alcotest.failf "read_live failed: %a" Header.pp_error e
     | Ok h ->
       Alcotest.(check int64) "txn_id = 0"         0L h.Header.txn_id;
       Alcotest.(check int64) "root_page = 0"      0L h.Header.root_page;
       Alcotest.(check int64) "freelist_page = 0"  0L h.Header.freelist_page;
       Alcotest.(check int64) "n_pages_total = 0"  0L h.Header.n_pages_total;
       Alcotest.(check int64) "schema_version = 0" 0L h.Header.schema_version)

(* commit increments txn_id; subsequent read_live returns txn_id=1 *)
let test_commit_increments_txn_id () =
  let (pager, _mb) = make_pager () in
  (match run (Header.init pager) with
   | Error e -> Alcotest.failf "init failed: %a" Header.pp_error e
   | Ok () ->
     let prev = zero_header in
     let new_state = make_state () in
     (match run (Header.commit pager ~prev_header:prev ~new_state) with
      | Error e -> Alcotest.failf "commit failed: %a" Header.pp_error e
      | Ok () ->
        match run (Header.read_live pager) with
        | Error e -> Alcotest.failf "read_live after commit: %a" Header.pp_error e
        | Ok h ->
          Alcotest.(check int64) "txn_id after commit = 1" 1L h.Header.txn_id))

(* after N=5 commits, read_live returns txn_id=N *)
let test_multiple_commits () =
  let (pager, _mb) = make_pager () in
  (match run (Header.init pager) with
   | Error e -> Alcotest.failf "init failed: %a" Header.pp_error e
   | Ok () ->
     let n = 5 in
     let prev_h = ref zero_header in
     for _ = 1 to n do
       let new_state = make_state () in
       (match run (Header.commit pager ~prev_header:!prev_h ~new_state) with
        | Error e -> Alcotest.failf "commit failed: %a" Header.pp_error e
        | Ok () ->
          match run (Header.read_live pager) with
          | Error e -> Alcotest.failf "read_live failed: %a" Header.pp_error e
          | Ok h -> prev_h := h)
     done;
     Alcotest.(check int64) "txn_id = 5 after 5 commits"
       (Int64.of_int n) !prev_h.Header.txn_id)

(* commit alternates between page 0 and page 1 *)
let test_commit_alternates_pages () =
  let (pager, mb) = make_pager () in
  (match run (Header.init pager) with
   | Error e -> Alcotest.failf "init failed: %a" Header.pp_error e
   | Ok () ->
     (* After init: both pages have txn_id=0.
        First commit (prev.txn_id=0): inactive = (0+1) mod 2 = page 1 → page 1 gets txn=1.
        Second commit (prev.txn_id=1): inactive = (1+1) mod 2 = page 0 → page 0 gets txn=2.
        Third commit (prev.txn_id=2): inactive = (2+1) mod 2 = page 1 → page 1 gets txn=3. *)
     let commit_and_check expected_page expected_txn prev_h =
       let new_state = make_state () in
       (match run (Header.commit pager ~prev_header:prev_h ~new_state) with
        | Error e -> Alcotest.failf "commit failed: %a" Header.pp_error e
        | Ok () ->
          let target_buf = raw_page mb expected_page in
          let tid = txn_id_of_raw target_buf in
          Alcotest.(check int64)
            (Printf.sprintf "page %d has txn_id=%Ld" expected_page expected_txn)
            expected_txn tid)
     in
     (* First commit: write page 1, txn=1 *)
     commit_and_check 1 1L zero_header;
     (* Second commit: write page 0, txn=2 *)
     let h1 = Header.{ zero_header with txn_id = 1L } in
     commit_and_check 0 2L h1;
     (* Third commit: write page 1, txn=3 *)
     let h2 = Header.{ zero_header with txn_id = 2L } in
     commit_and_check 1 3L h2)

(* corrupt page 0 → read_live returns the valid header from page 1 *)
let test_corrupt_page0_fallback_to_page1 () =
  let (pager, mb) = make_pager () in
  (match run (Header.init pager) with
   | Error e -> Alcotest.failf "init failed: %a" Header.pp_error e
   | Ok () ->
     (* Commit once so page 1 has txn_id=1 and page 0 still has txn_id=0. *)
     let new_state = make_state () in
     (match run (Header.commit pager ~prev_header:zero_header ~new_state) with
      | Error e -> Alcotest.failf "commit failed: %a" Header.pp_error e
      | Ok () ->
        corrupt_page mb 0;
        (* Use a fresh pager to bypass the cache and read from the mock store. *)
        let pager2 = fresh_pager_over mb () in
        match run (Header.read_live pager2) with
        | Error e -> Alcotest.failf "read_live with corrupt p0: %a" Header.pp_error e
        | Ok h ->
          Alcotest.(check int64) "falls back to page 1 (txn_id=1)" 1L h.Header.txn_id))

(* corrupt page 1 → read_live returns the valid header from page 0 *)
let test_corrupt_page1_fallback_to_page0 () =
  let (pager, mb) = make_pager () in
  (match run (Header.init pager) with
   | Error e -> Alcotest.failf "init failed: %a" Header.pp_error e
   | Ok () ->
     (* After init both pages have txn_id=0.
        First commit writes page 1 (txn_id=1).
        Second commit writes page 0 (txn_id=2). *)
     let commit h =
       let new_state = make_state () in
       match run (Header.commit pager ~prev_header:h ~new_state) with
       | Error e -> Alcotest.failf "commit failed: %a" Header.pp_error e
       | Ok () -> ()
     in
     commit zero_header;              (* page 1 ← txn=1 *)
     commit Header.{ zero_header with txn_id = 1L };  (* page 0 ← txn=2 *)
     corrupt_page mb 1;
     (* Use a fresh pager to bypass the cache and read from the mock store. *)
     let pager2 = fresh_pager_over mb () in
     (match run (Header.read_live pager2) with
      | Error e -> Alcotest.failf "read_live with corrupt p1: %a" Header.pp_error e
      | Ok h ->
        Alcotest.(check int64) "falls back to page 0 (txn_id=2)" 2L h.Header.txn_id))

(* both pages corrupt → read_live returns Error Both_headers_corrupt *)
let test_both_corrupt () =
  let (pager, mb) = make_pager () in
  (match run (Header.init pager) with
   | Error e -> Alcotest.failf "init failed: %a" Header.pp_error e
   | Ok () ->
     corrupt_page mb 0;
     corrupt_page mb 1;
     (* Use a fresh pager to bypass the cache and read from the mock store. *)
     let pager2 = fresh_pager_over mb () in
     match run (Header.read_live pager2) with
     | Ok _ -> Alcotest.fail "expected Both_headers_corrupt"
     | Error Header.Both_headers_corrupt -> ()   (* expected *)
     | Error e -> Alcotest.failf "wrong error: %a" Header.pp_error e)

(* non-Header kind page is treated as corrupt *)
let test_non_header_kind_treated_as_corrupt () =
  let (pager, mb) = make_pager () in
  (match run (Header.init pager) with
   | Error e -> Alcotest.failf "init failed: %a" Header.pp_error e
   | Ok () ->
     (* Commit so page 1 has txn_id=1 (the live header). *)
     let new_state = make_state () in
     (match run (Header.commit pager ~prev_header:zero_header ~new_state) with
      | Error e -> Alcotest.failf "commit failed: %a" Header.pp_error e
      | Ok () ->
        (* Overwrite page 0 with a valid Leaf page (kind=Leaf) — correct CRC but
           wrong kind, so it should be treated as corrupt. *)
        let buf = Cstruct.create Page.page_size in
        Page.write_common buf
          { Page.kind = Page.Leaf; flags = 0; n_keys = 0;
            right_page = 0l; crc32 = 0l };
        Page.seal buf;
        let bytes = Bytes.create Page.page_size in
        Cstruct.blit_to_bytes buf 0 bytes 0 Page.page_size;
        Hashtbl.replace mb.store 0L bytes;
        (* Use a fresh pager to bypass the cache and read from the mock store. *)
        let pager2 = fresh_pager_over mb () in
        (match run (Header.read_live pager2) with
         | Error e -> Alcotest.failf "read_live with leaf page 0: %a" Header.pp_error e
         | Ok h ->
           Alcotest.(check int64) "uses page 1 (txn_id=1)" 1L h.Header.txn_id)))

(* ------------------------------------------------------------------ *)
(* QCheck property tests                                               *)
(* ------------------------------------------------------------------ *)

(** Random sequence of commits; read_live always returns the latest state. *)
let prop_commit_sequence =
  QCheck.Test.make
    ~name:"prop_commit_sequence"
    ~count:10_000
    QCheck.(Gen.int_range 0 20 |> make)
    (fun n_commits ->
       let (pager, _mb) = make_pager () in
       (* init *)
       (match run (Header.init pager) with
        | Error _ -> false   (* init failure is a test infrastructure issue; skip *)
        | Ok () ->
          let rec loop i prev_h =
            if i = 0 then
              (* read_live must return txn_id = n_commits *)
              (match run (Header.read_live pager) with
               | Error _ -> false
               | Ok h -> Int64.equal h.Header.txn_id (Int64.of_int n_commits))
            else begin
              let new_state =
                Header.{ txn_id = 0L; root_page = Int64.of_int i;
                         freelist_page = 0L; n_pages_total = Int64.of_int (i + 2);
                         schema_version = 0L }
              in
              match run (Header.commit pager ~prev_header:prev_h ~new_state) with
              | Error _ -> false
              | Ok () ->
                match run (Header.read_live pager) with
                | Error _ -> false
                | Ok h ->
                  if not (Int64.equal h.Header.txn_id (Int64.of_int (n_commits - i + 1)))
                  then false
                  else loop (i - 1) h
            end
          in
          loop n_commits zero_header))

(* ------------------------------------------------------------------ *)
(* RUNNER                                                              *)
(* ------------------------------------------------------------------ *)

let () =
  let qcheck_tests =
    List.map QCheck_alcotest.to_alcotest [
      prop_commit_sequence;
    ]
  in
  Alcotest.run "header" [
    "basic", [
      Alcotest.test_case "init then read_live"                  `Quick test_init_then_read_live;
      Alcotest.test_case "commit increments txn_id"             `Quick test_commit_increments_txn_id;
      Alcotest.test_case "multiple commits (N=5)"               `Quick test_multiple_commits;
      Alcotest.test_case "commit alternates pages"              `Quick test_commit_alternates_pages;
      Alcotest.test_case "corrupt page 0 falls back to page 1"  `Quick test_corrupt_page0_fallback_to_page1;
      Alcotest.test_case "corrupt page 1 falls back to page 0"  `Quick test_corrupt_page1_fallback_to_page0;
      Alcotest.test_case "both pages corrupt"                   `Quick test_both_corrupt;
      Alcotest.test_case "non-Header kind treated as corrupt"   `Quick test_non_header_kind_treated_as_corrupt;
    ];
    "qcheck", qcheck_tests;
  ]
