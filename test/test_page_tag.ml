(** #174 — per-page schema-fingerprint stamp.

    Every B+-tree Branch/Leaf page of a tree carries that tree's 32-bit page
    tag (the low word of its schema fingerprint) in the reserved header bytes
    12–15, so an orphaned page self-identifies its schema during recovery
    (#85).  The tag is set per-tree via [Store.set_tree_tag]; pages of untagged
    trees (system trees, freelist) carry 0. *)

open Lwt.Syntax
open Granary_storage
module S = Granary_store.Store

let run = Lwt_main.run

type dev = { pages : (int64, bytes) Hashtbl.t }

let make_dev () =
  let d = { pages = Hashtbl.create 32 } in
  let read_page ~page_id buf =
    (match Hashtbl.find_opt d.pages page_id with
     | Some b -> Cstruct.blit_from_bytes b 0 buf 0 Page.page_size
     | None -> Cstruct.memset buf 0);
    Lwt.return_ok ()
  in
  let write_page ~page_id buf =
    let b = Bytes.create Page.page_size in
    Cstruct.blit_to_bytes buf 0 b 0 Page.page_size;
    Hashtbl.replace d.pages page_id b;
    Lwt.return_ok ()
  in
  let sync () = Lwt.return_ok () in
  let resize ~n_pages:_ = Lwt.return_ok () in
  d, read_page, write_page, sync, resize
;;

let open_store read_page write_page sync resize =
  let* r =
    S.open_block
      ~init_if_corrupt:true
      ~read_page
      ~write_page
      ~sync
      ~resize
      ~n_pages:0L
      ~close:(fun () -> Lwt.return_unit)
      ()
  in
  match r with
  | Ok st -> Lwt.return st
  | Error e -> Alcotest.failf "open_block: %a" S.pp_error e
;;

let test_structural_pages_carry_tree_tag () =
  run
    (let d, rp, wp, sy, rs = make_dev () in
     let* st = open_store rp wp sy rs in
     let tag = 0x1234ABCDl in
     S.set_tree_tag st 16 tag;
     let* tx = S.rw_begin st in
     let rec ins i =
       if i >= 500
       then Lwt.return_unit
       else
         let* () =
           S.put
             tx
             16
             (Bytes.of_string (Printf.sprintf "k%05d" i))
             (Bytes.of_string (Printf.sprintf "v%05d" i))
         in
         ins (i + 1)
     in
     let* () = ins 0 in
     let* () = S.commit tx in
     let* () = S.close st in
     (* Scan every flushed page; classify Branch/Leaf pages by their tag. *)
     let tagged = ref 0
     and unexpected = ref 0 in
     Hashtbl.iter
       (fun _pid b ->
          let buf = Cstruct.create Page.page_size in
          Cstruct.blit_from_bytes b 0 buf 0 Page.page_size;
          match Page.read_common buf with
          | exception _ -> ()
          | c ->
            if c.Page.kind = Page.Leaf || c.Page.kind = Page.Branch
            then (
              let t = Page.read_tag buf in
              if Int32.equal t tag
              then incr tagged
              else if not (Int32.equal t 0l)
              then incr unexpected))
       d.pages;
     Alcotest.(check bool)
       "user-tree structural pages stamped with the tree tag"
       true
       (!tagged > 0);
     Alcotest.(check int)
       "structural pages carry only the tree tag or 0 (system trees)"
       0
       !unexpected;
     Lwt.return_unit)
;;

(* System trees (here, the meta tree) are never tagged, so their pages stay 0. *)
let test_untagged_tree_pages_are_zero () =
  run
    (let d, rp, wp, sy, rs = make_dev () in
     let* st = open_store rp wp sy rs in
     (* No set_tree_tag for tree 16: its pages must stay 0. *)
     let* tx = S.rw_begin st in
     let rec ins i =
       if i >= 200
       then Lwt.return_unit
       else
         let* () =
           S.put tx 16 (Bytes.of_string (Printf.sprintf "k%05d" i)) (Bytes.of_string "v")
         in
         ins (i + 1)
     in
     let* () = ins 0 in
     let* () = S.commit tx in
     let* () = S.close st in
     let nonzero = ref 0 in
     Hashtbl.iter
       (fun _pid b ->
          let buf = Cstruct.create Page.page_size in
          Cstruct.blit_from_bytes b 0 buf 0 Page.page_size;
          match Page.read_common buf with
          | exception _ -> ()
          | c ->
            if
              (c.Page.kind = Page.Leaf || c.Page.kind = Page.Branch)
              && not (Int32.equal (Page.read_tag buf) 0l)
            then incr nonzero)
       d.pages;
     Alcotest.(check int) "untagged trees leave the page tag at 0" 0 !nonzero;
     Lwt.return_unit)
;;

let () =
  Alcotest.run
    "page_tag"
    [ ( "stamp"
      , [ Alcotest.test_case
            "structural pages carry tree tag"
            `Quick
            test_structural_pages_carry_tree_tag
        ; Alcotest.test_case
            "untagged trees stay zero"
            `Quick
            test_untagged_tree_pages_are_zero
        ] )
    ]
;;
