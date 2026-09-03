(** #613 — a freshly created WAL must not get the same [(salt, seed)]
    generation marker in every process.

    [Wal.init_header] drew both words from [Random.int64]. Nothing in the tree
    calls [Random.self_init] (and [self_init] is not available to a MirageOS
    unikernel in the first place), so OCaml's default PRNG state is identical in
    every freshly started process and every WAL such a process created got the
    same pair.

    Since #636 that pair is the generation marker recovery trusts to decide
    which frames belong to this file, so the consequence is not merely thin
    checksum entropy: a [-wal] file restored next to the WRONG main database
    verifies as that database's own log by construction.

    The fix draws the marker from {!Mirage_crypto_rng} — the entropy source
    [lib/storage] already depends on, and one that works on both platforms —
    with a settable seam for callers that have their own entropy or need the
    marker to be reproducible.

    Each test below restores the PRNG state the first "process" started from,
    which is how a same-process test reproduces the cross-process defect. *)

open Lwt.Syntax
module Wal = Granary_storage.Wal

(* ------------------------------------------------------------------ *)
(* In-memory byte-addressable device (same shape as test_wal.ml)        *)
(* ------------------------------------------------------------------ *)

type dev = { mutable buf : Bytes.t }

let mk_dev size = { buf = Bytes.make size '\x00' }
let dev_size d = Int64.of_int (Bytes.length d.buf)

let dev_grow d need =
  let cur = Bytes.length d.buf in
  if need > cur
  then (
    let new_size = max need (cur * 2) in
    let nb = Bytes.make new_size '\x00' in
    Bytes.blit d.buf 0 nb 0 cur;
    d.buf <- nb)
;;

let read_at d ~offset out =
  let off = Int64.to_int offset in
  let len = Cstruct.length out in
  if off + len > Bytes.length d.buf
  then Lwt.return (Error "read past EOF")
  else (
    Cstruct.blit_from_bytes d.buf off out 0 len;
    Lwt.return (Ok ()))
;;

let write_at d ~offset src =
  let off = Int64.to_int offset in
  let len = Cstruct.length src in
  dev_grow d (off + len);
  let tmp = Bytes.create len in
  Cstruct.blit_to_bytes src 0 tmp 0 len;
  Bytes.blit tmp 0 d.buf off len;
  Lwt.return (Ok ())
;;

let sync_ok () = Lwt.return (Ok ())

let open_on d =
  let* r =
    Wal.open_
      ~read_at:(read_at d)
      ~write_at:(write_at d)
      ~sync:sync_ok
      ~size_bytes:(dev_size d)
      ()
  in
  match r with
  | Ok w -> Lwt.return w
  | Error e -> Alcotest.failf "Wal.open_: %a" Wal.pp_error e
;;

(* Create a WAL on a fresh device, from the SAME [Random] state each time —
   i.e. as a freshly started process would. *)
let fresh_marker st =
  Random.set_state st;
  let d = mk_dev 65536 in
  let* w = open_on d in
  Lwt.return (Wal.salt w, Wal.seed w)
;;

(* ------------------------------------------------------------------ *)
(* 1. The defect                                                        *)
(* ------------------------------------------------------------------ *)

(* Two "processes" that start from the identical PRNG state must still create
   WALs with different markers.  This is the case that fails on main. *)
let two_fresh_wals_do_not_share_a_marker () =
  Mirage_crypto_rng_unix.use_default ();
  Wal.reset_initial_marker_source ();
  Lwt_main.run
    (let st = Random.get_state () in
     let* salt_a, seed_a = fresh_marker st in
     let* salt_b, seed_b = fresh_marker st in
     Alcotest.(check bool)
       "two freshly created WALs do not share a salt"
       false
       (Int64.equal salt_a salt_b);
     Alcotest.(check bool)
       "two freshly created WALs do not share a seed"
       false
       (Int64.equal seed_a seed_b);
     Lwt.return_unit)
;;

(* Ten of them, so a single unlucky draw cannot pass this by accident and a
   source that varies only one of the two words is caught. *)
let ten_fresh_wals_are_all_distinct () =
  Mirage_crypto_rng_unix.use_default ();
  Wal.reset_initial_marker_source ();
  Lwt_main.run
    (let st = Random.get_state () in
     let seen = Hashtbl.create 16 in
     let* () =
       Lwt_list.iter_s
         (fun _ ->
            let* salt, seed = fresh_marker st in
            Hashtbl.replace seen (salt, seed) ();
            Lwt.return_unit)
         [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]
     in
     Alcotest.(check int) "10 distinct markers" 10 (Hashtbl.length seen);
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* 2. The seam                                                          *)
(* ------------------------------------------------------------------ *)

(* A caller with its own entropy — or a fault-injection test that needs the
   marker to be REPRODUCIBLE rather than fresh — supplies the pair. *)
let an_explicit_source_is_used_verbatim () =
  Mirage_crypto_rng_unix.use_default ();
  Wal.set_initial_marker_source (fun () -> Some (0x0123_4567_89AB_CDEFL, 0x7654_3210L));
  Fun.protect ~finally:Wal.reset_initial_marker_source (fun () ->
    Lwt_main.run
      (let d = mk_dev 65536 in
       let* w = open_on d in
       Alcotest.(check int64)
         "salt is the supplied one"
         0x0123_4567_89AB_CDEFL
         (Wal.salt w);
       Alcotest.(check int64) "seed is the supplied one" 0x7654_3210L (Wal.seed w);
       (* Reproducible: a second fresh WAL gets the same pair. *)
       let d2 = mk_dev 65536 in
       let* w2 = open_on d2 in
       Alcotest.(check int64) "reproducible salt" (Wal.salt w) (Wal.salt w2);
       Alcotest.(check int64) "reproducible seed" (Wal.seed w) (Wal.seed w2);
       Lwt.return_unit))
;;

(* [None] selects the documented degraded fallback: OCaml's default [Random].
   Pinned rather than left implicit, because it is exactly #613's defect and it
   is what an application that never seeds the RNG still gets — the reason the
   fix is the source PLUS [Granary_unix.install] seeding, not the source
   alone. *)
let an_unseeded_source_degrades_to_random () =
  Wal.set_initial_marker_source (fun () -> None);
  Fun.protect ~finally:Wal.reset_initial_marker_source (fun () ->
    Lwt_main.run
      (let st = Random.get_state () in
       let* salt_a, seed_a = fresh_marker st in
       let* salt_b, seed_b = fresh_marker st in
       Alcotest.(check int64) "degraded: same salt from the same PRNG state" salt_a salt_b;
       Alcotest.(check int64) "degraded: same seed from the same PRNG state" seed_a seed_b;
       Lwt.return_unit))
;;

(* The reset restores the default source, so the degraded case above cannot
   leak into anything that runs after it. *)
let reset_restores_the_default_source () =
  Mirage_crypto_rng_unix.use_default ();
  Wal.set_initial_marker_source (fun () -> Some (7L, 8L));
  Wal.reset_initial_marker_source ();
  Lwt_main.run
    (let d = mk_dev 65536 in
     let* w = open_on d in
     Alcotest.(check bool) "not the overridden salt" false (Int64.equal 7L (Wal.salt w));
     Lwt.return_unit)
;;

(* ------------------------------------------------------------------ *)
(* 3. The marker still does its job                                     *)
(* ------------------------------------------------------------------ *)

let page b =
  let p = Cstruct.create 4096 in
  Cstruct.memset p b;
  p
;;

(* A per-file marker is only worth having if frames still verify under it.
   Round-trip a commit so a source that returned, say, a constant zero pair
   would not pass the tests above by being "different enough". *)
let frames_still_verify_under_a_drawn_marker () =
  Mirage_crypto_rng_unix.use_default ();
  Wal.reset_initial_marker_source ();
  Lwt_main.run
    (let d = mk_dev 262144 in
     let* w = open_on d in
     let* r = Wal.append_commit w [ 3L, page 0xAB ] in
     (match r with
      | Ok () -> ()
      | Error e -> Alcotest.failf "append_commit: %a" Wal.pp_error e);
     let* w2 = open_on d in
     Alcotest.(check int) "the commit is recovered" 1 (Wal.committed_frames w2);
     Alcotest.(check int64) "the marker survives the reopen" (Wal.salt w) (Wal.salt w2);
     let* () =
       match Wal.find_page w2 3L with
       | None -> Alcotest.fail "page 3 not found in the reopened WAL"
       | Some idx ->
         let* r = Wal.read_frame w2 idx in
         (match r with
          | Ok p ->
            Alcotest.(check int) "page contents" 0xAB (Cstruct.get_uint8 p 0);
            Lwt.return_unit
          | Error e -> Alcotest.failf "read_frame: %a" Wal.pp_error e)
     in
     Lwt.return_unit)
;;

let () =
  Alcotest.run
    "wal salt (#613)"
    [ ( "fresh markers"
      , [ Alcotest.test_case
            "two fresh WALs do not share a marker"
            `Quick
            two_fresh_wals_do_not_share_a_marker
        ; Alcotest.test_case
            "ten fresh WALs are all distinct"
            `Quick
            ten_fresh_wals_are_all_distinct
        ; Alcotest.test_case
            "frames still verify under a drawn marker"
            `Quick
            frames_still_verify_under_a_drawn_marker
        ] )
    ; ( "the seam"
      , [ Alcotest.test_case
            "an explicit source is used verbatim"
            `Quick
            an_explicit_source_is_used_verbatim
        ; Alcotest.test_case
            "an unseeded source degrades to Random"
            `Quick
            an_unseeded_source_degrades_to_random
        ; Alcotest.test_case
            "reset restores the default source"
            `Quick
            reset_restores_the_default_source
        ] )
    ]
;;
