(** Unit tests for the standalone WAL module. *)

open Lwt.Syntax

module Wal = Sqlocaml_storage.Wal

(* ------------------------------------------------------------------ *)
(* In-memory byte-addressable device                                    *)
(* ------------------------------------------------------------------ *)

type dev = {
  mutable buf : Bytes.t;
}

let mk_dev size = { buf = Bytes.make size '\x00' }

let dev_size d = Int64.of_int (Bytes.length d.buf)

let dev_grow d need =
  let cur = Bytes.length d.buf in
  if need > cur then begin
    let new_size = max need (cur * 2) in
    let nb = Bytes.make new_size '\x00' in
    Bytes.blit d.buf 0 nb 0 cur;
    d.buf <- nb
  end

let read_at d ~offset out =
  let off = Int64.to_int offset in
  let len = Cstruct.length out in
  let cur = Bytes.length d.buf in
  if off + len > cur then
    Lwt.return (Error "read past EOF")
  else begin
    Cstruct.blit_from_bytes d.buf off out 0 len;
    Lwt.return (Ok ())
  end

let write_at d ~offset src =
  let off = Int64.to_int offset in
  let len = Cstruct.length src in
  dev_grow d (off + len);
  let tmp = Bytes.create len in
  Cstruct.blit_to_bytes src 0 tmp 0 len;
  Bytes.blit tmp 0 d.buf off len;
  Lwt.return (Ok ())

let sync_ok () = Lwt.return (Ok ())

let fresh_wal ?(size = 65536) () =
  let d = mk_dev size in
  let* r =
    Wal.open_ ~read_at:(read_at d) ~write_at:(write_at d) ~sync:sync_ok
      ~size_bytes:(dev_size d)
  in
  match r with
  | Ok w -> Lwt.return (d, w)
  | Error e -> Alcotest.failf "Wal.open_ failed: %a" Wal.pp_error e

let page_with byte =
  let p = Cstruct.create 4096 in
  Cstruct.set_char p 0 byte;
  p

(* ------------------------------------------------------------------ *)
(* Tests                                                                *)
(* ------------------------------------------------------------------ *)

let test_empty_open () =
  Lwt_main.run (
    let* _, w = fresh_wal () in
    Alcotest.(check int) "fresh WAL has 0 frames" 0 (Wal.committed_frames w);
    Alcotest.(check (option int)) "no page in fresh WAL"
      None (Wal.find_page w 42L);
    Lwt.return_unit)

let test_append_and_read () =
  Lwt_main.run (
    let* _, w = fresh_wal () in
    let p1 = page_with 'A' in
    let p2 = page_with 'B' in
    let* r = Wal.append_commit w [(7L, p1); (9L, p2)] in
    (match r with
     | Ok () -> ()
     | Error e -> Alcotest.failf "append: %a" Wal.pp_error e);
    Alcotest.(check int) "2 frames committed" 2 (Wal.committed_frames w);
    let f7 = match Wal.find_page w 7L with
      | Some i -> i | None -> Alcotest.fail "page 7 missing"
    in
    let* read =
      let* r = Wal.read_frame w f7 in
      (match r with
       | Ok b -> Lwt.return b
       | Error e -> Alcotest.failf "read_frame: %a" Wal.pp_error e)
    in
    Alcotest.(check char) "p7 byte0" 'A' (Cstruct.get_char read 0);
    Lwt.return_unit)

let test_find_page_returns_latest () =
  Lwt_main.run (
    let* _, w = fresh_wal () in
    let p1 = page_with 'X' in
    let p2 = page_with 'Y' in
    let* _ = Wal.append_commit w [(5L, p1)] in
    let* _ = Wal.append_commit w [(5L, p2)] in
    let f5 = Wal.find_page w 5L |> Option.get in
    let* read =
      let* r = Wal.read_frame w f5 in
      match r with
      | Ok b -> Lwt.return b
      | Error e -> Alcotest.failf "read_frame: %a" Wal.pp_error e
    in
    Alcotest.(check char) "latest write wins" 'Y' (Cstruct.get_char read 0);
    Lwt.return_unit)

let test_recovery_replays_committed () =
  Lwt_main.run (
    let* d, w1 = fresh_wal () in
    let* _ = Wal.append_commit w1 [(1L, page_with 'A'); (2L, page_with 'B')] in
    let* _ = Wal.append_commit w1 [(3L, page_with 'C')] in
    Alcotest.(check int) "3 frames before reopen" 3 (Wal.committed_frames w1);
    (* Reopen the WAL on the same device buffer. *)
    let* r =
      Wal.open_ ~read_at:(read_at d) ~write_at:(write_at d) ~sync:sync_ok
        ~size_bytes:(dev_size d)
    in
    let w2 = match r with
      | Ok w -> w
      | Error e -> Alcotest.failf "reopen: %a" Wal.pp_error e
    in
    Alcotest.(check int) "recovered 3 frames" 3 (Wal.committed_frames w2);
    Alcotest.(check (option int)) "page 1 found"  (Wal.find_page w1 1L)
      (Wal.find_page w2 1L);
    Alcotest.(check (option int)) "page 3 found"  (Wal.find_page w1 3L)
      (Wal.find_page w2 3L);
    Lwt.return_unit)

let test_recovery_drops_partial_trailing_batch () =
  Lwt_main.run (
    let* d, w1 = fresh_wal () in
    let* _ = Wal.append_commit w1 [(1L, page_with 'A')] in
    (* Manually scribble a half-frame at frame_idx=1 to simulate a torn
       write: write only the metadata bytes, not the page bytes. *)
    let half = Cstruct.create 24 in
    Cstruct.BE.set_uint64 half 0 99L;        (* page_id *)
    Cstruct.BE.set_uint64 half 8 1L;         (* commit flag *)
    Cstruct.BE.set_uint64 half 16 0L;        (* bogus checksum *)
    let frame_off =
      Int64.add (Int64.of_int Wal.header_size_bytes)
        (Int64.of_int Wal.frame_size_bytes)
    in
    let* _ = write_at d ~offset:frame_off half in
    (* Reopen — the partial frame must be silently discarded. *)
    let* r =
      Wal.open_ ~read_at:(read_at d) ~write_at:(write_at d) ~sync:sync_ok
        ~size_bytes:(dev_size d)
    in
    let w2 = match r with
      | Ok w -> w
      | Error e -> Alcotest.failf "reopen: %a" Wal.pp_error e
    in
    Alcotest.(check int) "trailing partial batch dropped"
      1 (Wal.committed_frames w2);
    Alcotest.(check (option int)) "page 99 absent"
      None (Wal.find_page w2 99L);
    Lwt.return_unit)

let test_recovery_drops_uncommitted_batch () =
  Lwt_main.run (
    let* d, w1 = fresh_wal () in
    (* First batch: committed. *)
    let* _ = Wal.append_commit w1 [(10L, page_with 'A')] in
    (* Manually write a frame WITHOUT a commit flag (simulating a writer
       that crashed before fsyncing the commit marker). *)
    let buf = Cstruct.create Wal.frame_size_bytes in
    Cstruct.BE.set_uint64 buf 0 20L;
    Cstruct.BE.set_uint64 buf 8 0L;    (* commit = false *)
    Cstruct.BE.set_uint64 buf 16 0L;   (* bogus checksum *)
    let off =
      Int64.add (Int64.of_int Wal.header_size_bytes)
        (Int64.of_int Wal.frame_size_bytes)
    in
    let* _ = write_at d ~offset:off buf in
    let* r =
      Wal.open_ ~read_at:(read_at d) ~write_at:(write_at d) ~sync:sync_ok
        ~size_bytes:(dev_size d)
    in
    let w2 = match r with
      | Ok w -> w
      | Error e -> Alcotest.failf "reopen: %a" Wal.pp_error e
    in
    Alcotest.(check int) "uncommitted frame ignored"
      1 (Wal.committed_frames w2);
    Alcotest.(check (option int)) "page 20 absent" None (Wal.find_page w2 20L);
    Lwt.return_unit)

let test_reset_clears_index () =
  Lwt_main.run (
    let* _, w = fresh_wal () in
    let* _ = Wal.append_commit w [(7L, page_with 'A')] in
    Wal.reset w;
    Alcotest.(check int) "reset to 0 frames" 0 (Wal.committed_frames w);
    Alcotest.(check (option int)) "page absent after reset"
      None (Wal.find_page w 7L);
    Lwt.return_unit)

let test_append_overwrites_after_reset () =
  Lwt_main.run (
    let* _, w = fresh_wal () in
    let* _ = Wal.append_commit w [(1L, page_with 'A')] in
    Wal.reset w;
    let* _ = Wal.append_commit w [(2L, page_with 'B')] in
    Alcotest.(check int) "1 frame after re-append" 1 (Wal.committed_frames w);
    Alcotest.(check (option int)) "page 1 gone" None (Wal.find_page w 1L);
    Alcotest.(check bool) "page 2 present"
      true (Wal.find_page w 2L <> None);
    Lwt.return_unit)

(* Test that find_page_at respects the strict-less-than snapshot bound.
   Three commits write page 7 in successive frames (frame_idx 0, 1, 2).
   A reader captured at committed_frames=N should see frame N-1 at most. *)
let test_find_page_at_snapshot () =
  Lwt_main.run (
    let* _, w = fresh_wal () in
    let* _ = Wal.append_commit w [(7L, page_with '\xAA')] in
    let* _ = Wal.append_commit w [(7L, page_with '\xBB')] in
    let* _ = Wal.append_commit w [(7L, page_with '\xCC')] in
    (* Three frames: idx 0, 1, 2. *)
    Alcotest.(check (option int)) "snap=1 sees frame 0" (Some 0)
      (Wal.find_page_at w 7L ~max_frame:1);
    Alcotest.(check (option int)) "snap=2 sees frame 1" (Some 1)
      (Wal.find_page_at w 7L ~max_frame:2);
    Alcotest.(check (option int)) "snap=3 sees frame 2" (Some 2)
      (Wal.find_page_at w 7L ~max_frame:3);
    Alcotest.(check (option int)) "snap=0 sees nothing" None
      (Wal.find_page_at w 7L ~max_frame:0);
    Alcotest.(check (option int)) "absent page" None
      (Wal.find_page_at w 99L ~max_frame:3);
    Lwt.return_unit)

(* Verify find_page still returns the globally latest frame. *)
let test_find_page_still_latest () =
  Lwt_main.run (
    let* _, w = fresh_wal () in
    let* _ = Wal.append_commit w [(3L, page_with 'X')] in
    let* _ = Wal.append_commit w [(3L, page_with 'Y')] in
    let* _ = Wal.append_commit w [(3L, page_with 'Z')] in
    (* find_page must return frame 2 (the latest). *)
    Alcotest.(check (option int)) "latest frame is 2" (Some 2)
      (Wal.find_page w 3L);
    let* read =
      let* r = Wal.read_frame w 2 in
      match r with
      | Ok b -> Lwt.return b
      | Error e -> Alcotest.failf "read_frame: %a" Wal.pp_error e
    in
    Alcotest.(check char) "latest data is Z" 'Z' (Cstruct.get_char read 0);
    Lwt.return_unit)

let () =
  Alcotest.run "wal" [
    "basic", [
      Alcotest.test_case "empty open"              `Quick test_empty_open;
      Alcotest.test_case "append and read"         `Quick test_append_and_read;
      Alcotest.test_case "find latest write"       `Quick test_find_page_returns_latest;
    ];
    "recovery", [
      Alcotest.test_case "replay committed"        `Quick test_recovery_replays_committed;
      Alcotest.test_case "drop partial trailing"   `Quick test_recovery_drops_partial_trailing_batch;
      Alcotest.test_case "drop uncommitted"        `Quick test_recovery_drops_uncommitted_batch;
    ];
    "reset", [
      Alcotest.test_case "clears index"            `Quick test_reset_clears_index;
      Alcotest.test_case "overwrites old frames"   `Quick test_append_overwrites_after_reset;
    ];
    "snapshot", [
      Alcotest.test_case "find_page_at bounds"     `Quick test_find_page_at_snapshot;
      Alcotest.test_case "find_page still latest"  `Quick test_find_page_still_latest;
    ];
  ]
