open Lwt.Syntax

(* Default page size when none is supplied (#95). *)
let default_page_size = 4096

(* #772: the message a barrier-less adapter reports from [sync], and the reason
   text it hands to [Store.open_block]'s [~barrier].  Kept as one string so the
   refusal a caller sees at open time and the error a stray [sync] would return
   say the same thing, and so a test can pin the wording in one place. *)
let no_barrier_reason =
  "mirage_backend: Mirage_block.S exposes only get_info/read/write/disconnect and has no \
   flush or barrier operation, so this adapter cannot make a write durable. Supply \
   Mirage_backend.connect's ~barrier argument with a platform-specific flush, or run \
   with PRAGMA synchronous = off"
;;

module Make (B : Mirage_block.S) = struct
  type t =
    { dev : B.t
    ; page_size : int
    ; sectors_per_page : int
    ; capacity : int64
    ; mutable n_pages : int64
    ; barrier : (unit -> (unit, string) result Lwt.t) option
    }

  let connect ?(page_size = default_page_size) ?barrier dev =
    let* info = B.get_info dev in
    let sector_size = info.Mirage_block.sector_size in
    if page_size mod sector_size <> 0
    then
      Lwt.fail_with
        (Printf.sprintf
           "mirage_backend: page_size %d not divisible by sector_size %d"
           page_size
           sector_size)
    else (
      let sectors_per_page = page_size / sector_size in
      let capacity =
        Int64.div info.Mirage_block.size_sectors (Int64.of_int sectors_per_page)
      in
      Lwt.return { dev; page_size; sectors_per_page; capacity; n_pages = 0L; barrier })
  ;;

  let n_pages t = t.n_pages
  let page_size t = t.page_size

  let in_capacity t page_id =
    Int64.compare page_id 0L >= 0 && Int64.compare page_id t.capacity < 0
  ;;

  let read_page t ~page_id buf =
    if not (in_capacity t page_id)
    then
      Lwt.return
        (Error
           (Printf.sprintf
              "read_page: page_id=%Ld out of capacity=%Ld"
              page_id
              t.capacity))
    else (
      let sector = Int64.mul page_id (Int64.of_int t.sectors_per_page) in
      let* r = B.read t.dev sector [ buf ] in
      Lwt.return (Result.map_error (Format.asprintf "%a" B.pp_error) r))
  ;;

  let write_page t ~page_id buf =
    if not (in_capacity t page_id)
    then
      Lwt.return
        (Error
           (Printf.sprintf
              "write_page: page_id=%Ld out of capacity=%Ld"
              page_id
              t.capacity))
    else (
      let sector = Int64.mul page_id (Int64.of_int t.sectors_per_page) in
      let* r = B.write t.dev sector [ buf ] in
      Lwt.return (Result.map_error (Format.asprintf "%a" B.pp_write_error) r))
  ;;

  (* #772: this used to be [Lwt.return (Ok ())].  There is nothing in
     [Mirage_block.S] for it to call, so it reported every commit durable while
     the bytes were still in the host's page cache — under the default
     [synchronous = full] that is a durability claim the adapter cannot honour,
     and crash recovery had nothing to recover to.  With no [~barrier] supplied
     it now returns [Error] instead of lying.  [Store.open_block]'s [~barrier]
     is what turns this into a refusal at the point the durability level is
     chosen rather than a failure on some later commit; this arm is the
     defence-in-depth behind it, for any path that reaches [sync] anyway. *)
  let sync t () =
    match t.barrier with
    | Some flush -> flush ()
    | None -> Lwt.return (Error no_barrier_reason)
  ;;

  let durability_barrier t =
    match t.barrier with
    | Some _ -> `Available
    | None -> `Unavailable no_barrier_reason
  ;;

  let resize t ~n_pages =
    if Int64.compare n_pages t.capacity > 0
    then
      Lwt.return
        (Error
           (Printf.sprintf
              "resize: %Ld pages exceeds device capacity %Ld"
              n_pages
              t.capacity))
    else (
      t.n_pages <- n_pages;
      Lwt.return (Ok ()))
  ;;

  let close t = B.disconnect t.dev
end

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
