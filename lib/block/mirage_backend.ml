open Lwt.Syntax

let page_size = 4096

module Make (B : Mirage_block.S) = struct
  type t =
    { dev : B.t
    ; sectors_per_page : int
    ; capacity : int64
    ; mutable n_pages : int64
    }

  let connect dev =
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
      Lwt.return { dev; sectors_per_page; capacity; n_pages = 0L })
  ;;

  let n_pages t = t.n_pages

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

  let sync _t () = Lwt.return (Ok ())

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
