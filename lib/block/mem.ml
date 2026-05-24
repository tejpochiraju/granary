let page_size = 4096

type error =
  | Out_of_bounds of
      { page_id : int64
      ; n_pages : int64
      }

let pp_error fmt = function
  | Out_of_bounds { page_id; n_pages } ->
    Format.fprintf fmt "Out_of_bounds page_id=%Ld n_pages=%Ld" page_id n_pages
;;

type t =
  { mutable pages : Bytes.t array
  ; mutable n_pages : int64
  }

let pp fmt t = Format.fprintf fmt "Mem.t { n_pages = %Ld }" t.n_pages

let create ~n_pages =
  let n = Int64.to_int n_pages in
  let pages = Array.init n (fun _ -> Bytes.make page_size '\x00') in
  { pages; n_pages }
;;

let n_pages t = t.n_pages

let in_bounds t page_id =
  Int64.compare page_id 0L >= 0 && Int64.compare page_id t.n_pages < 0
;;

let read_page t ~page_id buf =
  if not (in_bounds t page_id)
  then Lwt.return (Error (Out_of_bounds { page_id; n_pages = t.n_pages }))
  else (
    let src = t.pages.(Int64.to_int page_id) in
    Cstruct.blit_from_bytes src 0 buf 0 page_size;
    Lwt.return (Ok ()))
;;

let write_page t ~page_id buf =
  if not (in_bounds t page_id)
  then Lwt.return (Error (Out_of_bounds { page_id; n_pages = t.n_pages }))
  else (
    let dst = t.pages.(Int64.to_int page_id) in
    Cstruct.blit_to_bytes buf 0 dst 0 page_size;
    Lwt.return (Ok ()))
;;

let sync _ = Lwt.return (Ok ())

let resize t ~n_pages =
  let n = Int64.to_int n_pages in
  let cur = Array.length t.pages in
  if n > cur
  then (
    let extra = Array.init (n - cur) (fun _ -> Bytes.make page_size '\x00') in
    t.pages <- Array.append t.pages extra)
  else if n < cur
  then t.pages <- Array.sub t.pages 0 n;
  t.n_pages <- n_pages;
  Lwt.return (Ok ())
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
