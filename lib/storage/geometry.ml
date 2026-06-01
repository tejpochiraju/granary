(* Page geometry: the per-file, creation-time choice of page size and
   reserved-bytes-per-page.  See geometry.mli for the contract. *)

type t =
  { page_size : int
  ; reserved_bytes_per_page : int
  }

let header_size = 16
let freelist_entry_size = 12
let min_page_size = 4096
let max_page_size = 65536

(* Smallest usable data area we permit after carving off the common header and
   the reserved bytes.  Keeps the b-tree functional (a leaf must hold a few
   small cells, an overflow page a useful chunk) even at the most aggressive
   reserved-bytes setting; mirrors SQLite's floor on usable page size. *)
let min_usable_data_bytes = 480

let pp fmt t =
  Format.fprintf
    fmt
    "@[<hv>{ page_size = %d;@ reserved_bytes_per_page = %d }@]"
    t.page_size
    t.reserved_bytes_per_page
;;

let default = { page_size = 4096; reserved_bytes_per_page = 0 }
let max_data_bytes t = t.page_size - header_size - t.reserved_bytes_per_page
let max_overflow_payload_bytes t = max_data_bytes t - 2
let max_freelist_entries_per_page t = max_data_bytes t / freelist_entry_size

type error =
  | Bad_page_size of int
  | Bad_reserved of int

let pp_error fmt = function
  | Bad_page_size n ->
    Format.fprintf
      fmt
      "Bad_page_size %d (must be a multiple of %d in [%d, %d])"
      n
      min_page_size
      min_page_size
      max_page_size
  | Bad_reserved n ->
    Format.fprintf
      fmt
      "Bad_reserved %d (must be >= 0 and leave >= %d usable bytes)"
      n
      min_usable_data_bytes
;;

let create ~page_size ~reserved_bytes_per_page =
  if
    page_size < min_page_size
    || page_size > max_page_size
    || page_size mod min_page_size <> 0
  then Error (Bad_page_size page_size)
  else if
    reserved_bytes_per_page < 0
    || page_size - header_size - reserved_bytes_per_page < min_usable_data_bytes
  then Error (Bad_reserved reserved_bytes_per_page)
  else Ok { page_size; reserved_bytes_per_page }
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
