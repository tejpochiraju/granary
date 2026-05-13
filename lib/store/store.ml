(* Phase 0 in-memory store. See store.mli for the full contract.
   Phase 1 replaces this module with CoW B+-tree on BLOCK. *)

module BytesMap = Map.Make (Bytes)

type ro
type rw

type tree_id = int

type t = {
  _trees : (tree_id, Bytes.t BytesMap.t ref) Hashtbl.t;
}

type 'a txn =
  | Ro : t -> ro txn
  | Rw : t -> rw txn

type cursor = unit

type seek_result =
  | Found of bytes
  | Not_found of [`Greater of bytes | `End]

let create () = { _trees = Hashtbl.create 16 }
let close _ = Lwt.return_unit

let ro_begin t = Lwt.return (Ro t)
let rw_begin t = Lwt.return (Rw t)
let commit _ = Lwt.return_unit
let rollback _ = Lwt.return_unit
let ro_end _ = Lwt.return_unit

let get _ _ _ = Lwt.return_none
let put _ _ _ _ = Lwt.return_unit
let del _ _ _ = Lwt.return_unit

let cursor_open _ _ = Lwt.return ()
let cursor_close _ = ()
let cursor_seek _ _ = Not_found `End
let cursor_first _ = Not_found `End
let cursor_next _ = None
let cursor_value _ = None
