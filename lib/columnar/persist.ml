open Lwt.Syntax
module Row = Sqlocaml_encoding.Row
module S = Sqlocaml_store.Store

(** Well-known key under which columnar store data is persisted within the
    table's tree_id.  A single zero byte avoids collision with any real
    application key. *)
let data_key = Bytes.make 1 '\x00'

(** [save tx tree_id store] persists [store] to the B-tree at [tree_id].
    The entire store is serialised via {!Col_store.encode} and stored as a
    single value; the B-tree overflow chain handles values up to 1 GiB. *)
let save (tx : S.rw S.txn) (tree_id : S.tree_id) (store : Col_store.t) : unit Lwt.t =
  let encoded = Col_store.encode store in
  S.put tx tree_id data_key encoded
;;

(** [load tx tree_id schema] reads a previously persisted columnar store from
    the B-tree at [tree_id].  Returns [None] when no data has been persisted
    yet (fresh table).  Raises [Failure] on corrupt data. *)
let load (tx : S.ro S.txn) (tree_id : S.tree_id) (schema : Row.column list)
  : Col_store.t option Lwt.t
  =
  let* data = S.get tx tree_id data_key in
  match data with
  | None -> Lwt.return None
  | Some buf ->
    let store = Col_store.decode schema buf in
    Col_store.mark_clean store;
    Lwt.return (Some store)
;;
