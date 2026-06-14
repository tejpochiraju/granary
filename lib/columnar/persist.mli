(** Persistence for columnar stores via the Store B-tree.

    Each columnar table has an allocated {!Sqlocaml_store.Store.tree_id} (stored
    in the catalog's {!Sqlocaml_catalog.Catalog.storage} variant).  The entire
    in-memory {!Col_store.t} is serialised via {!Col_store.encode} and stored
    as a single value under a well-known key within that tree.  The B-tree
    overflow chain transparently handles values up to 1 GiB. *)

module Row = Sqlocaml_encoding.Row
module S = Sqlocaml_store.Store

(** [save tx tree_id store] serialises [store] and writes it to the B-tree
    at [tree_id] under a reserved key.  Must be called within a write
    transaction. *)
val save : S.rw S.txn -> S.tree_id -> Col_store.t -> unit Lwt.t

(** [load tx tree_id schema] reads a previously persisted columnar store from
    the B-tree at [tree_id].  Returns [None] if no data has been persisted yet
    (fresh table).  Raises [Failure] on corrupt data. *)
val load : S.ro S.txn -> S.tree_id -> Row.column list -> Col_store.t option Lwt.t
