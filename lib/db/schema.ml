(* #433: read-only projection over a live [Catalog.t].  See schema.mli. *)

module Cat = Granary_catalog.Catalog

type t = Cat.t

type table =
  { name : string
  ; columns : Granary_encoding.Row.column list
  ; fk_constraints : Cat.fk_constraint list
  ; without_rowid : bool
  ; columnar : bool
  }

let pp fmt (t : t) = Format.fprintf fmt "@[<hv>Schema.t over %a@]" Cat.pp t

let pp_table fmt (tbl : table) =
  Format.fprintf
    fmt
    "@[<hv>Schema.table { name = %S;@ columns = %d;@ fks = %d;@ without_rowid = %b;@ \
     columnar = %b }@]"
    tbl.name
    (List.length tbl.columns)
    (List.length tbl.fk_constraints)
    tbl.without_rowid
    tbl.columnar
;;

let of_catalog (c : Cat.t) : t = c

(* The projection drops [Catalog.storage] entirely: its [Columnar] arm carries
   a [Col_store.t], which is mutable, so handing [table_meta] straight out
   would leave exactly the kind of out-of-band mutation path #433 removes. *)
let project (m : Cat.table_meta) : table =
  let without_rowid, columnar =
    match m.Cat.storage with
    | Cat.Row { without_rowid; _ } -> without_rowid, false
    | Cat.Columnar _ -> false, true
  in
  { name = m.Cat.name
  ; columns = m.Cat.columns
  ; fk_constraints = m.Cat.fk_constraints
  ; without_rowid
  ; columnar
  }
;;

let list_tables (t : t) = Lwt.map (List.map project) (Cat.list_tables t)
let find_table (t : t) ~name = Lwt.map (Option.map project) (Cat.find_table t ~name)
let table_exists (t : t) ~name = Cat.table_exists t ~name
let indexes_for_table (t : t) ~table = Cat.indexes_for_table t ~table
let find_index (t : t) ~name = Cat.find_index t ~name
let index_exists (t : t) ~name = Cat.index_exists t ~name
