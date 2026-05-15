(** Execute write operations against the store and catalog. *)
val execute :
  Sqlocaml_store.Store.t ->
  Sqlocaml_catalog.Catalog.t ->
  Plan.op ->
  unit Lwt.t

(** Execute read operations; returns a lazy stream of result rows. *)
val query :
  Sqlocaml_store.Store.t ->
  Sqlocaml_catalog.Catalog.t ->
  Plan.op ->
  Sqlocaml_encoding.Row.t Lwt_stream.t Lwt.t
