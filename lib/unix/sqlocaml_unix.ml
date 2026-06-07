(* Unix platform driver for sqlocaml (#170): file-backed database constructors
   plus the file provider that powers ATTACH and VACUUM on the otherwise
   platform-agnostic core.  The core libraries (block/store/db) carry no [unix]
   dependency; this driver supplies the Unix-specific pieces. *)

open Lwt.Syntax
module Unix_file = Unix_file
module Fault_inject = Fault_inject
module Store = Store

(* The process-wide provider the core uses for ATTACH and VACUUM.  VACUUM passes
   the source database's geometry (#176) so the rebuilt file keeps its page_size
   and reserved bytes; opening an existing file (ATTACH, vacuum reopen) ignores
   it and peeks the header instead. *)
let provider : Sqlocaml.Db.file_provider =
  { Sqlocaml.Db.open_store =
      (fun ?geom ~path () ->
        match geom with
        | None -> Store.open_file ~path ()
        | Some g ->
          Store.open_file
            ~page_size:g.Sqlocaml_storage.Geometry.page_size
            ~reserved_bytes_per_page:g.Sqlocaml_storage.Geometry.reserved_bytes_per_page
            ~explicit_geometry:true
            ~path
            ())
  ; remove_file =
      (fun p ->
        try Unix.unlink p with
        | Unix.Unix_error _ -> ())
  ; rename_file = Unix.rename
  }
;;

let install () = Sqlocaml.Db.set_file_provider provider

let to_db ?clock ?durability ~path = function
  | Error e ->
    Lwt.return
      (Error (Sqlocaml.Db.Runtime (Format.asprintf "%a" Sqlocaml_store.Store.pp_error e)))
  | Ok store ->
    let* db = Sqlocaml.Db.of_store ?clock ?durability ~file_path:path store in
    Lwt.return (Ok db)
;;

let open_file ?page_size ?reserved_bytes_per_page ?clock ~path () =
  install ();
  let explicit_geometry = page_size <> None || reserved_bytes_per_page <> None in
  let* r =
    Store.open_file ?page_size ?reserved_bytes_per_page ~explicit_geometry ~path ()
  in
  to_db ?clock ~path r
;;

let open_file_wal ?page_size ?reserved_bytes_per_page ?clock ?durability ~path () =
  install ();
  let explicit_geometry = page_size <> None || reserved_bytes_per_page <> None in
  let* r =
    Store.open_file_wal ?page_size ?reserved_bytes_per_page ~explicit_geometry ~path ()
  in
  to_db ?clock ?durability ~path r
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
