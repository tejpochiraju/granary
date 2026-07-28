(** Offline repair/recovery CLI: salvage a damaged database into a clean file.

    Usage: granary_recover --in <damaged.db> --out <clean.db>

    The tool reads every page from the source, verifies CRCs, opens the source
    database to extract recoverable rows, and writes them into a fresh output
    file with correct CRCs throughout.  The source is never modified. *)

open Lwt.Syntax

let usage_msg = "granary_recover --in <damaged.db> --out <clean.db>"
let input_path = ref None
let output_path = ref None

let () =
  Arg.parse
    [ "--in", Arg.String (fun s -> input_path := Some s), "Damaged database file"
    ; "--out", Arg.String (fun s -> output_path := Some s), "Output clean file"
    ]
    (fun _ -> ())
    usage_msg
;;

let main () : unit Lwt.t =
  let in_path =
    match !input_path with
    | Some p -> p
    | None ->
      Format.eprintf "Missing --in <path>\n%!";
      exit 1
  in
  let out_path =
    match !output_path with
    | Some p -> p
    | None ->
      Format.eprintf "Missing --out <path>\n%!";
      exit 1
  in
  (* 1. Open the damaged file for raw-page scanning (phase 1 of recovery). *)
  let* damaged_file_result = Granary_unix.Unix_file.open_ ~path:in_path () in
  let damaged_file =
    match damaged_file_result with
    | Ok f -> f
    | Error e ->
      Format.eprintf "Cannot open '%s': %a\n%!" in_path Granary_unix.Unix_file.pp_error e;
      exit 1
  in
  let n_pages = Granary_unix.Unix_file.n_pages damaged_file in
  (* read_page callback: reads a single raw page from the damaged file.
     Returns [None] when the page cannot be read (the recovery library
     treats these as silently skipped pages). *)
  let read_page (page_id : int64) : Cstruct.t option Lwt.t =
    let page_size = Granary_unix.Unix_file.page_size damaged_file in
    let buf = Cstruct.create page_size in
    let* result = Granary_unix.Unix_file.read_page damaged_file ~page_id buf in
    match result with
    | Ok () -> Lwt.return (Some buf)
    | Error _ -> Lwt.return None
  in
  (* open_source callback: opens the damaged file via the Store layer
     (which handles crash recovery / WAL replay) and loads the catalog.
     The store handle is kept alive for the extraction phase and closed
     during cleanup. *)
  let source_store_ref : Granary_store.Store.t option ref = ref None in
  let open_source () =
    let* store_result = Granary_unix.Store.open_file ~path:in_path () in
    match store_result with
    | Error e ->
      let msg = Format.asprintf "%a" Granary_store.Store.pp_error e in
      Lwt.return (Error msg)
    | Ok store ->
      source_store_ref := Some store;
      let* catalog = Granary_catalog.Catalog.open_ store in
      Lwt.return (Ok (store, catalog))
  in
  (* write_to callback: creates the output file on the first batch and
     writes recovered rows.  Each batch is written inside its own RW
     transaction.  The output store is kept open across batches and
     closed during cleanup. *)
  let output_store_ref : Granary_store.Store.t option ref = ref None in
  let write_to rows =
    let* store =
      match !output_store_ref with
      | Some s -> Lwt.return (Ok s)
      | None ->
        let* result = Granary_unix.Store.open_file ~path:out_path () in
        (match result with
         | Error e ->
           let msg = Format.asprintf "%a" Granary_store.Store.pp_error e in
           Lwt.return (Error msg)
         | Ok s ->
           output_store_ref := Some s;
           Lwt.return (Ok s))
    in
    match store with
    | Error msg -> Lwt.return (Error msg)
    | Ok store ->
      let* txn = Granary_store.Store.rw_begin store in
      let* () =
        Lwt_list.iter_s
          (fun (row : Granary_recovery.Recovery.recovered_row) ->
             Granary_store.Store.put txn row.tree_id row.key row.value)
          rows
      in
      let* () = Granary_store.Store.commit txn in
      Lwt.return (Ok ())
  in
  (* Run the full recovery pipeline. *)
  let* result =
    Granary_recovery.Recovery.recover ~read_page ~n_pages ~open_source ~write_to
  in
  (* Cleanup: close all open handles regardless of success or failure. *)
  let* _ = Granary_unix.Unix_file.close damaged_file in
  let* () =
    match !source_store_ref with
    | Some s -> Granary_store.Store.close s
    | None -> Lwt.return_unit
  in
  let* () =
    match !output_store_ref with
    | Some s -> Granary_store.Store.close s
    | None -> Lwt.return_unit
  in
  match result with
  | Error e ->
    Format.eprintf "Recovery failed: %s\n%!" e;
    Lwt.return_unit
  | Ok r ->
    Format.printf "%a\n%!" Granary_recovery.Recovery.pp_result r;
    Lwt.return_unit
;;

let () = Lwt_main.run (main ())
