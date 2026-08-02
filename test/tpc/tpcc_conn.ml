open Granary

type t =
  { db : Db.t
  ; path : string
  }

let name = "granary"
let pp fmt t = Format.fprintf fmt "tpcc_conn(%s)" t.path

let unwrap = function
  | Ok v -> v
  | Error e -> failwith (Format.asprintf "granary: %a" Db.pp_error e)
;;

(* ── Lwt-native operations ────────────────────────────────────────────── *)

let exec_lwt t sql = Lwt.map (fun r -> ignore (unwrap r)) (Db.execute t.db sql)

let render = function
  | Db.V_int i -> Int64.to_string i
  | Db.V_text s -> s
  | Db.V_null -> "NULL"
  | Db.V_real f -> Printf.sprintf "%.17g" f
  | Db.V_blob _ -> "<blob>"
;;

let query_rows_lwt t sql =
  let open Lwt.Syntax in
  let* stream = Lwt.map unwrap (Db.query t.db sql) in
  let+ rows = Lwt_stream.to_list stream in
  List.map (fun r -> Array.to_list (Array.map render r)) rows
;;

(* The profiles hold parameters apart from the SQL; granary is driven by
   literal SQL here, exactly as [Granary_engine] is, so [render] substitutes
   them — see {!Tpcc_txn.render} for why a placeholder/parameter mismatch
   raises instead of substituting what it can. *)
let ops t =
  { Tpcc_txn.query = (fun s -> query_rows_lwt t (Tpcc_txn.render s))
  ; Tpcc_txn.exec = (fun s -> exec_lwt t (Tpcc_txn.render s))
  }
;;

(* ── synchronous ENGINE view, for the load phase only ─────────────────── *)

let run = Lwt_main.run

let open_db ~dir =
  let path = Filename.concat dir "tpcc.db" in
  List.iter
    (fun p ->
       try Unix.unlink p with
       | _ -> ())
    [ path; path ^ "-wal" ];
  { db = unwrap (run (Granary_unix.open_file_wal ~clock:Unix.gettimeofday ~path ()))
  ; path
  }
;;

let exec t sql = run (exec_lwt t sql)
let query_rows t sql = run (query_rows_lwt t sql)
let close t = run (Db.close t.db)
