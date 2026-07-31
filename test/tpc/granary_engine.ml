open Granary

type t = { db : Db.t }

let name = "granary"
let run = Lwt_main.run

let unwrap = function
  | Ok v -> v
  | Error e -> failwith (Format.asprintf "granary: %a" Db.pp_error e)
;;

let open_db ~dir =
  let path = Filename.concat dir "tpch.db" in
  List.iter
    (fun p ->
       try Unix.unlink p with
       | _ -> ())
    [ path; path ^ "-wal" ];
  { db = unwrap (run (Granary_unix.open_file_wal ~clock:Unix.gettimeofday ~path ())) }
;;

let exec t sql = ignore (unwrap (run (Db.execute t.db sql)))

let render = function
  | Db.V_int i -> Int64.to_string i
  | Db.V_text s -> s
  | Db.V_null -> "NULL"
  | Db.V_real f -> Printf.sprintf "%.17g" f
  | Db.V_blob _ -> "<blob>"
;;

let query_rows t sql =
  run
    (let open Lwt.Syntax in
     let* stream = Lwt.map unwrap (Db.query t.db sql) in
     let* rows = Lwt_stream.to_list stream in
     Lwt.return (List.map (fun r -> Array.to_list (Array.map render r)) rows))
;;

let close t = run (Db.close t.db)
