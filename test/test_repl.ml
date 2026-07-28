(** Tests for the granary TUI REPL binary.

    The REPL is a nottui-lwt TUI application that requires a real terminal
    for interactive rendering.  Full behavioural tests (piping SQL + reading
    stdout) are not meaningful for a TUI: the binary reads keyboard events
    via notty, not plain stdin, and renders rows via lwd widgets rather than
    printing them to stdout.

    These tests cover the binary contract that is testable without a PTY:
      - binary is present at the expected path
      - it exits non-zero with an error message when given a non-existent DB path
        (this exercises main()'s open_db error branch before the TUI starts) *)

let repl_path () =
  (* test_repl.exe lives in _build/default/test/  The REPL binary is at
     _build/default/bin/repl/granary_repl.exe.  Resolve relative to our
     own exec path so we don't depend on cwd. *)
  let our_path = Sys.executable_name in
  let test_dir = Filename.dirname our_path in
  let build_root = Filename.dirname test_dir in
  Filename.concat
    (Filename.concat (Filename.concat build_root "bin") "repl")
    "granary_repl.exe"
;;

let test_binary_exists () =
  let bin = repl_path () in
  Alcotest.(check bool) "binary exists at expected path" true (Sys.file_exists bin)
;;

let test_bad_path_nonzero_exit () =
  let bin = repl_path () in
  let code =
    Sys.command
      (Printf.sprintf
         "%s /nonexistent_granary_test_db </dev/null 2>/dev/null"
         (Filename.quote bin))
  in
  Alcotest.(check bool) "non-existent db path exits nonzero" true (code <> 0)
;;

let () =
  Alcotest.run
    "repl"
    [ ( "binary"
      , [ Alcotest.test_case "binary exists" `Quick test_binary_exists
        ; Alcotest.test_case "bad path exits nonzero" `Quick test_bad_path_nonzero_exit
        ] )
    ]
;;
