let () =
  Alcotest.run
    "sqlocaml"
    [ "scaffold", [ Alcotest.test_case "compiles" `Quick (fun () -> ()) ] ]
;;
