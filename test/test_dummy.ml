let () =
  Alcotest.run
    "granary"
    [ "scaffold", [ Alcotest.test_case "compiles" `Quick (fun () -> ()) ] ]
;;
