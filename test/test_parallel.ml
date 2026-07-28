open Granary_parallel

let test_run_returns_branch_value () =
  let r = Parallel.run ~parallel:(fun () -> 42) ~sequential:(fun () -> 42) in
  Alcotest.(check int) "run returns the branch value" 42 r
;;

let test_run_dispatches_per_availability () =
  let p = ref 0
  and s = ref 0 in
  Parallel.run ~parallel:(fun () -> incr p) ~sequential:(fun () -> incr s);
  Alcotest.(check int) "exactly one branch ran" 1 (!p + !s);
  if Parallel.available ()
  then Alcotest.(check int) "parallel branch ran" 1 !p
  else Alcotest.(check int) "sequential branch ran" 1 !s
;;

let () =
  Alcotest.run
    "parallel"
    [ ( "gate"
      , [ Alcotest.test_case
            "run returns branch value"
            `Quick
            test_run_returns_branch_value
        ; Alcotest.test_case
            "run dispatches per availability"
            `Quick
            test_run_dispatches_per_availability
        ] )
    ]
;;
