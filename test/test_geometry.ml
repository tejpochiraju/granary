(** #95 — page geometry: a per-file, creation-time choice of [page_size] and
    [reserved_bytes_per_page], persisted in the header and immutable thereafter.

    [Geometry.t] is the pure value that carries this choice through the storage
    stack.  These tests pin down its derived sizes and its creation-time
    validation (page_size a 4096-multiple in [4096,65536]; reserved_bytes ≥ 0
    and small enough to leave a usable payload). *)

module G = Granary_storage.Geometry

let mk ?(reserved = 0) ps = G.create ~page_size:ps ~reserved_bytes_per_page:reserved

let ok name = function
  | Ok g -> g
  | Error e -> Alcotest.failf "%s: expected Ok, got Error %a" name G.pp_error e
;;

let is_error name = function
  | Error _ -> ()
  | Ok _ -> Alcotest.failf "%s: expected Error, got Ok" name
;;

(* The default geometry matches the historical compile-time constants:
   4096-byte page, no reserved bytes, 16-byte header => 4080 usable. *)
let test_default () =
  Alcotest.(check int) "default page_size" 4096 G.default.page_size;
  Alcotest.(check int) "default reserved" 0 G.default.reserved_bytes_per_page;
  Alcotest.(check int) "default max_data_bytes" 4080 (G.max_data_bytes G.default);
  Alcotest.(check int)
    "default max_overflow_payload_bytes"
    4078
    (G.max_overflow_payload_bytes G.default);
  Alcotest.(check int)
    "default max_freelist_entries_per_page"
    340
    (G.max_freelist_entries_per_page G.default)
;;

let test_derived_large_pages () =
  let g8 = ok "8K" (mk 8192) in
  Alcotest.(check int) "8K max_data_bytes" (8192 - 16) (G.max_data_bytes g8);
  let g16 = ok "16K" (mk 16384) in
  Alcotest.(check int) "16K max_data_bytes" (16384 - 16) (G.max_data_bytes g16);
  Alcotest.(check int)
    "16K overflow payload"
    (16384 - 16 - 2)
    (G.max_overflow_payload_bytes g16)
;;

let test_reserved_subtracts () =
  let g = ok "reserved 32" (mk 4096 ~reserved:32) in
  Alcotest.(check int) "reserved shrinks usable" (4096 - 16 - 32) (G.max_data_bytes g);
  Alcotest.(check int)
    "reserved shrinks overflow payload"
    (4096 - 16 - 32 - 2)
    (G.max_overflow_payload_bytes g)
;;

let test_valid_sizes_accepted () =
  ignore (ok "4096" (mk 4096));
  ignore (ok "8192" (mk 8192));
  ignore (ok "16384" (mk 16384));
  ignore (ok "65536" (mk 65536));
  (* Non-power-of-two 4096 multiple is allowed — addressing uses
     multiplication, not bit shifts (see #95). *)
  ignore (ok "12288" (mk 12288))
;;

let test_bad_page_size_rejected () =
  is_error "not a 4096 multiple" (mk 4097);
  is_error "not a 4096 multiple (5000)" (mk 5000);
  is_error "below floor" (mk 2048);
  is_error "above ceiling" (mk 131072);
  is_error "zero" (mk 0)
;;

let test_bad_reserved_rejected () =
  is_error "negative reserved" (mk 4096 ~reserved:(-1));
  (* Reserved so large it leaves no sane payload. *)
  is_error "reserved eats the page" (mk 4096 ~reserved:4096);
  is_error "reserved leaves < floor" (mk 4096 ~reserved:4000)
;;

(* QCheck: every accepted geometry has a positive, well-defined usable area. *)
let arb_ps = QCheck.make QCheck.Gen.(map (fun k -> 4096 * k) (int_range 1 16))

let prop_max_data_bytes_consistent =
  QCheck.Test.make
    ~count:200
    ~name:"max_data_bytes = page_size - 16 - reserved for accepted geometries"
    arb_ps
    (fun ps ->
       match G.create ~page_size:ps ~reserved_bytes_per_page:0 with
       | Error _ -> ps > 65536 (* only the > ceiling case may legitimately fail here *)
       | Ok g -> G.max_data_bytes g = ps - 16 && G.max_data_bytes g > 0)
;;

let () =
  Alcotest.run
    "geometry"
    [ ( "derived"
      , [ Alcotest.test_case "default" `Quick test_default
        ; Alcotest.test_case "large pages" `Quick test_derived_large_pages
        ; Alcotest.test_case "reserved subtracts" `Quick test_reserved_subtracts
        ] )
    ; ( "validation"
      , [ Alcotest.test_case "valid sizes accepted" `Quick test_valid_sizes_accepted
        ; Alcotest.test_case "bad page_size rejected" `Quick test_bad_page_size_rejected
        ; Alcotest.test_case "bad reserved rejected" `Quick test_bad_reserved_rejected
        ] )
    ; "properties", [ QCheck_alcotest.to_alcotest prop_max_data_bytes_consistent ]
    ]
;;
