module C = Sqlocaml_storage.Crypto

let key32 = String.make 32 'k'

let cipher () =
  match C.create ~key:key32 with
  | Ok t -> t
  | Error _ -> assert false
;;

let seed_rng () = Mirage_crypto_rng_unix.use_default ()

let mk_page () =
  let p = Cstruct.create 4096 in
  for i = 0 to 4096 - C.overhead - 1 do
    Cstruct.set_uint8 p i (i land 0xff)
  done;
  p
;;

let test_create_rejects_short_key () =
  match C.create ~key:"short" with
  | Error `Bad_key_length -> ()
  | Ok _ -> Alcotest.fail "expected Bad_key_length"
;;

let test_roundtrip () =
  let t = cipher () in
  let p = mk_page () in
  let orig = Cstruct.to_string p ~off:0 ~len:(4096 - C.overhead) in
  C.encrypt_page t ~page_id:7L p;
  Alcotest.(check bool)
    "ciphertext differs from plaintext"
    false
    (String.equal orig (Cstruct.to_string p ~off:0 ~len:(4096 - C.overhead)));
  (match C.decrypt_page t ~page_id:7L p with
   | Ok () -> ()
   | Error `Tag_mismatch -> Alcotest.fail "roundtrip decrypt failed");
  Alcotest.(check string)
    "plaintext restored"
    orig
    (Cstruct.to_string p ~off:0 ~len:(4096 - C.overhead))
;;

let test_tail_zeroed_after_decrypt () =
  let t = cipher () in
  let p = mk_page () in
  C.encrypt_page t ~page_id:1L p;
  (match C.decrypt_page t ~page_id:1L p with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "decrypt");
  for i = 4096 - C.overhead to 4095 do
    Alcotest.(check int) "tail zeroed" 0 (Cstruct.get_uint8 p i)
  done
;;

let test_wrong_key_fails () =
  let t = cipher () in
  let p = mk_page () in
  C.encrypt_page t ~page_id:3L p;
  let t2 =
    match C.create ~key:(String.make 32 'x') with
    | Ok t -> t
    | Error _ -> assert false
  in
  match C.decrypt_page t2 ~page_id:3L p with
  | Error `Tag_mismatch -> ()
  | Ok () -> Alcotest.fail "wrong key must fail"
;;

let test_wrong_page_id_fails () =
  let t = cipher () in
  let p = mk_page () in
  C.encrypt_page t ~page_id:3L p;
  match C.decrypt_page t ~page_id:4L p with
  | Error `Tag_mismatch -> ()
  | Ok () -> Alcotest.fail "AAD mismatch must fail"
;;

let test_tamper_fails () =
  let t = cipher () in
  let p = mk_page () in
  C.encrypt_page t ~page_id:3L p;
  Cstruct.set_uint8 p 0 (Cstruct.get_uint8 p 0 lxor 0xff);
  match C.decrypt_page t ~page_id:3L p with
  | Error `Tag_mismatch -> ()
  | Ok () -> Alcotest.fail "tampered ciphertext must fail"
;;

let test_nonce_freshness () =
  let t = cipher () in
  let p1 = mk_page ()
  and p2 = mk_page () in
  C.encrypt_page t ~page_id:5L p1;
  C.encrypt_page t ~page_id:5L p2;
  Alcotest.(check bool)
    "same plaintext + page_id → different ciphertext (fresh nonce)"
    false
    (Cstruct.equal p1 p2)
;;

let test_frame_roundtrip () =
  let t = cipher () in
  let pt = Cstruct.create 4096 in
  for i = 0 to 4095 do
    Cstruct.set_uint8 pt i (i * 7 land 0xff)
  done;
  let enc = C.encrypt_frame t ~page_id:9L ~plaintext:pt in
  Alcotest.(check int)
    "frame payload grows by overhead"
    (4096 + C.overhead)
    (Cstruct.length enc);
  match C.decrypt_frame t ~page_id:9L enc with
  | Ok dec -> Alcotest.(check bool) "frame roundtrip" true (Cstruct.equal pt dec)
  | Error `Tag_mismatch -> Alcotest.fail "frame decrypt failed"
;;

let test_canary () =
  let t = cipher () in
  let nonce = Mirage_crypto_rng.generate C.nonce_len in
  let tag = C.make_canary t ~nonce in
  Alcotest.(check bool) "right key passes canary" true (C.check_canary t ~nonce ~tag);
  let t2 =
    match C.create ~key:(String.make 32 'z') with
    | Ok t -> t
    | Error _ -> assert false
  in
  Alcotest.(check bool) "wrong key fails canary" false (C.check_canary t2 ~nonce ~tag)
;;

(* NIST SP 800-38D AES-256-GCM known-answer vector (test case 14):
   key = 32 zero bytes, IV = 12 zero bytes, P = 16 zero bytes, A = empty.
   Expected C = cea7403d4d606b6e074ec5d3baf39d18,
            T = d0d1c8a799996bf0265b98b5d48ab919. *)
let test_nist_kat () =
  let h s =
    String.init
      (String.length s / 2)
      (fun i -> Char.chr (int_of_string ("0x" ^ String.sub s (i * 2) 2)))
  in
  let key = h (String.make 64 '0') in
  let k = Mirage_crypto.AES.GCM.of_secret key in
  let nonce = h (String.make 24 '0') in
  let msg = h (String.make 32 '0') in
  let c, tag = Mirage_crypto.AES.GCM.authenticate_encrypt_tag ~key:k ~nonce msg in
  let to_hex s =
    String.concat
      ""
      (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))
  in
  Alcotest.(check string) "NIST ciphertext" "cea7403d4d606b6e074ec5d3baf39d18" (to_hex c);
  Alcotest.(check string) "NIST tag" "d0d1c8a799996bf0265b98b5d48ab919" (to_hex tag)
;;

let prop_roundtrip =
  QCheck.Test.make
    ~count:200
    ~name:"encrypt/decrypt roundtrip"
    QCheck.(string_size (Gen.return (4096 - C.overhead)))
    (fun s ->
       let t = cipher () in
       let p = Cstruct.create 4096 in
       Cstruct.blit_from_string s 0 p 0 (String.length s);
       C.encrypt_page t ~page_id:42L p;
       match C.decrypt_page t ~page_id:42L p with
       | Error _ -> false
       | Ok () -> String.equal s (Cstruct.to_string p ~off:0 ~len:(4096 - C.overhead)))
;;

let () =
  seed_rng ();
  Alcotest.run
    "crypto"
    [ ( "unit"
      , [ Alcotest.test_case
            "create rejects short key"
            `Quick
            test_create_rejects_short_key
        ; Alcotest.test_case "page roundtrip" `Quick test_roundtrip
        ; Alcotest.test_case "tail zeroed" `Quick test_tail_zeroed_after_decrypt
        ; Alcotest.test_case "wrong key" `Quick test_wrong_key_fails
        ; Alcotest.test_case "wrong page_id" `Quick test_wrong_page_id_fails
        ; Alcotest.test_case "tamper" `Quick test_tamper_fails
        ; Alcotest.test_case "nonce freshness" `Quick test_nonce_freshness
        ; Alcotest.test_case "frame roundtrip" `Quick test_frame_roundtrip
        ; Alcotest.test_case "canary" `Quick test_canary
        ; Alcotest.test_case "NIST KAT" `Quick test_nist_kat
        ] )
    ; "property", List.map QCheck_alcotest.to_alcotest [ prop_roundtrip ]
    ]
;;
