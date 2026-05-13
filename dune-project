(lang dune 3.16)
(name sqlocaml)
(generate_opam_files true)
(version 0.0.1)

(package
 (name sqlocaml)
 (synopsis "Pure-OCaml SQL engine")
 (description "Pure-OCaml SQL engine targeting MirageOS unikernels.")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.16))
  (lwt (>= 5.7))
  (cstruct (>= 6.2))
  (menhir (>= 20240715))
  (alcotest :with-test)
  (qcheck-alcotest :with-test)))
