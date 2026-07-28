(* MirageOS unikernel description for the sample granary demo (#403).

   `mirage configure -t <target>` reads this file and generates the build glue.
   The unikernel itself lives in unikernel.ml ([Unikernel.Make]); it takes a
   single block device. See mirage/README.md for build/run commands. *)

open Mirage

let main =
  main
    "Unikernel.Make"
    (block @-> job)
    ~packages:
      [ package
          ~libs:[ "granary"; "granary.sample"; "granary.store"; "granary.mirage_block" ]
          "granary"
      ; package "logs"
      ; package "cstruct"
      ; package "lwt"
      ]
;;

(* A file/Solo5 block device named "disk"; for `-t unix` it maps to a file, for
   `-t hvt` it is supplied at run time via `--block:disk=<img>`. *)
let disk = block_of_file "disk"
let () = register "granary-demo" [ main $ disk ]
