(** #174 — Schema fingerprint (FNV-1a 64-bit over a canonical, versioned
    serialization of a table's schema shape).

    The fingerprint must be:
    - deterministic and stable (same schema => same 64-bit value every call);
    - sensitive to every schema field that affects how a row decodes
      (column name, type, NOT NULL, PRIMARY KEY, DEFAULT, CHECK, GENERATED,
       column order, and the WITHOUT ROWID flag);
    - canonical / unambiguous (length-delimited, so reshuffling bytes across
      adjacent string fields cannot collide). *)

module SF = Granary_encoding.Schema_fingerprint
module Row = Granary_encoding.Row

let col
      ?(not_null = false)
      ?(primary_key = false)
      ?(pk_desc = false)
      ?(default = None)
      ?(check_sql = None)
      ?(generated_as = None)
      name
      ty
  : Row.column
  =
  { name; ty; not_null; primary_key; pk_desc; default; check_sql; generated_as }
;;

let fp ?(without_rowid = false) columns = SF.compute ~columns ~without_rowid

(* Two schemas whose fingerprints must differ. *)
let differ name a b =
  Alcotest.(check bool) (name ^ ": fingerprints differ") false (Int64.equal a b)
;;

(* Two schemas whose fingerprints must be equal. *)
let same name a b =
  Alcotest.(check bool) (name ^ ": fingerprints equal") true (Int64.equal a b)
;;

let test_deterministic () =
  let s = [ col "id" Row.Integer ~primary_key:true; col "name" Row.Text ] in
  same "repeat call" (fp s) (fp s)
;;

let test_order_matters () =
  let a = [ col "a" Row.Integer; col "b" Row.Text ] in
  let b = [ col "b" Row.Text; col "a" Row.Integer ] in
  differ "column order" (fp a) (fp b)
;;

let test_name_sensitive () =
  differ "column name" (fp [ col "a" Row.Integer ]) (fp [ col "b" Row.Integer ])
;;

let test_type_sensitive () =
  differ "column type" (fp [ col "a" Row.Integer ]) (fp [ col "a" Row.Text ])
;;

let test_not_null_sensitive () =
  differ
    "not_null flag"
    (fp [ col "a" Row.Integer ~not_null:false ])
    (fp [ col "a" Row.Integer ~not_null:true ])
;;

let test_primary_key_sensitive () =
  differ
    "primary_key flag"
    (fp [ col "a" Row.Integer ~primary_key:false ])
    (fp [ col "a" Row.Integer ~primary_key:true ])
;;

(* #312: pk_desc (INTEGER PRIMARY KEY DESC, a non-alias) changes the decode
   shape, so it must change the fingerprint. *)
let test_pk_desc_sensitive () =
  differ
    "pk_desc flag"
    (fp [ col "a" Row.Integer ~primary_key:true ~pk_desc:false ])
    (fp [ col "a" Row.Integer ~primary_key:true ~pk_desc:true ])
;;

let test_default_presence_sensitive () =
  differ
    "default presence"
    (fp [ col "a" Row.Integer ~default:None ])
    (fp [ col "a" Row.Integer ~default:(Some (Row.DV_int 1L)) ])
;;

let test_default_value_sensitive () =
  differ
    "default int value"
    (fp [ col "a" Row.Integer ~default:(Some (Row.DV_int 1L)) ])
    (fp [ col "a" Row.Integer ~default:(Some (Row.DV_int 2L)) ]);
  differ
    "default text value"
    (fp [ col "a" Row.Text ~default:(Some (Row.DV_text "x")) ])
    (fp [ col "a" Row.Text ~default:(Some (Row.DV_text "y")) ]);
  differ
    "default kind (int vs current_timestamp)"
    (fp [ col "a" Row.Integer ~default:(Some (Row.DV_int 0L)) ])
    (fp [ col "a" Row.Integer ~default:(Some Row.DV_current_timestamp) ])
;;

let test_check_sensitive () =
  differ
    "check_sql presence"
    (fp [ col "a" Row.Integer ~check_sql:None ])
    (fp [ col "a" Row.Integer ~check_sql:(Some "a > 0") ]);
  differ
    "check_sql text"
    (fp [ col "a" Row.Integer ~check_sql:(Some "a > 0") ])
    (fp [ col "a" Row.Integer ~check_sql:(Some "a < 0") ])
;;

let test_generated_sensitive () =
  differ
    "generated_as presence"
    (fp [ col "a" Row.Integer ~generated_as:None ])
    (fp [ col "a" Row.Integer ~generated_as:(Some ("b + 1", true)) ]);
  differ
    "generated stored vs virtual"
    (fp [ col "a" Row.Integer ~generated_as:(Some ("b + 1", true)) ])
    (fp [ col "a" Row.Integer ~generated_as:(Some ("b + 1", false)) ]);
  differ
    "generated expr text"
    (fp [ col "a" Row.Integer ~generated_as:(Some ("b + 1", true)) ])
    (fp [ col "a" Row.Integer ~generated_as:(Some ("b + 2", true)) ])
;;

let test_without_rowid_sensitive () =
  let s = [ col "id" Row.Integer ~primary_key:true ] in
  differ "without_rowid flag" (fp ~without_rowid:false s) (fp ~without_rowid:true s)
;;

(* The canonical serialization is length-delimited: splitting a string field
   differently across adjacent fields must not collide. Here the column NAME
   and a TEXT DEFAULT are adjacent string fields. "a" + "bc" vs "ab" + "c"
   would collide under naive concatenation. *)
let test_canonical_no_field_bleed () =
  let a = [ col "a" Row.Text ~default:(Some (Row.DV_text "bc")) ] in
  let b = [ col "ab" Row.Text ~default:(Some (Row.DV_text "c")) ] in
  differ "name/default boundary" (fp a) (fp b)
;;

let test_low32 () =
  Alcotest.(check int32) "low 32 bits" 0x55667788l (SF.low32 0x1122334455667788L);
  Alcotest.(check int32) "low 32 of small" 0x000000FFl (SF.low32 0xFFL)
;;

(* QCheck: compute is a pure function — same input yields the same value. *)
let gen_column : Row.column QCheck.Gen.t =
  QCheck.Gen.(
    map3
      (fun name ty (nn, pk, desc) ->
         Row.
           { name
           ; ty
           ; not_null = nn
           ; primary_key = pk
           ; pk_desc = pk && desc (* DESC only meaningful on a PRIMARY KEY column *)
           ; default = None
           ; check_sql = None
           ; generated_as = None
           })
      (string_size ~gen:(char_range 'a' 'z') (int_range 1 6))
      (oneof_list [ Row.Integer; Row.Text; Row.Real; Row.Blob ])
      (triple bool bool bool))
;;

let arb_schema = QCheck.make QCheck.Gen.(pair (list_size (int_range 0 8) gen_column) bool)

let prop_deterministic =
  QCheck.Test.make
    ~count:500
    ~name:"schema fingerprint is deterministic"
    arb_schema
    (fun (cols, wr) ->
       Int64.equal
         (SF.compute ~columns:cols ~without_rowid:wr)
         (SF.compute ~columns:cols ~without_rowid:wr))
;;

let () =
  Alcotest.run
    "schema_fingerprint"
    [ ( "sensitivity"
      , [ Alcotest.test_case "deterministic" `Quick test_deterministic
        ; Alcotest.test_case "order matters" `Quick test_order_matters
        ; Alcotest.test_case "name" `Quick test_name_sensitive
        ; Alcotest.test_case "type" `Quick test_type_sensitive
        ; Alcotest.test_case "not_null" `Quick test_not_null_sensitive
        ; Alcotest.test_case "primary_key" `Quick test_primary_key_sensitive
        ; Alcotest.test_case "pk_desc" `Quick test_pk_desc_sensitive
        ; Alcotest.test_case "default presence" `Quick test_default_presence_sensitive
        ; Alcotest.test_case "default value" `Quick test_default_value_sensitive
        ; Alcotest.test_case "check" `Quick test_check_sensitive
        ; Alcotest.test_case "generated" `Quick test_generated_sensitive
        ; Alcotest.test_case "without_rowid" `Quick test_without_rowid_sensitive
        ; Alcotest.test_case
            "canonical no field bleed"
            `Quick
            test_canonical_no_field_bleed
        ; Alcotest.test_case "low32" `Quick test_low32
        ] )
    ; "properties", [ QCheck_alcotest.to_alcotest prop_deterministic ]
    ]
;;
