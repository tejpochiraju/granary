type value =
  | IK_null
  | IK_int of int64
  | IK_real of float
  | IK_text of string
  | IK_blob of bytes

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

(** Write an int64 in big-endian into [buf] starting at [off]. *)
let write_be64 buf off n =
  for i = 0 to 7 do
    let byte = Int64.to_int (Int64.shift_right_logical n ((7 - i) * 8)) land 0xFF in
    Bytes.set_uint8 buf (off + i) byte
  done
;;

(** Read a big-endian int64 from [buf] starting at [off]. *)
let read_be64 buf off =
  let n = ref 0L in
  for i = 0 to 7 do
    let byte = Bytes.get_uint8 buf (off + i) in
    n := Int64.logor !n (Int64.shift_left (Int64.of_int byte) ((7 - i) * 8))
  done;
  !n
;;

(** Encode a string/bytes with null-byte escaping and two-byte [0x00 0x00] terminator.

    Encoding rules:
      - Any [0x00] byte in the source becomes [0x00 0xFF] in the output.
      - The payload is terminated by [0x00 0x00].

    Because no escaped content contains [0x00 0x00] (an input [0x00] is always
    followed by [0xFF] in the output), the terminator is unambiguous.

    This scheme is order-preserving under unsigned-byte comparison:
    For a shorter string A that is a prefix of longer string B at position k,
    A's next byte is the first terminator byte [0x00], while B's next byte is
    either a non-zero content byte (> 0x00) or another [0x00 0xFF] escape
    (whose second byte [0xFF] sorts after the terminator's second byte [0x00]).
    Hence A < B in byte order, matching String.compare / Bytes.compare. *)
let encode_escaped_bytes src =
  let len = Bytes.length src in
  (* Pessimistic upper bound: every byte doubles + 2-byte terminator *)
  let buf = Buffer.create ((len * 2) + 2) in
  for i = 0 to len - 1 do
    let b = Bytes.get_uint8 src i in
    if b = 0x00
    then (
      Buffer.add_char buf '\x00';
      Buffer.add_char buf '\xff')
    else Buffer.add_char buf (Char.chr b)
  done;
  (* Two-byte terminator *)
  Buffer.add_char buf '\x00';
  Buffer.add_char buf '\x00';
  Bytes.of_string (Buffer.contents buf)
;;

(** Decode an escaped sequence starting at [off] in [buf].
    Returns [(unescaped_bytes, next_offset)] or [Error msg]. *)
let decode_escaped_bytes buf off =
  let len = Bytes.length buf in
  let out = Buffer.create 16 in
  let i = ref off in
  let found_terminator = ref false in
  let error = ref None in
  while !i < len && (not !found_terminator) && !error = None do
    let b = Bytes.get_uint8 buf !i in
    if b = 0x00
    then
      (* Either 0x00 0xFF (escaped null) or 0x00 0x00 (terminator) *)
      if !i + 1 >= len
      then error := Some "unexpected end of input after 0x00"
      else (
        let b2 = Bytes.get_uint8 buf (!i + 1) in
        if b2 = 0x00
        then (
          (* terminator: 0x00 0x00 *)
          found_terminator := true;
          i := !i + 2)
        else if b2 = 0xFF
        then (
          (* escaped null byte *)
          Buffer.add_char out '\x00';
          i := !i + 2)
        else error := Some (Printf.sprintf "invalid escape byte: 0x00 0x%02x" b2))
    else (
      Buffer.add_char out (Char.chr b);
      incr i)
  done;
  match !error with
  | Some msg -> Error msg
  | None ->
    if not !found_terminator
    then Error "missing terminator in escaped sequence"
    else Ok (Bytes.of_string (Buffer.contents out), !i)
;;

(* ------------------------------------------------------------------ *)
(* encode_value                                                         *)
(* ------------------------------------------------------------------ *)

let encode_value v =
  match v with
  | IK_null -> Bytes.make 1 '\x00'
  | IK_int n ->
    (* tag 0x01 + 8 bytes big-endian with sign bit flipped *)
    let buf = Bytes.create 9 in
    Bytes.set_uint8 buf 0 0x01;
    let flipped = Int64.logxor n 0x8000_0000_0000_0000L in
    write_be64 buf 1 flipped;
    buf
  | IK_real f ->
    (* NaN → treat as NULL *)
    if Float.is_nan f
    then Bytes.make 1 '\x00'
    else (
      let buf = Bytes.create 9 in
      Bytes.set_uint8 buf 0 0x02;
      let bits = Int64.bits_of_float f in
      (* Order-preserving encoding for IEEE 754 doubles:
         - Negative float (sign bit = 1): flip ALL bits.
           Result has MSB=0, range [0x0010..., 0x7FFF...].
         - Positive float (sign bit = 0): flip ONLY the sign bit.
           Result has MSB=1, range [0x8000..., 0xFFEF...].
         All encoded negatives are less than all encoded positives.
         Within each group, the original IEEE ordering is preserved.
         -0.0 (0x8000_0000_0000_0000) → lognot → 0x7FFF_FFFF_FFFF_FFFF
         +0.0 (0x0000_0000_0000_0000) → flip sign → 0x8000_0000_0000_0000
         So -0.0 < +0.0 ✓ *)
      let stored =
        if Int64.shift_right_logical bits 63 = 1L
        then
          (* negative float: flip all bits *)
          Int64.lognot bits
        else
          (* positive float: flip only the sign bit *)
          Int64.logxor bits Int64.min_int
      in
      write_be64 buf 1 stored;
      buf)
  | IK_text s ->
    let src = Bytes.of_string s in
    let escaped = encode_escaped_bytes src in
    let elen = Bytes.length escaped in
    let buf = Bytes.create (1 + elen) in
    Bytes.set_uint8 buf 0 0x03;
    Bytes.blit escaped 0 buf 1 elen;
    buf
  | IK_blob b ->
    let escaped = encode_escaped_bytes b in
    let elen = Bytes.length escaped in
    let buf = Bytes.create (1 + elen) in
    Bytes.set_uint8 buf 0 0x04;
    Bytes.blit escaped 0 buf 1 elen;
    buf
;;

(* ------------------------------------------------------------------ *)
(* encode (multi-column + rowid)                                        *)
(* ------------------------------------------------------------------ *)

let encode cols ~rowid =
  (* Encode each column value *)
  let encoded_cols = List.map encode_value cols in
  (* Encode rowid: 8 bytes big-endian with sign bit flip (same as Rowid.encode) *)
  let rowid_buf = Bytes.create 8 in
  let flipped = Int64.logxor rowid 0x8000_0000_0000_0000L in
  write_be64 rowid_buf 0 flipped;
  (* Concatenate everything *)
  let total = List.fold_left (fun acc b -> acc + Bytes.length b) 0 encoded_cols + 8 in
  let buf = Bytes.create total in
  let off = ref 0 in
  List.iter
    (fun b ->
       let len = Bytes.length b in
       Bytes.blit b 0 buf !off len;
       off := !off + len)
    encoded_cols;
  Bytes.blit rowid_buf 0 buf !off 8;
  buf
;;

(* ------------------------------------------------------------------ *)
(* decode                                                               *)
(* ------------------------------------------------------------------ *)

(* Decode a single tagged column value at [!off], appending to [cols] and
   advancing [off]; records a message in [err] on malformed input. *)
let decode_index_col buf ~off ~cols ~err =
  let len = Bytes.length buf in
  let tag = Bytes.get_uint8 buf !off in
  incr off;
  match tag with
  | 0x00 -> cols := IK_null :: !cols
  | 0x01 ->
    (* 8 bytes big-endian, sign bit flipped *)
    if !off + 8 > len
    then err := Some "buffer too short for INTEGER"
    else (
      let stored = read_be64 buf !off in
      let n = Int64.logxor stored 0x8000_0000_0000_0000L in
      cols := IK_int n :: !cols;
      off := !off + 8)
  | 0x02 ->
    (* 8 bytes; MSB=0 means was negative (flip all bits back),
       MSB=1 means was positive (flip only sign bit back) *)
    if !off + 8 > len
    then err := Some "buffer too short for REAL"
    else (
      let stored = read_be64 buf !off in
      off := !off + 8;
      let bits =
        if Int64.shift_right_logical stored 63 = 0L
        then
          (* was negative float: flip all bits back *)
          Int64.lognot stored
        else
          (* was positive float: flip only sign bit back *)
          Int64.logxor stored Int64.min_int
      in
      cols := IK_real (Int64.float_of_bits bits) :: !cols)
  | 0x03 ->
    (* TEXT: escaped bytes + 0x00 0x00 terminator *)
    (match decode_escaped_bytes buf !off with
     | Error msg -> err := Some msg
     | Ok (raw, next_off) ->
       cols := IK_text (Bytes.to_string raw) :: !cols;
       off := next_off)
  | 0x04 ->
    (* BLOB: escaped bytes + 0x00 0x00 terminator *)
    (match decode_escaped_bytes buf !off with
     | Error msg -> err := Some msg
     | Ok (raw, next_off) ->
       cols := IK_blob raw :: !cols;
       off := next_off)
  | t -> err := Some (Printf.sprintf "unknown tag byte: 0x%02x" t)
;;

let decode buf =
  let len = Bytes.length buf in
  (* We need at least 8 bytes for the rowid *)
  if len < 8
  then Error "buffer too short: need at least 8 bytes for rowid"
  else (
    let off = ref 0 in
    let cols = ref [] in
    let err = ref None in
    (* Parse column values until 8 bytes remain for the rowid *)
    while !err = None && !off < len - 8 do
      if !off >= len
      then err := Some "unexpected end of buffer"
      else decode_index_col buf ~off ~cols ~err
    done;
    match !err with
    | Some msg -> Error msg
    | None ->
      (* The remaining bytes should be exactly 8 bytes for the rowid *)
      if len - !off <> 8
      then Error (Printf.sprintf "expected 8 rowid bytes, got %d" (len - !off))
      else (
        let stored = read_be64 buf !off in
        let rowid = Int64.logxor stored 0x8000_0000_0000_0000L in
        Ok (List.rev !cols, rowid)))
;;

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
