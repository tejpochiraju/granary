let encode_uint64 buf n =
  let n = ref n in
  while Int64.compare (Int64.shift_right_logical !n 7) 0L <> 0 do
    let b = Int64.to_int (Int64.logor (Int64.logand !n 0x7FL) 0x80L) in
    Buffer.add_char buf (Char.chr (b land 0xFF));
    n := Int64.shift_right_logical !n 7
  done;
  Buffer.add_char buf (Char.chr (Int64.to_int !n land 0x7F))

let decode_uint64 buf off =
  let result = ref 0L in
  let shift = ref 0 in
  let off = ref off in
  let cont = ref true in
  while !cont do
    let b = Char.code (Bytes.get buf !off) in
    incr off;
    let payload = Int64.of_int (b land 0x7F) in
    result := Int64.logor !result (Int64.shift_left payload !shift);
    shift := !shift + 7;
    if b land 0x80 = 0 then cont := false
  done;
  !result, !off

let zigzag_encode n =
  Int64.logxor (Int64.shift_left n 1) (Int64.shift_right n 63)

let zigzag_decode n =
  Int64.logxor (Int64.shift_right_logical n 1) (Int64.neg (Int64.logand n 1L))

let encode_int64 buf n = encode_uint64 buf (zigzag_encode n)

let decode_int64 buf off =
  let u, off = decode_uint64 buf off in
  zigzag_decode u, off
