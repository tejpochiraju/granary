module Bigarray = Bigarray

let pack_bits bits n =
  let n_bytes = (n + 7) / 8 in
  let b = Bytes.create n_bytes in
  for i = 0 to n - 1 do
    if Bigarray.Array1.get bits i <> 0
    then (
      let byte_idx = i / 8 in
      let bit_idx = i mod 8 in
      Bytes.set_uint8 b byte_idx (Bytes.get_uint8 b byte_idx lor (1 lsl bit_idx)))
  done;
  b
;;

let pack_bits_into buf bits n =
  let n_bytes = (n + 7) / 8 in
  for j = 0 to n_bytes - 1 do
    let acc = ref 0 in
    let base = j * 8 in
    for k = 0 to 7 do
      let idx = base + k in
      if idx < n && Bigarray.Array1.get bits idx <> 0 then acc := !acc lor (1 lsl k)
    done;
    Buffer.add_char buf (Char.chr !acc)
  done
;;

let unpack_bits buf off n =
  let bits = Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout n in
  for i = 0 to n - 1 do
    let byte_idx = i / 8 in
    let bit_idx = i mod 8 in
    let is_set = (Bytes.get_uint8 buf (off + byte_idx) lsr bit_idx) land 1 in
    Bigarray.Array1.set bits i is_set
  done;
  bits
;;
