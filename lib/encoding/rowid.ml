let encode n =
  let n = Int64.logxor n 0x8000_0000_0000_0000L in
  let b = Bytes.create 8 in
  for i = 0 to 7 do
    let byte = Int64.to_int (Int64.shift_right_logical n ((7 - i) * 8)) land 0xFF in
    Bytes.set_uint8 b i byte
  done;
  b

let decode b =
  let n = ref 0L in
  for i = 0 to 7 do
    let byte = Bytes.get_uint8 b i in
    n := Int64.logor !n (Int64.shift_left (Int64.of_int byte) ((7 - i) * 8))
  done;
  Int64.logxor !n 0x8000_0000_0000_0000L

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-7"]
[@@@ai_provider "Anthropic"]
