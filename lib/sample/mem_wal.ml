type t = { mutable buf : Bytes.t }

let create ?(initial_size = 65536) () = { buf = Bytes.make initial_size '\x00' }
let size_bytes t = Int64.of_int (Bytes.length t.buf)
let pp fmt t = Format.fprintf fmt "Mem_wal { size = %Ld }" (size_bytes t)

let grow t need =
  let cur = Bytes.length t.buf in
  if need > cur
  then (
    let nb = Bytes.make (max need (cur * 2)) '\x00' in
    Bytes.blit t.buf 0 nb 0 cur;
    t.buf <- nb)
;;

let read_at t ~offset out =
  let off = Int64.to_int offset in
  let len = Cstruct.length out in
  grow t (off + len);
  Cstruct.blit_from_bytes t.buf off out 0 len;
  Lwt.return (Ok ())
;;

let write_at t ~offset src =
  let off = Int64.to_int offset in
  let len = Cstruct.length src in
  grow t (off + len);
  Cstruct.blit_to_bytes src 0 t.buf off len;
  Lwt.return (Ok ())
;;

let sync () = Lwt.return (Ok ())

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-8"]
[@@@ai_provider "Anthropic"]
