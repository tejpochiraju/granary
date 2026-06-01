module GCM = Mirage_crypto.AES.GCM

let nonce_len = 16
let tag_len = 16
let overhead = nonce_len + tag_len

type t = { key : GCM.key }

let pp fmt _t = Format.pp_print_string fmt "<Crypto.t: key=<redacted>>"

let create ~key =
  if String.length key <> 32
  then Error `Bad_key_length
  else Ok { key = GCM.of_secret key }
;;

let adata_of_page_id page_id =
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 page_id;
  Bytes.unsafe_to_string b
;;

let encrypt_page t ~page_id buf =
  let n = Cstruct.length buf in
  let region = n - overhead in
  let pt = Cstruct.to_string buf ~off:0 ~len:region in
  let nonce = Mirage_crypto_rng.generate nonce_len in
  let adata = adata_of_page_id page_id in
  let ct, tag = GCM.authenticate_encrypt_tag ~key:t.key ~nonce ~adata pt in
  Cstruct.blit_from_string ct 0 buf 0 region;
  Cstruct.blit_from_string nonce 0 buf region nonce_len;
  Cstruct.blit_from_string tag 0 buf (region + nonce_len) tag_len
;;

let decrypt_page t ~page_id buf =
  let n = Cstruct.length buf in
  let region = n - overhead in
  let ct = Cstruct.to_string buf ~off:0 ~len:region in
  let nonce = Cstruct.to_string buf ~off:region ~len:nonce_len in
  let tag = Cstruct.to_string buf ~off:(region + nonce_len) ~len:tag_len in
  let adata = adata_of_page_id page_id in
  match GCM.authenticate_decrypt_tag ~key:t.key ~nonce ~adata ~tag ct with
  | None -> Error `Tag_mismatch
  | Some pt ->
    Cstruct.blit_from_string pt 0 buf 0 region;
    for i = region to n - 1 do
      Cstruct.set_uint8 buf i 0
    done;
    Ok ()
;;

let encrypt_frame t ~page_id ~plaintext =
  let len = Cstruct.length plaintext in
  let pt = Cstruct.to_string plaintext in
  let nonce = Mirage_crypto_rng.generate nonce_len in
  let adata = adata_of_page_id page_id in
  let ct, tag = GCM.authenticate_encrypt_tag ~key:t.key ~nonce ~adata pt in
  let out = Cstruct.create (len + overhead) in
  Cstruct.blit_from_string ct 0 out 0 len;
  Cstruct.blit_from_string nonce 0 out len nonce_len;
  Cstruct.blit_from_string tag 0 out (len + nonce_len) tag_len;
  out
;;

let decrypt_frame t ~page_id payload =
  let total = Cstruct.length payload in
  let len = total - overhead in
  let ct = Cstruct.to_string payload ~off:0 ~len in
  let nonce = Cstruct.to_string payload ~off:len ~len:nonce_len in
  let tag = Cstruct.to_string payload ~off:(len + nonce_len) ~len:tag_len in
  let adata = adata_of_page_id page_id in
  match GCM.authenticate_decrypt_tag ~key:t.key ~nonce ~adata ~tag ct with
  | None -> Error `Tag_mismatch
  | Some pt -> Ok (Cstruct.of_string pt)
;;

let canary_adata = "sqlocaml-enc-v1"

let make_canary t ~nonce =
  let _, tag = GCM.authenticate_encrypt_tag ~key:t.key ~nonce ~adata:canary_adata "" in
  tag
;;

let check_canary t ~nonce ~tag = String.equal tag (make_canary t ~nonce)

[@@@ai_disclosure "ai-generated"]
[@@@ai_model "claude-opus-4-8"]
[@@@ai_provider "Anthropic"]
