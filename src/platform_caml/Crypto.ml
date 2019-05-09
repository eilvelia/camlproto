open! Base

module SHA1 = Nocrypto.Hash.SHA1
module SHA256 = Nocrypto.Hash.SHA256
module AES = struct
  type key = Nocrypto.Cipher_block.AES.ECB.key
  let ecb_create_key = Nocrypto.Cipher_block.AES.ECB.of_secret
  let ecb_encrypt = Nocrypto.Cipher_block.AES.ECB.encrypt
  let ecb_decrypt = Nocrypto.Cipher_block.AES.ECB.decrypt
end


(* let _ =
  (* Nocrypto.Hash.SHA256.digest_size |> Int.to_string |> Caml.print_endline *)
  let t = Nocrypto.Hash.SHA256.init () in
  (* let in_cs = Nocrypto.Numeric.Int64.to_cstruct_be 412L in *)
  let in_cs = Cstruct.of_string "abc" in
  Nocrypto.Hash.SHA256.feed t in_cs;
  let out_cs = Nocrypto.Hash.SHA256.get t in
  (* let out = Nocrypto.Numeric.Int64.of_cstruct_be out_cs in *)
  (* out |> Int64.to_string |> Caml.print_endline *)
  out_cs |> Nocrypto.Base64.encode |> Cstruct.to_string *)
