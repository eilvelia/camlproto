open! Base
open Types

module TLM = TLGen.MTProto
module Crypto = Math.Crypto
module Bigint = Math.Bigint
module RsaManager = Crypto.Rsa.RsaManager

let random_padding (cs: Cstruct.t) (start: int): unit =
  let len = Cstruct.len cs in
  let size = len - start in
  let random_bytes = Crypto.SecureRand.rand_cs size in
  Cstruct.blit random_bytes 0 cs start size

let generate_tmp_aes server_nonce new_nonce =
  let hash1 = Crypto.SHA1.digest (Cstruct.append new_nonce server_nonce) in
  let hash2 = Crypto.SHA1.digest (Cstruct.append server_nonce new_nonce) in
  let hash3 = Crypto.SHA1.digest (Cstruct.append new_nonce new_nonce) in

  let tmp_aes_key = Cstruct.create_unsafe 32 in
  Cstruct.blit hash1 0 tmp_aes_key 0 20;
  Cstruct.blit hash2 0 tmp_aes_key 20 12;

  let tmp_aes_iv = Cstruct.create_unsafe 32 in
  Cstruct.blit hash2 12 tmp_aes_iv 0 8;
  Cstruct.blit hash3 0 tmp_aes_iv 8 20;
  Cstruct.blit new_nonce 0 tmp_aes_iv 28 4;

  (tmp_aes_key, tmp_aes_iv)

(* type auth_error = [
  | `InvalidNonce
  | `InvalidServerNonce
  | `DhGenRetry
  | `DhGenFail
] *)

exception AuthenticationError of string

let good_p = Cstruct.of_hex "
  C71CAEB9C6B1C9048E6C522F70F13F73980D40238E3E21C14934D037563D930F48198A0AA7C140
  58229493D22530F4DBFA336F6E0AC925139543AED44CCE7C3720FD51F69458705AC68CD4FE6B6B
  13ABDC9746512969328454F18FAF8C595F642477FE96BB2A941D5BCD1D4AC8CC49880708FA9B37
  8E3C4F3A9060BEE67CF9A4A4A695811051907E162753B56B0F6B410DBA74D8A84B2A14B3144E0E
  F1284754FD17ED950D5965B4B9DD46582DB1178D169C6BC465B0D6FF9CA3928FEF5B9AE4E418FC
  15E83EBEA0F87FA9FF5EED70050DED2849F47BF959D956850CE929851F0D8115F635B105EE2E4E
  15D04B2454BF6F4FADF034B10403119CD8E3B92FCC5B
"

let good_g = 3

let authenticate
  (type sender_t)
  (module Sender : MTProtoPlainObjSender with type t = sender_t)
  (t: sender_t)
  ?(reject_unknown_dh_params = false)
  (rsa: RsaManager.t)
=
  let open Sender in

  let nonce = Crypto.SecureRand.rand_cs 16 in

  (* Logger.dump "nonce" nonce; *)

  let%lwt (C_resPQ ({ server_nonce; _ } as res_pq)) =
    invoke_unencrypted_obj t (module TLM.C_req_pq_multi) { nonce } in

  if Cstruct.equal res_pq.nonce nonce |> not then
    raise @@ AuthenticationError "1: Invalid nonce from server";

  (* Logger.dump "server_nonce" res_pq.server_nonce; *)

  let pq = Cstruct.BE.get_uint64 res_pq.pq 0 in
  let (p, q) = Math.Factorization.pq_prime pq in

  Caml.print_endline @@ Printf.sprintf "pq %Ld %LX" pq pq;

  let p_bytes = Cstruct.create_unsafe 4 in
  Cstruct.BE.set_uint32 p_bytes 0 (Int32.of_int64_exn p);
  let q_bytes = Cstruct.create_unsafe 4 in
  Cstruct.BE.set_uint32 q_bytes 0 (Int32.of_int64_exn q);

  (* int256 (32 bytes) *)
  let new_nonce = Crypto.SecureRand.rand_cs 32 in

  let p_q_inner_data = TL.Encoder.encode TLM.C_p_q_inner_data.encode_boxed {
    pq = res_pq.pq;
    p = p_bytes;
    q = q_bytes;
    nonce;
    server_nonce;
    new_nonce;
  } |> TL.Encoder.to_cstruct in

  let p_q_inner_data_len = Cstruct.len p_q_inner_data in

  let data_with_hash = Cstruct.create_unsafe 255 in
  Cstruct.blit (Crypto.SHA1.digest p_q_inner_data) 0 data_with_hash 0 20;
  Cstruct.blit p_q_inner_data 0 data_with_hash 20 p_q_inner_data_len;
  random_padding data_with_hash (20 + p_q_inner_data_len);

  let (C_vector fingerprints) = res_pq.server_public_key_fingerprints in
  let (rsa_key, finger) = RsaManager.find_by_fingerprints rsa fingerprints in

  let encrypted_data = RsaManager.encrypt ~key:rsa_key data_with_hash in

  let%lwt dh_params = invoke_unencrypted_obj t (module TLM.C_req_DH_params) {
    nonce;
    server_nonce;
    p = p_bytes;
    q = q_bytes;
    public_key_fingerprint = finger;
    encrypted_data;
  } in

  match dh_params with
  | C_server_DH_params_ok params ->
    begin
      Caml.print_endline "server_DH_params_ok";

      if Cstruct.equal params.nonce nonce |> not then
        raise @@ AuthenticationError "2: Invalid nonce from server";

      if Cstruct.equal params.server_nonce server_nonce |> not then
        raise @@ AuthenticationError "2: Invalid server_nonce from server";

      let (tmp_key, tmp_iv) = generate_tmp_aes server_nonce new_nonce in

      let decrypted_answer_with_hash =
        Crypto.IGE.decrypt params.encrypted_answer tmp_key tmp_iv in
      let given_hash = Cstruct.sub decrypted_answer_with_hash 0 20 in
      let decrypted_answer = Cstruct.shift decrypted_answer_with_hash 20 in
      let (C_server_DH_inner_data server_dh_inner) =
        TLM.Server_DH_inner_data.decode (TL.Decoder.of_cstruct decrypted_answer) in

      (* Check hash *)
      let calc_hash =
        TL.Encoder.encode TLM.C_server_DH_inner_data.encode_boxed server_dh_inner
          |> TL.Encoder.to_cstruct |> Crypto.SHA1.digest in
      if Cstruct.equal calc_hash given_hash |> not then
        raise @@ AuthenticationError "3: Invalid hash";

      if Cstruct.equal server_dh_inner.nonce nonce |> not then
        raise @@ AuthenticationError "3: Invalid nonce from server";

      if Cstruct.equal server_dh_inner.server_nonce server_nonce |> not then
        raise @@ AuthenticationError "3: Invalid server_nonce from server";

      (* Logger.dump "dh_prime" server_dh_inner.dh_prime; *)
      (* Caml.print_endline @@ Printf.sprintf
        "server_time %d\n" server_dh_inner.server_time; *)

      let current_time = Platform.get_current_time () |> Float.to_int in
      let time_offset = server_dh_inner.server_time - current_time in

      if server_dh_inner.g <> good_g then begin
        Caml.print_endline @@ Printf.sprintf
          "Warning: Unknown DH g %d" server_dh_inner.g;
        if reject_unknown_dh_params then
          raise @@ AuthenticationError "3: Unknown DH g"
      end;

      if Cstruct.equal server_dh_inner.dh_prime good_p |> not then begin
        Logger.dump "Warning: Unknown DH p" server_dh_inner.dh_prime;
        if reject_unknown_dh_params then
          raise @@ AuthenticationError "3: Unknown DH p"
      end;

      let dh_prime = Bigint.of_cstruct_be server_dh_inner.dh_prime in
      let b = Bigint.of_cstruct_be @@ Crypto.SecureRand.rand_cs 256 in
      let g = Bigint.of_int server_dh_inner.g in
      let g_a = Bigint.of_cstruct_be server_dh_inner.g_a in

      (* pow(g, b) mod dh_prime *)
      let g_b = Bigint.powm g b dh_prime in
      let g_b_cs = Bigint.to_cstruct_be g_b in

      let g_a_b = Bigint.powm g_a b dh_prime in
      let auth_key = Bigint.to_cstruct_be g_a_b in

      Caml.print_endline @@ Printf.sprintf
        "Auth key created. Auth_key len: %d. Time_offset: %d."
        (Cstruct.len auth_key) time_offset;

      (* TODO: Checks from https://core.telegram.org/mtproto/security_guidelines *)

      if not Bigint.(g > one && g_a > one && g_b > one) then
        raise @@ AuthenticationError "3: Should be greater than 1";

      let d = Bigint.(dh_prime - one) in
      if not Bigint.(g < d && g_a < d && g_b < d) then
        raise @@ AuthenticationError "3: Should be less than dh_prime - 1";

      (* TODO: check that g_a and g_b are between 2^{2048-64} and dh_prime - 2^{2048-64} *)

      let client_dh_inner_data =
        TL.Encoder.encode TLM.C_client_DH_inner_data.encode_boxed {
          nonce;
          server_nonce;
          retry_id = 0L; (* TODO: *)
          g_b = g_b_cs;
        } |> TL.Encoder.to_cstruct in

      let len = Cstruct.len client_dh_inner_data in
      let len_with_hash = len + 20 in

      let data_with_hash = Cstruct.create (len_with_hash + (16 - len_with_hash % 16)) in
      Cstruct.blit (Crypto.SHA1.digest client_dh_inner_data) 0 data_with_hash 0 20;
      Cstruct.blit client_dh_inner_data 0 data_with_hash 20 len;
      random_padding data_with_hash (20 + len);
      let encrypted_data = Crypto.IGE.encrypt data_with_hash tmp_key tmp_iv in

      (* Logger.dump "client_dh_inner_data" client_dh_inner_data;
      Logger.dump "dh data_with_hash" data_with_hash; *)

      let%lwt dh_answer = invoke_unencrypted_obj t (module TLM.C_set_client_DH_params)
        { nonce; server_nonce; encrypted_data; } in

      match dh_answer with
      | C_dh_gen_ok dh_gen -> begin
        Caml.print_endline "dh_gen_ok";

        if Cstruct.equal dh_gen.nonce nonce |> not then
          raise @@ AuthenticationError "end: Invalid nonce from server";

        if Cstruct.equal dh_gen.server_nonce server_nonce |> not then
          raise @@ AuthenticationError "end: Invalid server_nonce from server";

        let auth_key_aux_hash = Crypto.SHA1.digest auth_key in
        let auth_key_aux_hash = Cstruct.sub auth_key_aux_hash 0 8 in

        let new_nonce_hash = Cstruct.create_unsafe 41 in
        Cstruct.blit new_nonce 0 new_nonce_hash 0 32;
        Cstruct.set_char new_nonce_hash 32 '\001';
        Cstruct.blit auth_key_aux_hash 0 new_nonce_hash 33 8;
        let new_nonce_hash = Crypto.SHA1.digest new_nonce_hash in
        let new_nonce_hash = Cstruct.sub new_nonce_hash 4 16 in

        if Cstruct.equal new_nonce_hash dh_gen.new_nonce_hash1 |> not then begin
          Logger.dump "client new_nonce_hash" new_nonce_hash;
          Logger.dump "server new_nonce_hash" dh_gen.new_nonce_hash1;
          raise @@ AuthenticationError "end: Invalid new_nonce_hash"
        end;

        let server_salt = Int64.(
          (Cstruct.LE.get_uint64 new_nonce 0)
          lxor (Cstruct.LE.get_uint64 server_nonce 0)
        ) in

        Lwt.return (auth_key, server_salt, time_offset)
      end
      | C_dh_gen_retry _ -> raise @@ AuthenticationError "dh_gen_retry" (* TODO: *)
      | C_dh_gen_fail _ -> raise @@ AuthenticationError "dh_gen_fail"
    end
  | C_server_DH_params_fail _ ->
    raise @@ AuthenticationError "server_DH_params_fail"
