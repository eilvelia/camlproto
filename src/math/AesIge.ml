open! Base

external (.%{}) : Cstruct.buffer -> int -> int = "%caml_ba_unsafe_ref_1"
external (.%{}<-) : Cstruct.buffer -> int -> int -> unit = "%caml_ba_unsafe_set_1"

(* Note: This does no bounds checking *)
let (.%{}<-) cs i v = cs.Cstruct.buffer.%{i + cs.off} <- v
let (.%{}) cs i = cs.Cstruct.buffer.%{i + cs.off}

module Make (ECB : PlatformTypes.AES) = struct
  open ECB

  let encrypt ~key ~iv plaintext =
    assert (Cstruct.length iv = 32);
    assert (Cstruct.length plaintext land 15 = 0);

    let plain_len = Cstruct.length plaintext in

    let iv1, iv2 = Cstruct.split iv 16 in
    let iv1 = ref iv1 and iv2 = ref iv2 in

    let aes_ecb_key = ecb_create_key key in

    let blocks_count = plain_len / 16 in

    let ciphertext = Cstruct.create_unsafe plain_len in
    let plain_block = Cstruct.create_unsafe 16 in

    for block_index = 0 to blocks_count - 1 do
      let index = block_index * 16 in

      for i = 0 to 15 do
        plain_block.%{i} <- plaintext.%{index + i} lxor !iv1.%{i}
      done;

      (* It would be better to copy it into the existing bigarray instead of
         allocating a new one, but Nocrypto.ECB does not support that *)
      let cipher_block = ecb_encrypt ~key:aes_ecb_key plain_block in

      for i = 0 to 15 do
        cipher_block.%{i} <- cipher_block.%{i} lxor !iv2.%{i}
      done;

      iv1 := cipher_block;
      iv2 := Cstruct.sub plaintext index 16;

      Cstruct.blit cipher_block 0 ciphertext index 16
    done;

    ciphertext

  let decrypt ~key ~iv ciphertext =
    assert (Cstruct.length iv = 32);

    let cipher_len = Cstruct.length ciphertext in

    let iv1, iv2 = Cstruct.split iv 16 in
    let iv1 = ref iv1 and iv2 = ref iv2 in

    let aes_ecb_key = ecb_create_key key in

    let blocks_count = cipher_len / 16 in

    let plaintext = Cstruct.create_unsafe cipher_len in
    let cipher_block = Cstruct.create_unsafe 16 in

    for block_index = 0 to blocks_count - 1 do
      let index = block_index * 16 in

      for i = 0 to 15 do
        cipher_block.%{i} <- ciphertext.%{index + i} lxor !iv2.%{i}
      done;

      (* Again, this allocates *)
      let plain_block = ecb_decrypt ~key:aes_ecb_key cipher_block in

      for i = 0 to 15 do
        plain_block.%{i} <- plain_block.%{i} lxor !iv1.%{i}
      done;

      iv1 := Cstruct.sub ciphertext index 16;
      iv2 := plain_block;

      Cstruct.blit plain_block 0 plaintext index 16
    done;

    plaintext
end
