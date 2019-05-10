open OUnit
open Common.Math.Crypto

let encrypt = IGE.encrypt
let decrypt = IGE.decrypt

let key = Cstruct.of_hex "
00010203 04050607 08090A0B 0C0D0E0F
"

let iv = Cstruct.of_hex "
00010203 04050607 08090A0B 0C0D0E0F
10111213 14151617 18191A1B 1C1D1E1F
"

let plaintext = Cstruct.of_hex "
00000000 00000000 00000000 00000000
00000000 00000000 00000000 00000000
"

let plaintext_no_padding = Cstruct.of_hex "
00000000 00000000 00000000 00000000
00000000 0000
"

let ciphertext = Cstruct.of_hex "
1A8519A6 557BE652 E9DA8E43 DA4EF445
3CF456B4 CA488AA3 83C79C98 B34797CB
"

let suite =
  "aes_ige" >::: [
    "encrypt" >:: (fun _ ->
      let encrypted = encrypt plaintext key iv in
      assert_equal (Cstruct.to_string encrypted) (Cstruct.to_string ciphertext)
    );
    "encrypt_with_padding" >:: (fun _ ->
      let encrypted = encrypt plaintext_no_padding key iv in
      assert_equal (Cstruct.to_string encrypted) (Cstruct.to_string ciphertext)
    );
    "decrypt" >:: (fun _ ->
      let decrypted = decrypt ciphertext key iv in
      assert_equal (Cstruct.to_string decrypted) (Cstruct.to_string plaintext)
    );
  ]
