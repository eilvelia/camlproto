(* TODO: Overwrite SecureRand with a deterministic one *)
module Math = Math.Make(PlatformCaml)

module IGE = Math.Crypto.IGE

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

let invalid_plaintext = Cstruct.of_hex "
  00000000 00000000 00000000 00000000
  00000000 0000
"

let ciphertext = Cstruct.of_hex "
  1A8519A6 557BE652 E9DA8E43 DA4EF445
  3CF456B4 CA488AA3 83C79C98 B34797CB
"

let%expect_test "encrypt should output the correct ciphertext" =
  Cstruct.hexdump @@ IGE.encrypt ~key ~iv plaintext;
  [%expect {|
    1a 85 19 a6 55 7b e6 52  e9 da 8e 43 da 4e f4 45
    3c f4 56 b4 ca 48 8a a3  83 c7 9c 98 b3 47 97 cb |}]

let%expect_test "encrypt should fail if size % 16 <> 0" =
  try Cstruct.hexdump @@ IGE.encrypt ~key ~iv invalid_plaintext with
  | _ -> print_endline "fail";
  [%expect{| fail |}]

let%expect_test "decrypt should output the correct plaintext" =
  Cstruct.hexdump @@ IGE.decrypt ~key ~iv ciphertext;
  [%expect {|
    00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
    00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00 |}]
