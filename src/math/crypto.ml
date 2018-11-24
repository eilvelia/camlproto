module SHA1 = Platform.PlatformCrypto.SHA1
module SHA256 = Platform.PlatformCrypto.SHA256
module AES_ECB = Platform.PlatformCrypto.AES
module IGE = Aes_ige.MakeIGE (AES_ECB)
module Rsa = Rsa
module BasicRand = Basic_rand
module SecureRand = Secure_rand
let crc32 = Crc32.crc32
