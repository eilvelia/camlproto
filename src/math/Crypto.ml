module SHA1 = Platform.PlatformCrypto.SHA1
module SHA256 = Platform.PlatformCrypto.SHA256
module ECB = Platform.PlatformCrypto.AES
module IGE = AesIge.MakeIGE (ECB)
module Rsa = Rsa
module BasicRand = BasicRand
module SecureRand = SecureRand
let crc32 = Crc32.crc32
