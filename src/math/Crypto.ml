open! Base

module Make (Platform: PlatformTypes.S) = struct
  module SHA1 = Platform.Crypto.SHA1
  module SHA256 = Platform.Crypto.SHA256
  module ECB = Platform.Crypto.AES
  module IGE = AesIge.Make(ECB)
  module Rsa = Rsa.Make(Platform)
  module SecureRand = Platform.SecureRand
  let crc32 = Crc32.crc32
end
