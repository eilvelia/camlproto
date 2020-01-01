open! Base

module Make (Platform: PlatformTypes.S) = struct
  module Crypto = Crypto.Make(Platform)
  module Factorization = Factorization.Make(Platform)
  module Bigint = Bigint.Make(Platform)
  module Gzip = Platform.Gzip
end

let crossplatform_crc32 = Crc32.crc32
