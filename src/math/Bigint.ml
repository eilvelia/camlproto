open! Base

module Make (Platform: PlatformTypes.S) = struct
  include Platform.Bigint

  (* let (>) = gt
  let (<) = lt *)
end
