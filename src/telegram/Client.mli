open! Base
open TLRuntime.Types

module Make (Platform: PlatformTypes.S) (T: TransportTypes.S): sig
  type t
  val create: ?auth_key:Cstruct.t -> unit -> t Lwt.t
  val init: Settings.t -> t -> unit Lwt.t
  val invoke
    : t
    -> ?content_related:bool
    -> (module TLFunc with type t = 'a and type ResultM.t = 'b)
    -> 'a
    -> 'b Lwt.t
end
