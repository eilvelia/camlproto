open! Base
open MTProto
open TLRuntime.Types

module Mtp = TLSchema.MTProto
module Tel = TLSchema.Telegram

let src = Logs.Src.create "camlproto.telegram.client"
module Log = (val Logs.src_log src : Logs.LOG)

let layer_const = 143

(* TODO: *)
(* module type TelegramSession = sig
end *)

module Make (Platform : PlatformTypes.S) (T : TransportTypes.S) = struct
  module MTP = MakeMTProtoV2Client(Platform)(T)

  type t = { mtp : MTP.t }

  let create ?(auth_key : Cstruct.t option) () =
    let%lwt mtp = MTP.create ?auth_key () in

    let%lwt () = match auth_key with
      | Some _ -> Lwt.return_unit
      | None ->
        let%lwt _new_auth_key = MTP.do_authentication mtp in
        (* Log.debug (fun m -> m "new auth_key:@.%a" hexdump_pp new_auth_key); *)
        Lwt.return_unit
    in

    MTP.loop mtp;

    Lwt.return { mtp }

  let invoke t = MTP.invoke t.mtp

  let init_with
    (t : t)
    (s : Settings.t)
    (type x result)
    (module X : TLFunc with type t = x and type ResultM.t = result)
    (x : x)
    : result Lwt.t
  =
    MTP.invoke t.mtp (module Tel.TL_invokeWithLayer(Tel.TL_initConnection(X))) {
      layer = layer_const;
      query = {
        api_id = s.api_id;
        device_model = s.device_model;
        system_version = s.system_version;
        app_version = s.app_version;
        system_lang_code = s.system_lang_code;
        lang_pack = s.lang_pack;
        lang_code = s.lang_code;
        proxy = s.proxy;
        params = s.params;
        query = x;
      }
    }

  let init t (s : Settings.t) =
    let%lwt (TL_config res) = init_with t s (module Tel.TL_help_getConfig) E in
    Log.info (fun m -> m "help.getConfig res. me_url_prefix: %s" res.me_url_prefix);
    Lwt.return_unit
end
