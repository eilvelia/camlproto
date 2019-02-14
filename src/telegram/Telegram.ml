open! Base
open MTProto
open MTProtoTransport
open TL.Types

module TLM = TLGen.MTProto
module TLT = TLGen.Telegram

let src = Logs.Src.create "camlproto.telegram"
module Log = (val Logs.src_log src : Logs.LOG)

module Settings = struct
  type t = {
    api_id: int;
    device_model: string;
    system_version: string;
    app_version: string;
    system_lang_code: string;
    lang_pack: string;
    lang_code: string;
    proxy: TLT.InputClientProxy.t option;
  }

  let create
    ~api_id
    ?(device_model = "Unknown Device")
    ?(system_version = "Unknown")
    ?(app_version = "1.0")
    ?(system_lang_code = "en")
    ?(lang_pack = "")
    ?(lang_code = "en")
    ?(proxy = None)
    ()
  = { api_id; device_model; system_version; app_version; system_lang_code;
      lang_pack; lang_code; proxy }
end

let layer_const = 82

(* TODO: *)
(* module type TelegramSession = sig
end *)

module MakeTelegramClient (T: MTProtoTransport) = struct
  module MTP = MakeMTProtoV2Client(T)

  type t = { mtp: MTP.t }

  let create ?(auth_key: Cstruct.t option) () =
    let%lwt mtp = MTP.create ?auth_key () in

    let%lwt () = match auth_key with
      | Some _ -> Lwt.return_unit
      | None ->
        let%lwt _new_auth_key = MTP.do_authentication mtp in
        (* Log.debug (fun m -> m "new auth_key:@.%a" hexdump_pp new_auth_key); *)
        Lwt.return_unit
    in

    let t = { mtp } in

    Lwt.return t

  let invoke t = MTP.invoke t.mtp

  let init_with
    (t: t)
    (s: Settings.t)
    (type x result)
    (module X : TLFunc with type t = x and type ResultM.t = result)
    (x: x)
    : result Lwt.t
  =
    MTP.invoke t.mtp (module TLT.C_invokeWithLayer(TLT.C_initConnection(X))) {
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
        query = x;
      }
    }

  let init (s: Settings.t) (t: t) =
    let%lwt (C_config res) = init_with t s (module TLT.C_help_getConfig) C in
    Log.info (fun m -> m "help.getConfig res. me_url_prefix: %s" res.me_url_prefix);
    Lwt.return_unit

  let loop t =
    Lwt.pick [MTP.recv_loop t.mtp; MTP.send_loop t.mtp]
end

module TelegramClient = MakeTelegramClient(TransportTcpFull)
(** [TelegramClient = MakeTelegramClient(TransportTcpFull)] *)
