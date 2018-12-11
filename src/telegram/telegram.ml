open! Base
open Mtproto
open Mtproto_transport
open TL.Types

(* module TLM = TLGen.MTProto *)
module TLT = TLGen.Telegram

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

module MakeTelegramClient (T: MTProtoTransport) = struct
  module MTP = MakeMTProtoV2Client(T)

  type t = { mtproto: MTP.t }

  let create () =
    let%lwt mtproto = MTP.create () in

    let t = { mtproto } in

    let%lwt () = MTP.do_authentication mtproto in

    Lwt.return t

  let invoke t = MTP.invoke t.mtproto

  let init_with
    t
    (s: Settings.t)
    (type a result)
    (module X : TLFunc with type t = a and type ResultM.t = result)
    (x: a)
    : result Lwt.t
  =
    MTP.invoke t.mtproto
      (module TLT.C_invokeWithLayer(TLT.C_initConnection(X))) {
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

  let connect (s: Settings.t) (t: t) =
    let%lwt (C_config res) = init_with t s (module TLT.C_help_getConfig) C in
    Caml.print_endline ("help.getConig res. me_url_prefix: " ^ res.me_url_prefix);
    Lwt.return ()

  let loop t =
    MTP.recv_loop t.mtproto
end

module TelegramClient = MakeTelegramClient(TransportTcpFull)
(** [TelegramClient = MakeTelegramClient(TransportTcpFull)] *)
