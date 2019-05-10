type t = {
  api_id: int;
  device_model: string;
  system_version: string;
  app_version: string;
  system_lang_code: string;
  lang_pack: string;
  lang_code: string;
  proxy: TLGen.Telegram.InputClientProxy.t option;
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
