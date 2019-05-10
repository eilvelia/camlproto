open Camlproto
open Telegram

module TLT = TLGen.Telegram

let prompt str = Lwt_io.(let%lwt () = write stdout str in read_line stdin)

let main () =
  (* api_id and api_hash can be obtained at https://my.telegram.org/ *)
  let%lwt phone_number = prompt "Enter your phone number: " in
  let%lwt api_id = prompt "Enter your api id: " in
  let api_id = int_of_string api_id in
  let%lwt api_hash = prompt "Enter your api hash: " in

  let module Client = MakeTelegramClient(PlatformCaml)(TransportTcpFullCaml) in
  let%lwt t = Client.create () in

  let promise =
    let%lwt () = Client.init (Settings.create ~api_id ()) t in
    let%lwt C_auth_sentCode { phone_code_hash; _ } =
      Client.invoke t (module TLT.C_auth_sendCode) {
        allow_flashcall = None;
        phone_number;
        current_number = None;
        api_id;
        api_hash;
      } in
    let%lwt phone_code = prompt "Enter phone code: " in
    let%lwt C_auth_authorization { user; _ } =
      Client.invoke t (module TLT.C_auth_signIn) {
        phone_number;
        phone_code_hash;
        phone_code;
      } in
    let (C_user { id; _ } | C_userEmpty { id }) = user in
    print_endline ("Signed as " ^ string_of_int id);
    Lwt.return_unit
  in

  Lwt.pick [promise; Client.loop t]

let () =
  Logs.(set_level (Some Debug));
  Logs.set_reporter (CamlReporter.reporter ());
  Lwt_main.run (main ())
