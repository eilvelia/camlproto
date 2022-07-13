open Camlproto

module T = TLGen.Telegram

module Client = Telegram.Client.Make(PlatformCaml)(TransportTcpFullCaml)

let prompt str = Lwt_io.(let%lwt () = write stdout str in read_line stdin)

let main () =
  (* api_id and api_hash can be obtained at https://my.telegram.org/ *)
  let%lwt phone_number = prompt "Enter your phone number: " in
  let%lwt api_id = prompt "Enter your api id: " in
  let api_id = int_of_string api_id in
  let%lwt api_hash = prompt "Enter your api hash: " in

  let%lwt t = Client.create () in

  let%lwt () = Client.init (Telegram.Settings.create ~api_id ()) t in
  let%lwt TL_auth_sentCode { phone_code_hash; _ } =
    Client.invoke t (module T.TL_auth_sendCode) {
      phone_number;
      api_id;
      api_hash;
      settings = TL_codeSettings {
        allow_flashcall = None;
        current_number = None;
        allow_app_hash = None;
      }
    } in
  let%lwt phone_code = prompt "Enter the code: " in
  let%lwt [@warning "-8"] TL_auth_authorization { user; _ } =
    Client.invoke t (module T.TL_auth_signIn) {
      phone_number;
      phone_code_hash;
      phone_code;
    } in
  let TL_user { id; _ } | TL_userEmpty { id } = user in
  print_endline ("Signed as " ^ string_of_int id);
  Lwt.return_unit

let () =
  Logs.(set_level (Some Debug));
  Logs.set_reporter (CamlReporter.reporter ());
  Lwt_main.run (main ())
