open Camlproto
open MTProto

module TLM = TLGen.MTProto

let main () =
  let module MTP = MakeMTProtoV2Client(PlatformCaml)(TransportTcpFullCaml) in
  let%lwt t = MTP.create () in
  let%lwt _ = MTP.do_authentication t in
  let send_pings () =
    let%lwt (C_pong a) = MTP.invoke t (module TLM.C_ping) { ping_id = 1L } in
    Printf.printf "<-- Pong 1 [ping_id %Ld]\n" a.ping_id;
    let%lwt () = Lwt_unix.sleep 1. in
    let%lwt (C_pong b) = MTP.invoke t (module TLM.C_ping) { ping_id = 2L } in
    Printf.printf "<-- Pong 2 [ping_id %Ld]\n" b.ping_id;
    Lwt.return_unit
  in
  let%lwt () = Lwt.pick [send_pings (); MTP.recv_loop t; MTP.send_loop t] in
  Lwt.return ()

let () =
  Logs.(set_level (Some Debug));
  Logs.set_reporter (CamlReporter.reporter ());
  Lwt_main.run (main ())
