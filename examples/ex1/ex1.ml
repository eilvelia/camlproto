open Camlproto
open MTProto
open MTProtoTransport

module TLG = TLGen.MTProto

let main () =
  let module MTP = MakeMTProtoV2Client(TransportTcpFull) in
  (* let module MTP = MakeMTProtoV2Client(TransportTcpAbridged) in *)
  let%lwt t = MTP.create () in
  let%lwt () = MTP.do_authentication t in
  let send_pings () =
    let%lwt (C_pong a) = MTP.invoke t (module TLG.C_ping) { ping_id = 1L } in
    Printf.printf "<-- Pong 1 [ping_id %Ld]\n" a.ping_id;
    let%lwt () = Lwt_unix.sleep 1. in
    let%lwt (C_pong b) = MTP.invoke t (module TLG.C_ping) { ping_id = 2L } in
    Printf.printf "<-- Pong 2 [ping_id %Ld]\n" b.ping_id;
    Lwt.return_unit
  in
  let%lwt () = Lwt.join [send_pings (); MTP.recv_loop t] in
  Lwt.return ()

let () = Lwt_main.run (main ())
