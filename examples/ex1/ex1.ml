open Camlproto
open MTProto
open MTProtoTransport

module TLG = TLGen.MTProto

let main () =
  let module MTP = MakeMTProtoV2Client(TransportTcpFull) in
  let%lwt t = MTP.create () in
  let%lwt () = MTP.do_authentication t in
  let%lwt () = MTP.send_encrypted_obj t (module TLG.C_ping) { ping_id = 2L } in
  let%lwt data = MTP.receive_encrypted t in
  Cstruct.hexdump data;
  Lwt.return ()

let () = Lwt_main.run (main ())
