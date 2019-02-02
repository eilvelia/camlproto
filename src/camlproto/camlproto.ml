open! Base

module TL = TL
module MTProto = Mtproto
module MTProtoTransport = Mtproto_transport
module Telegram = Telegram
module TLGen = TLGen
module Rsa = Math.Crypto.Rsa

module CamlReporter: sig
  val pp_header: Formatter.t -> Logs.level * Logs.src * string option -> unit
  val reporter: ?app:Formatter.t -> ?dst:Formatter.t -> unit -> Logs.reporter
end = struct
  let pp_header =
    let pf = Caml.Format.fprintf in
    fun ppf (level, src, header) -> match header with
      | None -> pf ppf "[%a][%s] " Logs.pp_level level (Logs.Src.name src)
      | Some h -> pf ppf "[%s][%s] " h (Logs.Src.name src)

  let reporter ?(app = Caml.Format.std_formatter) ?(dst = Caml.Format.err_formatter) () =
    let report src level ~over k msgf =
      (* Caml.print_endline "report"; *)
      let k _ = over (); k () in
      msgf @@ fun ?header ?tags:_ fmt ->
        let ppf = match level with Logs.App -> app | _ -> dst in
        Caml.(Format.kfprintf
          k ppf ("%a@[" ^^ fmt ^^ "@]@.") pp_header (level, src, header))
    in
    { Logs. report }
    (* Logs.{ report } *)
end
