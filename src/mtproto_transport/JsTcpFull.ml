open! Base

type js_t

class type js_tcp_full = object
  method create
    : Js.js_string Js.t -> Js.js_string Js.t -> (js_t -> unit) Js.callback -> unit Js.meth
  method send
    : js_t -> Cstruct.buffer -> (unit -> unit) Js.callback -> unit Js.meth
  method receive
    : js_t -> (Typed_array.arrayBuffer Js.t -> unit) Js.callback -> unit Js.meth
end

let js_tcp_full: js_tcp_full Js.t = Js.Unsafe.js_expr "js_tcp_full"

module Transport: Types.MTProtoTransport = struct
  type t = js_t
  exception Error of string (* Not used *)
  let create (address, port) =
    let (promise, resolver) = Lwt.task () in
    let cb t = Lwt.wakeup_later resolver t in
    js_tcp_full##create (Js.string address) (Js.string port) (Js.wrap_callback cb);
    promise
  let send t packet =
    Caml.print_endline "js_tcp_full##send start";
    let (promise, resolver) = Lwt.task () in
    let cb () = Lwt.wakeup_later resolver () in
    js_tcp_full##send t (Cstruct.to_bigarray packet) (Js.wrap_callback cb);
    Caml.print_endline "js_tcp_full##send end";
    promise
  let receive t =
    let (promise, resolver) = Lwt.task () in
    let cb x =
      let data = x
        |> Typed_array.Bigstring.of_arrayBuffer
        |> Cstruct.of_bigarray in Caml.print_endline (Int.to_string @@ Cstruct.len data); Cstruct.hexdump data;
      Lwt.wakeup_later resolver data
    in
    js_tcp_full##receive t (Js.wrap_callback cb);
    promise
end
