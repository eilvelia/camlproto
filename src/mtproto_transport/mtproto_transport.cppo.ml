open! Base

#ifdef JS_OF_OCAML

module TransportTcpFull = Js_tcp_full.TransportTcpFull

#else

module TransportTcpFull = Caml_tcp_full.TransportTcpFull
module TransportTcpAbridged = Caml_tcp_abridged.TransportTcpAbridged

#endif

include Types
