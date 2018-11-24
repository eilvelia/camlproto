open! Base

#ifdef JS_OF_OCAML

module TransportTcpFull = Js_tcp_full.TransportTcpFull

#else

module TransportTcpFull = Caml_tcp_full.TransportTcpFull

#endif

include Types
