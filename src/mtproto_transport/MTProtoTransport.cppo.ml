open! Base

#ifdef JS_OF_OCAML

module TransportTcpFull = JsTcpFull.Transport

#else

module TransportTcpFull = CamlTcpFull.Transport
module TransportTcpAbridged = CamlTcpAbridged.Transport

#endif

include Types
