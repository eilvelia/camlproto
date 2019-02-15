open! Base

include Types

#ifdef JS_OF_OCAML

module TcpFull = JsTcpFull.Transport

#else

module TcpFull = CamlTcpFull.Transport
module TcpAbridged = CamlTcpAbridged.Transport

#endif
