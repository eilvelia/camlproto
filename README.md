# Camlproto

MTProto client implementation in OCaml.

## Usage

### From OCaml

```ocaml
open Camlproto
open MTProto
open MTProtoTransport

module TLG = TLGen.MTProto

let main () =
  let module MTP = MakeMTProtoV2Client(TransportTcpFull) in
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
  let%lwt () = Lwt.pick [send_pings (); MTP.recv_loop t] in
  Lwt.return ()

let () = Lwt_main.run (main ())
```

(see [examples/ex1/](examples/ex1/))

### From JavaScript

wip

## TL <-> OCaml <-> JS mapping

### Builtins

| TL               | OCaml            | JavaScript       |
|------------------|------------------|------------------|
| `int`            | `int`            | `number`         |
| `nat` (`#`)      | `int32`          | `number`         |
| `long`           | `int64`          | `string`         |
| `string`         | `string`         | `string`         |
| `double`         | `float`          | `number`         |
| `int128`         | `Cstruct.t`      | `Uint8Array`     |
| `int256`         | `Cstruct.t`      | `Uint8Array`     |
| `bytes`          | `Cstruct.t`      | `Uint8Array`     |

### Non builtins

| TL               | OCaml            | JavaScript       |
|------------------|------------------|------------------|
| `Bool`           | `bool`           | `boolean`        |
| `true`           | default          | `true`           |
| `vector a`       | `'a list`        | `Array<A>`       |
| `Null`           | skipped          | skipped          |

### Other

| TL                       | OCaml            | JavaScript       |
|--------------------------|------------------|------------------|
| Conditional definitions  | `'a option`      | `A OR undefined` |

## Transport components

### Implemented

- tcp_full (ocaml)

- tcp_abridged (ocaml)

### In progress

- tcp_full (node.js)

### Unimplemented

- websocket secure (browser)

- tcp_intermediate

- tcp_obfuscated2

- http

- https

- udp

## Build

> Note: Node.js v6.0+ is also required.

Codegen:

```sh
cd codegen
npm install
npm run build
npm run codegen
cd ..
```

Compile ocaml code:

```sh
dune build
```

Run tests:

```sh
dune runtest
```
