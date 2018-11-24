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
  let%lwt () = MTP.send_encrypted_obj t (module TLG.C_ping) { ping_id = 2L } in
  let%lwt data = MTP.receive_encrypted t in
  Cstruct.hexdump data;
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

### In progress

- tcp_full (node.js)

### Unimplemented

- websocket secure (browser)

- tcp_abridged

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
npm run codegen:caml:mtproto
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
