# Camlproto

Fully type-safe MTProto client implementation in OCaml.

## Usage

### From OCaml

```ocaml
open Camlproto

module TLT = TLGen.Telegram

let prompt str = Lwt_io.(let%lwt () = write stdout str in read_line stdin)

module Client = Telegram.Client.Make(PlatformCaml)(TransportTcpFullCaml)

let main () =
  (* api_id and api_hash can be obtained at https://my.telegram.org/ *)
  let%lwt phone_number = prompt "Enter your phone number: " in
  let%lwt api_id = prompt "Enter your api id: " in
  let api_id = int_of_string api_id in
  let%lwt api_hash = prompt "Enter your api hash: " in

  let%lwt t = Client.create () in

  let promise =
    let%lwt () = Client.init (Telegram.Settings.create ~api_id ()) t in
    let%lwt C_auth_sentCode { phone_code_hash; _ } =
      Client.invoke t (module TLT.C_auth_sendCode) {
        allow_flashcall = None;
        phone_number;
        current_number = None;
        api_id;
        api_hash;
      } in
    let%lwt phone_code = prompt "Enter phone code: " in
    let%lwt C_auth_authorization { user; _ } =
      Client.invoke t (module TLT.C_auth_signIn) {
        phone_number;
        phone_code_hash;
        phone_code;
      } in
    let (C_user { id; _ } | C_userEmpty { id }) = user in
    print_endline ("Signed as " ^ string_of_int id);
    Lwt.return_unit
  in

  Lwt.pick [promise; Client.loop t]

let _ = Lwt_main.run (main ())
```

(see [examples/ex2/](examples/ex2/) and [examples/ex1/](examples/ex1/))

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

- tcp_full (ocaml, node.js)

- tcp_abridged (ocaml)

### In progress

- tcp_abridged (node.js)

### Not implemented

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
