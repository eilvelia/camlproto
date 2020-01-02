# Camlproto &nbsp; ![](https://github.com/Bannerets/camlproto/workflows/Build%20and%20test/badge.svg)

Portable and type-safe client implementation of Telegram's [MTProto][] protocol and [TL][] data serialization format.

[MTProto]: https://core.telegram.org/mtproto
[TL]: https://core.telegram.org/mtproto/TL

## Usage

### From OCaml

```ocaml
open Camlproto

module T = TLGen.Telegram

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
    let%lwt TL_auth_sentCode { phone_code_hash; _ } =
      Client.invoke t (module T.TL_auth_sendCode) {
        phone_number;
        api_id;
        api_hash;
        settings = TL_codeSettings {
         allow_flashcall = None;
         current_number = None;
         allow_app_hash = None;
        }
      } in
    let%lwt phone_code = prompt "Enter the code: " in
    let%lwt [@warning "-8"] TL_auth_authorization { user; _ } =
      Client.invoke t (module T.TL_auth_signIn) {
        phone_number;
        phone_code_hash;
        phone_code;
      } in
    let TL_user { id; _ } | TL_userEmpty { id } = user in
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
| `Bool`           | `bool`           | `boolean`        |
| `true`           | default          | `true`           |
| `vector a`       | `'a list`        | `Array<A>`       |
| `Null`           | default          | `true`           |

### Other

| TL                       | OCaml            | JavaScript       |
|--------------------------|------------------|------------------|
| Conditional definitions  | `'a option`      | `A \| undefined` |

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

```sh
dune build
```

Run the tests:

```sh
dune runtest
```
