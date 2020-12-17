WIP

Camlproto can also be compiled to JavaScript using js_of_ocaml.

## Usage from JavaScript

wip

## TL <-> JavaScript mapping

| TL               | JavaScript       |
|------------------|------------------|
| `int`            | `number`         |
| `nat` (`#`)      | `number`         |
| `long`           | `string`         |
| `string`         | `string`         |
| `double`         | `number`         |
| `int128`         | `Uint8Array`     |
| `int256`         | `Uint8Array`     |
| `bytes`          | `Uint8Array`     |
| `Bool`           | `boolean`        |
| `vector a`       | `Array<A>`       |
| `true`           | `true`           |
| `Null`           | `true`           |

### Other

| TL                       | JavaScript                      |
|--------------------------|---------------------------------|
| Conditional definitions  | <code>A &#124; undefined</code> |
