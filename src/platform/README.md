
Platform-dependent functions:

- get_current_time()
- aes ecb
- sha1
- sha256
- secure random (`rand_cs(size)` function that returns byte array)
- bigint, full list of functions is available in [types.ml][] file.
- gzip (decompress)

[types.ml]: types.ml

### OCaml

For crypto and secure random `Nocrypto` is used.

For bigint `Zarith` is used.

For gzip `Ezgip` is used.

### Node.js

For crypto builtin `crypto` is used.

For secure random `crypto.randomBytes(size)` is used.

For bigint `leemon` is used.

For gzip `pako` is used.

### Browser

WIP

<!-- For gzip `pako` is used. -->

<!-- TODO: -->
