
Platform-dependent functions:

- get_current_time()
- aes ecb
- sha1
- sha256
- secure random (`rand_cs(size)` function that returns byte array)
- bigint, full list of functions is available in [Types.ml][] file.
- gzip (decompress)

[Types.ml]: Types.ml

### OCaml

For crypto and secure random `Nocrypto` is used.

For bigint `Zarith` is used.

For gzip `Ezgzip` is used.

### JS

For bigint `leemon` is used.

For gzip `pako` is used.

#### Node.js

For crypto builtin `crypto` is used.

For secure random `crypto.randomBytes(size)` is used.

#### Browser

WIP

<!-- TODO: -->
