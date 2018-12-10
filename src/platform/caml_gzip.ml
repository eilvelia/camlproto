open! Base

let string_of_ezgzip_error (error: Ezgzip.error) =
  match error with
  | Truncated content ->
    Printf.sprintf "Truncated content after %d bytes" (String.length content)
  | Invalid_format -> "Invalid gzip format"
  | Compression_error msg -> Printf.sprintf "Compression error: %s" msg
  | Size { got; expected } ->
    Printf.sprintf
      "Size mismatch after decompression: got %d, expected %d" got expected
  | Checksum -> "Invalid checksum after decompression"

module Gzip: Types.GZIP = struct
  exception Error of string
  let decompress cs =
    (* TODO: inefficient *)
    let str = Cstruct.to_string cs in
    match Ezgzip.decompress str with
    | Ok x -> Cstruct.of_string x
    | Error (`Gzip err) ->
      let errstr = string_of_ezgzip_error err in
      Caml.print_endline errstr;
      raise @@ Error errstr
end
