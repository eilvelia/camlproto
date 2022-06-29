open! Base

(* let (.%[]<-) = Cstruct.set_char
let (.%[]) = Cstruct.get_char *)

let clone_cstruct (cs: Cstruct.t): Cstruct.t =
  let len = Cstruct.length cs in
  let cs' = Cstruct.create_unsafe len in
  Cstruct.blit cs 0 cs' 0 len;
  cs'

(* let print_cstruct name cs =
  Caml.print_endline (name ^ " " ^ (Int.to_string @@ Cstruct.length cs));
  Cstruct.hexdump cs *)

module Make (ECB: PlatformTypes.AES) = struct
  open ECB

  let encrypt (plain: Cstruct.t) (key: Cstruct.t) (iv: Cstruct.t) =
    let plain_len = Cstruct.length plain in
    let plain = if plain_len % 16 = 0
      then plain
      else begin (* Add padding *)
        let plain' = Cstruct.create (plain_len + 16 - plain_len % 16) in
        Cstruct.blit plain 0 plain' 0 plain_len;
        plain'
      end
    in
    let plain_len = Cstruct.length plain in

    let (iv1, iv2) = Cstruct.split iv (Cstruct.length iv / 2) in
    let (iv1, iv2) = (ref iv1, ref iv2) in

    let aes_key = ecb_create_key key in

    let blocks_count = plain_len / 16 in
    let cipher_text = Cstruct.create plain_len in

    for block_index = 0 to blocks_count - 1 do
      let index = block_index * 16 in
      (* Caml.print_endline @@ "loop iteration " ^ (Int.to_string block_index)
        ^ " index " ^ (Int.to_string index); *)

      (* print_cstruct "iv1" !iv1;
      print_cstruct "iv2" !iv2;
      print_cstruct "plain" plain; *)

      let plain_block = Cstruct.sub plain index 16 |> clone_cstruct in

      (* print_cstruct "plain_block" plain_block; *)

      for i = 0 to 15 do
        let el = Char.unsafe_of_int (
          (Char.to_int (Cstruct.get_char plain_block i))
          lxor (Char.to_int (Cstruct.get_char !iv1 i))
        ) in
        Cstruct.set_char plain_block i el
      done;

      let cipher_block = ecb_encrypt ~key:aes_key plain_block in

      (* print_cstruct "cipher_block" cipher_block; *)

      for i = 0 to 15 do
        let el = Char.unsafe_of_int (
          (Char.to_int (Cstruct.get_char cipher_block i))
          lxor (Char.to_int (Cstruct.get_char !iv2 i))
        ) in
        Cstruct.set_char cipher_block i el
      done;

      iv1 := cipher_block;
      iv2 := Cstruct.sub plain index 16;

      Cstruct.blit cipher_block 0 cipher_text index 16
    done;

    (* print_cstruct "end cipher_text" cipher_text; *)

    cipher_text

  let decrypt (cipher_text: Cstruct.t) (key: Cstruct.t) (iv: Cstruct.t) =
    let cipher_len = Cstruct.length cipher_text in

    let (iv1, iv2) = Cstruct.split iv (Cstruct.length iv / 2) in
    let (iv1, iv2) = (ref iv1, ref iv2) in

    let aes_key = ecb_create_key key in

    let blocks_count = cipher_len / 16 in
    let plain = Cstruct.create cipher_len in

    let cipher_block = Cstruct.create 16 in

    for block_index = 0 to blocks_count - 1 do
      let index = block_index * 16 in
      (* Caml.print_endline @@ "loop iteration " ^ (Int.to_string block_index)
        ^ " index " ^ (Int.to_string index); *)

      (* print_cstruct "iv1" !iv1;
      print_cstruct "iv2" !iv2;
      print_cstruct "cipher_text" cipher_text;
      print_cstruct "plain" plain;
      print_cstruct "cipher_block" cipher_block; *)

      for i = 0 to 15 do
        let el = Char.unsafe_of_int (
          (Char.to_int (Cstruct.get_char cipher_text (index + i)))
          lxor (Char.to_int (Cstruct.get_char !iv2 i))
        ) in
        Cstruct.set_char cipher_block i el
      done;

      let plain_block = ecb_decrypt ~key:aes_key cipher_block in

      (* print_cstruct "plain_block" plain_block; *)

      for i = 0 to 15 do
        let el = Char.unsafe_of_int (
          (Char.to_int (Cstruct.get_char plain_block i))
          lxor (Char.to_int (Cstruct.get_char !iv1 i))
        ) in
        Cstruct.set_char plain_block i el
      done;

      iv1 := Cstruct.sub cipher_text index 16;
      iv2 := plain_block;

      Cstruct.blit plain_block 0 plain index 16
    done;

    (* print_cstruct "end plain" plain; *)

    plain
end
