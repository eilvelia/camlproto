open! Base

let crc32 (data: string) =
  let r = ref 0xFF_FF_FF_FFl in
  for i = 0 to String.length data - 1 do
    let ch = data.[i] in
    let chcode = Char.to_int ch |> Int32.of_int_trunc in
    let open Int32 in
    r := !r lxor chcode;
    for _ = 1 to 8 do
      let mask = -(!r land 1l) in
      r := (!r lsr 1) lxor (0xEDB88320l land mask)
    done
  done;
  Int32.lnot !r

let%expect_test "crc32" =
  Caml.Printf.printf "%lX" @@ crc32 "Uido0Ooz";
  [%expect {| 37955B3B |}]

let calculate (comb: Ast.combinator_decl) builtin =
  let open Ast in
  let str =
    if builtin
    then
      let bcomb = Loc.map_annot comb @@ fun comb' ->
        let b_comb_result = match snd @@ comb'.comb_result with
          | EIdent id -> id
          | _ -> failwith "invalid result type of a builtin combinator"
        in
        { b_comb_id = comb'.comb_id; b_comb_result }
      in
      crc32_gen_builtin_combinator_decl bcomb
    else
      crc32_gen_combinator_decl comb
  in
  crc32 str
