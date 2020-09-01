open! Base

type position = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}
[@@deriving show, equal, compare, sexp, hash]

let get_pos_info pos =
  (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1)

let compare_position p1 p2 =
  let (_, p1l, p1c) = get_pos_info p1 in
  let (_, p2l, p2c) = get_pos_info p2 in
  let c = Int.compare p1l p2l in
  if c = 0 then Int.compare p1c p2c
  else c

type t = {
  loc_start: position;
  loc_end: position;
}
[@@deriving equal, compare, sexp, hash]

let make (loc_start, loc_end) = { loc_start; loc_end }

let make' loc_start loc_end = { loc_start; loc_end }

let of_lexbuf (lexbuf: Lexing.lexbuf) =
  { loc_start = lexbuf.lex_start_p; loc_end = lexbuf.lex_curr_p }

let to_tuple t =
  let _, l1, c1 = get_pos_info t.loc_start in
  let _, l2, c2 = get_pos_info t.loc_end in
  (l1, c1), (l2, c2)

let empty_pos = {
  pos_fname = "";
  pos_lnum = 1;
  pos_bol = 0;
  pos_cnum = 0;
}

let empty =
  { loc_start = empty_pos; loc_end = empty_pos }

let show t =
  let (fname, lstart, cstart) = get_pos_info t.loc_start in
  let (_, lend, cend) = get_pos_info t.loc_end in
  Printf.sprintf "%s:%d:%d-%d:%d" fname lstart cstart lend cend

let pp fmt t = Caml.Format.fprintf fmt "%s" (show t)

let (=) = equal

let[@inline] max f l1 l2 =
  if f l1 l2 >= 0 then l2 else l1

let[@inline] min f l1 l2 =
  if f l1 l2 < 0 then l2 else l1

let concat locs =
  let f acc l = {
    loc_start = min compare_position acc.loc_start l.loc_start;
    loc_end = max compare_position acc.loc_end l.loc_end;
  } in
  match locs with
  | [] -> empty
  | x :: xs -> List.fold xs ~init:x ~f

type 'node annot =
  (t [@sexp.opaque] [@compare.ignore] [@equal.ignore])
  * 'node
[@@deriving show, equal, compare, sexp, hash]

let[@inline] map_annot : 'a 'b. 'a annot -> ('a -> 'b) -> 'b annot =
  fun (loc, node) f -> (loc, f node)

let[@inline] value_map_annot : 'a 'b. 'a annot -> ('a -> 'b) -> 'b =
  fun (_loc, node) f -> f node
