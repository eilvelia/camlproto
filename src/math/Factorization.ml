open! Base

let src = Logs.Src.create "camlproto.math.factor"
module Log = (val Logs.src_log src : Logs.LOG)

(** [gcd x y] find the greatest common divisor for _unsigned_ [x] and [y] *)
let[@inline] gcd (x : int64) (y : int64) =
  let open Int64 in
  let t = x lor y in
  if x = 0L || y = 0L then
    t
  else
    let shift = ctz t in
    let u = ref x in
    let v = ref y in
    while !u <> 0L do
      u := !u lsr (ctz !u);
      v := !v lsr (ctz !v);
      if !u >= !v then (* TODO: >= does not work correctly for unsigned, and
                          unsigned_compare from OCaml 4.08 is too slow *)
        u := (!u - !v) lsr 1
      else
        v := (!v - !u) lsr 1
    done;
    !v lsl shift

let next_random_int64 max = BasicRand.int64 max

(* Koalas are so ugly sometimes. *)

let pq_factorize_int64 (what : int64) =
  if Int64.(what < 2L) then
    invalid_arg "pq_factorize expects 1 < n < 2^63";
  let g = ref 0L in
  let iter = ref 0 in
  let exit = ref false in
  for i = 0 to 2 do if not !exit && !iter < 1000 then begin
    let q = Int64.(next_random_int64 128L land 15L + 17L) in
    let x = ref Int64.(next_random_int64 what + 1L) in
    let y = ref !x in
    let lim = 1 lsl (i + 18) in

    let exit' = ref false in
    for j = 1 to lim - 1 do if not !exit' then begin
      Int.incr iter;
      let a = ref !x in
      let b = ref !x in
      let c = ref q in

      Int64.(
        while !b <> 0L do                    (* while b <> 0 *)
          if (!b land 1L) <> 0L then begin   (* if b is odd then *)
            c := !c + !a;                    (*   c += a *)
            if !c >= what then               (*   if c >= what then c -= what *)
              c := !c - what
          end;
          a := !a lsl 1;                     (* a *= 2 *)
          if !a >= what then begin           (* if a >= what then a -= what *)
            a := !a - what
          end;
          b := !b lsr 1                      (* a /= 2 *)
        done
      );

      x := !c;
      let z = Int64.(if !x < !y then what + !x - !y else !x - !y) in
      g := gcd z what;

      if Int64.(!g <> 1L) then
        exit' := true
      else if j land (j - 1) = 0 then
        y := !x
    end done;
    if Int64.(!g > 1L && !g < what) then
      exit := true
  end done;

  let g = !g in
  let f = Int64.(what / g) in
  if Int64.(f < g) then
    f, g
  else
    g, f

(* TODO: Use a different algorithm for pq_factorize? *)

let calc_size n = 8 - Int64.clz n / 8

let pq_factorize (cs : Cstruct.t) =
  if Cstruct.length cs > 8 then
    (* TODO: pq_factorize_big *)
    invalid_arg "pq_factorize does not support size > 8 buffers";
  let pq = Cstruct.BE.get_uint64 cs 0 in
  let p, q = pq_factorize_int64 pq in
  Log.debug (fun m -> m "Factorized %Ld -> %Ld, %Ld" pq p q);
  let p_size = calc_size p and q_size = calc_size q in
  let result = Cstruct.create_unsafe (p_size + q_size) in
  for i = 0 to p_size - 1 do
    let shift = (p_size - i - 1) * 8 in
    let byte = Int64.(to_int_trunc @@ p lsr shift land 0xffL) in
    Cstruct.set_uint8 result i byte
  done;
  for i = 0 to q_size - 1 do
    let shift = (q_size - i - 1) * 8 in
    let byte = Int64.(to_int_trunc @@ q lsr shift land 0xffL) in
    Cstruct.set_uint8 result (p_size + i) byte
  done;
  let p_buf = Cstruct.sub result 0 p_size in
  let q_buf = Cstruct.sub result p_size q_size in
  p_buf, q_buf

let%expect_test "should factorize correctly" =
  let p, q = pq_factorize_int64 0x1dfaf951107f49dfL in
  Caml.Printf.printf "%Ld, %Ld" p q;
  [%expect {| 1124459477, 1921201379 |}]

let%expect_test "should factorize correctly given a prime number input" =
  let p, q = pq_factorize_int64 67_280_421_310_721L in
  Caml.Printf.printf "%Ld, %Ld" p q;
  [%expect {| 1, 67280421310721 |}]

(* This doesn't support numbers in the negative range, the TDLib / TDesktop's
   "small" algorithm doesn't support pq > (1<<63) either *)
(* let%expect_test "should factorize correctly a number in the negative range" =
  let p, q = pq_factorize_int64 (-8390791955656038479L) (* 10055952118053513137 *) in
  (* Should be 1757925569, 5720351473 *)
  Caml.Printf.printf "%Ld, %Ld" p q *)
