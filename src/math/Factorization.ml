open! Base

module Make (Platform: PlatformTypes.S) = struct
  module Crypto = Crypto.Make(Platform)
  module BasicRand = Crypto.BasicRand

  (*
    lsl -- <<
    asr -- >>
    lsr -- >>>
  *)

  let gcd (x: int64) (y: int64): int64 =
    let open Int64 in

    let a: int64 ref = ref x in
    let b: int64 ref = ref y in

    while !a <> 0L && !b <> 0L do
      while !b land 1L = 0L do
        b := !b asr 1
      done;

      while !a land 1L = 0L do
        a := !a asr 1
      done;

      if !a > !b
        then a := !a - !b
        else b := !b - !a
    done;

    if !b = 0L then !a else !b

  (* let rec gcd a b =
    Int64.(if b = 0L then a else (gcd [@tailcall]) b (a % b)) *)

  let next_random_int64 max = BasicRand.int64 max

  (* Koalas are so ugly sometimes. *)

  let pq_prime (what: int64): int64 * int64 =
    let g = ref 0L in
    let exit = ref false in
    for i = 0 to 2 do if not !exit then begin
      let q = Int64.(((next_random_int64 128L) land 15L) + 17L) in
      let x = ref Int64.(next_random_int64 1_000_000_000L + 1L) in
      let y = ref !x in
      let lim = 1 lsl (i + 18) in

      let exit' = ref false in
      for j = 1 to lim - 1 do if not !exit' then begin
        let a = ref !x in
        let b = ref !x in
        let c = ref q in

        Int64.(
          while !b <> 0L do
            if (!b land 1L) <> 0L then begin
              c := !c + !a;
              if !c > what then
                c := !c - what
            end;
            a := !a + !a;
            if !a > what then begin
              a := !a - what
            end;
            b := !b asr 1
          done
        );

        x := !c;
        let z = Int64.(if !x < !y then !y - !x else !x - !y) in
        g := gcd z what;

        if Int64.(!g <> 1L) then
          exit' := true
        else if (j land (j - 1)) = 0 then
          y := !x
      end done;
      if Int64.(!g > 1L) then
        exit := true
    end done;

    Int64.(
      let g = !g in
      let f = what / g in
      if g > f
        then (f, g)
        else (g, f)
    )
end
