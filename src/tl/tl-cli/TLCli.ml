
let () =
 match Sys.argv with
 | [|_; filein; fileout|] ->
     let ic = open_in filein in
     let strin = really_input_string ic (in_channel_length ic) in
     close_in ic;
     let (errs, strout) = TLLib.translate strin in
     List.iter (fun e -> prerr_endline @@ TLLib.Err.show e) errs;
     let oc = open_out fileout in
     output_string oc strout
 | _ -> prerr_endline "Invalid arguments"
