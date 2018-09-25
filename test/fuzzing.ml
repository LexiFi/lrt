open Dynt
open Check

(* Generate random ttypes with random witnesses and throw them at our
   printing function *)

let f : Ttype.dynamic -> bool = function Ttype.Dyn (t,x) ->
  let () =
    Format.printf "Type: %a\n%!"
      Stype.print_stype (Ttype.stype_of_ttype t) ;
    Format.printf "Value: %a\n%!" (Print.print ~t) x
  in true

let () =
  let n = if Array.length Sys.argv > 1 then
      match int_of_string_opt Sys.argv.(1) with
      | Some i -> i
      | None -> 111
    else 111
  in
  let seed = if Array.length Sys.argv > 2 then
      match int_of_string_opt Sys.argv.(2) with
      | Some i -> i
      | None -> 42
    else 42
  in
  ignore (test n ~seed ~generator:(dynamic ~size:10 []) f)
