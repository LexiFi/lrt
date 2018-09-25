open Dynt
open Check

(* Generate random ttypes with random witnesses and use them for basic tests *)

let n = if Array.length Sys.argv > 1 then
    match int_of_string_opt Sys.argv.(1) with
    | Some i -> i
    | None -> 111
  else 111

let seed = if Array.length Sys.argv > 2 then
    match int_of_string_opt Sys.argv.(2) with
    | Some i -> i
    | None -> 42
  else 42

(* printing *)

let f : Ttype.dynamic -> bool = function Ttype.Dyn (t,x) ->
  let () =
    Format.printf "Type: %a\n%!"
      Stype.print_stype (Ttype.stype_of_ttype t) ;
    Format.printf "Value: %a\n%!" (Print.print ~t) x
  in true

let () =
  ignore (test n ~seed ~generator:(dynamic ~size:10 []) f)

(* variant *)

let f : Ttype.dynamic -> bool =
  function Ttype.Dyn (t,x) ->
    let v = Variant.variant ~t x in
    let () =
      Format.printf "Value: %a\n%!" (Print.print ~t) x;
      Format.printf "Variant: %a\n%!" Variant.print_variant v
    in
    let x' = Variant.of_variant ~t v in
    let s = Format.asprintf "%a%!" Variant.print_variant v in
    let v' = Variant_lexer.variant_of_string s in
    let x'' = Variant.of_variant ~t v' in
    let r = x = x' && x' = x'' && v = v' in
    if not r then
      Format.printf "Screwed:\nv': %a\nx'': %a\n%!"
        Variant.print_variant v'
        (Print.print ~t) x''
    ; r


let () =
  let seed = 2 * seed in
  match test n ~seed ~generator:(dynamic ~size:22 []) f with
  | Succeed _ -> ()
  | Throw {backtrace;_} -> failwith backtrace
  | Fail _ -> print_endline "test failed"
