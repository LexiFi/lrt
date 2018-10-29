(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the lrt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

open Lrt
open Check

(* Generate random ttypes with random witnesses and use them for basic tests *)

let n =
  if Array.length Sys.argv > 1 then
    match int_of_string_opt Sys.argv.(1) with Some i -> i | None -> 111
  else 111

let seed =
  if Array.length Sys.argv > 2 then
    match int_of_string_opt Sys.argv.(2) with Some i -> i | None -> 42
  else 42

(* printing *)

let f : Ttype.dynamic -> bool = function
  | Ttype.Dyn (t, x) ->
      let () =
        Format.printf "Type: %a\n%!" Ttype.print t ;
        Format.printf "Value: %a\n%!" (Print.print ~t) x
      in
      true

let () = ignore (test n ~seed ~generator:(dynamic ~size:10 []) f)

(* variant *)

let f : Ttype.dynamic -> bool = function
  | Ttype.Dyn (t, x) ->
      let v = Variant.to_variant ~t x in
      let () =
        Format.printf "Value: %a\n%!" (Print.print ~t) x ;
        Format.printf "Variant: %a\n%!" Variant.print_variant v
      in
      let x' = Variant.of_variant ~t v in
      let s = Format.asprintf "%a%!" Variant.print_variant v in
      let v' =
        try Variant.variant_of_string s with r ->
          Printf.eprintf "Could not parse string:\n%s\n%!" s ;
          raise r
      in
      let x'' = Variant.of_variant ~t v' in
      let r = compare x x' = 0 && compare x' x'' = 0 && compare v v' = 0 in
      if not r then
        Format.printf "Screwed:\nv': %a\nx'': %a\n%!" Variant.print_variant v'
          (Print.print ~t) x'' ;
      r

let () =
  let seed = 2 * seed in
  match test n ~seed ~generator:(dynamic ~size:22 []) f with
  | Succeed _ -> ()
  | Throw {backtrace; _} -> failwith backtrace
  | Fail _ -> print_endline "test failed"

(* json *)

let f : Ttype.dynamic -> bool = function
  | Ttype.Dyn (t, x) ->
      let Json.({to_json; of_json}) = Json.conv t in
      let j = to_json x in
      let () =
        Format.printf "Value: %a\n%!" (Print.print ~t) x ;
        Format.printf "Json: %s\n%!" (Json.to_pretty_string j)
      in
      let x' = of_json j in
      let s = Json.encode j in
      let j' =
        try Json.decode s with r ->
          Printf.eprintf "Json.decode:\n%s\n%!" s ;
          raise r
      in
      let x'' = of_json j' in
      let r = compare x x' = 0 && compare x' x'' = 0 && compare j j' = 0 in
      if not r then
        Format.printf "Screwed:\nv': %s\nx'': %a\n%!"
          (Json.to_pretty_string j') (Print.print ~t) x'' ;
      r

let () =
  let seed = 3 * seed in
  match test n ~seed ~generator:(dynamic ~size:22 []) f with
  | Succeed _ -> ()
  | Throw {backtrace; _} -> failwith backtrace
  | Fail _ -> print_endline "test failed"
