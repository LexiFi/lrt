(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the lrt package. Details can be found in the attached LICENSE file.    *)
(******************************************************************************)

open Lrt
open Check
open Variant_roundtrip_type

let _ =
  let seed = 42 and size = 100 and n = 100_000 in
  print_endline "[" ;
  let _ =
    test n ~seed ~generator:(of_type_gen ~size [] ~t) (fun x ->
        Format.printf "%a;\n" Variant.print_variant (Variant.to_variant ~t x) ;
        true )
  in
  print_endline "]"
