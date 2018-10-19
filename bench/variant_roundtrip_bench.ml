(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the dynt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

open Dynt

type t = Variant_roundtrip_type.t list [@@deriving t]

let filename =
  if Array.length Sys.argv > 1 then Sys.argv.(1)
  else (
    Printf.eprintf "Please provide filename of data as first argument" ;
    exit 1 )

let x = Variant.value_of_variant_in_file ~t filename

let[@landmark "test"] run () =
  let[@landmark "to_variant"] v = Variant.to_variant ~t x in
  let[@landmark "of_variant"] x' = Variant.of_variant ~t v in
  ignore x'

let _ = List.init 10 (fun _ -> run ())
