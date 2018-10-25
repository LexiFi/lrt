(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the dynt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

open Dynt

type t = Variant_roundtrip_type.t list [@@deriving t]

(*
let filename =
  if Array.length Sys.argv > 1 then Sys.argv.(1)
  else (
    Printf.eprintf "Please provide filename of data as first argument" ;
    exit 1 )

let value = Variant.value_of_variant_in_file ~t filename
*)

let[@landmark "iteration"] run () =
  let[@landmark "prepare"] Json.({of_json; to_json}) = Json.conv t in
  ignore (of_json, to_json)

(*
  let[@landmark "to_json"] json = to_json value in
  let[@landmark "of_json"] value' = of_json json in
  ignore value'
  *)

let _ = List.init 100_000 (fun _ -> run ())
