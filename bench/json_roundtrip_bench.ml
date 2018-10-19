open Dynt

type t = Variant_roundtrip_type.t list [@@deriving t]

let filename =
  if Array.length Sys.argv > 1 then Sys.argv.(1)
  else (
    Printf.eprintf "Please provide filename of data as first argument" ;
    exit 1 )

let value = Variant.value_of_variant_in_file ~t filename

let[@landmark "iteration"] run () =
  let[@landmark "prepare"] Json.({of_json; to_json}) = Json.conv t in
  let[@landmark "to_json"] json = to_json value in
  let[@landmark "of_json"] value' = of_json json in
  ignore value'

let _ = List.init 10 (fun _ -> run ())
