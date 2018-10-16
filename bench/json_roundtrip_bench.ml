open Dynt

type t = Variant_roundtrip_type.t list
[@@deriving t]

let filename =
  if Array.length Sys.argv > 1 then
    Sys.argv.(1)
  else begin
    Printf.eprintf "Please provide filename of data as first argument";
    exit 1
  end

let value = Variant.value_of_variant_in_file ~t filename

let of_json = Json.of_json ~t ?ctx:None
and to_json = Json.to_json ~t ?ctx:None

let[@landmark "test"] run () =
  let[@landmark "to_json"] json = to_json value in
  let[@landmark "of_json"] value' = of_json json in
  ignore(value')

let _ = List.init 10 (fun _ -> run ())
