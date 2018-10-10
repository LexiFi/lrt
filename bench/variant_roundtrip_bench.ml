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

let[@landmark "test"] () =
  let[@landmark "read_file"] v = Variant.variant_of_file filename in
  let[@landmark "of_variant"] x = Variant.of_variant ~t v in
  let[@landmark "to_variant"] v = Variant.to_variant ~t x in
  let[@landmark "printing"] () =
    let fmt = Format.formatter_of_out_channel (open_out "/dev/null") in
    Variant.print_variant fmt v
  in ()

