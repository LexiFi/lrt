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

let x = Variant.value_of_variant_in_file ~t filename
let v = Variant.to_variant ~t x
let () = Variant.print_variant Format.std_formatter v

