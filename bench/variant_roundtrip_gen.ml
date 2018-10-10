open Dynt
open Check
open Variant_roundtrip_type

let _ =
  let seed = 42
  and size = 100
  and n = 100_000
  in
  print_endline "[";
  let _ = test n ~seed ~generator:(of_type_gen ~size [] ~t) (fun x ->
      Format.printf "%a;\n" (Print.print ~t) x; true) in
  print_endline "]"
