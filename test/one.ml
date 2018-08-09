open Dynt.Types

type t = int * int

let t : t ttype = DT_tuple [ DT_int ; DT_int ] |> Obj.magic

let () =
  print_stype Format.std_formatter (stype_of_ttype t)
