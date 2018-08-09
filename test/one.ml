open Dynt.Types

type t = int * int

let t : t ttype = DT_tuple [ DT_int ; DT_int ] |> Obj.magic

let stdout = Format.std_formatter
let flush fmt = Format.pp_print_flush fmt ()

let%expect_test _ =
  print_stype stdout (stype_of_ttype t) ;
  flush stdout ;
  [%expect{| (int * int) |}]
