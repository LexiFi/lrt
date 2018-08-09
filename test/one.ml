open Dynt.Types

type t = int * int [@@deriving show]

let t : t ttype = DT_tuple [ DT_int ; DT_int ] |> Obj.magic

let stdout = Format.std_formatter
let flush fmt = Format.pp_print_flush fmt ()

let%expect_test _ =
  print_stype stdout (stype_of_ttype t) ;
  flush stdout ;
  [%expect{| (int * int) |}]

let%expect_test _ =
  show (42, 7) |> print_endline ;
  [%expect{| (42, 7) |}]
