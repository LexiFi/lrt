open Dynt.Types

type pair1 = int * int [@@deriving show]

let pair1_dynt : pair1 ttype = DT_tuple [ DT_int ; DT_int ] |> Obj.magic

let stdout = Format.std_formatter
let flush fmt = Format.pp_print_flush fmt ()

let%expect_test _ =
  print_stype stdout (stype_of_ttype pair1_dynt) ;
  flush stdout ;
  [%expect{| (int * int) |}]

let%expect_test _ =
  show_pair1 (42, 7) |> print_endline ;
  [%expect{| (42, 7) |}]

(* Now use our not yet premature deriving plugin *)

type pair2 = int * int [@@deriving dynt ]

let%expect_test _ =
  print_stype stdout (stype_of_ttype pair2_dynt) ;
  flush stdout ;
  [%expect{| (int * int) |}]
