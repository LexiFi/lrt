open Dynt

let print x =
  Types.stype_of_ttype x
  |> Format.(fprintf std_formatter "%a\n%!" Types.print_stype)

type t = int [@@deriving t]
type pair1 = string * int [@@deriving t]
type pair2 = int * string [@@deriving t]
type pair3 = float * int * string [@@deriving t]

let%expect_test _ =
  List.iter print [t ; pair1_t ; pair2_t ; pair3_t] ;
  [%expect{|
    int
    (string * int)
    (int * string)
    (float * int * string) |}]

type bool = string [@@deriving t]
let%expect_test _ =
  print bool_t ;
  [%expect{|
    string |}]
