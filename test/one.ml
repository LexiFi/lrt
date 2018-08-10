let print x =
  Dynt.Types.stype_of_ttype x
  |> Format.(fprintf std_formatter "%a\n%!" Dynt.Types.print_stype)

type t = int [@@deriving t]
type pair1 = string * int [@@deriving t]
type pair2 = int * string [@@deriving t]
type triple = float * int * string [@@deriving t]
type qdrple = float * int * string * int [@@deriving t]

let%expect_test _ =
  List.iter print [t ; pair1_t ; pair2_t ; triple_t ; qdrple_t] ;
  [%expect{|
    int
    (string * int)
    (int * string)
    (float * int * string)
    (float * int * string * int) |}]

type bool = string [@@deriving t]
let%expect_test _ =
  print bool_t ;
  [%expect{|
    string |}]

type two_public = Two.public [@@deriving t]
type two_hidden = Two.hidden [@@deriving t]
type two_hidden2 = two_hidden [@@deriving t]
let%expect_test _ =
  print two_public_t ;
  print two_hidden_t ;
  print two_hidden2_t ;
  [%expect{|
    (int * int)
    Two.hidden
    Two.hidden |}]
