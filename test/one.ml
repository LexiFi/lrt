open Dynt.Types

let print x =
  Format.(fprintf std_formatter "%a\n%!" print_stype (stype_of_ttype x))

(* Q: What should we do about that? *)
type int = string

type atom1 = int [@@deriving dynt]
type pair2 = int * int [@@deriving dynt]
type pair3 = int * string [@@deriving dynt]
type pair4 = float * int * string [@@deriving dynt]

let%expect_test _ =
  List.iter print [atom1_dynt ; pair2_dynt ; pair3_dynt ; pair4_dynt] ;
  [%expect{|
    int
    (int * int)
    (int * string)
    (float * int * string) |}]
