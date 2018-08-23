open Dynt

let print x =
  Types.stype_of_ttype x
  |> Format.(fprintf std_formatter "%a\n%!" Types.print_stype)

module Basic = struct

  type t = int [@@deriving t]
  type pair1 = string * int [@@deriving t]
  type pair2 = int * string [@@deriving t]
  type triple = float * int * string [@@deriving t]
  type quartet = float * int * string * int [@@deriving t]
  type quintet = float * int * string array * int * int [@@deriving t]

  let%expect_test _ =
    print t;
    print pair1_t;
    print pair2_t;
    print triple_t;
    print quartet_t;
    print quintet_t;
    [%expect{|
    int
    (string * int)
    (int * string)
    (float * int * string)
    (float * int * string * int)
    (float * int * string array * int * int) |}]

  type enum = North | East | South | West [@@deriving t]
  let%expect_test _ = print enum_t ;
    [%expect{| (enum = North | East | South | West) |}]

  type sum1 =
    | Option1 of string * int
    | Option2 of int * int * string
    | Option3
    | Option4 of unit * int
  [@@deriving t]

  let%expect_test _ = print sum1_t ;
    [%expect{|
    (sum1 =
       | Option1 of (string * int)
       | Option2 of (int * int * string)
       | Option3
       | Option4 of (unit * int)) |}]

  type record =
    { field1 : int * string
    ; field2 : string
    ; field3 : float
    ; field4 : int
    }
  [@@deriving t]

  let%expect_test _ = print record_t ;
    [%expect{|
    (record =
       {
         field1: (int * string);
         field2: string;
         field3: float;
         field4: int;
       }) |}]

  type inline_record =
    | Basic of int
    | Inline of { h : string ; p : float }
  [@@deriving t]

  let%expect_test _ = print inline_record_t ;
    [%expect{|
    (inline_record =
       | Basic of int
       | Inline of
        (inline_record.Inline =
           {
             h: string;
             p: float;
           }))
    |}]

end

module Recursion = struct

  type record = { a: int ; b: record} [@@deriving t]
  type natural = Z | S of natural [@@deriving t]
  type natural2 = Z | S of natural2 | Sum of {a: natural2;  b: natural2}
  [@@deriving t]

  let%expect_test _ =
    print record_t ;
    print natural_t ;
    print natural2_t ;
    [%expect{|
    (record =
       {
         a: int;
         b: record;
       })
    (natural =
       | Z
       | S of natural)
    (natural2 =
       | Z
       | S of natural2
       | Sum of
        (natural2.Sum =
           {
             a: natural2;
             b: natural2;
           })) |}]

end
