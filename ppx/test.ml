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

module Open = struct

  type 'num rectangle = { a: 'num ; b: 'num} [@@deriving t]
  type ('a,'b) alist = ('a * 'b) list [@@deriving t]
  type ('a,'b) alist2 = ('a, 'b) alist [@@deriving t]
  type aalist = (int,string) alist [@@deriving t]

  let%expect_test _ =
    print (rectangle_t int_t);
    print (alist_t int_t string_t);
    print (alist2_t int_t string_t);
    [%expect {|
      (int rectangle =
         {
           a: int;
           b: int;
         })
      (int * string) list
      (int * string) list |}]

  type ('a,'b,'c,'d,'e) weird_type = A of 'a | B of 'b | C of 'c | D of 'd
                                   | E of 'e [@@deriving t]
  let%expect_test _ =
    print (weird_type_t int_t float_t string_t (alist_t int_t float_t) int_t);
    [%expect {|
      ((int, float, string, (int * float) list, int) weird_type =
         | A of int
         | B of float
         | C of string
         | D of (int * float) list
         | E of int) |}]

  type 'a btree =
    { v: 'a
    ; l: 'a btree option
    ; r: 'a btree option
    } [@@deriving t]

  type 'a bbtree =
    | Inner of 'a * 'a bbtree * 'a bbtree
    | Leave of 'a
  [@@deriving t]

  type ('a,'b) either_list =
    | Either of { v : 'a list }
    | Or of { v : 'b list }
  [@@deriving t]

  let%expect_test _ =
    print (btree_t int_t);
    print (bbtree_t int_t);
    print (either_list_t string_t int_t);
    [%expect {|
    (int btree =
       {
         v: int;
         l: int btree option;
         r: int btree option;
       })
    (int bbtree =
       | Inner of (int * int bbtree * int bbtree)
       | Leave of int)
    ((string, int) either_list =
       | Either of
        ((string, int) either_list.Either =
           {
             v: string list;
           })
       | Or of
        ((string, int) either_list.Or =
           {
             v: int list;
           }))  |}]


end

module Arrows = struct

  type int_arrow = int -> int [@@deriving t]
  type 'a identity = 'a -> 'a [@@deriving t]
  type 'a advanced = ?n:int -> name:'a -> (string -> 'a) -> int [@@deriving t]

  let%expect_test _ =
    print int_arrow_t;
    print (identity_t (array_t string_t));
    print (advanced_t string_t);
    [%expect {|
      (int -> int)
      (string array -> string array)
      (?n:int -> (name:string -> ((string -> string) -> int))) |}]

end
