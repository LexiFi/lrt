open Dynt

let print x =
  Types.stype_of_ttype x
  |> Format.(fprintf std_formatter "%a\n%!" Types.print_stype)

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

type record1 =
  { field1 : int
  ; field2 : string
  ; field3 : float
  ; field4 : int
  }
[@@deriving t]
let%expect_test _ = print record1_t ;
  [%expect{|
    (record1 =
       {
         field1: int;
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

type recrec = { a: int ; b: recrec} [@@deriving t]
type natural = Z | S of natural [@@deriving t]
type natural2 = Z | S of natural2 | Sum of {a: natural2;  b: natural2}
[@@deriving t]
let%expect_test _ =
  print recrec_t ;
  print natural_t ;
  print natural2_t ;
  [%expect{|
    (recrec =
       {
         a: int;
         b: recrec;
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

module M : sig
  type 'num rectangle = { a: 'num ; b: 'num} [@@deriving t]
  type ('a, 'b) alist = ('a * 'b) list [@@deriving t]
  type ('a, 'b) alist2 = ('a * 'b) list [@@deriving t]
  type aalist = (int,string) alist [@@deriving t]
end = struct
  type 'num rectangle = { a: 'num ; b: 'num} [@@deriving t]
  type ('a,'b) alist = ('a * 'b) list [@@deriving t]
  type ('a,'b) alist2 = ('a, 'b) alist [@@deriving t]
  type aalist = (int,string) alist [@@deriving t]
end

let%expect_test _ =
  print (M.rectangle_t int_t);
  print (M.alist_t int_t string_t);
  print (M.alist2_t int_t string_t);
  print M.aalist_t;
  [%expect {|
    (int rectangle =
       {
         a: int;
         b: int;
       })
    (int * string) list
    (int * string) list
    (int * string) list |}]

type ('a,'b,'c,'d,'e) weird_type = A of 'a | B of 'b | C of 'c | D of 'd
                                 | E of 'e [@@deriving t]
let%expect_test _ =
  print (weird_type_t int_t float_t string_t (M.alist_t int_t float_t) int_t);
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

module Mutual = struct

  type foo = bar
  and bar = int [@@deriving t]

  let%expect_test _ =
    print (foo_t);
    print (bar_t);
    [%expect {|
      int
      int |}]

  type 'a weirdtree =
    { node : 'a wnode
    ; children : 'a weirdtree list
    }
  and 'a wnode =
    { basic : 'a
    ; weird : 'a weirdtree
    } [@@deriving t]

  let%expect_test _ =
    print (weirdtree_t string_t);
    print (wnode_t int_t);
    [%expect {|
      (string weirdtree =
         {
           node:
           (string wnode =
              {
                basic: string;
                weird: string weirdtree;
              });
           children: string weirdtree list;
         })
      (int wnode =
         {
           basic: int;
           weird:
           (int weirdtree =
              {
                node: int wnode;
                children: int weirdtree list;
              });
         }) |}]

  type 'a opt_list = None | Some of 'a value
  and 'a value = 'a list
  and 'a opt_llist = 'a opt_list list
  [@@deriving t]

  let%expect_test _ =
    print (opt_list_t int_t);
    print (value_t int_t);
    print (opt_llist_t int_t);
    [%expect {|
      (int opt_list =
         | None
         | Some of int list)
      int list
      (int opt_list =
         | None
         | Some of int list) list |}]

  type 'a forward_ref = 'a target
  and 'a target = 'a list
  [@@deriving t]

  let%expect_test _ =
    print (forward_ref_t int_t);
    print (target_t int_t);
    [%expect {|
      int list
      int list |}]

  type 'a ambiguous_list = Nil | Cons of 'a el
  and 'a el = Singleton of 'a | More of ('a * 'a ambiguous_list)
  [@@deriving t]

  let%expect_test _ =
    print (ambiguous_list_t int_t);
    print (el_t int_t);
    [%expect {|
      (int ambiguous_list =
         | Nil
         | Cons of
          (int el =
             | Singleton of int
             | More of (int * int ambiguous_list)))
      (int el =
         | Singleton of int
         | More of
          (int
           *
           (int ambiguous_list =
              | Nil
              | Cons of int el))) |}]

end

module NonregRec = struct

  type 'a good1 = { field: 'a good1 option; v : 'a }  [@@deriving t]
  type ('a,'b) good2 = {field: ('a,'b) good2 option } [@@deriving t]
  (* type 'a bad1 = { field: int bad1 option; v : 'a } [@@deriving t] *)
  (* type ('a,'b) bad2 = {field: ('b,'a) bad2 option } [@@deriving t] *)

  let%expect_test _ =
    print (good1_t (array_t int_t));
    print (good2_t string_t (array_t int_t));
    [%expect {|
      (int array good1 =
         {
           field: int array good1 option;
           v: int array;
         })
      ((string, int array) good2 =
         {
           field: (string, int array) good2 option;
         }) |}]

end

module Inline = struct

  (* type bad = 'a int [@@deriving t] *)
  (* let bad_t =  [%t: list list ] *)
  (* let bad_t =  [%t: 'a list ] *)

  let t1 (type t) (t : t Types.ttype) = [%t: (t * int) array]
  let t2 (type typ) (typ_t : typ Types.ttype) = [%t: (typ * int) list]


  let%expect_test _ =
    print [%t: int];
    print [%t: int -> int];
    print [%t: string list];
    print [%t: int NonregRec.good1];
    print (t1 int_t);
    print (t2 int_t);
    [%expect {|
      int
      (int -> int)
      string list
      (int good1 =
         {
           field: int good1 option;
           v: int;
         })
      (int * int) array
      (int * int) list |}]
end

module Objects = struct

  type 'a stack = < pop : 'a option; push : 'a -> unit > [@@deriving t]

  class type ['a] steak = object
    method pop : 'a option
    method push: 'a -> unit
  end [@@deriving t]

  class type ['a] tartare = object
    inherit ['a] steak
    method sauce: float
  end

  let%expect_test _ =
    print [%t: int stack];
    (* print [%t: int steak]; *)
    [%expect {|
       <
         pop: int option;
         push: (int -> unit);
      > |}]

end

module Properties = struct

  type 'a fields =
    { we : 'a [@need "some"] [@more "stuff"]
    ; fields : 'a [@with_ "properties"]
    } [@@deriving t]

  type 'a constructors =
    | A of 'a [@key "value"]
    | B of 'a [@k "v"] [@@deriving t]

  type 'a coretype = ('a [@some "prop"]) list [@@deriving t]

  type ('a, 'b) combined =
    | Core of ((('a [@w "a"]) * ('b [@w "b"]) [@w "a*b"]) list [@w "(a*b)list"])
          [@w "Core"]
    | Inline of { field : ('b [@w "b"]) [@w "field"] } [@w "Inline"]
  [@@deriving t] [@@prop "key" "value"]

  let%expect_test _ =
    print [%t: int fields];
    print [%t: int constructors];
    print [%t: int coretype];
    print [%t: (int,string) combined [@w "combined"]];
    [%expect {|
      (int fields =
         {
           we + [need = "some"; more = "stuff"]: int;
           fields + [with_ = "properties"]: int;
         })
      (int constructors =
         | A + [key = "value"] of int
         | B + [k = "v"] of int)
      int + [some = "prop"] list
      ((int, string) combined =
         | Core + [w = "Core"] of (int + [w = "a"] * string + [w = "b"]) + [w = "a*b"] list + [w = "(a*b)list"]
         | Inline + [w = "Inline"] of
          ((int, string) combined.Inline =
             {
               field + [w = "field"]: string + [w = "b"];
             })) + [w = "combined"] |}]

end

module Abstract = struct

  module Two : sig
    type public = int * int [@@deriving t]
    type hidden [@@deriving t]
  end = struct
    type public = int * int [@@deriving t]

    type hidden = string * int
    [@@abstract "One.Abstract.Two.hidden"]
    [@@deriving t]
  end

  type two_public = Two.public [@@deriving t]
  type two_hidden = Two.hidden [@@deriving t]
  type two_hidden2 = two_hidden [@@deriving t]
  let%expect_test _ =
    print two_public_t ;
    print two_hidden_t ;
    print two_hidden2_t ;
    [%expect{|
    (int * int)
    One.Abstract.Two.hidden
    One.Abstract.Two.hidden |}]

  type foo = int [@w "foo"]
  [@@abstract "One.Abstract.foo"]
  and bar = foo [@w "bar"]
  [@@deriving t]

  let%expect_test _ =
    print [%t: foo];
    print [%t: bar];
      [%expect {|
        One.Abstract.foo
        One.Abstract.foo + [w = "bar"] |}]

  module Hashtable : sig
    type ('a,'b) t [@@deriving t]
  end = struct
    type ('a,'b) t = ('a * 'b) list
    [@@abstract "Hashtable.t"]
    [@@deriving t]
  end

  let%expect_test _ =
    print [%t: (int,string) Hashtable.t];
    [%expect {| (int, string) Hashtable.t |}]

end

type nr = int [@@deriving t]
module Nonrec = struct
  (* This fails due to deriving's lack of support *)
  (* type nonrec nr = nr [@@deriving t] *)
end

