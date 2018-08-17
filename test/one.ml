open Dynt

let print x =
  Types.stype_of_ttype x
  |> Format.(fprintf std_formatter "%a\n%!" Types.print_stype)

type t = int [@@deriving t]
type pair1 = string * int [@@deriving t]
type pair2 = int * string [@@deriving t]
type triple = float * int * string [@@deriving t]
type quartet = float * int * string * int [@@deriving t]
type quintet = float * int * string * int * int [@@deriving t]

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
    (float * int * string * int * int) |}]

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

type enum = North | East | South | West [@@deriving t]
let%expect_test _ = print enum_t ;
  [%expect{| (enum = North | East | South | West) |}]

type sum1 =
  | Option1 of string * int
  | Option2 of int * int * string
  | Option3
  | Option4 of Two.public * Two.hidden
[@@deriving t]
let%expect_test _ = print sum1_t ;
  [%expect{|
    (sum1 =
       | Option1 of (string * int)
       | Option2 of (int * int * string)
       | Option3
       | Option4 of ((int * int) * Two.hidden)) |}]

type record1 =
  { field1 : int
  ; field2 : string
  ; field3 : Two.hidden
  ; field4 : Two.public
  }
[@@deriving t]
let%expect_test _ = print record1_t ;
  [%expect{|
    (record1 =
       {
         field1: int;
         field2: string;
         field3: Two.hidden;
         field4: (int * int);
       }) |}]

type inline_record =
  | Basic of Two.public
  | Inline of { h : Two.hidden ; p : Two.public }
[@@deriving t]
let%expect_test _ = print inline_record_t ;
  [%expect{|
    (inline_record =
       | Basic of (int * int)
       | Inline of
        (inline_record.Inline =
           {
             h: Two.hidden;
             p: (int * int);
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

module M : sig
  type 'num rectangle = { a: 'num ; b: 'num} [@@deriving t]
  type ('a, 'b) alist = ('a * 'b) list [@@deriving t]
  type aalist = (int,string) alist [@@deriving t]
end = struct
  type 'num rectangle = { a: 'num ; b: 'num} [@@deriving t]
  type ('a,'b) alist = ('a * 'b) list [@@deriving t]
  type aalist = (int,string) alist [@@deriving t]
end

let%expect_test _ =
  print (M.rectangle_t int_t);
  print (M.alist_t int_t string_t);
  print M.aalist_t;
  [%expect {|
    (rectangle =
       {
         a: int;
         b: int;
       })
    (int * string) list
    (int * string) list |}]

type ('a,'b,'c,'d,'e) weird_type = A of 'a | B of 'b | C of 'c | D of 'd
                                 | E of 'e [@@deriving t]
let%expect_test _ =
  print (weird_type_t int_t float_t string_t (M.alist_t int_t float_t) int_t);
  [%expect {|
    (weird_type =
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

let%expect_test _ =
  print (btree_t int_t);
  print (bbtree_t int_t);
  [%expect {|
    (btree =
       {
         v: int;
         l: int btree option;
         r: int btree option;
       })
    (bbtree =
       | Inner of (int * int bbtree * int bbtree)
       | Leave of int)  |}]

(* type 'a weirdtree = *)
  (* { node : 'a node *)
  (* ; children : 'a weirdtree list *)
  (* } *)
(* and 'a node = *)
  (* { basic : 'a *)
  (* ; weird : 'a weirdtree *)
  (* } [@@deriving t] *)

module N = struct
  open Types
  open Internal

  type 'a t = { pred : ('a * int) t option } [@@deriving t]

  let stype_marc =
    let outer_node = create_node "t" [ DT_var 0 ] in
    let inner_node = create_node "t" [DT_tuple [DT_var 0; DT_int]] in
    let descr = ["pred", [], DT_option (DT_node inner_node)], Record_regular in
    set_node_record outer_node descr;
    set_node_record inner_node descr;
    DT_node outer_node

  let t_marc (a : 'a ttype) : 'a t ttype =
    substitute [| stype_of_ttype a |] stype_marc |> Obj.magic

  let stype_p =
    let node = create_node "t" [ DT_var 0 ] in
    let descr = ["pred", [], DT_option (DT_node node)], Record_regular in
    set_node_record node descr;
    DT_node node

  let t_p (a : 'a ttype) : 'a t ttype =
    substitute [| stype_of_ttype a |] stype_p |> Obj.magic

  let zero = { pred = None }
  let succ (type a) (p : (a * int) t) : a t = { pred = Some p }
  let () =
    let _three : string t = succ (succ (succ zero)) in
    let _t : string t ttype = t string_t in
    (* Debug.print ~t three *) ()
end

let%expect_test _ =
  print (N.t string_t);
  print (N.t_marc string_t);
  print (N.t_p string_t);
  [%expect {|
    (t =
       {
         pred: string t option;
       })
    (string t =
       {
         pred:
         ((string * int) t =
            {
              pred: (string * int) t option;
            }) option;
       })
    (string t =
       {
         pred: string t option;
       }) |}]

