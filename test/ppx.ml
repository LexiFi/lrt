(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the dynt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

open Dynt

let print x = Format.printf "%a\n%!" Ttype.print x

module Basic = struct
  type t = int [@@deriving t]
  type pair1 = string * int [@@deriving t]
  type pair2 = int * string [@@deriving t]
  type triple = float * int * string [@@deriving t]
  type quartet = float * int * string * int [@@deriving t]
  type quintet = float * int * string array * int * int [@@deriving t]

  let%expect_test _ =
    print t ;
    print pair1_t ;
    print pair2_t ;
    print triple_t ;
    print quartet_t ;
    print quintet_t ;
    [%expect
      {|
    int
    (string * int)
    (int * string)
    (float * int * string)
    (float * int * string * int)
    (float * int * string array * int * int) |}]

  type enum = North | East | South | West [@@deriving t]

  let%expect_test _ =
    print enum_t ; [%expect {| (enum = North | East | South | West) |}]

  type sum1 =
    | Option1 of string * int
    | Option2 of int * int * string
    | Option3
    | Option4 of unit * int
  [@@deriving t]

  let%expect_test _ =
    print sum1_t ;
    [%expect
      {|
    (sum1 =
       | Option1 of (string * int)
       | Option2 of (int * int * string)
       | Option3
       | Option4 of (unit * int)) |}]

  type record =
    {field1: int * string; field2: string; field3: float; field4: int}
  [@@deriving t]

  let%expect_test _ =
    print record_t ;
    [%expect
      {|
    (record =
       {
         field1: (int * string);
         field2: string;
         field3: float;
         field4: int;
       }) |}]

  type inline_record = Basic of int | Inline of {h: string; p: float}
  [@@deriving t]

  let%expect_test _ =
    print inline_record_t ;
    [%expect
      {|
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
  type record = {a: int; b: record} [@@deriving t]
  type natural = Z | S of natural [@@deriving t]

  type natural2 = Z | S of natural2 | Sum of {a: natural2; b: natural2}
  [@@deriving t]

  let%expect_test _ =
    print record_t ;
    print natural_t ;
    print natural2_t ;
    [%expect
      {|
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
  type 'num rectangle = {a: 'num; b: 'num} [@@deriving t]
  type ('a, 'b) alist = ('a * 'b) list [@@deriving t]
  type ('a, 'b) alist2 = ('a, 'b) alist [@@deriving t]
  type aalist = (int, string) alist [@@deriving t]

  let%expect_test _ =
    print (rectangle_t int_t) ;
    print (alist_t int_t string_t) ;
    print (alist2_t int_t string_t) ;
    [%expect
      {|
      (int rectangle =
         {
           a: int;
           b: int;
         })
      (int * string) list
      (int * string) list |}]

  type ('a, 'b, 'c, 'd, 'e) weird_type =
    | A of 'a
    | B of 'b
    | C of 'c
    | D of 'd
    | E of 'e
  [@@deriving t]

  let%expect_test _ =
    print (weird_type_t int_t float_t string_t (alist_t int_t float_t) int_t) ;
    [%expect
      {|
      ((int, float, string, (int * float) list, int) weird_type =
         | A of int
         | B of float
         | C of string
         | D of (int * float) list
         | E of int) |}]

  type 'a btree = {v: 'a; l: 'a btree option; r: 'a btree option}
  [@@deriving t]

  type 'a bbtree = Inner of 'a * 'a bbtree * 'a bbtree | Leave of 'a
  [@@deriving t]

  type ('a, 'b) either_list = Either of {v: 'a list} | Or of {v: 'b list}
  [@@deriving t]

  let%expect_test _ =
    print (btree_t int_t) ;
    print (bbtree_t int_t) ;
    print (either_list_t string_t int_t) ;
    [%expect
      {|
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
    print int_arrow_t ;
    print (identity_t (array_t string_t)) ;
    print (advanced_t string_t) ;
    [%expect
      {|
      (int -> int)
      (string array -> string array)
      (?n:int -> (name:string -> ((string -> string) -> int))) |}]
end

module Mutual = struct
  type foo = bar

  and bar = int [@@deriving t]

  let%expect_test _ =
    print foo_t ; print bar_t ; [%expect {|
      int
      int |}]

  type 'a weirdtree = {node: 'a wnode; children: 'a weirdtree list}

  and 'a wnode = {basic: 'a; weird: 'a weirdtree} [@@deriving t]

  let%expect_test _ =
    print (weirdtree_t string_t) ;
    print (wnode_t int_t) ;
    [%expect
      {|
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

  and 'a opt_llist = 'a opt_list list [@@deriving t]

  let%expect_test _ =
    print (opt_list_t int_t) ;
    print (value_t int_t) ;
    print (opt_llist_t int_t) ;
    [%expect
      {|
      (int opt_list =
         | None
         | Some of int list)
      int list
      (int opt_list =
         | None
         | Some of int list) list |}]

  type 'a forward_ref = 'a target

  and 'a target = 'a list [@@deriving t]

  let%expect_test _ =
    print (forward_ref_t int_t) ;
    print (target_t int_t) ;
    [%expect {|
      int list
      int list |}]

  type 'a ambiguous_list = Nil | Cons of 'a el

  and 'a el = Singleton of 'a | More of ('a * 'a ambiguous_list)
  [@@deriving t]

  let%expect_test _ =
    print (ambiguous_list_t int_t) ;
    print (el_t int_t) ;
    [%expect
      {|
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
  type 'a good1 = {field: 'a good1 option; v: 'a} [@@deriving t]
  type ('a, 'b) good2 = {field: ('a, 'b) good2 option} [@@deriving t]

  (* type 'a bad1 = { field: int bad1 option; v : 'a } [@@deriving t] *)
  (* type ('a,'b) bad2 = {field: ('b,'a) bad2 option } [@@deriving t] *)

  let%expect_test _ =
    print (good1_t (array_t int_t)) ;
    print (good2_t string_t (array_t int_t)) ;
    [%expect
      {|
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

  let t1 (type t) (t : t Ttype.t) = [%t: (t * int) array]
  let t2 (type typ) (typ_t : typ Ttype.t) = [%t: (typ * int) list]

  let%expect_test _ =
    print [%t: int] ;
    print [%t: int -> int] ;
    print [%t: string list] ;
    print [%t: int NonregRec.good1] ;
    print (t1 int_t) ;
    print (t2 int_t) ;
    [%expect
      {|
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
  type 'a stack = < pop: 'a option ; push: 'a -> unit > [@@deriving t]

  (* This does not trigger deriving *)
  class type ['a] steak =
    object
      method pop : 'a option

      method push : 'a -> unit
    end [@@deriving t]

  class type ['a] tartare =
    object
      inherit ['a] steak

      method sauce : float
    end

  let%expect_test _ =
    print [%t: int stack] ;
    (* print [%t: int steak]; *)
    [%expect
      {|
       <
         pop: int option;
         push: (int -> unit);
      > |}]
end

module Properties = struct
  type 'a fields =
    { we: 'a [@prop {need= "some"; more= "record"}]
    ; fields: 'a [@prop {_with= "properties"}] }
  [@@deriving t]

  type 'a constructors =
    | A of 'a [@prop {key= "value"}]
    | B of 'a [@prop {k= "v"}]
  [@@deriving t]

  type 'a coretype = ('a[@prop {some= "prop"}]) list [@@deriving t]
  type many = (int[@prop {a= "b"; b= "c"}]) [@@deriving t]
  type many' = (many[@prop {c= "d"}]) [@@deriving t]

  type ('a, 'b) combined =
    | Core of
        (((('a[@prop {w= "a"}]) * ('b[@prop {w= "b"}]))[@prop {w= "a*b"}]) list[@prop
                                                                               { 
                                                                               w=
                                                                               "(a*b)list"
                                                                               }])
        [@prop {w= "Core"}]
    | Inline of {field: ('b[@prop {w= "b"}]) [@prop {w= "field"}]}
        [@prop {w= "Inline"}]
  [@@deriving t] [@@prop {key= "value"}]

  let%expect_test _ =
    print [%t: int fields] ;
    print [%t: int constructors] ;
    print [%t: int coretype] ;
    print [%t: many'] ;
    print [%t: ((int, string) combined[@prop {w= "combined"}])] ;
    [%expect
      {|
      (int fields =
         {
           we [@prop {need = "some"; more = "record"}]: int;
           fields [@prop {_with = "properties"}]: int;
         })
      (int constructors =
         | A [@prop {key = "value"}] of int
         | B [@prop {k = "v"}] of int)
      int [@prop {some = "prop"}] list
      int [@prop {a = "b"; b = "c"; c = "d"}]
      ((int, string) combined =
         | Core [@prop {w = "Core"}] of (int [@prop {w = "a"}] * string [@prop {w = "b"}]) [@prop {w = "a*b"}] list [@prop {w = "(a*b)list"}]
         | Inline [@prop {w = "Inline"}] of
          ((int, string) combined.Inline =
             {
               field [@prop {w = "field"}]: string [@prop {w = "b"}];
             })) [@prop {key = "value"; w = "combined"}] |}]

  type s' = (int[@prop {a= "b"; b= "c"}]) [@@deriving t]
  type s = (s'[@prop {c= "d"; d= "e"}]) [@@deriving t]
  type t = (int[@prop {a= "b"; b= "c"; c= "d"; d= "e"}]) [@@deriving t]

  let%test _ =
    fst (Ttype.consume_outer_props s_t) = fst (Ttype.consume_outer_props t)
end

module Abstract = struct
  module Two : sig
    type public = int * int
 and hidden [@@deriving t]
  end = struct
    type public = int * int

    and hidden = string * int
    [@@abstract "One.Abstract.Two.hidden"] [@@deriving t]
  end

  type two_public = Two.public [@@deriving t]
  type two_hidden = Two.hidden [@@deriving t]
  type two_hidden2 = two_hidden [@@deriving t]

  let%expect_test _ =
    print two_public_t ;
    print two_hidden_t ;
    print two_hidden2_t ;
    [%expect
      {|
    (int * int)
    One.Abstract.Two.hidden
    One.Abstract.Two.hidden |}]

  type foo = (int[@prop "w" "foo"]) [@@abstract "One.Abstract.foo"]

  and bar = (foo[@prop "w" "bar"]) [@@deriving t]

  let%expect_test _ =
    print [%t: foo] ;
    print [%t: bar] ;
    [%expect
      {|
        One.Abstract.foo
        One.Abstract.foo [@prop {w = "bar"}] |}]

  module Hashtable : sig
    type ('a, 'b) t [@@deriving t]
  end = struct
    type ('a, 'b) t = ('a * 'b) list [@@abstract "Hashtable.t"] [@@deriving t]
  end

  let%expect_test _ =
    print [%t: (int, string) Hashtable.t] ;
    [%expect {| (int, string) Hashtable.t |}]

  (* automatic names *)
  module A : sig
    type t [@@deriving t]
  end = struct
    type t = int [@@abstract] [@@deriving t]
  end

  module B = struct type t = A.t [@@deriving t] end

  module C : sig
    type t [@@deriving t]
  end = struct
    type t = A.t [@@abstract] [@@deriving t]
  end

  let%expect_test _ =
    print [%t: A.t] ;
    print [%t: B.t] ;
    print [%t: C.t] ;
    [%expect
      {|
      dynt_test#test/ppx.ml.Abstract.A.t
      dynt_test#test/ppx.ml.Abstract.A.t
      dynt_test#test/ppx.ml.Abstract.C.t |}]

  (* use non abstract ttype internally *)

  module D : sig
    type a
 and b = int * a * c
 and c = string [@@deriving t]

    type d
 and e = int * d * f
 and f = string [@@deriving t]
  end = struct
    (* Idea 1:
     * -> stype of a will leak via b
     *)
    type a' = int * int

    and b = int * a' * c

    and c = string [@@deriving t]

    type a = a' [@@abstract "D.a"] [@@deriving t]

    let%expect_test _ =
      print a'_t ; print a_t ; [%expect {|
        (int * int)
        D.a |}]

    (* Idea 2:
     * -> stype of d does not leak
     *)
    type d' = int * int

    and d = d' [@@abstract "D.d"]

    and e = int * d * f

    and f = string [@@deriving t]

    let%expect_test _ =
      print d'_t ; print d_t ; [%expect {|
        (int * int)
        D.d |}]
  end

  let%expect_test _ =
    print D.b_t ;
    print D.e_t ;
    [%expect
      {|
      (int * (int * int) * string)
      (int * D.d * string) |}]
end

module Alias = struct
  type t = Basic.sum1 =
    | Option1 of string * int
    | Option2 of int * int * string
    | Option3
    | Option4 of unit * int
  [@@deriving t]

  let%expect_test _ =
    print [%t: t] ;
    [%expect
      {|
      (t =
         | Option1 of (string * int)
         | Option2 of (int * int * string)
         | Option3
         | Option4 of (unit * int)) |}]
end

type nr = int [@@deriving t]

module Nonrec = struct
  type nonrec nr = nr * int [@@deriving t]

  let%expect_test _ =
    print [%t: nr] ;
    [%expect {| (int * int) |}]
end

module Overwrite = struct
  (* Pervasives have the be toplevel, not locally by the
   * derived code
   *
   * This test fails when ppx runtime includes Dynt.Pervasives
   *)

  type string = int [@@deriving t]
  type int = string [@@deriving t]

  let%expect_test _ =
    print [%t: string] ;
    print [%t: int] ;
    [%expect {|
      int
      int |}]
end

module Patch = struct
  type a = int [@@deriving t]
  type b = int
  type c = (b[@patch a_t]) [@@deriving t]
  type meta = string list [@@deriving t] [@@abstract "Patch.meta"]

  type ('a, 'b) ht = {table: (('a, 'b) Hashtbl.t[@patch hashtbl_t]); meta: meta}
  [@@deriving t]

  (* Nice error messages included
  type 'a arr = 'a array
  type 'a lst = { arr : ('a arr [@patch list_t])} [@@deriving t]
  *)

  let%expect_test _ =
    print [%t: c] ;
    print [%t: (string, float) ht] ;
    [%expect
      {|
      int
      ((string, float) ht =
         {
           table: (string, float) Hashtbl.t;
           meta: Patch.meta;
         }) |}]
end

module FloatRecord = struct
  (* Check for correct use of Record_regular and Record_float.
   * This might break the record builders in xtypes while getters are not
   * affected. Thus we build values using xtypes and compare
   * ocaml getters with xtype getters *)
  type alias = float [@@deriving t]

  type regular = {rgx: float; rgy: float; rgz: string}

  and float1 = {f1x: float; f1y: float; f1z: float}

  and float2 = {f2x: float; f2y: alias; f2z: float}

  and 'a record = {f3x: 'a; f3y: 'a; f3z: 'a} [@@deriving t]

  let test ttype gx gy =
    let _ = Random.self_init () in
    let fpaths = Xtype.all_paths ttype float_t in
    let geti i t =
      let lens = Path.lens (List.nth fpaths i) in
      match lens.get t with None -> assert false | Some x -> x
    in
    let generator = Check.of_type_gen ~size:101 ~t:ttype [] in
    let check a b t =
      if compare (a t) (b t) = 0 then true
      else (
        Format.eprintf "screwed: a:%f b:%f\n%!" (a t) (b t) ;
        false )
    in
    let property t = check gx (geti 0) t && check gy (geti 1) t in
    match Check.test 42 ~seed:(Random.bits ()) ~generator property with
    | Succeed _ -> true
    | Fail _ -> Format.eprintf "fail\n%!" ; false
    | Throw {backtrace; _} ->
        Format.eprintf "throw\n%s%!" backtrace ;
        false

  let%test _ = test regular_t (fun t -> t.rgx) (fun t -> t.rgy)
  let%test _ = test float1_t (fun t -> t.f1x) (fun t -> t.f1y)
  let%test _ = test float2_t (fun t -> t.f2x) (fun t -> t.f2y)
  let%test _ = test (record_t float_t) (fun t -> t.f3x) (fun t -> t.f3y)
end

module Unboxed = struct
  (* Check for correct use of Record_regular and Record_unboxed.
   * This might break the record builders in xtypes while getters are not
   * affected. Thus we build values using xtypes and compare
   * ocaml getters with xtype getters *)

  let test root target gf eq =
    let _ = Random.self_init () in
    let fpaths = Xtype.all_paths root target in
    let geti i t =
      let lens = Path.lens (List.nth fpaths i) in
      match lens.get t with None -> assert false | Some x -> x
    in
    let generator = Check.of_type_gen ~size:101 ~t:root [] in
    let check a b t =
      let x_a, x_b = (a t, b t) in
      if eq x_a x_b then true else false
      (* let p f = Inspect.Sexpr.dump_with_formatter f in
        Format.eprintf "obj: %a\nocaml: %a\npath: %a\n\n%!" p t p x_a p x_b ;
        false *)
    in
    let property t = check gf (geti 0) t in
    let seed = Random.bits () in
    match Check.test 666 ~seed ~generator property with
    | Succeed _ -> true
    | Fail _ -> Format.eprintf "fail\n%!" ; false
    | Throw {backtrace; _} ->
        Format.eprintf "throw:\n%s%!" backtrace ;
        false

  (* nan = nan -> false *)
  let tfloat t get = test t float_t get (fun a b -> compare a b = 0)
  let tint t get = test t int_t get ( = )

  type r1 = {f1: float} [@@deriving t]

  let%test _ = tfloat r1_t (fun t -> t.f1)

  type r2 = {f2: float} [@@deriving t] [@@unboxed]

  let%test _ = tfloat r2_t (fun t -> t.f2)

  type r3 = {f3: int} [@@deriving t]

  let%test _ = tint r3_t (fun t -> t.f3)

  type r4 = {f4: int} [@@deriving t] [@@unboxed]

  let%test _ = tint r4_t (fun t -> t.f4)

  type 'a r5 = {f5: 'a} [@@deriving t] [@@ocaml.unboxed]

  let%test _ = tint (r5_t int_t) (fun t -> t.f5)
  let%test _ = tfloat (r5_t float_t) (fun t -> t.f5)

  type s1 = A of float [@@deriving t]

  let%test _ = tfloat s1_t (function A x -> x)

  type s2 = A of float [@@deriving t] [@@ocaml.unboxed]

  let%test _ = tfloat s2_t (function A x -> x)

  type s3 = A of int [@@deriving t]

  let%test _ = tint s3_t (function A x -> x)

  type s4 = A of int [@@deriving t] [@@unboxed]

  let%test _ = tint s4_t (function A x -> x)

  type 'a s5 = A of 'a [@@deriving t] [@@unboxed]

  let%test _ = tint (s5_t int_t) (function A x -> x)
  let%test _ = tfloat (s5_t float_t) (function A x -> x)

  type 'a x1 = X of {x1: 'a} [@@deriving t]

  let%test _ = tint (x1_t int_t) (function X x -> x.x1)
  let%test _ = tfloat (x1_t float_t) (function X x -> x.x1)

  type 'a x2 = X of {x2: 'a} [@@unboxed] [@@deriving t]

  let%test _ = tint (x2_t int_t) (function X x -> x.x2)
  let%test _ = tfloat (x2_t float_t) (function X x -> x.x2)
end

module NoName = struct
  (* What happens to abstract names, if there is no name?
   * -> It's broken *)

  include (
    struct
        type t = int [@@abstract] [@@deriving t]
      end :
      sig
        type t [@@deriving t]
      end )

  module F (X : sig
    type t [@@deriving t]
  end) =
  struct
    type s = X.t [@@deriving t]
    type t = s [@@abstract] [@@deriving t]
  end

  module M = F (struct type t = int [@@deriving t] end)

  (* module N = F(struct type t = string [@@deriving t] end) *)

  let%expect_test _ =
    print t ;
    print M.s_t ;
    print M.t ;
    (* print N.s_t; *)
    (* print N.t; *)
    [%expect
      {|
      dynt_test#test/ppx.ml.NoName.t
      int
      dynt_test#test/ppx.ml.NoName.F.t |}]
end
