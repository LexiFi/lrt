(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the lrt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

open Lrt

(*
type id = int -> int [@@deriving t]
let fn :id = fun x -> x
let _ =
  Format.printf "%a\n%!" Stype.print_stype (Ttype.stype_of_ttype id_t);
  Print.show ~t:Variant.t (Variant.variant ~t:id_t fn) *)

let pprint p = Format.(printf "%a\n%!" Path.print p)
let tprint x = Format.printf "%a\n%!" Ttype.print x
let vprint t v = Format.(printf "%a\n%!" (Print.print ~t) v)

let dprint dyn =
  let open Ttype in
  let (Dyn (t, v)) = dyn in
  vprint t v

type t = A of {b: int array list * string} [@@deriving t]

let value = A {b= ([[|0; 1|]; [|0; 1; 2|]; [|0|]], "string")}
let assert_some = function Some x -> x | None -> assert false

let%expect_test _ =
  let f p =
    let Path.({get; _}) = Path.lens p in
    let t = Xtype.project_path t p in
    pprint p ;
    tprint t ;
    get value |> assert_some |> vprint t ;
    print_endline "----------------"
  in
  f Path.[] ;
  f [%path? [A b]] ;
  f [%path? [A b; ([], _)]] ;
  f [%path? [A b; (_, [])]] ;
  f [%path? [A b; ([], _); [1]]] ;
  f [%path? [A b; ([], _); [1]; [|2|]]] ;
  [%expect
    {|
    [%path? []]
    (lrt_test#test/path.ml.t =
       | A of
        (t.A =
           {
             b: (int array list * string);
           }))
    A {b = ([[|0; 1|]; [|0; 1; 2|]; [|0|]], "string")}
    ----------------
    [%path? [A b]]
    (int array list * string)
    ([[|0; 1|]; [|0; 1; 2|]; [|0|]], "string")
    ----------------
    [%path? [A b; ([],_)]]
    int array list
    [[|0; 1|]; [|0; 1; 2|]; [|0|]]
    ----------------
    [%path? [A b; (_,[])]]
    string
    "string"
    ----------------
    [%path? [A b; ([],_); [1]]]
    int array
    [|0; 1; 2|]
    ----------------
    [%path? [A b; ([],_); [1]; [|2|]]]
    int
    2
    ---------------- |}]

type y = Int of int | Bool of bool | Pair of int * string
type z = Y of {y1: y; y2: y; y3: y}
type x = {x1: z; x2: z}
type r = x * y
type s = r list
type f = s array
type e = {e: f}

let p = [%path? [e; [|50|]; [1]; ([], _); x1; Y y2; Pair (_, [])]]

let%expect_test _ =
  pprint p ;
  [%expect {| [%path? [e; [|50|]; [1]; ([],_); x1; Y y2; Pair (_,[])]] |}]

let value = [|["hey"; "hi"]|]
let p2 = [%path? [[|0|]; [1]]]
let l2 = Lrt.Path.lens p2

let%expect_test _ =
  print_endline (l2.get value |> assert_some) ;
  print_endline (l2.set value "salut" |> assert_some |> l2.get |> assert_some) ;
  [%expect {|
    hi
    salut |}]
