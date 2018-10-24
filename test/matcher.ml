(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the dynt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

open Dynt

type a = int * int

and b = int option

and c = string list

and d = int * (int * string)

and 'a e = 'a list [@@deriving t]

module Matcher = Matcher.Make (struct type 'a t = unit -> unit end)

let matcher =
  Matcher.(
    let m = create ~modulo_props:true in
    add m ~t:a_t (fun _ -> print_endline "a") ;
    add m ~t:b_t (fun _ -> print_endline "b") ;
    add m ~t:c_t (fun _ -> print_endline "c") ;
    add0 m
      ( module struct
        type t = d [@@deriving t]

        let return () = print_endline "d"
      end ) ;
    add1 m
      ( module struct
        type 'a t = 'a e [@@deriving t]

        let return _a_t () = print_endline "e"
      end ) ;
    add2 m
      ( module struct
        type ('a, 'b) t = (('a, 'b) Hashtbl.t[@patch hashtbl_t]) [@@deriving t]

        let return _a_t _b_t () = print_endline "f"
      end ) ;
    m)

let apply : type a. Matcher.t -> t:a Ttype.t -> unit =
 fun matcher ~t ->
  let open Matcher in
  match apply matcher ~t with
  | None -> print_endline "Not found"
  | Some (M0 (module M : M0 with type matched = a)) -> M.return ()
  | Some (M1 (module M : M1 with type matched = a)) -> M.return ()
  | Some (M2 (module M : M2 with type matched = a)) -> M.return ()

let%expect_test _ =
  apply matcher ~t:a_t ;
  apply matcher ~t:b_t ;
  apply matcher ~t:c_t ;
  apply matcher ~t:d_t ;
  apply matcher ~t:(e_t float_t) ;
  apply matcher ~t:(hashtbl_t int_t float_t) ;
  [%expect {|
    a
    b
    c
    d
    e
    f |}]
