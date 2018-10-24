(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the dynt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

open Dynt

module T = struct
  (* stripped down version for documentation *)

  type t0 = string list

  and 'a t1 = 'a array

  and ('a, 'b) t2 = ('a * 'b) option [@@deriving t]

  module Matcher = Matcher.Make (struct type 'a t = unit -> unit end)

  let m = Matcher.create ~modulo_props:true
  let pp_ty = Ttype.print

  let () =
    let open Matcher in
    add m ~t:[%t: string list] (fun () ->
        Format.printf "t0 = %a\n%!" pp_ty t0_t ) ;
    add1 m
      ( module struct
        type 'a t = 'a t1 [@@deriving t]

        let return a_t () =
          Format.printf "%a t1 = %a\n%!" pp_ty a_t pp_ty (t1_t a_t)
      end ) ;
    add2 m
      ( module struct
        type ('a, 'b) t = ('a, 'b) t2 [@@deriving t]

        let return a_t b_t () =
          Format.printf "(%a, %a) t2 = %a\n%!" pp_ty a_t pp_ty b_t pp_ty
            (t2_t a_t b_t)
      end )

  let apply : type a. Matcher.t -> t:a Ttype.t -> unit =
   fun matcher ~t ->
    let open Matcher in
    match apply matcher ~t with
    | None -> print_endline "Not found"
    | Some (M0 (module M : M0 with type matched = a)) -> M.return ()
    | Some (M1 (module M : M1 with type matched = a)) -> M.return ()
    | Some (M2 (module M : M2 with type matched = a)) -> M.return ()

  let%expect_test _ =
    apply m ~t:[%t: t0] ;
    apply m ~t:[%t: int t1] ;
    apply m ~t:[%t: bool t1] ;
    apply m ~t:[%t: float option] ;
    apply m ~t:[%t: (float, string) t2] ;
    apply m ~t:[%t: (unit, string) t2] ;
    [%expect
      {|
      t0 = string list
      int t1 = int array
      bool t1 = bool array
      Not found
      (float, string) t2 = (float * string) option
      (unit, string) t2 = (unit * string) option |}]
end

type a = int * int

and b = int option

and c = string list

and d = int * (int * string)

and 'a e = 'a list

and ('a, 'b) f = (('a, 'b) Hashtbl.t[@patch hashtbl_t]) [@@deriving t]

module Matcher = Matcher.Make (struct type 'a t = unit -> unit end)

let pp_ty = Ttype.print

let matcher =
  Matcher.(
    let m = create ~modulo_props:true in
    add m ~t:a_t (fun () -> Format.printf "a = %a\n%!" pp_ty a_t) ;
    add m ~t:b_t (fun () -> Format.printf "b = %a\n%!" pp_ty b_t) ;
    add m ~t:c_t (fun () -> Format.printf "c = %a\n%!" pp_ty c_t) ;
    add0 m
      ( module struct
        type t = d [@@deriving t]

        let return () = Format.printf "d = %a\n%!" pp_ty d_t
      end ) ;
    add1 m
      ( module struct
        type 'a t = 'a e [@@deriving t]

        let return a_t () =
          Format.printf "%a e = %a\n%!" pp_ty a_t pp_ty (e_t a_t)
      end ) ;
    add2 m
      ( module struct
        type ('a, 'b) t = ('a, 'b) f [@@deriving t]

        let return a_t b_t () =
          Format.printf "(%a, %a) f = %a\n%!" pp_ty a_t pp_ty b_t pp_ty
            (t a_t b_t)
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
  apply matcher ~t:(f_t int_t float_t) ;
  [%expect
    {|
    a = (int * int)
    b = int option
    c = string list
    d = (int * (int * string))
    float e = float list
    (int, float) f = (int, float) Hashtbl.t |}]
