(* Let's try to unify multiple types at once. *)

open Dynt

type a = (int * int)
and b = int option
and c = string list
and d = (int * (int * string))
and 'a e = 'a list
[@@deriving t]

let e_t = e_t

module type C0 = sig
  include Xtype.T0
  type res
  val f: t -> res
end

module type C1 = sig
  include Xtype.T1
  type res
  val f: 'a Ttype.t -> 'a t -> res
end

module type C2 = sig
  include Xtype.T2
  type res
  val f: 'a Ttype.t -> 'b Ttype.t -> ('a, 'b) t -> res
end

module Matcher : sig
  type 'a t
  type 'a candidates

  val empty: 'a candidates
  val add: t: 'b Ttype.t -> f:('b -> 'a) -> 'a candidates -> 'a candidates
  val add0: (module C0 with type res = 'a) -> 'a candidates -> 'a candidates
  val add1: (module C1 with type res = 'a) -> 'a candidates -> 'a candidates
  val add2: (module C2 with type res = 'a) -> 'a candidates -> 'a candidates

  val create: 'a candidates -> 'a t
  (** Prepare the list of candidates for efficient matching. *)

  val apply: 'a t -> t: 'b Ttype.t -> 'b -> 'a
  (** [match c ~default ~t x] checks for type equality of [t] with a type
      registered with [add ~t ~f].

      Returns [f x] if such a [f] is present and otherwise raises [Not_found].
  *)
end = struct
  type 'a candidate =
    | T : { t: 'b Ttype.t; f:('b -> 'a) } -> 'a candidate
    | T0 : (module C0 with type res = 'a) -> 'a candidate
    | T1 : (module C1 with type res = 'a) -> 'a candidate
    | T2 : (module C2 with type res = 'a) -> 'a candidate

  type 'a candidates = 'a candidate list
  type 'a t = 'a candidate list

  let empty = []

  let add ~t ~f lst = (T {t;f}) :: lst
  let add0 m lst = (T0 m) :: lst
  let add1 m lst = (T1 m) :: lst
  let add2 m lst = (T2 m) :: lst

  let create x = List.rev x

  let apply : type a b. a t -> t: b Ttype.t -> b -> a =
    fun matcher ~t x ->
    let rec loop = function
      | [] -> raise Not_found
      | T c :: tl -> begin
        match Ttype.equality_modulo_props c.t t with
        | Some (TypEq.Eq) -> c.f x
        | None -> loop tl
      end
      | T0 (module C : C0 with type res = a) :: tl -> begin
          let module M = Xtype.Match0 (C) in
          match M.is_t t with
          | None -> loop tl
          | Some (M.Is (TypEq.Eq)) -> C.f x
        end
      | T1 (module C : C1 with type res = a) :: tl -> begin
          let module M = Xtype.Match1 (C) in
          match M.is_t t with
          | None -> loop tl
          | Some (M.Is (a, TypEq.Eq)) -> C.f a x
        end
      | T2 (module C : C2 with type res = a) :: tl -> begin
          let module M = Xtype.Match2 (C) in
          match M.is_t t with
          | None -> loop tl
          | Some (M.Is (a,b, TypEq.Eq)) -> C.f a b x
        end
    in loop matcher
end

let matcher =
  let open Matcher in
  empty
  |> add ~t:a_t ~f:(fun _ -> print_endline "a")
  |> add ~t:b_t ~f:(fun _ -> print_endline "b")
  |> add ~t:c_t ~f:(fun _ -> print_endline "c")
  |> add0 (module struct
    type res = unit
    type t = d [@@deriving t]
    let f _ = print_endline "d" end)
  |> add1 (module struct
    type res = unit
    type 'a t = 'a e [@@deriving t]
    let f _ _ = print_endline "e" end)
  |> add2 (module struct
    type res = unit
    type ('a,'b) t = ('a,'b) Hashtbl.t [@patch hashtbl_t] [@@deriving t]
    let f _ _ _ = print_endline "f" end)
  |> create

let%expect_test _ =
  Matcher.apply matcher ~t:a_t (1,1) ;
  Matcher.apply matcher ~t:b_t None ;
  Matcher.apply matcher ~t:c_t ["string";"list"] ;
  Matcher.apply matcher ~t:d_t (1,(1,"")) ;
  Matcher.apply matcher ~t:(e_t float_t) [1.] ;
  Matcher.apply matcher ~t:(hashtbl_t int_t float_t) (Hashtbl.create 1);
  [%expect {|
    a
    b
    c
    d
    e
    f |}]


